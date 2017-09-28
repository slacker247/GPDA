#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h> 
#include <time.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <errno.h>

#include "Globals.h"
#include "isql.h"

#include "forms.h"
#include "CLAforms.h"

/* --------------------------------------------------------------------- */

#ifndef FALSE
#define FALSE   0
#endif
#ifndef TRUE
#define TRUE    1
#endif

#define PUSHED  1

/* --------------------------------------------------------------------- */

typedef struct {
  char          ChType[40];
  char          filebase[128];
} CLATYPE;

typedef struct LEXICAL {
  float         lower;
  float         upper;
  char          desc[16];
};

/* --------------------------------------------------------------------- */

int             CLAinited = 0;
char            clatemp[1024];
char            CLAlabel[32];
char            CLAFileBase[64][64];

int             claMYSQL = 0;
extern int      bHTMLTable;
extern int      bBatch;
extern int      cDelimiter;
extern int      bColumnNames;
extern SQLHSTMT hStmt;
SQLHENV         CLAhEnv  = 0;
SQLHDBC         CLAhDbc  = 0;


FILE            *itelfp;
FILE            *fp;
int             Itel_filled = FALSE;
int             OutFileOpen = FALSE;
CLATYPE         iteltypes[20];

extern char     *SIMTEST;
extern char     *DEBUG;
extern char     *SOCKET;
extern char     UserName[48];
extern int      LexCount;;
extern LEXICAL  LexTable[9];

int             claFieldCount = 52;
char            claFieldNames[52][16] = {
                  "Case_ID",    "Hypothesis",   "Belief",      "Disbelief",
                  "Report Org", "Report Affil", "Report Desc", "Report Name",
                  "Enemy Org",  "Enemy Affil",  "Enemy Desc",  "Enemy Name",
                  "Ally Org",   "Ally Affil",   "Ally Desc",   "Ally Name",
                  "Other Org",  "Other Affil",  "Other Desc",  "Other Name",
                  "Region",     "Nation",       "Locale",      "City",    
                  "Structure",  "Latitude",     "Longitude",   "Altitude",
	  	  "Subject",    "Action",       "Objective",
                  "Strategic",  "Operation",    "Tactical",    "Task",
                  "Method",     "Desc_1",       "Desc_2",      "Desc_3",
                  "Casualties", "Injuries",     "Cost",        "Adjective",
                  "Start_Time", "End_Time",     "Duration",    "Time_Frame",
		  " ",
                  "Info-ID",    "Prepared",     "Prep-Date",   "Story" };
 
FD_bmcintel     *fd_bmcintel;
FD_clafields    *fd_clafields;
 
extern int      EraseActiveEntry(char *text);
extern int      StoreActiveEntry(char *text);
extern int      FinishUp();  

/* --------------------------------------------------------------------- */

void CLAinit();
void CLAnext();
void CLAshow(int xpos, int ypos, int width, int height, Window winid, int mode, char *fn);
void ClearFields();
void FillField(FILE *fp);
/*                                                                       */
/* --------------------------------------------------------------------- */
/*              M A I N   P R O G R A M   S T A R T S   H E R E          */
/* --------------------------------------------------------------------- */
/*                                                                       */
void CLAinit()
{
FILE            *typefp;
int             i, j, k;

   if (CLAinited) return;

   fd_bmcintel = create_form_bmcintel();
   fd_clafields = create_form_clafields();

   strcpy(CLAlabel, "Hypothesis-Tagger"); 

   fl_set_browser_fontsize(fd_bmcintel->intel_source, 10);
   fl_set_browser_fontstyle(fd_bmcintel->intel_source, FL_FIXED_STYLE|FL_BOLD_STYLE);

   for (i=0; i<LexCount; i++) { 
     fl_addto_choice(fd_bmcintel->itel_belief, LexTable[i].desc);
     fl_addto_choice(fd_bmcintel->itel_disbelief, LexTable[i].desc);
   }

   //itelmissionCB(NULL, 0);

   CLAinited = TRUE;
   typefp = fopen("GPDAinfo.log", "a");
   fprintf(typefp, "Hypothesis Tagging algorithm initialization complete.\n");
   fclose(typefp);

   return;
}

void CLAexitCB(FL_OBJECT *ob, long data)
{
   if (claMYSQL) CloseDatabase( CLAhEnv, CLAhDbc );

   fl_hide_form(fd_bmcintel->bmcintel);
   EraseActiveEntry(CLAlabel);

   FinishUp();
 
   return;
}

int CLAclose(FL_FORM *form, void *data)
{
   CLAexitCB(NULL, 0);

   return (0);
}
 
void CLAnoneCB(FL_OBJECT *ob, long data)
{
 
   return;
}

void CLAnext()
{
int             item = 0;
static int      opened = FALSE;
static FILE     *infile;
static int      icount = 0;

int             i,j,l,n;
int             nsource, nlines, srcid, nargs_in;
int             day;
float           hours;
float           x, y;
char            ch;
char            chsource[5][16] = { "Unknown", "Open Source", "Human Intel",
                                    "Signal Intel", "Image Intel" };
char            chline[80];
char            ipline[1280];
char            chsrc[16];
char            chout[5][120];

int             nargs;
unsigned int    xferbytes;
char            evid_lab1[8], evid_lab2[8], evid_lab3[8];
char            buf[2000], src[5], dst[5], dea[5];

   if (!opened) {
      infile = fopen("intel.in", "r+");
      opened = TRUE;
   }
//
//   Get the next INTEL record
//
   if (fscanf(infile, "%d %f", &day, &hours) <= 0) {
      fclose(infile);
      opened = FALSE;
      return;
   }

   fscanf(infile, "%d", &nsource);        // get number of sources
   strcpy(ipline, "\0");
   fl_clear_browser(fd_bmcintel->itel_sources);

   fl_set_browser_fontsize(fd_bmcintel->intel_source, FL_SMALL_SIZE);
   fl_set_browser_fontstyle(fd_bmcintel->intel_source, FL_FIXED_STYLE);
   fl_clear_browser(fd_bmcintel->intel_source);
   sprintf(clatemp, "@b@i@C4@N@_%-16s %-33s   %6s   %-10s   %-8s   %-8s   %-8s",
           "Evidence Source", "    Belief           Disbelief", "Time", "Duration",
           evid_lab1, evid_lab2, evid_lab3);
   fl_addto_browser(fd_bmcintel->intel_source, clatemp);

   for (i=0; i<nsource; i++) {
      j = 0;
      fscanf(infile, "%s %d %f %f\n", chsrc, &srcid, &x, &y);
      if (chsrc[0] == 'O') j = 1;
      else if (chsrc[0] == 'H') j = 2;
      else if (chsrc[0] == 'S') j = 3;
      else if (chsrc[0] == 'I') j = 4;
      fl_addto_browser(fd_bmcintel->itel_sources, chsource[j]);
      sprintf(chline, "%-16s %10d   %10.4f   %10.4f\n", chsource[j], srcid, x, y);
      strcat(ipline, chline);
   }

   fscanf(infile, "%d", &nlines);         // get number of lines
   fgetc(infile);                         // get newline char
   strcpy(chout[0], " ");
   strcpy(chout[1], " ");
   strcpy(chout[2], " ");
   strcpy(chout[3], " ");
   strcpy(chout[4], " ");
   for (i=0; i<nlines; i++) {
      ch = fgetc(infile); l = 0;          // get 1st char
      while (ch != '\n') {
        chline[l] = ch;
        l = l+1;
        ch = fgetc(infile);
      }
      chline[l] = '\0';
      strcpy(chout[i], chline);
   }
   sprintf(ipline, "%s\n%s\n%s\n%s\n%s", chout[0], chout[1], chout[2], chout[3], chout[4]);
}

void CLAshow(int xpos, int ypos, int width, int height, Window mainwinID,
	      int mode, char *fname)
{
FILE            *typefp;
int             i, j, nCols, num_types;
char            chflag;
char            chtemp[32];
Window          winid;

char            szDSN[MAX_DATA_WIDTH+1] = "MySQL";
char            szUID[MAX_DATA_WIDTH+1] = "root";
char            szPWD[MAX_DATA_WIDTH+1] = "gpda";
char            szSQL[9001]             = "show databases;";
char            chcase[32];
char            chvalue[64];
char            chfile[128];

SQLINTEGER    	nCol            		= 0;
SQLCHAR		szColumn[MAX_DATA_WIDTH+20]	= "";	
SQLCHAR		szColumnName[MAX_DATA_WIDTH+1]	= "";	
SQLCHAR		szHdrLine[32001]		= "";	
SQLUINTEGER	nOptimalDisplayWidth            = 10;
SQLLEN		nIndicator			= 0;
SQLCHAR		szColumnValue[MAX_DATA_WIDTH+1]	= "";
SQLRETURN	nReturn;
SQLCHAR         szSepLine[32001] = "";
SQLSMALLINT     cols;
SQLLEN          nRows = 0;
SQLINTEGER      ret;
int             nUserWidth  = 0;
//
//   Pop up the window
//
   if(!fl_form_is_visible(fd_bmcintel->bmcintel) ) {
      fl_transient();
      fl_winposition(xpos, ypos);
      fl_initial_winsize(width, height);
      winid = fl_prepare_form_window(fd_bmcintel->bmcintel,
                                     FL_PLACE_POSITION,FL_TRANSIENT, CLAlabel);
      //fl_set_form_geometry(fd_bmcintel->bmcintel, xpos, ypos, width, height);
      fl_winreshape(winid, xpos, ypos, width, height);
      fl_show_form_window(fd_bmcintel->bmcintel);
      fl_set_form_atclose(fd_bmcintel->bmcintel, CLAclose, 0);
      StoreActiveEntry(CLAlabel);
      ClearFields();
   }
//
//   Load the missions to select from
//
   fl_clear_menu(fd_bmcintel->menu_mission);
   if (claMYSQL) {
     //
     //   Open the Database for this Domain
     //
     if ( !OpenDatabase( &CLAhEnv, &CLAhDbc, szDSN, szUID, szPWD ))
       exit(-1);
     //
     //   Find out which mission databases are available
     //
     nRows = 0;
     strcpy(szSQL, "show databases;");
     ExecuteSQL( CLAhDbc, szSQL, cDelimiter, bColumnNames, bHTMLTable, nCols );
     if ( nCols > 0 ) {
       nReturn = SQLFetch( hStmt );
       while ( nReturn == SQL_SUCCESS || nReturn == SQL_SUCCESS_WITH_INFO ) {
	 for( nCol = 1; nCol <= nCols; nCol++ ) {
	   nReturn = SQLGetData( hStmt, nCol, SQL_C_CHAR, (SQLPOINTER)szColumnValue,
			       sizeof(szColumnValue), &nIndicator );
	   szColumnValue[MAX_DATA_WIDTH] = '\0';
	   if ( nReturn == SQL_SUCCESS && nIndicator != SQL_NULL_DATA ) {
	     sprintf(clatemp, "%s", szColumnValue);
	     if ( (strstr(clatemp, "test") == NULL) && (strstr(clatemp, "mysql") == NULL) )
	       fl_addto_menu(fd_bmcintel->menu_mission, clatemp);
	   } else if ( nReturn == SQL_ERROR ) {
	     break;
	   }
	 } // for columns
	 nRows++;
	 nReturn = SQLFetch( hStmt );
       } // while rows
     }
     SQLFreeStmt( hStmt, SQL_DROP );
   } else {
     //
     //   Load the initial Assessment types from initialization file
     //
     typefp = fopen("DSBinit.dat", "r");
     do fgets(clatemp, 128, typefp); while (clatemp[0] == '#');    // Read # assessments
     sscanf(clatemp, "%d", &num_types);
     for (j=0; j<num_types; j++) {
       do fgets(clatemp, 128, typefp); while (clatemp[0] == '#');
       //
       //   Read assessment type and file base names
       //
       sscanf(clatemp, "%s %s", chvalue, CLAFileBase[j]);
       strsub(chvalue, '_', ' ');
       fl_addto_menu(fd_bmcintel->menu_mission, chvalue);
     }
     fclose(typefp);
   }

   Itel_filled = FALSE;

   return;
}
/*                                                                       */
/* --------------------------------------------------------------------- */
/*      C A L L B A C K    R O U T I N E S    S T A R T    H E R E       */
/* --------------------------------------------------------------------- */
/*                                                                       */
void clamissionCB(FL_OBJECT *ob, long data)
{
int             i, j, k, m, nCols;
char            chflag;
char            szSQL[32];
char            chtemp[32];
SQLINTEGER    	nCol            		= 0;
SQLCHAR		szColumn[MAX_DATA_WIDTH+20]	= "";	
SQLCHAR		szColumnName[MAX_DATA_WIDTH+1]	= "";	
SQLCHAR		szHdrLine[32001]		= "";	
SQLUINTEGER	nOptimalDisplayWidth            = 10;
SQLLEN		nIndicator			= 0;
SQLCHAR		szColumnValue[MAX_DATA_WIDTH+1]	= "";
SQLRETURN	nReturn;
SQLCHAR         szSepLine[32001] = "";
SQLSMALLINT     cols;
SQLLEN          nRows = 0;
SQLINTEGER      ret;

   fl_set_input(fd_bmcintel->itel_field[51], fl_get_menu_text(fd_bmcintel->menu_mission));
//
//   Re-label the widgets according to the mission
//
   if (claMYSQL) { 
     //
     //   Use selected mission
     //
     i = fl_get_menu(fd_bmcintel->menu_mission);
     sprintf(szSQL, "use %s;", fl_get_menu_text(fd_bmcintel->menu_mission));

     ExecuteSQL( CLAhDbc, szSQL, cDelimiter, bColumnNames, bHTMLTable, nCols );
     //
     //   Get the field labels
     //
     i = -1;
     j = -1;
     nRows = 0;
     strcpy(szSQL, "SELECT * FROM Labels;");
     ExecuteSQL( CLAhDbc, szSQL, cDelimiter, bColumnNames, bHTMLTable, nCols );
     if ( nCols > 0 ) {
       nReturn = SQLFetch( hStmt );
       while ( nReturn == SQL_SUCCESS || nReturn == SQL_SUCCESS_WITH_INFO ) {
	 for( nCol = 1; nCol <= nCols; nCol++ ) {
	   nReturn = SQLGetData( hStmt, nCol, SQL_C_CHAR, (SQLPOINTER)szColumnValue,
			       sizeof(szColumnValue), &nIndicator );
	   szColumnValue[MAX_DATA_WIDTH] = '\0';
	   if ( nReturn == SQL_SUCCESS && nIndicator != SQL_NULL_DATA ) {
	     sprintf(clatemp, "%s", szColumnValue);
	     strsub(clatemp, '_', ' ');
	     chflag = clatemp[0];
	     clatemp[0] = ' ';
	     if (chflag == '>') {
	       i++;
	       strcpy(claFieldNames[i], clatemp);
	       fl_set_object_label(fd_bmcintel->itel_field[i], clatemp);
	     } else if (chflag == '+') {
	       j++;
	       fl_set_object_label(fd_bmcintel->itel_frame[j], clatemp);
	     } else if (chflag == '-') { 
	       i++;
	       strcpy(claFieldNames[i], clatemp);
	     }
	   } else if ( nReturn == SQL_ERROR ) {
	     break;
	   }
	 } // for columns
	 nRows++;
	 nReturn = SQLFetch( hStmt );
       } // while rows
     } else {
       fl_show_messages("Sorry, no Labels available for this mission!!");
     }
     SQLFreeStmt( hStmt, SQL_DROP );
     FillField(NULL);
   } else {
     j = fl_get_menu(fd_bmcintel->menu_mission) - 1;

     if (strcmp(CLAFileBase[j], "*") == 0) {
       strcpy(clatemp, fl_show_fselector("RQ Label file", "DSBFiles/", "*.rq", NULL));
       if (strlen(clatemp) == 0) return;
     } else {
       strcpy(clatemp, "DSBFiles/");
       strcat(clatemp, CLAFileBase[j]);
       strcat(clatemp, ".rq");
     } 
     if ((fp = fopen(clatemp, "r")) != NULL) {
       i = -1;
       j = -1;
       while (!feof(fp)) {
	 do fgets(clatemp, 128, fp); while ((clatemp[0] == '#') && !feof(fp));
	 if (feof(fp)) break;
	 sscanf(clatemp, "%c %s", &chflag, chtemp);
	 strsub(chtemp, '_', ' ');
	 if (chflag == '-') {
	   i++;
	   strcpy(claFieldNames[i], chtemp);
	 }
	 else if (chflag == '+') {
	   j++;
	   fl_set_object_label(fd_bmcintel->itel_frame[j], chtemp);
	 }
	 else if (chflag == '>') {
	   i++;
	   fl_set_object_label(fd_bmcintel->itel_field[i], chtemp);
	   strcpy(claFieldNames[i], chtemp);
	 }
       }
       fclose(fp);
     }
     if ((fp = fopen("NewEvid.lock", "r")) != NULL) {
       FillField(fp);
       fclose(fp);
     }
   }

   return;
}

void CLAtimingCB(FL_OBJECT *ob, long data)
{
 
   return;
}

void itelsubjectCB(FL_OBJECT *ob, long data)
{
 
   return;
}

void itelocationCB(FL_OBJECT *ob, long data)
{
 
   return;
}

void itelevidenceCB(FL_OBJECT *ob, long data)
{

   fl_set_input(fd_bmcintel->itel_field[1],
	        fl_get_choice_text(fd_bmcintel->itel_source));
   return;
}

void ClearFields()
{
int             i, k;

   for (i=4; i<claFieldCount-9; i++) {
     fl_set_input(fd_bmcintel->itel_field[i],    "?");
   }

   return;
}

void FillField(FILE *fp)
{
int             i, j, k, nCols;
int             noEOF = TRUE;
long            irc;
char            chname[1024];
char            *str;
time_t          clock;
struct tm       *loctime;
char            szSQL[32];
SQLINTEGER    	nCol            		= 0;
SQLCHAR		szColumn[MAX_DATA_WIDTH+20]	= "";	
SQLCHAR		szColumnName[MAX_DATA_WIDTH+1]	= "";	
SQLCHAR		szHdrLine[32001]		= "";	
SQLUINTEGER	nOptimalDisplayWidth            = 10;
SQLLEN		nIndicator			= 0;
SQLCHAR		szColumnValue[MAX_DATA_WIDTH+1]	= "";
SQLRETURN	nReturn;
SQLCHAR         szSepLine[32001] = "";
SQLSMALLINT     cols;
SQLLEN          nRows = 0;
SQLINTEGER      ret; 
//
//   Get all field values for form fill-in
//
   if (claMYSQL) {
     nRows = 0;
     sprintf(szSQL, "SELECT * FROM Evidence WHERE Hypothesis = ?;");
     ExecuteSQL( CLAhDbc, szSQL, cDelimiter, bColumnNames, bHTMLTable, nCols );
     fprintf(stderr, "SQL: %s returned %d\n", szSQL, nCols);
     //
     if ( nCols > 0 ) {
       nReturn = SQLFetch( hStmt );
       if ( nReturn == SQL_SUCCESS || nReturn == SQL_SUCCESS_WITH_INFO ) {
	 for( nCol = 1; nCol <= nCols; nCol++ ) {
	   nReturn = SQLGetData( hStmt, nCol, SQL_C_CHAR, (SQLPOINTER)szColumnValue,
			     sizeof(szColumnValue), &nIndicator );
	   szColumnValue[MAX_DATA_WIDTH] = '\0';
	   if ( nReturn == SQL_SUCCESS && nIndicator != SQL_NULL_DATA ) {
	     sprintf(chname, "%s", szColumnValue);
	     fprintf(stderr, "%s\n", szColumnValue);
	     if ((nCol > 4) && (nCol < 48)) {
	       fl_set_input(fd_bmcintel->itel_field[nCol-1], chname);
	     } else if ( nReturn == SQL_ERROR ) {
	       break;
	     }
	   }
	 } // for columns
	 nRows++;
	 nReturn = SQLFetch( hStmt );
       } // if row data
     } else {
       fl_show_messages("Sorry, all Evidence classified!!");
     }
     SQLFreeStmt( hStmt, SQL_DROP );
   } else {
     fscanf(fp, "%s ", chname);                // Get Case #
     if (feof(fp)) return;
     fscanf(fp, "%s ", chname);                // Get Hypothesis
     fl_set_input(fd_bmcintel->itel_field[1], chname);
     fscanf(fp, "%s ", chname);                // Get Belief
     fscanf(fp, "%s ", chname);                // Get Disbelief

     for (i=4; i<claFieldCount-9; i++) {
       fscanf(fp, "%s ", chname);
       fl_set_input(fd_bmcintel->itel_field[i], chname);
     }

     fscanf(fp, "%s ", chname);                // Get Unused
     fscanf(fp, "%s ", chname);                // Get Start Time
     fscanf(fp, "%s ", chname);                // Get End Time
     fscanf(fp, "%s ", chname);                // Get Duration
     fscanf(fp, "%s ", chname);                // Get Time Frame
     fscanf(fp, "%s ", chname);                // Get Info ID
     fl_set_input(fd_bmcintel->itel_field[48], chname);
     fscanf(fp, "%s ", chname);                // Get Preparer
     fl_set_input(fd_bmcintel->itel_field[49], chname);
     fscanf(fp, "%s ", chname);                // Get (or set) Prepared Date
     if (strlen(chname) < 2) {
       time(&clock);
       loctime = gmtime(&clock);
       str = asctime(loctime);
       fl_set_input(fd_bmcintel->itel_field[50], str);
     } else {
       fl_set_input(fd_bmcintel->itel_field[50], chname);
     }
     //fscanf(fp, "%s\n", chname);               // Get Mission
     //fl_set_input(fd_bmcintel->itel_field[51], chname);
   }

   return;
}

void ProcessField(FILE *fp, const char *str)
{
   if ((str == NULL) || (strcmp(str, " ") == 0)) {
     strcpy(clatemp, "?");
   } else {
     strcpy(clatemp, str);
     strsub(clatemp, ' ', '_');
   }
   fprintf(fp, "%s ", clatemp);

   return;
}

void clabuttonCB(FL_OBJECT *ob, long data)
{
int             i, j, k, m;

   for (i=0; i<claFieldCount-4; i++) {
     fl_set_object_label(fd_clafields->itel_fldselect[i], claFieldNames[i]);
     if ((strlen(claFieldNames[i]) > 1) && (strcmp(claFieldNames[i], "N/A") != 0))
       fl_set_button(fd_clafields->itel_fldselect[i], PUSHED);
   }
   fl_set_object_label(fd_clafields->itel_fldselect[48], claFieldNames[48]);
   fl_set_object_label(fd_clafields->itel_fldselect[49], claFieldNames[49]);
   fl_set_object_label(fd_clafields->itel_fldselect[50], claFieldNames[50]);
   fl_set_object_label(fd_clafields->itel_fldselect[51], claFieldNames[51]);

   fl_set_button(fd_clafields->itel_fldselect[00], FALSE);
   fl_set_button(fd_clafields->itel_fldselect[02], FALSE);
   fl_set_button(fd_clafields->itel_fldselect[03], FALSE);
   fl_set_button(fd_clafields->itel_fldselect[48], FALSE);
   fl_set_button(fd_clafields->itel_fldselect[49], FALSE);
   fl_set_button(fd_clafields->itel_fldselect[50], FALSE);
   fl_set_button(fd_clafields->itel_fldselect[51], FALSE);

   fl_clear_browser(fd_clafields->cla_classes);
   fl_set_input(fd_clafields->cla_closest, " ");
   fl_show_form(fd_clafields->clafields, FL_PLACE_CENTER,FL_FULLBORDER,
		"Field Select");

   return;
}

void claclassCB(FL_OBJECT *object, long item_no)
{
FILE            *outfp;
FILE            *grafp;
FILE            *lnkfp;
FILE            *clafp;
int             TABFILE = FALSE;
int             item = item_no;
int             i, j, k, nCols;
int             noEOF = TRUE;
int             Vn, Vtop;
long            irc;
float           P;
const char      *fname;
char            chtemp[1024];
char            chname[64];
char            chclass[64];
char            szSQL[9001];
SQLINTEGER    	nCol            		= 0;
SQLCHAR		szColumn[MAX_DATA_WIDTH+20]	= "";	
SQLCHAR		szColumnName[MAX_DATA_WIDTH+1]	= "";	
SQLCHAR		szHdrLine[32001]		= "";	
SQLUINTEGER	nOptimalDisplayWidth            = 10;
SQLLEN		nIndicator			= 0;
SQLCHAR		szColumnValue[MAX_DATA_WIDTH+1]	= "";
SQLRETURN	nReturn;
SQLCHAR         szSepLine[32001] = "";
SQLSMALLINT     cols;
SQLLEN          nRows = 0;
SQLINTEGER      ret; 

   switch (item) {

     case 1: // Field Selection
       //
       //   Process 'Classify' request
       //   --------------------------
       //
       //   Run 'Autoclass'
       //
       //sprintf(chtemp, "Classifing evidence using Autoclass");
       //fl_set_input(fd_claster->message, chtemp);
       //
       //   Buid the (2) parameter files
       //
       outfp = fopen("./CLASSIFY/rq-train.r-params", "w");
       fprintf(outfp, "# OVERRIDE PARAMETERS TO AUTOCLASS-REPORTS  -- AutoClass C\n");
       fprintf(outfp, "#\n");
       fprintf(outfp, "xref_class_report_att_list = 1, 2, 3\n");
       fprintf(outfp, "break_on_warnings_p = false\n");
       fprintf(outfp, "report_mode = \"data\"\n");
       fclose(outfp);
       //
       outfp = fopen("./CLASSIFY/rq-train.s-params", "w");
       fprintf(outfp, "# OVERRIDE PARAMETERS TO AUTOCLASS-SEARCH  -- AutoClass C\n");
       fprintf(outfp, "#\n");
       fprintf(outfp, "break_on_warnings_p = false\n");
       fprintf(outfp, "max_tries = 4\n");
       fprintf(outfp, "fixed_j = 4\n");
       fprintf(outfp, "force_new_search_p = false\n");
       fprintf(outfp, "min_report_period = 30000\n");
       fclose(outfp);
       //
       //   Build the header file
       //
       outfp = fopen("./CLASSIFY/rq-train.hd2", "w");
       fprintf(outfp, "# AutoClass C Header file -- extension .hd2\n");
       fprintf(outfp, "#\n");
       fprintf(outfp, "num_db2_format_defs 2\n");
       fprintf(outfp, "number of attributes %d\n", claFieldCount);
       fprintf(outfp, "separator_char ' '\n");
       //
       fprintf(outfp, "#\n");
       for (i=0; i<claFieldCount; i++) {
	 strcpy(clatemp, claFieldNames[i]);
	 if (fl_get_button(fd_clafields->itel_fldselect[i]) == PUSHED)
	   fprintf(outfp, "%d discrete nominal \"%s\" range 10\n", i, clatemp);
	 else
	   fprintf(outfp, "%d dummy nil \"%s\"\n", i, clatemp);	   
       }
       fclose(outfp);
       //
       //   Build the test data file
       //
       outfp = fopen("./CLASSIFY/rq-test.db2", "w");
       fprintf(outfp, "# AutoClass C Test Data file -- extension .db2\n");
       fprintf(outfp, "#\n");
       fprintf(outfp, "%d ", -1);
       //
       for (i=1; i<claFieldCount; i++) {
	 strcpy(clatemp, claFieldNames[i]);
	 strsub(clatemp, ' ', '_');
	 fprintf(outfp, "%s ", clatemp);	   
       }
       fprintf(outfp, "\n");
       fclose(outfp);
       //
       //   Build the training data file
       //
       if (claMYSQL) {
	 outfp = fopen("./CLASSIFY/rq-train.db2", "w");
	 fprintf(outfp, "# AutoClass C Training Data file -- extension .db2\n");
	 fprintf(outfp, "#\n");
	 //
	 nRows = 0;
	 sprintf(szSQL, "SELECT * FROM Evidence WHERE Hypothesis != \"?\";");
         ExecuteSQL( CLAhDbc, szSQL, cDelimiter, bColumnNames, bHTMLTable, nCols );
	 //
	 if ( nCols > 0 ) {
	   nReturn = SQLFetch( hStmt );
	   while ( nReturn == SQL_SUCCESS || nReturn == SQL_SUCCESS_WITH_INFO ) {
	     for( nCol = 1; nCol <= nCols; nCol++ ) {
	       nReturn = SQLGetData( hStmt, nCol, SQL_C_CHAR, (SQLPOINTER)szColumnValue,
				   sizeof(szColumnValue), &nIndicator );
	       szColumnValue[MAX_DATA_WIDTH] = '\0';
	       if ( nReturn == SQL_SUCCESS && nIndicator != SQL_NULL_DATA ) {
		 sprintf(clatemp, "%s", szColumnValue);
		 strsub(clatemp, ' ', '_');
		 fprintf(outfp, "%s ", clatemp);
	       } else if ( nReturn == SQL_ERROR ) {
		 break;
	       }
	     } // for columns
	     fprintf(outfp, "\n");
	     nRows++;
	     nReturn = SQLFetch( hStmt );
	   } // while rows
	 } else {
	   fl_show_messages("Sorry, no Evidence available for this mission!!");
	 }
	 SQLFreeStmt( hStmt, SQL_DROP );
	 fclose(outfp);
       } else {

       }
       //
       //   Build the AutoClass command line
       //   
       sprintf(clatemp, "Classify.sh %s %s", "rq-train", "rq-test");

       irc = fl_exe_command(clatemp, 1);

       if (irc != 0) {
	 fl_show_messages("Classifying Error - See Command Log for error code");
	 fprintf(stderr, " Return code of Autoclass is %d\n", irc);
       }
       //
       //   Get all the possible classess (hypothesis)
       //
       if ((clafp = fopen("./CLASSIFY/rq-train.class-data-1", "r")) == NULL) return;

       noEOF = TRUE;
       while (noEOF) {
	 fgets(clatemp, 1024, clafp);
	 if (feof(clafp)) {
	   noEOF = FALSE;
	   break;
	 }
	 if (strstr(clatemp, "DATA_CLASS") != NULL) {
	   sscanf(clatemp, "%s %d", chname, &k);
	   fgets(clatemp, 1024, clafp);
	   fgets(clatemp, 1024, clafp);
	   fgets(clatemp, 1024, clafp);
	   fgets(clatemp, 1024, clafp);
	   fgets(clatemp, 1024, clafp);
	   sscanf(clatemp, "%s %s", chname, chclass);
	   strcpy(clatemp, fl_get_choice_item_text(fd_bmcintel->itel_source, k+1));
	   strsub(clatemp, ' ', '_');
	   sprintf(chtemp, "    %-10d %-24s", k, clatemp);
	   fl_addto_browser(fd_clafields->cla_classes, chtemp);
	 }
       }
       fclose(clafp);
       //
       //   Get the closest match
       //
       if ((clafp = fopen("./CLASSIFY/rq-test.case-data-1", "r")) == NULL) return;

       noEOF = TRUE;
       while (noEOF) {
	 fgets(clatemp, 1024, clafp);
	 if (feof(clafp)) {
	   noEOF = FALSE;
	   break;
	 }
	 if (strstr(clatemp, "DATA_CASE") != NULL) {
	   fgets(clatemp, 1024, clafp);
	   fgets(clatemp, 1024, clafp);
	   sscanf(clatemp, "%s %d %f", chname, &k, &P);// Get case ID, Class #, Probability
	   fl_select_browser_line(fd_clafields->cla_classes, k+1);
	   sscanf(fl_get_browser_line(fd_clafields->cla_classes, k+1),
		  "%d %s", &i, chtemp);
	   fl_set_input(fd_clafields->cla_closest, chtemp);
	   sprintf(clatemp, "%5.1f%%", P*100.0);
	   fl_set_input(fd_clafields->cla_percent, clatemp);
	 }
       }
       fclose(clafp);
       break;

     case 12: // Select All
       for (i=0; i<claFieldCount; i++) {
	 fl_set_button(fd_clafields->itel_fldselect[i], PUSHED);
       }
       break;

     case 13: // Deselect All
       for (i=0; i<claFieldCount; i++) {
	 fl_set_button(fd_clafields->itel_fldselect[i], 0);
       }
       break;

     default:
       break;
   }       
}

void cladoneCB(FL_OBJECT *ob, long data)
{
int             irc;

   fl_hide_form(fd_clafields->clafields);

   if (strlen(fl_get_input(fd_clafields->cla_closest)) > 1) {
     strcpy(clatemp, "Classifier determined hypothesis\n\n");
     strcat(clatemp, fl_get_input(fd_clafields->cla_closest));
     strcat(clatemp, "\n\n to be closest match.\n\nAccept?");
     irc = fl_show_question(clatemp, 1);
     if (irc == 1) {
       fl_set_input(fd_bmcintel->itel_field[1],
		    fl_get_input(fd_clafields->cla_closest));
     }
   }
}

void itelmissionCB(FL_OBJECT *ob, long data)
{
FILE            *outfp;
int             i, j, k, m;
const char      *fname;
char            chtemp[128];
char            chname[64];
char            chclass[64];
//
//   Get all node names for the selected mission
//
/*
   if (data == 1) 
     fl_set_input(fd_bmcintel->itel_field[51],
		  fl_get_choice_text(fd_bmcintel->itel_mission));
  
   j = fl_get_choice(fd_bmcintel->itel_mission) - 1;

   if (strcmp(iteltypes[j].filebase, "*") == 0) {
     strcpy(chtemp, fl_show_fselector("Node file", "DSBFiles/", "*.node", NULL));
     if (strlen(chtemp) == 0) return;
   } else {
     strcpy(chtemp, "DSBFiles/");
     strcat(chtemp, iteltypes[j].filebase);
     strcat(chtemp, ".node");
   }

   fl_clear_choice(fd_bmcintel->itel_source);

   if ((outfp = fopen(chtemp, "r")) == NULL) return;
   //do fgets(clatemp, 128, outfp); while ((clatemp[0] == '#'));
   fscanf(outfp, "%d %s", &k, chclass);
   for (i=0; i<k; i++) {
     //do fgets(clatemp, 128, outfp); while ((clatemp[0] == '#'));
     fscanf(outfp, "%d %s", &m, chclass);
     for (j=0; j<m; j++) {
       //do fgets(clatemp, 128, outfp); while ((clatemp[0] == '#'));
       fscanf(outfp, "%s", chname);
       strsub(chname, '_', ' ');
       fl_addto_choice(fd_bmcintel->itel_source, chname);
    }
   }
   fclose(outfp);
*/
}

void clanodeCB(FL_OBJECT *object, long item_no)
{
FILE            *fp, *typefp;
int             item = item_no;
int             i, j, k;
int             num_types;
char            mission[64]; 


   switch (item) {
     case 0: // Open 
       typefp = fopen("DSBinit.dat", "r");
       do fgets(clatemp, 128, typefp); while (clatemp[0] == '#');    // Read # missions
       sscanf(clatemp, "%d", &num_types);

       for (j=0; j<num_types; j++) {
	 do fgets(clatemp, 128, typefp); while (clatemp[0] == '#');  // Read assessment type
	 sscanf(clatemp, "%s", mission);
	 strsub(mission, '_', ' ');
       }
       fclose(typefp);
       break;

     case 9: // Done
       break;

     default:
       break;
   }

   return;
}

extern "C" { 
void
SC_intel_time(int day, float hours)
{
char   chline[16];
/*
   sprintf(chline, "%d", day);
   fl_set_input(fd_bmcintel->intel_day, chline);
   sprintf(chline, "%f", hours);
   fl_set_input(fd_bmcintel->intel_hour, chline);
*/     
}

void
SC_intel_text(char *str)
{
 
   fl_clear_browser(fd_bmcintel->intel_browser);
   fl_add_browser_line(fd_bmcintel->intel_browser, str);
}

void
SC_intel_src(char *chline)
{
   fl_clear_browser(fd_bmcintel->intel_source);   
   fl_addto_browser(fd_bmcintel->intel_source, chline);
}
}  // "C" linkages 
