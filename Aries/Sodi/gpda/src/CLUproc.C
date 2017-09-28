#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h> 
#include <time.h>
#include <signal.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <stream.h>
#include <sys/ioctl.h>
#ifdef SC_THREADS
#include "pthread.h"
#endif

#include "Globals.h"
#include "isql.h"

#include "forms.h"
#include "CLUforms.h"
#include "DSBalgo.h"

#define PUSHED  1
#define NPARMS  29

/* --------------------------------------------------------------------- */

typedef struct {
  int           Vn;
  int           nlinks;
  int           select;
  int           links[6];
  char          name[32];
  char          value[32];
} FIELD;

typedef struct {
  int           pflag;                  // 0=>No arg, -1=>int arg, +1=>float arg
  char          chvalue[16];            // Parameter value
  char          chparam[16];            // Parameter name
} PARAMETER;

/* --------------------------------------------------------------------- */

int             CLUwinX, CLUwinY;
int             CLUwinW, CLUwinH;
char            CLUlabel[32];
char            CLUFileBase[64][64];
Window          CLUwinid;

SQLHENV         CLUhEnv  = 0;
SQLHDBC         CLUhDbc  = 0;
int             cluMYSQL = 0;
extern int      bHTMLTable;
extern int      bBatch;
extern int      cDelimiter;
extern int      bColumnNames;
extern SQLHSTMT hStmt;

FILE            *clufp;
int             cluFileOpen = FALSE;
int             n_Pushed, n_Ids, n_Fields, n_Recs;
FIELD           fields[52];
PARAMETER       params[NPARMS] = {
                  { -1,  "0.000",  "limit"},      { -1,  "0.000",  "iterations"},
                  {  1,  "0.000",  "threshold"},  { -1,  "0.000",  "nsubs"},
                  {  0,  "0.000",  "prune"},      { -1,  "0.000",  "prune2"},
                  { -1,  "0.000",  "beam"},       {  0,  "0.000",  "overlap"},
                  {  0,  "0.000",  "undirect"},   {  1,  "0.000",  "con"},
                  {  1,  "0.000",  "com"},        {  1,  "0.000",  "cov"},
                  {  0,  "0.000",  "alt"},        { -1,  "0.000",  "ps"},
                  { -1,  "0.000",  "size"},       { -1,  "0.000",  "output"},
                  { -1,  "0.000",  "nproc"},      { -1,  "0.000",  "plot"},
                  {  0,  "0.000",  "oldeval"},    {  0,  "0.000",  "savesub"},
                  {  0,  "0.000",  "cluster"},    {  0,  "0.000",  "truelabel"},
                  {  0,  "0.000",  "exhaust"},    {  0,  "0.000",  "supervided"},
                  {  0,  "0.000",  "scratch"},    { -1,  "0.000",  "numpos"},
                  {  1,  "0.000",  "minpercent"}, {  0,  "0.000",  "negweight"},
                  {  0,  "0.000",  "display"} }; 

char            clutemp[1024];
char            clufname[64];
char            clu_gfn[64];
char            clu_tfn[64];

int             cluFieldCount = 52;
char            cluFieldNames[52][16] = {
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

FD_itelfields   *fd_itelfields;
FD_defnode      *fd_defnode;

/* --------------------------------------------------------------------- */

void CLUinit();
void CLUshow(int xpos, int ypos, int width, int height, Window mainwinID);
void CLUexitCB(FL_OBJECT *object, long item_no);
int  CLUclose(FL_FORM *form, void *data);
void CLUnoneCB(FL_OBJECT *ob, long data);

extern int  EraseActiveEntry(char *text);
extern int  StoreActiveEntry(char *text);
extern int  FinishUp();

/*                                                                       */
/* --------------------------------------------------------------------- */
/*              M A I N   P R O G R A M   S T A R T S   H E R E          */
/* --------------------------------------------------------------------- */
/*                                                                       */
void CLUinit()
{
int             i;

   fd_itelfields = create_form_itelfields();
   fd_defnode = create_form_defnode();

   strcpy(CLUlabel, "Subdue-Clustering");

   return;
}

void CLUshow(int xpos, int ypos, int width, int height, Window mainwinID)
{
FILE            *typefp;
FILE            *fp;
int             i, j, nCols, num_types;
const char      *fname;
char            chflag;
char            chtemp[1024];
char            chname[64];
char            chclass[64];

char            szDSN[MAX_DATA_WIDTH+1] = "MySQL";
char            szUID[MAX_DATA_WIDTH+1] = "root";
char            szPWD[MAX_DATA_WIDTH+1] = "gpda";
char            szSQL[9001]             = "show databases;";
char            chcase[32];
char            chvalue[64];
char            chfile[128];
Window          winid;

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

   if(!fl_form_is_visible(fd_itelfields->itelfields) ) {
      fl_transient();
      fl_winposition(xpos, ypos);
      fl_initial_winsize(width, height);
      CLUwinid = fl_prepare_form_window(fd_itelfields->itelfields,
                                 FL_PLACE_POSITION,FL_TRANSIENT, CLUlabel);
      fl_winreshape(CLUwinid, xpos, ypos, width, height);
      fl_get_wingeometry(CLUwinid, &CLUwinX, &CLUwinY, &CLUwinW, &CLUwinH);
      CLUwinX = CLUwinX;
      CLUwinY = CLUwinY;
      fl_show_form_window(fd_itelfields->itelfields);
      fl_set_form_atclose(fd_itelfields->itelfields, CLUclose, 0);
      fl_set_form_title(fd_itelfields->itelfields, CLUlabel);
      StoreActiveEntry(CLUlabel);
   }

   fl_hide_object(fd_itelfields->cluapply);

   fl_clear_menu(fd_itelfields->menu_mission);

   if (cluMYSQL) {
     //
     //   Open the Database for this Domain
     //
     if ( !OpenDatabase( &CLUhEnv, &CLUhDbc, szDSN, szUID, szPWD ))
       exit(-1);
     //
     //   Find out which mission databases are available
     //
     nRows = 0;
     strcpy(szSQL, "show databases;");
     ExecuteSQL( CLUhDbc, szSQL, cDelimiter, bColumnNames, bHTMLTable, nCols );
     if ( nCols > 0 ) {
       nReturn = SQLFetch( hStmt );
       while ( nReturn == SQL_SUCCESS || nReturn == SQL_SUCCESS_WITH_INFO ) {
	 for( nCol = 1; nCol <= nCols; nCol++ ) {
	   nReturn = SQLGetData( hStmt, nCol, SQL_C_CHAR, (SQLPOINTER)szColumnValue,
			       sizeof(szColumnValue), &nIndicator );
	   szColumnValue[MAX_DATA_WIDTH] = '\0';
	   if ( nReturn == SQL_SUCCESS && nIndicator != SQL_NULL_DATA ) {
	     sprintf(clutemp, "%s", szColumnValue);
	     if ( (strstr(clutemp, "test") == NULL) && (strstr(clutemp, "mysql") == NULL) )
	       fl_addto_menu(fd_itelfields->menu_mission, clutemp);
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
     do fgets(clutemp, 128, typefp); while (clutemp[0] == '#');    // Read # assessments
     sscanf(clutemp, "%d", &num_types);
     for (j=0; j<num_types; j++) {
       do fgets(clutemp, 128, typefp); while (clutemp[0] == '#');
       //
       //   Read assessment type and file base names
       //
       sscanf(clutemp, "%s %s", chvalue, CLUFileBase[j]);
       strsub(chvalue, '_', ' ');
       fl_addto_menu(fd_itelfields->menu_mission, chvalue);
     }
     fclose(typefp);
   }

   for (i=0; i<cluFieldCount; i++) {
     fl_set_object_label(fd_itelfields->itel_fldselect[i], cluFieldNames[i]);
   }

   strcpy(clu_gfn, "SubdueTemp.g");                    // Define the graph output file
   strcpy(clu_tfn, "Intel.tab");                       // Define the input file

   if ((clufp = fopen(clu_tfn, "r")) == NULL) {        // Open the input file, if possible
     printf("Subdue - Input file not found: %s\n", clu_tfn);
     return;
   }
   fscanf(clufp, "%s %d\n", chclass, &n_Fields);       // Get Mission domain name & # fields
   fl_set_input(fd_itelfields->clu_classfn, chclass);  // Tell user

   typefp = fopen("GPDAinfo.log", "a");
   fprintf(typefp, "Hypothesis Generator algorithm initialization complete.\n");
   fclose(typefp);

   return;
}
void CLUexitCB(FL_OBJECT *object, long item_no)
{
int             item = item_no;

   if (cluMYSQL) CloseDatabase( CLUhEnv, CLUhDbc );

   fl_hide_form(fd_itelfields->itelfields);
   EraseActiveEntry(CLUlabel);

   FinishUp();

   return;
}

int CLUclose(FL_FORM *form, void *data)
{
long            item;

   item = (long)data;
   CLUexitCB(NULL, item);

   return(0);
}

void CLUnoneCB(FL_OBJECT *ob, long data)
{
   return;
}

/*                                                                       */
/* --------------------------------------------------------------------- */
/*      C A L L B A C K    R O U T I N E S    S T A R T    H E R E       */
/* --------------------------------------------------------------------- */
/*                                                                       */
static void dismiss_alert(int ID, void *data)
{
   fl_hide_message();
}

void show_timed_alert(const char *s1, const char *s2, const char *s3, int c)
{
  fl_add_timeout(3000, dismiss_alert, 0);
  fl_show_messages(s1);
}
 
void cluviewCB(FL_OBJECT *object, long item_no)
{
int             item = item_no;
int             irc;
char            filename[64];                                                                 

   if (strcmp(clu_gfn, "\0") != 0) {
     sprintf(clutemp, "dotty %s.dot", clu_gfn);
     irc = fl_exe_command(clutemp, 1);
   }

   return;
}

void cluclassCB(FL_OBJECT *object, long item_no)
{
int             TABFILE = FALSE;
int             item = item_no;
FILE            *outfp;
FILE            *grafp;
FILE            *lnkfp;
FILE            *fp;
int             i, j, k, n, level, nCols;
int             noEOF = TRUE;
int             Vn, Vtop;
int             ipath, Ke;
int             nodefirst[10];
int             linktable[10];
long            irc;
float           results[6];
const char      *fname;
char            chflag;
char            chtemp[1024];
char            chname[64];
char            chclass[64];
char            In_String[64];   /* Out - Parent node name                            */
char            Out_String[64];  /* Out - Child node name                             */
char            B_String[64];    /* Out - String describing belief value              */
char            D_String[64];    /* Out - String describing belief value              */
float           B_Weight;        /* Out - Line-source belief value                    */
float           D_Weight;        /* Out - Line-source disbelief value                 */

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

   switch (item) {
     case 0: // 'Apply' Field Selections
       strcpy(chclass, fl_get_input(fd_itelfields->clu_classfn));
       //strcpy(clutemp, fl_get_menu_text(fd_itelfields->menu_mission));
       //strsub(clutemp, ' ', '_');
       //strcat(clutemp, ".g");
       //strcpy(clu_gfn, clutemp);
       grafp = fopen(clu_gfn, "w");                  // Open the directed graph file (.g)
       //
       //   Use only those fields selected
       //
       n_Ids = 0;
       for (i=0; i<cluFieldCount; i++) {
	 strcpy(fields[i].name, fl_get_object_label(fd_itelfields->itel_fldselect[i]));
	 strsub(fields[i].name, ' ', '-');
	 fields[i].select = FALSE;
	 if (fl_get_button(fd_itelfields->itel_fldselect[i]) == PUSHED) {
	   fields[i].select = TRUE;
	   n_Ids++;
	 }
       }
       //
       /*
       sprintf(clutemp, "./DSBFiles/%s.dsbn", chclass);
       DS_AlgoInit(clutemp, 0);
       if ((fp = fopen("SubdueTemp.pre", "w")) == NULL) return;
       k = 0;
       ipath = 0;
       nodefirst[0] = 1;
       nodefirst[1] = 2;
       for (level=0; level<DS_AlgoGetTreeDepth(); level++) {
	 nodefirst[level+2] = nodefirst[level+1] + DS_AlgoGetLevelWidth(level);
         for (i=0; i<DS_AlgoGetLevelWidth(level); i++) {
	   k++;
	 }
       }
       Ke = k+2;
       //
       //   Take care of the non-existent top-level node.
       //
       fprintf(fp, "%% %d            # No. of nodes\n", Ke-1);
       fprintf(fp, "v %d %s\n", 1, chclass);
       //
       //   Take care of the nodes on levels 1..'depth'.
       //
       k = 1;
       for (level=0; level<DS_AlgoGetTreeDepth(); level++) {
         for (i=0; i<DS_AlgoGetLevelWidth(level); i++) {
	   k++;
	   DS_AlgoGetNodeName(i, level, clutemp);
	   strsub(clutemp, ' ', '-');
           fprintf(fp, "v %d %s\n", k, clutemp);
	 }
       }
       //
       //   Take care of the non-existent top-level edges by connecting
       //   them up with each node on the 1st level of the network.
       //
       for (i=0; i<DS_AlgoGetLevelWidth(0); i++) fprintf(fp, "e %d %d %s\n", 1, i+2, "Dummy");
       //
       //   Take care of the nodes on levels 1..'depth' by connecting
       //   them with nodes on the next level of the network.
       //
       k = 1;
       for (level=0; level<DS_AlgoGetTreeDepth()-1; level++) {
         for (i=0; i<DS_AlgoGetLevelWidth(level); i++) {
	   k++;
	   n = 0;
           for (j=0; j<DS_AlgoGetLevelWidth(level+1); j++) {
             DS_AlgoGetMatrix(i, level, j, level+1, In_String, Out_String,
			    B_String, D_String, B_Weight, D_Weight);
             if (B_Weight > 0.0) { linktable[n] = nodefirst[level+2]+j; n++; }
	   }
           for (j=0; j<n; j++) {
	     fprintf(fp, "e %d %d %s\n", k, linktable[j], "Causes");
	   }
	 }
       }
       //
       fclose(fp);
       */
       //
       Vn = 0;
       n_Recs = -1;
       noEOF = TRUE;
       fprintf(stderr, "Classifing file %s using Simple Hierarchy\n", clu_tfn);
       //
       //   Process next record until EOF
       //
       while (noEOF) {
	 n_Recs++;
	 //
	 //   Process next record of 'n_Fields' fields
	 //
	 fprintf(grafp, "%% ------------ Record  %d -------------\n", n_Recs+1);
	 Vn++; fprintf(grafp, "v %d  %s\n", Vn, chclass); Vtop = Vn;

	 for (i=0; i<cluFieldCount; i++) {
	   fscanf(clufp, "%s ", chname);             // Get field value
	   strcpy(fields[i].value, chname);          // Save for duration of record processing
	   if (feof(clufp)) {
	     noEOF = FALSE;
	     break;
	   }
	   if (fields[i].select) { 
	     //
	     //   Build the graph vertices for the fields in this record
	     //
	     Vn++;
	     fields[i].Vn = Vn;
	     fprintf(grafp, "v %d  %s\n", Vn, fields[i].value);
	   }
	 } // -- end record processing --
	 //
	 //   Build the graph edges for the fields that are selected in this record
	 //
	 for (i=0; i<n_Fields; i++) {
	   /*	     
	     printf("-- Field name ......... %s\n", fields[i].name);
	     printf("-- Field value ........ %s\n", fields[i].value);
	     printf("-- Field number ....... %d [%d]\n", i, n_Recs);
	     printf("-- Field selected ..... %d\n", fields[i].select);
	     printf("-- Field node # ....... %d\n", fields[i].Vn);
	     printf("-- Field links ........ %d\n", fields[i].nlinks);
	     if (fields[i].nlinks > 0) {
	       for (j=0; j<fields[i].nlinks; j++)
	         printf("   Link[%d] ....... %d\n", j, fields[i].links[j]);
	     }
	     printf("\n");
	     */	     
	   if (fields[i].select) {
	     /* **********************************************************************
		if (fields[i].nlinks > 0) {
		 for (j=0; j<fields[i].nlinks; j++)
		   fprintf(grafp, "e %d %d %s\n",
			   fields[fields[i].links[j]-1].Vn, fields[i].Vn, fields[i].name);
	       }
	       *********************************************************************** */
	     fprintf(grafp, "e %d %d %s\n", Vtop, fields[i].Vn, fields[i].name);
	   }
	 }
	 if (feof(clufp)) break;
       } // -- end while --
       //
       fclose(clufp); cluFileOpen = FALSE;
       fclose(grafp);
       //
       sprintf(clutemp, "Class name ..... %s\nNo. Records .... %d\nFields/Record .. %d",
		 chclass, n_Recs, n_Ids);
       fl_show_messages(clutemp);
       //
       //   Run Subdue
       //   ----------
       //
       fprintf(stderr, "Doing cluster analysis using graph file %s\n", clu_gfn);

       sprintf(clutemp, "Subdue -cluster -truelabel %s", clu_gfn);
       fl_set_cursor(CLUwinid, XC_watch);
       irc = fl_exe_command(clutemp, 1);
       fl_reset_cursor(CLUwinid);

       if (irc != 0) {
	 fl_show_messages("Clustering Error - See Command Log for error code");
	 fprintf(stderr, " Return code of Subdue is %d\n", irc);
	 fl_show_command_log(FL_FULLBORDER);
       }

       if (strcmp(clu_gfn, "\0") != 0) {
	 sprintf(clutemp, "dotty %s.dot", clu_gfn);
	 irc = fl_exe_command(clutemp, 1);
       }
       break;

     case 12: // Select All
       for (i=0; i<cluFieldCount; i++) {
	 fl_set_button(fd_itelfields->itel_fldselect[i], PUSHED);
       }
       break;

     case 13: // Deselect All
       for (i=0; i<cluFieldCount; i++) {
	 fl_set_button(fd_itelfields->itel_fldselect[i], 0);
       }
       break;

     default:
       break;
   }                                                         

   return;
}

void clunodeCB(FL_OBJECT *object, long item_no)
{
FILE            *fp, *typefp;
int             item = item_no;
int             i, j, k;
int             num_types;
char            mission[64]; 


   switch (item) {
     case 0: // Open 
       typefp = fopen("DSBinit.dat", "r");
       do fgets(clutemp, 128, typefp); while (clutemp[0] == '#');    // Read # missions
       sscanf(clutemp, "%d", &num_types);

       for (j=0; j<num_types; j++) {
	 do fgets(clutemp, 128, typefp); while (clutemp[0] == '#');  // Read assessment type
	 sscanf(clutemp, "%s", mission);
	 strsub(mission, '_', ' ');
	 fl_addto_choice(fd_defnode->clu_mission, mission);
       }
       fclose(typefp);

       fl_set_input(fd_defnode->clu_nodename, "New Tactical Obj");

       fl_show_form(fd_defnode->defnode,FL_PLACE_CENTER,FL_FULLBORDER,"Node Definition");
       break;

     case 9: // Done
       if ((fp = fopen("NewNode.lock", "w")) != NULL) {
	 strcpy(clutemp, fl_get_choice_text(fd_defnode->clu_mission));
	 strsub(clutemp, ' ', '_');
	 fprintf(fp, "%s", clutemp);
	 strcpy(clutemp, fl_get_input(fd_defnode->clu_nodename));
	 strsub(clutemp, ' ', '_');
	 fprintf(fp, "    %s\n", clutemp);
	 fclose(fp);
       }

       fl_hide_form(fd_defnode->defnode);
       break;

     default:
       break;
   }

   return;
}

void cluresetCB(FL_OBJECT *object, long item_no)
{
int             item = item_no;                                                        

   strcpy(clufname, "\0");
   strcpy(clu_gfn,  "\0");

   //fl_set_input(fd_cluster->message, " ");

   return;
}

void cluparamCB(FL_OBJECT *object, long item_no)
{
int             item = item_no;                                                        

   return;
}

void cluvalueCB(FL_OBJECT *object, long item_no)
{
int             item = item_no;
int             i; 

//0=>No arg, -1=>int arg, +1=>float arg

   i = item_no - 1;
   /*
   if (params[i].pflag == 1)
     sprintf(params[i].chvalue, "%f",
	 (float)fl_get_counter_value(fd_cluster->cluvalue[item_no]));
   else
     sprintf(params[i].chvalue, "%d",
	 (int)fl_get_counter_value(fd_cluster->cluvalue[item_no]));

   //fprintf(stderr, " %d %d %s %s\n", item_no, i, params[i].chparam, params[i].chvalue);
   */
   return;
}

void clumissionCB(FL_OBJECT *object, long item_no)
{
FILE            *fp;
int             item = item_no;
int             i, j, nCols;
char            chflag;
char            chtemp[64];
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

   if (cluMYSQL) {
     //
     //   Use selected mission
     //
     i = fl_get_menu(fd_itelfields->menu_mission);
     sprintf(szSQL, "use %s;", fl_get_menu_text(fd_itelfields->menu_mission));

     ExecuteSQL( CLUhDbc, szSQL, cDelimiter, bColumnNames, bHTMLTable, nCols );
     //
     //   Get the field labels
     //
     i = -1;
     nRows = 0;
     strcpy(szSQL, "SELECT * FROM Labels;");
     ExecuteSQL( CLUhDbc, szSQL, cDelimiter, bColumnNames, bHTMLTable, nCols );
     if ( nCols > 0 ) {
       nReturn = SQLFetch( hStmt );
       while ( nReturn == SQL_SUCCESS || nReturn == SQL_SUCCESS_WITH_INFO ) {
	 for( nCol = 1; nCol <= nCols; nCol++ ) {
	   nReturn = SQLGetData( hStmt, nCol, SQL_C_CHAR, (SQLPOINTER)szColumnValue,
			       sizeof(szColumnValue), &nIndicator );
	   szColumnValue[MAX_DATA_WIDTH] = '\0';
	   if ( nReturn == SQL_SUCCESS && nIndicator != SQL_NULL_DATA ) {
	     sprintf(clutemp, "%s", szColumnValue);
	     strsub(clutemp, '_', ' ');
	     chflag = clutemp[0];
	     clutemp[0] = ' ';
	     if ((chflag == '>') || (chflag == '-')) {
	       i++;
	       strcpy(cluFieldNames[i], clutemp);
	       fl_set_object_label(fd_itelfields->itel_fldselect[i], cluFieldNames[i]);
	       if (strlen(cluFieldNames[i]) > 1)
		 fl_set_button(fd_itelfields->itel_fldselect[i], PUSHED);
	       else
		 fl_set_button(fd_itelfields->itel_fldselect[i], FALSE);
	     }
	   } else if ( nReturn == SQL_ERROR ) {
	     break;
	   }
	 } // for columns
	 nRows++;
	 nReturn = SQLFetch( hStmt );
       } // while rows
       fl_redraw_form(fd_itelfields->itelfields);
     } else {
       fl_show_messages("Sorry, no Labels available for this mission!!");
     }
     SQLFreeStmt( hStmt, SQL_DROP );
   } else {
     i = fl_get_menu(fd_itelfields->menu_mission);
     strcpy(clutemp, "DSBFiles/");
     strcat(clutemp, CLUFileBase[i]);
     strcat(clutemp, ".rq");
     if ((fp = fopen(clutemp, "r")) != NULL) {
       i = -1;
       j = -1;
       while (!feof(fp)) {
	 do fgets(clutemp, 128, fp); while ((clutemp[0] == '#') && !feof(fp));
	 if (feof(fp)) break;
	 sscanf(clutemp, "%c %s", &chflag, chtemp);
	 strsub(chtemp, '_', ' ');
	 if (chflag == '-') {
	   i++;
	   strcpy(cluFieldNames[i], chtemp);
	 }
	 else if (chflag == '>') {
	   i++;
	   strcpy(cluFieldNames[i], chtemp);
	 }
       }
       fclose(fp);
     }
   }

   for (i=0; i<cluFieldCount; i++) {
     fl_set_object_label(fd_itelfields->itel_fldselect[i], cluFieldNames[i]);
     if (strlen(cluFieldNames[i]) > 1)
       fl_set_button(fd_itelfields->itel_fldselect[i], PUSHED);
   }

   fl_set_button(fd_itelfields->itel_fldselect[00], FALSE);
   fl_set_button(fd_itelfields->itel_fldselect[02], FALSE);
   fl_set_button(fd_itelfields->itel_fldselect[03], FALSE);
   fl_set_button(fd_itelfields->itel_fldselect[48], FALSE);
   fl_set_button(fd_itelfields->itel_fldselect[49], FALSE);
   fl_set_button(fd_itelfields->itel_fldselect[50], FALSE);
   fl_set_button(fd_itelfields->itel_fldselect[51], FALSE);

   fl_show_object(fd_itelfields->cluapply);

   fl_redraw_form(fd_itelfields->itelfields);               // Clean up any overdraws

   return;
}
