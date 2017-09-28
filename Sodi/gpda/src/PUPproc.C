
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
#include "PUPforms.h"
#include "PUPproc.H"

int             PUPwinX, PUPwinY;
int             PUPwinW, PUPwinH;
char            PUPlabel[32];
Window          PUPwinid;

float           EstScore[20], ActScore[20], Worth[20];
char            puptemp[1024];

int             PUPMYSQL         = 0;
extern int      bHTMLTable;
extern int      bBatch;
extern int      cDelimiter;
extern int      bColumnNames;
extern SQLHSTMT hStmt;
SQLHENV         PUPhEnv  = 0;
SQLHDBC         PUPhDbc  = 0;

FD_pup          *fd_pup;

void PUPinit();
void PUPshow(int xpos, int ypos, int width, int height, Window mainwinID, PUPINFO info);
void PUPexitCB(FL_OBJECT *object, long item_no);
int  PUPclose(FL_FORM *form, void *data);
void PUPnoneCB(FL_OBJECT *ob, long data);

int TestSQL();

extern int  EraseActiveEntry(char *text);
extern int  StoreActiveEntry(char *text);
extern int  FinishUp();

/*                                                                       */
/* --------------------------------------------------------------------- */
/*              M A I N   P R O G R A M   S T A R T S   H E R E          */
/* --------------------------------------------------------------------- */
/*                                                                       */
void PUPinit()
{
int             i;

   fd_pup = create_form_pup();

   strcpy(PUPlabel, "Plan-Update");

   fl_set_browser_fontsize(fd_pup->pup_log, FL_NORMAL_SIZE); 
   fl_set_browser_fontstyle(fd_pup->pup_log, FL_FIXEDBOLD_STYLE);
   fl_set_browser_fontsize(fd_pup->pup_strategy, FL_SMALL_SIZE); 
   fl_set_browser_fontstyle(fd_pup->pup_strategy, FL_FIXEDBOLD_STYLE);

   fl_set_input_cursor_visible(fd_pup->pup_shell, FALSE);

   return;
}

void PUPshow(int xpos, int ypos, int width, int height, Window mainwinID, PUPINFO info)
{
FILE            *fp;
FILE            *outfp;
int             i, j, k, m, n, item = 0;
int             nlevels, nnodes, nplans, mode, flags;
int             nCols;
float           Ts, Te, Weight, Tbelief, Tdisbelief;
float           Rbelief;
char            Tu;
const char      *fname;
char            szDSN[MAX_DATA_WIDTH+1] = "MySQL";
char            szUID[MAX_DATA_WIDTH+1] = "root";
char            szPWD[MAX_DATA_WIDTH+1] = "gpda";
char            szSQL[9001]             = "show databases;";
char            chname[64];
char            chclass[64];
char            chcase[32];
char            chvalue[64];
char            chfile[128];
char            chtemp[256]; 
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

   if(!fl_form_is_visible(fd_pup->pup) ) {
      fl_transient();
      fl_winposition(xpos, ypos);
      fl_initial_winsize(width, height);
      winid = fl_prepare_form_window(fd_pup->pup,
                                     FL_PLACE_POSITION,FL_TRANSIENT, PUPlabel);
      fl_winreshape(winid, xpos, ypos, width, height);
      fl_show_form_window(fd_pup->pup);
      fl_set_form_atclose(fd_pup->pup, PUPclose, 0);
      StoreActiveEntry(PUPlabel);
   }
   //
   fl_clear_browser(fd_pup->pup_strategy);
   sprintf(chtemp, "@N@_%-32s  %-5s  %-5s  %-5s  %-8s  %-48s",
	   "        Plan Name",
	   " Est.",
	   " Act.",
	   "Worth",
	   " Action",
	   "  Fix");
   fl_add_browser_line(fd_pup->pup_strategy, chtemp);
//
//   Open the Database for this Domain
//
   if (PUPMYSQL) {
     if ( !OpenDatabase( &PUPhEnv, &PUPhDbc, szDSN, szUID, szPWD ))
     exit(-1);
     //
     //   Find out which databases are available
     //
     fl_clear_menu(fd_pup->menu_mission);
     nRows = 0;
     strcpy(szSQL, "show databases;");
     nCols = ExecuteSQL( PUPhDbc, szSQL, cDelimiter, bColumnNames, bHTMLTable );
     if ( nCols > 0 ) {
       nReturn = SQLFetch( hStmt );
       while ( nReturn == SQL_SUCCESS || nReturn == SQL_SUCCESS_WITH_INFO ) {
	 for( nCol = 1; nCol <= nCols; nCol++ ) {
	   nReturn = SQLGetData( hStmt, nCol, SQL_C_CHAR, (SQLPOINTER)szColumnValue,
			       sizeof(szColumnValue), &nIndicator );
	   szColumnValue[MAX_DATA_WIDTH] = '\0';
	   if ( nReturn == SQL_SUCCESS && nIndicator != SQL_NULL_DATA ) {
	     sprintf(puptemp, "%s", szColumnValue);
	     if ( (strstr(puptemp, "test") == NULL) && (strstr(puptemp, "mysql") == NULL) )
	       fl_addto_menu(fd_pup->menu_mission, puptemp);
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
     //   Load the initial mission types from initialization file
     //
     fp = fopen("DSBinit.dat", "r");
     do fgets(puptemp, 128, fp); while (puptemp[0] == '#');    // Read # assessments
     sscanf(puptemp, "%d", &n);
     for (j=0; j<n; j++) {
       do fgets(puptemp, 128, fp); while (puptemp[0] == '#');
       //
       //   Read assessment type and file base names
       //
       sscanf(puptemp, "%s %s", chtemp, chclass);
       strsub(chtemp, '_', ' ');
       fl_addto_menu(fd_pup->menu_mission, chtemp);
     }
     fclose(fp);
   }

   return;
}
void PUPexitCB(FL_OBJECT *object, long item_no)
{
int             item = item_no;

   CloseDatabase( PUPhEnv, PUPhDbc );

   fl_hide_form(fd_pup->pup);
   EraseActiveEntry(PUPlabel);

   FinishUp();

   return;
}

int PUPclose(FL_FORM *form, void *data)
{
long            item;

   item = (long)data;
   PUPexitCB(NULL, item);

   return(0);
}

void PUPnoneCB(FL_OBJECT *ob, long data)
{
   return;
}
/*                                                                       */
/* --------------------------------------------------------------------- */
/*      C A L L B A C K    R O U T I N E S    S T A R T    H E R E       */
/* --------------------------------------------------------------------- */
/*                                                                       */
void FillFields(int i)
{
char            *p;

   sprintf(puptemp, "%f", EstScore[i-1]);
   fl_set_input(fd_pup->pup_estimated, puptemp);
   sprintf(puptemp, "%f", ActScore[i-1]);
   fl_set_input(fd_pup->pup_actual, puptemp);
   sprintf(puptemp, "%f", Worth[i-1]);
   fl_set_input(fd_pup->pup_worth, puptemp);

   p = strchr(fl_get_browser_line(fd_pup->pup_strategy, i+1), '|');
   p++;
   p++;

   if (p != NULL) {
     strcpy(puptemp, p);
     strtrm(puptemp);
     fl_set_input(fd_pup->pup_shell, puptemp);
   }

   p = p-12;
   sscanf(p, "%s", puptemp);
   fl_set_object_label(fd_pup->pup_action, puptemp);

   return;
}

void pupexecCB(FL_OBJECT *ob, long data)
{
char            chtemp[64];

   strcpy(chtemp, fl_get_choice_text(fd_pup->pup_update)); 
   strsub(chtemp, ' ', '_');
   sprintf(puptemp, "140900:17Jul02+%s  %s  '%s'  %f  %f  %f   #Commander#",
	   fl_get_input(fd_pup->pup_time),
	   chtemp,
	   fl_get_choice_text(fd_pup->pup_hypothesis),
	   0.76, 0.76, 0.76,
	   fl_get_choice_text(fd_pup->pup_update));
   fl_addto_browser(fd_pup->pup_log, puptemp);

   strcpy(puptemp, fl_get_object_label(fd_pup->pup_action));

   if (strcmp(puptemp, "Operator") == 0) {
     //fl_show_messages("A manual action by an operator is required for this fix.");
   } else if (strcmp(puptemp, "Message") == 0) {
     fl_show_messages("Message action not yet implemented.");
   } else if (strcmp(puptemp, "Script") == 0) {
     system(fl_get_input(fd_pup->pup_shell));
   }

   return;
}

void pupupdateCB(FL_OBJECT *ob, long data)
{
int             i, j, k, n;

   i = fl_get_choice(fd_pup->pup_update);

   fl_select_browser_line(fd_pup->pup_strategy, i+1);

   FillFields(i);

   return;
}

void pupstratCB(FL_OBJECT *ob, long data)
{
int             i, j, k, n;

   i = fl_get_browser(fd_pup->pup_strategy)-1;    // Skip header line

   printf("Strategy %d\n", i);

   fl_set_choice(fd_pup->pup_update, i);

   FillFields(i);

   return;
}

void pupmenuCB(FL_OBJECT *ob, long data)
{
int             i, j, k, n;

//i = fl_get_browser(fd_pup->pup_strategy)-1;    // Skip header line

   PUPexitCB(NULL, 0);

   return;
}

void pupmissionCB(FL_OBJECT *ob, long data)
{
int             i, j, k, n, nCols;
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

   fl_clear_choice(fd_pup->pup_story);
   //
   if (PUPMYSQL) { 
     //
     //   Use selected mission
     //
     i = fl_get_menu(fd_pup->menu_mission);
     sprintf(szSQL, "use %s;", fl_get_menu_text(fd_pup->menu_mission));
     fprintf(stderr, "Using %s\n", szSQL);

     nCols = ExecuteSQL( PUPhDbc, szSQL, cDelimiter, bColumnNames, bHTMLTable );
     //
     //   Get all the tables
     //
     nRows = 0;
     strcpy(szSQL, "show tables;");
     nCols = ExecuteSQL( PUPhDbc, szSQL, cDelimiter, bColumnNames, bHTMLTable );
     if ( nCols > 0 ) {
       nReturn = SQLFetch( hStmt );
       while ( nReturn == SQL_SUCCESS || nReturn == SQL_SUCCESS_WITH_INFO ) {
	 for( nCol = 1; nCol <= nCols; nCol++ ) {
	   //nOptimalDisplayWidth = OptimalDisplayWidth( hStmt, nCol, nUserWidth );
	   nReturn = SQLGetData( hStmt, nCol, SQL_C_CHAR, (SQLPOINTER)szColumnValue,
			       sizeof(szColumnValue), &nIndicator );
	   szColumnValue[MAX_DATA_WIDTH] = '\0';
	   if ( nReturn == SQL_SUCCESS && nIndicator != SQL_NULL_DATA ) {
	     printf("    Data in row %d %s\n", nRows+1, szColumnValue );
	     sprintf(puptemp, "%s", szColumnValue);
	     if ( (strstr(puptemp, "Evidence") == NULL) && (strstr(puptemp, "Labels") == NULL) )
	       fl_addto_choice(fd_pup->pup_story, puptemp);
	   } else if ( nReturn == SQL_ERROR ) {
	     break;
	   }
	 } // for columns
	 nRows++;
	 nReturn = SQLFetch( hStmt );
       } // while rows
     } else {
       fl_show_messages("Sorry, no stories available for this mission!!");
     }
     SQLFreeStmt( hStmt, SQL_DROP );
   } else {
     /*
     fprintf(stderr, "Loading story from %s.story\n", dsbtypes[k].filebase);
     strcpy(dsbtemp, "DSBFiles/");
     strcat(dsbtemp, dsbtypes[k].filebase);
     strcat(dsbtemp, ".stry");
     if ((fp = fopen(dsbtemp, "r")) != NULL) {
       do fgets(dsbtemp, 128, fp); while (dsbtemp[0] == '#');
       sscanf(dsbtemp, "%d", &nstory);
       if (nstory > 0) {
	 for (istory=0; istory<nstory; istory++) {
	   do fgets(dsbtemp, 128, fp); while (dsbtemp[0] == '#');
	   sscanf(puptemp, "%s %f %f %f %s", chstory,
		  &StoryB[istory], &StoryD[istory], &StoryU[istory],
		  chtemp);
	   fl_addto_choice(fd_pup->dsb_story, puptemp);
	 }
       } else {
     */
	 strcpy(puptemp, "Master");
	 fl_addto_choice(fd_pup->pup_story, puptemp);
	 //}
	 //}
   }

   return;
}

void pupstoryCB(FL_OBJECT *ob, long data)
{
FILE            *fp;
int             i, j, k, n, nCols;
int             nlevels, nnodes, nplans, mode, flags;
float           Ts, Te, Weight, Tbelief, Tdisbelief;
float           Rbelief;
float           results[6];
char            chname[64];
char            chclass[64];
char            chcase[32];
char            chvalue[64];
char            chfile[128];
char            chtemp[256]; 
 float           B = 0.6;
 float           D = 0.2;
 float           T = 0.1;
char            szSQL[128];
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

   fl_clear_choice(fd_pup->pup_hypothesis);
   //
   if (PUPMYSQL) { 
     //
     //   Use selected story
     //
     i = fl_get_choice(fd_pup->pup_story);
     sprintf(szSQL, "SELECT HYPOTHESIS FROM %s;", fl_get_choice_text(fd_pup->pup_story));
     fprintf(stderr, "Using %s\n", szSQL);

     nCols = ExecuteSQL( PUPhDbc, szSQL, cDelimiter, bColumnNames, bHTMLTable );
     //
     //   Get all the hypothesis'
     //   
     nRows = 0;
     if ( nCols > 0 ) {
       nReturn = SQLFetch( hStmt );
       while ( nReturn == SQL_SUCCESS || nReturn == SQL_SUCCESS_WITH_INFO ) {
	 for( nCol = 1; nCol <= nCols; nCol++ ) {
	   nReturn = SQLGetData( hStmt, nCol, SQL_C_CHAR, (SQLPOINTER)szColumnValue,
			       sizeof(szColumnValue), &nIndicator );
	   szColumnValue[MAX_DATA_WIDTH] = '\0';
	   if ( nReturn == SQL_SUCCESS && nIndicator != SQL_NULL_DATA ) {
	     sprintf(puptemp, "%s", szColumnValue);
	     fprintf(stderr, "Hypothesis %d is [%s]\n", nRows, puptemp);
	     fflush(stderr);
	     fl_addto_choice(fd_pup->pup_hypothesis, puptemp);
	   } else if ( nReturn == SQL_ERROR ) {
	     break;
	   }
	 } // for columns
	 nRows++;
	 nReturn = SQLFetch( hStmt );
       } // while rows
     } else {
       fl_show_messages("Sorry, no hypothesis defined in story!!");
     }
     SQLFreeStmt( hStmt, SQL_DROP );
   } else {
     sprintf(puptemp, "%s.plan", fl_get_menu_text(fd_pup->menu_mission));
     strsub(puptemp, ' ', '_');
     if ((fp = fopen(puptemp, "r")) != NULL) {
       while (!feof(fp)) {
	 fscanf(fp, "%s %d %f %f %f %d\n",
	      chcase, &nplans, &Weight, &Tbelief, &Tdisbelief, &flags);
	 fl_addto_choice(fd_pup->pup_hypothesis, chcase);
	 for (k=0; k<nplans; k++) {
	   fscanf(fp, "%s %s %s", chfile, chvalue, puptemp);
	 }
       }
     }
   }

   return;
}

void puphypothCB(FL_OBJECT *ob, long data)
{
FILE            *fp;
int             i, j, k, n;
int             nlevels, nnodes, nplans, mode, flags;
int             nCols;
float           Ts, Te, Weight, Tbelief, Tdisbelief;
float           Rbelief;
float           results[6];
char            chname[64];
char            chclass[64];
char            chcase[32];
char            chvalue[64];
char            chfile[128];
char            chtemp[256]; 
 float           B = 0.6;
 float           D = 0.2;
 float           T = 0.1;
char            szSQL[128];
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

   fl_set_input(fd_pup->pup_estimated, "");
   fl_set_input(fd_pup->pup_actual, "");
   fl_set_input(fd_pup->pup_worth, "");
   fl_set_input(fd_pup->pup_shell, "");
   //
   fl_clear_choice(fd_pup->pup_update);
   fl_clear_browser(fd_pup->pup_strategy);
   sprintf(chtemp, "@N@_%-32s  %-5s  %-5s  %-5s  %-8s  %-48s",
	   "        Plan Name",
	   " Est.",
	   " Act.",
	   "Worth",
	   " Action",
	   "  Fix");
   fl_add_browser_line(fd_pup->pup_strategy, chtemp);
   //
   sprintf(puptemp, "%s.plan", fl_get_menu_text(fd_pup->menu_mission));
   if ((fp = fopen(puptemp, "r")) != NULL) {
     while (!feof(fp)) {
       fscanf(fp, "%s %d %f %f %f %d\n",
	      chcase, &nplans, &Weight, &Tbelief, &Tdisbelief, &flags);
       if (strcmp(chcase, fl_get_choice_text(fd_pup->pup_hypothesis)) == 0) {
	 sprintf(szSQL, "SELECT BELIEF, DISBELIEF FROM %s;",
		 fl_get_choice_text(fd_pup->pup_story));
	 fprintf(stderr, "Using %s\n", szSQL);
	 nCols = ExecuteSQL( PUPhDbc, szSQL, cDelimiter, bColumnNames, bHTMLTable );
	 nRows = 0;
	 if ( nCols > 0 ) {
	   nReturn = SQLFetch( hStmt );
	   while ( nReturn == SQL_SUCCESS || nReturn == SQL_SUCCESS_WITH_INFO ) {
	     for( nCol = 1; nCol <= nCols; nCol++ ) {
	       nReturn = SQLGetData( hStmt, nCol, SQL_C_CHAR, (SQLPOINTER)szColumnValue,
				     sizeof(szColumnValue), &nIndicator );
	       szColumnValue[MAX_DATA_WIDTH] = '\0';
	       if ( nReturn == SQL_SUCCESS && nIndicator != SQL_NULL_DATA ) {
		 sprintf(puptemp, "%s", szColumnValue);
		 results[nCol-1] = atof(puptemp);
	       } else if ( nReturn == SQL_ERROR ) {
		 break;
	       }
	     } // for columns
	     nRows++;
	     nReturn = SQLFetch( hStmt );
	   } // while rows
	 } else {
	   //fl_show_messages("Sorry, no belief saved for this story!!");
	 }
	 SQLFreeStmt( hStmt, SQL_DROP );
	 //
	 sprintf(puptemp, "%f", B);
	 fl_set_input(fd_pup->pup_belief, puptemp);
	 sprintf(puptemp, "%f", D);
	 fl_set_input(fd_pup->pup_disbelief, puptemp);
	 sprintf(puptemp, "%f", T);
	 fl_set_input(fd_pup->pup_time, puptemp);
	 Rbelief = Weight*(Tbelief-B) + (1.0-Weight)*(Tdisbelief-D);
	 //
	 for (k=0; k<nplans; k++) {
	   fscanf(fp, "%s %s %s", chfile, chvalue, chtemp);
	   strsub(chfile, '_', ' ');
	   strsub(chtemp, '_', ' ');
	   fl_addto_choice(fd_pup->pup_update, chfile);
	   //
	   EstScore[k] = 0.45;
	   ActScore[k] = 0.61;
	   Worth[k] = EstScore[k]/ActScore[k];
	   //
	   sprintf(puptemp, "%-32s  %5.2f  %5.2f  %5.2f  %-8s  | %-48s",
		   chfile,
		   EstScore[k], ActScore[k], Worth[k],
		   chvalue,
		   chtemp);
	   fl_add_browser_line(fd_pup->pup_strategy, puptemp);
	 }
	 goto done;
       } else {
	 for (k=0; k<nplans; k++) {
	   fscanf(fp, "%s %s %s", chfile, chvalue, puptemp);
	 }
       }
     }
done:
     sprintf(puptemp, "%f", Tbelief);
     fl_set_input(fd_pup->pup_bthresh, puptemp);
     sprintf(puptemp, "%f", Tdisbelief);
     fl_set_input(fd_pup->pup_dthresh, puptemp);
     sprintf(puptemp, "%f", Rbelief);
     fl_set_input(fd_pup->pup_weight, puptemp);
     //
     fclose(fp);
   } else {
     fl_show_messages("Sorry, no .plan file available!");
   }

   return;
}
