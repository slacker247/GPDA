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

#include "forms.h"
#include "CLUforms.h"
#include "ITELforms.h"

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
Window          CLUwinid;

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

extern int      FieldCount ;
extern char     FieldNames[52][16];

FD_cluster      *fd_cluster;
FD_itelfields   *fd_itelfields;
FD_defnode      *fd_defnode;
extern FD_bmcintel *fd_bmcintel;

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

   fd_cluster = create_form_cluster();
   fd_itelfields = create_form_itelfields();
   fd_defnode = create_form_defnode();

   strcpy(CLUlabel, "Subdue-Clustering");

   return;
}

void CLUshow(int xpos, int ypos, int width, int height, Window mainwinID)
{

   if(!fl_form_is_visible(fd_cluster->cluster) ) {
      fl_transient();
      fl_winposition(xpos, ypos);
      fl_initial_winsize(width, height);
      CLUwinid = fl_prepare_form_window(fd_cluster->cluster,
                                 FL_PLACE_POSITION,FL_TRANSIENT, CLUlabel);
      fl_winreshape(CLUwinid, xpos, ypos, width, height);
      fl_get_wingeometry(CLUwinid, &CLUwinX, &CLUwinY, &CLUwinW, &CLUwinH);
      CLUwinX = CLUwinX + 4;
      CLUwinY = CLUwinY + 24;
      fl_show_form_window(fd_cluster->cluster);
      fl_set_form_atclose(fd_cluster->cluster, CLUclose, 0);
      fl_set_form_title(fd_cluster->cluster, CLUlabel);
      StoreActiveEntry(CLUlabel);
   }

   fl_hide_object(fd_cluster->clu_waitmsg);
   fl_hide_object(fd_cluster->clu_donemsg);

   cluresetCB(NULL, 0);

   return;
}
void CLUexitCB(FL_OBJECT *object, long item_no)
{
int             item = item_no;

   fl_hide_form(fd_cluster->cluster);
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
 
void cluapplyCB(FL_OBJECT *object, long item_no)
{
int             item = item_no;
int             i, j, k;
int             irc;
const char      *fname;
char            filename[64];
char            chtemp[64];

   fl_hide_object(fd_cluster->clu_waitmsg);
   fl_hide_object(fd_cluster->clu_donemsg);

   if (fl_get_button(fd_itelfields->clu_subdue) == PUSHED) {
     //
     //   Process 'cluster' request
     //   -------------------------
     //
     //   Get the graph file name
     //
     if (strcmp(clufname, "\0") == 0) {
       fname = fl_show_fselector("Graph file", "./", "*.g", NULL);
       strcpy(filename, fname);
     } else {
       strcpy(filename, clufname);
       strcat(filename, ".g");
     }
     strcpy(clu_gfn, filename);
     sprintf(clutemp, "Doing cluster analysis using graph file %s.", clu_gfn);
     fl_set_input(fd_cluster->message, clutemp);
     //
     fl_show_object(fd_cluster->clu_waitmsg);
     //
     //   Setup 'Subdue' parameters
     //
     strcpy(clutemp, "Subdue");
     for (i=0; i<NPARMS; i++) {
       if (fl_get_button(fd_cluster->param[i+1]) == PUSHED) {
	 sprintf(chtemp, " -%s", params[i].chparam);
	 strcat(clutemp, chtemp);
	 strcpy(chtemp, "\0");
	 switch (i+1) {
	 case 14: // 'ps'
	   sprintf(chtemp, " %s", fl_get_input(fd_cluster->clu_subsfn));
	   break;

	 case 15: // 'size'
	   sprintf(chtemp, " %d %d",
		   (int)fl_get_counter_value(fd_cluster->clu_lowsize),
		   (int)fl_get_counter_value(fd_cluster->clu_highsize));
	   break;

	 case 16: // 'output'
	   sprintf(chtemp, " %d", fl_get_choice(fd_cluster->out_choice));
	   break;

	 case 18: // 'plot'
	   sprintf(chtemp, " %s", fl_get_input(fd_cluster->clu_plotfn));
	   break;

	 default:
	   if (params[i].pflag != 0)
	     sprintf(chtemp, " %s", params[i].chvalue);
	   break;
	 }
	 strcat(clutemp, chtemp);
       }
     }
     //
     //   Run 'Subdue'
     //   
     sprintf(chtemp, " %s", clu_gfn);
     strcat(clutemp, chtemp);
     //thiswin = fl_winget();
     //fl_set_cursor(thiswin, XC_watch);
     irc = fl_exe_command(clutemp, 1);
     //fl_reset_cursor(thiswin);
     fl_hide_object(fd_cluster->clu_waitmsg);
     fl_show_object(fd_cluster->clu_donemsg);

     if (irc != 0) {
       fl_show_messages("Clustering Error - See Command Log for error code");
       fprintf(stderr, " Return code of Subdue is %d\n", irc);
     }
   }

   return;
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
int             i, j, k;
int             noEOF = TRUE;
int             Vn, Vtop;
long            irc;
const char      *fname;
char            chtemp[1024];
char            chname[64];
char            chclass[64];

   switch (item) {

     case 1: // Field Selection (Data Mining)
       //
       //   Display all field IDs for user selection
       //
       for (i=0; i<FieldCount; i++) {
	 fl_set_object_label(fd_itelfields->itel_fldselect[i], FieldNames[i]);
	 if (strlen(FieldNames[i]) > 1)
	   fl_set_button(fd_itelfields->itel_fldselect[i], PUSHED);
       }
       /* 
       //
       //   Get value of each field from this record
       //   (This needs changing to set selection from list of RQs
       //    present in DS network)
       //
       noEOF = TRUE;
       while (noEOF) {
	 n_Pushed = 0;
	 for (i=0; i<n_Fields; i++) {
	   fscanf(clufp, "%s ", chname);          // Get field value
	   if (feof(clufp)) {
	     noEOF = FALSE;
	     break;
	   }
	   if (strcmp(chname, "N/A") != 0) {
	     fl_set_button(fd_itelfields->itel_fldselect[i], PUSHED);
	     n_Pushed++;
	   }
	 }
	 if (feof(clufp)) break;
       }
       */
       //
       //   The user must explicitly select data entry info fields
       //
       fl_set_button(fd_itelfields->itel_fldselect[48], FALSE);
       fl_set_button(fd_itelfields->itel_fldselect[49], FALSE);
       fl_set_button(fd_itelfields->itel_fldselect[50], FALSE);
       fl_set_button(fd_itelfields->itel_fldselect[51], FALSE);

       fl_set_button(fd_itelfields->clu_subdue, PUSHED);
       fl_clear_browser(fd_itelfields->clu_classes);
       fl_set_input(fd_itelfields->clu_closest, " ");
       fl_show_form(fd_itelfields->itelfields, FL_PLACE_CENTER,FL_FULLBORDER,
		      "Field Select");
       break;

     case 10: // 'Apply' Field Selections
       if (fl_get_button(fd_itelfields->clu_subdue) == PUSHED) {
	 //
	 //   Process 'Cluster' request
	 //   -------------------------
	 //
	 if (strcmp(clufname, "\0") == 0) {
	   fname = fl_show_fselector("Table file", "./", "*.tab", NULL);
	   strcpy(clutemp, fname);
	 } else {
	   strcpy(clutemp, clufname);
	   strcat(clutemp, ".tab");
	 }
	 strcpy(clu_tfn, clutemp);
	 if ((clufp = fopen(clu_tfn, "r")) == NULL) return;
	 cluFileOpen = TRUE;

	 fl_set_input(fd_itelfields->clu_classfn, clu_tfn);
	 fl_set_button(fd_itelfields->clu_subdue, PUSHED);

	 fscanf(clufp, "%s", chclass);                 // Get Mission domain name
	 fscanf(clufp, "%d\n", &n_Fields);             // Get fields/record & EOR
	 strsub(chclass, ' ', '-');
	 strcpy(clufname, chclass);
	 fgets(clutemp, 1024, clufp);                  // Get field names

	 if (TABFILE) {
	   outfp = fopen("SubdueTemp.tab", "w");
	   fprintf(outfp, "%s  %d\n", chclass, n_Fields);
	 }

	 strcpy(clutemp, chclass);
	 strcat(clutemp, ".g");
	 grafp = fopen(clutemp, "w");                  // Open the directed graph file (.g)
	 //
	 //   Output field IDs for only those fields selected
	 //
	 n_Ids = 0;
	 for (i=0; i<FieldCount; i++) {
	   strcpy(fields[i].name, fl_get_object_label(fd_itelfields->itel_fldselect[i]));
	   fields[i].select = FALSE;
	   if (fl_get_button(fd_itelfields->itel_fldselect[i]) == PUSHED) {
	     if (TABFILE) fprintf(outfp, "%s ", FieldNames[i]);
	     fields[i].select = TRUE;
	     n_Ids++;
	   }
	 }
	 if (TABFILE) fprintf(outfp, "\n");
	 //
	 Vn = 0;
	 n_Recs = -1;
	 noEOF = TRUE;
	 /* *************************************************************************
	 //
	 //   Get the field link structure
	 //
	 strcpy(clutemp, chclass);
	 strcat(clutemp, ".link");
	 lnkfp = fopen(clutemp, "r");
	 for (i=0; i<FieldCount; i++) {
	   fscanf(lnkfp, "%d", &fields[i].nlinks);     // Set # of back links for this field 
	   for (j=0;j<6;j++)
	   fields[i].links[j] = 0;                     // Clear any leftover link pointers
	   if (fields[i].nlinks > 0) {
	     for (j=0; j<fields[i].nlinks; j++) {      // Set all back links for this field
	       fscanf(lnkfp, "%d", &fields[i].links[j]);
	     }
	   }
	 }
	 fclose(lnkfp);
	 **************************************************************************** */
	 sprintf(chtemp, "Classifing file %s using Simple Heirarchy", clu_tfn);
	 fl_set_input(fd_cluster->message, chtemp);
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

	   for (i=0; i<FieldCount; i++) {
	     fscanf(clufp, "%s ", chname);             // Get field value
	     strcpy(fields[i].value, chname);          // Save for duration of record processing
	     if (feof(clufp)) {
	       noEOF = FALSE;
	       break;
	     }
	     if (fields[i].select) {
	       if (TABFILE) {
		 //
		 //   Build the flat file with selected fields
		 //
		 if ((strcmp(chname, "N/A") == 0) || (strcmp(chname, "n/a") == 0)) {
		   fprintf(outfp, "%f ", (float)drand48());
		 } else  { 
		   fprintf(outfp, "%s ", chname);
		 }
	       } 
	       //
	       //   Build the graph vertices for the fields in this record
	       //
	       Vn++;
	       fields[i].Vn = Vn;
	       fprintf(grafp, "v %d  %s\n", Vn, fields[i].value);
	     }
	   } // -- end record processing --
	   //
	   if (TABFILE) fprintf(outfp, "\n");
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
	 if (TABFILE) fclose(outfp);
	 //
	 sprintf(clutemp, "Class name ..... %s\nNo. Records .... %d\nFields/Record .. %d",
		 chclass, n_Recs, n_Ids);
	 fl_show_messages(clutemp);
       } else {
	 //
	 //   Process 'Classify' request
	 //   --------------------------
	 //
	 //if ((clufp = fopen(fl_get_input(fd_itelfields->clu_classfn), "r")) == NULL) return;
	 //
	 //   Run 'Autoclass'
	 //
	 sprintf(chtemp, "Classifing file %s using Autoclass", " ");
	 fl_set_input(fd_cluster->message, chtemp);
   
	 sprintf(clutemp, "Classify.sh %s %s", "rq-train", "rq-test");

	 irc = fl_exe_command(clutemp, 1);

	 fl_hide_object(fd_cluster->clu_waitmsg);
	 fl_show_object(fd_cluster->clu_donemsg);

	 if (irc != 0) {
	   fl_show_messages("Classifying Error - See Command Log for error code");
	   fprintf(stderr, " Return code of Autoclass is %d\n", irc);
	 }
	 //
	 //   Get all the possible classess (hypothesis)
	 //
	 if ((clufp = fopen("./CLASSIFY/rq-train.class-data-1", "r")) == NULL) return;

	 noEOF = TRUE;
	 while (noEOF) {
	   fgets(clutemp, 1024, clufp);
	   if (feof(clufp)) {
	     noEOF = FALSE;
	     break;
	   }
	   if (strstr(clutemp, "DATA_CLASS") != NULL) {
	     sscanf(clutemp, "%s %d", chname, &k);
	     fgets(clutemp, 1024, clufp);
	     fgets(clutemp, 1024, clufp);
	     fgets(clutemp, 1024, clufp);
	     fgets(clutemp, 1024, clufp);
	     fgets(clutemp, 1024, clufp);
	     sscanf(clutemp, "%s %s", chname, chclass);
	     strcpy(clutemp, fl_get_choice_item_text(fd_bmcintel->itel_source, k+1));
	     strsub(clutemp, ' ', '_');
	     sprintf(chtemp, "    %-10d %-24s", k, clutemp);
	     fl_addto_browser(fd_itelfields->clu_classes, chtemp);
	   }

	 }
	 fclose(clufp);
	 //
	 //   Get the closest match
	 //
	 if ((clufp = fopen("./CLASSIFY/rq-test.case-data-1", "r")) == NULL) return;

	 noEOF = TRUE;
	 while (noEOF) {
	   fgets(clutemp, 1024, clufp);
	   if (feof(clufp)) {
	     noEOF = FALSE;
	     break;
	   }
	   if (strstr(clutemp, "DATA_CASE") != NULL) {
	     fgets(clutemp, 1024, clufp);
	     fgets(clutemp, 1024, clufp);
	     sscanf(clutemp, "%s %d %d", chname, &k, &j);   // Get case ID, Class #, Probability
	     fl_select_browser_line(fd_itelfields->clu_classes, k+1);
	     sscanf(fl_get_browser_line(fd_itelfields->clu_classes, k+1), "%d %s", &i, chtemp);
	     fl_set_input(fd_itelfields->clu_closest, chtemp);
	   }

	 }
	 fclose(clufp);
       }
       break;

     case 11: // Done
       fl_hide_form(fd_itelfields->itelfields);

       if (fl_get_button(fd_itelfields->clu_classify) == PUSHED) {
	 if (strlen(fl_get_input(fd_itelfields->clu_closest)) > 0) {
	   strcpy(clutemp, "Classifier determined hypothesis\n\n");
	   strcat(clutemp, fl_get_input(fd_itelfields->clu_closest));
	   strcat(clutemp, "\n\n to be closest match.\n\nAccept?");
	   irc = fl_show_question(clutemp, 1);
	   if (irc == 1) {
	     fl_set_input(fd_bmcintel->itel_field[1],
			  fl_get_input(fd_itelfields->clu_closest));
	   }
	 }
       }
       break;

     case 12: // Select All
       for (i=0; i<FieldCount; i++) {
	 fl_set_button(fd_itelfields->itel_fldselect[i], PUSHED);
       }
       break;

     case 13: // Deselect All
       for (i=0; i<FieldCount; i++) {
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

   fl_set_input(fd_cluster->message, " ");

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

   if (params[i].pflag == 1)
     sprintf(params[i].chvalue, "%f",
	 (float)fl_get_counter_value(fd_cluster->cluvalue[item_no]));
   else
     sprintf(params[i].chvalue, "%d",
	 (int)fl_get_counter_value(fd_cluster->cluvalue[item_no]));

   //fprintf(stderr, " %d %d %s %s\n", item_no, i, params[i].chparam, params[i].chvalue);
   return;
}
