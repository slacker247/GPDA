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
#include "PUPforms.h"
#include "PUPproc.H"

int             PUPwinX, PUPwinY;
int             PUPwinW, PUPwinH;
char            PUPlabel[32];
Window          PUPwinid;

float           EstScore[20], ActScore[20], Worth[20];
char            puptemp[1024];

FD_pup          *fd_pup;

void PUPinit();
void PUPshow(int xpos, int ypos, int width, int height, Window mainwinID, PUPINFO info);
void PUPexitCB(FL_OBJECT *object, long item_no);
int  PUPclose(FL_FORM *form, void *data);
void PUPnoneCB(FL_OBJECT *ob, long data);

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
float           Ts, Te, Weight, Tbelief, Tdisbelief;
float           Rbelief;
char            Tu;
const char      *fname;
char            chname[64];
char            chclass[64];
char            chcase[32];
char            chvalue[64];
char            chfile[128];
char            chtemp[256]; 
Window          winid;

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
//   Get all node names for the selected mission
//
   strcpy(chfile, "CND");
   if (strcmp(chfile, "*") == 0) {
     strcpy(chtemp, fl_show_fselector("Node file", "DSBFiles/", "*.dsbn", NULL));
     if (strlen(chtemp) == 0) return;
   } else {
     strcpy(chtemp, "DSBFiles/");
     strcat(chtemp, chfile);
     strcat(chtemp, ".dsbn");
   }

   fl_clear_choice(fd_pup->pup_hypothesis);

   n = 0;
   if ((outfp = fopen(chtemp, "r")) == NULL) return;
   //
   fgets(puptemp, 128, outfp); 
   fscanf(outfp, "%d %s %*d %*f %*f %*s", &k, chclass);
   for (i=0; i<k; i++) {
     fscanf(outfp, "%d %s", &m, chclass);
     for (j=0; j<m; j++) {
       fscanf(outfp, "%s %*s %*f %*f", chname);
       n++;
       if ((info.row == i) && (info.col == j)) info.node = n;
       strsub(chname, '_', ' ');
       fl_addto_choice(fd_pup->pup_hypothesis, chname);
     }
   }
   fclose(outfp);
   //
   fl_set_choice(fd_pup->pup_hypothesis, info.node);
   //
   sprintf(puptemp, "%f", info.belief);
   fl_set_input(fd_pup->pup_belief, puptemp);
   sprintf(puptemp, "%f", info.disbelief);
   fl_set_input(fd_pup->pup_disbelief, puptemp);
   sprintf(puptemp, "%f", info.time);
   fl_set_input(fd_pup->pup_time, puptemp);
   //
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
   if ((fp = fopen(info.filename, "r")) != NULL) {
     fscanf(fp, "%d %s %d %f %f %c\n", &nlevels, chcase, &mode, &Ts, &Te, &Tu);
     for (j=0; j<nlevels; j++) {
       fscanf(fp, "%d %s", &nnodes, chcase);
       for (i=0; i<nnodes; i++) {
	 fscanf(fp, "%s %d %f %f %f %d\n",
		chcase, &nplans, &Weight, &Tbelief, &Tdisbelief, &flags);
	 if ((i==info.col) && (j==info.row)) {
	   Rbelief = Weight*(Tbelief-info.belief) + (1.0-Weight)*(Tdisbelief-info.disbelief);
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
void PUPexitCB(FL_OBJECT *object, long item_no)
{
int             item = item_no;

   fl_hide_form(fd_pup->pup);
   EraseActiveEntry(PUPlabel);

   //FinishUp();

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
   sprintf(puptemp, "140900:17Jul02+%s  %s  '%s'  %f  %f  %f  /%s/  #Dennis Ellis#",
	   fl_get_input(fd_pup->pup_time),
	   chtemp,
	   fl_get_input(fd_pup->pup_hypothesis),
	   0.76, 0.76, 0.76,
	   fl_get_choice_text(fd_pup->pup_update));
   fl_addto_browser(fd_pup->pup_log, puptemp);

   strcpy(puptemp, fl_get_object_label(fd_pup->pup_action));

   if (strcmp(puptemp, "Operator") == 0) {
     fl_show_messages("A manual action by an operator is required for this fix.");
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

   fl_set_choice(fd_pup->pup_update, i);

   FillFields(i);

   return;
}
