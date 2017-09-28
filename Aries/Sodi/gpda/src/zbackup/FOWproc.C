#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h> 
#include <time.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <errno.h>
#ifdef LINKSC
#include <rpc/xdr.h>
#endif

#include "forms.h"
#include "FOWforms.h"

/* --------------------------------------------------------------------- */

#ifndef FALSE
#define FALSE   0
#endif
#ifndef TRUE
#define TRUE    1
#endif

#define NO      0
#define YES     1  

/* --------------------------------------------------------------------- */

typedef struct {
int             opindex[40];
float           fowfactor[40];
} FOWOP;

typedef struct {
int             causeindex;
int             enabled;
FOWOP           effect[3];
} FOW;

/* --------------------------------------------------------------------- */

char            FOWtemp[128];
char            FOWlabel[32];

float           FowFactor;
int             FowEnable;
FOW             FowArray[20];
//
//   Allocate the OPERATOR variables
//
int             num_opers = 12;
int             curr_oper = 0;
int             oper_index;
const char      FowOps[12][12] = { "<No-op>", 
                                  "Random",     "Delay",      "Disperse",   "Degrade",
                                  "Mistype",    "Conflict",   "Overload",   "Derate",
                                  "Transmit",   "Disconnect", "Threshold" };
//
//   Allocate the Default Values of Fog
//
float       FowOpsLow[12] = { 0.0, 0.2, 1.33, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2 };
float       FowOpsMed[12] = { 0.0, 0.4, 1.66, 1.0, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4 };
float       FowOpsHigh[12]  = { 0.0, 0.6, 2.00, 2.0, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6 };
char        FowOpsHelp[12][64] = { "  No operator assigned.", 
                                 "  Compare a uniform random number to a threshold.",
                                 "  Extend the receive and transmit time by a delay factor.",
                                 "  Increase the standard deviation by a factor.",
                                 "  Decrease the belief by a factor.",
                                 "  Change the evidence type to OTHER with less impact.",
                                 "  Change a fraction of positive evidence to negative.",
                                 "  Send more messages (by some factor) with same result.",
                                 "  Decrease the network weights by some factor.",
                                 "  Decrease the number of results sent.",
                                 "  Zeros some factor of the network links.",
                                 "  Increase the decision threshold by some factor." };
//
//   Allocate the Causes of Fog variables
//
int             num_fogs = 15;
int             curr_fog = 0;
const char      FowCauses[15][20] = { "Loss",      "Latency",     "Asynchrony", "Degradation",
                                      "Ambiguity", "Conflict",    "Overload",   "Bad_Luck",
                                      "Surprises", "Disposition", "Cognition",  "Confusion",
                                      "Priorties", "Miscommunication", "Assumptions" };
//
//   Allocate the Effects of Fog
//
const char      FowEffects[3][20] = { "Evidence", "Weights", "Outcomes" };

char            chline[256];
int             effectindex;        // 0 -> Evidence; 1 -> Weight; 2 -> Outcome
int             itemindex;
int             causeindex;
//
//   Allocate Forms
//
FD_FogInput     *fd_FogInput;
FD_SetDialog    *fd_SetDialog;
FD_HelpDialog   *fd_HelpDialog;

/* --------------------------------------------------------------------- */

void FOWinit();
void FOWshow(int xpos, int ypos, int width, int height, Window winid);
void FOWload(const char *infilename);
void FOWhelp(int itemindex, int causeindex, int effectindex);

void FOWset(int Icause, int Ieffect, float fog[2]);
int  FOWget(int Icause, int Ieffect, float fog[2]);
void FOWon();
void FOWoff();
int  FOWtest();

extern int  EraseActiveEntry(char *text);
extern int  StoreActiveEntry(char *text);
extern int  FinishUp();  

/* --------------------------------------------------------------------- */

void
FOWinit()
{
int     i, j;

   fd_FogInput   = create_form_FogInput();
   fd_SetDialog  = create_form_SetDialog();
   fd_HelpDialog = create_form_HelpDialog();

   strcpy(FOWlabel, "Fog-of-War"); 

   for (i=0; i<num_fogs;  i++) {
       FowArray[i].causeindex = i;
   }
   /*
   for (i=0; i<num_opers; i++)
       fl_addto_choice(fd_SetDialog->fog_set_choice, FowOps[i]);
   */
   for (i=0; i<3; i++) {
     for (j=0; j<20; j++) {
        FowArray[j].enabled = NO;
        FowArray[j].effect[i].opindex[j] = 1;
        FowArray[j].effect[i].fowfactor[j] = 0.0;
        FowArray[j].effect[i].opindex[j+20] = 0;
        FowArray[j].effect[i].fowfactor[j+20] = 0.0;
     }
   }

   FOWload("FOWinit.dat");

   FowEnable = FALSE;

   return;
}

int FOWclose(FL_FORM *form, void *data)
{
   FOGexitCB(NULL, 0);

   return (0);
}

void FOWshow(int xpos, int ypos, int width, int height, Window mainwinID)
{
Window winid;

   if(!fl_form_is_visible(fd_FogInput->FogInput) ) {
      fl_transient();
      fl_winposition(xpos, ypos);
      fl_initial_winsize(width, height);
      winid = fl_prepare_form_window(fd_FogInput->FogInput,
                                     FL_PLACE_POSITION,FL_TRANSIENT, FOWlabel);
      //fl_set_form_geometry(fd_bmcintel->bmcintel, xpos, ypos, width, height);
      fl_winreshape(winid, xpos, ypos, width, height);
      fl_show_form_window(fd_FogInput->FogInput);
      fl_set_form_atclose(fd_FogInput->FogInput, FOWclose, 0);
      StoreActiveEntry(FOWlabel);
   }
 
   return;
}

void
FOWset(int Icause, int Ieffect, float fog[2])
{
   FowArray[Icause].effect[Ieffect].fowfactor[Icause] = fog[0];
   FowArray[Icause].effect[Ieffect].fowfactor[Icause+20] = fog[1];
}

int
FOWget(int Icause, int Ieffect, float fog[2])
{
int     irc=0;

   fog[0] = FowArray[Icause].effect[Ieffect].fowfactor[Icause];
   fog[1] = FowArray[Icause].effect[Ieffect].fowfactor[Icause+20];

   return (FowArray[Icause].enabled);
}

void
FOWon()
{
   FowEnable = TRUE;
}

void
FOWoff()
{
   FowEnable = FALSE;
}

int
FOWtest()
{
   return (FowEnable);
}

void
FOWload(const char *infilename)
{
int        item = 0;
FILE       *LOADfp;
int        i,j;
char       chtemp[40];
int        nfogs, nopers, neffects, n, m, op1, op2, enable;
float      x, y, z;

   if (infilename != NULL) {
      LOADfp = fopen(infilename, "r");

      fscanf(LOADfp, "%d", &nfogs);
      for (i=0; i<num_fogs;  i++) {
         fscanf(LOADfp, "%d %s", &n, chline);
      }

      fscanf(LOADfp, "%d", &nopers);
      for (i=0; i<num_opers;  i++) {
         fscanf(LOADfp, "%d %s %f %f %f", &n, chline, &x, &y, &z);
      }

      fscanf(LOADfp, "%d", &neffects);

      fscanf(LOADfp, "%d %d %s\n", &n, &m, chline);
      for (j=n; j<=m; j++) {
        fscanf(LOADfp, "%d %s %f %d %s %f\n", &op1, chline, &x, &op2, chtemp, &y);
	FowArray[j].effect[0].opindex[j]      = op1;
	FowArray[j].effect[0].fowfactor[j]    = x;
	FowArray[j].effect[0].opindex[j+20]   = op2;
	FowArray[j].effect[0].fowfactor[j+20] = y;
        fl_set_object_label(fd_FogInput->evid_button[j], FowOps[op1]);
        fl_set_object_label(fd_FogInput->evid_button[j+20], FowOps[op2]);
      }
      fscanf(LOADfp, "%d %d %s\n", &n, &m, chline);
      for (j=n; j<=m; j++) {
        fscanf(LOADfp, "%d %s %f %d %s %f\n", &op1, chline, &x, &op2, chtemp, &y);
	FowArray[j].effect[1].opindex[j]      = op1;
	FowArray[j].effect[1].fowfactor[j]    = x;
	FowArray[j].effect[1].opindex[j+20]   = op2;
	FowArray[j].effect[1].fowfactor[j+20] = y;
        fl_set_object_label(fd_FogInput->weight_button[j], FowOps[op1]);
        fl_set_object_label(fd_FogInput->weight_button[j+20], FowOps[op2]);
      }
      fscanf(LOADfp, "%d %d %s\n", &n, &m, chline);
      for (j=n; j<=m; j++) {
        fscanf(LOADfp, "%d %s %f %d %s %f\n", &op1, chline, &x, &op2, chtemp, &y);
	FowArray[j].effect[2].opindex[j]      = op1;
	FowArray[j].effect[2].fowfactor[j]    = x;
	FowArray[j].effect[2].opindex[j+20]   = op2;
	FowArray[j].effect[2].fowfactor[j+20] = y;
        fl_set_object_label(fd_FogInput->outcome_button[j], FowOps[op1]);
        fl_set_object_label(fd_FogInput->outcome_button[j+20], FowOps[op2]);
      }
      fscanf(LOADfp, "%d\n", &n);
      for (j=0; j<n; j++) {
        fscanf(LOADfp, "%d\n", &m);
        FowArray[j].enabled = m;
        if (m >= 0) fl_set_button(fd_FogInput->cause_button[j], m);
      }
   }
}

void
FOWhelp(int itemindex, int causeindex, int effectindex)
{
int    item = 0;
int    opindex;
char   chdesc[2][16] = { "    ", "Description:" };

     opindex = FowArray[causeindex].effect[effectindex].opindex[itemindex];
     fl_set_object_label(fd_HelpDialog->help_oper, FowOps[opindex]);
     sprintf(chline, "%f",
         FowArray[causeindex].effect[effectindex].fowfactor[itemindex]);
     fl_set_object_label(fd_HelpDialog->help_factor, chline);
     fl_clear_browser(fd_HelpDialog->help_browser);
     fl_addto_browser(fd_HelpDialog->help_browser, chdesc[0]);
     fl_addto_browser(fd_HelpDialog->help_browser, chdesc[1]);
     fl_addto_browser(fd_HelpDialog->help_browser,
         FowOpsHelp[FowArray[causeindex].effect[effectindex].opindex[itemindex]]);
     fl_addto_browser(fd_HelpDialog->help_browser, chdesc[0]);
     sprintf(chline, "      Default Low:  %f", FowOpsLow[opindex]);
     fl_addto_browser(fd_HelpDialog->help_browser, chline);
     sprintf(chline, "      Default Med:  %f", FowOpsMed[opindex]);
     fl_addto_browser(fd_HelpDialog->help_browser, chline);
     sprintf(chline, "      Default High: %f", FowOpsHigh[opindex]);
     fl_addto_browser(fd_HelpDialog->help_browser, chline);
     fl_show_form(fd_HelpDialog->HelpDialog,FL_PLACE_CENTER,FL_FULLBORDER,"Fog of War Help");
}
/*                                                                       */
/* --------------------------------------------------------------------- */
/*      C A L L B A C K    R O U T I N E S    S T A R T    H E R E       */
/* --------------------------------------------------------------------- */
/*                                                                       */
void FOGexitCB(FL_OBJECT *ob, long data)
{
   fl_hide_form(fd_FogInput->FogInput);
   EraseActiveEntry(FOWlabel);
 
   return;
}
 
void FOGnoneCB(FL_OBJECT *ob, long data)
{
 
   return;
}

void fog_loadCB(FL_OBJECT *object, long item_no)
{
const char *infilename;

   sprintf(chline, "FOWinit.dat");
   infilename = fl_show_fselector("Load file", "./", "*.fog", chline);
   FOWload(infilename);
}

void fog_saveCB(FL_OBJECT *object, long item_no)
{
int        item = 0;
FILE       *SAVEfp;
int        i,j,n,m;
const char *otfilename;

   sprintf(chline, "FOWsave.fog");
   otfilename = fl_show_fselector("Save file", "./", "*.fog", chline);

   if (otfilename != NULL) {
      SAVEfp = fopen(otfilename, "w+");

      fprintf(SAVEfp, "%5d\n", num_fogs);
      for (i=0; i<num_fogs;  i++) {
         fprintf(SAVEfp, "%5d %20s\n", i, FowCauses[i]);
      }

      fprintf(SAVEfp, "%5d\n", num_opers);
      for (i=0; i<num_opers;  i++) {
         fprintf(SAVEfp, "%5d %20s  %8.6f  %8.6f  %8.6f\n", i, FowOps[i], 
                FowOpsLow[i], FowOpsMed[i], FowOpsHigh[i]);
      }

      fprintf(SAVEfp, "%5d\n", 3);

      n = 0; m = 7;
      fprintf(SAVEfp, "%5d %5d %15s\n", n, m, "Evidence");
      for (j=n; j<=m; j++) {
        fprintf(SAVEfp, "%5d %20s  %8.6f  %5d %20s  %8.6f\n",
	        FowArray[j].effect[0].opindex[j],
                "<No-op>",
	        FowArray[j].effect[0].fowfactor[j],
	        FowArray[j].effect[0].opindex[j+20],
                "<No-op>",
	        FowArray[j].effect[0].fowfactor[j+20]);
      }
      n = 10; m = 16;
      fprintf(SAVEfp, "%5d %5d %15s\n", n, m, "Weights");
      for (j=n; j<=m; j++) {
        fprintf(SAVEfp, "%5d %20s  %8.6f  %5d %20s  %8.6f\n",
	        FowArray[j].effect[1].opindex[j],
                "<No-op>",
	        FowArray[j].effect[1].fowfactor[j],
	        FowArray[j].effect[1].opindex[j+20],
                "<No-op>",
	        FowArray[j].effect[1].fowfactor[j+20]);
      }
      fprintf(SAVEfp, "%5d %5d %15s\n", n, m, "Outcomes");
      for (j=n; j<=m; j++) {
        fprintf(SAVEfp, "%5d %20s  %8.6f  %5d %20s  %8.6f\n",
	        FowArray[j].effect[2].opindex[j],
                "<No-op>",
	        FowArray[j].effect[2].fowfactor[j],
	        FowArray[j].effect[2].opindex[j+20],
                "<No-op>",
	        FowArray[j].effect[2].fowfactor[j+20]);
      }
      fprintf(SAVEfp, "%5d\n", 20);
      for (j=0; j<20; j++) {
        fprintf(SAVEfp, "%5d\n", FowArray[j].enabled);
      }
   }
}

void fog_buttonCB(FL_OBJECT *object, long item_no)
{

   FowEnable = !FowEnable;
   fprintf(stderr, "Fog of War is %d\n", FOWtest() );
}

void cause_buttonCB(FL_OBJECT *object, long item_no)
{

   FowArray[item_no].enabled = !FowArray[item_no].enabled;
}

void fog_evidCB(FL_OBJECT *object, long item_no)
{
int    item = 0;
int    opindex;
char   chdesc[2][16] = { "    ", "Description:" };

   itemindex   = item_no;                             // 0 - 39
   causeindex  = item_no;
   if (causeindex > 19) causeindex = causeindex - 20; // 0 - 19
   effectindex = 0;                                   // 0 -  2
   opindex = FowArray[causeindex].effect[effectindex].opindex[itemindex];
   oper_index = opindex;
   if (fl_mouse_button() == FL_LEFTMOUSE) {
     fl_set_object_label(fd_SetDialog->set_value, FowOps[opindex]);
     fl_show_form(fd_SetDialog->SetDialog, FL_PLACE_CENTER,FL_FULLBORDER, "Set Evidence Factors");
   } else {
     FOWhelp(itemindex, causeindex, effectindex);
   }
}

void fog_weightCB(FL_OBJECT *object, long item_no)
{
int item = 0, opindex;

   itemindex   = item_no;
   causeindex  = item_no;
   if (causeindex > 19) causeindex = causeindex - 20;
   effectindex = 1;
   opindex = FowArray[causeindex].effect[effectindex].opindex[itemindex];
   oper_index = opindex;
   if (fl_mouse_button() == FL_LEFTMOUSE) {
     fl_set_object_label(fd_SetDialog->set_value, FowOps[opindex]);
     fl_show_form(fd_SetDialog->SetDialog, FL_PLACE_CENTER,FL_FULLBORDER, "Set Weight Factors");
   } else {
     FOWhelp(itemindex, causeindex, effectindex);
   }
}

void fog_outcomeCB(FL_OBJECT *object, long item_no)
{
int item, opindex;

   itemindex   = item_no;
   causeindex  = item_no;
   if (causeindex > 19) causeindex = causeindex - 20;
   effectindex = 2;
   opindex = FowArray[causeindex].effect[effectindex].opindex[itemindex];
   oper_index = opindex;
   if (fl_mouse_button() == FL_LEFTMOUSE) {
     fl_set_object_label(fd_SetDialog->set_value, FowOps[opindex]);
     fl_show_form(fd_SetDialog->SetDialog, FL_PLACE_CENTER,FL_FULLBORDER, "Set Outcome Factors");
   } else {
     FOWhelp(itemindex, causeindex, effectindex);
   }
}

void fog_cancelCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   fl_hide_form(fd_SetDialog->SetDialog);
}

void fog_acceptCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   FowArray[causeindex].effect[effectindex].fowfactor[itemindex] = FowFactor;
   FowArray[causeindex].effect[effectindex].opindex[itemindex]   = oper_index;
   switch(effectindex) {
     case 0:
        fl_set_object_label(fd_FogInput->evid_button[itemindex], FowOps[oper_index]);        
        break;
     case 1:
        fl_set_object_label(fd_FogInput->weight_button[itemindex], FowOps[oper_index]);
        break;
     case 2:
        fl_set_object_label(fd_FogInput->outcome_button[itemindex], FowOps[oper_index]);
        break;
     default:
        break;
   }

   fl_hide_form(fd_SetDialog->SetDialog);
}

void fog_settingCB(FL_OBJECT *object, long item_no)
{
int item=0;

   switch(item_no) {
     case 0:
        FowFactor = FowOpsLow[oper_index];
        break;
     case 1:
        FowFactor = FowOpsMed[oper_index];
        break;
     case 2:
        FowFactor = FowOpsHigh[oper_index];
        break;
     default:
        FowFactor = 0.0;
        break;
   }
   sprintf(chline, "%f", FowFactor);
   fl_set_input(fd_SetDialog->set_value, chline); 
}

void fog_sliderCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   FowFactor = (float)fl_get_slider_value(fd_SetDialog->set_slider);
   sprintf(chline, "%f", FowFactor);
   fl_set_input(fd_SetDialog->set_value, chline);   
}

void fog_typeCB(FL_OBJECT *object, long item_no)
{
int item = 0;

//oper_index = fl_get_choice(fd_SetDialog->fog_set_choice)-1;
//fl_set_object_label(fd_SetDialog->set_value, FowOps[oper_index]);
}

void help_doneCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   fl_hide_form(fd_HelpDialog->HelpDialog);
}

void help_browserCB(FL_OBJECT *object, long item_no)
{
int item = 0;

}
