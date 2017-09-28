#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <time.h>
#include <sys/types.h>

#include "forms.h"
#include "CCPforms.h"

char            ccptemp[128];
char            CCMlabel[32];
char            CCSlabel[32];
char            CCAlabel[32];

FD_ccpmanage    *fd_ccpmanage;
FD_ccpsurvival  *fd_ccpsurvival;
FD_ccpattack    *fd_ccpattack;

void CCPinit();
void CCPshow(int xpos, int ypos, int width, int height, Window mainwinID, int which);
extern int  EraseActiveEntry(char *text);
extern int  StoreActiveEntry(char *text);

/*                                                                       */
/* --------------------------------------------------------------------- */
/*              M A I N   P R O G R A M   S T A R T S   H E R E          */
/* --------------------------------------------------------------------- */
/*                                                                       */
void CCPinit()
{
int             i;

   fd_ccpmanage = create_form_ccpmanage();
   fd_ccpsurvival = create_form_ccpsurvival();
   fd_ccpattack = create_form_ccpattack();

   return;
}

int CCPclose(FL_FORM *form, void *data)
{
long            item;

   item = (long)data;
   CCPexitCB(NULL, item);
}

void CCPshow(int xpos, int ypos, int width, int height, Window mainwinID, int which)
{
Window          winid;

   switch (which) {
     case 1:
       if(!fl_form_is_visible(fd_ccpmanage->ccpmanage) ) {
          strcpy(CCMlabel, "Force Management");
          fl_transient();
          fl_winposition(xpos, ypos);
          fl_initial_winsize(width, height);
          winid = fl_prepare_form_window(fd_ccpmanage->ccpmanage,
                                     FL_PLACE_POSITION,FL_TRANSIENT, CCMlabel);
          //fl_set_form_geometry(fd_bmcintel->bmcintel, xpos, ypos, width, height);
          fl_winreshape(winid, xpos, ypos, width, height);
          fl_show_form_window(fd_ccpmanage->ccpmanage);
          fl_set_form_atclose(fd_ccpmanage->ccpmanage, CCPclose, (void *)1);
          StoreActiveEntry(CCMlabel);
       }
       break;

     case 2:
       if(!fl_form_is_visible(fd_ccpsurvival->ccpsurvival) ) {
          strcpy(CCSlabel, "Force Survival");
          fl_transient();
          fl_winposition(xpos, ypos);
          fl_initial_winsize(width, height);
          winid = fl_prepare_form_window(fd_ccpsurvival->ccpsurvival,
                                     FL_PLACE_POSITION,FL_TRANSIENT, CCSlabel);
          //fl_set_form_geometry(fd_bmcintel->bmcintel, xpos, ypos, width, height);
          fl_winreshape(winid, xpos, ypos, width, height);
          fl_show_form_window(fd_ccpsurvival->ccpsurvival);
          fl_set_form_atclose(fd_ccpsurvival->ccpsurvival, CCPclose, (void *)2);
          StoreActiveEntry(CCSlabel);
       }
       break;

     case 3:
       if(!fl_form_is_visible(fd_ccpattack->ccpattack) ) {
          strcpy(CCAlabel, "Attack Assessment");
          fl_transient();
          fl_winposition(xpos, ypos);
          fl_initial_winsize(width, height);
          winid = fl_prepare_form_window(fd_ccpattack->ccpattack,
                                     FL_PLACE_POSITION,FL_TRANSIENT, CCAlabel);
          //fl_set_form_geometry(fd_bmcintel->bmcintel, xpos, ypos, width, height);
          fl_winreshape(winid, xpos, ypos, width, height);
          fl_show_form_window(fd_ccpattack->ccpattack);
          fl_set_form_atclose(fd_ccpattack->ccpattack, CCPclose, (void *)3);
          StoreActiveEntry(CCAlabel);
       }
       break;

     default:
       break;
   }

   return;
}
/*                                                                       */
/* --------------------------------------------------------------------- */
/*      C A L L B A C K    R O U T I N E S    S T A R T    H E R E       */
/* --------------------------------------------------------------------- */
/*                                                                       */
void CCPexitCB(FL_OBJECT *object, long item_no)
{
int item = item_no;

   switch (item) {
     case 1:
       fl_hide_form(fd_ccpmanage->ccpmanage);
       EraseActiveEntry(CCMlabel);
       break;

     case 2:
       fl_hide_form(fd_ccpsurvival->ccpsurvival);
       EraseActiveEntry(CCSlabel);
       break;

     case 3:
       fl_hide_form(fd_ccpattack->ccpattack);
       EraseActiveEntry(CCAlabel);
       break;

     default:
       break;
   }
}

void CCPnoneCB(FL_OBJECT *ob, long data)
{

   return;
}
