#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h> 
#include <time.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <errno.h>

#include "forms.h"
#include "REDIforms.h"

#ifndef FALSE
#define FALSE   0
#endif
#ifndef TRUE
#define TRUE    1
#endif 

#define DEGRAD  57.29577951308232
#define RADDEG  0.0174532925199432958

char            reditemp[128];
char            REDIlabel[32];

int             n_sites_icbm = 4;
int             n_sites_slbm = 3;
int             n_sites_bomb = 4;
int             n_sites_tank = 30;
int             n_sites_recc = 5;
int             n_sites_c2   = 3;
char            sites_icbm[4][32]  = { "FE Warren", "Malmstrom", "Minot", "TOTAL" };
char            sites_slbm[3][32]  = { "Bangor", "Kingsbay", "TOTAL" };
char            sites_bomb[4][32]  = { "Barksdale", "Minot", "Whiteman", "TOTAL" };
char            sites_tank[30][32] = { "Bangor", "Beale", "Birmingham", "Chicago", "Eielson",
                                       "Fairchild", "ForbesField", "GenMitchell", "GrandFork",
                                       "Grissom", "Kadena", "KeyField", "Lincoln", "MacDill",
                                       "March", "McConnell", "McGhee", "McGuire", "Mildenhall",
                                       "NiagaraFalls", "Pease", "Phoenix", "Pittsburg",
                                       "Rickenbach", "Robins", "SaltLakeCity", "Selfridge",
                                       "Sym.", "Tinker", "TOTAL" };
char            sites_recc[5][32]  = { "Beale", "Offutt", "Rota", "Whidbey", "TOTAL" };
char            sites_c2[3][32]    = { "Offutt", "Tinker", "TOTAL" };

char            chredi[1280];

FD_readistatus *fd_readistatus;

int         REDIinit();
void        REDIshow(int xpos, int ypos, int width, int height, Window mainwinID);
extern int  EraseActiveEntry(char *text);
extern int  StoreActiveEntry(char *text);

/*                                                                       */
/* --------------------------------------------------------------------- */
/*              M A I N   P R O G R A M   S T A R T S   H E R E          */
/* --------------------------------------------------------------------- */
/*                                                                       */
int
REDIinit()
{

   fd_readistatus = create_form_readistatus();

   strcpy(REDIlabel, "Ready Assess");

   rediselectCB(NULL, 2L);

   return 0;
}

int REDIclose(FL_FORM *form, void *data)
{
   REDIexitCB(NULL, 0);
}

void REDIshow(int xpos, int ypos, int width, int height, Window mainwinID)
{
Window          winid;

   if(!fl_form_is_visible(fd_readistatus->readistatus) ) {
      fl_transient();
      fl_winposition(xpos, ypos);
      fl_initial_winsize(width, height);
      winid = fl_prepare_form_window(fd_readistatus->readistatus,
                                     FL_PLACE_POSITION,FL_TRANSIENT, "Ready-Assess");
      fl_winreshape(winid, xpos, ypos, width, height);
      fl_show_form_window(fd_readistatus->readistatus);
      fl_set_form_atclose(fd_readistatus->readistatus, REDIclose, 0);
      StoreActiveEntry("Ready-Assess");
   }

   return;
}
/*                                                                       */
/* --------------------------------------------------------------------- */
/*      C A L L B A C K    R O U T I N E S    S T A R T    H E R E       */
/* --------------------------------------------------------------------- */
/*                                                                       */
void REDIexitCB(FL_OBJECT *ob, long data)
{
   fl_hide_form(fd_readistatus->readistatus);
   EraseActiveEntry("Ready-Assess");

   return;
}

void REDInoneCB(FL_OBJECT *ob, long data)
{

   return;
}

void redinoneCB(FL_OBJECT *ob, long data)
{
  /* fill-in code for callback */
}

void rediselectCB(FL_OBJECT *ob, long data)
{
int             item = data;
int             i;

   fl_clear_browser(fd_readistatus->site_browser);
   //fl_set_object_label(fd_readistatus->redi_sites, ob->label);

   switch (item) {
     case 0:
       fl_set_object_label(fd_readistatus->redi_sites, "ICBM");
       for (i=0; i<n_sites_icbm; i++) {
          strcpy(chredi, "@C2");
          strcat(chredi, sites_icbm[i]);
          fl_addto_browser(fd_readistatus->site_browser, chredi);
       }                
       break;

     case 1:
       fl_set_object_label(fd_readistatus->redi_sites, "SLBM");
       for (i=0; i<n_sites_slbm; i++) {
          strcpy(chredi, "@C2");
          strcat(chredi, sites_slbm[i]);
          fl_addto_browser(fd_readistatus->site_browser, chredi);
       }         
       break;

     case 2:
       fl_set_object_label(fd_readistatus->redi_sites, "BOMBERS");
       for (i=0; i<n_sites_bomb; i++) {
          strcpy(chredi, "@C2");
          strcat(chredi, sites_bomb[i]);
          fl_addto_browser(fd_readistatus->site_browser, chredi);
       }         
       break;

     case 3:
       fl_set_object_label(fd_readistatus->redi_sites, "TANKERS");
       for (i=0; i<n_sites_tank; i++) {
          strcpy(chredi, "@C2");
          if (i == 11) strcpy(chredi, "@C3");
          if (i == 19) strcpy(chredi, "@C1");
          strcat(chredi, sites_tank[i]);
          fl_addto_browser(fd_readistatus->site_browser, chredi);
       }         
       break;

     case 4:
       fl_set_object_label(fd_readistatus->redi_sites, "RECCE");
       for (i=0; i<n_sites_recc; i++) {
          strcpy(chredi, "@C2");
          strcat(chredi, sites_recc[i]);
          fl_addto_browser(fd_readistatus->site_browser, chredi);
       }         
       break;

     case 5:
       fl_set_object_label(fd_readistatus->redi_sites, "C2");
       for (i=0; i<n_sites_c2; i++) {
          strcpy(chredi, "@C2");
          strcat(chredi, sites_c2[i]);
          fl_addto_browser(fd_readistatus->site_browser, chredi);
       }         
       break;

     default:
       break;
   }
}

void basebrowserCB(FL_OBJECT *ob, long data)
{
  /* fill-in code for callback */
}

void redicanvasCB(FL_OBJECT *ob, long data)
{
  /* fill-in code for callback */
}

void redistatusCB(FL_OBJECT *ob, long data)
{
  /* fill-in code for callback */
}



