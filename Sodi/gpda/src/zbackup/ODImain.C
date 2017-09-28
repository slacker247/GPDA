#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h>
#include <time.h>
#include <signal.h>
/*
 *   Include the network/socket stuff
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <string.h>
#include <unistd.h>
#include <stream.h>
#define SIGCHILD SIGCLD

#include "DataParser.H"

#include "forms.h"
#include "ODIforms.h"

/* --------------------------------------------------------------------- */
 
#ifndef FALSE
#define FALSE   0
#endif
#ifndef TRUE
#define TRUE    1
#endif
 
/* --------------------------------------------------------------------- */
  
typedef int     Boolean;
char            *name, *cmdlevel, *position;
long            mission;

FL_OBJECT       *scipm;
FL_OBJECT       *bmdoipm, *jntfipm;

FD_ODI          *fd_ODI;
FD_help         *fd_help;
FD_login        *fd_login;
FD_cmocnode     *fd_cmocnode;

void nmdsdfCB(FL_OBJECT *ob, long data);
/*
void configedCB(FL_OBJECT *ob, long data);
void engageCB(FL_OBJECT *ob, long data);
void plannerCB(FL_OBJECT *ob, long data);
void patriotCB(FL_OBJECT *ob, long data);
void reasonerCB(FL_OBJECT *ob, long data);
void predatorCB(FL_OBJECT *ob, long data);
void datamineCB(FL_OBJECT *ob, long data);
*/ 
extern int  EraseActiveEntry(char *text);
extern int  StoreActiveEntry(char *text);
 
/* --------------------------------------------------------------------- */

char *
strsub(char *istr, char och, char nch)
{
int    i;
 
   for (i=0; i<strlen(istr); i++) if (istr[i] == och) istr[i] = nch;
   return (istr);
}
/*                                                                       */
/* --------------------------------------------------------------------- */
/*              M A I N   P R O G R A M   S T A R T S   H E R E          */
/* --------------------------------------------------------------------- */
/*                                                                       */
int main(int argc, char *argv[])
{
int             i;
char            *BITMAPDIR;
char            xpmfile[128];

   fl_initialize(&argc, argv, 0, 0, 0);
   fd_ODI = create_form_ODI();
   fd_help = create_form_help();
   fd_login = create_form_login();
   fd_cmocnode = create_form_cmocnode();

   fl_show_form(fd_ODI->ODI, FL_PLACE_CENTER,FL_FULLBORDER, "ODI");

   fl_do_forms();

   return 0;
}
/*                                                                       */
/* --------------------------------------------------------------------- */
/*      C A L L B A C K    R O U T I N E S    S T A R T    H E R E       */
/* --------------------------------------------------------------------- */
/*                                                                       */
void ODIexitCB(FL_OBJECT *ob, long data)
{
   fl_hide_form(fd_ODI->ODI);
   //EraseActiveEntry(ITELlabel);
 
   exit(0);
}

void ODInoneCB(FL_OBJECT *ob, long data)
{
 
   return;
}

void nothingCB(FL_OBJECT *ob, long data)
{
}

void loginCB(FL_OBJECT *object, long item_no)
{
int             item = item_no;
int             pid;
char            *display;
char            UserName[64]; 

   switch (item) {

     case 0:
       strcpy(UserName, fl_get_input(fd_login->username));
       if (fl_form_is_visible(fd_login->login))
         fl_hide_form(fd_login->login);
       fl_show_form(fd_cmocnode->cmocnode, FL_PLACE_CENTER,FL_FULLBORDER, "Position");
       break;

     case 10:
       if (fl_form_is_visible(fd_cmocnode->cmocnode))
	 fl_hide_form(fd_cmocnode->cmocnode);
       nmdsdfCB(NULL, mission);
       break;

     default:
       break;
   } 
}

void positionCB(FL_OBJECT *object, long item_no)
{
int item = item_no;

   position = object->label;
   //fl_set_object_label(fd_bmc3->node_position, position);
}

void cmdcenterCB(FL_OBJECT *object, long item_no)
{
int             item = item_no;

   cmdlevel = object->label;
   //fl_set_object_label(fd_bmc3->node_center, cmdlevel);
   //fl_set_choice(fd_bmc3->bmc_choice, item+1);
}

void signoffCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   exit(0);
}
/*                                                                       */
/*                         Handle the main buttons                       */
/*                         -----------------------                       */
/*                                                                       */
void battleCB(FL_OBJECT *ob, long data)
{
int             MustLogin = 1;

   mission = data;

   MustLogin = (getenv("PLANLOGIN")==NULL ? 0 : 1);  // Go through login screens
   if (MustLogin)
     fl_show_form(fd_login->login, FL_PLACE_CENTER,FL_FULLBORDER, "Login");
   else
     nmdsdfCB(NULL, data);
}

void nmdsdfCB(FL_OBJECT *ob, long data)
{
int             pid;
char            *display;
char            charg[8];

   display = getenv("REMOTE");

   switch (data) {
   case 0:
     strcpy(charg, "nmd");
     break;
   case 1:
     strcpy(charg, "aoc");
     break;
   case 2:
     strcpy(charg, "iow");
     break;
   case 3:
     strcpy(charg, "aoc");
     break;
   }
   fprintf(stderr, " ODI running in mode %s\n", charg);
   switch (pid = fork()) {
   case 0:
     if (display)
        execlp("gpda", "gpda", charg, "-display", display, NULL);
     else
        execlp("gpda", "gpda", charg, NULL);
     perror("gpda");
     exit (255);
     break;
   case -1:
     printf("Child process startup failed. PID is %d\n", pid);
     break;
   }
}

void visualCB(FL_OBJECT *ob, long data)
{
int             pid;
char            *display;

   display = getenv("REMOTE");

   switch (pid = fork()) {
   case 0:
     if (display)
        execlp("jtamv", "jtamv", "-display", display, NULL);
     else
        execlp("jtamv", "jtamv", NULL);
     perror("jtamv");
     exit (255);
     break;
   case -1:
     printf("Child process startup failed. PID = %d\n", pid);
     break;
   }
}

void plannerCB(FL_OBJECT *ob, long data)
{
int             pid;
char            *display;

   display = getenv("REMOTE");

   switch (pid = fork()) {
   case 0:
     if (display)
        execlp("odisim", "odisim", "-display", display, NULL);
     else
       execlp("odisim", "odisim", NULL);
     perror("odisim");
     exit (255);
     break;
   case -1:
     printf("Child process startup failed. PID is %d\n", pid);
     break;
   }
}

void patriotCB(FL_OBJECT *ob, long data)
{
int             pid;
char            *display;

   display = getenv("REMOTE");
 
   switch (pid = fork()) {
   case 0:
     if (display)
        execlp("patriot", "patriot", "-display", display, NULL);
     else
        execlp("patriot", "patriot", NULL);
     perror("patriot");
     exit (255);
     break;
   case -1:
     printf("Child process startup failed. PID = %d\n", pid);
     break;
   }
}

void engageCB(FL_OBJECT *ob, long data)
{
int             pid;
char            *display;

   display = getenv("REMOTE");

   switch (pid = fork()) {
   case 0:
     if (display)
        execlp("compose", "compose", "-display", display, NULL);
     else
        execlp("compose", "compose", NULL);
     perror("compose");
     exit (255);
     break;
   case -1:
     printf("Child process startup failed. PID = %d\n", pid);
     break;
   }
}

void reasonerCB(FL_OBJECT *ob, long data)
{
int             pid;
char            *display;

   display = getenv("REMOTE");

   switch (pid = fork()) {
   case 0:
     if (display)
        execlp("reasoner", "reasoner", "-display", display, NULL);
     else
        execlp("reasoner", "reasoner", NULL);
     perror("reasoner");
     exit (255);
     break;
   case -1:
     printf("Child process startup failed. PID = %d\n", pid);
     break;
   }
}

void predatorCB(FL_OBJECT *ob, long data)
{
int             pid;
char            *display;

   display = getenv("REMOTE");
 
   switch (pid = fork()) {
   case 0:
     if (display)
       execlp("skyfly", "skyfly", "-display", display, NULL);
     else
       execlp("skyfly", "skyfly", NULL);
     perror("skyfly");
     exit (255);
     break;
   case -1:
     printf("Child process startup failed. PID = %d\n", pid);
     break;
   }
}

void configedCB(FL_OBJECT *ob, long data)
{
int             pid;
char            *display;

   display = getenv("REMOTE");
 
   switch (pid = fork()) {
   case 0:
     if (display)
       execlp("jtamved", "jtamved", "-display", display, NULL);
     else
       execlp("jtamved", "jtamved", NULL);
     perror("jtamved");
     exit (255);
     break;
   case -1:
     printf("Child process startup failed. PID = %d\n", pid);
     break;
   }
}

void datamineCB(FL_OBJECT *ob, long data)
{
int             pid;
char            *display;

   display = getenv("REMOTE");
 
   //system("java -jar weka.jar");
   system("csh weka.csh");

}

void fusionCB(FL_OBJECT *ob, long data)
{
int             pid;
char            *display;
char            chprog[16];

   switch (data) {
   case 0:
     strcpy(chprog, "DSB");
     break;

   case 1:
     strcpy(chprog, "OPT");
     break;
   }

   display = getenv("REMOTE");
 
   switch (pid = fork()) {
   case 0:
     if (display)
       execlp(chprog, chprog, "-display", display, NULL);
     else
       execlp(chprog, chprog, NULL);
     perror(chprog);
     exit (255);
     break;
   case -1:
     printf("Child process startup failed. PID = %d\n", pid);
     break;
   }
}

void splashhelpCB(FL_OBJECT *ob, long data)
{
int             pid;
char            *display;
char            fname[32];
char            chline[64];

   switch (data) {
   case 0:  // Datamining
     strcpy(fname, "Help/datamining.hlp");
     break;

   case 1:

     break;

   default:

     break;
   }

   fl_load_browser(fd_help->helptext, fname);
   fl_show_form(fd_help->help, FL_PLACE_CENTER,FL_FULLBORDER,
		"Help Getting Started");

   sprintf(chline, "cat %s", fname);
   system(chline);
}

void helpdoneCB(FL_OBJECT *ob, long data)
{

   fl_hide_form(fd_help->help);
}
