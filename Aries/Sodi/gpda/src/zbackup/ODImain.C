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

   fl_show_form(fd_ODI->ODI, FL_PLACE_CENTER,FL_FULLBORDER, "GPDA-Splash");

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
 
   exit(0);
}

void ODInoneCB(FL_OBJECT *ob, long data)
{
 
   return;
}
/*                                                                       */
/*                         Handle the main buttons                       */
/*                         -----------------------                       */
/*                                                                       */
void battleCB(FL_OBJECT *ob, long data)
{
int             pid;
char            *display;
char            charg[8];

   display = getenv("REMOTE");

   mission = data;

   switch (data) {
   case 6:
     fl_set_object_label(fd_ODI->rhs_button[1], "Parse");
     fl_set_object_label(fd_ODI->rhs_button[2], "Korea DB");
     fl_set_object_label(fd_ODI->rhs_button[3], "Mine");
     fl_set_object_label(fd_ODI->rhs_button[4], "Fuse");
     fl_set_object_label(fd_ODI->rhs_button[5], "Optimize");
     break;

   case 8:
     fl_set_object_label(fd_ODI->rhs_button[1], "Bx");
     fl_set_object_label(fd_ODI->rhs_button[2], "KB");
     fl_set_object_label(fd_ODI->rhs_button[3], "EBO");
     fl_set_object_label(fd_ODI->rhs_button[4], "TCT");
     fl_set_object_label(fd_ODI->rhs_button[5], "Task");
     break;

   default:
     break;
   }

   return;
}

void RHSideCB(FL_OBJECT *ob, long data)
{
int             pid;
char            *display;
char            oditemp[64];

   display = getenv("REMOTE");

   if (mission == 6) {
     switch (data) {
     case 1:
       strcpy(oditemp, "Gate.sh");
       break;

     case 2:
       strcpy(oditemp, "TGT");
       break;

     case 3:
       strcpy(oditemp, "Weka.sh");
       break;

     case 4:
       strcpy(oditemp, "DSB");
       break;

     case 5:
       strcpy(oditemp, "OPT");
       break;

     default:
       return;
       break;
     }
   } else {
     switch (data) {
     case 1:
       strcpy(oditemp, "BOX");
       break;

     case 2:
       strcpy(oditemp, "Protege.sh");
       break;

     case 3:
       strcpy(oditemp, "DSB");
       break;

     case 4:
       strcpy(oditemp, "DSB");
       break;

     case 5:
       strcpy(oditemp, "PUP");
       break;

     default:
       return;
       break;
     }
   }

   switch (pid = fork()) {
   case 0:
     if (display)
       execlp(oditemp, oditemp, "-display", display, NULL);
     else
       execlp(oditemp, oditemp, NULL);
     perror(oditemp);
     exit (255);
     break;
   case -1:
     printf("Child process [%s] startup failed. PID = %d\n", oditemp, pid);
     break;
   }

   return;
}
