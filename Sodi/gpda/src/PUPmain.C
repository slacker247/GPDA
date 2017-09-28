#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h> 
#include <time.h>
/*
 *   Include the Xforms stuff
*/
#include "forms.h"

#include "PUPproc.H"
 
/* --------------------------------------------------------------------- */  

extern void PUPinit();
extern void PUPshow(int xpos, int ypos, int width, int height, Window winid,
                    PUPINFO info);
       int  EraseActiveEntry(char *text);
       int  StoreActiveEntry(char *text);
       int  FinishUp();
/*                                                                       */
/* --------------------------------------------------------------------- */
/*              M A I N   P R O G R A M   S T A R T S   H E R E          */
/* --------------------------------------------------------------------- */
/*                                                                       */
main(int argc, char *argv[])
{
int             PUPwinX = 120, PUPwinY = 120;
int             PUPwinW = 850, PUPwinH = 460;
char            chfile[64];
PUPINFO         pupinfo;
Window          mainwinID = 0;
//
// Initialize the 'FORMS' system and create the forms
//
   fl_initialize(&argc, argv, 0, 0, 0);
   PUPinit();

     pupinfo.col       = 0;
     pupinfo.row       = 0;
     //
     pupinfo.belief    = 0.6;
     pupinfo.disbelief = 0.2;
     pupinfo.time      = 0.1;
     //
     //strcpy(pupinfo.nodelabel, dsbtemp);
     //
     strcpy(chfile, "DSBFiles/");
     strcat(chfile, "CND");
     strcat(chfile, ".plan");
     strcpy(pupinfo.filename, chfile);

   PUPshow(PUPwinX, PUPwinY, PUPwinW, PUPwinH, mainwinID, pupinfo);

   fl_do_forms();
}
/*                                                                       */
/* --------------------------------------------------------------------- */
/*          S U P P O R T   R O U T I N E S   S T A R T   H E R E        */
/* --------------------------------------------------------------------- */
/*                                                                       */
int FinishUp()
{
   exit(0);

   return(0);
}

int EraseActiveEntry(char *text)
{
   return (0);
}

int StoreActiveEntry(char *text)
{
   return (1);
}

