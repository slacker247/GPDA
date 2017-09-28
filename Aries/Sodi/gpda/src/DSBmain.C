#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h> 
#include <time.h>
/*
 *   Include the Xforms stuff
*/
#include "forms.h"
#include "FOWtypes.h"
#include "DSBproc.h"
#include "messages.H"
 
/* --------------------------------------------------------------------- */  

extern void DSBinit();
extern void DSBshow(int xpos, int ypos, int w, int h, Window winid, INMSG *p);
extern void PUPinit();

       void PutOperator(int, char *);
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
int             DSBwinX = 120, DSBwinY = 120;
int             DSBwinW = 850, DSBwinH = 460;
Window          mainwinID = 0;
//
// Initialize the 'FORMS' system and create the forms
//
   fl_initialize(&argc, argv, 0, 0, 0);

   DSBinit();
   PUPinit();

   DSBshow(DSBwinX, DSBwinY, DSBwinW, DSBwinH, mainwinID, NULL);

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

void PutOperator(int S, char * string)
{
}
