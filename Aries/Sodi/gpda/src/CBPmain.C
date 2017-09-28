#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h> 
#include <time.h>

#include "Globals.h"
/*
 *   Include the Xforms stuff
*/
#include "forms.h"
//#include "gpdaProperties.h"

char        UserName[16] = "Dennis Ellis";
 
/* --------------------------------------------------------------------- */  

extern void CBPinit();
extern void CBPshow(int x, int y, int w, int h, Window winid, char *fn);
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
int             CBPwinX = 120, CBPwinY = 120;
int             CBPwinW = 850, CBPwinH = 460;
Window          mainwinID = 0;
//
// Initialize the 'FORMS' system and create the forms
//
   fl_initialize(&argc, argv, 0, 0, 0);

//   initResources();  // Call this only once, before 1st call to getResources()
//   getResources();
fl_mapcolor(FL_COL1, 229, 229, 229);

   CBPinit();

   CBPshow(CBPwinX, CBPwinY, CBPwinW, CBPwinH, mainwinID, "ISPAN_WO");

   fl_do_forms();

   return(0);
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

