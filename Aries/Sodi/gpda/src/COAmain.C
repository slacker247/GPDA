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

char        UserName[16] = "Dennis Ellis";
 
/* --------------------------------------------------------------------- */  

extern void COAinit();
extern void COAshow(int x, int y, int w, int h, Window winid, char *fn);
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
int             COAwinX = 120, COAwinY = 120;
int             COAwinW = 850, COAwinH = 460;
Window          mainwinID = 0;
//
// Initialize the 'FORMS' system and create the forms
//
   fl_initialize(&argc, argv, 0, 0, 0);
   COAinit();

   COAshow(COAwinX, COAwinY, COAwinW, COAwinH, mainwinID, "ISPAN_WO");

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

