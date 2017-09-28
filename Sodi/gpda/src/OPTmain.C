#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h> 
#include <time.h>
/*
 *   Include the Xforms stuff
*/
#include "forms.h"
//#include "FOGtypes.h"
 
/* --------------------------------------------------------------------- */  

extern void OPTinit();
extern void OPTshow(int xpos, int ypos, int width, int height, Window winid);
       int  EraseActiveEntry(char *text);
       int  StoreActiveEntry(char *text);
/*                                                                       */
/* --------------------------------------------------------------------- */
/*              M A I N   P R O G R A M   S T A R T S   H E R E          */
/* --------------------------------------------------------------------- */
/*                                                                       */
main(int argc, char *argv[])
{
int             OPTwinX = 120, OPTwinY = 120;
int             OPTwinW = 850, OPTwinH = 460;
Window          mainwinID = 0;
//
// Initialize the 'FORMS' system and create the forms
//
   fl_initialize(&argc, argv, 0, 0, 0);
   OPTinit();

   OPTshow(OPTwinX, OPTwinY, OPTwinW, OPTwinH, mainwinID);

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
   exit (0);
}

int StoreActiveEntry(char *text)
{
   return (1);
}

