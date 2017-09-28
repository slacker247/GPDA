/*******************************************************************
 FILE:		ToggleBP.h
 CONTENTS:	Private definitions for the ToggleButton widget.
 AUTHOR:	Paul D. Johnston
 HISTORY:
 Date		Action
 ---------	------------------------------------
 10/21/91	Created.

********************************************************************/

#ifndef TOGGLEBUTTONP_H
#define TOGGLEBUTTONP_H

/*
 * Include the superclass private headers.
 */
#include <X11/CoreP.h>
#include <X11/Xc/Xc.h>
#include <X11/Xc/ControlP.h>

/*
 * Include this widget class public header.
 */
#include <X11/Xc/ToggleB.h>


/* Private declarations and definitions. */
#define MIN_TB_WIDTH		90
#define MIN_TB_HEIGHT		40
#define MAX_TB_WIDTH		150
#define MAX_TB_HEIGHT		70



/*
 * Class part.
 */
typedef struct 
{
   int dummy;	/* Minimum of one member required. */
} ToggleButtonClassPart;

/*
 * Class record.
 */
typedef struct _ToggleButtonClassRec
{
   CoreClassPart core_class;
   ControlClassPart control_class;
   ToggleButtonClassPart toggleButton_class;
} ToggleButtonClassRec;

/*
 * Declare the widget class record as external for use in the widget source
 * file.
 */
extern ToggleButtonClassRec toggleButtonClassRec;



/*
 * Instance part.
 */
typedef struct
{
   /* Public instance variables */
   XtCallbackList callback;		/* application callback route */
   char *on_color;			/* color of ON state(ASCII) */
   char *off_color;			/* color of OFF state(ASCII) */
   XcToggleState state;			/* toggle button state */

   /* Private instance variables. */
   XRectangle face;			/* Geometry of the ToggleButton face */
   XRectangle indicator;		/* Geometry of the ToggleButton face */
   XPoint lbl;				/* Location of Label string */
   Pixel on_pixel;			/* color of ON state(pixel) */
   Pixel off_pixel;			/* color of OFF state(pixel) */
   Boolean armed;			/* Flag for arming the ToggleButton. */

} ToggleButtonPart;

/*
 * Instance record.
 */
typedef struct _ToggleButtonRec
{
   CorePart core;
   ControlPart control;
   ToggleButtonPart toggleButton;
} ToggleButtonRec;




#endif  /* TOGGLEBUTTONP_H */

