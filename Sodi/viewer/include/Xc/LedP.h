/*******************************************************************
 FILE:		LedP.h
 CONTENTS:	Private definitions for the LedButton widget.
 AUTHOR:	Paul D. Johnston
 HISTORY:
 Date		Action
 ---------	------------------------------------
 10/21/91	Created.

********************************************************************/

#ifndef LEDP_H
#define LEDP_H

/*
 * Include the superclass private headers.
 */
#include <X11/CoreP.h>
#include <X11/Xc/Xc.h>
#include <X11/Xc/ControlP.h>

/*
 * Include this widget class public header.
 */
#include <X11/Xc/Led.h>


/* Private declarations and definitions. */
#define MIN_LB_WIDTH		90
#define MIN_LB_HEIGHT		40
#define MAX_LB_WIDTH		150
#define MAX_LB_HEIGHT		70



/*
 * Class part.
 */
typedef struct 
{
   int dummy;	/* Minimum of one member required. */
} LedButtonClassPart;

/*
 * Class record.
 */
typedef struct _LedButtonClassRec
{
   CoreClassPart core_class;
   ControlClassPart control_class;
   LedButtonClassPart ledButton_class;
} LedButtonClassRec;

/*
 * Declare the widget class record as external for use in the widget source
 * file.
 */
extern LedButtonClassRec ledButtonClassRec;



/*
 * Instance part.
 */
typedef struct
{
   /* Public instance variables. */
   XtCallbackList callback;		/* callback function */
   Pixel state_fg_pixel;		/* state foreground pixel */
   Pixel state_bg_pixel;		/* state background pixel */
   int interval;			/* update interval */
   Pixel led_color;			/* color of the LED */
   char *state;				/* LED state string */
   Boolean led_visible;			/* LED visibility (T / F) */
   Boolean state_visible;		/* State String visibility (T / F)  */

   /* Private instance variables. */
   XRectangle face;			/* Geometry of the LedButton face */
   XPoint lbl;				/* Location of Label string */
   XRectangle state_box;		/* Geometry of State Box. */
   XPoint st;				/* Point in State Box to draw string */
   XRectangle led_face;			/* Geometry of the LED face */
   short inner_shade_depth;		/* shade_depth of the LED */
   Boolean armed;			/* Flag for arming the LedButton. */
   XtIntervalId interval_id;		/* ID for widget TimeOut procedure. */

} LedButtonPart;

/*
 * Instance record.
 */
typedef struct _LedButtonRec
{
   CorePart core;
   ControlPart control;
   LedButtonPart ledButton;
} LedButtonRec;




#endif  /* LEDP_H */

