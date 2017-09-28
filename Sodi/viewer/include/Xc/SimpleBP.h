/*******************************************************************
 FILE:		SimpleBP.h
 CONTENTS:	Private definitions for the SimpleButton widget.
 AUTHOR:	Paul D. Johnston
 HISTORY:
 Date		Action
 ---------	------------------------------------
 10/21/91	Created.

********************************************************************/

#ifndef SIMPLEBUTTONP_H
#define SIMPLEBUTTONP_H

/*
 * Include the superclass private headers.
 */
#include <X11/CoreP.h>
#include <X11/Xc/Xc.h>
#include <X11/Xc/ControlP.h>

/*
 * Include this widget class public header.
 */
#include <X11/Xc/SimpleB.h>


/* Private declarations and definitions. */
#define MIN_SB_WIDTH		90
#define MIN_SB_HEIGHT		40
#define MAX_SB_WIDTH		150
#define MAX_SB_HEIGHT		70



/*
 * Class part.
 */
typedef struct 
{
   int dummy;	/* Minimum of one member required. */
} SimpleButtonClassPart;

/*
 * Class record.
 */
typedef struct _SimpleButtonClassRec
{
   CoreClassPart core_class;
   ControlClassPart control_class;
   SimpleButtonClassPart simpleButton_class;
} SimpleButtonClassRec;

/*
 * Declare the widget class record as external for use in the widget source
 * file.
 */
extern SimpleButtonClassRec simpleButtonClassRec;



/*
 * Instance part.
 */
typedef struct
{
   /* Public instance variables. */
   XtCallbackList callback;

   /* Private instance variables. */
   XRectangle face;			/* Geometry of the SimpleButton face */
   XPoint lbl;				/* Location of Label string */
   Boolean armed;			/* Flag for arming the SimpleButton. */

} SimpleButtonPart;

/*
 * Instance record.
 */
typedef struct _SimpleButtonRec
{
   CorePart core;
   ControlPart control;
   SimpleButtonPart simpleButton;
} SimpleButtonRec;




#endif  /* SIMPLEBUTTONP_H */

