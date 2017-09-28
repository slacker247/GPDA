/*******************************************************************
 FILE:		ValueButtonP.h
 CONTENTS:	Private definitions for the ValueButton widget.
 AUTHOR:	Paul D. Johnston
 HISTORY:
 Date		Action
 ---------	------------------------------------
 10/21/91	Created.

********************************************************************/

#ifndef VBUTTONP_H
#define VBUTTONP_H

/*
 * Include the superclass private headers.
 */
#include <X11/CoreP.h>
#include <X11/Xc/Xc.h>
#include <X11/Xc/ValueP.h>

/*
 * Include this widget class public header.
 */
#include <X11/Xc/VButton.h>


/* Private declarations and definitions. */
#define MIN_VB_WIDTH		90
#define MIN_VB_HEIGHT		40
#define MAX_VB_WIDTH		150
#define MAX_VB_HEIGHT		70


/*
 * Class part.
 */
typedef struct 
{
   int dummy;	/* Minimum of one member required. */
} ValueButtonClassPart;

/*
 * Class record.
 */
typedef struct _ValueButtonClassRec
{
   CoreClassPart core_class;
   ControlClassPart control_class;
   ValueClassPart value_class;
   ValueButtonClassPart valueButton_class;
} ValueButtonClassRec;

/*
 * Declare the widget class record as external for use in the widget source
 * file.
 */
extern ValueButtonClassRec valueButtonClassRec;



/*
 * Instance part.
 */
typedef struct
{
   /* Public instance variables. */
   /* NONE */

   /* Private instance variables. */
   XRectangle face;			/* Geometry of the ValueButton face */
   XPoint lbl;				/* Location of Label string */
   XRectangle acpt;			/* Rectangle enclosing Accept Button. */
   XRectangle up_arrow;			/* Rectangle enclosing Up Arrow. */
   XRectangle down_arrow;		/* Rectangle enclosing Down Arrow. */
   Boolean acpt_armed;			/* Flag for arming the Accept Button */
   Boolean up_armed;			/* Flag for arming the Up Arrow. */
   Boolean down_armed;			/* Flag for arming the Down Arrow. */
   int interval_id;			/* X TimeOut Event interval ID. */

} ValueButtonPart;

/*
 * Instance record.
 */
typedef struct _ValueButtonRec
{
   CorePart core;
   ControlPart control;
   ValuePart value;
   ValueButtonPart valueButton;
} ValueButtonRec;



#endif  /* VBUTTONP_H */

