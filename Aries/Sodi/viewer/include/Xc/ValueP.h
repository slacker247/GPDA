/*******************************************************************
 FILE:		ValueP.h
 CONTENTS:	Private definitions for the Value widget class.
 AUTHOR:	Paul D. Johnston
 HISTORY:
 Date		Action
 ---------	------------------------------------
 3/11/92	Created.

********************************************************************/

#ifndef VALUEP_H
#define VALUEP_H

/*
 * Include the superclass private header.
 */
#include <X11/CoreP.h>
#include <X11/Xc/Xc.h>
#include <X11/Xc/ControlP.h>


/*
 * Include this widget class public header.
 */
#include <X11/Xc/Value.h>


/* Maximum and minimum decimal precision settings. */
#define MIN_DECIMALS		1
#define MAX_DECIMALS		6


/*
 * Class part.
 */
typedef struct 
{
   int dummy;	/* Minimum of one member required. */
} ValueClassPart;

/*
 * Class record.
 */
typedef struct _ValueClassRec
{
   CoreClassPart core_class;
   ControlClassPart control_class;
   ValueClassPart value_class;
} ValueClassRec;

/*
 * Declare the widget class record as external for use in the widget source
 * file.
 */
extern ValueClassRec valueClassRec;



/*
 * Instance part.
 */
typedef struct
{
   /* Public instance variables. */
   Pixel value_fg_pixel;		/* Value string foreground color */
   Pixel value_bg_pixel;		/* Value Box background color */
   XtCallbackList callback;		/* Widget callbacks */
   XcDType datatype;			/* Value's data type */
   int decimals;			/* No. of decimal points (if Fval). */
   XcVType increment;			/* Widget's increment. */
   XcVType upper_bound;			/* Upper range limit. */
   XcVType lower_bound;			/* Lower range limit. */
   XcVType val;				/* Storage for the current value
					 * manipulated by the widget.
					 */
   XcValueJustify justify;		/* Left, Right, or Center justification
					 * of the value within the Value Box.
					 */
   /* Private instance variables. */
   XRectangle value_box;		/* Value Box to display value in. */
   XPoint vp;				/* Place in Value Box to draw value. */

} ValuePart;

/*
 * Instance record.
 */
typedef struct _ValueRec
{
   CorePart core;
   ControlPart control;
   ValuePart value;
} ValueRec;



#endif  /* VALUEP_H */

