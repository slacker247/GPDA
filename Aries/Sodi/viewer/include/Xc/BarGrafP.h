/*******************************************************************
 FILE:		BarGrafP.h
 CONTENTS:	Private definitions for the BarGraph widget.
 AUTHOR:	Paul D. Johnston
 HISTORY:
 Date		Action
 ---------	------------------------------------
 4/15/91	Created.

********************************************************************/

#ifndef BARGRAPHP_H
#define BARGRAPHP_H

/*
 * Include the superclass private headers.
 */
#include <X11/CoreP.h>
#include <X11/Xc/Xc.h>
#include <X11/Xc/ValueP.h>

/*
 * Include this widget class public header.
 */
#include <X11/Xc/BarGraf.h>


/* Private declarations and definitions. */
#define MIN_BG_WIDTH		50
#define MIN_BG_HEIGHT		90
#define MAX_BG_WIDTH		150
#define MAX_BG_HEIGHT		300

/* Max number of Scale segments. */
#define MIN_SCALE_SEGS		0
#define MAX_SCALE_SEGS		20


/*
 * Class part.
 */
typedef struct 
{
   int dummy;	/* Minimum of one member required. */
} BarGraphClassPart;

/*
 * Class record.
 */
typedef struct _BarGraphClassRec
{
   CoreClassPart core_class;
   ControlClassPart control_class;
   ValueClassPart value_class;
   BarGraphClassPart barGraph_class;
} BarGraphClassRec;

/*
 * Declare the widget class record as external for use in the widget source
 * file.
 */
extern BarGraphClassRec barGraphClassRec;



/*
 * Instance part.
 */
typedef struct
{
   /* Public instance variables. */
   XcOrient orient;			/* BarGraph's orientation: vertical
					 * or horizontal.
					 */
   int interval;			/* Time interval for updateCallback */
   XtCallbackList update_callback;	/* The updateCallback function. */
   Pixel bar_background;		/* Background color of the bar. */
   Pixel bar_foreground;		/* Foreground color of the bar. */
   Pixel scale_pixel;			/* Color of the Scale indicator. */
   int num_segments;			/* Number of segments in the Scale */
   Boolean value_visible;		/* Enable/Disable display of the 
					 * value in the Value Box.
					 */

   /* Private instance variables. */
   XRectangle face;			/* Geometry of the BarGraph face */
   XPoint lbl;				/* Location of the Label string */
   XRectangle bar;			/* Rectangle for the Bar indicator. */
   XSegment scale_line;			/* Scale line along Bar indicator. */
   XPoint segs[MAX_SCALE_SEGS+1];	/* Line segments for the Scale. */
   int seg_length;			/* Length of Scale line segments. */
   XPoint max_val;			/* Point at which to draw the max
					 * value string on the Scale.
					 */
   XPoint min_val;			/* Point at which to draw the min
					 * value string on the Scale.
					 */
   int interval_id;			/* Xt TimeOut interval ID. */

} BarGraphPart;

/*
 * Instance record.
 */
typedef struct _BarGraphRec
{
   CorePart core;
   ControlPart control;
   ValuePart value;
   BarGraphPart barGraph;
} BarGraphRec;


/* Declare widget class functions here. */


#endif  /* BARGRAPHP_H */

