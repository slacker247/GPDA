/*******************************************************************
 FILE:		SliderP.h
 CONTENTS:	Private definitions for the Slider widget.
 AUTHOR:	Paul D. Johnston
 HISTORY:
 Date		Action
 ---------	------------------------------------
 10/21/91	Created.

********************************************************************/

#ifndef SLIDERP_H
#define SLIDERP_H

/*
 * Include the superclass private headers.
 */
#include <X11/CoreP.h>
#include <X11/Xc/Xc.h>
#include <X11/Xc/ValueP.h>

/*
 * Include this widget class public header.
 */
#include <X11/Xc/Slider.h>


/* Private declarations and definitions. */
#define MIN_SL_WIDTH		50
#define MIN_SL_HEIGHT		90
#define MAX_SL_WIDTH		150
#define MAX_SL_HEIGHT		300

/* Maximum zoom factor for fine-tuning values. */
#define MAX_ZOOM_FACTOR		50


/*
 * Class part.
 */
typedef struct 
{
   int dummy;	/* Minimum of one member required. */
} SliderClassPart;

/*
 * Class record.
 */
typedef struct _SliderClassRec
{
   CoreClassPart core_class;
   ControlClassPart control_class;
   ValueClassPart value_class;
   SliderClassPart slider_class;
} SliderClassRec;

/*
 * Declare the widget class record as external for use in the widget source
 * file.
 */
extern SliderClassRec sliderClassRec;



/*
 * Instance part.
 */
typedef struct
{
   /* Public instance variables. */
   XcOrient orient;			/* Slider's orientation: vertical
					 * or horizontal.
					 */
   Pixel nob_pixel;			/* Color of the Slider's Nob. */
   int max_factor;			/* Maximum Zoom Factor. */
   Boolean accept_enabled;		/* Enable/Disable Accept Button. */

   /* Private instance variables. */
   XRectangle face;			/* Geometry of the Slider face */
   XPoint lbl;				/* Location of the Label string */
   XRectangle nob;			/* Rectangle enclosing the Nob. */
   XRectangle chnl;			/* Rectangle enclosing the Channel. */
   XRectangle up_arrow;			/* Rectangle enclosing the Up Arrow. */
   XRectangle down_arrow;		/* Rectangle enclosing the Up Arrow. */
   XRectangle zoom;			/* Rectangle enclosing Zoom Factor. */
   XPoint zoom_lbl;			/* Point to draw zoom factor string. */
   XRectangle acpt;			/* Rectangle enclosing Accept Button. */
   Boolean nob_selected;		/* Nob selection flag */
   Boolean up_armed;			/* Up Zoom Button armed flag */
   Boolean down_armed;			/* Down Zoom Button armed flag */
   Boolean acpt_armed;			/* Accept Button armed flag */
   XcVType adjustment;			/* Fine adjustment of value. */
   int zoom_factor;			/* Zoom factor for fine-tuning */
   float zoom_window;			/* Zoom window set by zoom factor */
   float lower_zoom;			/* Lower bound of zoom window 
					 * around the current value.
					 */
   float upper_zoom;			/* Upper bound of zoom window 
					 * around the current value.
					 */

} SliderPart;

/*
 * Instance record.
 */
typedef struct _SliderRec
{
   CorePart core;
   ControlPart control;
   ValuePart value;
   SliderPart slider;
} SliderRec;


/* Declare widget class functions here. */


#endif  /* SLIDERP_H */

