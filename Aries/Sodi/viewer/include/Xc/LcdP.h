/*******************************************************************
 FILE:		LcdP.h
 CONTENTS:	Private definitions for the Lcd widget.
 AUTHOR:	Paul D. Johnston
 HISTORY:
 Date		Action
 ---------	------------------------------------
 10/21/91	Created.

********************************************************************/

#ifndef LCDP_H
#define LCDP_H

/*
 * Include the superclass private headers.
 */
#include <X11/CoreP.h>
#include <X11/Xc/Xc.h>
#include <X11/Xc/ValueP.h>

/*
 * Include this widget class public header.
 */
#include <X11/Xc/Lcd.h>


/* Private declarations and definitions. */
#define MIN_LCD_WIDTH		50
#define MIN_LCD_HEIGHT		90
#define MAX_LCD_WIDTH		150
#define MAX_LCD_HEIGHT		300

/* Maximum number of displayed value characters. */
#define MAX_CHARS		20

/* Number of LCD segments in each character. */
#define NUM_SEGS		9

/* Line width of each segment in the LCD display. */
#define MAX_SEG_WIDTH		10
#define MIN_SEG_WIDTH		1

/* Number of characters in the LCD character set. */
#define MAX_CHAR_TABLE_LEN	19


/* This enumeration defines the possible types of segments within the LCD. */
typedef enum
{
   TOP_SEG = 1,
   BOTTOM_SEG,
   LEFT_SEG,
   RIGHT_SEG,
   MIDDLE_SEG,
   PLUS_SEG,
   DOT_SEG
} Seg_type;


/*
 * Class part.
 */
typedef struct 
{
   int dummy;	/* Minimum of one member required. */
} LcdClassPart;

/*
 * Class record.
 */
typedef struct _LcdClassRec
{
   CoreClassPart core_class;
   ControlClassPart control_class;
   ValueClassPart value_class;
   LcdClassPart lcd_class;
} LcdClassRec;

/*
 * Declare the widget class record as external for use in the widget source
 * file.
 */
extern LcdClassRec lcdClassRec;


/* Structure used for holding info about each segment. */
typedef struct seg_s
{
   Seg_type stype;	/* Type of segment. */
   XSegment points;	/* Begin/end points for segment's line */
} Seg_struct;


/*
 * Instance part.
 */
typedef struct
{
   /* Public instance variables. */
   int value_length;			/* Max length (in chars) of the  
					 * value string.
					 */
   int interval;			/* Update interval in milliseconds */
   XtCallbackList update_callback;	/* Update callback function. */

   /* Private instance variables. */
   XRectangle face;			/* Geometry of the Lcd face */
   XPoint lbl;				/* Location of the Label string */
   short seg_width;			/* Line width of each segment */
   short char_width;			/* Width of each value character */
   XRectangle chars[MAX_CHARS];		/* Geometry of each character */
   XRectangle display;			/* Geometry of the LCD display */
   Seg_struct segs[NUM_SEGS];		/* Points for each segment's polygon */
   XtIntervalId interval_id;		/* ID for widget TimeOut procedure. */

} LcdPart;

/*
 * Instance record.
 */
typedef struct _LcdRec
{
   CorePart core;
   ControlPart control;
   ValuePart value;
   LcdPart lcd;
} LcdRec;


/* Declare widget class functions here. */


#endif  /* LCDP_H */

