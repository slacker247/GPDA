/*******************************************************************
 FILE:		BarGraf.h
 CONTENTS:	Public header file for the BarGraph widget.
 AUTHOR:	Paul D. Johnston
 HISTORY:
 Date		Action
 ---------	------------------------------------
 5/23/92	Changed the widget class name so that it is preceded
		by 'xc' with the first major word capitalized.
 4/15/92	Created.

********************************************************************/

#ifndef BARGRAPH_H
#define BARGRAPH_H

/* Superclass header */
#include <X11/Xc/Value.h>


/*
 * Define widget resource names, classes, and representation types.
 * Use these resource strings in your resource files.
 */
#define XcNbarBackground	"barBackground"
#define XcNbarForeground	"barForeground"
#define XcNscaleColor		"scaleColor"
#define XcNscaleSegments	"scaleSegments"
#define XcCScaleSegments	"ScaleSegments"
#define XcNvalueVisible		"valueVisible"



/* Class record declarations */

extern WidgetClass xcBarGraphWidgetClass;

typedef struct _BarGraphClassRec *BarGraphWidgetClass;
typedef struct _BarGraphRec *BarGraphWidget;


/* Widget functions */
extern void XcBGUpdateValue();

#endif

