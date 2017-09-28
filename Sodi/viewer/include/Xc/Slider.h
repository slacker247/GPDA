/*******************************************************************
 FILE:		Slider.h
 CONTENTS:	Public header file for the Slider widget.
 AUTHOR:	Paul D. Johnston
 HISTORY:
 Date		Action
 ---------	------------------------------------
 5/23/92	Changed the widget class name so that it is preceded
		by 'xc' with the first major word capitalized.
 3/26/92	Created.

********************************************************************/

#ifndef SLIDER_H
#define SLIDER_H

/* Superclass header */
#include <X11/Xc/Value.h>


/*
 * Define widget resource names, classes, and representation types.
 * Use these resource strings in your resource files.
 */
#define XcNnobColor		"nobColor"
#define XcNmaxZoomFactor	"maxZoomFactor"
#define XcCMaxZoomFactor	"MaxZoomFactor"
#define XcNenableAccept		"enableAccept"
#define XcCEnableAccept		"EnableAccept"



/* Class record declarations */

extern WidgetClass xcSliderWidgetClass;

typedef struct _SliderClassRec *SliderWidgetClass;
typedef struct _SliderRec *SliderWidget;


#endif

