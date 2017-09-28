/*******************************************************************
 FILE:		ToggleB.h
 CONTENTS:	Public header file for the ToggleButton widget.
 AUTHOR:	Paul D. Johnston
 HISTORY:
 Date		Action
 ---------	------------------------------------
 5/23/92	Changed the widget class name so that it is preceded
		by 'xc' with the first major word capitalized.
 10/21/91	Created.

********************************************************************/

#ifndef TOGGLEBUTTON_H
#define TOGGLEBUTTON_H

/* Superclass header */
#include <X11/Xc/Control.h>


#define MAX_COLOR_STR		20

/*
 * Define widget resource names, classes, and representation types.
 * Use these resource strings in your resource files.
 */
#define XcNonColor		"onColor"
#define XcNoffColor		"offColor"
#define XcNstate		"state"
#define XcCState		"State"
#define XcRToggleState		"XcToggleState"
#define XcEon			"on"
#define XcEoff			"off"
#define XcCColor		"Color"

/* Class record declarations */

extern WidgetClass xcToggleButtonWidgetClass;

typedef struct _ToggleButtonClassRec *ToggleButtonWidgetClass;
typedef struct _ToggleButtonRec *ToggleButtonWidget;

/* 
 * This enumeration defines the possible Toggle States (on of off).
 */
typedef enum 
{
   XcOn,
   XcOff
} XcToggleState;


/* function declaration */
extern void XcToggleSet();
extern void XcToggleGet();

#endif

