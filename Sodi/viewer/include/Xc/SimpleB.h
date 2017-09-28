/*******************************************************************
 FILE:		SimpleB.h
 CONTENTS:	Public header file for the SimpleButton widget.
 AUTHOR:	Paul D. Johnston
 HISTORY:
 Date		Action
 ---------	------------------------------------
 5/23/92	Changed the widget class name so that it is preceded
		by 'xc' with the first major word capitalized.
 10/21/91	Created.

********************************************************************/

#ifndef SIMPLEBUTTON_H
#define SIMPLEBUTTON_H

/* Superclass header */
#include <X11/Xc/Control.h>


/*
 * Define widget resource names, classes, and representation types.
 * Use these resource strings in your resource files.
 */



/* Class record declarations */

extern WidgetClass xcSimpleButtonWidgetClass;

typedef struct _SimpleButtonClassRec *SimpleButtonWidgetClass;
typedef struct _SimpleButtonRec *SimpleButtonWidget;


#endif

