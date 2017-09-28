/*******************************************************************
 FILE:		ValueButton.h
 CONTENTS:	Public header file for the ValueButton widget.
 AUTHOR:	Paul D. Johnston
 HISTORY:
 Date		Action
 ---------	------------------------------------
 5/23/92	Changed the widget class name so that it is preceded
		by 'xc' with the first major word capitalized.
 10/21/91	Created.

********************************************************************/

#ifndef VBUTTON_H
#define VBUTTON_H

/* Superclass header */
#include <X11/Xc/Value.h>


/*
 * Define widget resource names, classes, and representation types.
 * Use these resource strings in your resource files.
 */
/* NONE */


/* Class record declarations */

extern WidgetClass xcValueButtonWidgetClass;

typedef struct _ValueButtonClassRec *ValueButtonWidgetClass;
typedef struct _ValueButtonRec *ValueButtonWidget;


#endif

