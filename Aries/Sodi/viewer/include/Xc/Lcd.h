/*******************************************************************
 FILE:		Lcd.h
 CONTENTS:	Public header file for the Lcd widget.
 AUTHOR:	Paul D. Johnston
 HISTORY:
 Date		Action
 ---------	------------------------------------
 5/23/92	Changed the widget class name so that it is preceded
		by 'xc' with the first major word capitalized.
 4/04/92	Created.

********************************************************************/

#ifndef LCD_H
#define LCD_H

/* Superclass header */
#include "Xc/Value.h"


/*
 * Define widget resource names, classes, and representation types.
 * Use these resource strings in your resource files.
 */
#define XcNvalueLength		"valueLength"
#define XcCValueLength		"ValueLength"



/* Class record declarations */

extern WidgetClass xcLcdWidgetClass;

typedef struct _LcdClassRec *LcdWidgetClass;
typedef struct _LcdRec *LcdWidget;


/* Widget public functions */
extern void XcLcdUpdateValue(_WidgetRec *, v_t *);


#endif

