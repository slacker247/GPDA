/*******************************************************************
 FILE:		Led.h
 CONTENTS:	Public header file for the LedButton widget.
 AUTHOR:	Paul D. Johnston
 HISTORY:
 Date		Action
 ---------	------------------------------------
 10/21/91	Created.

********************************************************************/

#ifndef LED_H
#define LED_H

/* Superclass header */
#include <X11/Xc/Control.h>

#define MAX_COLOR_LEN		30
#define MAX_STR_LEN		40

/*
 * Define widget resource names, classes, and representation types.
 * Use these resource strings in your resource files.
 */
#define XcNstateForeground	"stateForeground"
#define XcNstateBackground	"stateBackground"
#define XcNledVisible		"ledVisible"
#define XcNstateVisible		"stateVisible"
#define XcNledColor		"ledColor"
#define XcNstate		"state"
#define XcCState		"State"
/* Class record declarations */

extern WidgetClass xcLedButtonWidgetClass;

typedef struct _LedButtonClassRec *LedButtonWidgetClass;
typedef struct _LedButtonRec *LedButtonWidget;

typedef struct _LedInfoRec
{
  char color[MAX_COLOR_LEN];
  char str[MAX_STR_LEN];
} XcLedInfoRec, *XcLedCallData;

/* function declaration */
extern void XcLedSetState();
 
#endif  /* LED_H */

