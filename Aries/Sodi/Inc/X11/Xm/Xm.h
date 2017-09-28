/**
 *
 * $Id: Xm.h.in,v 1.9 1999/11/04 11:43:44 danny Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/


#ifndef XM_H
#define XM_H

#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/Xatom.h>
#include <Xm/XmStrDefs.h>
#include <Xm/VendorS.h>

#ifdef __cplusplus
extern "C" {
#endif

/* handy little define */

#if NeedFunctionPrototypes
#define _XmANSI_ARGS_(args) args
#else
#define _XmANSI_ARGS_(args) ()
#endif

/* Version Information */

#define LESSTIF_VERSION 0
#define LESSTIF_REVISION 89

#define LesstifVersion (LESSTIF_VERSION * 1000 + LESSTIF_REVISION)
#define LesstifVERSION_STRING "@(#)GNU/LessTif Version 1.2 Release 0.89.4"

/* since we're replacing Motif 1.2... */
#define XmVERSION 1
#define XmREVISION 2
#define XmVersion (XmVERSION * 1000 + XmREVISION)
#define XmVERSION_STRING LesstifVERSION_STRING
#define XmUPDATE_LEVEL 0

extern int xmUseVersion;

/* Sometimes we have to change the Lesstif binary interface, but we
 * can't really change the Motif version number (i.e., library version number).
 * If you configure with --enable-back-compat you will retain the original
 * Lesstif binary interface.
 */
#define LESSTIF_BACK_COMPAT	0

/* pixmap stuff */

#define XmUNSPECIFIED_PIXMAP	2

/* define for easy checking for unset resources */

#define XmUNSPECIFIED (~0)

/*
 * vendor defines (hey, GNU is a vendor)
 */
#define XmSTRING_OS_CHARSET             XmSTRING_ISO8859_1
#ifndef XmFALLBACK_CHARSET
#define XmFALLBACK_CHARSET              XmSTRING_ISO8859_1
#endif

#define XmDEFAULT_FONT                  _XmSDEFAULT_FONT
#define XmDEFAULT_BACKGROUND            _XmSDEFAULT_BACKGROUND
#define XmDEFAULT_DARK_THRESHOLD        15
#define XmDEFAULT_LIGHT_THRESHOLD       77
#define XmDEFAULT_FOREGROUND_THRESHOLD  35

extern char    _XmSDEFAULT_FONT[];
extern char    _XmSDEFAULT_BACKGROUND[];

/*
 * XmString stuff
 */

typedef enum {
    XmFONT_IS_FONT,
    XmFONT_IS_FONTSET
} XmFontType;

enum {
    XmSTRING_DIRECTION_L_TO_R, 
    XmSTRING_DIRECTION_R_TO_L,
    XmSTRING_DIRECTION_DEFAULT = 255
};

enum {
    XmSTRING_COMPONENT_UNKNOWN,
    XmSTRING_COMPONENT_CHARSET,
    XmSTRING_COMPONENT_TEXT,
    XmSTRING_COMPONENT_DIRECTION,
    XmSTRING_COMPONENT_SEPARATOR,
    XmSTRING_COMPONENT_LOCALE_TEXT
    /* 6 - 125 is said to be reserved */
};

#define XmSTRING_COMPONENT_END          ((XmStringComponentType) 126)
/* anybody know what 127 is? */
#define XmSTRING_COMPONENT_USER_BEGIN   ((XmStringComponentType) 128)
/* 128-255 are user tags */
#define XmSTRING_COMPONENT_USER_END     ((XmStringComponentType) 255)

typedef unsigned char XmStringDirection;
typedef unsigned char *XmString;
typedef XmString *XmStringTable;
typedef char *XmStringCharSet;
typedef unsigned char XmStringComponentType;

typedef struct _XmFontListRec *XmFontListEntry, *XmFontList;
typedef struct __XmStringContextRec *_XmStringContext;
typedef struct __XmStringRec *_XmString;
typedef struct _XmtStringContextRec *XmStringContext;
typedef struct _XmFontListContextRec *XmFontContext;

/*
 * make sure the big three are always defined
 */

extern WidgetClass xmPrimitiveWidgetClass;

typedef struct _XmPrimitiveClassRec *XmPrimitiveWidgetClass;
typedef struct _XmPrimitiveRec      *XmPrimitiveWidget;

extern WidgetClass xmGadgetClass;

typedef struct _XmGadgetClassRec *XmGadgetClass;
typedef struct _XmGadgetRec      *XmGadget;

extern WidgetClass xmManagerWidgetClass;

typedef struct _XmManagerClassRec *XmManagerWidgetClass;
typedef struct _XmManagerRec      *XmManagerWidget;

#ifndef XmIsPrimitive
#define XmIsPrimitive(w)        XtIsSubclass(w, xmPrimitiveWidgetClass)
#endif

#ifndef XmIsGadget
#define XmIsGadget(w)           XtIsSubclass(w, xmGadgetClass)
#endif

#ifndef XmIsManager
#define XmIsManager(w)          XtIsSubclass(w, xmManagerWidgetClass)
#endif


/* unitType stuff */

enum {
    XmPIXELS,
    Xm100TH_MILLIMETERS,
    Xm1000TH_INCHES,
    Xm100TH_POINTS,
    Xm100TH_FONT_UNITS
};

/* delete Responses for VendorShell */

enum {
    XmDESTROY,
    XmUNMAP,
    XmDO_NOTHING
};

/* keyboard focus policies for VendorShell */

enum {
    XmEXPLICIT,
    XmPOINTER
};

/* Navigation stuff */

enum {
    XmNONE,
    XmTAB_GROUP,
    XmSTICKY_TAB_GROUP,
    XmEXCLUSIVE_TAB_GROUP
};

/* Audible Warning types for VendorShell */

enum {
/* implied
 *  XmNONE
 */
    XmBELL = 1
};

/* various widgets' orientation, menu definitions */

enum {
    XmNO_ORIENTATION,
    XmVERTICAL,
    XmHORIZONTAL
};

/* row column types */

enum {
    XmWORK_AREA,
    XmMENU_BAR,
    XmMENU_PULLDOWN,
    XmMENU_POPUP,
    XmMENU_OPTION
};

/* row column packing strategies */

enum {
    XmNO_PACKING,
    XmPACK_TIGHT,
    XmPACK_COLUMN,
    XmPACK_NONE
};

enum {
/* implied
 *  XmALIGNMENT_BASELINE_TOP,
 *  XmALIGNMENT_CENTER,
 *  XmALIGNMENT_BASELINE_BOTTOM,
 */
    XmALIGNMENT_CONTENTS_TOP = 3,
    XmALIGNMENT_CONTENTS_BOTTOM
};

enum {
    XmTEAR_OFF_ENABLED,
    XmTEAR_OFF_DISABLED
};

enum {
    XmUNPOST,
    XmUNPOST_AND_REPLAY
};

/* XmPanedWindow positioning */
enum {
	XmLAST_POSITION = -1,
	XmFIRST_POSITION
};

/* Label and Frame alignments */

enum {
    XmALIGNMENT_BEGINNING,
    XmALIGNMENT_CENTER,
    XmALIGNMENT_END
};

enum {
    XmALIGNMENT_BASELINE_TOP,
/* implied
 *  XmALIGNMENT_CENTER
 */
    XmALIGNMENT_BASELINE_BOTTOM = 2,
    XmALIGNMENT_WIDGET_TOP,
    XmALIGNMENT_WIDGET_BOTTOM
};

/* Frame Child Types */

enum {
    XmFRAME_GENERIC_CHILD,
    XmFRAME_WORKAREA_CHILD,
    XmFRAME_TITLE_CHILD
};

/* For toggle button stuff */
enum {
#if LESSTIF_BACK_COMPAT
    XmN_OF_MANY,
#else
    XmN_OF_MANY = 1,
#endif
    XmONE_OF_MANY
};

/* Form attachments */

enum {
    XmATTACH_NONE,
    XmATTACH_FORM,
    XmATTACH_OPPOSITE_FORM,
    XmATTACH_WIDGET,
    XmATTACH_OPPOSITE_WIDGET,
    XmATTACH_POSITION,
    XmATTACH_SELF
};

/* resize policies for some manager widgets */

enum {
    XmRESIZE_NONE,
    XmRESIZE_GROW,
    XmRESIZE_ANY
};

/* callback reasons */

enum {
    XmCR_NONE,			/* 0 */
    XmCR_HELP,			/* 1 */
    XmCR_VALUE_CHANGED,		/* 2 */
    XmCR_INCREMENT,		/* 3 */
    XmCR_DECREMENT,		/* 4 */
    XmCR_PAGE_INCREMENT,	/* 5 */
    XmCR_PAGE_DECREMENT,	/* 6 */
    XmCR_TO_TOP,		/* 7 */
    XmCR_TO_BOTTOM,		/* 8 */
    XmCR_DRAG,			/* 9 */
    XmCR_ACTIVATE,		/* 10 */
    XmCR_ARM,			/* 11 */
    XmCR_DISARM,		/* 12 */
    XmCR_DUMMY13,		/* 13 */
    XmCR_DUMMY14,		/* 14 */
    XmCR_DUMMY15,		/* 15 */
    XmCR_MAP,			/* 16 */
    XmCR_UNMAP,			/* 17 */
    XmCR_FOCUS,			/* 18 */
    XmCR_LOSING_FOCUS,		/* 19 */
    XmCR_MODIFYING_TEXT_VALUE,	/* 20 */
    XmCR_MOVING_INSERT_CURSOR,	/* 21 */
    XmCR_EXECUTE,		/* 22 */
    XmCR_SINGLE_SELECT,		/* 23 */
    XmCR_MULTIPLE_SELECT,	/* 24 */
    XmCR_EXTENDED_SELECT,	/* 25 */
    XmCR_BROWSE_SELECT,		/* 26 */
    XmCR_DEFAULT_ACTION,	/* 27 */
    XmCR_CLIPBOARD_DATA_REQUEST,/* 28 */
    XmCR_CLIPBOARD_DATA_DELETE,	/* 29 */
    XmCR_CASCADING,		/* 30 */
    XmCR_OK,			/* 31 */
    XmCR_CANCEL,		/* 32 */
    XmCR_DUMMY33,		/* 33 */
    XmCR_APPLY,			/* 34 */
    XmCR_NO_MATCH,		/* 35 */
    XmCR_COMMAND_ENTERED,	/* 36 */
    XmCR_COMMAND_CHANGED,	/* 37 */
    XmCR_EXPOSE,		/* 38 */
    XmCR_RESIZE,		/* 39 */
    XmCR_INPUT,			/* 40 */
    XmCR_GAIN_PRIMARY,		/* 41 */
    XmCR_LOSE_PRIMARY,		/* 42 */
    XmCR_CREATE,		/* 43 */
    XmCR_TEAR_OFF_ACTIVATE,	/* 44 */
    XmCR_TEAR_OFF_DEACTIVATE,	/* 45 */
    XmCR_OBSCURED_TRAVERSAL,	/* 46 */
    XmCR_PROTOCOLS		/* 47 */
};

/*
 * callback structures
 */

typedef struct {
    int reason;
    XEvent *event;
} XmAnyCallbackStruct;

typedef struct {
    int reason;
    XEvent *event;
    int click_count;
} XmArrowButtonCallbackStruct;

typedef struct {
    int reason;
    XEvent *event;
    XmString value;
    int length;
} XmCommandCallbackStruct;

typedef struct {
    int reason;
    XEvent *event;
    Window window;
} XmDrawingAreaCallbackStruct;

typedef struct
{
    int     reason;
    XEvent  *event;
    Window  window;
    int     click_count;
} XmDrawnButtonCallbackStruct;

typedef struct {
    int reason;
    XEvent *event;
    XmString value;
    int length;
    XmString mask;
    int mask_length;
    XmString dir;
    int dir_length;
    XmString pattern;
    int pattern_length;
} XmFileSelectionBoxCallbackStruct;

typedef struct {
    int reason;                   /* reason callback was called */
    XEvent *event;                /* points to event structure */
    XmString item;                /* item most recently selected */
    int item_length;              /* number of bytes in item member */
    int item_position;            /* item's position in XmNitems */
    XmString *selected_items;     /* list of items selected */
    int selected_item_count;      /* number of items in selected_items */
    int *selected_item_positions; /* array of ints marking selected items */
    int selection_type;           /* type of most recent selection */
} XmListCallbackStruct;

typedef struct {
    int reason;                   /* reason callback was called */
    XEvent *event;
    int click_count;
} XmPushButtonCallbackStruct;

typedef struct {
    int reason;
    XEvent *event;
    Widget widget;
    char *data;
    char *callbackstruct;
} XmRowColumnCallbackStruct;


typedef struct {
    int reason;                   /* reason callback was called */
    XEvent *event;
    int value;
} XmScaleCallbackStruct;

typedef struct {
    int reason;
    XEvent *event;
    int value;
    int pixel;
} XmScrollBarCallbackStruct;

typedef struct {
    int reason;
    XEvent *event;
    XmString value;
    int length;
} XmSelectionBoxCallbackStruct;

typedef struct {
    int reason;                   /* reason callback was called */
    XEvent *event;
    int set;
} XmToggleButtonCallbackStruct;

/* multiclick */

enum {
    XmMULTICLICK_DISCARD,
    XmMULTICLICK_KEEP
};

/* Drawn button overrides some of the ShadowTypes */
enum {
    XmSHADOW_IN = 7,
    XmSHADOW_OUT
};

/* Arrow button directions */
enum {
    XmARROW_UP,
    XmARROW_DOWN,
    XmARROW_LEFT,
    XmARROW_RIGHT
};

/* Shadow/Separator types */

enum {
    XmNO_LINE,
    XmSINGLE_LINE,
    XmDOUBLE_LINE,
    XmSINGLE_DASHED_LINE,
    XmDOUBLE_DASHED_LINE,
    XmSHADOW_ETCHED_IN,
    XmSHADOW_ETCHED_OUT,
    XmSHADOW_ETCHED_IN_DASH,
    XmSHADOW_ETCHED_OUT_DASH,
    XmINVALID_SEPARATOR_TYPE
};

/* XmLabel types */

enum {
    XmPIXMAP = 1,
    XmSTRING
};

/* DragDrop */
enum {
    XmWINDOW,
/* implied
 *  XmPIXMAP
 */
    XmCURSOR = 2
};

/* maximum value resources */

enum {
    XmMAX_ON_TOP,
    XmMAX_ON_BOTTOM,
    XmMAX_ON_LEFT,
    XmMAX_ON_RIGHT
};

/* list selection policies */

enum {
    XmSINGLE_SELECT,
    XmMULTIPLE_SELECT,
    XmEXTENDED_SELECT,
    XmBROWSE_SELECT
};

enum {
    XmSTATIC,
    XmDYNAMIC
};

/* scrolled window policy stuff */

enum {
    XmVARIABLE,
    XmCONSTANT,
    XmRESIZE_IF_POSSIBLE 
};

enum {
    XmAUTOMATIC,
    XmAPPLICATION_DEFINED
};

enum {
/* implied
 *  XmSTATIC,
 */
    XmAS_NEEDED = 1
};

enum {
    XmBOTTOM_RIGHT,
    XmTOP_RIGHT,
    XmBOTTOM_LEFT,
    XmTOP_LEFT
};

/* main window command window locations */

enum {
    XmCOMMAND_ABOVE_WORKSPACE,
    XmCOMMAND_BELOW_WORKSPACE
};

/* edit modes for text widgets */

enum {
    XmMULTI_LINE_EDIT,
    XmSINGLE_LINE_EDIT
};

/* text directions */

typedef enum {
    XmTEXT_FORWARD,
    XmTEXT_BACKWARD
} XmTextDirection;

typedef long XmTextPosition;
typedef Atom XmTextFormat;

#define XmFMT_8_BIT	((XmTextFormat) XA_STRING)
#define XmFMT_16_BIT	((XmTextFormat) 2)  /* they _mean_ XA_SECONDARY ??? */

/*
 * something about backwards compatibility... besides, xmcd needs these
 */
#define FMT8BIT  XmFMT_8_BIT
#define FMT16BIT XmFMT_16_BIT

#define XmFMT_8_BIT	((XmTextFormat) XA_STRING)
#define XmFMT_16_BIT	((XmTextFormat) 2)  /* they _mean_ XA_SECONDARY ??? */

/* Stuff for Text Widgets */
typedef enum {
    XmSELECT_POSITION,
    XmSELECT_WHITESPACE,
    XmSELECT_WORD,
    XmSELECT_LINE,
    XmSELECT_ALL,
    XmSELECT_PARAGRAPH
} XmTextScanType;

/* highlight mode for text and textfield widgets */

typedef enum {
    XmHIGHLIGHT_NORMAL,
    XmHIGHLIGHT_SELECTED,
    XmHIGHLIGHT_SECONDARY_SELECTED
} XmHighlightMode;

typedef struct {
    char *ptr;
    int length;
    XmTextFormat format;
} XmTextBlockRec, *XmTextBlock;

/* keep the members comma separated, as that can be endian dependent */
typedef struct {
    int  reason;   
    XEvent *event;
    Boolean doit;
    XmTextPosition currInsert, newInsert;
    XmTextPosition startPos, endPos;
    XmTextBlock text;
} XmTextVerifyCallbackStruct, *XmTextVerifyPtr;

typedef struct {
    wchar_t *wcsptr;
    int length;
} XmTextBlockRecWcs, *XmTextBlockWcs;

typedef struct {
    int  reason;   
    XEvent *event;
    Boolean doit;
    XmTextPosition currInsert, newInsert;
    XmTextPosition startPos, endPos;
    XmTextBlockWcs text;
} XmTextVerifyCallbackStructWcs, *XmTextVerifyPtrWcs;

#define XmCOPY_FAILED           0
#define XmCOPY_SUCCEEDED        1
#define XmCOPY_TRUNCATED        2

/* dialog child types */

enum {
    XmDIALOG_NONE,
    XmDIALOG_APPLY_BUTTON,
    XmDIALOG_CANCEL_BUTTON,
    XmDIALOG_DEFAULT_BUTTON,
    XmDIALOG_OK_BUTTON,
    XmDIALOG_FILTER_LABEL,
    XmDIALOG_FILTER_TEXT,
    XmDIALOG_HELP_BUTTON,
    XmDIALOG_LIST,
    XmDIALOG_LIST_LABEL,
    XmDIALOG_MESSAGE_LABEL,
    XmDIALOG_SELECTION_LABEL,
    XmDIALOG_SYMBOL_LABEL,
    XmDIALOG_TEXT,
    XmDIALOG_SEPARATOR,
    XmDIALOG_DIR_LIST,
    XmDIALOG_DIR_LIST_LABEL
};

#define XmDIALOG_COMMAND_TEXT    XmDIALOG_TEXT
#define XmDIALOG_FILE_LIST       XmDIALOG_LIST
#define XmDIALOG_FILE_LIST_LABEL XmDIALOG_LIST_LABEL
#define XmDIALOG_HISTORY_LIST    XmDIALOG_LIST
#define XmDIALOG_PROMPT_LABEL    XmDIALOG_SELECTION_LABEL
#define XmDIALOG_VALUE_TEXT      XmDIALOG_TEXT

/* dialog styles */
enum {
    XmDIALOG_MODELESS,
    XmDIALOG_PRIMARY_APPLICATION_MODAL,
    XmDIALOG_FULL_APPLICATION_MODAL,
    XmDIALOG_SYSTEM_MODAL
};

/* this is obsolete.  Use XmDIALOG_PRIMARY_APPLICATION_MODAL */
#define XmDIALOG_APPLICATION_MODAL XmDIALOG_PRIMARY_APPLICATION_MODAL

/* child placements (for selection boxes) */

enum {
    XmPLACE_TOP,
    XmPLACE_ABOVE_SELECTION,
    XmPLACE_BELOW_SELECTION
};

/* file type masks for filesb */
#define XmFILE_DIRECTORY    (1 << 0)
#define XmFILE_REGULAR      (1 << 1)
#define XmFILE_ANY_TYPE     (XmFILE_DIRECTORY | XmFILE_REGULAR)

/* defines for selection dialog type: */

enum {
    XmDIALOG_WORK_AREA,
    XmDIALOG_PROMPT,
    XmDIALOG_SELECTION,
    XmDIALOG_COMMAND,
    XmDIALOG_FILE_SELECTION
};

/* dialog types */
enum {
    XmDIALOG_TEMPLATE,
    XmDIALOG_ERROR,
    XmDIALOG_INFORMATION,
    XmDIALOG_MESSAGE,
    XmDIALOG_QUESTION,
    XmDIALOG_WARNING,
    XmDIALOG_WORKING
};

/* traversal stuff */

typedef enum {
    XmVISIBILITY_UNOBSCURED,
    XmVISIBILITY_PARTIALLY_OBSCURED,
    XmVISIBILITY_FULLY_OBSCURED
} XmVisibility;

typedef enum {
    XmTRAVERSE_CURRENT,
    XmTRAVERSE_NEXT,
    XmTRAVERSE_PREV,
    XmTRAVERSE_HOME,
    XmTRAVERSE_NEXT_TAB_GROUP,
    XmTRAVERSE_PREV_TAB_GROUP,
    XmTRAVERSE_UP,
    XmTRAVERSE_DOWN,
    XmTRAVERSE_LEFT,
    XmTRAVERSE_RIGHT
} XmTraversalDirection;

typedef struct {
    int reason;
    XEvent *event;
    Widget traversal_destination;
    XmTraversalDirection direction;
} XmTraverseObscuredCallbackStruct;

typedef unsigned char XmNavigationType;

/* simple menu stuff */

typedef unsigned char XmButtonType;
typedef XmButtonType * XmButtonTypeTable;
typedef KeySym * XmKeySymTable;
typedef XmStringCharSet * XmStringCharSetTable;

enum {
    XmPUSHBUTTON = 1,
    XmTOGGLEBUTTON,
    XmRADIOBUTTON,
    XmCASCADEBUTTON,
    XmSEPARATOR,
    XmDOUBLE_SEPARATOR,
    XmTITLE
};

#define XmCHECKBUTTON	XmTOGGLEBUTTON

/* Stuff needed by the base class stuff in BaseClass.c */

typedef XtPointer (*XmResourceBaseProc)(Widget w, XtPointer);

typedef struct _XmSecondaryResourceDataRec {
    XmResourceBaseProc base_proc;
    XtPointer client_data;
    String name;
    String res_class;
    XtResourceList resources;
    Cardinal num_resources;
} XmSecondaryResourceDataRec, *XmSecondaryResourceData;

Cardinal XmGetSecondaryResourceData(WidgetClass wc,
				    XmSecondaryResourceData **resData);

/************************ ImageCache.c ***************************/
Pixmap XmGetPixmap(Screen *screen, char *image_name, Pixel foreground, Pixel background);
Pixmap XmGetPixmapByDepth(Screen *screen, char *image_name, Pixel foreground, Pixel background, int depth);
Boolean XmDestroyPixmap(Screen *screen, Pixmap pixmap);
Boolean XmInstallImage(XImage *image, char *image_name);
Boolean XmUninstallImage(XImage *image);

/************************** Manager.c *****************************/

void XmUpdateDisplay(Widget w);

/************************* Primitive.c ****************************/

typedef long XmOffset;
typedef XmOffset *XmOffsetPtr;

void XmResolvePartOffsets(WidgetClass w_class, XmOffsetPtr *offset);
void XmResolveAllPartOffsets(WidgetClass w_class, XmOffsetPtr *offset,
			     XmOffsetPtr *constraint_offset);
Boolean XmWidgetGetBaselines(Widget wid, Dimension **baselines,
			     int *line_count);
Boolean XmWidgetGetDisplayRect(Widget wid, XRectangle *displayrect);

/************************ ResConvert.c ****************************/

void XmRegisterConverters(void);
void XmCvtStringToUnitType(XrmValuePtr args, Cardinal *num_args, XrmValue *from_val, XrmValue *to_val);
char *XmRegisterSegmentEncoding(char *fontlist_tag, char *ct_encoding);
char *XmMapSegmentEncoding(char *fontlist_tag);
XmString XmCvtCTToXmString(char *text);
Boolean XmCvtTextToXmString(Display *display, XrmValuePtr args,
			    Cardinal *num_args, XrmValue *from_val,
			    XrmValue *to_val, XtPointer *converter_data);
char *XmCvtXmStringToCT(XmString string);
Boolean XmCvtXmStringToText(Display *display, XrmValuePtr args,
			    Cardinal *num_args, XrmValue *from_val,
			    XrmValue *to_val, XtPointer *converter_data);

/************************** ResInd.c ******************************/

int XmConvertUnits(Widget widget, int orientation,
		   int from_unit_type, int from_value, int to_unit_type);
int  XmCvtToHorizontalPixels(Screen *screen, int from_val, int from_type);
int  XmCvtToVerticalPixels(Screen *screen, int from_val, int from_type);
int  XmCvtFromHorizontalPixels(Screen *screen, int from_val, int to_type);
int  XmCvtFromVerticalPixels(Screen *screen, int from_val, int to_type);
void XmSetFontUnits(Display *display, int h_value, int v_value);
void XmSetFontUnit(Display *display, int value);

/************************* MenuUtil.c *****************************/

void XmSetMenuCursor(Display *display, Cursor cursorId);
Cursor XmGetMenuCursor(Display *display);

/************************** Simple.c ******************************/

Widget XmCreateSimpleCheckBox(Widget parent, char *name,
			      Arg *arglist, Cardinal argcount);
Widget XmCreateSimpleMenuBar(Widget parent, char *name,
			     Arg *arglist, Cardinal argcount);
Widget XmCreateSimpleOptionMenu(Widget parent, char *name,
				Arg *arglist, Cardinal argcount);
Widget XmCreateSimplePopupMenu(Widget parent, char *name,
			       Arg *arglist, Cardinal argcount);
Widget XmCreateSimplePulldownMenu(Widget parent, char *name,
				  Arg *arglist, Cardinal argcount);
Widget XmCreateSimpleRadioBox(Widget parent, char *name,
			      Arg *arglist, Cardinal argcount);

/************************* VaSimple.c *****************************/

Widget XmVaCreateSimpleCheckBox(Widget parent, String name,
				XtCallbackProc callback, ...);
Widget XmVaCreateSimpleMenuBar(Widget parent, String name,
			       ...);
Widget XmVaCreateSimpleOptionMenu(Widget parent, String name,
				  XmString option_label,
				  KeySym option_mnemonic,
				  int button_set,
				  XtCallbackProc callback, ...);
Widget XmVaCreateSimplePopupMenu(Widget parent, String name,
				 XtCallbackProc callback, ...);
Widget XmVaCreateSimplePulldownMenu(Widget parent, String name,
				    int post_from_button,
				    XtCallbackProc callback, ...);
Widget XmVaCreateSimpleRadioBox(Widget parent, String name,
				int button_set,
				XtCallbackProc callback, ...);

/************************** TrackLoc.c *****************************/

Widget XmTrackingEvent(Widget widget, Cursor cursor, Boolean confine_to,
		       XEvent *event_return);
Widget XmTrackingLocate(Widget widget, Cursor cursor, Boolean confine_to);

/**************************** Visual.c *****************************/

typedef void (*XmColorProc)(XColor *bg_color, XColor *fg_color,
			    XColor *sel_color, XColor *ts_color, XColor *bs_color);


extern XmColorProc XmSetColorCalculation(XmColorProc proc);
extern XmColorProc XmGetColorCalculation(void);
extern void XmGetColors(Screen *screen, Colormap color_map,
			Pixel background, Pixel *foreground_ret,
			Pixel *top_shadow_ret, Pixel *bottom_shadow_ret,
			Pixel *select_ret);
extern void XmChangeColor(Widget widget, Pixel background);

/*************************** XmString.c ****************************/

Dimension XmStringBaseline(XmFontList fontlist, XmString string);
Boolean XmStringByteCompare(XmString s1, XmString s2);
Boolean XmStringCompare(XmString s1, XmString s2);
XmString XmStringConcat(XmString s1, XmString s2);
XmString XmStringCreate(char *text, char *tag);
XmString XmStringCreateLtoR(char *text, char *tag);
XmString XmStringLtoRCreate(char *text, char *tag);
XmString XmStringCreateLocalized(char *text);
XmString XmStringCreateSimple(char *text);
XmString XmStringDirectionCreate(XmStringDirection direction);
void XmStringDraw(Display *d, 
		  Window w,
		  XmFontList fontlist,
		  XmString string,
		  GC gc,
		  Position x,
		  Position y,
		  Dimension width,
		  unsigned char alignment,
		  unsigned char layout_direction,
		  XRectangle *clip);
void XmStringDrawImage(Display *d, Window w,
		       XmFontList fontlist,
		       XmString string,
		       GC gc,
		       Position x,
		       Position y,
		       Dimension width,
		       unsigned char alignment,
		       unsigned char layout_direction,
		       XRectangle *clip);
void XmStringDrawUnderline(Display *d, Window w,
			   XmFontList fontlist, XmString string,
			   GC gc, Position x, Position y, Dimension width,
			   unsigned char alignment,
			   unsigned char layout_direction,
			   XRectangle *clip,
			   XmString underline);
Boolean XmStringEmpty(XmString s1);
void XmStringExtent(XmFontList fontlist, 
		    XmString string,
		    Dimension *width,
		    Dimension *height);
void XmStringFree(XmString string);
void XmStringFreeContext(XmStringContext context);
Boolean XmStringGetLtoR(XmString string,
			XmStringCharSet tag,
			char **text);
XmStringComponentType XmStringGetNextComponent(XmStringContext context,
					       char **text,
					       XmStringCharSet *tag,
					       XmStringDirection *direction,
					       XmStringComponentType *unknown_tag,
					       unsigned short *unknown_length,
					       unsigned char **unknown_value);
Boolean XmStringGetNextSegment(XmStringContext context,
			       char **text,
			       XmStringCharSet *tag,
			       XmStringDirection *direction,
			       Boolean *separator);
Boolean XmStringHasSubstring(XmString string,
			     XmString substring);
Dimension XmStringHeight(XmFontList fontlist, XmString string);
Boolean XmStringInitContext(XmStringContext *context,
			    XmString string);
int XmStringLength(XmString s1);
int XmStringLineCount(XmString string);
XmString XmStringNConcat(XmString s1, XmString s2, int num_bytes);
XmString XmStringCopy(XmString s);
XmString XmStringNCopy(XmString s1, int num_bytes);
XmStringComponentType XmStringPeekNextComponent(XmStringContext context);
XmString XmStringSegmentCreate(char *text, char *tag, 
			       XmStringDirection direction,
			       Boolean separator);
XmString XmStringSeparatorCreate(void);

Dimension XmStringWidth(XmFontList fontList, XmString string);


/*************************** FontList.c *****************************/

XmFontList XmFontListAppendEntry(XmFontList oldlist,
				 XmFontListEntry entry);
XmFontList XmFontListCreate(XFontStruct *font,
			    XmStringCharSet charset);
XmFontList XmFontListAdd(XmFontList old,
			 XFontStruct *font,
			 XmStringCharSet charset);
XmFontList XmFontListCopy(XmFontList fontlist);
XmFontListEntry XmFontListEntryCreate(char *tag,
				      XmFontType type,
				      XtPointer font);
void XmFontListEntryFree(XmFontListEntry *entry);
XtPointer XmFontListEntryGetFont(XmFontListEntry entry,
				 XmFontType *type_return);
char *XmFontListEntryGetTag(XmFontListEntry entry);
XmFontListEntry XmFontListEntryLoad(Display *display,
				    char *font_name,
				    XmFontType type,
				    char *tag);
void XmFontListFree(XmFontList list);
void XmFontListFreeFontContext(XmFontContext context);
Boolean XmFontListInitFontContext(XmFontContext *context,
				  XmFontList fontlist);
XmFontListEntry XmFontListNextEntry(XmFontContext context);
Boolean XmFontListGetNextFont(XmFontContext context, XmStringCharSet *charset, XFontStruct **font);
XmFontList XmFontListRemoveEntry(XmFontList oldlist,
				 XmFontListEntry entry);


/************************** Dest.c ***********************************/

Widget XmGetDestination(Display *display);

/*************************** Traversal.c *****************************/

Boolean XmIsTraversable(Widget widget);
XmVisibility XmGetVisibility(Widget widget);
void XmAddTabGroup(Widget widget);
void XmRemoveTabGroup(Widget widget);
Widget XmGetTabGroup(Widget widget);
Boolean XmProcessTraversal(Widget widget, XmTraversalDirection direction);
Widget XmGetFocusWidget(Widget widget);

/**************************** XmIm.c *********************************/

void XmImRegister(Widget w, unsigned int reserved);
void XmImUnregister(Widget w);
void XmImSetFocusValues(Widget w, ArgList args, Cardinal num_args);
void XmImSetValues(Widget w, ArgList args, Cardinal num_args);
void XmImUnsetFocus(Widget w);
XIM XmImGetXIM(Widget w);
int XmImMbLookupString(Widget w, XKeyPressedEvent *event, char *buf,                        int nbytes,
		       KeySym *keysym, int *status);
extern void XmImVaSetFocusValues(Widget w, ...);
extern void XmImVaSetValues(Widget w, ...);

#define	XmTextSetTopPosition	XmTextSetTopCharacter
#ifdef __cplusplus
}
#endif

#endif /* XM_H */

