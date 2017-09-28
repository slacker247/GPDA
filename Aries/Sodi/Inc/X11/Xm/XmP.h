/**
 *
 * $Id: XmP.h,v 1.7 1998/09/22 22:53:55 gritton Exp $
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

#ifndef XM_P_H
#define XM_P_H

#include <Xm/Xm.h>
#include <Xm/DrawP.h>
#include <X11/IntrinsicP.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * shorthand macros
 */
#ifdef XtDisplay
#undef XtDisplay
#endif
#define XtDisplay(widget) \
    (XtIsWidget(widget) \
	? ((Widget)(widget))->core.screen->display \
	: ((Object)(widget))->object.parent->core.screen->display)

#ifdef XtScreen
#undef XtScreen
#endif
#define XtScreen(widget) \
    (XtIsWidget(widget) \
	? ((Widget)(widget))->core.screen \
	: ((Object)(widget))->object.parent->core.screen)

#ifdef XtWindow
#undef XtWindow
#endif
#define XtWindow(widget) \
    (XtIsWidget(widget) \
	? ((Widget)(widget))->core.window \
	: ((Object)(widget))->object.parent->core.window)

#ifdef XtName
#undef XtName
#endif
#define XtName(widget) \
    XrmQuarkToString(((Object)(widget))->object.xrm_name)

#ifdef XtClass
#undef XtClass
#endif
#define XtClass(widget) \
    (((Object)(widget))->object.widget_class)

#ifdef XtSuperclass
#undef XtSuperclass
#endif
#define XtSuperclass(widget) \
    (XtClass(widget)->core_class.superclass)

#ifdef XtIsRealized
#undef XtIsRealized
#endif
#define XtIsRealized(widget) \
    (XtIsWidget(widget) \
	? ((Widget)(widget))->core.window \
	: ((Object)(widget))->object.parent->core.window)

#ifdef XtIsManaged
#undef XtIsManaged
#endif
#define XtIsManaged(widget) \
    (((XmGadget)(widget))->rectangle.managed)

#ifdef XtParent
#undef XtParent
#endif
#define XtParent(widget) \
    (((Object)(widget))->object.parent)

/*
 * #defines for useful core record variables
 */
#ifdef	__BOUNDS_CHECKING_ON
/* This is a version that works with Bounds Checking GCC */
#define	XtWidth(w)	 (((XmGadget)(w))->rectangle.width)
#define	XtHeight(w)	 (((XmGadget)(w))->rectangle.height)
#define XtX(w)		 (((XmGadget)(w))->rectangle.x)
#define XtY(w)		 (((XmGadget)(w))->rectangle.y)
#define XtBackground(w)	 (((Widget)(w))->core.background_pixel)
#define XtBorderWidth(w) (((XmGadget)(w))->rectangle.border_width)
#define XtSensitive(w)	 (((XmGadget)(w))->rectangle.sensitive && \
			  ((XmGadget)(w))->rectangle.ancestor_sensitive)
#define XtCoreProc(w,p)  (((XmGadget)(w))->core.widget_class->core_class.p)
#else
#define XtWidth(w)	 (((Widget)(w))->core.width)
#define XtHeight(w)	 (((Widget)(w))->core.height)
#define XtX(w)		 (((Widget)(w))->core.x)
#define XtY(w)		 (((Widget)(w))->core.y)
#define XtBackground(w)	 (((Widget)(w))->core.background_pixel)
#define XtBorderWidth(w) (((Widget)(w))->core.border_width)
#define XtSensitive(w)	 (((Widget)(w))->core.sensitive && \
			  ((Widget)(w))->core.ancestor_sensitive)
#define XtCoreProc(w,p)  (((Widget)(w))->core.widget_class->core_class.p)
#endif

/*
 * idiocy for K&R (which, BTW, won't work anyway).
 */
#ifndef XmConst
#if (defined(__STDC__) && __STDC__) || !defined(NO_CONST)
#define XmConst	const
#else
#define XmConst
#endif /* __STDC__ */
#endif /* !XmConst */

/*
 * menu values
 */
enum {
    XmMENU_POPDOWN,
    XmMENU_PROCESS_TREE, 
    XmMENU_TRAVERSAL, 
    XmMENU_SHELL_POPDOWN,
    XmMENU_CALLBACK,
    XmMENU_BUTTON,
    XmMENU_CASCADING,
    XmMENU_SUBMENU,
    XmMENU_ARM,
    XmMENU_DISARM,
    XmMENU_BAR_CLEANUP,
    XmMENU_STATUS,
    XmMENU_MEMWIDGET_UPDATE,
    XmMENU_BUTTON_POPDOWN,
    XmMENU_RESTORE_EXCLUDED_TEAROFF_TO_TOPLEVEL_SHELL,
    XmMENU_RESTORE_TEAROFF_TO_TOPLEVEL_SHELL,
    XmMENU_RESTORE_TEAROFF_TO_MENUSHELL,
    XmMENU_GET_LAST_SELECT_TOPLEVEL,
    XmMENU_TEAR_OFF_ARM
};

#define XmMENU_TORN_BIT                         (1 << 0)
#define XmMENU_TEAR_OFF_SHELL_DESCENDANT_BIT    (1 << 1)
#define XmMENU_POPUP_POSTED_BIT                 (1 << 2)

#define XmIsTorn(m) \
    ((m) & XmMENU_TORN_BIT)
#define XmPopupPosted(m) \
    ((m) & XmMENU_POPUP_POSTED_BIT)
#define XmIsTearOffShellDescendant(m) \
    ((m) & XmMENU_TEAR_OFF_SHELL_DESCENDANT_BIT)

/*
 * constants used in button/SimpleMenu communication
 */
typedef struct _XmSimpleMenuRec {
    int count;
    int post_from_button;
    XtCallbackProc callback;
    XmStringTable label_string;
    String *accelerator;
    XmStringTable accelerator_text;
    XmKeySymTable mnemonic;
    XmStringCharSetTable mnemonic_charset;
    XmButtonTypeTable button_type;
    int button_set;
    XmString option_label;
    KeySym option_mnemonic;
} XmSimpleMenuRec, *XmSimpleMenu;

/* For MapEvent: _XmMatchBtnEvent */
#define XmIGNORE_EVENTTYPE      -1

/* Default minimum Toggle indicator dimension */
#define XmDEFAULT_INDICATOR_DIM   9

/* DefaultButtonShadow stuff */
#define Xm3D_ENHANCE_PIXEL              2
#define XmDEFAULT_TOP_MARGIN            0
#define XmDEFAULT_BOTTOM_MARGIN         0


/*
 * synthetic resource stuff
 */

typedef enum { 
    XmSYNTHETIC_NONE, 
    XmSYNTHETIC_LOAD 
} XmImportOperator;

typedef void (*XmExportProc)(Widget, int, XtArgVal *);
typedef XmImportOperator (*XmImportProc)(Widget, int, XtArgVal*);

typedef struct _XmSyntheticResource {
    String resource_name;
    Cardinal resource_size;
    Cardinal resource_offset;
    XmExportProc export_proc;
    XmImportProc import_proc;
} XmSyntheticResource;

/*
 * ParentProcess structures
 */

enum {
    XmPARENT_PROCESS_ANY, 
    XmINPUT_ACTION
};

enum {
    XmPARENT_ACTIVATE,
    XmPARENT_CANCEL
};

#define XmRETURN XmPARENT_ACTIVATE
#define XmCANCEL XmPARENT_CANCEL

typedef struct {
    int process_type;
} XmParentProcessAnyRec;

typedef struct {
    int process_type;
    XEvent *event;
    int action;
    String *params;
    Cardinal *num_params;
} XmParentInputActionRec;

typedef union {
    XmParentProcessAnyRec any;
    XmParentInputActionRec input_action;
} XmParentProcessDataRec, *XmParentProcessData;

#define XmINVALID_DIMENSION (0xFFFF)

enum {
    XmBASELINE_GET,
    XmBASELINE_SET
};

typedef struct _XmBaselineMargins {
    unsigned char get_or_set;
    Dimension margin_top;
    Dimension margin_bottom;
    Dimension shadow;
    Dimension highlight;
    Dimension text_height;
    Dimension text_width;
    Dimension margin_height;
} XmBaselineMargins;

typedef enum {
    XmFOCUS_IN,
    XmFOCUS_OUT,
    XmENTER,
    XmLEAVE
} XmFocusChange;

typedef enum{
    XmNOT_NAVIGABLE,
    XmCONTROL_NAVIGABLE,
    XmTAB_NAVIGABLE,
    XmDESCENDANTS_NAVIGABLE,
    XmDESCENDANTS_TAB_NAVIGABLE
} XmNavigability;

#define XmVoidProc      XtProc

typedef Boolean (*XmParentProcessProc)(Widget, XmParentProcessData);
typedef void    (*XmWidgetDispatchProc)
			(Widget gadget, XEvent *event, Mask event_mask);
typedef void    (*XmMenuPopupProc)(Widget, Widget, XEvent *);
typedef void    (*XmMenuTraversalProc)( Widget, Widget, XmTraversalDirection);
typedef void    (*XmResizeFlagProc)(Widget, Boolean);
typedef void    (*XmRealizeOutProc)(Widget, Mask *, XSetWindowAttributes *);
typedef Boolean (*XmVisualChangeProc)
			(Widget gadget, Widget cur_mgr, Widget new_mgr);
typedef void    (*XmTraversalProc)(Widget, XtPointer, XtPointer, int);
typedef void    (*XmFocusMovedProc)( Widget, XtPointer, XtPointer) ;
typedef void    (*XmCacheCopyProc)(XtPointer, XtPointer, size_t);
typedef int     (*XmCacheCompareProc)(XtPointer, XtPointer);
typedef Boolean (*XmWidgetBaselineProc)
			(Widget w, Dimension **baselines, int *num_baselines);
typedef Boolean (*XmWidgetDisplayRectProc)(Widget w, XRectangle *rect);
typedef void    (*XmWidgetMarginsProc)(Widget w, XmBaselineMargins *margins);
typedef XmNavigability    (*XmWidgetNavigableProc)(Widget w);
typedef void    (*XmFocusChangeProc)(Widget w, XmFocusChange change);

typedef void    (*XmMenuProc)(int function, Widget widget, ...);

typedef void    (*XmGadgetCacheProc)(XtPointer);
typedef Boolean (*XmTraversalChildrenProc)
			(Widget mw, Widget **children, Cardinal *num_children);

/*
 * virtkey stuff
 */

typedef struct {
    Modifiers mod;
    char      *key;
    char      *action;
} _XmBuildVirtualKeyStruct;


/*
 * stuff needed by the Text and TextField widgets to do their rendering
 */

typedef struct {
    XmTextPosition position;
    XmHighlightMode mode;
} _XmHighlightRec;

typedef struct {
    Cardinal number;
    Cardinal maximum;
    _XmHighlightRec *list;
} _XmHighlightData;

typedef struct {
    Atom selection;
    Atom target;
} _XmTextInsertPair;

typedef enum {
    XmDEST_SELECT,
    XmPRIM_SELECT
} XmSelectType;

typedef struct {
    Boolean done_status;
    Boolean success_status;
    XmSelectType select_type;
    XSelectionRequestEvent *event;
} _XmInsertSelect;

typedef struct {
    XEvent *event;
    String *params;
    Cardinal *num_params;
} _XmTextActionRec;

typedef struct {
    Widget widget;
    XmTextPosition insert_pos;
    int num_chars;
    Time timestamp;
    Boolean move;
} _XmTextDropTransferRec;

typedef struct {
    XmTextPosition position;
    Atom target;
    Time time;
    int num_chars;
    int ref_count;
} _XmTextPrimSelect;

typedef struct {
    Screen *screen;
    XContext context;
    unsigned char type;
} XmTextContextDataRec, *XmTextContextData;

enum {
    _XM_IS_DEST_CTX,
    _XM_IS_GC_DATA_CTX,
    _XM_IS_PIXMAP_CTX
};

#define XmTEXT_DRAG_ICON_WIDTH  64
#define XmTEXT_DRAG_ICON_HEIGHT 64
#define XmTEXT_DRAG_ICON_X_HOT  10
#define XmTEXT_DRAG_ICON_Y_HOT   4

/*
 * geometry stuff, used in GeoUtils.c
 */

enum{
    XmGET_ACTUAL_SIZE = 1,
    XmGET_PREFERRED_SIZE,
    XmGEO_PRE_SET,
    XmGEO_POST_SET
};

/* fill modes for the GeoLayoutRec's below */
enum {
    XmGEO_EXPAND,
    XmGEO_CENTER,
    XmGEO_PACK
};

/* fit modes for the GeoLayoutRec's below */
enum {
    XmGEO_PROPORTIONAL,
    XmGEO_AVERAGING,
    XmGEO_WRAP
};

enum {
    XmGEO_ROW_MAJOR,
    XmGEO_COLUMN_MAJOR
};

typedef struct _XmGeoMatrixRec *XmGeoMatrix;
typedef union _XmGeoMajorLayoutRec *XmGeoMajorLayout;
typedef struct _XmKidGeometryRec {
    Widget kid;
    XtWidgetGeometry box;
} XmKidGeometryRec, *XmKidGeometry;

typedef void (*XmGeoArrangeProc)(XmGeoMatrix, Position, Position,
				 Dimension *, Dimension *);
typedef Boolean (*XmGeoExceptProc)(XmGeoMatrix);
typedef void (*XmGeoExtDestructorProc)(XtPointer);
typedef void (*XmGeoSegmentFixUpProc)(XmGeoMatrix, int,
				      XmGeoMajorLayout, XmKidGeometry);

typedef struct {
    Boolean end;
    XmGeoSegmentFixUpProc fix_up;
    Dimension even_width;
    Dimension even_height;
    Dimension min_height;
    Boolean stretch_height;
    Boolean uniform_border;
    Dimension border;
    unsigned char fill_mode;
    unsigned char fit_mode;
    Boolean sticky_end;
    Dimension space_above;
    Dimension space_end;
    Dimension space_between;
    Dimension max_box_height;
    Dimension boxes_width;
    Dimension fill_width;
    Dimension box_count;
} XmGeoRowLayoutRec, *XmGeoRowLayout;

typedef struct {
    Boolean end;
    XmGeoSegmentFixUpProc fix_up;
    Dimension even_height;
    Dimension even_width;
    Dimension min_width;
    Boolean stretch_width;
    Boolean uniform_border;
    Dimension border;
    unsigned char fill_mode;
    unsigned char fit_mode;
    Boolean sticky_end;
    Dimension space_left;
    Dimension space_end;
    Dimension space_between;
    Dimension max_box_width;
    Dimension boxed_height;
    Dimension fill_height;
    Dimension box_count;
} XmGeoColumnLayoutRec, *XmGeoColumnLayout;

typedef union _XmGeoMajorLayoutRec {
    XmGeoRowLayoutRec row;
    XmGeoColumnLayoutRec col;
} XmGeoMajorLayoutRec;

typedef struct _XmGeoMatrixRec {
    Widget composite;
    Widget instigator;
    XtWidgetGeometry instig_request;
    XtWidgetGeometry parent_request;
    XtWidgetGeometry *in_layout;
    XmKidGeometry boxes;  /* there is a NULL pointer add the end of each row */
    XmGeoMajorLayout layouts;
    Dimension margin_w;
    Dimension margin_h;
    Boolean stretch_boxes;
    Boolean uniform_border;
    Dimension border;
    Dimension max_major;
    Dimension boxes_minor;
    Dimension fill_minor;
    Dimension width;
    Dimension height;
    XmGeoExceptProc set_except;
    XmGeoExceptProc almost_except;
    XmGeoExceptProc no_geo_request;
    XtPointer extension;
    XmGeoExtDestructorProc ext_destructor;
    XmGeoArrangeProc arrange_boxes;
    unsigned char major_order;
} XmGeoMatrixRec;

typedef XmGeoMatrix (*XmGeoCreateProc)(Widget, Widget, XtWidgetGeometry *);

/*
 * inheritance stuff
 */

#define XmInheritCallbackProc      ((XtCallbackProc) _XtInherit)
#define XmInheritTraversalProc     ((XmTraversalProc) _XtInherit)
#define XmInheritParentProcess     ((XmParentProcessProc) _XtInherit)
#define XmInheritWidgetProc        ((XtWidgetProc) _XtInherit)
#define XmInheritMenuProc          ((XmMenuProc) _XtInherit)
#define XmInheritTranslations      XtInheritTranslations
#define XmInheritCachePart         ((XtCacheClassPartPtr) _XtInherit)
#define XmInheritBaselineProc      ((XmWidgetBaselineProc) _XtInherit)
#define XmInheritDisplayRectProc   ((XmWidgetDisplayRectProc) _XtInherit)
#define XmInheritMarginsProc ((XmWidgetMarginsProc) _XtInherit)
#define XmInheritGeoMatrixCreate   ((XmGeoCreateProc) _XtInherit)
#define XmInheritFocusMovedProc    ((XmFocusMovedProc) _XtInherit)
#define XmInheritClass             ((WidgetClass) &_XmInheritClass)
#define XmInheritInitializePrehook ((XtInitProc) _XtInherit)
#define XmInheritSetValuesPrehook  ((XtSetValuesFunc) _XtInherit)
#define XmInheritInitializePosthook ((XtInitProc) _XtInherit)
#define XmInheritSetValuesPosthook ((XtSetValuesFunc) _XtInherit)
#define XmInheritGetValuesPosthook ((XtArgsProc) _XtInherit)
#define XmInheritSecObjectCreate   ((XtInitProc) _XtInherit)
#define XmInheritGetSecResData     ((XmGetSecResDataFunc) _XtInherit)
#define XmInheritInputDispatch     ((XmWidgetDispatchProc) _XtInherit)
#define XmInheritVisualChange      ((XmVisualChangeProc) _XtInherit)
#define XmInheritGetValuesPrehook  ((XtArgsProc) _XtInherit)
#define XmInheritArmAndActivate	   ((XtActionProc) _XtInherit)
#define XmInheritActionProc        ((XtActionProc) _XtInherit)
#define XmInheritFocusChange       ((XmFocusChangeProc) _XtInherit)
#define XmInheritWidgetNavigable   ((XmWidgetNavigableProc) _XtInherit)
#define XmInheritClassPartInitPrehook ((XtWidgetClassProc) _XtInherit)
#define XmInheritClassPartInitPosthook ((XtWidgetClassProc) _XtInherit)
#define XmInheritBorderHighlight   ((XtWidgetProc) _XtInherit)
#define XmInheritBorderUnhighlight ((XtWidgetProc) _XtInherit)

#define XmInheritRealize           ((XtRealizeProc) _XtInherit)
#define XmInheritResize            ((XtWidgetProc) _XtInherit)
#define XmInheritSetOverrideCallback ((XtWidgetProc) _XtInherit)
#define XmInheritTraversalChildrenProc ((XmTraversalChildrenProc) _XtInherit)



/*
 * fast subclassing definitions
 */

enum {
    XmCASCADE_BUTTON_BIT = 1,
    XmCASCADE_BUTTON_GADGET_BIT,
    XmCOMMAND_BOX_BIT,
    XmDIALOG_SHELL_BIT,
    XmLIST_BIT,
    XmFORM_BIT,
    XmTEXT_FIELD_BIT,
    XmGADGET_BIT,
    XmLABEL_BIT,
    XmLABEL_GADGET_BIT,
    XmMAIN_WINDOW_BIT,
    XmMANAGER_BIT,
    XmMENU_SHELL_BIT,
    XmDRAWN_BUTTON_BIT,
    XmPRIMITIVE_BIT,
    XmPUSH_BUTTON_BIT,
    XmPUSH_BUTTON_GADGET_BIT,
    XmROW_COLUMN_BIT,
    XmSCROLL_BAR_BIT,
    XmSCROLLED_WINDOW_BIT,
    XmSELECTION_BOX_BIT,
    XmSEPARATOR_BIT,
    XmSEPARATOR_GADGET_BIT,
    XmTEXT_BIT,
    XmTOGGLE_BUTTON_BIT,
    XmTOGGLE_BUTTON_GADGET_BIT,
    XmDROP_TRANSFER_BIT,
    XmDROP_SITE_MANAGER_BIT,
    XmDISPLAY_BIT,
    XmSCREEN_BIT,
    XmARROW_BUTTON_BIT = 32,
    XmARROW_BUTTON_GADGET_BIT,
    XmBULLETIN_BOARD_BIT,
    XmDRAWING_AREA_BIT,
    XmFILE_SELECTION_BOX_BIT,
    XmFRAME_BIT,
    XmMESSAGE_BOX_BIT,
    XmSASH_BIT,
    XmSCALE_BIT,
    XmPANED_WINDOW_BIT,
    XmVENDOR_SHELL_BIT,
    XmCLIP_WINDOW_BIT,
    XmDRAG_ICON_BIT,
    XmTEAROFF_BUTTON_BIT,
    XmDRAG_OVER_SHELL_BIT,
    XmDRAG_CONTEXT_BIT,

	/*
	 * There are more widgets here in the 2.0 directory's version of XmP.h.
	 * Be sure to check there for conflicts when adding widget bits here...
	 */

    XmFAST_SUBCLASS_TAIL_BIT,

    XmFIRST_APPLICATION_SUBCLASS_BIT = 192
};

#define XmLAST_FAST_SUBCLASS_BIT (XmFAST_SUBCLASS_TAIL_BIT - 1)

#undef XmIsCascadeButton
#define XmIsCascadeButton(w) \
    (_XmIsFastSubclass(XtClass(w), XmCASCADE_BUTTON_BIT))

#undef XmIsCascadeButtonGadget
#define XmIsCascadeButtonGadget(w) \
    (_XmIsFastSubclass(XtClass(w), XmCASCADE_BUTTON_GADGET_BIT))

#undef XmIsCommandBox
#define XmIsCommandBox(w) \
    (_XmIsFastSubclass(XtClass(w), XmCOMMAND_BOX_BIT))

#undef XmIsDialogShell
#define XmIsDialogShell(w) \
    (_XmIsFastSubclass(XtClass(w), XmDIALOG_SHELL_BIT))

#undef XmIsDisplay
#define XmIsDisplay(w) \
    (_XmIsFastSubclass(XtClass(w), XmDISPLAY_BIT))

#undef XmIsList
#define XmIsList(w) \
    (_XmIsFastSubclass(XtClass(w), XmLIST_BIT))

#undef XmIsForm
#define XmIsForm(w) \
    (_XmIsFastSubclass(XtClass(w), XmFORM_BIT))

#undef XmIsTextField
#define XmIsTextField(w) \
    (_XmIsFastSubclass(XtClass(w), XmTEXT_FIELD_BIT))

#undef XmIsGadget
#define XmIsGadget(w) \
    (_XmIsFastSubclass(XtClass(w), XmGADGET_BIT))

#undef XmIsLabel
#define XmIsLabel(w) \
    (_XmIsFastSubclass(XtClass(w), XmLABEL_BIT))

#undef XmIsLabelGadget
#define XmIsLabelGadget(w) \
    (_XmIsFastSubclass(XtClass(w), XmLABEL_GADGET_BIT))

#undef XmIsMainWindow
#define XmIsMainWindow(w) \
    (_XmIsFastSubclass(XtClass(w), XmMAIN_WINDOW_BIT))

#undef XmIsManager
#define XmIsManager(w) \
    (_XmIsFastSubclass(XtClass(w), XmMANAGER_BIT))

#undef XmIsMenuShell
#define XmIsMenuShell(w) \
    (_XmIsFastSubclass(XtClass(w), XmMENU_SHELL_BIT))

#undef XmIsDragIcon
#define XmIsDragIcon(w) \
    (_XmIsFastSubclass(XtClass(w), XmDRAG_ICON_BIT))

#undef XmIsDropSiteManager
#define XmIsDropSiteManager(w) \
    (_XmIsFastSubclass(XtClass(w), XmDROP_SITE_MANAGER_BIT))

#undef XmIsDropTransfer
#define XmIsDropTransfer(w) \
    (_XmIsFastSubclass(XtClass(w), XmDROP_TRANSFER_BIT))

#undef XmIsDragOverShell
#define XmIsDragOverShell(w) \
    (_XmIsFastSubclass(XtClass(w), XmDRAG_OVER_SHELL_BIT))

#undef XmIsDragContext
#define XmIsDragContext(w) \
    (_XmIsFastSubclass(XtClass(w), XmDRAG_CONTEXT_BIT))

#undef XmIsDrawnButton
#define XmIsDrawnButton(w) \
    (_XmIsFastSubclass(XtClass(w), XmDRAWN_BUTTON_BIT))

#undef XmIsPrimitive
#define XmIsPrimitive(w) \
    (_XmIsFastSubclass(XtClass(w), XmPRIMITIVE_BIT))

#undef XmIsPushButton
#define XmIsPushButton(w) \
    (_XmIsFastSubclass(XtClass(w), XmPUSH_BUTTON_BIT))

#undef XmIsPushButtonGadget
#define XmIsPushButtonGadget(w) \
    (_XmIsFastSubclass(XtClass(w), XmPUSH_BUTTON_GADGET_BIT))

#undef XmIsRowColumn
#define XmIsRowColumn(w) \
    (_XmIsFastSubclass(XtClass(w), XmROW_COLUMN_BIT))

#undef XmIsScreen
#define XmIsScreen(w) \
    (_XmIsFastSubclass(XtClass(w), XmSCREEN_BIT))

#undef XmIsScrollBar
#define XmIsScrollBar(w) \
    (_XmIsFastSubclass(XtClass(w), XmSCROLL_BAR_BIT))

#undef XmIsScrolledWindow
#define XmIsScrolledWindow(w) \
    (_XmIsFastSubclass(XtClass(w), XmSCROLLED_WINDOW_BIT))

#undef XmIsSelectionBox
#define XmIsSelectionBox(w) \
    (_XmIsFastSubclass(XtClass(w), XmSELECTION_BOX_BIT))

#undef XmIsSeparator
#define XmIsSeparator(w) \
    (_XmIsFastSubclass(XtClass(w), XmSEPARATOR_BIT))

#undef XmIsSeparatorGadget
#define XmIsSeparatorGadget(w) \
    (_XmIsFastSubclass(XtClass(w), XmSEPARATOR_GADGET_BIT))

#undef XmIsText
#define XmIsText(w) \
    (_XmIsFastSubclass(XtClass(w), XmTEXT_BIT))

#undef XmIsTearOffButton
#define XmIsTearOffButton(w) \
    (_XmIsFastSubclass(XtClass(w), XmTEAROFF_BUTTON_BIT))

#undef XmIsToggleButton
#define XmIsToggleButton(w) \
    (_XmIsFastSubclass(XtClass(w), XmTOGGLE_BUTTON_BIT))

#undef XmIsToggleButtonGadget
#define XmIsToggleButtonGadget(w) \
    (_XmIsFastSubclass(XtClass(w), XmTOGGLE_BUTTON_GADGET_BIT))

#undef XmIsArrowButton
#define XmIsArrowButton(w) \
    (_XmIsFastSubclass(XtClass(w), XmARROW_BUTTON_BIT))

#undef XmIsArrowButtonGadget
#define XmIsArrowButtonGadget(w) \
    (_XmIsFastSubclass(XtClass(w), XmARROW_BUTTON_GADGET_BIT))

#undef XmIsBulletinBoard
#define XmIsBulletinBoard(w) \
    (_XmIsFastSubclass(XtClass(w), XmBULLETIN_BOARD_BIT))

#undef XmIsDrawingArea
#define XmIsDrawingArea(w) \
    (_XmIsFastSubclass(XtClass(w), XmDRAWING_AREA_BIT))

#undef XmIsFileSelectionBox
#define XmIsFileSelectionBox(w) \
    (_XmIsFastSubclass(XtClass(w), XmFILE_SELECTION_BOX_BIT))

#undef XmIsFrame
#define XmIsFrame(w) \
    (_XmIsFastSubclass(XtClass(w), XmFRAME_BIT))

#undef XmIsMessageBox
#define XmIsMessageBox(w) \
    (_XmIsFastSubclass(XtClass(w), XmMESSAGE_BOX_BIT))

#undef XmIsSash
#define XmIsSash(w) \
    (_XmIsFastSubclass(XtClass(w), XmSASH_BIT))

#undef XmIsScale
#define XmIsScale(w) \
    (_XmIsFastSubclass(XtClass(w), XmSCALE_BIT))

#undef XmIsPanedWindow
#define XmIsPanedWindow(w) \
    (_XmIsFastSubclass(XtClass(w), XmPANED_WINDOW_BIT))

/*
 * XmPartOffset bogosity
 */

#define XmObjectIndex           0
#define ObjectIndex             XmObjectIndex
#define XmRectObjIndex          (XmObjectIndex + 1)
#define RectObjIndex            XmRectObjIndex
#define XmWindowObjIndex        (XmRectObjIndex + 1)
#define WindowObjIndex          XmWindowObjIndex
#define XmCoreIndex             0
#define CoreIndex               XmCoreIndex
#define XmCompositeIndex        (XmWindowObjIndex + 2)
#define CompositeIndex          XmCompositeIndex
#define XmConstraintIndex       (XmCompositeIndex + 1)
#define ConstraintIndex         XmConstraintIndex
#define XmGadgetIndex           (XmRectObjIndex + 1)
#define XmPrimitiveIndex        (XmWindowObjIndex + 2)
#define XmManagerIndex          (XmConstraintIndex + 1)

#define XmArrowBIndex           (XmPrimitiveIndex + 1)
#define XmArrowButtonIndex      XmArrowBIndex
#define XmLabelIndex            (XmPrimitiveIndex + 1)
#define XmListIndex             (XmPrimitiveIndex + 1)
#define XmScrollBarIndex        (XmPrimitiveIndex + 1)
#define XmSeparatorIndex        (XmPrimitiveIndex + 1)
#define XmTextIndex             (XmPrimitiveIndex + 1)

#define XmCascadeBIndex         (XmLabelIndex + 1)
#define XmCascadeButtonIndex    XmCascadeBIndex
#define XmDrawnBIndex           (XmLabelIndex + 1)
#define XmDrawnButtonIndex      XmDrawnBIndex
#define XmPushBIndex            (XmLabelIndex + 1)
#define XmPushButtonIndex       XmPushBIndex
#define XmToggleBIndex          (XmLabelIndex + 1)
#define XmToggleButtonIndex     XmToggleBIndex
#define XmTearOffButtonIndex    (XmPushBIndex + 1)

#define XmArrowBGIndex          (XmGadgetIndex + 1)
#define XmArrowButtonGadgetIndex XmArrowBGIndex
#define XmLabelGIndex           (XmGadgetIndex + 1)
#define XmLabelGadgetIndex      XmLabelGIndex
#define XmSeparatoGIndex        (XmGadgetIndex + 1)
#define XmSeparatorGadgetIndex  XmSeparatoGIndex

#define XmCascadeBGIndex        (XmLabelGIndex + 1)
#define XmCascadeButtonGadgetIndex XmCascadeBGIndex
#define XmPushBGIndex           (XmLabelGIndex + 1)
#define XmPushButtonGadgetIndex XmPushBGIndex
#define XmToggleBGIndex         (XmLabelGIndex + 1)
#define XmToggleButtonGadgetIndex XmToggleBGIndex

#define XmBulletinBIndex        (XmManagerIndex + 1)
#define XmBulletinBoardIndex    XmBulletinBIndex
#define XmDrawingAIndex         (XmManagerIndex + 1)
#define XmDrawingAreaIndex      XmDrawingAIndex
#define XmFrameIndex            (XmManagerIndex + 1)
#define XmPanedWIndex           (XmManagerIndex + 1)
#define XmPanedWindowIndex      XmPanedWIndex
#define XmSashIndex             (XmPrimitiveIndex + 1)
#define XmRowColumnIndex        (XmManagerIndex + 1)
#define XmScaleIndex            (XmManagerIndex + 1)
#define XmScrolledWIndex        (XmManagerIndex + 1)
#define XmScrolledWindowIndex   XmScrolledWIndex

#define XmFormIndex             (XmBulletinBIndex + 1)
#define XmMessageBIndex         (XmBulletinBIndex + 1)
#define XmMessageBoxIndex       XmMessageBIndex
#define XmSelectioBIndex        (XmBulletinBIndex + 1)
#define XmSelectionBoxIndex     XmSelectioBIndex

#define XmMainWIndex            (XmScrolledWIndex + 1)
#define XmMainWindowIndex       XmMainWIndex

#define XmCommandIndex          (XmSelectioBIndex + 1)
#define XmFileSBIndex           (XmSelectioBIndex + 1)
#define XmFileSelectionBoxIndex XmFileSBIndex

#define XmShellIndex            (XmCompositeIndex + 1)
#define ShellIndex              XmShellIndex
#define XmOverrideShellIndex    (XmShellIndex + 1)
#define OverrideShellIndex      XmOverrideShellIndex
#define XmWMShellIndex          (XmShellIndex + 1)
#define WMShellIndex            XmWMShellIndex
#define XmVendorShellIndex      (XmWMShellIndex + 1)
#define VendorShellIndex        XmVendorShellIndex
#define XmTransientShellIndex   (XmVendorShellIndex + 1)
#define TransientShellIndex     XmTransientShellIndex
#define XmTopLevelShellIndex    (XmVendorShellIndex + 1)
#define TopLevelShellIndex      XmTopLevelShellIndex
#define XmApplicationShellIndex (XmTopLevelShellIndex + 1)
#define ApplicationShellIndex   XmApplicationShellIndex
#define XmDisplayIndex          (XmApplicationShellIndex + 1)

#define XmDialogSIndex          (XmTransientShellIndex + 1)
#define XmDialogShellIndex      XmDialogSIndex
#define XmMenuShellIndex        (XmOverrideShellIndex + 1)

#define XmDragIconIndex         (XmRectObjIndex + 1)
#define XmDropSiteManagerIndex  (XmObjectIndex + 1)
#define XmDropTransferIndex     (XmObjectIndex + 1)
#define XmDragOverShellIndex    (XmVendorShellIndex + 1)
#define XmDragContextIndex      (XmCoreIndex + 1)

#define XmOFFSETBITS (sizeof(Cardinal)*8/2)
#define XmOFFSETMASK ((1<<XmOFFSETBITS)-1)

typedef struct _XmPartResource {
    String resource_name;
    String resource_class;
    String resource_type;
    Cardinal resource_size;
    Cardinal resource_offset;
    String default_type;
    XtPointer default_addr;
} XmPartResource;

#define XmPartOffset(part, variable) \
    ((part##Index) << XmOFFSETBITS) + XtOffsetOf( part##Part, variable)

#define XmConstraintPartOffset(part, var) \
    ((part##Index) << XmOFFSETBITS) + XtOffsetOf(part##ConstraintPart, var)

#define XmGetPartOffset(r, off) \
    ((r)->resource_offset & 0xffff) + \
	(*(off))[(r)->resource_offset >> XmOFFSETBITS];

#define XmField(widget, offsetrecord, part, variable, type) \
    (*(type *)(((char *) (widget)) + offsetrecord[part##Index] + \
                XtOffsetOf( part##Part, variable)))

#define XmConstraintField(widget, offsetrecord, part, variable, type) \
    (*(type *)(((char *) (widget)->core.constraints) + \
		offsetrecord[part##Index] + \
		XtOffsetOf( part##ConstraintPart, variable)))

/*
 * these structures must match those of XRectangle, XRegion
 */

typedef struct {
    short x1, x2, y1, y2;
} XmRegionBox;

typedef struct _XmRegion {
    long	size;
    long	numRects;
    XmRegionBox	*rects;
    XmRegionBox	extents;
} XmRegionRec, *XmRegion;

/*********************** GadgetUtils.c *****************************/

XmGadget _XmInputInGadget(Widget cw, int x, int y);
XmGadget _XmInputForGadget(Widget cw, int x, int y);
void _XmConfigureObject(Widget g, Position x, Position y,
			Dimension width, Dimension height,
			Dimension border_width);
void XmeConfigureObject(Widget g, Position x, Position y,
			Dimension width, Dimension height,
			Dimension border_width);
void _XmResizeObject(Widget g, Dimension width, Dimension height,
		     Dimension border_width);
void _XmMoveObject(Widget g, Position x, Position y);
void _XmRedisplayGadgets(Widget w, XEvent *event, Region region);
void XmeRedisplayGadgets(Widget w, XEvent *event, Region region);
void _XmDispatchGadgetInput(Widget g, XEvent *event, Mask mask);
Time __XmGetDefaultTime(Widget w, XEvent *event);

/************************* ImageCache.c *******************************/

#define _XmCreateImage(IMAGE, DISPLAY, DATA, WIDTH, HEIGHT, BYTE_ORDER) {\
    IMAGE = XCreateImage(DISPLAY,\
			 DefaultVisual(DISPLAY, DefaultScreen(DISPLAY)),\
			 1,\
			 XYBitmap,\
			 0,\
			 DATA,\
			 WIDTH, HEIGHT,\
			 8,\
			 (WIDTH+7) >> 3);\
    IMAGE->byte_order = BYTE_ORDER;\
    IMAGE->bitmap_unit = 8;\
    IMAGE->bitmap_bit_order = LSBFirst;\
}

Boolean _XmInstallImage(XImage *image, char *image_name,
		        int hot_x, int hot_y);
Boolean _XmGetImage(Screen *screen, char *image_name, XImage **image);
Boolean _XmGetPixmapData(Screen *screen,
			 Pixmap pixmap,
			 char **image_name,
			 int *depth,
			 Pixel *foreground,
			 Pixel *background,
			 int *hot_x,
			 int *hot_y,
			 unsigned int *width,
			 unsigned int *height);
Boolean XmeGetPixmapData(Screen *screen,
			 Pixmap pixmap,
			 char **image_name,
			 int *depth,
			 Pixel *foreground,
			 Pixel *background,
			 int *hot_x,
			 int *hot_y,
			 unsigned int *width,
			 unsigned int *height);
Pixmap _XmGetPixmap(Screen *screen,
		    char *image_name,
		    int depth,
		    Pixel foreground,
		    Pixel background);
Boolean _XmInstallPixmap(Pixmap pixmap,
			 Screen *screen,
			 char *image_name,
			 Pixel foreground,
			 Pixel background);

/************************** MapEvent.c *****************************/

Boolean _XmMapBtnEvent(String str,
		       int *eventType,
		       unsigned int *button,
		       unsigned int *modifiers);
Boolean _XmMapKeyEvent(String str,
		       int *eventType,
		       unsigned *keysym,
		       unsigned int *modifiers);
Boolean _XmMatchBtnEvent(XEvent *event,
		         int eventType,
			 unsigned int button,
			 unsigned int modifiers);
Boolean _XmMatchKeyEvent(XEvent *event,
			 int eventType,
			 unsigned int key,
			 unsigned int modifiers);

/************************** ReadImage.c *****************************/

XImage *_XmGetImageFromFile(char *filename);
XImage *_XmGetImageAndHotSpotFromFile(char *filename, int *hot_x, int *hot_y);

/************************* ResConvert.c *****************************/

enum { XmLABEL_FONTLIST = 1,
       XmBUTTON_FONTLIST,
       XmTEXT_FONTLIST
};

void _XmRegisterConverters(void);
void _XmWarning(Widget w, char *message, ...);
void XmeWarning(Widget w, char *message, ...);
Boolean _XmStringsAreEqual(char *in_str, char *text_str);
Boolean XmeNamesAreEqual(char *in_str, char *text_str);
XmFontList _XmGetDefaultFontList(Widget w, unsigned char fontListType);
char *_XmConvertCSToString(XmString cs);
Boolean _XmCvtXmStringToCT(XrmValue *from, XrmValue *to);

/**************************** ResInd.c *****************************/

void _XmBuildResources(XmSyntheticResource **wc_resources_ptr,
		       int *wc_num_resources_ptr,
		       XmSyntheticResource *sc_resources,
		       int sc_num_resources);
void _XmInitializeSyntheticResources(XmSyntheticResource *resources,
				     int num_resources);
void _XmPrimitiveGetValuesHook(Widget w,
			       ArgList args,
			       Cardinal *num_args);
void _XmGadgetGetValuesHook(Widget w,
			    ArgList args,
			    Cardinal *num_args);
void _XmManagerGetValuesHook(Widget w,
			     ArgList args,
			     Cardinal *num_args);
void _XmExtGetValuesHook(Widget w,
			 ArgList args,
			 Cardinal *num_args);
void _XmExtImportArgs(Widget w,
		      ArgList args,
		      Cardinal *num_args);
void _XmPrimitiveImportArgs(Widget w,
			    ArgList args,
			    Cardinal *num_args);
void _XmGadgetImportArgs(Widget w,
			 ArgList args,
			 Cardinal *num_args);
void _XmGadgetImportSecondaryArgs(Widget w,
				  ArgList args,
				  Cardinal *num_args);
void _XmManagerImportArgs(Widget w,
			  ArgList args,
			  Cardinal *num_args);
int _XmConvertUnits(Screen *screen,
		    int dimension,
		    int from_type,
		    int from_val,
		    int to_type);
XmImportOperator _XmToHorizontalPixels(Widget widget,
				       int offset,
				       XtArgVal *value);
XmImportOperator XmeToHorizontalPixels(Widget widget,
				       int offset,
				       XtArgVal *value);
XmImportOperator _XmToVerticalPixels(Widget widget,
				     int offset,
				     XtArgVal *value);
XmImportOperator XmeToVerticalPixels(Widget widget,
				     int offset,
				     XtArgVal *value);
void _XmFromHorizontalPixels(Widget widget,
			     int offset,
			     XtArgVal *value);
void XmeFromHorizontalPixels(Widget widget,
			     int offset,
			     XtArgVal *value);
void _XmFromVerticalPixels(Widget widget,
			   int offset,
			   XtArgVal *value);
void XmeFromVerticalPixels(Widget widget,
			   int offset,
			   XtArgVal *value);
void _XmSortResourceList(XrmResource *list[], Cardinal len);
void _XmUnitTypeDefault(Widget widget,
		        int offset,
		        XrmValue *value);
unsigned char _XmGetUnitType(Widget widget);

/************************* UniqueEvent.c *****************************/

Boolean _XmIsEventUnique(XEvent *event);
void _XmRecordEvent(XEvent *event);

/*************************** Visual.c ********************************/

#define XmLOOK_AT_SCREEN          (1<<0)
#define XmLOOK_AT_CMAP            (1<<1)
#define XmLOOK_AT_BACKGROUND      (1<<2)
#define XmLOOK_AT_FOREGROUND      (1<<3)
#define XmLOOK_AT_TOP_SHADOW      (1<<4)
#define XmLOOK_AT_BOTTOM_SHADOW   (1<<5)
#define XmLOOK_AT_SELECT          (1<<6)

#define XmBACKGROUND     ((unsigned char) (1<<0))
#define XmFOREGROUND     ((unsigned char) (1<<1))
#define XmTOP_SHADOW     ((unsigned char) (1<<2))
#define XmBOTTOM_SHADOW  ((unsigned char) (1<<3))
#define XmSELECT         ((unsigned char) (1<<4))

typedef struct _XmColorData {
   Screen * screen;
   Colormap color_map;
   unsigned char allocated;
   XColor background;
   XColor foreground;
   XColor top_shadow;
   XColor bottom_shadow;
   XColor select;
} XmColorData;

void _XmRegisterPixmapConverters(void);
char *_XmGetBGPixmapName(void);
void _XmClearBGPixmap(void);
void _XmForegroundColorDefault(Widget widget, int offset, XrmValue *value);
void _XmHighlightColorDefault(Widget widget, int offset, XrmValue *value);
void _XmBackgroundColorDefault(Widget widget, int offset, XrmValue *value);
void _XmTopShadowColorDefault(Widget widget, int offset, XrmValue *value);
void _XmBottomShadowColorDefault(Widget widget, int offset, XrmValue *value);
void _XmPrimitiveTopShadowPixmapDefault(Widget widget, int offset,
				        XrmValue *value);
void _XmManagerTopShadowPixmapDefault(Widget widget, int offset,
				      XrmValue *value);
void _XmPrimitiveHighlightPixmapDefault(Widget widget, int offset,
					XrmValue *value);
void _XmManagerHighlightPixmapDefault(Widget widget, int offset,
				      XrmValue *value);
void _XmGetDefaultThresholdsForScreen(Screen *screen);
String _XmGetDefaultBackgroundColorSpec(Screen *screen);
void _XmSetDefaultBackgroundColorSpec(Screen *screen, String new_color_spec);
XmColorData *_XmGetDefaultColors(Screen *screen, Colormap color_map);
Boolean _XmSearchColorCache(unsigned int which, XmColorData *values,
			    XmColorData **ret);
XmColorData *_XmAddToColorCache(XmColorData *new_rec);
Pixel _XmBlackPixel(Screen *screen, Colormap colormap, XColor blackcolor);
Pixel _XmWhitePixel(Screen *screen, Colormap colormap, XColor whitecolor);
Pixel _XmAccessColorData(XmColorData *cd, unsigned char which);
XmColorData *_XmGetColors(Screen *screen, Colormap color_map, Pixel background);
void _XmSelectColorDefault(Widget w, int offset, XrmValue *val);

/**************************** XmString.c **********************************/

XFontStruct *_XmGetFirstFont(XmFontListEntry entry);
Boolean _XmFontListGetDefaultFont(XmFontList fontlist,
				  XFontStruct **font_struct);
Boolean _XmFontListSearch(XmFontList fontlist, XmStringCharSet charset,
			  short *indx, XFontStruct **font_struct);
Boolean _XmStringIsXmString(XmString string);
Boolean _XmStringInitContext(_XmStringContext *context, _XmString string);
Boolean _XmStringGetNextSegment(_XmStringContext context,
				XmStringCharSet *charset,
				XmStringDirection *direction,
				char **text,
				short *char_count,
				Boolean *separator) ;
void _XmStringFreeContext(_XmStringContext context) ;
Dimension _XmStringWidth(XmFontList fontlist, _XmString string) ;
Dimension _XmStringHeight(XmFontList fontlist, _XmString string) ;
void _XmStringExtent(XmFontList fontlist, _XmString string,
		     Dimension *width, Dimension *height) ;
Boolean _XmStringEmpty(_XmString string);
void _XmStringDraw(Display *d, Window w, XmFontList fontlist, _XmString string,
		   GC gc, Position x, Position y, Dimension width,
		   unsigned char align, unsigned char lay_dir,
		   XRectangle *clip);
void _XmStringDrawImage(Display *d, Window w,
			XmFontList fontlist, _XmString string,
			GC gc, Position x, Position y, Dimension width,
			unsigned char align, unsigned char lay_dir,
			XRectangle *clip);
void _XmStringDrawUnderline(Display *d, Window w, XmFontList f, _XmString s,
			    GC gc, Position x, Position y, Dimension width,
			    unsigned char align, unsigned char lay_dir,
			    XRectangle *clip, _XmString u);
void _XmStringDrawMnemonic(Display *d, Window w,
                           XmFontList fontlist, _XmString string,
                           GC gc, Position x, Position y, Dimension width,
                           unsigned char alignment,
                           unsigned char layout_direction,
                           XRectangle *clip,
                           String mnemonic, XmStringCharSet charset);
_XmString _XmStringCreate(XmString cs);
void _XmStringFree(_XmString string);
char *_XmStringGetCurrentCharset(void) ;
char *_XmCharsetCanonicalize(String charset);
void _XmStringUpdate(XmFontList fontlist, _XmString string);
_XmString _XmStringCopy(_XmString string);
Boolean _XmStringByteCompare(_XmString a, _XmString b);
Boolean _XmStringHasSubstring(_XmString string, _XmString substring);
XmString _XmStringCreateExternal(XmFontList fontlist, _XmString cs);
Dimension _XmStringBaseline(XmFontList fontlist, _XmString string);
int _XmStringLineCount(_XmString string);
char * _XmStringGetTextConcat(XmString string);
Boolean _XmStringIsCurrentCharset(XmStringCharSet c);
Boolean _XmStringSingleSegment(XmString str, char **pTextOut,
			       XmStringCharSet *pCharsetOut);
void _XmStringUpdateWMShellTitle(XmString xmstr, Widget shell);
void XmeStringUpdateWMShellTitle(XmString xmstr, Widget shell);

/************************* Traversal.c ********************************/

#define XmTAB_ANY	((XmNavigationType)255)
#define XmNONE_OR_BC	((XmNavigationType)254)

typedef struct _XmFocusMovedCallbackStruct {
    int 	reason;
    XEvent  	*event;
    Boolean 	cont;
    Widget	old_focus;
    Widget	new_focus;
    unsigned char focus_policy;
} XmFocusMovedCallbackStruct, *XmFocusMovedCallback;

typedef struct _XmFocusDataRec *XmFocusData;

XmFocusData _XmCreateFocusData(void);
void _XmDestroyFocusData(XmFocusData focusData);
void _XmSetActiveTabGroup(XmFocusData focusData, Widget tabGroup);
Widget _XmGetActiveItem(Widget w);
void _XmNavigInitialize(Widget request, Widget new_wid,
			ArgList args, Cardinal *num_args);
Boolean _XmNavigSetValues(Widget current, Widget request, Widget new_wid,
			  ArgList args, Cardinal *num_args);
void _XmNavigChangeManaged(Widget wid);
void XmeNavigChangeManaged(Widget wid);
void _XmNavigResize(Widget wid);
void _XmValidateFocus(Widget wid);
void _XmNavigDestroy(Widget wid);
Boolean _XmCallFocusMoved(Widget old, Widget new_wid, XEvent *event);
Boolean _XmMgrTraversal(Widget wid, XmTraversalDirection direction);
void _XmClearFocusPath(Widget wid);
Boolean _XmFocusIsHere(Widget w);
void _XmProcessTraversal(Widget w, XmTraversalDirection dir, Boolean check);
unsigned char _XmGetFocusPolicy(Widget w);
Widget _XmFindTopMostShell(Widget w);
void _XmFocusModelChanged(Widget wid,
			  XtPointer client_data, XtPointer call_data);
Boolean _XmGrabTheFocus(Widget w, XEvent *event);
XmFocusData _XmGetFocusData(Widget wid);
Boolean _XmCreateVisibilityRect(Widget w, XRectangle *rectPtr);
void _XmSetRect(XRectangle *rect, Widget w);
int _XmIntersectRect(XRectangle *srcRectA, Widget widget, XRectangle *dstRect);
int _XmEmptyRect(XRectangle *r);
void _XmClearRect(XRectangle *r);
Boolean _XmIsNavigable(Widget wid);
void _XmWidgetFocusChange(Widget wid, XmFocusChange change);
Widget _XmNavigate(Widget wid, XmTraversalDirection direction);
Widget _XmFindNextTabGroup(Widget wid);
Widget _XmFindPrevTabGroup(Widget wid);
void _XmSetInitialOfTabGroup(Widget tab_group, Widget init_focus);
void _XmResetTravGraph(Widget wid);
Boolean _XmFocusIsInShell(Widget wid);
Boolean XmeFocusIsInShell(Widget wid);
Boolean _XmShellIsExclusive(Widget wid);
Widget _XmGetFirstFocus(Widget wid);
				
/*********************** TravAct.c ******************************/

void _XmTrackShellFocus(Widget wid,
			XtPointer client_data,
			XEvent *event,
			Boolean *dontSwallow);

void _XmManagerEnter(Widget wid,
		     XEvent *event_in,
		     String *params,
		     Cardinal *num_params);

void _XmManagerLeave(Widget wid,
		     XEvent *event_in,
		     String *params,
		     Cardinal *num_params);

void _XmManagerFocusInInternal(Widget wid,
			       XEvent *event,
			       String *params,
			       Cardinal *num_params);

void _XmManagerFocusIn(Widget mw,
		       XEvent *event,
		       String *params,
		       Cardinal *num_params);

void _XmManagerFocusOut(Widget wid,
		        XEvent *event,
		        String *params,
		        Cardinal *num_params);

void _XmManagerUnmap(Widget mw,
		     XEvent *event,
		     String *params,
		     Cardinal *num_params);

void _XmPrimitiveEnter(Widget w, 
		       XEvent *event, 
		       String *params, 
		       Cardinal *num_params);

void _XmPrimitiveLeave(Widget w, 
		       XEvent *event, 
		       String *params, 
		       Cardinal *num_params);

void _XmPrimitiveFocusOut(Widget w, 
			  XEvent *event, 
			  String *params, 
			  Cardinal *num_params);

void _XmPrimitiveFocusIn(Widget w, 
			 XEvent *event, 
			 String *params, 
			 Cardinal *num_params);

void _XmPrimitiveUnmap(Widget w,
		       XEvent *event,
		       String *params,
		       Cardinal *num_params);

void _XmEnterGadget(Widget w,
		    XEvent *event,
		    String *params,
		    Cardinal *num_params);

void _XmLeaveGadget(Widget w,
		    XEvent *event,
		    String *params,
		    Cardinal *num_params);

void _XmFocusInGadget(Widget w,
		      XEvent *event,
		      String *params,
		      Cardinal *num_params);

void _XmFocusOutGadget(Widget w,
		       XEvent *event,
		       String *params,
		       Cardinal *num_params);

/************************ GeoUtils.c ***************************/

void _XmGeoAdjustBoxes(XmGeoMatrix geoSpec);
void _XmGeoArrangeBoxes(XmGeoMatrix geoSpec, Position x, Position y,
			       Dimension *pW, Dimension *pH);
Dimension _XmGeoBoxesSameWidth(XmKidGeometry rowPtr, Dimension width);
Dimension _XmGeoBoxesSameHeight(XmKidGeometry rowPtr, Dimension height);
void _XmGeoClearRectObjAreas(RectObj r, XWindowChanges *old);
int _XmGeoCount_kids(CompositeWidget c);
void _XmGeoGetDimensions(XmGeoMatrix geoSpec);
XmKidGeometry _XmGetKidGeo(Widget wid, Widget instigator,
			   XtWidgetGeometry *request, 
			   int uniform_border, Dimension border,
			   int uniform_width_margins, 
			   int uniform_height_margins,
			   Widget help, int geo_type);
void _XmGeoLoadValues(Widget wid, int geoType, Widget instigator, 
		      XtWidgetGeometry *request,
		      XtWidgetGeometry *geoResult);
XmGeoMatrix _XmGeoMatrixAlloc(unsigned int numRows,
			      unsigned int numBoxes,
			      unsigned int extSize);
void _XmGeoMatrixFree(XmGeoMatrix geo_spec);
void _XmGeoMatrixGet(XmGeoMatrix geoSpec, int geoType);
void _XmGeoMatrixSet(XmGeoMatrix geoSpec);
Boolean _XmGeoReplyYes(Widget wid, XtWidgetGeometry *desired,
				   XtWidgetGeometry *response);
Boolean _XmGeoSetupKid(XmKidGeometry geo, Widget kidWid);
Boolean _XmGeometryEqual(Widget wid, XtWidgetGeometry *geoA,
				     XtWidgetGeometry *geoB);
void _XmHandleSizeUpdate(Widget wid, unsigned char policy,
			 XmGeoCreateProc createMatrix);
XtGeometryResult _XmHandleQueryGeometry(Widget wid,
				        XtWidgetGeometry *intended,
				        XtWidgetGeometry *desired, 
				        unsigned char policy,
				        XmGeoCreateProc createMatrix);
XtGeometryResult _XmHandleGeometryManager(Widget wid, Widget instigator,
					  XtWidgetGeometry *desired, 
					  XtWidgetGeometry *allowed,
					  unsigned char policy, 
					  XmGeoMatrix *cachePtr,
					  XmGeoCreateProc createMatrix);
XtGeometryResult _XmMakeGeometryRequest(Widget w,
					XtWidgetGeometry *geom);
void _XmMenuBarFix(XmGeoMatrix geoSpec, int action,
		   XmGeoMajorLayout layoutPtr, XmKidGeometry rowPtr);
void _XmSeparatorFix(XmGeoMatrix geoSpec, int action,
		     XmGeoMajorLayout layoutPtr, XmKidGeometry rowPtr);
void _XmSetKidGeo(XmKidGeometry kg, Widget instigator);

/*************************** Region.c ***************************/
XmRegion _XmRegionCreate(void);
XmRegion _XmRegionCreateSize(long size);
void _XmRegionComputeExtents(XmRegion r);
void _XmRegionGetExtents(XmRegion r, XRectangle *rect);
void _XmRegionUnionRectWithRegion(XRectangle *rect,
				  XmRegion source,
				  XmRegion dest);
void _XmRegionIntersectRectWithRegion(XRectangle *rect,
				      XmRegion source,
				      XmRegion dest);
long _XmRegionGetNumRectangles(XmRegion r);
void _XmRegionGetRectangles(XmRegion r,
			    XRectangle **rects,
			    long *nrects);
void _XmRegionSetGCRegion(Display *dpy,
			  GC gc,
			  int x_origin,
			  int y_origin,
			  XmRegion r);
void _XmRegionDestroy(XmRegion r);
void _XmRegionOffset(XmRegion pRegion, int x, int y);
void _XmRegionIntersect(XmRegion reg1, XmRegion reg2, XmRegion newReg);
void _XmRegionUnion(XmRegion reg1, XmRegion reg2, XmRegion newReg);
void _XmRegionSubtract(XmRegion regM, XmRegion regS, XmRegion regD);
Boolean _XmRegionIsEmpty(XmRegion r);
Boolean _XmRegionEqual(XmRegion r1, XmRegion r2);
Boolean _XmRegionPointInRegion(XmRegion pRegion, int x, int y);
void _XmRegionClear(XmRegion r );
void _XmRegionShrink(XmRegion r, int dx, int dy);
void _XmRegionDrawShadow(Display *display,
			 Drawable d,
			 GC top_gc,
			 GC bottom_gc,
			 XmRegion region,
			 Dimension border_thick,
			 Dimension shadow_thick,
			 unsigned int shadow_type);

/****************************** Dest.c ***************************/

void _XmSetDestination(Display *dpy, Widget w);

/***************************** XmIm.c ****************************/

void _XmImChangeManaged(Widget vw);
void _XmImRealize(Widget vw);
void _XmImResize(Widget vw);
void _XmImRedisplay(Widget vw);

/************************* DragBS.c ******************************/

void _XmInitAtomPairs(Display *display);
void _XmInitTargetsTable(Display *display);
Cardinal _XmIndexToTargets(Widget shell, Cardinal t_index, Atom **targetsRtn);
Cardinal _XmTargetsToIndex(Widget shell, Atom *targets, Cardinal numTargets);
Atom _XmAllocMotifAtom(Widget shell, Time time);
void _XmFreeMotifAtom(Widget shell, Atom atom);
void _XmDestroyMotifWindow(Display *dpy);
Window _XmGetDragProxyWindow(Display *display);

/************************* DragOverS.c ***************************/

void _XmDragOverHide(Widget w, Position clipOriginX, Position clipOriginY,
		     XmRegion clipRegion);
void _XmDragOverShow(Widget w, Position clipOriginX, Position clipOriginY,
		     XmRegion clipRegion);
void _XmDragOverMove(Widget w, Position x, Position y);
void _XmDragOverChange(Widget w, unsigned char dropSiteStatus);
void _XmDragOverFinish(Widget w, unsigned char completionStatus);
Cursor _XmDragOverGetActiveCursor( Widget w);
void _XmDragOverSetInitialPosition(Widget w,
				   Position initialX, Position initialY);

/************************** DragUnder.c *******************************/

void _XmDragUnderAnimation(Widget w, XtPointer clientData, XtPointer callData);

/**********************************************************************/

/****************** THESE AREN'T SUPPOSED TO BE USED ******************/

#ifdef XM_1_1_BC

#define XmVPANED_BIT		XmPANED_WINDOW_BIT

#define LOOK_AT_SCREEN		(1<<0)
#define LOOK_AT_CMAP		(1<<1)
#define LOOK_AT_BACKGROUND	(1<<2)
#define LOOK_AT_FOREGROUND	(1<<3)
#define LOOK_AT_TOP_SHADOW	(1<<4)
#define LOOK_AT_BOTTOM_SHADOW	(1<<5)
#define LOOK_AT_SELECT		(1<<6)

#define XmStrlen(s)		((s) ? strlen(s) : 0)

#define DEFAULT_INDICATOR_DIM	9

#ifndef MAX
#define MAX(a,b)		((a) > (b) ? (a) : (b))
#endif

#define RX(r)		(((RectObj)(r))->rectangle.x)
#define RY(r)		(((RectObj)(r))->rectangle.y)
#define RWidth(r)	(((RectObj)(r))->rectangle.width)
#define RHeight(r)	(((RectObj)(r))->rectangle.height)
#define RBorder(r)	(((RectObj)(r))->rectangle.border_width)

#define GMode(g)	((g)->request_mode)
#define IsX(g)		(GMode(g) & CWX)
#define IsY(g)		(GMode(g) & CWY)
#define IsWidth(g)	(GMode(g) & CWWidth)
#define IsHeight(g)	(GMode(g) & CWHeight)
#define IsBorder(g)	(GMode(g) & CWBorderWidth)
#define IsWidthHeight(g) ((GMode(g) & CWWidth) || (GMode(g) & CWHeight))
#define IsQueryOnly(g)	(GMode(g) & XtCWQueryOnly)

#define MAXDIMENSION    ((1 << 31)-1)

#define Max(x, y)       (((x) > (y)) ? (x) : (y))
#define Min(x, y)       (((x) < (y)) ? (x) : (y))
#define AssignMax(x, y) if ((y) > (x)) x = (y)
#define AssignMin(x, y) if ((y) < (x)) x = (y)


#define DIALOG_SUFFIX "_popup"
#define DIALOG_SUFFIX_SIZE 6

#define XM_3D_ENHANCE_PIXEL 2
#define XM_DEFAULT_TOP_MARGIN 0
#define XM_DEFAULT_BOTTOM_MARGIN 0

extern WidgetClass xmWorldObjectClass;
extern WidgetClass xmDesktopObjectClass;
extern WidgetClass xmDisplayObjectClass;
extern WidgetClass xmScreenObjectClass;

#endif /* XM_1_1_BC */

/**********************************************************************/

#include <Xm/VendorSP.h>
#include <Xm/ManagerP.h>
#include <Xm/PrimitiveP.h>
#include <Xm/GadgetP.h>
#include <Xm/BaseClassP.h> 

#ifdef __cplusplus
}
#endif

#endif /* XM_P_H */
