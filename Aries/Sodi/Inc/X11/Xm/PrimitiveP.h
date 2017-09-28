/**
 *
 * $Id: PrimitiveP.h,v 1.3 1998/11/15 06:59:41 gritton Exp $
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

#ifndef XM_PRIMITIVE_P_H
#define XM_PRIMITIVE_P_H

#include <Xm/XmP.h>

#ifdef __cplusplus
extern "C" {
#endif

#define PCEPTR(cl) \
    ((XmPrimitiveClassExt *)(&(((XmPrimitiveWidgetClass)(cl))->primitive_class.extension)))
#define _XmGetPrimitiveClassExtPtr(cl, o) \
    ((*PCEPTR(cl) && (((*PCEPTR(cl))->record_type) == (o))) \
	? PCEPTR(cl) \
	: ((XmPrimitiveClassExt *)_XmGetClassExtensionPtr(((XmGenericClassExt *)PCEPTR(cl)), (o))))


/* The Primitive Widget Class Extension Record.. */

#define XmPrimitiveClassExtVersion 1

typedef struct _XmPrimitiveClassExtRec {
    XtPointer next_extension;
    XrmQuark record_type;
    long version;
    Cardinal record_size;
    XmWidgetBaselineProc widget_baseline;
    XmWidgetDisplayRectProc widget_display_rect;
    XmWidgetMarginsProc widget_margins;
} XmPrimitiveClassExtRec, *XmPrimitiveClassExt;

/*
 * Define the primitive instance part
 */
typedef struct {
    Pixel foreground;

    Dimension shadow_thickness;
    Pixel top_shadow_color;
    Pixmap top_shadow_pixmap;
    Pixel bottom_shadow_color;
    Pixmap bottom_shadow_pixmap;

    Dimension highlight_thickness;
    Pixel highlight_color;
    Pixmap highlight_pixmap;

    XtCallbackList help_callback;
    XtPointer user_data;

    Boolean traversal_on;
    Boolean highlight_on_enter;
    Boolean have_traversal;

    unsigned char unit_type;
    XmNavigationType navigation_type;

    Boolean highlight_drawn;
    Boolean highlighted;

    GC highlight_GC;
    GC top_shadow_GC;
    GC bottom_shadow_GC;
} XmPrimitivePart;

/* Define the full instance record */
typedef struct _XmPrimitiveRec {
    CorePart core;
    XmPrimitivePart primitive;
} XmPrimitiveRec;

/* Define class part structure */
typedef struct {
    XtWidgetProc border_highlight;
    XtWidgetProc border_unhighlight;
    String translations;
    XtActionProc arm_and_activate;
    XmSyntheticResource *syn_resources;
    int num_syn_resources;
    XtPointer extension;
} XmPrimitiveClassPart;

/* Define the full class record */
typedef struct _XmPrimitiveClassRec {
    CoreClassPart core_class;
    XmPrimitiveClassPart primitive_class;
} XmPrimitiveClassRec;

/* External definition for class record */
extern XmPrimitiveClassRec xmPrimitiveClassRec;

/* private action procs from Primitive.c */

extern void _XmPrimitiveEnter(Widget w, XEvent *event, String *params, Cardinal *num_params);
extern void _XmPrimitiveLeave(Widget w, XEvent *event, String *params, Cardinal *num_params);
extern void _XmPrimitiveFocusIn(Widget w, XEvent *event, String *params, Cardinal *num_params);
extern void _XmPrimitiveFocusOut(Widget w, XEvent *event, String *params, Cardinal *num_params);
extern void _XmTraverseLeft(Widget w, XEvent *event, String *params, Cardinal *num_params);
extern void _XmTraverseRight(Widget w, XEvent *event, String *params, Cardinal *num_params);
extern void _XmTraverseNext(Widget w, XEvent *event, String *params, Cardinal *num_params);
extern void _XmTraversePrev(Widget w, XEvent *event, String *params, Cardinal *num_params);
extern void _XmTraverseDown(Widget w, XEvent *event, String *params, Cardinal *num_params);
extern void _XmTraverseUp(Widget w, XEvent *event, String *params, Cardinal *num_params);
extern void _XmTraverseHome(Widget w, XEvent *event, String *params, Cardinal *num_params);
extern void _XmTraverseNextTabGroup(Widget w, XEvent *event, String *params, Cardinal *num_params);
extern void _XmTraversePrevTabGroup(Widget w, XEvent *event, String *params, Cardinal *num_params);
extern void _XmPrimitiveParentActivate(Widget w, XEvent *event, String *params, Cardinal *num_params);
extern void _XmPrimitiveParentCancel(Widget w, XEvent *event, String *params, Cardinal *num_params);
extern void _XmPrimitiveHelp(Widget w, XEvent *event, String *params, Cardinal *num_params);
extern void _XmPrimitiveUnmap(Widget w, XEvent *event, String *params, Cardinal *num_params);
extern Boolean _XmDifferentBackground(Widget w, Widget parent);

/*
 * easy access macros
 */
#define Prim_ShadowThickness(w) \
    (((XmPrimitiveWidget)(w))->primitive.shadow_thickness)

#define Prim_HaveTraversal(w) \
    (((XmPrimitiveWidget)(w))->primitive.have_traversal)

#ifdef __cplusplus
}
#endif

#endif /* XM_PRIMITIVE_P_H */
