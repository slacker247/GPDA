/**
 *
 * $Id: DrawingAP.h,v 1.2 1998/09/17 10:24:04 kbwj Exp $
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

#ifndef XM_DRAWINGA_P_H
#define XM_DRAWINGA_P_H

#include <Xm/ManagerP.h>
#include <Xm/DrawingA.h>

#ifdef __cplusplus
extern "C" {
#endif

#define XmRESIZE_SWINDOW 10


/* Define the drawing area instance part */
typedef struct {
    Dimension margin_width;
    Dimension margin_height;

    XtCallbackList resize_callback;
    XtCallbackList expose_callback;
    XtCallbackList input_callback;

    unsigned char resize_policy;
} XmDrawingAreaPart;

/* Define the full instance record */
typedef struct _XmDrawingAreaRec {
    CorePart core;
    CompositePart composite;
    ConstraintPart constraint;
    XmManagerPart manager;
    XmDrawingAreaPart drawing_area;
} XmDrawingAreaRec;

/* Define class part structure */
typedef struct {
    XtPointer extension;
} XmDrawingAreaClassPart;

/* Defint the full class record */
typedef struct _XmDrawingAreaClassRec {
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ConstraintClassPart constraint_class;
    XmManagerClassPart manager_class;
    XmDrawingAreaClassPart drawing_area_class;
} XmDrawingAreaClassRec;

extern XmDrawingAreaClassRec xmDrawingAreaClassRec;

/*
 * actions
 */
void _XmDrawingAreaInput(Widget wid, XEvent *event,
			 String *params, Cardinal *num_params);

#ifdef __cplusplus
}
#endif

#endif /* XM_DRAWINGA_P_H */
