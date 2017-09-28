/**
 *
 * $Id: Display.h,v 1.2 1998/03/27 22:23:55 miers Exp $
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

#ifndef DISPLAY_H_INCLUDED
#define DISPLAY_H_INCLUDED

#include <Xm/Xm.h>
#include <X11/Shell.h>
#include <Xm/DragC.h>
#include <Xm/DropSMgr.h>

#ifdef __cplusplus
extern "C" {
#endif

enum {
    XmDRAG_NONE,
    XmDRAG_DROP_ONLY,
    XmDRAG_PREFER_PREREGISTER,
    XmDRAG_PREREGISTER,
    XmDRAG_PREFER_DYNAMIC,
    XmDRAG_DYNAMIC,
    XmDRAG_PREFER_RECEIVER
};

extern WidgetClass xmDisplayClass;

typedef struct _XmDisplayRec *XmDisplay;
typedef struct _XmDisplayClassRec *XmDisplayClass;

extern Widget XmGetXmDisplay(Display *Dsp);
extern Widget XmGetDragContext(Widget w, Time time);

#define XmGetDisplay(w) XmGetXmDisplay(XtDisplayOfObject(w))

#ifndef XmIsDisplay
#define XmIsDisplay(w) XtIsSubclass((w), xmDisplayClass)
#endif

#ifdef __cplusplus
}
#endif

#endif /* DISPLAY_H_INCLUDED */
