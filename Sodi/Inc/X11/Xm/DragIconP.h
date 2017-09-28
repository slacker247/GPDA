/**
 *
 * $Id: DragIconP.h,v 1.1 1997/02/20 22:28:49 miers Exp $
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

#ifndef _XM_DRAG_ICON_P_H
#define _XM_DRAG_ICON_P_H

#include <Xm/VendorSEP.h>
#include <Xm/DragIcon.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef void (*XmCloneVisualProc)(XmDragIconObject, Widget, Widget);
typedef void (*XmMovePixmapProc)(XmDragIconObject,
				 XmDragIconObject,
				 XmDragIconObject,
				 Position, Position);

typedef struct {
    XtPointer extension;
} XmDragIconClassPart;

/*
 * this is a lie.  The parent class of XmDragIcon is the object
 * class.  But for compatibility, we leave it this way.
 */
typedef struct _XmDragIconClassRec {
    RectObjClassPart rectangle_class;
    XmDragIconClassPart dragIcon_class;
} XmDragIconClassRec;

extern XmDragIconClassRec xmDragIconClassRec;

typedef struct {
    Cardinal depth;
    Pixmap pixmap;
    Dimension width, height;
    Pixmap mask;
    Position hot_x, hot_y;
    Position offset_x, offset_y;
    unsigned char attachment;
    Boolean isDirty;
    Region      region;
    Region      restore_region;
    Position    x_offset, y_offset;
} XmDragIconPart, *XmDragIconPartPtr;

typedef struct _XmDragIconRec {
    ObjectPart object;
    RectObjPart rectangle;
    XmDragIconPart drag;
} XmDragIconRec;

extern Widget _XmGetTextualDragIcon(Widget w);
extern Widget XmeGetTextualDragIcon(Widget w);
extern void _XmDestroyDefaultDragIcon(XmDragIconObject icon);
extern Boolean _XmDragIconIsDirty(XmDragIconObject icon);
extern void _XmDragIconClean(XmDragIconObject icon1,
                            XmDragIconObject icon2,
                            XmDragIconObject icon3);

#ifdef __cplusplus
}
#endif

#endif
