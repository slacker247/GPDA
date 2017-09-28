/**
 *
 * $Id: DragIcon.h,v 1.2 1997/02/22 04:41:38 miers Exp $
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

#ifndef XM_DRAGICON_H
#define XM_DRAGICON_H

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

enum {
    XmATTACH_NORTH_WEST,
    XmATTACH_NORTH,
    XmATTACH_NORTH_EAST,
    XmATTACH_EAST,
    XmATTACH_SOUTH_EAST,
    XmATTACH_SOUTH,
    XmATTACH_SOUTH_WEST,
    XmATTACH_WEST,
    XmATTACH_CENTER,
    XmATTACH_HOT
};

#ifndef XmIsDragIcon
#define XmIsDragIcon(w) XtIsSubclass(w, xmDragIconObjectClass)
#endif

typedef struct _XmDragIconRec *XmDragIconObject;
typedef struct _XmDragIconClassRec *XmDragIconObjectClass;
extern WidgetClass xmDragIconObjectClass;

Widget XmCreateDragIcon (Widget widget, String name, ArgList arglist, Cardinal argcount);

#ifdef __cplusplus
}
#endif 

#endif /* XM_DRAGICON_H */
