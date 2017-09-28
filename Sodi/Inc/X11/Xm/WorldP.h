/**
 *
 * $Id: WorldP.h,v 1.1 1997/02/20 22:30:11 miers Exp $
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
#ifndef _XM_WORLD_P_H
#define _XM_WORLD_P_H

#include <Xm/DesktopP.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifndef XmIsWorldObject
#define XmIsWorldObject(w)	XtIsSubclass(w, xmWorldClass)
#endif

typedef struct _XmWorldRec *XmWorldObject;
typedef struct _XmWorldClassRec *XmWorldObjectClass;
extern WidgetClass xmWorldClass;

typedef struct _XmWorldClassPart {
    XtPointer extension;
} XmWorldClassPart, *XmWorldClassPartPtr;

typedef struct _XmWorldClassRec {
    ObjectClassPart object_class;
    XmExtClassPart ext_class;
    XmDesktopClassPart desktop_class;
    XmWorldClassPart world_class;
} XmWorldClassRec;

typedef struct {
    int	foo;
} XmWorldPart, *XmWorldPartPtr;

extern XmWorldClassRec 	xmWorldClassRec;

typedef struct _XmWorldRec {
    ObjectPart object;
    XmExtPart ext;
    XmDesktopPart desktop;
    XmWorldPart world;
} XmWorldRec;

extern XmWorldObject _XmGetWorldObject(Widget shell,
				       ArgList args,
				       Cardinal *num_args);

#ifdef __cplusplus
}
#endif

#endif
