/**
 *
 * $Id: Frame.h,v 1.1 1997/02/20 22:29:05 miers Exp $
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

#ifndef XM_FRAME_H
#define XM_FRAME_H

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

extern WidgetClass xmFrameWidgetClass;

typedef struct _XmFrameRec *XmFrameWidget;
typedef struct _XmFrameClassRec *XmFrameWidgetClass;

Widget XmCreateFrame(Widget parent,
		     char *name,
		     Arg *arglist,
		     Cardinal argCount);

#ifndef XmIsFrame
#define XmIsFrame(w) XtIsSubclass((w), xmFrameWidgetClass)
#endif

#ifdef __cplusplus
}
#endif

#endif /* XM_FRAME_H */
