/**
 *
 * $Id: Screen.h,v 1.2 1997/11/07 22:27:44 rwscott Exp $
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
#ifndef _XM_SCREEN_H
#define _XM_SCREEN_H

#ifdef __cplusplus
extern "C" {
#endif

#include <Xm/Xm.h>

#ifndef XmIsScreen
#define XmIsScreen(w) (XtIsSubclass(w, xmScreenClass))
#endif

extern WidgetClass xmScreenClass;

typedef struct _XmScreenRec *XmScreen;
typedef struct _XmScreenClassRec *XmScreenClass;
extern WidgetClass xmScreenClass;

extern Widget XmGetXmScreen(Screen *screen);

#ifdef __cplusplus
}
#endif

#endif /* _XM_SCREEN_H */

