/**
 *
 * $Id: LabelG.h,v 1.1 1997/02/20 22:29:09 miers Exp $
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

#ifndef LABELG_H_INCLUDED
#define LABELG_H_INCLUDED

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

extern WidgetClass xmLabelGadgetClass;

typedef struct _XmLabelGadgetRec *XmLabelGadget;
typedef struct _XmLabelGadgetClassRec *XmLabelGadgetClass;
typedef struct _XmLabelGCacheObjRec *XmLabelGCacheObject;

#ifndef XmIsLabelGadget
#define XmIsLabelGadget(w) XtIsSubclass((w), xmLabelGadgetClass)
#endif

extern Widget XmCreateLabelGadget(Widget parent,
				  char *name,
				  Arg *arglist,
				  Cardinal argCount);

extern Widget XmCreateLabelGadget(Widget parent, char *name, Arg *arglist, Cardinal argcount);

#ifdef __cplusplus
}
#endif

#endif /* LABEL_H_INCLUDED */
