/**
 * 
 * $Id: FileSB.h,v 1.1 1997/02/20 22:29:01 miers Exp $
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


#ifndef XM_FILESB_H
#define XM_FILESB_H

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef void (*XmSearchProc)(Widget, XtPointer);
typedef void (*XmQualifyProc)(Widget, XtPointer, XtPointer);

extern WidgetClass xmFileSelectionBoxWidgetClass;

typedef struct _XmFileSelectionBoxRec *XmFileSelectionBoxWidget;
typedef struct _XmFileSelectionBoxClassRec *XmFileSelectionBoxWidgetClass;

#ifndef XmIsFileSelectionBox
#define XmIsFileSelectionBox(w) XtIsSubclass((w), xmFileSelectionBoxWidgetClass)
#endif

extern Widget XmCreateFileSelectionBox(Widget parent, char *name, Arg *argList, Cardinal argcount);
extern Widget XmCreateFileSelectionDialog(Widget parent, char *name, Arg *argList, Cardinal argcount);
extern Widget XmFileSelectionBoxGetChild(Widget parent, unsigned char child);
extern void XmFileSelectionDoSearch(Widget widget, XmString dirmask);

#ifdef __cplusplus
}
#endif

#endif /* XM_FILESB_H */
