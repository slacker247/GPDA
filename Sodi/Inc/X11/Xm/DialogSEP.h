/**
 *
 * $Id: DialogSEP.h,v 1.1 1997/02/20 22:28:42 miers Exp $
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

#ifndef _XM_DIALOG_S_E_P
#define _XM_DIALOG_S_E_P

#include <Xm/VendorSEP.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifndef XmIsDialogShellExt
#define XmIsDialogShellExt(w)	XtIsSubclass(w, xmDialogShellExtObjectClass)
#endif

extern WidgetClass xmDialogShellExtObjectClass;

typedef struct _XmDialogShellExtClassRec *XmDialogShellExtObjectClass;
typedef struct _XmDialogShellExtRec *XmDialogShellExtObject;


typedef struct _XmDialogShellExtClassPart{
   XtPointer extension;   /* Pointer to extension record */
}XmDialogShellExtClassPart, *XmDialogShellExtClassPartPtr;

typedef struct _XmDialogShellExtClassRec{
    ObjectClassPart object_class;
    XmExtClassPart ext_class;
    XmDesktopClassPart desktop_class;
    XmShellExtClassPart shell_class;
    XmVendorShellExtClassPart vendor_class;
    XmDialogShellExtClassPart dialog_class;
}XmDialogShellExtClassRec;

typedef struct _XmDialogShellExtPart{
    int empty;
} XmDialogShellExtPart;

extern XmDialogShellExtClassRec xmDialogShellExtClassRec;

typedef struct _XmDialogShellExtRec{
    ObjectPart object;
    XmExtPart ext;
    XmDesktopPart desktop;
    XmShellExtPart shell;
    XmVendorShellExtPart vendor;
    XmDialogShellExtPart dialog;
}XmDialogShellExtRec;

#ifdef __cplusplus
}
#endif

#endif
