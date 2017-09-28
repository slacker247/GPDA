/**
 *
 * $Id: DialogSP.h,v 1.1 1997/02/20 22:28:43 miers Exp $
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

#ifndef XM_DIALOGS_P_H
#define XM_DIALOGS_P_H

#include <Xm/XmP.h>
#include <Xm/DialogS.h>
#include <X11/ShellP.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
   XtPointer extension;
} XmDialogShellPart;

/* Define the full instance record */
typedef struct _XmDialogShellRec {
    CorePart core;
    CompositePart composite;
    ShellPart shell;
    WMShellPart wmshell;
    VendorShellPart vendor;
    TransientShellPart transient;
    XmDialogShellPart dialogshell;
} XmDialogShellRec;

/* Define class part structure */
typedef struct {
    XtPointer extension;
} XmDialogShellClassPart;

/* Defint the full class record */
typedef struct _XmDialogShellClassRec {
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ShellClassPart shell_class;
    WMShellClassPart wmshell_class;
    VendorShellClassPart vendor_class;
    TransientShellClassPart transient_class;
    XmDialogShellClassPart dialogshell_class;
} XmDialogShellClassRec;

extern XmDialogShellClassRec xmDialogShellClassRec;

#ifdef __cplusplus
}
#endif

#endif /* XM_DIALOGS_P_H */
