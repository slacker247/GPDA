/**
 * 
 * $Id: MessageB.h,v 1.1 1997/02/20 22:29:20 miers Exp $
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


#ifndef XM_MESSAGEB_H
#define XM_MESSAGEB_H

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

extern WidgetClass xmMessageBoxWidgetClass;

typedef struct _XmMessageBoxRec *XmMessageBoxWidget;
typedef struct _XmMessageBoxClassRec *XmMessageBoxWidgetClass;

#ifndef XmIsMessageBox
#define XmIsMessageBox(w) XtIsSubclass((w), xmMessageBoxWidgetClass)
#endif

extern Widget XmCreateMessageBox(Widget parent, char *name, Arg *argList, Cardinal argcount);
extern Widget XmCreateErrorDialog(Widget parent, char *name, Arg *argList, Cardinal argcount);
extern Widget XmCreateInformationDialog(Widget parent, char *name, Arg *argList, Cardinal argcount);
extern Widget XmCreateMessageDialog(Widget parent, char *name, Arg *argList, Cardinal argcount);
extern Widget XmCreateQuestionDialog(Widget parent, char *name, Arg *argList, Cardinal argcount);
extern Widget XmCreateTemplateDialog(Widget parent, char *name, Arg *argList, Cardinal argcount);
extern Widget XmCreateWarningDialog(Widget parent, char *name, Arg *argList, Cardinal argcount);
extern Widget XmCreateWorkingDialog(Widget parent, char *name, Arg *argList, Cardinal argcount);
extern Widget XmMessageBoxGetChild(Widget parent, unsigned char child);

#ifdef __cplusplus
}
#endif

#endif /* XM_MESSAGEB_H */
