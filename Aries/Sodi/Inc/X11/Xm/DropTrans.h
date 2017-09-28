/**
 *
 * $Id: DropTrans.h,v 1.1 1997/02/20 22:28:58 miers Exp $
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

#ifndef XM_DROPTRANS_H
#define XM_DROPTRANS_H

#ifdef __cplusplus
extern "C" {
#endif

#include <Xm/DropTrans.h>
#include <Xm/Xm.h>

enum {
    XmTRANSFER_FAILURE,
    XmTRANSFER_SUCCESS
};

extern WidgetClass xmDropTransferObjectClass;
   
typedef struct _XmDropTransferClassRec *XmDropTransferObjectClass;
typedef struct _XmDropTransferRec *XmDropTransferObject;

#ifndef XmIsDropTransfer
#define XmIsDropTransfer(w) XtIsSubclass((w), xmDropTransferObjectClass)
#endif
     
typedef struct _XmDropTransferEntryRec {
   XtPointer client_data;
   Atom target;
} XmDropTransferEntryRec, *XmDropTransferEntry;

Widget XmDropTransferStart(Widget refWidget, ArgList args, Cardinal argCount);
void XmDropTransferAdd(Widget widget, XmDropTransferEntry transfers, Cardinal num_transfers);

#ifdef __cplusplus
}
#endif

#endif /* XM_DROPTRANS_H */
