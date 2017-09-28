/**
 * 
 * $Id: BulletinB.h,v 1.1 1997/02/20 22:28:29 miers Exp $
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

#ifndef XM_BULLETINB_H
#define XM_BULLETINB_H

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

extern WidgetClass xmBulletinBoardWidgetClass;

typedef struct _XmBulletinBoardRec *XmBulletinBoardWidget;
typedef struct _XmBulletinBoardClassRec *XmBulletinBoardWidgetClass;

#ifndef XmIsBulletinBoard
#define XmIsBulletinBoard(w) XtIsSubclass((w), xmBulletinBoardWidgetClass)
#endif

extern Widget XmCreateBulletinBoard(Widget parent, char *name, Arg *arglist, Cardinal argcount);
extern Widget XmCreateBulletinBoardDialog(Widget parent, char *name, Arg *arglist, Cardinal argcount);

#ifdef __cplusplus
}
#endif

#endif /* XM_BULLETINB_H */
