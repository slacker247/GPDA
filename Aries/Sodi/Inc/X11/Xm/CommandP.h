/**
 *
 * $Id: CommandP.h,v 1.1 1997/02/20 22:28:36 miers Exp $
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

#ifndef XM_COMMAND_P_H
#define XM_COMMAND_P_H

#include <Xm/Command.h>
#include <Xm/SelectioBP.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Define the command instance part */
typedef struct {
    XtCallbackList callback;
    XtCallbackList value_changed_callback;
    int history_max_items;
    Boolean error;
} XmCommandPart;

/* Define the full instance record */
typedef struct _XmCommandRec {
    CorePart core;
    CompositePart composite;
    ConstraintPart constraint;
    XmManagerPart manager;
    XmBulletinBoardPart bulletin_board;
    XmSelectionBoxPart selection_box;
    XmCommandPart command;
} XmCommandRec;

/* Define class part structure */
typedef struct {
    XtPointer extension;
} XmCommandClassPart;

/* Define the full class record */
typedef struct _XmCommandClassRec {
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ConstraintClassPart constraint_class;
    XmManagerClassPart manager_class;
    XmBulletinBoardClassPart bulletin_board_class;
    XmSelectionBoxClassPart selection_box_class;
    XmCommandClassPart command_class;
} XmCommandClassRec;

extern XmCommandClassRec xmCommandClassRec;

/*
 * actions
 */
void _XmCommandReturn(Widget wid, XEvent *event,
		      String *params, Cardinal *numParams);
void _XmCommandUpOrDown(Widget wid, XEvent *event,
			String *argv, Cardinal *argc);

#ifdef __cplusplus
}
#endif

#endif /* XM_COMMAND_P_H */
