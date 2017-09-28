/** 
 *
 * $Id: FormP.h,v 1.5 1999/06/09 14:02:30 rwscott Exp $
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


#ifndef XM_FORM_P_H
#define XM_FORM_P_H

#include <Xm/XmP.h>
#include <Xm/Form.h>
#include <Xm/BulletinBP.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Frankly I don't know where these come from.
 * They don't seem to be present in the OSF/Motif docs anywhere,
 * nor in anything else I found.
 */
#if 0
#define SORTED_LEFT_TO_RIGHT    (1 << 0)
#define SORTED_TOP_TO_BOTTOM    (1 << 1)
#define DONT_USE_BOTTOM         (1 << 2)
#define DONT_USE_RIGHT          (1 << 3)
#define WIDGET_DONT_USE_BOTTOM  (1 << 4)
#define WIDGET_DONT_USE_RIGHT   (1 << 5)
#define LEFT_NOT_MOVABLE        (1 << 6)
#define RIGHT_NOT_MOVABLE       (1 << 7)
#define TOP_NOT_MOVABLE         (1 << 8)
#define BOTTOM_NOT_MOVABLE      (1 << 9)
#define NOT_MOVABLE_RIGHT       (1 << 10)
#define NOT_MOVABLE_DOWN        (1 << 11)
#define DONT_ATTEMPT_MOVE_RIGHT (1 << 12)
#define DONT_ATTEMPT_MOVE_DOWN  (1 << 13)
#endif

/* Define the form instance part */
typedef struct {
    Dimension horizontal_spacing;
    Dimension vertical_spacing;
    int fraction_base;
    Boolean rubber_positioning;
    Widget first_child;
    Boolean initial_width, initial_height;
    Boolean processing_constraints;
} XmFormPart;

/* define the full instance record */
typedef struct _XmFormRec {
    CorePart core;
    CompositePart composite;
    ConstraintPart constraint;
    XmManagerPart manager;
    XmBulletinBoardPart bulletin_board;
    XmFormPart form;
} XmFormRec;

/* Define class part structure */
typedef struct {
    XtPointer extension;
} XmFormClassPart;

/* Define the full class record */
typedef struct _XmFormClassRec {
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ConstraintClassPart constraint_class;
    XmManagerClassPart manager_class;
    XmBulletinBoardClassPart bulletin_board_class;
    XmFormClassPart form_class;
} XmFormClassRec;

typedef struct _XmFormAttachmentRec
{
    unsigned char type;
    Widget w;
    int percent;
    int offset;
    int value;
    int tempValue;
} XmFormAttachmentRec, *XmFormAttachment;

/* Define the Constraint Resources */
typedef struct _XmFormConstraintPart {
    XmFormAttachmentRec	att[4];
    Widget              next_sibling;
    Boolean		sorted;
    Boolean		resizable;
    Dimension		preferred_width, preferred_height;
/* lesstif */
    Position		x, y;
    int			w, h; /* These are not Dimension; can be negative */
    Boolean		width_from_side, height_from_side;	/* See Form.c */
/* lesstif */
} XmFormConstraintPart, *XmFormConstraint;

typedef struct _XmFormConstraintRec {
    XmManagerConstraintPart manager;
    XmFormConstraintPart form;
} XmFormConstraintRec, *XmFormConstraints, *XmFormConstraintPtr;

extern XmFormClassRec xmFormClassRec;

#ifdef __cplusplus
}
#endif

#endif /* XM_FORM_P_H */
