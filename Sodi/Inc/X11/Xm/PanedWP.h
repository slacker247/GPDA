/**
 *
 * $Id: PanedWP.h,v 1.1 1997/02/20 22:29:24 miers Exp $
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

#ifndef XM_PANEDW_P_H
#define XM_PANEDW_P_H

#include <Xm/PanedW.h>
#include <Xm/ManagerP.h>

#ifdef __cplusplus
extern "C" {
#endif


/* Define the Constraint Resources */
typedef struct _XmPanedWindowConstraintPart {
    int position;		/* position location in PanedWindow */
    int dheight;		/* desired height */
    Position dy;		/* desired location */
    Position olddy;		/* last value of above */
    Dimension min;		/* min height */
    Dimension max;		/* max height */
    Boolean isPane;		/* true if constraint of a pane, else false */

    Boolean allow_resize;	/* true if child resize requests ok */
    Boolean skip_adjust;	/* true if child height can't change without
				 * user input */
    /* private part */
    Widget sash;               /* Sash widget attached to this child */
    Widget separator;          /* Separator widget attached to this child */
    short position_index;

    /* provided for, but not used */
#ifdef ORIENTED_PANEDW
    unsigned char orientation;
#endif
} XmPanedWindowConstraintPart;

typedef struct _XmPanedWindowConstraintRec {
    XmManagerConstraintPart manager;
    XmPanedWindowConstraintPart panedw;
} XmPanedWindowConstraintRec, *XmPanedWindowConstraintPtr;

/* Define the paned window instance part */
typedef struct {
    Boolean refiguremode;		/* refigure now or later */
    Boolean separator_on;		/* separators visible */

    Dimension margin_height;
    Dimension margin_width;

    Dimension spacing;

    Dimension sash_width;
    Dimension sash_height;
    Dimension sash_shadow_thickness;
    Position sash_indent;

    /* private part */
    int starty;				/* mouse start y */
    short increment_count;		/* sash increment count */
    short pane_count;			/* number of managed panes */
    short num_slots;			/* number of avail slots for children */
    short num_managed_children;		/* number of managed children */

    Boolean recursively_called;		/* for change_managed and creation
					 * of sash and sep children */
    Boolean resize_at_realize;		/* Obscure M*tif comment: for realize
					 * if GeometryNo condition??? */

    XmPanedWindowConstraintPtr top_pane;
    XmPanedWindowConstraintPtr bottom_pane;

    GC flipgc;				/* GC for animating borders */
    WidgetList managed_children;	/* guess */
} XmPanedWindowPart;

/* Define the full instance record */
typedef struct _XmPanedWindowRec {
    CorePart core;
    CompositePart composite;
    ConstraintPart constraint;
    XmManagerPart manager;
    XmPanedWindowPart paned_window;
} XmPanedWindowRec;

/* Define class part structure */
typedef struct {
    XtPointer extension;
} XmPanedWindowClassPart;

/* Define the full class record */
typedef struct _XmPanedWindowClassRec {
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ConstraintClassPart constraint_class;
    XmManagerClassPart manager_class;
    XmPanedWindowClassPart paned_window_class;
} XmPanedWindowClassRec;

extern XmPanedWindowClassRec xmPanedWindowClassRec;

#ifdef __cplusplus
}
#endif


#endif /* XM_PANEDW_P_H */
