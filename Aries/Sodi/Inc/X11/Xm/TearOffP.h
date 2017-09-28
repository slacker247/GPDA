/**
 *
 * $Id: TearOffP.h,v 1.1 1997/02/20 22:29:51 miers Exp $
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

#ifndef XM_TEAROFF_P_H
#define XM_TEAROFF_P_H

#include <Xm/XmP.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _XmExcludedParentPaneRec {
   short pane_list_size;
   Widget *pane;
   short num_panes;
} XmExcludedParentPaneRec;
   
extern XmExcludedParentPaneRec _XmExcludedParentPane;

extern void _XmTearOffBtnDownEventHandler(Widget reportingWidget,
					  XtPointer data,
					  XEvent *event,
					  Boolean *cont);

extern void _XmTearOffBtnUpEventHandler(Widget reportingWidget,
					XtPointer data,
					XEvent *event,
					Boolean *cont);

extern void _XmDestroyTearOffShell(Widget w);
   
extern void _XmDismissTearOff(Widget shell, 
			      XtPointer closure, 
			      XtPointer call_data);
   
extern void _XmTearOffInitiate(Widget w,
			       XEvent *event);
   
extern void _XmAddTearOffEventHandlers(Widget w);
   
extern Boolean _XmIsTearOffShellDescendant(Widget w);
   
extern void _XmLowerTearOffObscuringPoppingDownPanes(Widget ancestor,
						     Widget tearOff);
   
extern void _XmRestoreExcludedTearOffToToplevelShell(Widget w,
						     XEvent *event);
   
extern void _XmRestoreTearOffToToplevelShell(Widget w,
					     XEvent *event);
   
extern void _XmRestoreTearOffToMenuShell(Widget w,
					 XEvent *event);
   
#ifdef __cplusplus
}
#endif

#endif /* XM_TEAROFF_P_H */
