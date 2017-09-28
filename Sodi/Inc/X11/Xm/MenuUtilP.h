/**
 *
 * $Id: MenuUtilP.h,v 1.4 1998/03/12 21:51:35 rwscott Exp $
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

#ifndef MENUUTIL_P_H
#define MENUUTIL_P_H

#include <Xm/XmP.h>
#include <Xm/RowColumnP.h>

#ifdef __cplusplus
extern "C" {
#endif

extern void _XmMenuArmItem(Widget w);

extern void _XmMenuDisarmItem(Widget w);

extern Boolean _XmIsActiveTearOff(Widget w);

extern Widget _XmGetRC_PopupPosted(Widget);

extern void _XmMenuEscape(Widget, XEvent *, String *, Cardinal *);
extern void _XmMenuTraverseLeft(Widget, XEvent *, String *, Cardinal *);
extern void _XmMenuTraverseRight(Widget, XEvent *, String *, Cardinal *);
extern void _XmMenuTraverseUp(Widget, XEvent *, String *, Cardinal *);
extern void _XmMenuTraverseDown(Widget, XEvent *, String *, Cardinal *);

extern void _XmRC_GadgetTraverseDown(Widget, XEvent *, String *, Cardinal *);

extern void _XmRC_GadgetTraverseUp(Widget, XEvent *, String *, Cardinal *);
extern void _XmRC_GadgetTraverseLeft(Widget, XEvent *, String *, Cardinal *);
extern void _XmRC_GadgetTraverseRight(Widget, XEvent *, String *, Cardinal *);

extern Boolean _XmGetInDragMode(Widget w);
extern void _XmSetInDragMode(Widget w, Boolean flag);

extern void _XmRecordEvent(XEvent *event);
extern Boolean _XmIsEventUnique(XEvent *event);

extern int _XmGrabPointer(Widget w,
			  int owner_events,
			  unsigned int event_mask,
			  int pointer_mode,
			  int keyboard_mode,
			  Window confine_to,
			  Cursor cursor,
			  Time time);

extern void _XmUngrabPointer(Widget w, Time t);

extern int _XmGrabKeyboard(Widget widget,
			   int owner_events,
			   int pointer_mode,
			   int keyboard_mode,
			   Time time);

extern void _XmUngrabKeyboard(Widget w, Time t);

extern XtPointer _XmGetMenuProcContext(void);
extern void _XmSaveMenuProcContext(XtPointer address);

extern void _XmMenuTraversalHandler(Widget w, Widget pw,
				    XmTraversalDirection direction);

extern void _XmMenuSetInPMMode(Widget wid, Boolean flag);

extern void _XmSetMenuTraversal(Widget wid, Boolean traversalOn);

extern void _XmLeafPaneFocusOut(Widget wid);

void _XmSaveCoreClassTranslations(Widget widget);
void _XmRestoreCoreClassTranslations(Widget widget);

XmMenuState _XmGetMenuState(Widget widget);

#ifdef __cplusplus
}
#endif

#endif /* MENUUTIL_P_H */
