/**
 *
 * $Id: Text.h,v 1.1 1997/02/20 22:29:52 miers Exp $
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

#ifndef XM_TEXT_H
#define XM_TEXT_H

#include <Xm/Xm.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

extern WidgetClass xmTextWidgetClass;

typedef struct _XmTextRec *XmTextWidget;
typedef struct _XmTextClassRec *XmTextWidgetClass;

extern Widget XmCreateText(Widget parent,
			    char *name,
			    Arg *arglist,
			    Cardinal argCount);

extern Widget XmCreateScrolledText(Widget parent,
				   char *name,
				   Arg *arglist,
				   Cardinal argcount);

#ifndef XmIsText
#define XmIsText(w) XtIsSubclass((w), xmTextWidgetClass)
#endif

typedef struct _XmTextSourceRec *XmTextSource;

void XmTextClearSelection(Widget widget, Time time);
Boolean XmTextCopy(Widget widget, Time time);
Boolean XmTextCut(Widget widget, Time time);
void XmTextDisableRedisplay(Widget widget);
void XmTextEnableRedisplay(Widget widget);
Boolean XmTextFindString(Widget widget, XmTextPosition start, char *string, 
			 XmTextDirection direction, XmTextPosition *position);
Boolean XmTextFindStringWcs(Widget widget, XmTextPosition start, wchar_t *wcstring, 
			    XmTextDirection direction, XmTextPosition *position);
int XmTextGetBaseline(Widget widget);
XmTextPosition XmTextGetCursorPosition(Widget widget);
Boolean XmTextGetEditable(Widget widget);
XmTextPosition XmTextGetInsertionPosition(Widget widget);
XmTextPosition XmTextGetLastPosition(Widget widget);
int XmTextGetMaxLength(Widget widget);
char *XmTextGetSelection(Widget widget);
Boolean XmTextGetSelectionPosition(Widget widget, XmTextPosition *left, XmTextPosition *right);
wchar_t *XmTextGetSelectionWcs(Widget widget);
XmTextSource XmTextGetSource(Widget widget);
char *XmTextGetString(Widget widget);
wchar_t *XmTextGetStringWcs(Widget widget);
int XmTextGetSubstring(Widget widget, XmTextPosition start, int num_chars, int buffer_size,
		       char *buffer);
int XmTextGetSubstringWcs(Widget widget, XmTextPosition start, int num_chars, int buffer_size,
			  wchar_t *buffer);
XmTextPosition XmTextGetTopCharacter(Widget widget);
void XmTextInsert(Widget widget, XmTextPosition position, char *string);
void XmTextInsertWcs(Widget widget, XmTextPosition position, wchar_t *wcstring);
Boolean XmTextPaste(Widget widget);
Boolean XmTextPosToXY(Widget widget, XmTextPosition position, Position *x, Position *y);
Boolean XmTextRemove(Widget widget);
void XmTextReplace(Widget widget, XmTextPosition from_pos, XmTextPosition to_pos,
		   char *value);
void XmTextReplaceWcs(Widget widget, XmTextPosition from_pos, XmTextPosition to_pos,
		      wchar_t *wcstring);
void XmTextScroll(Widget widget, int lines);
void XmTextSetAddMode(Widget widget, Boolean state);
void XmTextSetCursorPosition(Widget widget, XmTextPosition position);
void XmTextSetEditable(Widget widget, Boolean editable);
void XmTextSetHighlight(Widget widget, XmTextPosition left, XmTextPosition right, XmHighlightMode mode);
void XmTextSetInsertionPosition(Widget widget, XmTextPosition position);
void XmTextSetMaxLength(Widget widget, int max_length);
void XmTextSetSelection(Widget widget, XmTextPosition first, XmTextPosition last, Time time);
void XmTextSetSource(Widget widget, XmTextSource source, XmTextPosition top_character, XmTextPosition cursor_position);
void XmTextSetString(Widget widget, char *value);
void XmTextSetStringWcs(Widget widget, wchar_t *wcstring);
void XmTextSetTopCharacter(Widget widget, XmTextPosition top_character);
void XmTextShowPosition(Widget widget, XmTextPosition position);
XmTextPosition XmTextXYToPos(Widget widget, Position x, Position y);

#ifdef __cplusplus
}
#endif

#endif /* XM_TEXT_H */
