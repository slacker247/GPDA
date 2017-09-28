/**
 *
 * $Id: TextF.h,v 1.1 1997/02/20 22:29:53 miers Exp $
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

#ifndef XM_TEXTF_H
#define XM_TEXTF_H

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

extern WidgetClass xmTextFieldWidgetClass;

typedef struct _XmTextFieldRec *XmTextFieldWidget;
typedef struct _XmTextFieldClassRec *XmTextFieldWidgetClass;

extern Widget XmCreateTextField(Widget parent,
				char *name,
				Arg *arglist,
				Cardinal argCount);

#ifndef XmIsTextField
#define XmIsTextField(w) XtIsSubclass((w), xmTextFieldWidgetClass)
#endif

void XmTextFieldClearSelection(Widget widget, Time time);
Boolean XmTextFieldCopy(Widget widget, Time time);
Boolean XmTextFieldCut(Widget widget, Time time);
int XmTextFieldGetBaseline(Widget widget);
XmTextPosition XmTextFieldGetCursorPosition(Widget widget);
Boolean XmTextFieldGetEditable(Widget widget);
XmTextPosition XmTextFieldGetInsertionPosition(Widget widget);
XmTextPosition XmTextFieldGetLastPosition(Widget widget);
int XmTextFieldGetMaxLength(Widget widget);
char *XmTextFieldGetSelection(Widget widget);
Boolean XmTextFieldGetSelectionPosition(Widget widget, XmTextPosition *left, XmTextPosition *right);
wchar_t *XmTextFieldGetSelectionWcs(Widget widget);
char *XmTextFieldGetString(Widget widget);
wchar_t *XmTextFieldGetStringWcs(Widget widget);
int XmTextFieldGetSubstring(Widget widget, XmTextPosition start, int num_chars, int buffer_size, char *buffer);
int XmTextFieldGetSubstringWcs(Widget widget, XmTextPosition start, int num_chars, int buffer_size, wchar_t *buffer);
void XmTextFieldInsert(Widget widget, XmTextPosition position, char *string);
void XmTextFieldInsertWcs(Widget widget, XmTextPosition position, wchar_t *wcstring);
Boolean XmTextFieldPaste(Widget widget);
Boolean XmTextFieldPosToXY(Widget widget, XmTextPosition position, Position *x, Position *y);
Boolean XmTextFieldRemove(Widget widget);
void XmTextFieldReplace(Widget widget, XmTextPosition from_pos, XmTextPosition to_pos, char *value);
void XmTextFieldReplaceWcs(Widget widget, XmTextPosition from_pos, XmTextPosition to_pos, wchar_t *wcstring);
void XmTextFieldSetAddMode(Widget widget, Boolean state);
void XmTextFieldSetCursorPosition(Widget widget, XmTextPosition position);
void XmTextFieldSetEditable(Widget widget, Boolean editable);
void XmTextFieldSetHighlight(Widget widget, XmTextPosition left, XmTextPosition right, XmHighlightMode mode);
void XmTextFieldSetInsertionPosition(Widget widget, XmTextPosition position);
void XmTextFieldSetMaxLength(Widget widget, int max_length);
void XmTextFieldSetSelection(Widget widget, XmTextPosition first, XmTextPosition last, Time time);
void XmTextFieldSetString(Widget widget, char *value);
void XmTextFieldSetStringWcs(Widget widget, wchar_t *wcstring);
void XmTextFieldShowPosition(Widget widget, XmTextPosition position);
XmTextPosition XmTextFieldXYToPos(Widget widget, Position x, Position y);

#ifdef __cplusplus
}
#endif

#endif /* XM_TEXT_H */
