/**
 *
 * $Id: FilePickerP.h,v 1.1 1998/11/14 21:08:58 rwscott Exp $
 *
 * Copyright (C) 1996 Free Software Foundation, Inc.
 *
 * This file is part of the GNU LessTif Extension Library.
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
#ifndef _FILEPICKERP_H
#define _FILEPICKERP_H

#include <FilePicker.h>
#include <Xm/FormP.h>

typedef struct {
	Widget TextField;
	Widget Label;
	Widget Button;
	Widget FileSelectionDialog;
	String Value;
} XltFilePickerPart;

typedef struct _XltFilePickerRec {
	CorePart core;
	CompositePart composite;
	ConstraintPart constraint;
	XmManagerPart manager;
	XmBulletinBoardPart bulletin_board;
	XmFormPart form;
	XltFilePickerPart file_picker;
} XltFilePickerRec;

typedef struct {
	Widget file_selection_dialog;
	XtPointer extension;
} XltFilePickerClassPart;

typedef struct _XltFilePickerClassRec {
	CoreClassPart core_class;
	CompositeClassPart composite_class;
	ConstraintClassPart constraint_class;
	XmManagerClassPart manager_class;
	XmBulletinBoardClassPart bulletin_board_class;
	XmFormClassPart form_class;
	XltFilePickerClassPart file_picker_class;
} XltFilePickerClassRec;

extern XltFilePickerClassRec xltFilePickerClassRec;

#ifndef XltInheritFileSelectionDialog
#define XltInheritFileSelectionDialog ((Widget) _XtInherit)
#endif

#define FilePicker_Value(m) (((XltFilePickerWidget)(m))->file_picker.Value)
#define FilePicker_Button(m) (((XltFilePickerWidget)(m))->file_picker.Button)
#define FilePicker_Label(m) (((XltFilePickerWidget)(m))->file_picker.Label)
#define FilePicker_TextField(m) (((XltFilePickerWidget)(m))->file_picker.TextField)
#define FilePicker_FileSelectionDialog(m) (((XltFilePickerWidget)(m))->file_picker.FileSelectionDialog)
#define FilePickerClass_FileSelectionDialog(m) (((XltFilePickerWidgetClass)XtClass(m))->file_picker_class.file_selection_dialog)

#endif
