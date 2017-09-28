/**
 *
 * $Id: TextInP.h,v 1.1 1997/02/20 22:29:55 miers Exp $
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

#ifndef _XM_TEXT_IN_P
#define _XM_TEXT_IN_P

#include <Xm/Text.h>
#include <Xm/TextStrSoP.h>

#ifdef __cplusplus
extern "C" {
#endif

extern XtPointer _XmdefaultTextActionsTable;
extern Cardinal _XmdefaultTextActionsTableSize;

typedef struct {
    int x;
    int y;
} SelectionHint;

typedef struct _InputDataRec {
    XmTextWidget widget;
    XmTextScanType *sarray;
    int sarraycount;
    int new_sel_length;
    int threshold;
    SelectionHint selectionHint;
    SelectionHint Sel2Hint;
    XtIntervalId select_id;
    XmTextScanType stype;
    XmTextScanDirection extendDir;
    XmTextScanDirection Sel2ExtendDir;
    XmTextPosition origLeft, origRight;
    XmTextPosition Sel2OrigLeft, Sel2OrigRight;
    XmTextPosition stuffpos;
    XmTextPosition sel2Left, sel2Right;
    XmTextPosition anchor;
    Position select_pos_x;
    Position select_pos_y;
    Boolean pendingdelete;
    Boolean syncing;
    Boolean extending;
    Boolean Sel2Extending;
    Boolean hasSel2;
    Boolean has_destination;
    Boolean selectionMove;
    Boolean cancel;
    Boolean overstrike;
    Boolean sel_start;
    Time dest_time;
    Time sec_time;
    Time lasttime;
} InputDataRec, *InputData;


typedef void (*InputGetSecResProc)(XmSecondaryResourceData *);
typedef void (*InputCreateProc)(Widget, ArgList, Cardinal);
typedef void (*InputGetValuesProc)(Widget, ArgList, Cardinal);
typedef void (*InputSetValuesProc)(Widget, Widget, Widget, ArgList, Cardinal *);
typedef void (*InputInvalidateProc)(XmTextWidget,
				    XmTextPosition,
				    XmTextPosition,
				    long);

typedef struct _InputRec {
    struct _InputDataRec *data;
    InputInvalidateProc Invalidate;
    InputGetValuesProc  GetValues;
    InputSetValuesProc	SetValues;
    XtWidgetProc	destroy;
    InputGetSecResProc  GetSecResData;
} InputRec;

extern XtPointer _XmdefaultTextActionsTable;
extern Cardinal  _XmdefaultTextActionsTableSize;

extern Widget _XmTextGetDropReciever(Widget w);
extern Boolean _XmTextHasDestination(Widget w);
extern void _XmTextInputGetSecResData(XmSecondaryResourceData *secResDataRtn);
extern XmTextPosition _XmTextGetAnchor(XmTextWidget tw);
extern void _XmTextInputCreate(Widget wid, ArgList args, Cardinal num_args);
extern Boolean _XmTextSetDestinationSelection(Widget w,
					      XmTextPosition position,
					      Boolean disown,
					      Time set_time);
extern Boolean _XmTextSetSel2(XmTextWidget tw,
			      XmTextPosition left,
			      XmTextPosition right,
			      Time set_time);
extern Boolean _XmTextGetSel2(XmTextWidget tw,
			      XmTextPosition *left,
			      XmTextPosition *right);


#define In_HighlightStart(i) (i->origLeft)
#define In_HighlightEnd(i) (i->origRight)
#define In_HighlightPivot(i) (i->anchor)
#define In_SecondaryStart(i) (i->Sel2OrigLeft)
#define In_SecondaryEnd(i) (i->Sel2OrigRight)
#define In_SecondaryPivot(i) (i->stuffpos)
#define In_PendingDelete(i) (i->pendingdelete)
#define In_SelectId(i) (i->select_id)
#define In_SelArray(i) (i->sarray)
#define In_SelArrayCount(i) (i->sarraycount)
#define In_SelArrayIndex(i) (i->new_sel_length)
#define In_SelectThreshold(i) (i->select_threshold)
#define In_TimerId(i) (i->timer_id)
#define In_LastTime(i) (i->lasttime)

#ifdef __cplusplus
}
#endif

#endif
