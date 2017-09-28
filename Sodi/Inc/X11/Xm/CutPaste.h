/**
 *
 * $Id: CutPaste.h,v 1.2 1998/10/14 05:42:16 gritton Exp $
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

#ifndef XM_CUTPASTE_H
#define XM_CUTPASTE_H

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

enum {
    XmClipboardFail,
    XmClipboardSuccess,
    XmClipboardTruncate,
#if LESSTIF_BACK_COMPAT
    XmClipboardLocked,
#else
    XmClipboardLocked = 4,
#endif
    XmClipboardBadFormat,
    XmClipboardNoData
};

/* These are for compatibility with pre-1.2 values */
enum {
    ClipboardFail,
    ClipboardSuccess,
    ClipboardTruncate,
#if LESSTIF_BACK_COMPAT
    ClipboardLocked,
#else
    ClipboardLocked = 4,
#endif
    ClipboardBadFormat,
    ClipboardNoData
};

typedef struct {
    long DataId;
    long PrivateId;
} XmClipboardPendingRec, *XmClipboardPendingList;

typedef void (*XmCutPasteProc)(Widget, int *, int *, int *);
typedef void (*VoidProc)(Widget w, int *data_id, int *private_id, int *reason);


int XmClipboardBeginCopy(Display *display, Window window,
			 XmString clip_label, Widget widget,
			 VoidProc callback, long *item_id);
int XmClipboardCancelCopy(Display *display, Window window, long item_id);
int XmClipboardCopy(Display *display, Window window, long item_id, char *format_name,
		    XtPointer buffer, unsigned long length, long private_id, long *data_id);
int XmClipboardCopyByName(Display *display, Window window, long data_id,
			  XtPointer buffer, unsigned long length, long private_id);
int XmClipboardEndCopy(Display *display, Window window, long item_id);
int XmClipboardEndRetrieve(Display *display, Window window);
int XmClipboardInquireCount(Display *display, Window window, int *count, unsigned long *max_length);
int XmClipboardInquireFormat(Display *display, Window window, int index, XtPointer format_name_buf,
			     unsigned long buffer_len, unsigned long *copied_len);
int XmClipboardInquireLength(Display *display, Window window, char *format_name, unsigned long *length);
int XmClipboardInquirePendingItems(Display *display, Window window, char *format_name, 
				   XmClipboardPendingList *item_list, unsigned long *count);
int XmClipboardLock(Display *display, Window window);
int XmClipboardRegisterFormat(Display *display, char *format_name, int format_length);
int XmClipboardRetrieve(Display *display, Window window, char *format_name, XtPointer buffer, 
			unsigned long length, unsigned long *num_bytes, long *private_id);
int XmClipboardStartCopy(Display *display, Window window, XmString clip_label, Time timestamp,
			 Widget widget, XmCutPasteProc callback, long *item_id);
int XmClipboardStartRetrieve(Display *display, Window window, Time timestamp);
int XmClipboardUndoCopy(Display *display, Window window);
int XmClipboardUnlock(Display *display, Window window, Boolean remove_all_locks);
int XmClipboardWidthdrawFormat(Display *display, Window window, int data_id);

#ifdef __cplusplus
}
#endif

#endif /* XM_CUTPASTE_H */
