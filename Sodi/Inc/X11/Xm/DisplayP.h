/**
 *
 * $Id: DisplayP.h,v 1.1 1997/02/20 22:28:44 miers Exp $
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

#ifndef XM_DISPLAY_P_H
#define XM_DISPLAY_P_H

#include <Xm/Display.h>
#include <Xm/VirtKeysP.h>
#include <Xm/DragCP.h>
#include <Xm/VendorSEP.h>
#include <X11/ShellP.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _XmModalDataRec {
    Widget wid;
    XmVendorShellExtObject ve;
    XmVendorShellExtObject grabber;
    Boolean exclusive;
    Boolean springLoaded;
} XmModalDataRec, *XmModalData;

typedef Widget (*XmDisplayGetDisplayProc)(Display *);

typedef struct {
    unsigned char dragInitiatorProtocolStyle;
    unsigned char dragReceiverProtocolStyle;

    unsigned char userGrabbed;

    WidgetClass   dragContextClass;
    WidgetClass   dropTransferClass;
    WidgetClass   dropSiteManagerClass;
    XmDragContext activeDC;
    XmDropSiteManagerObject dsm;
    Time          lastDragTime;
    Window        proxyWindow;

    XmModalData   modals;
    Cardinal      numModals;
    Cardinal      maxModals;
    XtPointer     xmim_info;

    String        bindingsString;
    XmKeyBinding  bindings;
    XKeyEvent     *lastKeyEvent;
    unsigned char keycode_tag[XmKEYCODE_TAG_SIZE];
    
    int		  shellCount;
    XtPointer     displayInfo;
} XmDisplayPart;

typedef struct _XmDisplayInfo {
    Cursor          SashCursor;             /* Sash.c */
    Widget          destinationWidget;      /* Dest.c */
    Cursor          TearOffCursor;          /* TearOff.c */
    XtPointer       UniqueStamp;            /* UniqueEvent.c */
} XmDisplayInfo;

/* Define the full instance record */
typedef struct _XmDisplayRec {
    CorePart core;
    CompositePart 	composite;
    ShellPart 		shell;
    WMShellPart		wm;
    VendorShellPart	vendor;
    TopLevelShellPart 	topLevel;
    ApplicationShellPart application;
    XmDisplayPart	display;
} XmDisplayRec;

/* Define class part structure */
typedef struct {
    XmDisplayGetDisplayProc GetDisplay;
    XtPointer               extension;
} XmDisplayClassPart;

/* Define the full class record */
typedef struct _XmDisplayClassRec {
    CoreClassPart      		core_class;
    CompositeClassPart 		composite_class;
    ShellClassPart  		shell_class;
    WMShellClassPart   		wm_shell_class;
    VendorShellClassPart 	vendor_shell_class;
    TopLevelShellClassPart 	top_level_shell_class;
    ApplicationShellClassPart 	application_shell_class;
    XmDisplayClassPart		display_class;
} XmDisplayClassRec;

/* External definition for class record */

extern XmDisplayClassRec 	xmDisplayClassRec;

/*
 * Once again internal stuff -- you're better of not knowing.... --aldi
 */
extern XmDropSiteManagerObject _XmGetDropSiteManagerObject(XmDisplay xmDisplay);
extern unsigned char _XmGetDragProtocolStyle(Widget w);
extern unsigned char _XmGetDragTrackingMode(Widget w);
extern Widget _XmGetDragContextFromHandle(Widget w, Atom iccHandle);
extern WidgetClass _XmGetXmDisplayClass(void);
extern WidgetClass _XmSetXmDisplayClass(WidgetClass wc);

extern String _Xm_MOTIF_DRAG_AND_DROP_MESSAGE;

#ifdef __cplusplus
}
#endif

#endif /* XM_DISPLAY_P_H */
