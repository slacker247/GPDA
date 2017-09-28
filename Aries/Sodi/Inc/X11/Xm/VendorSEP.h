/**
 *
 * $Id: VendorSEP.h,v 1.3 1998/09/24 20:06:06 pgw Exp $
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

#ifndef XM_VENDOR_E_P_H
#define XM_VENDOR_E_P_H

#include <Xm/XmP.h>
#include <Xm/VendorS.h>
#include <Xm/MwmUtil.h>
#include <Xm/ExtObjectP.h>
#include <Xm/DesktopP.h>
#include <Xm/ShellEP.h>

#ifdef __cplusplus
extern "C" {
#endif


#ifndef XmIsVendorShellExt
#define XmIsVendorShellExt(w) XtIsSubclass(w, xmVendorShellExtObjectClass)
#endif

typedef struct _XmVendorShellExtRec *XmVendorShellExtObject;
typedef struct _XmVendorShellExtClassRec *XmVendorShellExtObjectClass;
extern WidgetClass xmVendorShellExtObjectClass;

#define XmInheritProtocolHandler        ((XtCallbackProc)_XtInherit)

typedef struct _XmVendorShellExtClassPart{
    XtCallbackProc delete_window_handler;
    XtCallbackProc offset_handler;
    XtPointer extension;
} XmVendorShellExtClassPart, *XmVendorShellExtClassPartPtr;

typedef struct _XmVendorShellExtClassRec{
    ObjectClassPart object_class;
    XmExtClassPart ext_class;
    XmDesktopClassPart desktop_class;
    XmShellExtClassPart shell_class;
    XmVendorShellExtClassPart vendor_class;
} XmVendorShellExtClassRec;

typedef struct {
	Widget		w;
	String		s;
} _XmAcceleratorStruct;

typedef struct {
    XmFontList default_font_list;
    unsigned char focus_policy;
    XmFocusData focus_data;
    unsigned char delete_response;
    unsigned char unit_type;
    MwmHints mwm_hints;
    MwmInfo mwm_info;
    String mwm_menu;
    XtCallbackList focus_moved_callback;

    Widget old_managed;
    Position xAtMap, yAtMap, xOffset, yOffset;
    unsigned long lastOffsetSerial;
    unsigned long lastMapRequest;
#if LESSTIF_BACK_COMPAT
    Boolean externalRepoosition;
#else
    Boolean externalReposition;
#endif
    unsigned char mapStyle;
    XtCallbackList realize_callback;
    XtGrabKind grab_kind;
    unsigned char audible_warning;
    XmFontList button_font_list;
    XmFontList label_font_list;
    XmFontList text_font_list;
    String input_method_string;
    String preedit_type_string;
    unsigned int light_threshold;
    unsigned int dark_threshold;
    unsigned int foreground_threshold;
    unsigned int im_height;
    XtPointer im_info;
    Boolean im_vs_height_set;

} XmVendorShellExtPart, *XmVendorShellExtPartPtr;

extern XmVendorShellExtClassRec  xmVendorShellExtClassRec;

typedef struct _XmVendorShellExtRec {
    ObjectPart object;
    XmExtPart ext;
    XmDesktopPart desktop;
    XmShellExtPart shell;
    XmVendorShellExtPart vendor;
} XmVendorShellExtRec;

/*
 * protos
 */
unsigned char _XmGetAudibleWarning(Widget w);
char *_XmGetIconPixmapName(void);
void _XmClearIconPixmapName(void); 

#ifdef __cplusplus
}
#endif

#endif /* XM_VENDOR_E_P_H */
