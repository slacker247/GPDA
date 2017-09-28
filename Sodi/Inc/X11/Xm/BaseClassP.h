/**
 *
 * $Id: BaseClassP.h,v 1.1 1997/02/20 22:28:28 miers Exp $
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

#ifndef XM_BASECLASS_P_H
#define XM_BASECLASS_P_H

#include <Xm/XmP.h>

#ifdef __cplusplus
extern "C" {
#endif

#define _XmBCEPTR(wc) \
    ((XmBaseClassExt*)(&(((WidgetClass)(wc))->core_class.extension)))
#define _XmBCE(wc) \
    ((XmBaseClassExt)(((WidgetClass)(wc))->core_class.extension))

#define _XmGetBaseClassExtPtr(wc, owner) \
    ((_XmBCE(wc) && (((_XmBCE(wc))->record_type) == owner)) \
	? _XmBCEPTR(wc) \
	: ((XmBaseClassExt *)_XmGetClassExtensionPtr\
				((XmGenericClassExt*)_XmBCEPTR(wc),owner)))

#define _XmGetFlagsBit(field, bit) \
    (field[((bit) >> 3)]) & (1 << ((bit) & 0x07))

#define _XmSetFlagsBit(field, bit) \
    (field[((bit) >> 3)] |= (1 << ((bit) & 0x07)))

#define _XmFastSubclassInit(wc, bit_field) \
    { \
	if ((_Xm_fastPtr = _XmGetBaseClassExtPtr(wc, XmQmotif)) && \
	    (*_Xm_fastPtr)) \
	_XmSetFlagsBit((*_Xm_fastPtr)->flags, bit_field) ; \
    }

#define _XmIsFastSubclass(wc, bit) \
    (((_Xm_fastPtr = _XmGetBaseClassExtPtr((wc), XmQmotif)) && (*_Xm_fastPtr)) \
	? (_XmGetFlagsBit(((*_Xm_fastPtr)->flags), bit) \
	    ? TRUE \
	    : FALSE) \
	: FALSE)

#define XmBaseClassExtVersion 2L

typedef Cardinal (*XmGetSecResDataFunc)(WidgetClass,
					XmSecondaryResourceData **);

typedef struct _XmObjectClassExtRec 
{
    XtPointer next_extension;
    XrmQuark record_type;
    long version;
    Cardinal record_size;
} XmObjectClassExtRec, *XmObjectClassExt;

typedef struct _XmGenericClassExtRec {
    XtPointer next_extension;
    XrmQuark record_type;
    long version;
    Cardinal record_size;
} XmGenericClassExtRec, *XmGenericClassExt;

typedef struct _XmWrapperDataRec 
{
    struct _XmWrapperDataRec *next;
    WidgetClass	widgetClass;
    XtInitProc initializeLeaf;
    XtSetValuesFunc setValuesLeaf;
    XtArgsProc getValuesLeaf;
    XtRealizeProc realize;
    XtWidgetClassProc classPartInitLeaf;
    XtWidgetProc resize;
    XtGeometryHandler geometry_manager;
} XmWrapperDataRec, *XmWrapperData;

typedef struct _XmBaseClassExtRec 
{
    XtPointer next_extension;
    XrmQuark record_type;
    long version;
    Cardinal record_size;
    XtInitProc initializePrehook;
    XtSetValuesFunc setValuesPrehook;
    XtInitProc initializePosthook;
    XtSetValuesFunc setValuesPosthook;
    WidgetClass secondaryObjectClass;
    XtInitProc secondaryObjectCreate;
    XmGetSecResDataFunc getSecResData;
    unsigned char flags[32];
    XtArgsProc getValuesPrehook;
    XtArgsProc getValuesPosthook;
    XtWidgetClassProc classPartInitPrehook;
    XtWidgetClassProc classPartInitPosthook;
    XtResourceList ext_resources;
    XtResourceList compiled_ext_resources;
    Cardinal num_ext_resources;
    Boolean use_sub_resources;
    XmWidgetNavigableProc widgetNavigable;
    XmFocusChangeProc focusChange;
    XmWrapperData wrapperData;
} XmBaseClassExtRec, *XmBaseClassExt;

typedef struct _XmWidgetExtDataRec 
{
    Widget widget;
    Widget reqWidget;
    Widget oldWidget;
} XmWidgetExtDataRec, *XmWidgetExtData;

extern XrmQuark XmQmotif;
extern int _XmInheritClass;
extern XmBaseClassExt *_Xm_fastPtr;

extern Boolean _XmIsSlowSubclass(WidgetClass wc,
				 unsigned int bit);
extern XmGenericClassExt *_XmGetClassExtensionPtr(XmGenericClassExt *lstHeadPtr,
						  XrmQuark owner);
extern void _XmPushWidgetExtData(Widget widget,
				 XmWidgetExtData data,
				 unsigned char extType);
extern void _XmPopWidgetExtData(Widget widget,
				XmWidgetExtData *dataRtn,
				unsigned char extType);
extern XmWidgetExtData _XmGetWidgetExtData(Widget widget,
					   unsigned char extType);
extern void _XmFreeWidgetExtData(Widget widget);
extern void _XmBaseClassPartInitialize(WidgetClass wc);
extern void _XmInitializeExtensions(void);
extern Boolean _XmIsStandardMotifWidgetClass(WidgetClass wc);
extern Cardinal _XmSecondaryResourceData(XmBaseClassExt bcePtr,
					 XmSecondaryResourceData **secResDataRtn,
					 XtPointer client_data,
					 String name,
					 String class_name,
					 XmResourceBaseProc basefunctionpointer);
extern void _XmTransformSubResources(XtResourceList comp_resources,
				     Cardinal num_comp_resources,
				     XtResourceList *resources,
				     Cardinal *num_resources);

/* Available with Motif 2.0 and up... */
extern Boolean _XmIsSubclassOf(WidgetClass wc, WidgetClass sclass);

#ifdef __cplusplus
}
#endif

#endif /* XM_BASECLASS_P_H */
