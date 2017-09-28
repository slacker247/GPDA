/**
 *
 * $Id: ToggleBGP.h,v 1.4 1999/09/10 04:28:14 gritton Exp $
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
#ifndef XM_TOGGLEBG_P_H
#define XM_TOGGLEBG_P_H

#include <Xm/ToggleBG.h>
#include <Xm/LabelGP.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * gadget cache class and instance structures
 */
typedef struct _XmToggleButtonGCacheObjClassPart {
    int foo;
} XmToggleButtonGCacheObjClassPart;

typedef struct _XmToggleButtonGCacheObjClassRec {
    ObjectClassPart object_class;
    XmExtClassPart ext_class;
    XmLabelGCacheObjClassPart label_class_cache;
    XmToggleButtonGCacheObjClassPart toggle_class_cache;
} XmToggleButtonGCacheObjClassRec;

extern XmToggleButtonGCacheObjClassRec xmToggleButtonGCacheObjClassRec;

typedef struct _XmToggleButtonGCacheObjPart {
    unsigned char ind_type;
    Boolean visible;
    Dimension spacing;
    Dimension indicator_dim;
    Pixmap on_pixmap;
    Pixmap insen_pixmap;
    Boolean ind_on;
    Boolean fill_on_select;
    Pixel select_color;
    GC select_GC;
    GC background_GC;
} XmToggleButtonGCacheObjPart;

typedef struct _XmToggleButtonGCacheObjRec {
    ObjectPart object;
    XmExtPart ext;
    XmLabelGCacheObjPart label_cache;
    XmToggleButtonGCacheObjPart toggle_cache;
} XmToggleButtonGCacheObjRec;


/*
 * gadget class and instance records
 */
typedef struct {
    XtPointer extension;
} XmToggleButtonGadgetClassPart;

typedef struct _XmToggleButtonGadgetClassRec {
    RectObjClassPart rect_class;
    XmGadgetClassPart gadget_class;
    XmLabelGadgetClassPart label_class;
    XmToggleButtonGadgetClassPart toggle_class;
} XmToggleButtonGadgetClassRec;

extern XmToggleButtonGadgetClassRec xmToggleButtonGadgetClassRec;

typedef struct {
    Boolean indicator_set;
    Boolean set;
    Boolean visual_set;

    Boolean Armed;

    XtCallbackList value_changed_CB;
    XtCallbackList arm_CB;
    XtCallbackList disarm_CB;

    XmToggleButtonGCacheObjPart *cache;
} XmToggleButtonGadgetPart;

/* Define the full instance record */
typedef struct _XmToggleButtonGadgetRec {
    ObjectPart object;
    RectObjPart rectangle;
    XmGadgetPart gadget;
    XmLabelGadgetPart label;
    XmToggleButtonGadgetPart toggle;
} XmToggleButtonGadgetRec;

/*
 * access macros
 */
#define TBG_IndType(w) \
    (((XmToggleButtonGadget)(w))->toggle.cache->ind_type)

#define TBG_Visible(w) \
    (((XmToggleButtonGadget)(w))->toggle.cache->visible)

#define TBG_Spacing(w) \
    (((XmToggleButtonGadget)(w))->toggle.cache->spacing)

#define TBG_IndicatorDim(w) \
    (((XmToggleButtonGadget)(w))->toggle.cache->indicator_dim)

#define TBG_OnPixmap(w) \
    (((XmToggleButtonGadget)(w))->toggle.cache->on_pixmap)

#define TBG_InsenPixmap(w) \
    (((XmToggleButtonGadget)(w))->toggle.cache->insen_pixmap)

#define TBG_IndOn(w) \
    (((XmToggleButtonGadget)(w))->toggle.cache->ind_on)

#define TBG_FillOnSelect(w) \
    (((XmToggleButtonGadget)(w))->toggle.cache->fill_on_select)

#define TBG_SelectColor(w) \
    (((XmToggleButtonGadget)(w))->toggle.cache->select_color)

#define TBG_SelectGC(w) \
    (((XmToggleButtonGadget)(w))->toggle.cache->select_GC)

#define TBG_BackgroundGC(w) \
    (((XmToggleButtonGadget)(w))->toggle.cache->background_GC)

#define TBG_Cache(w) \
    (((XmToggleButtonGadget)(w))->toggle.cache)

#define TBG_ClassCachePart(w) \
    (((XmToggleButtonGadgetClass)xmToggleButtonGadgetClass)->gadget_class.cache_part)

#define TBG_IndicatorSet(w) \
    (((XmToggleButtonGadget)(w))->toggle.indicator_set)

#define TBG_Set(w) \
    (((XmToggleButtonGadget)(w))->toggle.set)

#define TBG_VisualSet(w) \
    (((XmToggleButtonGadget)(w))->toggle.visual_set)

#define TBG_ArmCB(w) \
    (((XmToggleButtonGadget)(w))->toggle.arm_CB)

#define TBG_ValueChangedCB(w) \
    (((XmToggleButtonGadget)(w))->toggle.value_changed_CB)

#define TBG_DisarmCB(w) \
    (((XmToggleButtonGadget)(w))->toggle.disarm_CB)

#define TBG_Armed(w) \
    (((XmToggleButtonGadget)(w))->toggle.Armed)

extern int _XmToggleBCacheCompare(XtPointer A, XtPointer B);

#ifdef __cplusplus
}
#endif

#endif /* XM_TOGGLEBG_P_H */
