/**
 *
 * $Id: SeparatorP.h,v 1.1 1997/02/20 22:29:48 miers Exp $
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


#ifndef XM_SEPARATOR_P_H
#define XM_SEPARATOR_P_H

#include <Xm/Separator.h>
#include <Xm/PrimitiveP.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    Dimension margin;
    unsigned char orientation;
    unsigned char separator_type;
    GC separator_GC;
} XmSeparatorPart;

/* Define the full instance record */
typedef struct _XmSeparatorRec {
    CorePart core;
    XmPrimitivePart primitive;
    XmSeparatorPart separator;
} XmSeparatorRec;

/* Define class part structure */
typedef struct {
    XtPointer extension;
} XmSeparatorClassPart;

/* Define the full class record */
typedef struct _XmSeparatorClassRec {
    CoreClassPart core_class;
    XmPrimitiveClassPart primitive_class;
    XmSeparatorClassPart separator_class;
} XmSeparatorClassRec;

/* External definition for the class record */

extern XmSeparatorClassRec xmSeparatorClassRec;

#ifdef __cplusplus
}
#endif

#endif /* XM_SEPARATOR_P_H */
