/**
 *
 * $Id: VaSimpleP.h,v 1.1 1997/02/20 22:30:05 miers Exp $
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

#ifndef XM_VA_SIMPLE_P_H
#define XM_VA_SIMPLE_P_H

#include <Xm/XmP.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifdef STDC_HEADERS
#include <stdarg.h>
#define Va_start(a,b) va_start(a,b)
#else /* ! STDC_HEADERS */
#include <varargs.h>
#define Va_start(a,b) va_start(a)
#endif

#if XtSpecificationRelease < 6

typedef struct {
    String      name;   /* resource name */
    String      type;   /* representation type name */
    XtArgVal    value;  /* representation */
    int         size;   /* size of representation */
} XtTypedArg, *XtTypedArgList;

#endif

extern void _XmCountVaList(va_list var,
			   int *button_count,
			   int *args_count,
			   int *typed_count,
			   int *total_count);
extern void _XmVaToTypedArgList(va_list var,
				int max_count,
				XtTypedArgList *args_return,
				Cardinal *num_args_return);

#ifdef __cplusplus
}
#endif


#endif /* XM_VA_SIMPLE_P_H */
