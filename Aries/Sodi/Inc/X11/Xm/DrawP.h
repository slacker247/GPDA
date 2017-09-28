/**
 *
 * $Id: DrawP.h,v 1.5 1998/11/13 07:38:22 gritton Exp $
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

#ifndef XM_DRAW_P_H
#define XM_DRAW_P_H

#include <Xm/XmP.h>

#ifdef __cplusplus
extern "C" {
#endif

extern void _XmDrawShadows(Display *display,
			   Drawable d,
			   GC top_gc,
			   GC bottom_gc,
			   Position x, Position y,
			   Dimension width, Dimension height,
			   Dimension shadow_thick,
			   unsigned int shadow_type);

extern void _XmDrawShadow(Display *display,
			  Drawable d,
			  GC top_gc,
			  GC bottom_gc,
			  Dimension shadow_thick,
			  Position x, Position y,
			  Dimension width, Dimension height);

extern void _XmClearBorder(Display *display,
			   Window w,
			   Position x, Position y,
			   Dimension width, Dimension height,
			   Dimension shadow_thick);

extern void _XmDrawSeparator(Display *display,
			    Drawable d,
			    GC top_gc,
			    GC bottom_gc,
			    GC separator_gc,
			    Position x, Position y,
			    Dimension width, Dimension height,
			    Dimension shadow_thick,
			    Dimension margin,
			    unsigned char orientation,
			    unsigned char separator_type);

extern void _XmDrawDiamond(Display *display,
			   Drawable d,
			   GC top_gc,
			   GC bottom_gc,
			   GC center_gc,
			   Position x, Position y,
			   Dimension width, Dimension height,
			   Dimension shadow_thick,
			   Dimension fill);

extern void _XmDrawSimpleHighlight(Display *display,
				   Drawable d,
				   GC gc,
				   Position x, Position y,
				   Dimension width, Dimension height,
				   Dimension highlight_thick);

extern void _XmDrawHighlight(Display *display,
			     Drawable d,
			     GC gc,
			     Position x, Position y,
			     Dimension width, Dimension height,
			     Dimension highlight_thick,
			     int line_style);

extern void _XmDrawArrow(Display *display,
			 Drawable d,
			 GC top_gc,
			 GC bot_gc,
			 GC cent_gc,
			 Position x, Position y,
			 Dimension width, Dimension height,
			 Dimension shadow_thick,
			 unsigned char direction);

extern void _XmHighlightBorder(Widget w);
extern void _XmUnhighlightBorder(Widget w);

#ifdef __cplusplus
}
#endif

#endif /* XM_DRAW_P_H */
