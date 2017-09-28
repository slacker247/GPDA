/**
 *
 * $Id: List.h,v 1.1 1997/02/20 22:29:12 miers Exp $
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

#ifndef LIST_H_INCLUDED
#define LIST_H_INCLUDED

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

extern WidgetClass xmListWidgetClass;

typedef struct _XmListRec *XmListWidget;
typedef struct _XmListClassRec *XmListWidgetClass;

#ifndef XmIsList
#define XmIsList(w) XtIsSubclass((w), xmListWidgetClass)
#endif

/* selection type values */ 

enum {
    XmINITIAL,
    XmMODIFICATION,
    XmADDITION 
};

extern Widget XmCreateList(Widget parent,
			   char *name,
			   Arg *arglist,
			   Cardinal argCount);
extern Widget XmCreateScrolledList(Widget parent,
				   char *name,
				   Arg *arglist,
				   Cardinal argCount);
extern void XmListAddItem(Widget widget,
			  XmString item,
			  int position);
extern void XmListAddItems(Widget widget,
			   XmString *items,
			   int item_count,
			   int position);
extern void XmListAddItemUnselected(Widget widget,
				    XmString item,
				    int position);
extern void XmListAddItemsUnselected(Widget widget,
				     XmString *items,
				     int item_count,
				     int position);
extern void XmListDeleteAllItems(Widget widget);
extern void XmListDeleteItem(Widget widget,
			     XmString item);
extern void XmListDeleteItems(Widget widget,
			      XmString *items,
			      int item_count);
extern void XmListDeleteItemsPos(Widget widget,
				 int item_count,
				 int position);
extern void XmListDeletePos(Widget widget,
			    int position);
extern void XmListDeletePositions(Widget widget,
				  int *position_list,
				  int position_count);
extern void XmListDeselectAllItems(Widget widget);
extern void XmListDeselectItem(Widget widget,
			       XmString item);
extern void XmListDeselectPos(Widget widget,
			      int position);
extern int XmListGetKbdItemPos(Widget widget);
extern Boolean XmListGetMatchPos(Widget widget,
				 XmString item,
				 int **position_list,
				 int *position_count);
extern Boolean XmListGetSelectedPos(Widget widget,
				    int **position_list,
				    int *position_count);
extern Boolean XmListItemExists(Widget widget,
				XmString item);
extern int XmListItemPos(Widget widget,
			 XmString item);
extern Boolean XmListPosSelected(Widget widget,
				 int position);
extern Boolean XmListPosToBounds(Widget widget,
				 int position,
				 Position *x,
				 Position *y,
				 Dimension *width,
				 Dimension *height);
extern void XmListReplaceItems(Widget widget,
			       XmString *old_items,
			       int item_count,
			       XmString *new_items);
extern void XmListReplaceItemsPos(Widget widget,
				  XmString *new_items,
				  int item_count,
				  int position);
extern void XmListReplaceItemsPosUnselected(Widget widget,
					    XmString *new_items,
					    int item_count,
					    int position);
extern void XmListReplaceItemsUnselected(Widget widget,
					 XmString *old_items,
					 int item_count,
					 XmString *new_items);
extern void XmListReplacePositions(Widget widget,
				   int *position_list,
				   XmString *item_list,
				   int item_count);
extern void XmListSelectItem(Widget widget,
			     XmString item,
			     Boolean notify);
extern void XmListSelectPos(Widget widget,
			    int position,
			    Boolean notify);
extern void XmListSetAddMode(Widget widget,
			     Boolean mode);
extern void XmListSetBottomItem(Widget widget,
				XmString item);
extern void XmListSetBottomPos(Widget widget,
			       int position);
extern void XmListSetHorizPos(Widget widget,
			      int position);
extern void XmListSetItem(Widget widget,
			  XmString item);
extern Boolean XmListSetKbdItemPos(Widget widget,
				   int position);
extern void XmListSetPos(Widget widget,
			 int position);
extern void XmListUpdateSelectedList(Widget widget);
extern int XmListYToPos(Widget widget,
			Position y);


#ifdef __cplusplus
}
#endif

#endif /* LIST_H_INCLUDED */
