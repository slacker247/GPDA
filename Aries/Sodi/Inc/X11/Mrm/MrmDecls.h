/**
 *
 * $Id: MrmDecls.h,v 1.2 1999/02/02 18:22:48 gritton Exp $
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
#ifndef MRM_DECLS_H
#define MRM_DECLS_H

extern char _MrmMsg_0000[];
extern char _MrmMsg_0001[];
extern char _MrmMsg_0002[];
extern char _MrmMsg_0003[];
extern char _MrmMsg_0004[];
extern char _MrmMsg_0005[];
extern char _MrmMsg_0006[];
extern char _MrmMsg_0007[];
extern char _MrmMsg_0008[];
extern char _MrmMsg_0009[];
extern char _MrmMsg_0010[];
extern char _MrmMsg_0011[];
extern char _MrmMsg_0012[];
extern char _MrmMsg_0013[];
extern char _MrmMsg_0014[];
extern char _MrmMsg_0015[];
extern char _MrmMsg_0016[];
extern char _MrmMsg_0017[];
extern char _MrmMsg_0018[];
extern char _MrmMsg_0019[];
extern char _MrmMsg_0020[];
extern char _MrmMsg_0021[];
extern char _MrmMsg_0022[];
extern char _MrmMsg_0023[];
extern char _MrmMsg_0024[];
extern char _MrmMsg_0025[];
extern char _MrmMsg_0026[];
extern char _MrmMsg_0027[];
extern char _MrmMsg_0028[];
extern char _MrmMsg_0029[];
extern char _MrmMsg_0030[];
extern char _MrmMsg_0031[];
extern char _MrmMsg_0032[];
extern char _MrmMsg_0033[];
extern char _MrmMsg_0034[];
extern char _MrmMsg_0035[];
extern char _MrmMsg_0036[];
extern char _MrmMsg_0037[];
extern char _MrmMsg_0038[];
extern char _MrmMsg_0039[];
extern char _MrmMsg_0040[];
extern char _MrmMsg_0041[];
extern char _MrmMsg_0042[];
extern char _MrmMsg_0043[];
extern char _MrmMsg_0044[];
extern char _MrmMsg_0045[];
extern char _MrmMsg_0046[];
extern char _MrmMsg_0047[];
extern char _MrmMsg_0048[];
extern char _MrmMsg_0049[];
extern char _MrmMsg_0050[];
extern char _MrmMsg_0051[];
extern char _MrmMsg_0052[];
extern char _MrmMsg_0053[];
extern char _MrmMsg_0054[];
extern char _MrmMsg_0055[];
extern char _MrmMsg_0056[];
extern char _MrmMsg_0057[];
extern char _MrmMsg_0058[];
extern char _MrmMsg_0059[];
extern char _MrmMsg_0060[];
extern char _MrmMsg_0061[];
extern char _MrmMsg_0062[];
extern char _MrmMsg_0063[];
extern char _MrmMsg_0064[];
extern char _MrmMsg_0065[];
extern char _MrmMsg_0066[];
extern char _MrmMsg_0067[];
extern char _MrmMsg_0068[];
extern char _MrmMsg_0069[];
extern char _MrmMsg_0070[];
extern char _MrmMsg_0071[];
extern char _MrmMsg_0072[];
extern char _MrmMsg_0073[];
extern char _MrmMsg_0074[];
extern char _MrmMsg_0075[];
extern char _MrmMsg_0076[];
extern char _MrmMsg_0077[];
extern char _MrmMsg_0078[];
extern char _MrmMsg_0079[];
extern char _MrmMsg_0080[];
extern char _MrmMsg_0081[];
extern char _MrmMsg_0082[];
extern char _MrmMsg_0083[];
extern char _MrmMsg_0084[];
extern char _MrmMsg_0085[];
extern char _MrmMsg_0086[];
extern char _MrmMsg_0087[];
extern char _MrmMsg_0088[];
extern char _MrmMsg_0089[];
extern char _MrmMsg_0090[];
extern char _MrmMsg_0091[];
extern char _MrmMsg_0092[];
extern char _MrmMsg_0093[];
extern char _MrmMsg_0094[];
extern char _MrmMsg_0095[];
extern char _MrmMsg_0096[];
extern char _MrmMsg_0097[];
extern char _MrmMsg_0098[];
extern char _MrmMsg_0099[];
extern char _MrmMsg_0100[];
extern char _MrmMsg_0101[];
extern char _MrmMsg_0102[];
extern char _MrmMsg_0103[];
extern char _MrmMsg_0104[];
extern char _MrmMsg_0105[];
extern char _MrmMsg_0106[];
extern char _MrmMsg_0107[];
extern char _MrmMsg_0108[];
extern char _MrmMsg_0109[];
extern char _MrmMsg_0110[];

#ifdef __cplusplus
extern "C" {
#endif

extern void MrmInitialize( void );

extern Cardinal MrmFetchLiteral(MrmHierarchy hierarchy_id,
				String index,
				Display *display,
				XtPointer *value_return,
				MrmCode *type_return);
extern Cardinal MrmFetchIconLiteral(MrmHierarchy hierarchy_id,
				    String index,
				    Screen *screen,
				    Display *display,
				    Pixel fgpix,
				    Pixel bgpix,
				    Pixmap *pixmap_return);
extern Cardinal MrmFetchBitmapLiteral(MrmHierarchy hierarchy_id,
				      String index,
				      Screen *screen,
				      Display *display,
				      Pixmap *pixmap_return,
				      Dimension *width,
				      Dimension *height);
extern Cardinal MrmFetchColorLiteral(MrmHierarchy hierarchy_id,
				     String index,
				     Display *display,
				     Colormap cmap,
				     Pixel *pixel_return);
extern Cardinal MrmOpenHierarchy(MrmCount num_files,
				 String *name_list,
				 MrmOsOpenParamPtr *os_ext_list,
				 MrmHierarchy *hierarchy_id_return);
extern Cardinal MrmOpenHierarchyPerDisplay(Display *display,
					   MrmCount num_files,
					   String *name_list,
					   MrmOsOpenParamPtr *os_ext_list,
					   MrmHierarchy *hierarchy_id_return);
extern Cardinal MrmRegisterNames(MrmRegisterArglist reglist, MrmCount num_reg);
extern Cardinal MrmRegisterNamesInHierarchy(MrmHierarchy hierarchy_id,
					    MrmRegisterArglist reglist,
					    MrmCount num_reg);
#ifdef __cplusplus
extern Cardinal MrmRegisterClass(MrmType class_code,
				 String class_name,
				 String create_name,
				 Widget (*creator)(...),
				 WidgetClass class_record);
#else
/* Is there a way to make the compiler happy?
 * This gives "function declaration isn't a prototype",
 * but adding (...) for __STDC__ like for __cplusplus above gives
 * "ANSI C requires a named argument before `...'".
 */
extern Cardinal MrmRegisterClass(MrmType class_code,
				 String class_name,
				 String create_name,
				 Widget (*creator)(),
				 WidgetClass class_record);
#endif
extern Cardinal MrmCloseHierarchy(MrmHierarchy hierarchy_id);
extern Cardinal MrmFetchInterfaceModule(MrmHierarchy hierarchy_id ,
					char *module_name,
					Widget parent,
					Widget *w_return);
extern Cardinal MrmFetchWidget(MrmHierarchy hierarchy_id,
			       String index,
			       Widget parent,
			       Widget *w_return,
			       MrmType *class_return);
extern Cardinal MrmFetchWidgetOverride(MrmHierarchy hierarchy_id,
				       String index,
				       Widget parent,
				       String ov_name,
				       ArgList ov_args,
				       Cardinal ov_num_args,
				       Widget *w_return,
				       MrmType *class_return);
extern Cardinal MrmFetchSetValues(MrmHierarchy hierarchy_id,
				  Widget w,
				  ArgList args,
				  Cardinal num_args);

#ifdef __cplusplus
}
#endif

#endif
