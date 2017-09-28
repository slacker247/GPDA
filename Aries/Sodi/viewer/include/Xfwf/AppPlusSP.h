/* $Id: AppPlusSP.h,v 1.4 1995/02/02 00:57:10 cwikla Exp $ */
/*
 * Copyright 1994,1995 John L. Cwikla
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appears in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of John L. Cwikla or
 * Wolfram Research, Inc not be used in advertising or publicity
 * pertaining to distribution of the software without specific, written
 * prior permission.  John L. Cwikla and Wolfram Research, Inc make no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 * John L. Cwikla and Wolfram Research, Inc disclaim all warranties with
 * regard to this software, including all implied warranties of
 * merchantability and fitness, in no event shall John L. Cwikla or
 * Wolfram Research, Inc be liable for any special, indirect or
 * consequential damages or any damages whatsoever resulting from loss of
 * use, data or profits, whether in an action of contract, negligence or
 * other tortious action, arising out of or in connection with the use or
 * performance of this software.
 *
 * Author:
 *  John L. Cwikla
 *  X Programmer
 *  Wolfram Research Inc.
 *
 *  cwikla@wri.com
*/


#ifndef _AppPlusShellP_h
#define _AppPlusShellP_h

#include <X11/ShellP.h>

#include "AppPlusS.h"

typedef struct _AppPlusShellClassPart
{
	Colormap colormap;
	Visual *visual;
	int visualClass;
	int applicationDepth;
#if HAS_XCC
	XCC xcc;
#endif /* HAS_XCC */
} AppPlusShellClassPart;

typedef struct _AppPlusShellClassRec
{
	CoreClassPart core_class;
	CompositeClassPart composite_class;
	ShellClassPart shell_class;
	WMShellClassPart wm_shell_class;
	VendorShellClassPart vendor_shell_class;
	TopLevelShellClassPart top_level_class;
	ApplicationShellClassPart application_class;
	AppPlusShellClassPart app_plus_shell_class;
} AppPlusShellClassRec, *AppPlusShellWidgetClass;

extern AppPlusShellClassRec appPlusShellClassRec;

typedef struct _AppPlusShellPart
{
	XtPointer data;
	Boolean usePrivateColormap;
	Boolean allowEditRes;
	Boolean useStandardColormaps;
	Atom standardColormap;

	int visualClass;
	int applicationDepth;

	VisualID visualID;

	XtCallbackList saveYourselfCallback;
	XtCallbackList focusChangeCallback;

/*
**  Private
*/
	Atom saveYourself[2];
	int numberProtos;
} AppPlusShellPart;

typedef struct _AppPlusShellRec
{
	CorePart core;
	CompositePart	 composite;
	ShellPart shell;
	WMShellPart wm;
	VendorShellPart vendor; 
	TopLevelShellPart top_level;
	ApplicationShellPart application;
	AppPlusShellPart app_plus_shell;
} AppPlusShellRec, *AppPlusShellPtr;
 
#endif /* _AppPlusShellP_h */
