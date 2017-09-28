/**
 *
 * $Id: XmosP.h,v 1.3 1998/10/17 06:21:18 gritton Exp $
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

#ifndef XM_OS_P_H
#define XM_OS_P_H

#include <Xm/XmP.h>

#ifdef __cplusplus
extern "C" {
#endif

extern int _XmSleep(unsigned int secs);

extern int _XmMicroSleep(long secs);
extern int XmeMicroSleep(long secs);

extern String _XmOSGetHomeDirName(void);
extern String XmeGetHomeDirName(void);

extern XmString _XmOSGetLocalizedString(char *reserved,
					Widget w,
					String resourceName,
					String value);

extern String _XmOSFindPatternPart(String fileSpec);

extern void _XmOSQualifyFileSpec(String dirSpec,
				 String filterSpec,
				 String *pQualifiedDir,
				 String *pQualifiedPattern);

extern void _XmOSGetDirEntries(String qualifiedDir,
			       String matchPattern,
			       unsigned char fileType,
			       Boolean matchDotsLiterally,
			       Boolean listWithFullPath,
			       String **pEntries,
			       unsigned int *pNumEntries,
			       unsigned int *pNumAlloc);

extern void _XmOSBuildFileList(String dirPath,
			       String pattern,
			       unsigned char typeMask,
			       String **pEntries,
			       unsigned int *pNumEntries,
			       unsigned int *pNumAlloc);

extern int _XmOSFileCompare(XmConst void *sp1, XmConst void *sp2);

extern String _XmOSGetHomeDirName(void);

extern String _XmOSGetCurrentDirName(void);

#ifdef __cplusplus
}
#endif

#endif /* XM_OS_P_H */
