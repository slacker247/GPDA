/**
 *
 * $Id: MrmPrivate.h,v 1.1 1997/02/20 22:28:20 miers Exp $
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


#ifndef MRM_PRIVATE_H
#define MRM_PRIVATE_H

#include <Mrm/MrmPublic.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    Display *display;
    MrmHierarchy hierarchy_for_display;
} MrmPerDisplay, *MrmPerDisplayList;

struct MrmHierachyStruct {
    MrmCount number_of_files;
    String *files;
};

#ifdef __cplusplus
};
#endif

#endif /* MRM_PRIVATE_H */
