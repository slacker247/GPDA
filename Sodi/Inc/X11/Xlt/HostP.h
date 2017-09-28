/**
 *
 * $Id: HostP.h,v 1.2 1998/08/23 15:28:36 rwscott Exp $
 *
 * Copyright (C) 1996 Free Software Foundation, Inc.
 *
 * This file is part of the GNU LessTif Extension Library.
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

#ifndef _HOSTP_H
#define _HOSTP_H

#include <Host.h>
#include <X11/ObjectP.h>

typedef enum { MODE_ASCII, MODE_BINARY } InputMode;

typedef struct {
	int Fd;
	String Name;
	String Port;
	XtInputId InputId;
	XtIntervalId OutputId;
	XtCallbackList DisconnectCallback;
	XtCallbackList ConnectCallback;
	XtCallbackList InputCallback;
	XtCallbackList AsciiInputCallback;
	XtCallbackList BinaryInputCallback;
	char *OutputData;
	int OutputMaxSize;
	int OutputSize;
	char *InputData;
	int InputMaxSize;
	int InputSize;
	InputMode Mode;
	String Terminator;
	int x;
	int y;
	int tmp;
	Boolean Throttle;
	int Delay;
	Widget host_dialog;
	int InputNeed;
	XtCallbackList OutputCallback;
} XltHostPart;

#define Host_Fd(w) (((XltHostWidget)w)->host.Fd)
#define Host_Name(w) (((XltHostWidget)w)->host.Name)
#define Host_Port(w) (((XltHostWidget)w)->host.Port)
#define Host_InputId(w) (((XltHostWidget)w)->host.InputId)
#define Host_OutputId(w) (((XltHostWidget)w)->host.OutputId)
#define Host_DisconnectCallback(w) (((XltHostWidget)w)->host.DisconnectCallback)
#define Host_OutputCallback(w) (((XltHostWidget)w)->host.OutputCallback)
#define Host_ConnectCallback(w) (((XltHostWidget)w)->host.ConnectCallback)
#define Host_InputCallback(w) (((XltHostWidget)w)->host.InputCallback)
#define Host_AsciiInputCallback(w) (((XltHostWidget)w)->host.AsciiInputCallback)
#define Host_BinaryInputCallback(w) (((XltHostWidget)w)->host.BinaryInputCallback)
#define Host_OutputData(w) (((XltHostWidget)w)->host.OutputData)
#define Host_OutputMaxSize(w) (((XltHostWidget)w)->host.OutputMaxSize)
#define Host_OutputSize(w) (((XltHostWidget)w)->host.OutputSize)
#define Host_InputData(w) (((XltHostWidget)w)->host.InputData)
#define Host_InputMaxSize(w) (((XltHostWidget)w)->host.InputMaxSize)
#define Host_InputSize(w) (((XltHostWidget)w)->host.InputSize)
#define Host_Mode(w) (((XltHostWidget)w)->host.Mode)
#define Host_Terminator(w) (((XltHostWidget)w)->host.Terminator)
#define Host_x(w) (((XltHostWidget)w)->host.x)
#define Host_y(w) (((XltHostWidget)w)->host.y)
#define Host_tmp(w) (((XltHostWidget)w)->host.tmp)
#define Host_Throttle(w) (((XltHostWidget)w)->host.Throttle)
#define Host_Delay(w) (((XltHostWidget)w)->host.Delay)
#define Host_Dialog(w) (((XltHostWidget)w)->host.host_dialog)
#define Host_InputNeed(w) (((XltHostWidget)w)->host.InputNeed)

typedef struct _XltHostRec {
	ObjectPart object;
	XltHostPart host;
} XltHostRec;

typedef struct {
	XtPointer extension;
} XltHostClassPart;

typedef struct _XltHostClassRec {
	ObjectClassPart object_class;
	XltHostClassPart host_class;
} XltHostClassRec;

extern XltHostClassRec xrwsHostClassRec;
#endif
