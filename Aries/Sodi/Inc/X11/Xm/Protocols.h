/**
 *
 * $Id: Protocols.h,v 1.2 1997/11/07 22:27:43 rwscott Exp $
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

#ifndef XM_PROTOCOLS_H
#define XM_PROTOCOLS_H

#include <Xm/Xm.h>
#include <Xm/AtomMgr.h>

#ifdef __cplusplus
extern "C" {
#endif

#define XmCR_WM_PROTOCOLS 6666

#define XM_WM_PROTOCOL_ATOM(shell) \
    XmInternAtom(XtDisplay(shell), "WM_PROTOCOLS", FALSE)

#define XmAddWMProtocols(shell, protocols, num_protocols) \
    XmAddProtocols(shell, XM_WM_PROTOCOL_ATOM(shell), \
		   protocols, num_protocols)

#define XmRemoveWMProtocols(shell, protocols, num_protocols) \
    XmRemoveProtocols(shell, XM_WM_PROTOCOL_ATOM(shell), \
		      protocols, num_protocols)

#define XmAddWMProtocolCallback(shell, protocol, callback, closure) \
    XmAddProtocolCallback(shell, XM_WM_PROTOCOL_ATOM(shell), \
			  protocol, callback, closure)

#define XmRemoveWMProtocolCallback(shell, protocol, callback, closure) \
    XmRemoveProtocolCallback(shell, XM_WM_PROTOCOL_ATOM(shell), \
			     protocol, callback, closure)

#define XmActivateWMProtocol(shell, protocol) \
      XmActivateProtocol(shell, XM_WM_PROTOCOL_ATOM(shell), \
			 protocol)

#define XmDeactivateWMProtocol(shell, protocol) \
      XmDeactivateProtocol(shell, XM_WM_PROTOCOL_ATOM(shell), \
			   protocol)

#define XmSetWMProtocolHooks(shell, protocol, pre_h, pre_c, post_h, post_c) \
      XmSetProtocolHooks(shell, XM_WM_PROTOCOL_ATOM(shell), \
			 protocol, pre_h, pre_c, post_h, post_c)


extern WidgetClass xmProtocolClass;

void XmActivateProtocol(Widget shell,
			Atom property,
			Atom protocol);

void XmAddProtocolCallback(Widget shell,
			   Atom property,
			   Atom protocol,
			   XtCallbackProc callback,
			   XtPointer closure);

void XmAddProtocols(Widget shell,
		    Atom property,
		    Atom *protocol,
		    Cardinal num_protocols);

void XmRemoveProtocolCallback(Widget shell,
			      Atom property,
			      Atom protocol,
			      XtCallbackProc callback,
			      XtPointer closure);

void XmRemoveProtocols(Widget shell,
		       Atom property,
		       Atom *protocols,
		       Cardinal num_protocols);

extern void XmDeactivateProtocol(Widget shell,
				 Atom property,
				 Atom proto_atom);

extern void XmSetProtocolHooks(Widget shell,
			       Atom property,
			       Atom proto_atom,
			       XtCallbackProc pre_hook,
			       XtPointer pre_closure,
			       XtCallbackProc post_hook,
			       XtPointer post_closure);

#ifdef __cplusplus
}
#endif

#endif /* XM_PROTOCOLS_H */

