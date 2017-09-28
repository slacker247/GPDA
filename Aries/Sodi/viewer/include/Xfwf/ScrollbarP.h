/* Generated by wbuild
 * (generator version 3.1)
 */
#ifndef _XFWF_SCROLLBARP_H
#define _XFWF_SCROLLBARP_H
#include <Xfwf/BoardP.h>
#include <Xfwf/Scrollbar.h>
_XFUNCPROTOBEGIN

typedef struct {
/* methods */
/* class variables */
int dummy;
} XfwfScrollbarClassPart;

typedef struct _XfwfScrollbarClassRec {
CoreClassPart core_class;
CompositeClassPart composite_class;
XfwfCommonClassPart xfwfCommon_class;
XfwfFrameClassPart xfwfFrame_class;
XfwfBoardClassPart xfwfBoard_class;
XfwfScrollbarClassPart xfwfScrollbar_class;
} XfwfScrollbarClassRec;

typedef struct {
/* resources */
XtCallbackList  scrollCallback;
XtCallbackProc  scrollResponse;
String  childList;
/* private state */
XtCallbackProc  child_scroll;
XtCallbackList  responseCallback;
} XfwfScrollbarPart;

typedef struct _XfwfScrollbarRec {
CorePart core;
CompositePart composite;
XfwfCommonPart xfwfCommon;
XfwfFramePart xfwfFrame;
XfwfBoardPart xfwfBoard;
XfwfScrollbarPart xfwfScrollbar;
} XfwfScrollbarRec;

externalref XfwfScrollbarClassRec xfwfScrollbarClassRec;

_XFUNCPROTOEND
#endif /* _XFWF_SCROLLBARP_H */