/* Generated by wbuild
 * (generator version 3.1)
 */
#ifndef _XFWF_SCROLLBAR_H
#define _XFWF_SCROLLBAR_H
#include <Xfwf/Board.h>
_XFUNCPROTOBEGIN
void XfwfSetScrollbar(
#if NeedFunctionPrototypes
Widget,double ,double 
#endif
);
#ifndef XtNscrollCallback
#define XtNscrollCallback "scrollCallback"
#endif
#ifndef XtCScrollCallback
#define XtCScrollCallback "ScrollCallback"
#endif
#ifndef XtRCallback
#define XtRCallback "Callback"
#endif

#ifndef XtNscrollResponse
#define XtNscrollResponse "scrollResponse"
#endif
#ifndef XtCScrollResponse
#define XtCScrollResponse "ScrollResponse"
#endif
#ifndef XtRXTCallbackProc
#define XtRXTCallbackProc "XTCallbackProc"
#endif

#ifndef XtNchildList
#define XtNchildList "childList"
#endif
#ifndef XtCChildList
#define XtCChildList "ChildList"
#endif
#ifndef XtRString
#define XtRString "String"
#endif

typedef struct _XfwfScrollbarClassRec *XfwfScrollbarWidgetClass;
typedef struct _XfwfScrollbarRec *XfwfScrollbarWidget;
externalref WidgetClass xfwfScrollbarWidgetClass;
_XFUNCPROTOEND
#endif /* _XFWF_SCROLLBAR_H */
