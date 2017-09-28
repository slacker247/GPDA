#ifndef _XRWS_H
#define _XRWS_H
/*
	$Id: Xlt.h,v 1.4 1998/11/14 21:08:58 rwscott Exp $
*/

#include <assert.h>
#include <X11/IntrinsicP.h>
#include <Xm/PushB.h>

#define XltCheckArgSize(a,n) assert(sizeof(a)/sizeof(a[0]) <= n)

#define	XltGetArgs(w,a,n) { \
	XltCheckArgSize(a, n); \
	XtGetValues(w, a, n); \
}
#define	XltSetArgs(w,a,n) { \
	XltCheckArgSize(a, n); \
	XtSetValues(w, a, n); \
}

#ifdef __cplusplus
extern "C" {
#endif

void XltDisplayFallbackResources(char **Fallback);
void XltDisplayOptions(XrmOptionDescRec *opTable, int num_options);
Boolean XltYesNo(Widget w, String Question);
Boolean XltWorking(Widget w, String Question, int PercentDone);
void XltWaitTillMapped(Widget w);
void XltSetClientIcon(Widget W, char **icon);

Widget XltToolBarAddItem(Widget ToolBar, char *Label, char **PixmapData);
void XltToolBarConfigure(Widget w, Widget ToolBar);

void XltRedirectStdErr(Widget Parent);
void XltSelectDebugLevel(Widget W, int *DebugLevelPtr, XmPushButtonCallbackStruct *reason);

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif
