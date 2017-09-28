/* Generated by wbuild
 * (generator version 3.1)
 */
#ifndef _XFWF_ANIMATOR_H
#define _XFWF_ANIMATOR_H
#include <Xfwf/Board.h>
_XFUNCPROTOBEGIN
void XfwfStartAnimation(
#if NeedFunctionPrototypes
Widget
#endif
);
void XfwfStopAnimation(
#if NeedFunctionPrototypes
Widget
#endif
);
#ifndef XtNimages
#define XtNimages "images"
#endif
#ifndef XtCImages
#define XtCImages "Images"
#endif
#ifndef XtRImageList
#define XtRImageList "ImageList"
#endif

#ifndef XtNintervals
#define XtNintervals "intervals"
#endif
#ifndef XtCIntervals
#define XtCIntervals "Intervals"
#endif
#ifndef XtRCardinalList
#define XtRCardinalList "CardinalList"
#endif

#ifndef XtNdefaultInterval
#define XtNdefaultInterval "defaultInterval"
#endif
#ifndef XtCDefaultInterval
#define XtCDefaultInterval "DefaultInterval"
#endif
#ifndef XtRCardinal
#define XtRCardinal "Cardinal"
#endif

#ifndef XtNcycle
#define XtNcycle "cycle"
#endif
#ifndef XtCCycle
#define XtCCycle "Cycle"
#endif
#ifndef XtRBoolean
#define XtRBoolean "Boolean"
#endif

typedef struct _XfwfAnimatorClassRec *XfwfAnimatorWidgetClass;
typedef struct _XfwfAnimatorRec *XfwfAnimatorWidget;
externalref WidgetClass xfwfAnimatorWidgetClass;
_XFUNCPROTOEND
#endif /* _XFWF_ANIMATOR_H */
