/* Generated by wbuild
 * (generator version 3.1)
 */
#ifndef _XFWF_SPINLABEL_H
#define _XFWF_SPINLABEL_H
#include <Xfwf/Label.h>
_XFUNCPROTOBEGIN
typedef enum {XfwfFirst,XfwfPrev,XfwfNext,XfwfLast} SpinType;

#ifndef XtNarrowForeground
#define XtNarrowForeground "arrowForeground"
#endif
#ifndef XtCArrowForeground
#define XtCArrowForeground "ArrowForeground"
#endif
#ifndef XtRPixel
#define XtRPixel "Pixel"
#endif

#ifndef XtNcallback
#define XtNcallback "callback"
#endif
#ifndef XtCCallback
#define XtCCallback "Callback"
#endif
#ifndef XtRCallback
#define XtRCallback "Callback"
#endif

#ifndef XtNlabelframeWidth
#define XtNlabelframeWidth "labelframeWidth"
#endif
#ifndef XtCLabelframeWidth
#define XtCLabelframeWidth "LabelframeWidth"
#endif
#ifndef XtRDimension
#define XtRDimension "Dimension"
#endif

#ifndef XtNlabelframeType
#define XtNlabelframeType "labelframeType"
#endif
#ifndef XtCLabelframeType
#define XtCLabelframeType "LabelframeType"
#endif
#ifndef XtRFrameType
#define XtRFrameType "FrameType"
#endif

#ifndef XtNhorizontal
#define XtNhorizontal "horizontal"
#endif
#ifndef XtCHorizontal
#define XtCHorizontal "Horizontal"
#endif
#ifndef XtRBoolean
#define XtRBoolean "Boolean"
#endif

typedef struct _XfwfSpinLabelClassRec *XfwfSpinLabelWidgetClass;
typedef struct _XfwfSpinLabelRec *XfwfSpinLabelWidget;
externalref WidgetClass xfwfSpinLabelWidgetClass;
_XFUNCPROTOEND
#endif /* _XFWF_SPINLABEL_H */
