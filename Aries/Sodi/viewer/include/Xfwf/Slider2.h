/* Generated by wbuild
 * (generator version 3.1)
 */
#ifndef _XFWF_SLIDER2_H
#define _XFWF_SLIDER2_H
#include <Xfwf/Label.h>
_XFUNCPROTOBEGIN
#include <Xfwf/scroll.h>
void XfwfGetThumb(
#if NeedFunctionPrototypes
Widget,XfwfScrollInfo *
#endif
);
#define range(x) (0.0 <=(x )&&(x )<=1.0 )


void XfwfMoveThumb(
#if NeedFunctionPrototypes
Widget,double ,double 
#endif
);
void XfwfResizeThumb(
#if NeedFunctionPrototypes
Widget,double ,double 
#endif
);
#ifndef XtNthumbColor
#define XtNthumbColor "thumbColor"
#endif
#ifndef XtCThumbColor
#define XtCThumbColor "ThumbColor"
#endif
#ifndef XtRXFwfColor
#define XtRXFwfColor "XFwfColor"
#endif

#ifndef XtNthumbPixmap
#define XtNthumbPixmap "thumbPixmap"
#endif
#ifndef XtCThumbPixmap
#define XtCThumbPixmap "ThumbPixmap"
#endif
#ifndef XtRPixmap
#define XtRPixmap "Pixmap"
#endif

#ifndef XtNminsize
#define XtNminsize "minsize"
#endif
#ifndef XtCMinsize
#define XtCMinsize "Minsize"
#endif
#ifndef XtRDimension
#define XtRDimension "Dimension"
#endif

#ifndef XtNthumbFrameWidth
#define XtNthumbFrameWidth "thumbFrameWidth"
#endif
#ifndef XtCThumbFrameWidth
#define XtCThumbFrameWidth "ThumbFrameWidth"
#endif
#ifndef XtRDimension
#define XtRDimension "Dimension"
#endif

#ifndef XtNthumbFrameType
#define XtNthumbFrameType "thumbFrameType"
#endif
#ifndef XtCThumbFrameType
#define XtCThumbFrameType "ThumbFrameType"
#endif
#ifndef XtRFrameType
#define XtRFrameType "FrameType"
#endif

#ifndef XtNthumbHollow
#define XtNthumbHollow "thumbHollow"
#endif
#ifndef XtCThumbHollow
#define XtCThumbHollow "ThumbHollow"
#endif
#ifndef XtRBoolean
#define XtRBoolean "Boolean"
#endif

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

#ifndef XtNdontMoveOff
#define XtNdontMoveOff "dontMoveOff"
#endif
#ifndef XtCDontMoveOff
#define XtCDontMoveOff "DontMoveOff"
#endif
#ifndef XtRBoolean
#define XtRBoolean "Boolean"
#endif

#ifndef XtNscaleCoordinates
#define XtNscaleCoordinates "scaleCoordinates"
#endif
#ifndef XtCScaleCoordinates
#define XtCScaleCoordinates "ScaleCoordinates"
#endif
#ifndef XtRBoolean
#define XtRBoolean "Boolean"
#endif

typedef struct _XfwfSlider2ClassRec *XfwfSlider2WidgetClass;
typedef struct _XfwfSlider2Rec *XfwfSlider2Widget;
externalref WidgetClass xfwfSlider2WidgetClass;
_XFUNCPROTOEND
#endif /* _XFWF_SLIDER2_H */