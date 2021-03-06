/* Generated by wbuild
 * (generator version 3.1)
 */
#ifndef _XFWF_INDICATORP_H
#define _XFWF_INDICATORP_H
#include <Xfwf/LabelP.h>
#include <Xfwf/Indicator.h>
_XFUNCPROTOBEGIN
typedef void (*compute_slider_Proc)(
#if NeedFunctionPrototypes
Widget,Position *,Position *,Dimension *,Dimension *,Position *,Position *,Dimension *,Dimension *
#endif
);
#define XtInherit_compute_slider ((compute_slider_Proc) _XtInherit)
typedef void (*compute_range_Proc)(
#if NeedFunctionPrototypes
Widget,Position *,Position *,Dimension *,Dimension *
#endif
);
#define XtInherit_compute_range ((compute_range_Proc) _XtInherit)
typedef void (*move_slider_Proc)(
#if NeedFunctionPrototypes
Widget,int ,int ,int ,int ,int 
#endif
);
#define XtInherit_move_slider ((move_slider_Proc) _XtInherit)
typedef void (*move_label_Proc)(
#if NeedFunctionPrototypes
Widget,int ,int ,int ,int ,int 
#endif
);
#define XtInherit_move_label ((move_label_Proc) _XtInherit)
typedef void (*compute_info_Proc)(
#if NeedFunctionPrototypes
Widget,Position *,Position *,Dimension *,Dimension *,float *
#endif
);
#define XtInherit_compute_info ((compute_info_Proc) _XtInherit)
typedef void (*clear_area_Proc)(
#if NeedFunctionPrototypes
Widget,Position ,Position ,Dimension ,Dimension ,Position ,Position ,Dimension ,Dimension 
#endif
);
#define XtInherit_clear_area ((clear_area_Proc) _XtInherit)
typedef void (*scroll_response_Proc)(
#if NeedFunctionPrototypes
Widget ,XtPointer ,XtPointer 
#endif
);
#define XtInherit_scroll_response ((scroll_response_Proc) _XtInherit)

typedef struct {
/* methods */
compute_slider_Proc compute_slider;
compute_range_Proc compute_range;
move_slider_Proc move_slider;
move_label_Proc move_label;
compute_info_Proc compute_info;
clear_area_Proc clear_area;
scroll_response_Proc scroll_response;
/* class variables */
} XfwfIndicatorClassPart;

typedef struct _XfwfIndicatorClassRec {
CoreClassPart core_class;
CompositeClassPart composite_class;
XfwfCommonClassPart xfwfCommon_class;
XfwfFrameClassPart xfwfFrame_class;
XfwfBoardClassPart xfwfBoard_class;
XfwfLabelClassPart xfwfLabel_class;
XfwfIndicatorClassPart xfwfIndicator_class;
} XfwfIndicatorClassRec;

typedef struct {
/* resources */
Pixel  sliderColor;
Dimension  sliderWidth;
Pixel  rangeColor;
XtCallbackList  scrollCallback;
XtCallbackProc  scrollResponse;
Dimension  sliderFrameWidth;
FrameType  sliderFrameType;
Dimension  hairlineWidth;
Dimension  hairlineMargin;
Pixel  hairlineColor;
Dimension  labelMargin;
Dimension  sliderMargin;
/* private state */
float  slider_pos;
float  rangeMin;
float  rangeMax;
Boolean  drag_in_progress;
int  m_delta;
GC  slidergc;
GC  sliderlightgc;
GC  sliderdarkgc;
GC  rangegc;
GC  hairlinegc;
} XfwfIndicatorPart;

typedef struct _XfwfIndicatorRec {
CorePart core;
CompositePart composite;
XfwfCommonPart xfwfCommon;
XfwfFramePart xfwfFrame;
XfwfBoardPart xfwfBoard;
XfwfLabelPart xfwfLabel;
XfwfIndicatorPart xfwfIndicator;
} XfwfIndicatorRec;

externalref XfwfIndicatorClassRec xfwfIndicatorClassRec;

_XFUNCPROTOEND
#endif /* _XFWF_INDICATORP_H */
