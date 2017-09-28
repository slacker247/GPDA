/* Generated by wbuild
 * (generator version 3.1)
 */
#ifndef _XFWF_LABELP_H
#define _XFWF_LABELP_H
#include <Xfwf/BoardP.h>
#include <Xfwf/Label.h>
_XFUNCPROTOBEGIN
typedef void (*set_label_Proc)(
#if NeedFunctionPrototypes
Widget,String 
#endif
);
#define XtInherit_set_label ((set_label_Proc) _XtInherit)
typedef Region  (*set_label_region_Proc)(
#if NeedFunctionPrototypes
Widget,Region 
#endif
);
#define XtInherit_set_label_region ((set_label_region_Proc) _XtInherit)

typedef struct {
/* methods */
set_label_Proc set_label;
set_label_region_Proc set_label_region;
/* class variables */
} XfwfLabelClassPart;

typedef struct _XfwfLabelClassRec {
CoreClassPart core_class;
CompositeClassPart composite_class;
XfwfCommonClassPart xfwfCommon_class;
XfwfFrameClassPart xfwfFrame_class;
XfwfBoardClassPart xfwfBoard_class;
XfwfLabelClassPart xfwfLabel_class;
} XfwfLabelClassRec;

typedef struct {
/* resources */
String  label;
String  tablist;
XFontStruct * font;
XfwfColor  foreground;
XfwfColor  hlForeground;
XfwfAlignment  alignment;
Dimension  topMargin;
Dimension  bottomMargin;
Dimension  leftMargin;
Dimension  rightMargin;
Boolean  shrinkToFit;
int  rvStart;
int  rvLength;
int  hlStart;
int  hlLength;
/* private state */
int  nlines;
int * tabs;
GC  gc;
GC  rv_gc;
GC  hl_gc;
GC  graygc;
Dimension  label_width;
Dimension  label_height;
} XfwfLabelPart;

typedef struct _XfwfLabelRec {
CorePart core;
CompositePart composite;
XfwfCommonPart xfwfCommon;
XfwfFramePart xfwfFrame;
XfwfBoardPart xfwfBoard;
XfwfLabelPart xfwfLabel;
} XfwfLabelRec;

externalref XfwfLabelClassRec xfwfLabelClassRec;

_XFUNCPROTOEND
#endif /* _XFWF_LABELP_H */
