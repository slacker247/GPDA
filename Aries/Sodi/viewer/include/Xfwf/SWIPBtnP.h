/* Generated by wbuild
 * (generator version 3.1)
 */
#ifndef _XFWF_SWIPBTNP_H
#define _XFWF_SWIPBTNP_H
#include <Xfwf/ButtonP.h>
#include <Xfwf/SWIPBtn.h>
_XFUNCPROTOBEGIN

typedef struct {
/* methods */
/* class variables */
int dummy;
} XfwfSWIPButtonClassPart;

typedef struct _XfwfSWIPButtonClassRec {
CoreClassPart core_class;
CompositeClassPart composite_class;
XfwfCommonClassPart xfwfCommon_class;
XfwfFrameClassPart xfwfFrame_class;
XfwfBoardClassPart xfwfBoard_class;
XfwfLabelClassPart xfwfLabel_class;
XfwfButtonClassPart xfwfButton_class;
XfwfSWIPButtonClassPart xfwfSWIPButton_class;
} XfwfSWIPButtonClassRec;

typedef struct {
/* resources */
/* private state */
int dummy;
} XfwfSWIPButtonPart;

typedef struct _XfwfSWIPButtonRec {
CorePart core;
CompositePart composite;
XfwfCommonPart xfwfCommon;
XfwfFramePart xfwfFrame;
XfwfBoardPart xfwfBoard;
XfwfLabelPart xfwfLabel;
XfwfButtonPart xfwfButton;
XfwfSWIPButtonPart xfwfSWIPButton;
} XfwfSWIPButtonRec;

externalref XfwfSWIPButtonClassRec xfwfSWIPButtonClassRec;

_XFUNCPROTOEND
#endif /* _XFWF_SWIPBTNP_H */