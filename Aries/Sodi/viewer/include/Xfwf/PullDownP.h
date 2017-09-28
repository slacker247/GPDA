/* Generated by wbuild
 * (generator version 3.1)
 */
#ifndef _XFWF_PULLDOWNP_H
#define _XFWF_PULLDOWNP_H
#include <Xfwf/ButtonP.h>
#include <Xfwf/PullDown.h>
_XFUNCPROTOBEGIN

typedef struct {
/* methods */
/* class variables */
int dummy;
} XfwfPullDownClassPart;

typedef struct _XfwfPullDownClassRec {
CoreClassPart core_class;
CompositeClassPart composite_class;
XfwfCommonClassPart xfwfCommon_class;
XfwfFrameClassPart xfwfFrame_class;
XfwfBoardClassPart xfwfBoard_class;
XfwfLabelClassPart xfwfLabel_class;
XfwfButtonClassPart xfwfButton_class;
XfwfPullDownClassPart xfwfPullDown_class;
} XfwfPullDownClassRec;

typedef struct {
/* resources */
Widget  popup;
String  menu;
Boolean  cascaded;
String  hotkey;
Cursor  menuCursor;
XtCallbackList  prepare;
XtCallbackList  changeSelection;
/* private state */
Boolean  own_popup;
} XfwfPullDownPart;

typedef struct _XfwfPullDownRec {
CorePart core;
CompositePart composite;
XfwfCommonPart xfwfCommon;
XfwfFramePart xfwfFrame;
XfwfBoardPart xfwfBoard;
XfwfLabelPart xfwfLabel;
XfwfButtonPart xfwfButton;
XfwfPullDownPart xfwfPullDown;
} XfwfPullDownRec;

externalref XfwfPullDownClassRec xfwfPullDownClassRec;

_XFUNCPROTOEND
#endif /* _XFWF_PULLDOWNP_H */
