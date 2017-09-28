/* Generated by wbuild
 * (generator version 3.1)
 */
#ifndef _XFWF_MENUBARP_H
#define _XFWF_MENUBARP_H
#include <Xfwf/RowsP.h>
#include <Xfwf/MenuBar.h>
_XFUNCPROTOBEGIN
typedef void (*process_menu_Proc)(
#if NeedFunctionPrototypes
Widget,Widget ,Cursor 
#endif
);
#define XtInherit_process_menu ((process_menu_Proc) _XtInherit)

typedef struct {
/* methods */
process_menu_Proc process_menu;
/* class variables */
} XfwfMenuBarClassPart;

typedef struct _XfwfMenuBarClassRec {
CoreClassPart core_class;
CompositeClassPart composite_class;
XfwfCommonClassPart xfwfCommon_class;
XfwfFrameClassPart xfwfFrame_class;
XfwfBoardClassPart xfwfBoard_class;
XfwfRowsClassPart xfwfRows_class;
XfwfMenuBarClassPart xfwfMenuBar_class;
} XfwfMenuBarClassRec;

typedef struct {
/* resources */
/* private state */
Widget  current_menu;
} XfwfMenuBarPart;

typedef struct _XfwfMenuBarRec {
CorePart core;
CompositePart composite;
XfwfCommonPart xfwfCommon;
XfwfFramePart xfwfFrame;
XfwfBoardPart xfwfBoard;
XfwfRowsPart xfwfRows;
XfwfMenuBarPart xfwfMenuBar;
} XfwfMenuBarRec;

externalref XfwfMenuBarClassRec xfwfMenuBarClassRec;

_XFUNCPROTOEND
#endif /* _XFWF_MENUBARP_H */
