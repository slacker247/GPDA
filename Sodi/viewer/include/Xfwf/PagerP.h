/* Generated by wbuild
 * (generator version 3.1)
 */
#ifndef _XFWF_PAGERP_H
#define _XFWF_PAGERP_H
#include <Xfwf/BoardP.h>
#include <Xfwf/Pager.h>
_XFUNCPROTOBEGIN

typedef struct {
/* methods */
/* class variables */
int dummy;
} XfwfPagerClassPart;

typedef struct _XfwfPagerClassRec {
CoreClassPart core_class;
CompositeClassPart composite_class;
XfwfCommonClassPart xfwfCommon_class;
XfwfFrameClassPart xfwfFrame_class;
XfwfBoardClassPart xfwfBoard_class;
XfwfPagerClassPart xfwfPager_class;
} XfwfPagerClassRec;

typedef struct {
/* resources */
String  text;
String  fontFamily;
XFontStruct * roman;
int  lines;
Boolean  wrap;
float  baseline;
int  margin;
String  tablist;
XfwfColor  foreground;
/* private state */
String  fulltext;
int * page;
int  pageno;
Widget  flip_back;
Widget  flip_forward;
String  roman_name;
GC  romangc;
int  baselineskip;
XfwfIcon  f_back;
XfwfIcon  f_forward;
int * tabs;
} XfwfPagerPart;

typedef struct _XfwfPagerRec {
CorePart core;
CompositePart composite;
XfwfCommonPart xfwfCommon;
XfwfFramePart xfwfFrame;
XfwfBoardPart xfwfBoard;
XfwfPagerPart xfwfPager;
} XfwfPagerRec;

externalref XfwfPagerClassRec xfwfPagerClassRec;

_XFUNCPROTOEND
#endif /* _XFWF_PAGERP_H */