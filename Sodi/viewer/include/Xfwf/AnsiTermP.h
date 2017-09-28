/* Generated by wbuild
 * (generator version 3.1)
 */
#ifndef _XFWF_ANSITERMP_H
#define _XFWF_ANSITERMP_H
#include <Xfwf/BoardP.h>
#include <Xfwf/AnsiTerm.h>
_XFUNCPROTOBEGIN
#define ATTRIB_BOLD (1 <<0 )


#define ATTRIB_REV (1 <<1 )


#define ATTRIB_ULINE (1 <<2 )


#define ATTRIB_INVIS (1 <<3 )


#define ATTRIB_DIRTY (1 <<7 )


#define Init 0 


#define Esc 1 


#define EscLB 2 


#define EscLBGT 3 


#define EscLBGT2 4 


#define Register0 5 


#define Register1 6 


#define Register2 7 


#define Register3 8 


#define Register4 9 


typedef void (*scroll_response_Proc)(
#if NeedFunctionPrototypes
Widget ,XtPointer ,XtPointer 
#endif
);
#define XtInherit_scroll_response ((scroll_response_Proc) _XtInherit)
typedef void (*write_Proc)(
#if NeedFunctionPrototypes
Widget,char *,int 
#endif
);
#define XtInherit_write ((write_Proc) _XtInherit)

typedef struct {
/* methods */
scroll_response_Proc scroll_response;
write_Proc write;
/* class variables */
} XfwfAnsiTermClassPart;

typedef struct _XfwfAnsiTermClassRec {
CoreClassPart core_class;
CompositeClassPart composite_class;
XfwfCommonClassPart xfwfCommon_class;
XfwfFrameClassPart xfwfFrame_class;
XfwfBoardClassPart xfwfBoard_class;
XfwfAnsiTermClassPart xfwfAnsiTerm_class;
} XfwfAnsiTermClassRec;

typedef struct {
/* resources */
int  rows;
int  columns;
String  font;
XFontStruct * boldfont;
int  margin;
XfwfColor  foreground;
XtCallbackList  keyCallback;
XtCallbackList  resizeCallback;
XtCallbackList  scrollCallback;
XtCallbackProc  scrollResponse;
/* private state */
int  cellwidth;
int  cellheight;
int  uline_pos;
int  uline_thick;
XFontStruct * fnt;
GC  gc;
GC  boldgc;
GC  revgc;
GC  revboldgc;
char ** contents;
char ** attribs;
int  allocated_rows;
int  allocated_columns;
int  cursorx;
int  cursory;
int  old_cx;
int  old_cy;
int  savedx;
int  savedy;
Bool  cursor_on;
char  cur_attrib;
Bool  insert_mode;
char  last_char;
int  state;
int  escparm[5];
XtIntervalId  timer;
int  start_x;
int  start_y;
int  end_x;
int  end_y;
char * selection;
int  selection_len;
Bool  drag_started;
int  locked_lines;
int  xoffset;
int  yoffset;
} XfwfAnsiTermPart;

typedef struct _XfwfAnsiTermRec {
CorePart core;
CompositePart composite;
XfwfCommonPart xfwfCommon;
XfwfFramePart xfwfFrame;
XfwfBoardPart xfwfBoard;
XfwfAnsiTermPart xfwfAnsiTerm;
} XfwfAnsiTermRec;

externalref XfwfAnsiTermClassRec xfwfAnsiTermClassRec;

_XFUNCPROTOEND
#endif /* _XFWF_ANSITERMP_H */
