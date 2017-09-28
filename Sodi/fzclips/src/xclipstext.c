/******************************* ClipsTextAct.c ******************************/
/*                                                                           */
/*****************************************************************************/
/*                                                                           */
/*                                                                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <X11/Xatom.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/keysym.h>


#include <X11/Xmu/Atoms.h>
#include <X11/Xmu/Misc.h>

#ifdef XAW3D
#include <X11/Xaw3d/TextP.h>
#else
#include <X11/Xaw/TextP.h>
#endif


#include "clips.h"

/* next 2 lines added to fix?? cntrl-c problem */
#include "evaluatn.h"
#include "filecom.h"

extern void ClearCLIPSCallback();
extern void ResetCallback();
extern void LoadFactsCallback();
extern void FactsWindowCallback();
extern void AgendaWindowCallback();
extern void LoadRulesCallback();
extern void DribbleCallback();
extern void QuitCallback();
extern void RunCallback();
extern void SaveRulesCallback();
extern void StepCallback();
extern void EditCallback();
extern void CommandLineCLIPSCallback();
extern void EditorSaveAsCallback();
extern void EditorHelpSelect();
extern void EditorLoadFileToCLIPSCallback();
extern void EditorRevertCallback();
extern void EditorSaveCallback();


#define SrcScan                XawTextSourceScan
#define FindDist               XawTextSinkFindDistance
#define FindPos                XawTextSinkFindPosition

/*
 * These are defined in TextPop.c
 */

void _XawTextInsertFileAction(), _XawTextInsertFile(), _XawTextSearch();
void _XawTextSearch(), _XawTextDoSearchAction(), _XawTextDoReplaceAction();
void _XawTextSetField(), _XawTextPopdownSearchAction();

/*
 * These are defined in Text.c
 */

char * _XawTextGetText();
void _XawTextBuildLineTable(), _XawTextAlterSelection(), _XawTextVScroll();
void _XawTextSetSelection(), _XawTextCheckResize(), _XawTextExecuteUpdate();
void _XawTextSetScrollBars(), _XawTextClearAndCenterDisplay();
Atom * _XawTextSelectionList();

/*
 *  These are defined in xclips.c
 */

int SetClipsStart(),GetClipsStart();

void  MoveEndOfFile();
extern int CLIPSInputCount;

static void StartAction(ctx, event)
  TextWidget ctx;
  XEvent *event;
  {
  _XawTextPrepareToUpdate(ctx);
  if (event != NULL) {
    switch (event->type) {
    case ButtonPress:
    case ButtonRelease:
      ctx->text.time = event->xbutton.time;
      ctx->text.ev_x = event->xbutton.x;
      ctx->text.ev_y = event->xbutton.y;
      break;
    case KeyPress:
    case KeyRelease:
      ctx->text.time = event->xkey.time;
      ctx->text.ev_x = event->xkey.x;
      ctx->text.ev_y = event->xkey.y;
      break;
    case MotionNotify:
      ctx->text.time = event->xmotion.time;
      ctx->text.ev_x = event->xmotion.x;
      ctx->text.ev_y = event->xmotion.y;
      break;
    case EnterNotify:
    case LeaveNotify:
      ctx->text.time = event->xcrossing.time;
      ctx->text.ev_x = event->xcrossing.x;
      ctx->text.ev_y = event->xcrossing.y;
    }
  }
  }

static void EndAction(ctx)
  TextWidget ctx;
  {
  _XawTextCheckResize(ctx);
  _XawTextExecuteUpdate(ctx);
  ctx->text.mult = 1;
  }



/*
 * These functions are superceeded by insert-selection.
 */

static void StuffFromBuffer(ctx, buffer)
  TextWidget ctx;
  int buffer;
  {
  extern char *XFetchBuffer();
  XawTextBlock text;
  text.ptr = XFetchBuffer(XtDisplay(ctx), &(text.length), buffer);
  text.firstPos = 0;
  if (_XawTextReplace(ctx, ctx->text.insertPos, ctx->text.insertPos, &text)) {
    XBell(XtDisplay(ctx), 0);
    return;
  }
  ctx->text.insertPos = SrcScan(ctx->text.source, ctx->text.insertPos,
				XawstPositions, XawsdRight, text.length, TRUE);
  XtFree(text.ptr);
  }

void UnKill(ctx, event)
  TextWidget ctx;
  XEvent *event;
  {
  StartAction(ctx, event);
  StuffFromBuffer(ctx, 1);
  EndAction(ctx);
  }

void Stuff(ctx, event)
  TextWidget ctx;
  XEvent *event;
  {
  StartAction(ctx, event);
  StuffFromBuffer(ctx, 0);
  EndAction(ctx);
  }


struct _SelectionList
  {
  String *params;
  Cardinal count;
  Time time;
  };

static void GetSelection();

static void ClipsGetSelection();

/* ARGSUSED */
static void _SelectionReceived(w, client_data, selection, type, value, length, format)
Widget w;
caddr_t client_data;
Atom *selection, *type;
caddr_t value;
unsigned long *length;
int *format;
{
  TextWidget ctx = (TextWidget)w;
  XawTextBlock text;

  if (*type == 0 /*XT_CONVERT_FAIL*/ || *length == 0) {
    struct _SelectionList* list = (struct _SelectionList*)client_data;
    if (list != NULL) {
      GetSelection(w, list->time, list->params, list->count);
      XtFree(client_data);
    }
    return;
  }

  StartAction(ctx, NULL);
  text.ptr = (char*)value;
  text.firstPos = 0;
  text.length = *length;
  text.format = FMT8BIT;
  if (_XawTextReplace(ctx, ctx->text.insertPos, ctx->text.insertPos, &text)) {
    XBell(XtDisplay(ctx), 0);
    return;
  }
  ctx->text.insertPos = SrcScan(ctx->text.source, ctx->text.insertPos,
                                XawstPositions, XawsdRight, text.length, TRUE);

  EndAction(ctx);
  _XawTextSetScrollBars(ctx);
  XtFree(client_data);
  XtFree(value);
}

static void _ClipsSelectionReceived(w, client_data, selection, type, value, length, format)
  Widget w;
  caddr_t client_data;
  Atom *selection, *type;
  caddr_t value;
  unsigned long *length;
  int *format;
  {
  TextWidget ctx = (TextWidget)w;
  XawTextBlock text;
  extern Boolean send_to_clips;
  char *cmdstr;
  extern int SetCommandString();
  extern int AppendCommandString();
  extern char *GetCommandString();

  if (*type == 0 /*XT_CONVERT_FAIL*/ || *length == 0) {
    struct _SelectionList* list = (struct _SelectionList*)client_data;
    if (list != NULL) {
      ClipsGetSelection(w, list->time, list->params, list->count);
      XtFree(client_data);
    }
      return;
  }
  StartAction(ctx, NULL);
  text.ptr = (char*)value;
  if(send_to_clips)
   {
       cmdstr = GetCommandString();
       if(cmdstr == NULL)
         {
           SetCommandString(text.ptr);
         }
       else
         {
           AppendCommandString(text.ptr);
          }
       send_to_clips = False;
   }
  text.firstPos = 0;
  text.length = *length;
  text.format = FMT8BIT;
  if (_XawTextReplace(ctx, ctx->text.insertPos, ctx->text.insertPos, &text)) {
    XBell(XtDisplay(ctx), 0);
    return;
  }
  ctx->text.insertPos = SrcScan(ctx->text.source, ctx->text.insertPos, 
				XawstPositions, XawsdRight, text.length, TRUE);

  EndAction(ctx);
  _XawTextSetScrollBars(ctx);
  XtFree(client_data);
  XtFree(value);
}

static void GetSelection(w, time, params, num_params)
Widget w;
Time time;
String *params;                 /* selections in precedence order */
Cardinal num_params;
{
    Atom selection;
    int buffer;

    XmuInternStrings(XtDisplay(w), params, (Cardinal)1, &selection);
    switch (selection) {
      case XA_CUT_BUFFER0: buffer = 0; break;
      case XA_CUT_BUFFER1: buffer = 1; break;
      case XA_CUT_BUFFER2: buffer = 2; break;
      case XA_CUT_BUFFER3: buffer = 3; break;
      case XA_CUT_BUFFER4: buffer = 4; break;
      case XA_CUT_BUFFER5: buffer = 5; break;
      case XA_CUT_BUFFER6: buffer = 6; break;
      case XA_CUT_BUFFER7: buffer = 7; break;
      default:         buffer = -1;
    }
    if (buffer >= 0) {
        int nbytes;
        unsigned long length;
        int fmt8 = 8;
        Atom type = XA_STRING;
        char *line = XFetchBuffer(XtDisplay(w), &nbytes, buffer);
        if (length = nbytes)
            _SelectionReceived(w, NULL, &selection, &type, (caddr_t)line,
                               &length, &fmt8);
        else if (num_params > 1)
            GetSelection(w, time, params+1, num_params-1);
    } else {
        struct _SelectionList* list;
        if (--num_params) {
            list = XtNew(struct _SelectionList);
            list->params = params + 1;
            list->count = num_params;
            list->time = time;
        } else list = NULL;
        XtGetSelectionValue(w, selection, XA_STRING,
                            (XtSelectionCallbackProc)_SelectionReceived,
                            (XtPointer)list, time);
    }
}

static void ClipsGetSelection(w, time, params, num_params)
Widget w;
Time time;
String *params;			/* selections in precedence order */
Cardinal num_params;
{
    Atom selection;
    int buffer;
    TextWidget ctx = (TextWidget)w;
   
/*    XmuInternStrings(XtDisplay(w), params, (Cardinal)1, &selection);*/
    selection = XInternAtom(XtDisplay(w), *params, False);
    switch (selection) {
      case XA_CUT_BUFFER0: buffer = 0; break;
      case XA_CUT_BUFFER1: buffer = 1; break;
      case XA_CUT_BUFFER2: buffer = 2; break;
      case XA_CUT_BUFFER3: buffer = 3; break;
      case XA_CUT_BUFFER4: buffer = 4; break;
      case XA_CUT_BUFFER5: buffer = 5; break;
      case XA_CUT_BUFFER6: buffer = 6; break;
      case XA_CUT_BUFFER7: buffer = 7; break;
      default:	       buffer = -1;
    }
    if (buffer >= 0) {
	int nbytes;
	unsigned long length;
	int fmt8 = 8;
	Atom type = XA_STRING;
	char *line = XFetchBuffer(XtDisplay(w), &nbytes, buffer);
	if (length = nbytes)
	    _ClipsSelectionReceived(w, NULL, &selection, &type, (caddr_t)line,
			       &length, &fmt8);
	else if (num_params > 1)
	    ClipsGetSelection(w, time, params+1, num_params-1);
    } else {
	struct _SelectionList* list;

	if (--num_params) {
	    list = (struct _SelectionList*)XtNew(struct _SelectionList);
	    list->params = params + 1;
	    list->count = num_params;
	    list->time = time;
	} else list = NULL;
	XtGetSelectionValue(w, selection, XA_STRING, 
                            (XtSelectionCallbackProc)_ClipsSelectionReceived,
			    (XtPointer)list, time);
    }
}

static void InsertSelection(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;         /* precedence list of selections to try */
Cardinal *num_params;
{
  StartAction((TextWidget)w, event); /* Get Time. */
  GetSelection(w, ((TextWidget)w)->text.time, params, *num_params);
  EndAction((TextWidget)w);
}

static void InsertClipsSelection(w, event, params, num_params)
  Widget w;
  XEvent *event;
  String *params;		/* precedence list of selections to try */
  Cardinal *num_params;
  {
  extern Widget dialog_text;

  TextWidget ctx = (TextWidget)w;

  MoveEndOfFile(dialog_text,event);
  StartAction((TextWidget)w, event); /* Get Time. */
  ClipsGetSelection(w, ((TextWidget)w)->text.time, params, *num_params);
  EndAction((TextWidget)w);
  }

/************************************************************
 *
 * Routines for Moving Around.
 *
 ************************************************************/

static void Move(ctx, event, dir, type, include)
  TextWidget ctx;
  XEvent *event;
  XawTextScanDirection dir;
  XawTextScanType type;
  Boolean include;
  {
  StartAction(ctx, event);
  ctx->text.insertPos = SrcScan(ctx->text.source, ctx->text.insertPos, type, dir, ctx->text.mult, include);
  EndAction(ctx);
  }

static void MoveForwardChar(w, event)
  Widget w;
  XEvent *event;
  {
  Move((TextWidget) w, event, XawsdRight, XawstPositions, TRUE);
  }

static void MoveBackwardChar(w, event)
  Widget w;
  XEvent *event;
  {
  Move((TextWidget) w, event, XawsdLeft, XawstPositions, TRUE);
  }

static void MoveForwardWord(w, event)
  Widget w;
  XEvent *event;
  {
  Move((TextWidget) w, event, XawsdRight, XawstWhiteSpace, FALSE);
  }

static void MoveBackwardWord(w, event)
  Widget w;
  XEvent *event;
  {
  Move((TextWidget) w, event, XawsdLeft, XawstWhiteSpace, FALSE);
  }

static void MoveForwardParagraph(w, event)
  Widget w;
  XEvent *event;
  {
  Move((TextWidget) w, event, XawsdRight, XawstParagraph, FALSE);
  }

static void MoveBackwardParagraph(w, event)
  Widget w;
  XEvent *event;
  {
  Move((TextWidget) w, event, XawsdLeft, XawstParagraph, FALSE);
  }

static void MoveToLineEnd(w, event)
  Widget w;
  XEvent *event;
  {
  Move((TextWidget) w, event, XawsdRight, XawstEOL, FALSE);
  }

static void MoveToLineStart(w, event)
  Widget w;
  XEvent *event;
  {
  Move((TextWidget) w, event, XawsdLeft, XawstEOL, FALSE);
  }


static void MoveLine(ctx, event, dir)
  TextWidget ctx;
  XEvent *event;
  XawTextScanDirection dir;
  {
  XawTextPosition new, next_line, junk;
  int from_left, garbage;

  StartAction(ctx, event);

  if (dir == XawsdLeft)
    ctx->text.mult++;

  new = SrcScan(ctx->text.source, ctx->text.insertPos, XawstEOL, XawsdLeft, 1, FALSE);

  FindDist(ctx->text.sink, new, ctx->text.margin.left, ctx->text.insertPos, &from_left, &junk, &garbage);

  new = SrcScan(ctx->text.source, ctx->text.insertPos, XawstEOL, dir, ctx->text.mult, (dir == XawsdRight));

  next_line = SrcScan(ctx->text.source, new, XawstEOL, XawsdRight, 1, FALSE);

  FindPos(ctx->text.sink, new, ctx->text.margin.left, from_left, FALSE, &(ctx->text.insertPos), &garbage, &garbage);
  
  if (ctx->text.insertPos > next_line)
    ctx->text.insertPos = next_line;

  EndAction(ctx);
}

static void MoveNextLine(w, event)
  Widget w;
  XEvent *event;
  {
  MoveLine( (TextWidget) w, event, XawsdRight);
  }

static void MovePreviousLine(w, event)
  Widget w;
  XEvent *event;
  {
  MoveLine( (TextWidget) w, event, XawsdLeft);
  }

void MoveBeginningOfFile(w, event)
  Widget w;
  XEvent *event;
  {
  Move((TextWidget) w, event, XawsdLeft, XawstAll, TRUE);
  }

void MoveEndOfFile(w, event)
  Widget w;
  XEvent *event;
  {
  Move((TextWidget) w, event, XawsdRight, XawstAll, TRUE);
  }

static void Scroll(ctx, event, dir)
  TextWidget ctx;
  XEvent *event;
  XawTextScanDirection dir;
  {
  StartAction(ctx, event);

  if (dir == XawsdLeft)
    _XawTextVScroll(ctx, ctx->text.mult);
  else
    _XawTextVScroll(ctx, -ctx->text.mult);

  EndAction(ctx);
  }

static void ScrollOneLineUp(w, event)
  Widget w;
  XEvent *event;
  {
  Scroll( (TextWidget) w, event, XawsdLeft);
  }

static void ScrollOneLineDown(w, event)
  Widget w;
  XEvent *event;
  {
  Scroll( (TextWidget) w, event, XawsdRight);
  }

static void MovePage(ctx, event, dir)
  TextWidget ctx;
  XEvent *event;
  XawTextScanDirection dir;
  {
  int scroll_val = Max(1, ctx->text.lt.lines - 2);

  if (dir == XawsdLeft)
    scroll_val = -scroll_val;

  StartAction(ctx, event);
  _XawTextVScroll(ctx, scroll_val);
  ctx->text.insertPos = ctx->text.lt.top;
  EndAction(ctx);
  }

static void MoveNextPage(w, event)
  Widget w;
  XEvent *event;
  {
  MovePage((TextWidget) w, event, XawsdRight);
  }

static void MovePreviousPage(w, event)
  Widget w;
  XEvent *event;
  {
  MovePage((TextWidget) w, event, XawsdLeft);
  }

/************************************************************
 *
 * Delete Routines.
 *
 ************************************************************/

static void _DeleteOrKill(ctx, from, to, kill)
  TextWidget ctx;
  XawTextPosition from, to;
  Boolean	kill;
  {
  XawTextBlock text;
  char *ptr;
  
  if (kill && from < to)
    {
    ptr = _XawTextGetText(ctx, from, to);
    XStoreBuffer(XtDisplay(ctx), ptr, strlen(ptr), 1);
    XtFree(ptr);
    }
  text.length = 0;
  text.firstPos = 0;
  if (_XawTextReplace(ctx, from, to, &text))
    {
    XBell(XtDisplay(ctx), 50);
    return;
    }
  ctx->text.insertPos = from;
  ctx->text.showposition = TRUE; 
  }

static void DeleteOrKill(ctx, event, dir, type, include, kill)
  TextWidget ctx;
  XEvent *event;
  XawTextScanDirection dir;
  XawTextScanType type;
  Boolean include, kill;
  {
  XawTextPosition from, to;
  
  StartAction(ctx, event);
  to = SrcScan(ctx->text.source, ctx->text.insertPos, type, dir, ctx->text.mult, include);
  
  if (dir == XawsdLeft)
    {
    from = to;
    to = ctx->text.insertPos;
    }
  else 
    from = ctx->text.insertPos;

  _DeleteOrKill(ctx, from, to, kill);
  _XawTextSetScrollBars(ctx);
  EndAction(ctx);
  }

static void DeleteForwardChar(w, event)
  Widget w;
  XEvent *event;
  {
  DeleteOrKill((TextWidget) w, event, XawsdRight, XawstPositions, TRUE, FALSE);
  }

static void DeleteBackwardChar(w, event)
Widget w;
XEvent *event;
{
  DeleteOrKill((TextWidget) w, event, XawsdLeft, XawstPositions, TRUE, FALSE);
}

static void DeleteClipsBackwardChar(w, event)
  Widget w;
  XEvent *event;
  {
  TextWidget ctx = (TextWidget)w;
  char *cmdstr,strbuf[2];
  extern Boolean quit_get_event;
  extern char* GetCommandString();

  MoveEndOfFile(w,event);
  strbuf[1] = 0;
  if(CLIPSInputCount == 0)
    return;
  if((!quit_get_event)&&(get_clips_command())&& (!GetManagerList()))
   {
      cmdstr = GetCommandString();
      if((cmdstr != NULL) ? (cmdstr[0] != EOS) :FALSE)
       {
        strbuf[0] = XK_BackSpace;
        ExpandCommandString(strbuf[0]);
      }
   }
  DeleteOrKill(ctx, event, XawsdLeft, XawstPositions, TRUE, FALSE);
  }

static void DeleteForwardWord(w, event)
  Widget w;
  XEvent *event;
  {
  DeleteOrKill((TextWidget) w, event, XawsdRight, XawstWhiteSpace, FALSE, FALSE);
  }

static void DeleteBackwardWord(w, event)
  Widget w;
  XEvent *event;
  {
  DeleteOrKill((TextWidget) w, event, XawsdLeft, XawstWhiteSpace, FALSE, FALSE);
  }

static void KillForwardWord(w, event)
  Widget w;
  XEvent *event;
  {
  DeleteOrKill((TextWidget) w, event, XawsdRight, XawstWhiteSpace, FALSE, TRUE);
  }

static void KillBackwardWord(w, event)
  TextWidget w;
  XEvent *event;
  {
  DeleteOrKill((TextWidget) w, event, XawsdLeft, XawstWhiteSpace, FALSE, TRUE);
  }

static void KillToEndOfLine(w, event)
  Widget w;
  XEvent *event;
  {
  TextWidget ctx = (TextWidget) w;
  XawTextPosition end_of_line;

  StartAction(ctx, event);
  end_of_line = SrcScan(ctx->text.source, ctx->text.insertPos, XawstEOL, XawsdRight, ctx->text.mult, FALSE);
  if (end_of_line == ctx->text.insertPos)
    end_of_line = SrcScan(ctx->text.source, ctx->text.insertPos, XawstEOL, XawsdRight, ctx->text.mult, TRUE);

  _DeleteOrKill(ctx, ctx->text.insertPos, end_of_line, TRUE);
  EndAction(ctx);
  _XawTextSetScrollBars(ctx);
  }

static void KillToEndOfParagraph(w, event)
  Widget w;
  XEvent *event;
  {
  DeleteOrKill((TextWidget) w, event, XawsdRight, XawstParagraph, FALSE, TRUE);
  }

static void KillCurrentSelection(w, event)
  Widget w;
  XEvent *event;
  {
  _XawTextZapSelection( (TextWidget) w, event, TRUE);
  }

void DeleteCurrentSelection(w, event)
  Widget w;
  XEvent *event;
  {
  _XawTextZapSelection( (TextWidget) w, event, FALSE);
  }

/************************************************************
 *
 * Insertion Routines.
 *
 ************************************************************/

static int InsertNewLineAndBackupInternal(ctx)
  TextWidget ctx;
  {
  int count, error = XawEditDone;
  XawTextBlock text;
  char *buf, *ptr;

  ptr = buf = XtMalloc(sizeof(char) * ctx->text.mult);
  for (count = 0; count < ctx->text.mult; count++, ptr++)
    ptr[0] = '\n';

  text.length = ctx->text.mult;
  text.ptr = buf;
  text.firstPos = 0;
  text.format = FMT8BIT;

  if (_XawTextReplace(ctx, ctx->text.insertPos, ctx->text.insertPos, &text))
    {
    XBell( XtDisplay(ctx), 50);
    error = XawEditError;
    }
  else 
    ctx->text.showposition = TRUE;

  XtFree(buf);
  return(error);
  }

static void InsertNewLineAndBackup(w, event)
  Widget w;
  XEvent *event;
  {
  StartAction( (TextWidget) w, event );
  (void) InsertNewLineAndBackupInternal( (TextWidget) w );
  EndAction( (TextWidget) w );
  _XawTextSetScrollBars( (TextWidget) w);
  }

static int LocalInsertNewLine(ctx, event)
TextWidget ctx;
XEvent *event;
{
  StartAction(ctx, event);
  if (InsertNewLineAndBackupInternal(ctx) == XawEditError)
    return(XawEditError);
  ctx->text.insertPos = SrcScan(ctx->text.source, ctx->text.insertPos,
                             XawstPositions, XawsdRight, ctx->text.mult, TRUE);
  EndAction(ctx);
  _XawTextSetScrollBars(ctx);
  return(XawEditDone);
}

static void InsertNewLine(w, event)
Widget w;
XEvent *event;
{
  (void) LocalInsertNewLine( (TextWidget) w, event);
}

int LocalClipsInsertNewLine(ctx, event)
  TextWidget ctx;
  XEvent *event;
  {

  StartAction(ctx, event);
  if (InsertNewLineAndBackupInternal(ctx) == XawEditError)
    return(XawEditError);
  ctx->text.insertPos = SrcScan(ctx->text.source, ctx->text.insertPos, XawstPositions, XawsdRight, ctx->text.mult, TRUE);
  EndAction(ctx);
  _XawTextSetScrollBars(ctx);
  return(XawEditDone);
  }

static void InsertClipsNewLine(w, event)
  Widget w;
  XEvent *event;
  {
  TextWidget ctx = (TextWidget)w;
  char *cmdstr,strbuf[2];
  extern Boolean quit_get_event;
  strbuf[1] = 0;

  MoveEndOfFile(w,event);
  if((!quit_get_event)&&(get_clips_command())&& (!GetManagerList()))
    {
      strbuf[0] = XK_Linefeed;
      ExpandCommandString(strbuf[0]);
      quit_get_event = True;
    }
  (void)LocalClipsInsertNewLine(ctx,event);
  }

static void InsertNewLineAndIndent(w, event)
  Widget w;
  XEvent *event;
  {
  XawTextBlock text;
  XawTextPosition pos1, pos2;
  TextWidget ctx = (TextWidget) w;

  StartAction(ctx, event);
  pos1 = SrcScan(ctx->text.source, ctx->text.insertPos, XawstEOL, XawsdLeft, 1, FALSE);
  pos2 = SrcScan(ctx->text.source, pos1, XawstEOL, XawsdLeft, 1, TRUE);
  pos2 = SrcScan(ctx->text.source, pos2, XawstWhiteSpace, XawsdRight, 1, TRUE);
  text.ptr = _XawTextGetText(ctx, pos1, pos2);
  text.length = strlen(text.ptr);
  if (LocalInsertNewLine(ctx, event)) return;
  text.firstPos = 0;
  if (_XawTextReplace(ctx,ctx->text.insertPos, ctx->text.insertPos, &text))
    {
    XBell(XtDisplay(ctx), 50);
    EndAction(ctx);
    return;
    }
  ctx->text.insertPos = SrcScan(ctx->text.source, ctx->text.insertPos, XawstPositions, XawsdRight, text.length, TRUE);
  XtFree(text.ptr);
  EndAction(ctx);
  _XawTextSetScrollBars(ctx);
  }

/************************************************************
 *
 * Selection Routines.
 *
 *************************************************************/

static void SelectWord(w, event, params, num_params)
  Widget w;
  XEvent *event;
  String *params;
  Cardinal *num_params;
  {
  TextWidget ctx = (TextWidget) w;
  XawTextPosition l, r;

  StartAction(ctx, event);
  l = SrcScan(ctx->text.source, ctx->text.insertPos, XawstWhiteSpace, XawsdLeft, 1, FALSE);
  r = SrcScan(ctx->text.source, l, XawstWhiteSpace, XawsdRight, 1, FALSE);
  _XawTextSetSelection(ctx, l, r, params, *num_params);
  EndAction(ctx);
  }

static void SelectAll(w, event, params, num_params)
  Widget w;
  XEvent *event;
  String *params;
  Cardinal *num_params;
  {
  TextWidget ctx = (TextWidget) w;

  StartAction(ctx, event);
  _XawTextSetSelection(ctx,zeroPosition,ctx->text.lastPos,params,*num_params);
  EndAction(ctx);
  }

static void ModifySelection(ctx, event, mode, action, params, num_params)
  TextWidget ctx;
  XEvent *event;
  XawTextSelectionMode mode;
  XawTextSelectionAction action;
  String *params;		/* unused */
  Cardinal *num_params;	/* unused */
  {
  StartAction(ctx, event);
  _XawTextAlterSelection(ctx, mode, action, params, num_params);
  EndAction(ctx);
  }

static void SelectStart(w, event, params, num_params)
  Widget w;
  XEvent *event;
  String *params;		/* unused */
  Cardinal *num_params;	/* unused */
  {
  TextWidget ctx = (TextWidget)w;
  ModifySelection((TextWidget) w, event, XawsmTextSelect, XawactionStart, params, num_params);
  }

static void SelectAdjust(w, event, params, num_params)
  Widget w;
  XEvent *event;
  String *params;		/* unused */
  Cardinal *num_params;	/* unused */
  {
  ModifySelection((TextWidget) w, event, XawsmTextSelect, XawactionAdjust, params, num_params);
  }

static void SelectEnd(w, event, params, num_params)
  Widget w;
  XEvent *event;
  String *params;
  Cardinal *num_params;
  {
  ModifySelection((TextWidget) w, event, XawsmTextSelect, XawactionEnd, params, num_params);
  }

static void ExtendStart(w, event, params, num_params)
  Widget w;
  XEvent *event;
  String *params;		/* unused */
  Cardinal *num_params;	/* unused */
  {
  ModifySelection((TextWidget) w, event, XawsmTextExtend, XawactionStart, params, num_params);
  }

static void ExtendAdjust(w, event, params, num_params)
  Widget w;
  XEvent *event;
  String *params;		/* unused */
  Cardinal *num_params;	/* unused */
  {
  ModifySelection((TextWidget) w, event, XawsmTextExtend, XawactionAdjust, params, num_params);
  }

static void ExtendEnd(w, event, params, num_params)
  TextWidget w;
  XEvent *event;
  String *params;
  Cardinal *num_params;
  {
  ModifySelection((TextWidget) w, event, XawsmTextExtend, XawactionEnd, params, num_params);
  }

/************************************************************
 *
 * Misc. Routines.
 *
 ************************************************************/

static void RedrawDisplay(w, event)
  Widget w;
  XEvent *event;
  {
  StartAction( (TextWidget) w, event);
  _XawTextClearAndCenterDisplay((TextWidget) w);
  EndAction( (TextWidget) w);
  }

/*ARGSUSED*/
static void TextFocusIn (w, event)
  TextWidget w;
  XEvent *event;
  {
  TextWidget ctx = (TextWidget) w;

  ctx->text.hasfocus = TRUE; 
  }

/*ARGSUSED*/
static void TextFocusOut(w, event)
  TextWidget w;
  XEvent *event;
  {
  TextWidget ctx = (TextWidget) w;

  ctx->text.hasfocus = FALSE;
  }

static XComposeStatus compose_status = {NULL, 0};

/*	Function Name: AutoFill
 *	Description: Breaks the line at the previous word boundry when
 *                   called inside InsertChar.
 *	Arguments: ctx - The text widget.
 *	Returns: none
 */

static void AutoFill(ctx)
  TextWidget ctx;
  {
  int width, height, x, line_num, max_width;
  XawTextPosition ret_pos;
  XawTextBlock text;

  if ( !((ctx->text.auto_fill) && (ctx->text.mult == 1)) )
    return;

  for ( line_num = 0; line_num < ctx->text.lt.lines ; line_num++)
    if ( ctx->text.lt.info[line_num].position >= ctx->text.insertPos )
      break;
  line_num--;			/* backup a line. */

  max_width = Max(0, (unsigned int)ctx->core.width - HMargins(ctx));

  x = ctx->text.margin.left;
  XawTextSinkFindPosition( ctx->text.sink,ctx->text.lt.info[line_num].position, x, max_width, TRUE, &ret_pos, &width, &height);
  
  if ( ret_pos >= ctx->text.insertPos )
    return;
  
  text.ptr = "\n";
  text.length = 1;
  text.firstPos = 0;
  text.format = FMT8BIT;

  _XawTextReplace(ctx, ret_pos - 1, ret_pos, &text);
  }


static void InsertChar(w, event)
Widget w;
XEvent *event;
{
  TextWidget ctx = (TextWidget) w;
  char *ptr, strbuf[BUFSIZ];
  int count, error;
  KeySym keysym;
  XawTextBlock text;

  if ( (text.length = XLookupString (&event->xkey, strbuf, BUFSIZ,
                               &keysym, &compose_status)) == 0) {
    return;
  }

  text.ptr = ptr = XtMalloc(sizeof(char) * text.length * ctx->text.mult);
  for (count = 0 ; count < ctx->text.mult ; count++) {
    strncpy(ptr, strbuf, text.length);
    ptr += text.length;
  }

  text.length = text.length * ctx->text.mult;
  text.firstPos = 0;
  text.format = FMT8BIT;

  StartAction(ctx, event);

  error = _XawTextReplace(ctx, ctx->text.insertPos,ctx->text.insertPos, &text);

  if (error == XawEditDone) {
    ctx->text.insertPos =
      SrcScan(ctx->text.source, ctx->text.insertPos,
              XawstPositions, XawsdRight, text.length, TRUE);
    AutoFill(ctx);
  }
  else
    XBell(XtDisplay(ctx), 50);

  XtFree(text.ptr);
  EndAction(ctx);
  _XawTextSetScrollBars(ctx);
}

static void InsertClipsChar(w, event)
  Widget w;
  XEvent *event;
  {
  TextWidget ctx = (TextWidget) w;
  char *ptr, strbuf[BUFSIZ];
  int count, error;
  KeySym keysym;
  XawTextBlock text;
  extern Boolean quit_get_event;
  char *cmdstr;
  
  MoveEndOfFile(w, event);
  if ( (text.length = XLookupString (&event->xkey, strbuf, BUFSIZ, &keysym, &compose_status)) == 0)
    return;
  if((!quit_get_event)&&(get_clips_command())&& (!GetManagerList()))
   {
        strbuf[1] = 0;
        if((keysym>= XK_space) && (keysym<= XK_asciitilde))
          {
          ExpandCommandString(strbuf[0]);
          }
    }
  else
    return;
  text.ptr = ptr = XtMalloc(sizeof(char) * text.length * ctx->text.mult);
  for (count = 0 ; count < ctx->text.mult ; count++)
    {
    strncpy(ptr, strbuf, text.length);
    ptr += text.length;
    }

  text.length = text.length * ctx->text.mult;
  text.firstPos = 0;
  text.format = FMT8BIT;
  
  StartAction(ctx, event);
  
  error = _XawTextReplace(ctx, ctx->text.insertPos,ctx->text.insertPos, &text);

  if (error == XawEditDone)
    {
    ctx->text.insertPos = SrcScan(ctx->text.source, ctx->text.insertPos, XawstPositions, XawsdRight, text.length, TRUE);
    AutoFill(ctx);
    }
  else 
    XBell(XtDisplay(ctx), 50);

  XtFree(text.ptr);
  EndAction(ctx);
  _XawTextSetScrollBars(ctx);
  }

/*ARGSUSED*/
static void InsertString(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
  TextWidget ctx = (TextWidget) w;
  XawTextBlock text;
  int      i;

  text.firstPos = 0;
  StartAction(ctx, event);
  for (i = *num_params; i; i--, params++) {
    unsigned char hexval;
    if ((*params)[0] == '0' && (*params)[1] == 'x' && (*params)[2] != '\0') {
      char c, *p;
      hexval = 0;
      for (p = *params+2; (c = *p); p++) {
        hexval *= 16;
        if (c >= '0' && c <= '9')
          hexval += c - '0';
        else if (c >= 'a' && c <= 'f')
          hexval += c - 'a' + 10;
        else if (c >= 'A' && c <= 'F')
          hexval += c - 'A' + 10;
        else break;
      }
      if (c == '\0') {
        text.ptr = (char*)&hexval;
        text.length = 1;
      } else text.length = strlen(text.ptr = *params);
    } else text.length = strlen(text.ptr = *params);
    if (text.length == 0) continue;
    if (_XawTextReplace(ctx, ctx->text.insertPos,
                        ctx->text.insertPos, &text)) {
      XBell(XtDisplay(ctx), 50);
      EndAction(ctx);
      return;
    }
    ctx->text.insertPos =
      SrcScan(ctx->text.source, ctx->text.insertPos,
              XawstPositions, XawsdRight, text.length, TRUE);
  }
  EndAction(ctx);
}

/*ARGSUSED*/
void InsertClipsString(w, event, params, num_params)
  Widget w; 
  XEvent *event;
  String *params;
  Cardinal *num_params;
  {
  TextWidget ctx = (TextWidget) w;
  XawTextBlock text;
  int	   i;
  
  text.firstPos = 0;
  StartAction(ctx, event);
  for (i = *num_params; i; i--, params++) 
    {
    unsigned char hexval;
    if ((*params)[0] == '0' && (*params)[1] == 'x' && (*params)[2] != '\0') 
      {
      char c, *p;
      hexval = 0;
      for (p = *params+2; (c = *p); p++) 
        {
        hexval *= 16;
        if (c >= '0' && c <= '9')
          hexval += c - '0';
        else if (c >= 'a' && c <= 'f')
          hexval += c - 'a' + 10;
        else if (c >= 'A' && c <= 'F')
          hexval += c - 'A' + 10;
        else break;
        }
      if (c == '\0') 
        {
        text.ptr = (char*)&hexval;
        text.length = 1;
        } 
      else 
        text.length = strlen(text.ptr = *params);
      } 
    else 
      text.length = strlen(text.ptr = *params);
    if (text.length == 0) 
      continue;
    if (_XawTextReplace(ctx, ctx->text.insertPos, ctx->text.insertPos, &text)) 
      {
      XBell(XtDisplay(ctx), 50);
      EndAction(ctx);
      return;
      }
    ctx->text.insertPos = SrcScan(ctx->text.source, ctx->text.insertPos, XawstPositions, XawsdRight, text.length, TRUE);
    }
  EndAction(ctx);

  }

static void DisplayCaret(w, event, params, num_params)
  Widget w;
  XEvent *event;		/* CrossingNotify special-cased */
  String *params;		/* Off, False, No, On, True, Yes, etc. */
  Cardinal *num_params;	/* 0, 1 or 2 */
  {
  TextWidget ctx = (TextWidget)w;
  Boolean display_caret = True;

  if (event->type == EnterNotify || event->type == LeaveNotify)
    {
    /* for Crossing events, the default case is to check the focus
     * field and only change the caret when focus==True.  A second
     * argument of "always" will cause the focus field to be ignored.
     */
    Boolean check_focus = True;
    if (*num_params == 2 && strcmp(params[1], "always") == 0)
      check_focus = False;
    if (check_focus && !event->xcrossing.focus)
      return;
    }

  if (*num_params > 0)  /* default arg is "True" */
    {
    XrmValue from, to;
    from.size = strlen(from.addr = params[0]);
    XtConvert(w, XtRString, &from, XtRBoolean, &to);
    if (to.addr != NULL) display_caret = *(Boolean*)to.addr;
    if (ctx->text.display_caret == display_caret) return;
    }
  StartAction(ctx, event);
  ctx->text.display_caret = display_caret;
  EndAction(ctx);
  }

/*	Function Name: Multiply
 *	Description: Multiplies the current action by the number passed.
 *	Arguments: w - the text widget.
 *                 event - ** NOT USED **.
 *                 params, num_params - The parameter list, see below.
 *	Returns: none.
 *
 * Parameter list;
 *  
 * The parameter list may contain either a number or the string 'Reset'.
 * 
 * A number will multiply the current multiplication factor by that number.
 * Many of the text widget actions will will perform n actions, where n is
 * the multiplication factor.
 *
 * The string reset will reset the mutiplication factor to 1.
 * 
 */

static void Multiply(w, event, params, num_params)
  Widget w;
  XEvent *event;
  String * params;
  Cardinal * num_params;
  {
  TextWidget ctx = (TextWidget) w;
  int mult;

  if (*num_params != 1)
    {
    XtAppError(XtWidgetToApplicationContext(w), "The multiply action takes exactly one argument.");
    XBell(XtDisplay(w), 0);
    return;
    }

  if ( (params[0][0] == 'r') || (params[0][0] == 'R') )
    {
    XBell(XtDisplay(w), 0);
    ctx->text.mult = 1;
    return;
    }

  if ( (mult = atoi(params[0])) == 0 )
    {
    char buf[BUFSIZ];
    sprintf(buf, "%s %s", "Text Widget: The multiply action's argument", "must be a number greater than zero, or 'Reset'.");
    XtAppError(XtWidgetToApplicationContext(w), buf);
    XBell(XtDisplay(w), 0);
    return;
    }

  ctx->text.mult *= mult;
  }

/*	Function Name: StripOutOldCRs
 *	Description: strips out the old carrige returns.
 *	Arguments: ctx - the text widget.
 *                 from - starting point.
 *                 to - the ending point
 *	Returns: the new ending location (we may add some caracters).
 */

static XawTextPosition StripOutOldCRs(ctx, from, to)
  TextWidget ctx;
  XawTextPosition from, to;
  {
  XawTextPosition startPos, endPos, eop_begin, eop_end, temp;
  Widget src = ctx->text.source;
  XawTextBlock text;
  char *buf;

  text.ptr= "  ";
  text.firstPos = 0;
  text.format = FMT8BIT;
   
/*
 * Strip out CR's. 
 */

  eop_begin = eop_end = startPos = endPos = from;
  while (TRUE)
    {
    endPos=SrcScan(src, startPos, XawstEOL, XawsdRight, 1, FALSE);

    temp=SrcScan(src, endPos, XawstWhiteSpace, XawsdLeft, 1, FALSE);
    temp=SrcScan(src, temp, XawstWhiteSpace, XawsdRight, 1, FALSE);
    if (temp > startPos)
      endPos = temp;

    if (endPos >= to)
      break;

    if (endPos >= eop_begin)
      {
      startPos = eop_end;
      eop_begin = SrcScan(src, startPos, XawstParagraph, XawsdRight, 1, FALSE);
      eop_end = SrcScan(src, startPos, XawstParagraph, XawsdRight, 1, TRUE);
      }
    else
      {
      XawTextPosition periodPos, next_word;
      int i, len;

      periodPos= SrcScan(src, endPos, XawstPositions, XawsdLeft, 1, TRUE);
      next_word = SrcScan(src, endPos, XawstWhiteSpace, XawsdRight, 1, FALSE);

      len = next_word - periodPos;

      text.length = 1;
      buf = _XawTextGetText(ctx, periodPos, next_word);
      if ( (periodPos < endPos) && (buf[0] == '.') )
        text.length++;	/* Put in two spaces. */

      /*
       * Remove all extra spaces. 
       */

      for (i = 1 ; i < len; i++) 
        if ( !isspace(buf[i]) || ((periodPos + i) >= to) )
	  break;
      
      XtFree(buf);

      to -= (i - text.length - 1);
      startPos = SrcScan(src, periodPos, XawstPositions, XawsdRight, i, TRUE);
      _XawTextReplace(ctx, endPos, startPos, &text);
      startPos -= i - text.length;
      }
    }
  return(to);
  }

/*	Function Name: InsertNewCRs
 *	Description: Inserts the new Carrige Returns.
 *	Arguments: ctx - the text widget.
 *                 from, to - the ends of the region.
 *	Returns: none
 */

static void InsertNewCRs(ctx, from, to)
  TextWidget ctx;
  XawTextPosition from, to;
  {
  XawTextPosition startPos, endPos, space, eol;
  XawTextBlock text;
  int i, width, height, len;
  char * buf;

  text.ptr = "\n";
  text.length = 1;
  text.firstPos = 0;
  text.format = FMT8BIT;

  startPos = from;
  while (TRUE)
    {
    XawTextSinkFindPosition( ctx->text.sink, startPos, (int) ctx->text.margin.left, (int) (ctx->core.width - HMargins(ctx)), TRUE, &eol, &width, &height);
    if (eol >= to)
      break;

    eol = SrcScan(ctx->text.source, eol, XawstPositions, XawsdLeft, 1, TRUE);
    space= SrcScan(ctx->text.source, eol, XawstWhiteSpace, XawsdRight, 1,TRUE);
    
    startPos = endPos = eol;
    if (eol == space) 
      return;

    len = (int) (space - eol);
    buf = _XawTextGetText(ctx, eol, space);
    for ( i = 0 ; i < len ; i++)
      if (!isspace(buf[i]))
	break;

    to -= (i - 1);
    endPos = SrcScan(ctx->text.source, endPos, XawstPositions, XawsdRight, i, TRUE);
    XtFree(buf);
    
    _XawTextReplace(ctx, startPos, endPos, &text);
    startPos = SrcScan(ctx->text.source, startPos, XawstPositions, XawsdRight, 1, TRUE);
    }
  }  
  
/*	Function Name: FormRegion
 *	Description: Forms up the region specified.
 *	Arguments: ctx - the text widget.
 *                 from, to - the ends of the region.
 *	Returns: none.
 */

static void FormRegion(ctx, from, to)
  TextWidget ctx;
  XawTextPosition from, to;
  {
  if (from >= to) return;

  to = StripOutOldCRs(ctx, from, to);
  InsertNewCRs(ctx, from, to);
  _XawTextBuildLineTable(ctx, ctx->text.lt.top, TRUE);
  }

/*	Function Name: FromParagraph.
 *	Description: reforms up the current paragraph.
 *	Arguments: w - the text widget.
 *                 event - the X event.
 *                 params, num_params *** NOT USED ***.
 *	Returns: none
 */

static void FormParagraph(w, event, params, num_params)
  Widget w;
  XEvent *event;
  String * params;
  Cardinal * num_params;
  {
  TextWidget ctx = (TextWidget) w;
  XawTextPosition from, to;

  StartAction(ctx, event);

  from =  SrcScan(ctx->text.source, ctx->text.insertPos, XawstParagraph, XawsdLeft, 1, FALSE);
  to  =  SrcScan(ctx->text.source, from, XawstParagraph, XawsdRight, 1, FALSE);

  FormRegion(ctx, from, to);
  EndAction(ctx);
  _XawTextSetScrollBars(ctx);
  }

/*	Function Name: TransposeCharacters
 *	Description: Swaps the character to the left of the mark with
 *                   the character to the right of the mark.
 *	Arguments: w - the text widget.
 *                 event - the event that cause this action.
 *                 params, num_params *** NOT USED ***.
 *	Returns: none.
 */
	     
static void TransposeCharacters(w, event, params, num_params)
  Widget w;
  XEvent *event;
  String * params;
  Cardinal * num_params;
  {
  TextWidget ctx = (TextWidget) w;
  XawTextPosition start, end;
  XawTextBlock text;
  unsigned char * buf, c;
  int i;

  StartAction(ctx, event);

/*
 * Get bounds. 
 */

  start = SrcScan(ctx->text.source, ctx->text.insertPos, XawstPositions, XawsdLeft, 1, TRUE);
  end = SrcScan(ctx->text.source, ctx->text.insertPos, XawstPositions, XawsdRight, ctx->text.mult, TRUE);

  if ( (start == ctx->text.insertPos) || (end == ctx->text.insertPos) ) 
    XBell(XtDisplay(w), 0);	/* complain. */
  else
    {
    ctx->text.insertPos = end;

    /*
     * Retrieve text and swap the characters. 
     */
    
    buf = (unsigned char *) _XawTextGetText(ctx, start, end);
    text.length = strlen((char *)buf);
    text.firstPos = 0;
    text.format = FMT8BIT;
    
    c = buf[0];
    for (i = 1 ; i < text.length ; i++)
      buf[i - 1] = buf[i];
    buf[i - 1] = c;
    
    /* 
     * Store new text is source.
     */
    
    text.ptr = (char *) buf;
    _XawTextReplace (ctx, start, end, &text);
    
    XtFree(buf);
    }
  EndAction(ctx);
  }

/*	Function Name: NoOp
 *	Description: This action performs no action, and allows
 *                   the user or application programmer to unbind a 
 *                   translation.
 *	Arguments: w - the text widget.
 *                 event - *** UNUSED ***.
 *                 parms and num_params - the action parameters.
 *	Returns: none.
 *
 * Note: If the parameter list contains the string "RingBell" then
 *       this action will ring the bell.
 */

static void NoOp(w, event, params, num_params)
  Widget w;
  XEvent *event;
  String *params;
  Cardinal *num_params;
  {
  if (*num_params != 1)
    return;

  switch(params[0][0])
    {
    case 'R':
    case 'r': XBell(XtDisplay(w), 0);
    default: break;
    }
  }
	
/* CLIPS key bound functions for the menus */

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/
static void Clear_CLIPS(w,event)
  Widget w;
  XEvent *event;
  {
  ClearCLIPSCallback(w, (XtPointer)NULL, (XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void IntReset(w, event)
  Widget w;
  XEvent *event;
  {
  ResetCallback(w, (XtPointer)NULL, (XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void FactsWindow(w, event)
  Widget w;
  XEvent *event;
  {
  FactsWindowCallback(w, (XtPointer)NULL, (XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void AgendaWindow(w, event)
  Widget w;
  XEvent *event;
  {
  AgendaWindowCallback(w, (XtPointer)NULL, (XtPointer)NULL);
  }
  
/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void LoadRules(w, event)
  Widget w;
  XEvent *event;
  {
  LoadRulesCallback(w, (XtPointer)NULL, (XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void Dribble(w, event)
  Widget w;
  XEvent *event;
  {
  DribbleCallback(w, (XtPointer)NULL, (XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void Quit(w, event)
  Widget w;
  XEvent *event;
  {
  QuitCallback(w, (XtPointer)NULL, (XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void IntRun(w, event)
  Widget w;
  XEvent *event;
  {
  RunCallback(w, (XtPointer)NULL, (XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void SaveRules(w, event)
  Widget w;
  XEvent *event;
  {
  SaveRulesCallback(w, (XtPointer)NULL, (XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void Step(w, event)
  Widget w;
  XEvent *event;
  {
  StepCallback(w, (XtPointer)NULL, (XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void Edit(w, event)
  Widget w;
  XEvent *event;
  {
  EditCallback(w, (XtPointer)NULL, (XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void CommandLineCLIPS(w, event)
  Widget w;
  XEvent *event;
  {
  CommandLineCLIPSCallback(w, (XtPointer)NULL, (XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void FindBalance(w,event)
  Widget w;
  XEvent *event;
  {
    FindMatchingParenthesisCallback(w,(XtPointer)w,(XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void CompleteConstructInDialog(w,event)
  Widget w;
  XEvent *event;
  {
    /* next 8 lines added to fix?? cntrl-c problem */
    extern Boolean quit_get_event;
  
    if (quit_get_event == TRUE)
      {
       SetHaltExecution(CLIPS_TRUE);
       CloseAllBatchSources();
      }
    else
       CompletionDialogCallback(w,NULL,(XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void CompleteConstructInEditor(w,event)
  Widget w;
  XEvent *event;
  {
       CompletionEditCallback(w,(XtPointer)w,(XtPointer)NULL);
  }


/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void ClearScreen(w,event)
  Widget w;
  XEvent *event;
  {
    ClearScreenCallback(w,(XtPointer)w,(XtPointer)NULL);
  }

/*****************************************************************************
*
        Name:           
        Description:
        Arguments:
        Return:
******************************************************************************
*/

static void StopExecution(w,event)
  Widget w;
  XEvent *event;
  {
    extern Boolean periodicChecking;

    if(periodicChecking)
     {
      SetHaltExecution(CLIPS_TRUE);
      CloseAllFiles();
     }
  }

/*****************************************************************************
*  Action Table
******************************************************************************
*/


XtActionsRec ClipsTxtActsTable[] =
  {
/* delete bindings */
  {"delete-clips-previous-character",  (XtActionProc)DeleteClipsBackwardChar},
/* new line stuff */
  {"Clipsnewline",              (XtActionProc)InsertClipsNewLine},
/* Selection stuff */
  {"insert-clips-selection",    (XtActionProc)InsertClipsSelection},
/* Miscellaneous */
  {"insert-clips-string",       (XtActionProc)InsertClipsString},
  {"insert-clips-char",         (XtActionProc)InsertClipsChar},
/* CLIPS Dialog Window key bindings for menus */
  {"clear-clips",               (XtActionProc)Clear_CLIPS},
  {"reset",                     (XtActionProc)IntReset},
  {"facts-window",              (XtActionProc)FactsWindow},
  {"agenda-window",             (XtActionProc)AgendaWindow},
  {"load-constructs",           (XtActionProc)LoadRules},
  {"dribble",                   (XtActionProc)Dribble},
  {"quit",                      (XtActionProc)Quit},
  {"run",                       (XtActionProc)IntRun},
  {"save-rules",                (XtActionProc)SaveRules},
  {"step",                      (XtActionProc)Step},
  {"edit",                      (XtActionProc)Edit},
  {"command-line-clips",        (XtActionProc)CommandLineCLIPS},
  {"balance",                   (XtActionProc)FindBalance},
  {"clear-screen",              (XtActionProc)ClearScreen},
  {"complete-construct-dialog", (XtActionProc)CompleteConstructInDialog},
  {"complete-construct-editor", (XtActionProc)CompleteConstructInEditor},
  {"stop-execution",            (XtActionProc)StopExecution},
  };

Cardinal ClipsTxtActsTableCount = XtNumber(ClipsTxtActsTable);

