/********************************* xmenu_exec.c *******************************/
/*                                                                            */
/******************************************************************************/
/*                                                                            */
/*        Author:  BeBe Ly - NASA/Johnson Space Center                        */
/*                 Daniel J. McCoy - University of Houston - Downtown         */
/*                                                                            */
/******************************************************************************/

#include <stdio.h>
#include "xsetup.h"

/********** local functions visible outside this file **********/
void ResetCallback();
void RunCallback();
void StepCallback();
void ClearCLIPSCallback();

/********** local functions not visible outside this file **********/
static void ResetClips();
static void ClearClips();

/********** external functions from xmenu.c **********/
extern void CancelPopupSelect();

extern Widget toplevel, dialog_text;
extern Boolean quit_get_event;
extern Pixmap clips_logo;
extern XEvent event;                      /* Current event */
extern Arg args[10];
extern int EvaluatingTopLevelCommand;
extern Widget ExecItemWidgets[5];

/*******************************************************************************
          Name:        ResetCallback
          Description: Called when Reset is selected form Execution menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void ResetCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  MoveEndOfFile(dialog_text, &event);
  if(GetNextFact(NULL))
    {
    Widget confirmshell, confirm;

    confirmshell = XtCreatePopupShell("Confirmation",
                                      topLevelShellWidgetClass,
                                      toplevel,
                                      NULL, 0);

    XtSetArg(args[0], XtNlabel, "The fact list\nis not empty!");
    XtSetArg(args[1], XtNicon, clips_logo);
    confirm = XtCreateManagedWidget("confirm",
                                    dialogWidgetClass,
                                    confirmshell, args, 2);

    XawDialogAddButton(confirm, "Reset", ResetClips, (XtPointer) confirm);
    XawDialogAddButton(confirm, "Cancel", CancelPopupSelect,
                       (XtPointer) confirm);

    XtPopup(confirmshell, XtGrabNonexclusive);
    }

  else
    {
    PrintCLIPS("wclips", "(reset)\n");
    SetCommandString("(reset)\n");

    /* ============================================ */
    /*  Set this flag to True to break out of the   */
    /*  event loop so CLIPS could process the       */
    /*  command.                                    */
    /* ============================================ */
 
    quit_get_event = True;
    }
  }

/*******************************************************************************
          Name:        RunCallback
          Description: Called when Run is selected form Execution menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void RunCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
/*  if(EvaluatingTopLevelCommand)
   return;*/
  MoveEndOfFile(dialog_text, &event);
  PrintCLIPS("wclips", "(run)\n");
  SetCommandString("(run)\n");
    /* ============================================ */
    /*  Set this flag to True to break out of the   */
    /*  event loop so CLIPS could process the       */ 
    /*  command.                                    */
    /* ============================================ */

  quit_get_event = True;
 
  }

/*******************************************************************************
          Name:        StepCallback
          Description: Called when Step is selected form Execution menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void StepCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
/*  if(EvaluatingTopLevelCommand)
   return;*/
  MoveEndOfFile(dialog_text, &event);
  PrintCLIPS("wclips", "(run 1)\n");
  SetCommandString("(run 1)\n");
    /* ============================================ */
    /*  Set this flag to True to break out of the   */
    /*  event loop so CLIPS could process the       */ 
    /*  command.                                    */
    /* ============================================ */

   quit_get_event = True;
  }

/*******************************************************************************
          Name:        ClearCLIPSCallback
          Description: Called when Clear CLIPS is selected form Execution menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void ClearCLIPSCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget confirmshell, confirm;

/*  if(EvaluatingTopLevelCommand)
   return;*/
  confirmshell = XtCreatePopupShell("Confirmation",
                                    topLevelShellWidgetClass,
                                    toplevel,
                                    NULL, 0);

  XtSetArg(args[0], XtNlabel, "Clear CLIPS!\nAre you sure?");
  XtSetArg(args[1], XtNicon, clips_logo);
  confirm = XtCreateManagedWidget("confirm",
                                  dialogWidgetClass,
                                  confirmshell,
                                  args, 2);

  XawDialogAddButton(confirm, "Clear", ClearClips, (XtPointer) confirm);
  XawDialogAddButton(confirm, "Cancel", CancelPopupSelect, (XtPointer) confirm);

  XtPopup(confirmshell, XtGrabNonexclusive);

    /* ============================================ */
    /*  Set this flag to True to break out of the   */
    /*  event loop so CLIPS could process the       */ 
    /*  command.                                    */
    /* ============================================ */

  quit_get_event = True;
  }

/*******************************************************************************
          Name:        ResetClips
          Description: Calls the command `reset' in CLIPS
          Arguments:  w - Not Used
                       client_data - Child of widget to destroy
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
static void ResetClips(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  XtDestroyWidget(XtParent((Widget) client_data));
  PrintCLIPS("wclips","(reset)\n");
  SetCommandString("(reset)\n");
    /* ============================================ */
    /*  Set this flag to True to break out of the   */
    /*  event loop so CLIPS could process the       */ 
    /*  command.                                    */
    /* ============================================ */

  quit_get_event = True;
  }

/*******************************************************************************
          Name:        ClearClips
          Description: Calls the `clear' command in CLIPS
          Arguments:  w - Not Used
                       client_data - Not Used
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
static void ClearClips(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  XtDestroyWidget(XtParent((Widget) client_data));
  PrintCLIPS("wclips","(clear)\n");
  SetCommandString("(clear)\n");
    /* ============================================ */
    /*  Set this flag to True to break out of the   */
    /*  event loop so CLIPS could process the       */ 
    /*  command.                                    */
    /* ============================================ */

  quit_get_event = True;
  }

/*******************************************************************************
 *  ClearScreenCallback
 *  Description: is called when a clear screen is requested
 *  Input : unused
 *******************************************************************************/
void ClearScreenCallback(w,client_data,call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
    int n = 0;
    
    XtSetArg(args[n],XtNstring,"");n++;
    XtSetValues(dialog_text,args,n);
  }

