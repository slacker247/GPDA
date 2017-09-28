/******************************** xmenu_opt.c ********************************/
/*                                                                           */
/*****************************************************************************/
/*                                                                           */
/*        Author:  BeBe Ly - NASA/Johnson Space Center                       */
/*                 Daniel J. McCoy - University of Houston - Downtown        */
/*                                                                           */
/*****************************************************************************/

#include <stdio.h>
#include "xsetup.h"
#include "xclips.h"
#include "setup.h"
#include "engine.h"
#include "crstrtgy.h"

/********** local functions visible outside this file **********/
void OptionsWindow();
void SetStrategyCallback();
void SetSalienceCallback();
void OkayOptionsCallback();

/********** from CLIPS code **********/
extern int SetCommandString();
extern int PrintCLIPS();
extern int GetFactDuplication();
extern int SetFactDuplication();
extern int GetDynamicDeftemplateChecking();
extern int SetDynamicDeftemplateChecking();
extern int GetIncrementalReset();
extern int SetIncrementalReset();

/********* from xmenu.c **********/
extern void PopdownSelect();

/********** Global variables **********/
extern Widget dialog_text;         /* the standard IO (main CLIPS window) */
extern Widget toplevel;

extern XEvent event;              /* Current event */
extern Boolean quit_get_event;    /* Flag to break out of the get event loop*/
extern Pixmap checker;
extern Arg args[10];



Widget optionsShell = NULL,optionsForm = NULL;
Widget option_widgets[7];
Widget strategy_widgets[7];
Widget sal_opt_widgets[3];
int optionFlags[2] = {DEPTH_STRATEGY,WHEN_DEFINED};


/*******************************************************************************
          Name:        OptionsWindow
          Description: Creates the Options menu
          arguements:  w - Widget that invokes the callbaack function
                       client,data - unused
          Returns:     None
*******************************************************************************/
void OptionsWindow(w,client,data)
  Widget w;
  XtPointer client,data;
  {
    Widget Cancel,Okay,menu,entry,salienceEval,strategy;
    int n,i;
    static char *optionList1[7] = {"Static Constraint Checking",
                                   "Dynamic Constraint Checking",
                                   "Reset Global Variables",
                                   "Sequence Expansion Operator Recognition",
                                   "Incremental Reset",
                                   "Auto-Float Dividend",
                                   "Fact Duplication"};

    static char *optionList2[7] = {"Depth","Breadth","LEX","MEA","Complexity","Simplicity","Random"};
    static char *optionList3[3] = {"When Defined","When Activated","Every Cycle"};
    Pixmap SalienceBM,StrategyBM;

  /* =============================================== */
  /*    If optionsShell exists pop it up             */
  /* =============================================== */

    if(optionsShell != NULL)
     {
       UpdateOptionsMenu();
       XtPopup(optionsShell,XtGrabNonexclusive);
       return;
     }
  /* ================================================ */
  /*  If optionShell not available create one.        */
  /*  The followings are the widget classes used in   */
  /*  building the option menu                        */
  /*     topLevelShellWidgetClass                     */
  /*        formWidgetClass                           */
  /*          menuButtonWidgetClass                   */
  /*            simpleMenuWidgetClass                 */
  /*             smeBSBOjectClass                     */
  /* ================================================ */
    n = 0;
    XtSetArg( args[n], XtNwidth,250);n++;
    XtSetArg( args[n], XtNheight,400);n++;
    optionsShell = XtCreatePopupShell("Execution Options",topLevelShellWidgetClass,XtParent(
w),NULL,0);

  /* ================================================ */
  /*  Create the outside form for the option menu     */
  /* ================================================ */

    optionsForm = XtCreateManagedWidget( "watch_form", formWidgetClass,
                                        optionsShell, args,n);
    SalienceBM = XCreateBitmapFromData(XtDisplay(toplevel),
                                  RootWindowOfScreen(XtScreen(toplevel)),
                                  salience_bits,
                                  salience_width,
                                  salience_height);

  /* ================================================ */
  /*   Create the salience Evaluation setting menu    */
  /* ================================================ */

    n = 0;
    XtSetArg(args[n],XtNbitmap,SalienceBM);n++;
    XtSetArg(args[n],XtNwidth,180);n++;
    salienceEval = XtCreateManagedWidget("salienceEvaluation",
                                 menuButtonWidgetClass,
                                 optionsForm,
                                 args, n);
    menu = XtCreatePopupShell("menu",
                                simpleMenuWidgetClass,
                                salienceEval,
                                NULL,0);
    n = 0;
    XtSetArg(args[n],XtNleftMargin,15);n++;
    XtSetArg(args[n],XtNleftBitmap,checker);n++;
    for(i = 0; i < 3 ; i++)
      {
       sal_opt_widgets[i] = XtCreateManagedWidget(optionList3[i],
                                smeBSBObjectClass,
                                menu,
                                args, n);
       XtAddCallback(sal_opt_widgets[i],XtNcallback,SetSalienceCallback,(XtPointer)i);
       n = 1;
     }

  /* ================================================= */
  /*   Create the strategy setting menu                */
  /* ================================================= */

    StrategyBM = XCreateBitmapFromData(XtDisplay(toplevel),
                                  RootWindowOfScreen(XtScreen(toplevel)),
                                  strategy_bits,
                                  strategy_width,
                                  strategy_height);
    n = 0;
    XtSetArg(args[n],XtNhorizDistance,10);n++;
    XtSetArg(args[n],XtNfromHoriz,salienceEval);n++;
    XtSetArg(args[n],XtNbitmap,StrategyBM);n++;
    XtSetArg(args[n],XtNwidth,150);n++;
    strategy = XtCreateManagedWidget("strategy",
                                 menuButtonWidgetClass,
                                 optionsForm,
                                 args, n);
    menu = XtCreatePopupShell("menu",
                            simpleMenuWidgetClass,
                            strategy,
                            NULL, 0);
    n = 0;
    XtSetArg(args[n],XtNleftMargin, 15);n++;
    XtSetArg(args[n], XtNleftBitmap, checker);n++;
    for(i = 0; i < 7; i++)
     {
       strategy_widgets[i] = XtCreateManagedWidget(optionList2[i],
                                smeBSBObjectClass,
                                menu,
                                args, n);
       XtAddCallback(strategy_widgets[i],XtNcallback,SetStrategyCallback,(XtPointer)i);
       n = 1;
     }
  /* ========================================== */
  /*   Create the rest of the option menu       */
  /* ========================================== */

    n = 0;
    XtSetArg(args[n],XtNwidth,200);n++;
    XtSetArg(args[n],XtNhorizDistance,80);n++;
    XtSetArg(args[n],XtNvertDistance,5);n++;
    XtSetArg(args[n],XtNfromVert,salienceEval);n++;
    for(i = 0;i < 7; i++)
     {
        if((i == INT_STA_CONSTRAINT_CHK) || ( i == INT_AUTO_FLOAT_DIV) ||
           (i == INT_INCREMENTAL_RESET) || ( i == INT_RESET_GLOBALS))
          {
            XtSetArg(args[n], XtNstate,True);n++;
          }
        option_widgets[i] = XtCreateManagedWidget(optionList1[i],
                                      toggleWidgetClass,
                                      optionsForm,
                                      args, n);
        n = 3;
        XtSetArg(args[n],XtNfromVert,option_widgets[i]);n++;
     }
   /* ====================================== */
   /*  Create the "Cancel" button            */
   /* ====================================== */

    n = 0;
    XtSetArg(args[n],XtNshapeStyle,XmuShapeRoundedRectangle);n++;
    XtSetArg(args[n],XtNwidth,150);n++;
    XtSetArg(args[n],XtNfromVert,option_widgets[6]);n++;
    XtSetArg(args[n],XtNvertDistance,31);n++;
    XtSetArg(args[n],XtNlabel,"Cancel");n++;
    Cancel = XtCreateManagedWidget("watchButton",
                                       commandWidgetClass,
                                       optionsForm,
                                        args, n);
    XtAddCallback(Cancel,XtNcallback,PopdownSelect,(XtPointer)optionsForm);

   /* ====================================== */
   /*  Create the "OKay" button              */
   /* ====================================== */
    n = 4;
    XtSetArg(args[n],XtNfromHoriz,Cancel);n++;
    XtSetArg(args[n],XtNhorizDistance,30);n++;
    XtSetArg(args[n],XtNlabel,"OKay");n++;
    Okay = XtCreateManagedWidget("watchButton",
                                       commandWidgetClass,
                                       optionsForm,
                                        args, n);
    XtAddCallback(Okay,XtNcallback,OkayOptionsCallback,(XtPointer)NULL);
    XtPopup(optionsShell,XtGrabNonexclusive);
  }



/*******************************************************************************
          Name:        SetStrategyCallback
          Description: Called when Depth Strategy is selected from Options menu.
                       It marks the new selection in the Depth Strategy menu,
                       but the value is changed only when the Okay button 
                       is pressed.
          Arguments:   w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void SetStrategyCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  int i,n = 0,index = (int)client_data;

  MoveEndOfFile(dialog_text, &event);

  XtSetArg(args[n], XtNleftBitmap, None);n++;
  for(i = 0 ; i <=  RANDOM_STRATEGY;i++)
    XtSetValues(strategy_widgets[i], args, n);
  XtSetArg(args[0], XtNleftBitmap, checker);n++;
  XtSetValues(strategy_widgets[index], args, n);
  optionFlags[STRATEGY_FLAG] = index;
  quit_get_event = True;
  }

/*******************************************************************************
          Name:        SetSalienceCallback
          Description: Called when Evaluate When salience is set
                       It marks the salience evaluation method to
                       the new selected method, but the new value only
                       changes when Okay button is pressed.
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void SetSalienceCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  int i,n = 0,index = (int)client_data;
  MoveEndOfFile(dialog_text, &event);

  n = 0;
  XtSetArg(args[n], XtNleftBitmap, None);n++;
  for(i = 0; i <= EVERY_CYCLE; i++)
   {
     if(i != index)
      XtSetValues(sal_opt_widgets[i],args,n);
   }
  n = 0;
  XtSetArg(args[n], XtNleftBitmap, checker);n++;
  XtSetValues(sal_opt_widgets[index], args, n);
  optionFlags[SALIENCE_FLAG] = index;
  quit_get_event = True;
  }


/**************************************************************************
 OkayOptionsCallback                                         
 Description : This function reset the option flags to the new values
               and remove the window from the screen.
 Arguments   : w - widget that where the event happened
 Return      : None
**************************************************************************/
void OkayOptionsCallback(w,client,call)
Widget w;
XtPointer client,call;
{
  Boolean OnOff;
  int i,n,flag;
 
   if((GetStrategy()) != optionFlags[STRATEGY_FLAG])
     SetStrategy(optionFlags[STRATEGY_FLAG]);
   if((optionFlags[SALIENCE_FLAG]) != GetSalienceEvaluation())
     SetSalienceEvaluation(optionFlags[SALIENCE_FLAG]);
   n = 0;

/* ========================================================= */

   XtSetArg(args[n],XtNstate,&OnOff);n++;
   XtGetValues(option_widgets[INT_FACT_DUPLICATION],args,n);
   if((OnOff)&&(!GetFactDuplication()))
    {
      SetFactDuplication(CLIPS_TRUE);
    }
   else if ((!OnOff)&&(GetFactDuplication()))
    {
      SetFactDuplication(CLIPS_FALSE);
    }
/* ========================================================= */

   XtGetValues(option_widgets[INT_DYN_CONSTRAINT_CHK],args,n);
   if((OnOff)&&(!GetDynamicConstraintChecking()))
    {
      SetDynamicConstraintChecking(CLIPS_TRUE);
    }
   else if((!OnOff) &&(GetDynamicConstraintChecking()))
    {
      SetDynamicConstraintChecking(CLIPS_FALSE);
    }

/* ========================================================= */

   XtGetValues(option_widgets[INT_STA_CONSTRAINT_CHK],args,n);
   if((OnOff)&&(!GetStaticConstraintChecking()))
    {
      SetStaticConstraintChecking(CLIPS_TRUE);
    }
   else if((!OnOff) &&(GetStaticConstraintChecking()))  
    { 
      SetStaticConstraintChecking(CLIPS_FALSE);
    }
/* ========================================================= */

   XtGetValues(option_widgets[INT_SEQUENCE_OPT_REG],args,n);
   if((OnOff)&&(!GetSequenceOperatorRecognition()))
    {
      SetSequenceOperatorRecognition(CLIPS_TRUE);
    }
   else if((!OnOff) &&(GetSequenceOperatorRecognition()))
    {
      SetSequenceOperatorRecognition(CLIPS_FALSE);
    }

/* ========================================================= */

   XtGetValues(option_widgets[INT_AUTO_FLOAT_DIV],args,n);
   if((OnOff)&&(!GetAutoFloatDividend()))
    {
      SetAutoFloatDividend(CLIPS_TRUE);
    }
   else if((!OnOff) &&(GetAutoFloatDividend()))
    {
      SetAutoFloatDividend(CLIPS_FALSE);
    }
/* ========================================================= */

   XtGetValues(option_widgets[INT_INCREMENTAL_RESET],args,n);
   if((OnOff)&&(!GetIncrementalReset()))
    {
      SetIncrementalReset(CLIPS_TRUE);
    }
   else if((!OnOff) &&(GetIncrementalReset()))
    {
      SetIncrementalReset(CLIPS_FALSE);
    }
/* ========================================================= */

   XtGetValues(option_widgets[INT_RESET_GLOBALS],args,n);
   if((OnOff)&&(!GetResetGlobals()))
    {
      SetResetGlobals(CLIPS_TRUE);
    }
   else if((!OnOff) &&(GetResetGlobals()))
    {
      SetResetGlobals(CLIPS_FALSE);
    }
   XtPopdown(XtParent(XtParent(w)));
  quit_get_event = True;
}

/**************************************************************************/

/**************************************************************************/



