/*********************************** xmenu.c **********************************/
/*                                                                            */
/******************************************************************************/
/*                                                                            */
/*        Authors:  BeBe Ly - NASA/Johnson Space Center                       */
/*                  Daniel J. McCoy - University of Houston-Downtown          */
/*                                  & Loral Space Information Systems         */
/*                                                                            */
/******************************************************************************/
/*  This file contains all the functions that create top level menus          */
/******************************************************************************/

#include <stdio.h>
#include <errno.h>
#include "xsetup.h"
#include "xclips.h"
#include "xmenu.h"
#include "setup.h"

#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>

#if SOLARIS 
#include <dirent.h>
#else
#include <sys/dir.h>
#endif

/********** Global variables **********/
extern Widget toplevel,dialog;
extern Pixmap checker, clips_logo;
extern Widget strategy_widgets[];  /* Widgets in the options window */
extern Widget sal_opt_widgets[];
extern Widget option_widgets[];
extern Widget optionsShell,optionsForm;

/********* local functions visible outside this file **********/
void CreatePullDownMenus();
void CancelPopupSelect();
void MenuFunc();

/********** local functions not visible outside this function **********/
void AboutXCLIPS();

static void CreateFileMenu();
/********** callbacks for File menu **********/
extern void EditCallback();
extern void CompletionDialogCallback();
extern void CompletionEditCallback();
extern void LoadRulesCallback();
extern void LoadBatchCallback();
extern void LoadBinaryCallback();
extern void LoadFactsCallback();
extern void DribbleCallback();
extern void SaveRulesCallback();
extern void SaveBinaryCallback();
extern void SaveFactsCallback();
extern void QuitCallback();
extern void LoadRules();
extern void LoadBatch();
extern void DribbleOn();
extern void LoadBinary();
extern void LoadTheFacts();
extern void Save();

static void CreateExecutionMenu();
/********** callbacks for Execution menu **********/
extern void ModuleCallback();
extern void ResetCallback();
extern void RunCallback();
extern void StepCallback();
extern void ClearCLIPSCallback();
extern void ClearScreenCallback();

static void CreateWatchMenu();
void PopdownSelect();
/********** callbacks for Watch menu **********/
extern void WatchAllCallback();
extern void WatchNoneCallback();
extern void OkWatchCallback();
extern void SetWatchFlagsCallback();
extern void WatchWindow();
static void CreateBrowseMenu();

static void CreateOptionsMenu();
/********** callbacks for Options menu **********/
extern void SetSalienceCallback();
extern void OptionsWindow();
extern void SetStrategyCallback();
extern void OkayOptionsCallback();
extern void CancelOptionsCallback();

static void CreateWindowsMenu();
/********** callbacks for Browse menu **********/
extern void ModuleCallback();
extern void DefruleManagerCallback();
extern void DeffactManagerCallback();
extern void DeftemplateManagerCallback();
extern void DeffunctionManagerCallback();
extern void DefglobalManagerCallback();
extern void DefgenericManagerCallback();
extern void DefinstancesManagerCallback();
extern void DefclassManagerCallback();
extern void AgendaManagerCallback();

/********* callbacks for Window menu **********/

extern void FactsWindowCallback();
extern void AgendaWindowCallback();
extern void InstancesWindowCallback();
extern void GlobalsWindowCallback();
extern void FocusWindowCallback();
extern void AllWindowsCallback();
extern void NoWindowsCallback();
extern void CommandLineCLIPSCallback();
extern void ColorUtilityCallback();

/********** from CLIPS code **********/
extern int SetCommandString();
extern int PrintCLIPS();
extern int AddRouter();
extern void DeleteRouter();

/********** from xclips.c **********/
extern int XclipsExit();
extern int XclipsQuery();

/********** from xedit.c **********/
extern void EditNewFile();
extern void EditorRevert();
extern void EditorSaveAs();

/********** from ClipsTextAct.c **********/
extern void MoveEndOfFile();

/********** external variables **********/

extern Arg args[10];
Widget defrule_manager = NULL, deffact_manager = NULL, deftemplate_manager = NULL,
       deffunction_manager = NULL, defgeneric_manager = NULL, definstances_manager = NULL,
       defclass_manager = NULL, agenda_manager = NULL,defglobal_manager = NULL;

extern Widget agenda_form, agenda_text;
extern Widget dialog_text,facts_form, facts_text;
extern char path[255];
extern XEvent event;

/********** Variables defined in this file is available to others ***********/

Widget facts_window, agenda_window, 
       instances_window, globals_window, focus_window;
Widget file_dribble; 
Widget button_form, button;
Widget file, file_list;

Widget FileItemWidgets[7];
Widget ExecItemWidgets[5];
char **filenames;
int number_entries;
int file_item = -1;

/*******************************************************************************
          Name:        CreatePullDownMenus
          Description: Creates all pulldown menus
          arguements:  parent - widget all menu buttons will be in
          Returns:     None
*******************************************************************************/
void CreatePullDownMenus(parent)
  Widget parent;
  {
  button_form = XtCreateManagedWidget("buttonForm",
                                      formWidgetClass,
                                      parent,
                                      NULL, 0);
  XtSetArg(args[0], XtNbitmap, clips_logo);
  XtSetArg(args[1], XtNinternalHeight, 0);
  XtSetArg(args[2], XtNinternalWidth, 0);
  XtSetArg(args[3], XtNshapeStyle, XmuShapeOval);
  XtSetArg(args[4], XtNborderWidth, 0);
  button = XtCreateManagedWidget("button",
                                 commandWidgetClass,
                                 button_form,
                                 args, 5);

  XtAddCallback(button, XtNcallback, AboutXCLIPS, NULL);

  CreateFileMenu(button_form);
  CreateExecutionMenu(button_form);
  CreateBrowseMenu(button_form);
  CreateWindowsMenu(button_form);
  }

/*******************************************************************************
          Name:        CreateFileMenu
          Description: Creates File menu
          arguements:  parent - form widget the menu button will be in
          Returns:     None
*******************************************************************************/
static void CreateFileMenu(parent)
  Widget parent;
  {
  Widget line,menu, entry;
  int i = 0;

  XtSetArg(args[0], XtNfromHoriz, button);
  XtSetArg(args[1], XtNlabel, "File");
  button = XtCreateManagedWidget("button",
                                 menuButtonWidgetClass,
                                 parent,
                                 args, 2);

  menu = XtCreatePopupShell("menu",
                            simpleMenuWidgetClass,
                            button,
                            NULL, 0);

  XtSetArg(args[0], XtNleftMargin, 15);

  /* =========================================== */
  /*  Create Edit item in the file menu          */
  /* =========================================== */

  FileItemWidgets[i] = XtCreateManagedWidget("Edit...         ^V",
                                smeBSBObjectClass,
                                menu,
                                args, 1);
  XtAddCallback(FileItemWidgets[i++], XtNcallback, EditCallback, NULL);

  /* =========================================== */
  /*  Create Complete item in the file menu      */
  /* =========================================== */

  FileItemWidgets[i] = XtCreateManagedWidget("Complete...     ^C",smeBSBObjectClass,
                                menu,
                                args, 1);
  XtAddCallback(FileItemWidgets[i++], XtNcallback, CompletionDialogCallback,(XtPointer)NULL);
  
  line = XtCreateManagedWidget("line",
                               smeLineObjectClass,
                               menu,
                               NULL, 0);

  /* =========================================== */
  /*  Create Load item in the file menu          */
  /* =========================================== */

  FileItemWidgets[i] = XtCreateManagedWidget("Load...         ^L",
                                smeBSBObjectClass,
                                menu,
                                args, 1);
  XtAddCallback(FileItemWidgets[i++], XtNcallback, LoadRulesCallback, NULL);

  /* =========================================== */
  /*  Create Load batch item in the file menu    */
  /* =========================================== */

  FileItemWidgets[i] = XtCreateManagedWidget("Load Batch...",
                                smeBSBObjectClass,
                                menu,
                                args, 1);
  XtAddCallback(FileItemWidgets[i++], XtNcallback, LoadBatchCallback, NULL);

  /* =========================================== */
  /*  Create Load Binary item in the file menu   */
  /* =========================================== */

  FileItemWidgets[i] = XtCreateManagedWidget("Load Binary...",
                                smeBSBObjectClass,
                                menu,
                                args, 1);
  XtAddCallback(FileItemWidgets[i++], XtNcallback, LoadBinaryCallback, NULL);

  /* =========================================== */
  /*  Create Dribble item in the file menu       */
  /* =========================================== */

  file_dribble = XtCreateManagedWidget("Dribble...      ^D",
                                       smeBSBObjectClass,
                                       menu,
                                       args, 1);
  XtAddCallback(file_dribble, XtNcallback, DribbleCallback, NULL);

  line = XtCreateManagedWidget("line",
                               smeLineObjectClass,
                               menu,
                               NULL, 0);
  /* =========================================== */
  /*  Create Save binary item in the file menu   */
  /* =========================================== */

  FileItemWidgets[i] = XtCreateManagedWidget("Save Binary...",
                                smeBSBObjectClass,
                                menu,
                                args, 1);
  XtAddCallback(FileItemWidgets[i++], XtNcallback, SaveBinaryCallback, NULL);

  line = XtCreateManagedWidget("line",
                               smeLineObjectClass,
                               menu,
                               NULL, 0);
  /* =========================================== */
  /*  Create Quit item in the file menu          */
  /* =========================================== */

  FileItemWidgets[i] = XtCreateManagedWidget("Quit            ^Q",
                                smeBSBObjectClass,
                                menu,
                                args, 1);
  XtAddCallback(FileItemWidgets[i], XtNcallback, QuitCallback, NULL);
  }

/*******************************************************************************
          Name:        CreateExecutionMenu
          Description: Creates Execution menu
          arguements:  parent - form widget the menu button will be in
          Returns:     None
*******************************************************************************/
static void CreateExecutionMenu(parent)
  Widget parent;
  {
  Widget line,menu, entry;
  int i = 0;

  XtSetArg(args[0], XtNfromHoriz, button);
  XtSetArg(args[1], XtNlabel, "Execution");
  button = XtCreateManagedWidget("button",
                                 menuButtonWidgetClass,
                                 parent,
                                 args, 2);

  menu = XtCreatePopupShell("menu",
                            simpleMenuWidgetClass,
                            button,
                            NULL, 0);

  ExecItemWidgets[i] = XtCreateManagedWidget("Reset        ^E",
                                smeBSBObjectClass,
                                menu,
                                NULL, 0);
  XtAddCallback(ExecItemWidgets[i], XtNcallback, ResetCallback, NULL);
  i++;
  ExecItemWidgets[i] = XtCreateManagedWidget("Run          ^R",
                                smeBSBObjectClass,
                                menu,
                                NULL, 0);
  XtAddCallback(ExecItemWidgets[i], XtNcallback, RunCallback, NULL);
  i++;
  ExecItemWidgets[i] = XtCreateManagedWidget("Step         ^T",
                                smeBSBObjectClass,
                                menu,
                                NULL, 0);
  XtAddCallback(ExecItemWidgets[i], XtNcallback, StepCallback, NULL);
  i++;
  entry = XtCreateManagedWidget("Watch...",
                                smeBSBObjectClass,
                                menu,
                                NULL, 0);
  XtAddCallback(entry,XtNcallback,WatchWindow,NULL);
  
  entry = XtCreateManagedWidget("Options...",
                                smeBSBObjectClass,
                                menu,
                                NULL, 0);
  XtAddCallback(entry,XtNcallback,OptionsWindow,NULL);
  line =  XtCreateManagedWidget("line",
                               smeLineObjectClass,
                               menu,
                               NULL, 0);

  ExecItemWidgets[i] = XtCreateManagedWidget("Clear CLIPS  ^K",
                                smeBSBObjectClass,
                                menu,
                                NULL, 0);
  XtAddCallback(ExecItemWidgets[i], XtNcallback, ClearCLIPSCallback, NULL);
  i++;
  ExecItemWidgets[i] = XtCreateManagedWidget("Clear Window ^N",
                                smeBSBObjectClass,
                                menu,
                                NULL, 0);
  XtAddCallback(ExecItemWidgets[i],XtNcallback,ClearScreenCallback,NULL);
  }

/*******************************************************************************
          Name:        CreateWatchMenu
          Description: Creates Watch menu
          arguements:  parent - form widget the menu button will be in
          Returns:     None
*******************************************************************************/
static void CreateWatchMenu(parent)
  Widget parent;
  {

  int n = 0;

  XtSetArg(args[n], XtNfromHoriz, button);n++;
  XtSetArg(args[n], XtNlabel, "Watch");n++;
  button = XtCreateManagedWidget("watchButton",
                                 commandWidgetClass,
                                 button_form,
                                 args, n);

  XtAddCallback(button, XtNcallback, WatchWindow, NULL);

 }

/*******************************************************************************
          Name:        CreateBrowseMenu
          Description: Creates Browse menu
          arguements:  parent - form widget the menu button will be in
          Returns:     None
*******************************************************************************/
static void CreateBrowseMenu(parent)
  Widget parent;
  {
  Widget menu,entry,line;

  XtSetArg(args[0], XtNfromHoriz, button);
  XtSetArg(args[1], XtNlabel, "Browse");
  button = XtCreateManagedWidget("button",
                                 menuButtonWidgetClass,
                                 parent,
                                 args, 2);
  menu = XtCreatePopupShell("menu",
                            simpleMenuWidgetClass,
                            button,
                            NULL, 0);

  entry = XtCreateManagedWidget("Module...",
                                smeBSBObjectClass,
                                menu,
                                NULL, 0);
  XtAddCallback(entry, XtNcallback,ModuleCallback,NULL);

  line =  XtCreateManagedWidget("line",
                               smeLineObjectClass,
                               menu,
                               NULL, 0);

  XtSetArg(args[0], XtNleftMargin, 15);
  XtSetArg(args[1], XtNsensitive, False);

  defrule_manager = XtCreateManagedWidget("Defrule Manager...",
                                          smeBSBObjectClass,
                                          menu,
                                          args, 2);
  XtAddCallback(defrule_manager, XtNcallback, DefruleManagerCallback, NULL);

  deffact_manager = XtCreateManagedWidget("Deffacts Manager...",
                                          smeBSBObjectClass,
                                          menu,
                                          args, 2);
  XtAddCallback(deffact_manager, XtNcallback, DeffactManagerCallback, NULL);

  deftemplate_manager = XtCreateManagedWidget("Deftemplates Manager...",
                                              smeBSBObjectClass,
                                              menu,
                                              args, 2);
  XtAddCallback(deftemplate_manager, XtNcallback,
                DeftemplateManagerCallback, NULL);

  deffunction_manager = XtCreateManagedWidget("Deffunction Manager...",
                                              smeBSBObjectClass,
                                              menu,
                                              args, 2);
  XtAddCallback(deffunction_manager, XtNcallback,
                DeffunctionManagerCallback, NULL);

  defglobal_manager = XtCreateManagedWidget("Defglobal Manager...",
                                            smeBSBObjectClass,
                                              menu,
                                              args, 2);
  XtAddCallback(defglobal_manager, XtNcallback,
                DefglobalManagerCallback, NULL);

  defgeneric_manager = XtCreateManagedWidget("Defgeneric Manager...",
                                             smeBSBObjectClass,
                                             menu,
                                             args, 2);
  XtAddCallback(defgeneric_manager, XtNcallback,
                DefgenericManagerCallback, NULL);

  defclass_manager = XtCreateManagedWidget("Defclass Manager...",
                                           smeBSBObjectClass,
                                           menu,
                                           args, 2);
  XtAddCallback(defclass_manager, XtNcallback, DefclassManagerCallback, NULL);

  definstances_manager = XtCreateManagedWidget("Definstances Manager...",
                                               smeBSBObjectClass,
                                               menu,
                                               args, 2);
  XtAddCallback(definstances_manager, XtNcallback,
                DefinstancesManagerCallback, NULL);

  agenda_manager = XtCreateManagedWidget("Agenda Manager...",
                                         smeBSBObjectClass,
                                         menu,
                                         args, 1);
  XtAddCallback(agenda_manager, XtNcallback, AgendaManagerCallback, NULL);

}


/*******************************************************************************
          Name:        CreateWindowsMenu
          Description: Creates the Windows menu
          arguements:  parent - form widget the menu button will be in
          Returns:     None
*******************************************************************************/
static void CreateWindowsMenu(parent)
  Widget parent;
  {
  Widget line,menu, entry,all,none;

  XtSetArg(args[0], XtNfromHoriz, button);
  XtSetArg(args[1], XtNlabel, "Windows");
  button = XtCreateManagedWidget("button",
                                 menuButtonWidgetClass,
                                 parent,
                                 args, 2);

  menu = XtCreatePopupShell("menu",
                            simpleMenuWidgetClass,
                            button,
                            NULL, 0);

  /* =========================================== */
  /*   Create the "Fact Window" button           */
  /* =========================================== */

  XtSetArg(args[0], XtNleftMargin, 15);
  facts_window = XtCreateManagedWidget("Facts Window",
                                       smeBSBObjectClass,
                                       menu,
                                       args, 1);
  XtAddCallback(facts_window, XtNcallback, FactsWindowCallback, NULL);

  /* =========================================== */
  /*   Create the "Agenda Window" button         */
  /* =========================================== */

  agenda_window = XtCreateManagedWidget("Agenda Window",
                                        smeBSBObjectClass,
                                        menu,
                                        args, 1);
  XtAddCallback(agenda_window, XtNcallback, AgendaWindowCallback, NULL);
 
  /* =========================================== */
  /*   Create the "Instances Window" button      */
  /* =========================================== */

  instances_window =  XtCreateManagedWidget("Instances Window",
                                        smeBSBObjectClass,
                                        menu,
                                        args, 1);

  XtAddCallback(instances_window, XtNcallback,InstancesWindowCallback,NULL);

  /* =========================================== */
  /*   Create the "Global Window" button         */
  /* =========================================== */

  globals_window =  XtCreateManagedWidget("Global Window",
                                        smeBSBObjectClass,
                                        menu,
                                        args, 1);
  XtAddCallback(globals_window, XtNcallback,GlobalsWindowCallback,NULL);

  /* =========================================== */
  /*   Create the "Focus Window" button          */
  /* =========================================== */

  focus_window = XtCreateManagedWidget("Focus Window",
                                        smeBSBObjectClass,
                                        menu,
                                        args, 1);
  XtAddCallback(focus_window, XtNcallback,FocusWindowCallback,NULL);

  line = XtCreateManagedWidget("line", smeLineObjectClass, menu, NULL, 0);

  /* =========================================== */
  /*   Create the "All Windows" button           */
  /* =========================================== */

  all = XtCreateManagedWidget("All Windows",
                                        smeBSBObjectClass,
                                        menu,
                                        args, 1);
  XtAddCallback(all,XtNcallback,AllWindowsCallback,NULL);

  /* =========================================== */
  /*   Create the "None" button                  */
  /* =========================================== */

  none = XtCreateManagedWidget("None",
                                smeBSBObjectClass,
                                menu,
                                args, 1);
  XtAddCallback(none,XtNcallback,NoWindowsCallback,NULL);
  line = XtCreateManagedWidget("line", smeLineObjectClass, menu, NULL, 0);

  /* =========================================== */
  /*   Create the "Command Line CLIPS" button    */
  /* =========================================== */


  entry = XtCreateManagedWidget("Command Line CLIPS ^Z",
                                smeBSBObjectClass,
                                menu,
                                args, 1);
  XtAddCallback(entry, XtNcallback, CommandLineCLIPSCallback, NULL);

  /* =========================================== */
  /*   Create the "Color Utilities" button       */
  /* =========================================== */

  entry = XtCreateManagedWidget("Color Utility",
                                smeBSBObjectClass,
                                menu,
                                args, 1);
  XtAddCallback(entry, XtNcallback, ColorUtilityCallback, NULL);

  }

/*******************************************************************************
          Name:        AboutXCLIPS
          Description: Called when CLIPS logo is selected form menu form.
                       It displays the general information about CLIPS.

          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void AboutXCLIPS(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget about, about_form, about_list, about_quit;

  about = XtCreatePopupShell("About XCLIPS",
                             topLevelShellWidgetClass,
                             toplevel,
                             NULL, 0);

  XtSetArg(args[0], XtNdefaultDistance, 0);
  about_form = XtCreateManagedWidget("about_form",
                                     formWidgetClass,
                                     about,
                                     args, 1);

  XtSetArg(args[0], XtNborderWidth, 0);
  XtSetArg(args[1], XtNdefaultColumns, 1);
  XtSetArg(args[2], XtNforceColumns, True);
  XtSetArg(args[3], XtNlist, about_info);
  XtSetArg(args[4], XtNallowVert, True);
  XtSetArg(args[5], XtNallowHoriz, True);
  about_list = XtCreateManagedWidget("menu",
                                     listWidgetClass,
                                     about_form,
                                     args, 6);
  XtAddCallback(about_list, XtNcallback, CancelPopupSelect,
                (XtPointer)about_form);

  XtPopup(about, XtGrabNone);
  }

/*******************************************************************************
          Name:        DialogReturn
          Description: This function will be executed when return was pressed
                       while curssor is in the dialog box of the file select
                       window.
          Arguments:  w - Widget that caused action to be called
                       event - Not used
                       params - Dialog widget
                       num_params - Not used
          Returns:     None
*******************************************************************************/
void DialogReturn(w, event, params, num_params)
  Widget w;
  XEvent *event;
  String *params;
  Cardinal *num_params;
  {
  MenuFunc(w, params, (XtPointer)NULL);
  }

/*******************************************************************************
          Name:        MenuFunc
          Description: Simulates callbacks for the dialog box of the file
                       select window. which callback function is executed
                       depends on which menu item from the file menu of 
                       the main CLIPS window is activated.
          Arguments:  w - Dialog widget
                       client_data - Dialog widget
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
void MenuFunc(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  String filename = XawDialogGetValueString(XtParent(w));
  char fullpath[255];
  Widget popup = XtParent(XtParent(XtParent(w)));

  if(filename[0] == 0)
     return;
  strcpy(fullpath, path);
  strcat(fullpath, filename);
  MoveEndOfFile(dialog_text, &event);
  switch(file_item)
    {
    case EDIT:
      XtDestroyWidget(popup);
      EditNewFile(w, client_data, call_data);
    break;

    case LOADBATCH:
      XtDestroyWidget(popup);
      LoadBatch(fullpath);
    break;

    case LOADBINARY:
      XtDestroyWidget(popup);
      LoadBinary(fullpath);
    break;

    case LOADFACTS:
      XtDestroyWidget(popup);
      LoadTheFacts(fullpath);
    break;

    case LOADRULES:
      XtDestroyWidget(popup);
      LoadRules(fullpath);
    break;

    case DRIBBLEON:
      XtDestroyWidget(popup);
      DribbleOn(fullpath);
    break;

     case SAVERULES:
      Save(w, client_data, call_data);
    break;

    case SAVEBINARY:
      Save(w, client_data, call_data);
    break;

    case SAVEFACTS:
      Save(w, client_data, call_data);
    break;

     }
  }

/*******************************************************************************
          Name:        CancelPopupSelect
          Description: Destroys a the parent of the widget sent
          Arguments:  w - Not Used
                       client_data - Child of widget to destroy
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
void CancelPopupSelect(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  XtDestroyWidget(XtParent((Widget) client_data));
  }


/*******************************************************************************
          Name:        PopdownSelect
          Description: Popdown the parent of the widget sent
          Arguments:  w - Not Used
                       client_data - Child of widget to destroy
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
void PopdownSelect(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
    XtPopdown(XtParent((Widget) client_data));
  }
