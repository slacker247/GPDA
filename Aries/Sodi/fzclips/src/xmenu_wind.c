/******************************** xmenu _wind.c ********************************/
/*                                                                            */
/******************************************************************************/
/*                                                                            */
/*        Author:   BeBe Ly - NASA/Johnson Space Center                       */
/*                  Daniel J. McCoy - University of Houston-Downtown          */
/*                                   & Loral Space Information Systems        */
/*                                                                            */
/******************************************************************************/
/*        Modified by:  Bebe Ly - NASA/Johnson Space Center                   */
/*        Date: Feb 1993                                                      */
/******************************************************************************/

#include <stdio.h>
#include "xsetup.h"
#include "xmenu_wind.h"
#include "xclips.h"
#include "setup.h"
#include "ruledef.h"
#include "dffctdef.h"
#include "tmpltdef.h"
#include "agenda.h"
#include "dffnxfun.h"
#include "globldef.h"
#include "genrcfun.h"
#include "defins.h"
#include "object.h"
#include "engine.h"
#include "classcom.h"
#include "genrccom.h"
#include "moduldef.h"

/********** local functions visible outside this file **********/
void DefruleManagerCallback();
void DeffactManagerCallback();
void DeftemplateManagerCallback();
void DeffunctionManagerCallback();
void efglobalManagerCallback();
void DefgenericManagerCallback();
void DefinstancesManagerCallback();
void DefclassManagerCallback();
void AgendaManagerCallback();
void FactsWindowCallback();
void AgendaWindowCallback();
void InstancesWindowCallback();
void GlobalsWindowCallback();
void FocusWindowCallback();
void CommandLineCLIPSCallback();
void ColorUtilityCallback();
void CancelSelectPrimary();

/********** local functions not visible outside this file **********/
static void AgendaFire();
static void AgendaManagerSelect();
static void AgendaRemove();
static void CancelSelectSecondary();
static void CancelRulemngrSelect();
static void DeffactsManagerSelect();
static void DeffactsPprint();
static void DeffactsRemove();
static void DeftemplateManagerSelect();
static void DeftemplatePprint();
static void DeftemplateRemove();
static void Restart();
static void RuleManagerSelect();
static void DefruleMatchesCallback();
static void DefrulePprintCallback();
static void DefruleRemoveCallback();
static void DefruleRemoveBreakCallback();
static void DefruleSetBreakCallback();
static void DefruleRefreshCallback();
static void DeffunctionRemoveCallback();
static void DeffunctionPprintCallback();
static void DefglobalPprintCallback();
static void DefglobalRemoveCallback();
static void DefgenericRemoveCallback();
static void DefgenericPprintCallback();
static void DefgenericMethodCallback();
static void DefgenericWatchCallback();
static void DefgenericMngrCheckBoxCallback();
static void RemoveDefmethodCallback();
static void DefmethodPprintCallback();
static void DefmethodWatchCallback();
static void DefinstancesRemoveCallback();
static void DefinstancesPprintCallback();
static void DefclassRemoveCallback();
static void DefclassDescribeCallback();
static void DefclassBrowseCallback();
static void DefclassPprintCallback();
static void DefclassMessageHandlersCallback();
static void MessageHandlerPprintCallback();
static void RemoveMessageHandlerCallback();
static void DefmessHdlrMngrWatchCallback();
static void DefmessHdlrMngrCheckBoxCallback();
static void DefruleBreakPointCallback();
static void DefruleActivationCallback();
static void DefruleFiringsCallback();
static void DeftemplateWatchCallback();
static void WatchInstancesCallback();
static void WatchSlotCallback();
static void DefruleMngrCheckboxesCallback();
static void DefclssMngrChckbxCallback();
static void DeftemplateMngrCheckboxCallback();
static void DeffunctionWatchCallback();
static void DeffunctionMngrCheckboxCallback();
static void DefmethodMngrCheckBoxCallback();
static void DefmoduleSelectCallback();
static void DoneSelectDefmoduleCallback();
static char **IntGetModuleList();

/********** from CLIPS code **********/
extern int SetCommandString();
extern int PrintCLIPS();
extern int XclipsExit();
extern int XclipsQuery();
extern int XclipsPrint();
extern VOID *FindDefgeneric();
extern VOID *FindDefclass();
extern unsigned GetNextDefmethod();
extern VOID GetDefmethodDescription();
extern char *GetDefmessageHandlerName();
extern char *GetDefmessageHandlerType();
extern unsigned GetNextDefmessageHandler();
extern char *itoa();

/********** Global variables **********/
extern Widget toplevel, dialog, facts, agenda,instances, globals,focus;
extern Arg args[10];
extern XEvent event;     /* Current event */
extern Boolean quit_get_event;
extern Pixmap checker;
extern Boolean Browse_status[];
extern Widget dialog_text;
extern int EvaluatingTopLevelCommand;

/********** local variables available to other files ***********/
Widget agenda = NULL,agenda_form = NULL, agenda_text = NULL, facts = NULL, facts_form = NULL, 
       facts_text = NULL,instances = NULL,instances_form = NULL,
       instances_text = NULL,globals = NULL,globals_form = NULL,
       globals_text = NULL,focus = NULL, focus_form  = NULL,focus_text = NULL;

/********** local variables **********/
Boolean list_change = False;
Boolean list1_change = False;
static Widget manager_list, manager_list1;
static Boolean defrulemanager_flag = False;
static Boolean deffactsmanager_flag = False;
static Boolean deftemplatemanager_flag = False;
static Boolean deffunctionmanager_flag = False;
static Boolean defglobalmanager_flag = False;
static Boolean defgenericmanager_flag = False;
static Boolean defmethodmanager_flag = False;
static Boolean definstancesmanager_flag = False;
static Boolean defclassmanager_flag = False;
static Boolean agendamanager_flag = False;
static Boolean defmessagehandler_flag = False;
Widget facts_window, agenda_window,instances_window, globals_window,focus_window;
static String curr_def_name;
String *item_list;
String *item_list1;


/*******************************************************************************
          Name:        ModuleCallback
          Description: Called when Module is selected form Execution menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void ModuleCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
    static char** moduleList = NULL;
    char *ModuleName;
    int i = 0;
    Widget defmoduleShell = NULL, defmoduleForm = NULL, defmoduleViewport = NULL,
           defmoduleList = NULL, select = NULL, done = NULL;

     /* ============================== */
     /*  Get the module list           */
     /* ============================== */

     moduleList = IntGetModuleList();
     /* ======================================================== */
     /*  Create the defmodule window                             */
     /* ======================================================== */

      defmoduleShell = XtCreatePopupShell("Defmodule",
                                      topLevelShellWidgetClass,
                                      toplevel,
                                      NULL, 0);
      defmoduleForm = XtCreateManagedWidget("manager_form",
                                              formWidgetClass,
                                              defmoduleShell,
                                              NULL, 0);
      XtSetArg(args[0],XtNallowHoriz,True);
      XtSetArg(args[1],XtNallowVert,True);
      defmoduleViewport = XtCreateManagedWidget("manager_viewport",
                                                  viewportWidgetClass,
                                                  defmoduleForm,
                                                  args,2);
      if(moduleList == NULL)
       {
         moduleList = (char**)balloc(2,String);
         moduleList[0] = "";
         moduleList[1] = NULL;
       }
      else  /* Find the index of the current module from the list */
       {
        ModuleName = GetDefmoduleName((struct defmodule*)GetCurrentModule());
        for(i= 0; moduleList[i] != NULL;)
         {
           if(strcmp(ModuleName,moduleList[i]) == 0)
             break;
           i++;
         }
       }
      XtSetArg(args[0],XtNlist,moduleList);
      defmoduleList = XtCreateManagedWidget("manager_list",
                                       listWidgetClass,
                                       defmoduleViewport,
                                       args, 1);
       /* ====================================== */
       /* Highlight the current module           */
       /* ====================================== */

        XawListHighlight(defmoduleList,i);

        XtAddCallback(defmoduleList,XtNcallback,DefmoduleSelectCallback,(XtPointer)defmoduleList);
       /* ====================================== */
       /* Create the Done Button                 */
       /* ====================================== */
        XtSetArg(args[0], XtNlabel,"Done");
        XtSetArg(args[1], XtNfromHoriz, defmoduleViewport);
        done = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 defmoduleForm,
                                 args,2);
        XtAddCallback(done,XtNcallback,DoneSelectDefmoduleCallback,(XtPointer)moduleList);
        XtPopup(defmoduleShell, XtGrabNonexclusive);

  }

/****************************************************************************
***
 *  IntGetModuleList
 *  Description:
 *  Input :
 *  Returns: A list of modules
 ****************************************************************************
***/
static char **IntGetModuleList()
{
   int maxItems = 20, itemCount = 0;
   char* name;
   static char **itemList = NULL;
   struct defmodule *theDefmodule = NULL;

   if((theDefmodule = (struct defmodule *)GetNextDefmodule(NULL)) == NULL)
        return(NULL);

   itemList = (String *)calloc(maxItems,sizeof(String));
   while( theDefmodule != NULL)
    {
      name = GetDefmoduleName(theDefmodule);
      itemList[itemCount]  = balloc(strlen(name) + 1,char);
      strcpy(itemList[itemCount],name);
      itemCount++;
      if (itemCount == (maxItems - 1))
       {
            maxItems = 2*maxItems;
            itemList = (String *)realloc(itemList,maxItems * sizeof(String));
       }
       theDefmodule = (struct defmodule *)GetNextDefmodule(theDefmodule);
    }
   itemList[itemCount] = NULL;
   /*sortList(itemList,itemCount);*/
   return(itemList);


}


/*******************************************************************************
          Name:        DoneSelectDefmoduleCallback
          Description: Called when Done is selected form module window
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
static void DoneSelectDefmoduleCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
{
    char** list = (char**)client_data;
    int i = 0;

    XtDestroyWidget(XtParent(XtParent(w)));
    if(list == NULL)
      return;
    while(list[i] != NULL)
     {
       free(list[i]);
       i++;
     }
    free(list);
}
/****************************************************************************
**
        Name:           DefmoduleSelectCallback
        Description:
        Arguments:
        Return:
*****************************************************************************
**/
static void DefmoduleSelectCallback(w,client_data,call_data)
Widget w;
XtPointer client_data,call_data;
{
   VOID* defmodulePtr = NULL;
   XawListReturnStruct *current = XawListShowCurrent((Widget)client_data);

  if(current->list_index == XAW_LIST_NONE)
      return;
  SetCurrentModule(FindDefmodule(current->string));
 /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */

  quit_get_event = True; 

}


/*******************************************************************************
          Name:        DefruleManagerCallback
          Description: Pop up the Rule Manager
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void DefruleManagerCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
   static Widget defrulemanager = NULL;
   Widget defrulemanager_form,defrulemanager_viewport,
          remove = NULL, refresh = NULL, matches = NULL, pprint = NULL, 
          break_point = NULL,break_point_label = NULL,watch_activation_label = NULL,
          watch_firing_label = NULL,
          watch_activation = NULL,watch_firing = NULL, cancel = NULL;
   static Widget CheckBoxes[3];
   int itemCount = 0;
   char buffer[MAX_CHAR_IN_BUF];
   
  /* ======================= */
  /*  Get the rule list      */
  /* ======================= */
 
  itemCount = IntGetDefruleLis();

  if(item_list == NULL)
    {
        ClearParameters();
        defrulemanager_flag = False;
        return;
     }

  defrulemanager_flag = True;
  
 /* ======================================== */
  /* Get the title for defrule manager window */
  /* ======================================== */
  sprintf(buffer,"Defrule Manager - %d Items",itemCount);


  /* ====================================================== */
  /*  Create the defrule manager window                     */
  /* ====================================================== */
 
  defrulemanager = XtCreatePopupShell(buffer,
                                      topLevelShellWidgetClass,
                                      toplevel,
                                      NULL, 0);

  defrulemanager_form = XtCreateManagedWidget("manager_form",
                                              formWidgetClass,
                                              defrulemanager,
                                              NULL, 0);

  XtSetArg(args[0],XtNallowHoriz,True);
  XtSetArg(args[1],XtNallowVert,True);
  defrulemanager_viewport = XtCreateManagedWidget("manager_viewport",
                                                  viewportWidgetClass,
                                                  defrulemanager_form,
                                                  args, 2);
  XtSetArg(args[0], XtNlist, item_list);
  manager_list = XtCreateManagedWidget("manager_list",
                                       listWidgetClass,
                                       defrulemanager_viewport,
                                       args, 1);

  XtSetArg(args[0], XtNfromHoriz, defrulemanager_viewport);

  /* ==================================================== */
  /*  Create the Remove button                            */
  /* ==================================================== */

  XtSetArg(args[1], XtNlabel, "Remove");
  remove = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 defrulemanager_form,
                                 args, 2);
  XtAddCallback(remove, XtNcallback, DefruleRemoveCallback, (XtPointer)manager_list);

  /* ==================================================== */
  /*  Create the Refresh button                           */
  /* ==================================================== */

  XtSetArg(args[1], XtNfromVert, remove);
  XtSetArg(args[2], XtNlabel, "Refresh");
  refresh = XtCreateManagedWidget("managerButton",
                                  commandWidgetClass,
                                  defrulemanager_form,
                                  args, 3);
  XtAddCallback(refresh, XtNcallback, DefruleRefreshCallback, (XtPointer)manager_list);

  /* ==================================================== */
  /*  Create the Matches button                           */
  /* ==================================================== */

  XtSetArg(args[1], XtNfromVert, refresh);
  XtSetArg(args[2], XtNlabel, "Matches");
  matches = XtCreateManagedWidget("managerButton",
                                  commandWidgetClass,
                                  defrulemanager_form,
                                  args, 3);
  XtAddCallback(matches, XtNcallback, DefruleMatchesCallback, (XtPointer)manager_list);

  /* ==================================================== */
  /*  Create the Pprint button                            */
  /* ==================================================== */

  XtSetArg(args[1], XtNfromVert, matches);
  XtSetArg(args[2], XtNlabel, "Pprint");
  pprint = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 defrulemanager_form,
                                 args, 3);
  XtAddCallback(pprint, XtNcallback, DefrulePprintCallback, (XtPointer)manager_list);

  /* ==================================================== */
  /*  Create the BreakPoint button                        */
  /* ==================================================== */
 
  XtSetArg(args[1], XtNfromVert, pprint);
  XtSetArg(args[2], XtNlabel," ");
  CheckBoxes[0] = break_point = XtCreateManagedWidget("managerButton",
                                    toggleWidgetClass,
                                    defrulemanager_form,
                                    args, 3);
  XtAddCallback(break_point,XtNcallback,DefruleBreakPointCallback,(XtPointer)manager_list);
  XtSetArg(args[0], XtNfromHoriz,break_point);
  XtSetArg(args[1], XtNfromVert, pprint);
  XtSetArg(args[2], XtNlabel,"Breakpoint");
  XtSetArg(args[3], XtNborderWidth,0);
  break_point_label = XtCreateManagedWidget("checkBoxLabel",
                                    labelWidgetClass,
                                    defrulemanager_form,
                                    args, 4);
  /* ==================================================== */
  /*  Create the Watch Activation button                  */
  /* ==================================================== */

  XtSetArg(args[0], XtNfromHoriz,defrulemanager_viewport);
  XtSetArg(args[1], XtNfromVert,break_point);
  XtSetArg(args[2], XtNlabel," ");
  CheckBoxes[1] = watch_activation = XtCreateManagedWidget("managerButton",
                                    toggleWidgetClass,
                                    defrulemanager_form,
                                    args, 3);
  XtAddCallback(watch_activation,XtNcallback,DefruleActivationCallback,(XtPointer)manager_list);
  XtSetArg(args[0], XtNfromHoriz,watch_activation);
  XtSetArg(args[1], XtNfromVert, break_point);
  XtSetArg(args[2], XtNlabel,"Watch Activation");
  XtSetArg(args[3], XtNborderWidth,0);
  watch_activation_label = XtCreateManagedWidget("checkBoxLabel",
                                    labelWidgetClass,
                                    defrulemanager_form,
                                    args, 4);
  /* ==================================================== */
  /*  Create the Watch Firing button                            */
  /* ==================================================== */

  XtSetArg(args[0], XtNfromHoriz,defrulemanager_viewport);
  XtSetArg(args[1], XtNfromVert,watch_activation);
  XtSetArg(args[2], XtNlabel," ");

  CheckBoxes[2] = watch_firing = XtCreateManagedWidget("managerButton", 
                                    toggleWidgetClass,
                                    defrulemanager_form,
                                    args, 3); 
  XtAddCallback(watch_firing,XtNcallback,DefruleFiringsCallback,(XtPointer)manager_list);
  XtSetArg(args[0], XtNfromHoriz,watch_firing);
  XtSetArg(args[1], XtNfromVert, watch_activation);
  XtSetArg(args[2], XtNlabel,"Watch Firing");
  XtSetArg(args[3], XtNborderWidth,0);
  watch_firing_label = XtCreateManagedWidget("checkBoxLabel",
                                    labelWidgetClass,
                                    defrulemanager_form,
                                    args, 4);
  XtSetArg(args[0], XtNfromHoriz,defrulemanager_viewport);
  XtSetArg(args[1], XtNfromVert, watch_firing);
  XtSetArg(args[2], XtNlabel, "Done");
  cancel = XtCreateManagedWidget("managerCancel",
                                 commandWidgetClass,
                                 defrulemanager_form,
                                 args, 3);
  XtAddCallback(cancel, XtNcallback, CancelSelectPrimary, (XtPointer) defrulemanager);

  /* ====================================================== */
  /*  Add the callback function to the manager_list widget */
  /* ====================================================== */

  XtAddCallback(manager_list,XtNcallback,DefruleMngrCheckboxesCallback,(XtPointer)CheckBoxes);
  XtPopup(defrulemanager, XtGrabNonexclusive);

  }

/*******************************************************************************
          Name:        DeffactManagerCallback
          Description: Pop up the Deffacts Manager
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void DeffactManagerCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget deffactsmanager,deffactsmanager_form, deffactsmanager_viewport,
         remove, pprint, cancel;
  char buffer[MAX_CHAR_IN_BUF];
  int itemCount;

  itemCount = IntGetFactList();

  if(item_list == NULL)
    {
     ClearParameters();
     deffactsmanager_flag = False;
     return;
    }

  deffactsmanager_flag = True;

  /* ======================================== */
  /* Get the title for deffact manager window */
  /* ======================================== */
  sprintf(buffer,"Deffacts Manager - %d Items",itemCount);

  /* ==================================================== */
  /*  Create the Deffacts Manager window                  */ 
  /* ==================================================== */

  deffactsmanager = XtCreatePopupShell(buffer,
                                       topLevelShellWidgetClass,
                                       toplevel,
                                       NULL, 0);

  deffactsmanager_form = XtCreateManagedWidget("manager_form",
                                               formWidgetClass,
                                               deffactsmanager,
                                               NULL, 0);
  XtSetArg(args[0],XtNallowHoriz,True);
  XtSetArg(args[1],XtNallowVert,True);
  deffactsmanager_viewport = XtCreateManagedWidget("manager_viewport",
                                                         viewportWidgetClass,
                                                         deffactsmanager_form,
                                                         args, 2);

  XtSetArg(args[0], XtNlist, item_list);
  manager_list = XtCreateManagedWidget("manager_list",
                                        listWidgetClass,
                                        deffactsmanager_viewport,
                                        args, 1);
  /* ==================================================== */
  /*  Create the Remove button                            */
  /* ==================================================== */

  XtSetArg(args[0], XtNfromHoriz, deffactsmanager_viewport);
  XtSetArg(args[1], XtNlabel, "Remove");
  remove = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 deffactsmanager_form,
                                 args, 2);
  XtAddCallback(remove, XtNcallback, DeffactsRemove, (XtPointer)manager_list);

  /* ==================================================== */
  /*  Create the Pprint button                            */
  /* ==================================================== */

  XtSetArg(args[1], XtNfromVert, remove);
  XtSetArg(args[2], XtNlabel, "Pprint");
  pprint = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 deffactsmanager_form,
                                 args, 3);
  XtAddCallback(pprint, XtNcallback, DeffactsPprint, (XtPointer)manager_list);

  /* ==================================================== */
  /*  Create the Done button                              */
  /* ==================================================== */

  XtSetArg(args[1], XtNfromVert, pprint);
  XtSetArg(args[2], XtNlabel, "Done");
  cancel = XtCreateManagedWidget("managerCancel",
                                 commandWidgetClass,
                                 deffactsmanager_form,
                                 args, 3);
  XtAddCallback(cancel, XtNcallback, CancelSelectPrimary, (XtPointer) deffactsmanager);

  XtPopup(deffactsmanager, XtGrabNonexclusive);
 
  }

/*******************************************************************************
          Name:        DeftemplateManagerCallback
          Description: Pop up the Deftemplate Manager
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void DeftemplateManagerCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget deftemplatemanager, deftemplatemanager_form,
         deftemplatemanager_viewport, remove, pprint,watch_label, cancel;
  static Widget watch = NULL;
  int itemCount = 0;
  char buffer[MAX_CHAR_IN_BUF];

  itemCount = IntGetDeftemplateList();

  if(item_list == NULL)
    {
    deftemplatemanager_flag = False;
    return;
    }

  deftemplatemanager_flag = True;

  /* ======================================== */
  /* Get the title for deffact manager window */
  /* ======================================== */
  sprintf(buffer,"Deftemplate Manager - %d Items",itemCount);

  /* ==================================================== */
  /*  Create the Deftemplate Mananaager window            */
  /* ==================================================== */

  deftemplatemanager = XtCreatePopupShell(buffer,
                                          topLevelShellWidgetClass,
                                          toplevel,
                                          NULL, 0);

  deftemplatemanager_form = XtCreateManagedWidget("manager_form",
                                                  formWidgetClass,
                                                  deftemplatemanager,
                                                  NULL, 0);

  XtSetArg(args[0],XtNallowHoriz,True);
  XtSetArg(args[1],XtNallowVert,True);
  deftemplatemanager_viewport = XtCreateManagedWidget("manager_viewport",
                                                      viewportWidgetClass,
                                                      deftemplatemanager_form,
                                                      args, 2);

  XtSetArg(args[0], XtNlist, item_list);
  manager_list = XtCreateManagedWidget("manager_list",
                                       listWidgetClass,
                                       deftemplatemanager_viewport,
                                       args, 1);
  /* ==================================================== */
  /*  Create the Remove button                            */
  /* ==================================================== */

  XtSetArg(args[0], XtNfromHoriz, deftemplatemanager_viewport);
  XtSetArg(args[1], XtNlabel, "Remove");
  remove = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 deftemplatemanager_form,
                                 args, 2);
  XtAddCallback(remove, XtNcallback, DeftemplateRemove,
                (XtPointer)manager_list);

  /* ==================================================== */
  /*  Create the Pprint button                            */
  /* ==================================================== */

  XtSetArg(args[1], XtNfromVert, remove);
  XtSetArg(args[2], XtNlabel, "Pprint");
  pprint = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 deftemplatemanager_form,
                                 args, 3);
  XtAddCallback(pprint, XtNcallback, DeftemplatePprint,
                (XtPointer)manager_list);

  /* ==================================================== */
  /*  Create the Watch button                             */
  /*===================================================g */

  XtSetArg(args[1], XtNfromVert, pprint);
  XtSetArg(args[2], XtNlabel," ");
  watch = XtCreateManagedWidget("managerButton",
                                 toggleWidgetClass,
                                 deftemplatemanager_form,
                                 args, 3); 
  XtAddCallback(watch,XtNcallback,DeftemplateWatchCallback,(XtPointer)manager_list);
  XtAddCallback(manager_list,XtNcallback,DeftemplateMngrCheckboxCallback,(XtPointer)watch);
  XtSetArg(args[0],XtNfromHoriz,watch);
  XtSetArg(args[1],XtNfromVert,pprint);
  XtSetArg(args[2],XtNlabel,"Watch");
  XtSetArg(args[3],XtNborderWidth,0);
  watch_label = XtCreateManagedWidget("checkBoxLabel",
                                 labelWidgetClass,
                                 deftemplatemanager_form,
                                 args,4);
  /* ==================================================== */
  /*  Create the Done button                              */
  /* ==================================================== */

  XtSetArg(args[0],XtNfromHoriz,deftemplatemanager_viewport);
  XtSetArg(args[1], XtNfromVert, watch);
  XtSetArg(args[2], XtNlabel, "Done");
  cancel = XtCreateManagedWidget("managerCancel",
                                 commandWidgetClass,
                                 deftemplatemanager_form,
                                 args, 3);
  XtAddCallback(cancel,XtNcallback,CancelSelectPrimary, (XtPointer)deftemplatemanager);

  XtPopup(deftemplatemanager, XtGrabNonexclusive);
  }

/*******************************************************************************
          Name:        DeffunctionManagerCallback
          Description: Pops up the Deffunction Manager
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void DeffunctionManagerCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget deffunctionmanager, deffunctionmanager_form,
         deffunctionmanager_viewport,watch_label, remove, pprint, cancel;

  static Widget watch;
  int itemCount = 0;
  char buffer[MAX_CHAR_IN_BUF];

  itemCount = IntGetDeffunctionList();

  if(item_list == NULL)
    {
      ClearParameters();
      deffunctionmanager_flag = False;
      return;
    }

  deffunctionmanager_flag = True;
 
 /* ============================================= */
  /* Get the title for deffunction manager window */
  /* ============================================ */
  sprintf(buffer,"Deffunction Manager -  %d Items",itemCount);

  /* ==================================================== */
  /*  Create the Deffunction Manager window               */
  /* ==================================================== */

  deffunctionmanager = XtCreatePopupShell(buffer,
                                          topLevelShellWidgetClass,
                                          toplevel,
                                          NULL, 0);

  deffunctionmanager_form = XtCreateManagedWidget("manager_form",
                                                  formWidgetClass,
                                                  deffunctionmanager,
                                                  NULL, 0);
  XtSetArg(args[0],XtNallowHoriz,True);
  XtSetArg(args[1],XtNallowVert,True);
  deffunctionmanager_viewport = XtCreateManagedWidget("manager_viewport",
                                                      viewportWidgetClass,
                                                      deffunctionmanager_form,
                                                      args, 2);

  XtSetArg(args[0], XtNlist, item_list);
  manager_list = XtCreateManagedWidget("manager_list",
                                       listWidgetClass,
                                       deffunctionmanager_viewport,
                                       args, 1);
  /* ==================================================== */
  /*  Create the Remove button                            */
  /* ==================================================== */

  XtSetArg(args[0], XtNfromHoriz, deffunctionmanager_viewport);
  XtSetArg(args[1], XtNlabel, "Remove");
  remove = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 deffunctionmanager_form,
                                 args, 2);
  XtAddCallback(remove, XtNcallback, DeffunctionRemoveCallback,
                (XtPointer)manager_list);
  /* ==================================================== */
  /*  Create the Pprint button                            */
  /* ==================================================== */

  XtSetArg(args[1], XtNfromVert, remove);
  XtSetArg(args[2], XtNlabel, "Pprint");
  pprint = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 deffunctionmanager_form,
                                 args, 3);
  XtAddCallback(pprint, XtNcallback, DeffunctionPprintCallback,
                (XtPointer)manager_list);

  /* ==================================================== */
  /*  Create the Watch button                            */
  /* ==================================================== */
  
  XtSetArg(args[1], XtNlabel, " ");
  XtSetArg(args[2],XtNfromVert,pprint);
  watch = XtCreateManagedWidget("managerButton",
                                toggleWidgetClass,
                                deffunctionmanager_form,
                                args,3);
  XtAddCallback(watch,XtNcallback,DeffunctionWatchCallback,(XtPointer)manager_list);
  XtAddCallback(manager_list,XtNcallback,DeffunctionMngrCheckboxCallback,(XtPointer)watch);
  XtSetArg(args[0], XtNfromHoriz,watch);
  XtSetArg(args[1], XtNlabel, "Watch");
  watch_label = XtCreateManagedWidget("checkBoxLabel",
                                labelWidgetClass,
                                deffunctionmanager_form,
                                args,3);

  /* ==================================================== */
  /*  Create the Cancel button                            */
  /* ==================================================== */

  XtSetArg(args[0], XtNfromHoriz,deffunctionmanager_viewport);
  XtSetArg(args[1], XtNfromVert, watch);
  XtSetArg(args[2], XtNlabel, "Done");
  cancel = XtCreateManagedWidget("managerCancel",
                                 commandWidgetClass,
                                 deffunctionmanager_form,
                                 args, 3);
  XtAddCallback(cancel, XtNcallback, CancelSelectPrimary,
                (XtPointer)deffunctionmanager);

  XtPopup(deffunctionmanager, XtGrabNonexclusive);
  }

/*******************************************************************************
          Name:        DefglobalManagerCallback
          Description: Pops up the Defglobal Manager
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void DefglobalManagerCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
{
   Widget defglobalmanager, defglobalmanager_form,
         defglobalmanager_viewport, remove, pprint, cancel;
   char buffer[MAX_CHAR_IN_BUF];
   int itemCount = 0;

  itemCount = IntGetDefglobalList();

  if(item_list == NULL)
    {
      ClearParameters();
      defglobalmanager_flag = False;
      return;
    }

  defglobalmanager_flag = True;
  /* ========================================== */
  /* Get the title for defglobal manager window */
  /* ========================================== */

  sprintf(buffer,"Defglobal Manager - %d Items",itemCount);

  /* ==================================================== */
  /*  Create the Defglobal Manager window                 */
  /* ==================================================== */

  defglobalmanager = XtCreatePopupShell(buffer,
                                          topLevelShellWidgetClass,
                                          toplevel,
                                          NULL, 0);

  defglobalmanager_form = XtCreateManagedWidget("manager_form",
                                                  formWidgetClass,
                                                  defglobalmanager,
                                                  NULL, 0);
  XtSetArg(args[0],XtNallowHoriz,True);
  XtSetArg(args[1],XtNallowVert,True);
  defglobalmanager_viewport = XtCreateManagedWidget("manager_viewport",
                                                      viewportWidgetClass,
                                                      defglobalmanager_form,
                                                      args, 2);

  XtSetArg(args[0], XtNlist, item_list);
  manager_list = XtCreateManagedWidget("manager_list",
                                       listWidgetClass,
                                       defglobalmanager_viewport,
                                       args, 1);
  /* ==================================================== */
  /*  Create the Remove button                            */
  /* ==================================================== */

  XtSetArg(args[0], XtNfromHoriz, defglobalmanager_viewport);
  XtSetArg(args[1], XtNlabel, "Remove");
  remove = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 defglobalmanager_form,
                                 args, 2);
  XtAddCallback(remove, XtNcallback, DefglobalRemoveCallback,
                (XtPointer)manager_list);
  /* ==================================================== */
  /*  Create the Pprint button                            */
  /* ==================================================== */

  XtSetArg(args[1], XtNfromVert, remove);
  XtSetArg(args[2], XtNlabel, "Pprint");
  pprint = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 defglobalmanager_form,
                                 args, 3);
  XtAddCallback(pprint, XtNcallback,DefglobalPprintCallback,
                (XtPointer)manager_list);
  /* ==================================================== */
  /*  Create the Cancel button                            */
  /* ==================================================== */

  XtSetArg(args[1], XtNfromVert, pprint);
  XtSetArg(args[2], XtNlabel, "Done");
  cancel = XtCreateManagedWidget("managerCancel",
                                 commandWidgetClass,
                                 defglobalmanager_form,
                                 args, 3);
  XtAddCallback(cancel, XtNcallback, CancelSelectPrimary,
                (XtPointer)defglobalmanager);

  XtPopup(defglobalmanager, XtGrabNonexclusive);


  }

/*******************************************************************************
          Name:        IntGetDefglobal
          Description: 
          Arguments:  
          Returns:     
*******************************************************************************/

IntGetDefglobalList()
{
  struct defglobal* defglPtr = NULL;
  int itemCount = 0,maxItems = 20;
  char *name;

  if((defglPtr = (struct defglobal*)GetNextDefglobal(NULL)) == NULL)
   {
      item_list = NULL;
      return(0);
   }
  item_list = (String*)calloc(maxItems,sizeof(String));
  while(defglPtr != NULL)
   {
     name = (char*)GetDefglobalName((VOID*)defglPtr);
     item_list[itemCount] = balloc(strlen(name) + 1,char);
     strcpy(item_list[itemCount++],name);
     if(itemCount == (maxItems - 1))
        {
          maxItems = maxItems * 2;
          item_list = (String *)realloc(item_list,maxItems * sizeof(String));
        }
     defglPtr = (struct defglobal*)GetNextDefglobal(defglPtr);

   }
  item_list[itemCount] = NULL;
  sortList(item_list,itemCount);
  return(itemCount);
}

/*******************************************************************************
          Name:        DefglobalRemoveCallback
          Description: 
          Arguments:  
          Returns:     
*******************************************************************************/
static void DefglobalRemoveCallback(w,client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
     Widget list_widget = (Widget)client_data;
     XawListReturnStruct *current = XawListShowCurrent(list_widget);


     if(current->list_index == XAW_LIST_NONE)
      return;
     MoveEndOfFile(dialog_text, &event);
     SetCommandString("(undefglobal ");
     PrintCLIPS("wclips","(undefglobal ");
     AppendCommandString(current->string);
     PrintCLIPS("wclips",current->string);
     AppendCommandString(")\n");
     PrintCLIPS("wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
     quit_get_event = True;
     list_change = True;

  }


/*******************************************************************************
          Name:       DefglobalPprintCallback 
          Description: 
          Arguments:  
          Returns:     
*******************************************************************************/
static  void DefglobalPprintCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {

  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);

  if(current->list_index == XAW_LIST_NONE ||
     GetDefglobalPPForm(FindDefglobal(current->string)) == NULL)
      return;
  MoveEndOfFile(dialog_text, &event);
  SetCommandString("(ppdefglobal ");
  PrintCLIPS("wclips","(ppdefglobal ");
  AppendCommandString(current->string);
  PrintCLIPS("wclips",current->string);
  AppendCommandString(")\n");
  PrintCLIPS("wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
  quit_get_event = True;

  }

/*******************************************************************************
          Name:        DefgenericManagerCallback
          Description: Pops up the Defgeneric Manager
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void DefgenericManagerCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget defgenericmanager, defgenericmanager_form, defgenericmanager_viewport,
          watch, watch_label,remove, pprint, methods, cancel;
  char buffer[MAX_CHAR_IN_BUF];
  int itemCount = 0;

  itemCount = IntGetDefgenericList();

  if(item_list == NULL)
    {
    ClearParameters();
    defgenericmanager_flag = False;
    release(curr_def_name);
    return;
    }

  defgenericmanager_flag = True;

  /* =========================================== */
  /* Get the title for defgeneric manager window */
  /* =========================================== */
  sprintf(buffer,"Defgeneric Manager - %d Items",itemCount);

  /* ==================================================== */
  /*  Create the Defgeneric Manager                       */
  /* ==================================================== */

  defgenericmanager = XtCreatePopupShell(buffer,
                                         topLevelShellWidgetClass,
                                         toplevel,
                                         NULL, 0);

  defgenericmanager_form = XtCreateManagedWidget("manager_form",
                                                 formWidgetClass,
                                                 defgenericmanager,
                                                 NULL, 0);
  XtSetArg(args[0],XtNallowHoriz,True);
  XtSetArg(args[1],XtNallowVert,True);
  defgenericmanager_viewport = XtCreateManagedWidget("manager_viewport",
                                                     viewportWidgetClass,
                                                     defgenericmanager_form,
                                                     args, 2);

  XtSetArg(args[0], XtNlist, item_list);
  manager_list = XtCreateManagedWidget("manager_list",
                                       listWidgetClass,
                                       defgenericmanager_viewport, 
                                       args, 1);
  /* ==================================================== */
  /*  Create the Remove button                            */
  /* ==================================================== */

  XtSetArg(args[0], XtNfromHoriz, defgenericmanager_viewport);
  XtSetArg(args[1], XtNlabel, "Remove");
  remove = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 defgenericmanager_form,
                                 args, 2);
  XtAddCallback(remove, XtNcallback, DefgenericRemoveCallback, (XtPointer)manager_list);

  /* ==================================================== */
  /*  Create the Pprint button                            */
  /* ==================================================== */

  XtSetArg(args[1], XtNfromVert, remove);
  XtSetArg(args[2], XtNlabel, "Pprint");
  pprint = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 defgenericmanager_form,
                                 args, 3);
  XtAddCallback(pprint, XtNcallback, DefgenericPprintCallback, (XtPointer)manager_list);

  /* ==================================================== */
  /*  Create the Methods button                           */
  /* ==================================================== */

  XtSetArg(args[1], XtNfromVert, pprint);
  XtSetArg(args[2], XtNlabel, "Methods...");
  methods = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 defgenericmanager_form,
                                 args, 3);
  XtAddCallback(methods,XtNcallback,DefgenericMethodCallback, (XtPointer)manager_list);

  /* ==================================================== */
  /*  Create the Watch button                            */
  /* ==================================================== */
  XtSetArg(args[1], XtNlabel, " ");
  XtSetArg(args[2],XtNfromVert,methods);
  watch = XtCreateManagedWidget("managerButton",
                                toggleWidgetClass,
                                defgenericmanager_form,
                                args,3);
  XtAddCallback(watch,XtNcallback,DefgenericWatchCallback,(XtPointer)manager_list);
  XtAddCallback(manager_list,XtNcallback,DefgenericMngrCheckBoxCallback,(XtPointer)watch);
  XtSetArg(args[0], XtNfromHoriz,watch);
  XtSetArg(args[1], XtNlabel, "Watch");
  watch_label = XtCreateManagedWidget("checkBoxLabel",
                                labelWidgetClass,
                                defgenericmanager_form,
                                args,3);

  /* ==================================================== */
  /*  Create the Done button                              */
  /* ==================================================== */
  XtSetArg(args[0], XtNfromHoriz,defgenericmanager_viewport);
  XtSetArg(args[1], XtNfromVert, watch);
  XtSetArg(args[2], XtNlabel, "Done");
  cancel = XtCreateManagedWidget("managerCancel",
                                 commandWidgetClass,
                                 defgenericmanager_form,
                                 args, 3);
  XtAddCallback(cancel, XtNcallback, CancelSelectPrimary,(XtPointer)defgenericmanager);

  XtPopup(defgenericmanager, XtGrabNonexclusive);
  }


/*******************************************************************************
          Name:        DefinstancesManagerCallback
          Description: Pops up the Definstances Manager
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void DefinstancesManagerCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget definstancesmanager, definstancesmanager_form,
         definstancesmanager_viewport, remove, pprint, cancel;
  char buffer[MAX_CHAR_IN_BUF];
  int itemCount = 0;

  itemCount = IntGetDefinstancesList();

  if(item_list == NULL)
    {
    ClearParameters();
    definstancesmanager_flag = False;
    return;
    }

  definstancesmanager_flag = True;
  /* =========================================== */
  /* Get the title for definstance manager window */
  /* =========================================== */
  sprintf(buffer,"Definstance Manager - %d Items",itemCount);

  /* ==================================================== */
  /*  Create the Definstances Manager window              */
  /* ==================================================== */

  definstancesmanager = XtCreatePopupShell(buffer,
                                           topLevelShellWidgetClass,
                                           toplevel,
                                           NULL, 0);

  definstancesmanager_form = XtCreateManagedWidget("manager_form",
                                                   formWidgetClass,
                                                   definstancesmanager,
                                                   NULL, 0);

  XtSetArg(args[0],XtNallowHoriz,True);
  XtSetArg(args[1],XtNallowVert,True);
  definstancesmanager_viewport = XtCreateManagedWidget("manager_viewport",
                                                       viewportWidgetClass,
                                                       definstancesmanager_form,
                                                       args, 2);

  XtSetArg(args[0], XtNlist, item_list);
  manager_list = XtCreateManagedWidget("manager_list",
                                       listWidgetClass,
                                       definstancesmanager_viewport,
                                       args, 1);
  XtSetArg(args[0], XtNfromHoriz, definstancesmanager_viewport);
  /* ==================================================== */
  /*  Create the Remove button                            */
  /* ==================================================== */

  XtSetArg(args[1], XtNlabel, "Remove");
  remove = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 definstancesmanager_form,
                                 args, 2);
  XtAddCallback(remove, XtNcallback, DefinstancesRemoveCallback,
                (XtPointer)manager_list);

  /* ==================================================== */
  /*  Create the Pprint button                            */
  /* ==================================================== */

  XtSetArg(args[1], XtNfromVert, remove);
  XtSetArg(args[2], XtNlabel, "Pprint");
  pprint = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 definstancesmanager_form,
                                 args, 3);
  XtAddCallback(pprint, XtNcallback, DefinstancesPprintCallback,
                (XtPointer)manager_list);

  /* ==================================================== */
  /*  Create the Done button                              */
  /* ==================================================== */

  XtSetArg(args[1], XtNfromVert, pprint);
  XtSetArg(args[2], XtNlabel, "Done");
  cancel = XtCreateManagedWidget("managerCancel",
                                 commandWidgetClass,
                                 definstancesmanager_form,
                                 args, 3);
  XtAddCallback(cancel, XtNcallback, CancelSelectPrimary,
                (XtPointer)definstancesmanager);

  XtPopup(definstancesmanager, XtGrabNonexclusive);
  }

/*******************************************************************************
          Name:        DefclassManagerCallback
          Description: Pops up the Defclass Manager
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void DefclassManagerCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget defclassmanager, defclassmanager_form, defclassmanager_viewport,
         remove,  describe, browse, pprint, message_handlers, watch_instances,
         watch_slot, watch_instances_label,watch_slot_label,cancel;
  static Widget CheckBoxes[3];
  char buffer[MAX_CHAR_IN_BUF];
  int itemCount = 0;
 
  itemCount = IntGetDefclassList();

  if(item_list == NULL)
    {
    ClearParameters();
    release(curr_def_name);
    defclassmanager_flag = False;
    return;
    }
  
  defclassmanager_flag = True;
  /* =========================================== */
  /* Get the title for defclass manager window   */
  /* =========================================== */
  sprintf(buffer,"Defclass Manager - %d Items",itemCount);

  /* =========================================== */
  /* Create the parent window for the defclass   */
  /* window manager                              */
  /* =========================================== */

  defclassmanager = XtCreatePopupShell(buffer,
                                       topLevelShellWidgetClass,
                                       toplevel,
                                       NULL, 0);

  defclassmanager_form = XtCreateManagedWidget("manager_form",
                                               formWidgetClass,
                                               defclassmanager,
                                               NULL, 0);

  /* =========================================== */
  /* Create the list widget for the defclass     */
  /* list                                        */
  /* =========================================== */

  XtSetArg(args[0],XtNallowHoriz,True);
  XtSetArg(args[1],XtNallowVert,True);
  defclassmanager_viewport = XtCreateManagedWidget("manager_viewport",
                                                   viewportWidgetClass,
                                                   defclassmanager_form,
                                                   args, 2);

  XtSetArg(args[0], XtNlist, item_list);
  manager_list = XtCreateManagedWidget("manager_list",
                                       listWidgetClass,
                                       defclassmanager_viewport,
                                       args, 1);

  /* =========================================== */
  /* Create the Remove button                    */
  /* =========================================== */

  XtSetArg(args[0], XtNfromHoriz, defclassmanager_viewport);
  XtSetArg(args[1], XtNlabel, "Remove");
  remove = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 defclassmanager_form,
                                 args, 2);
  XtAddCallback(remove, XtNcallback, DefclassRemoveCallback, (XtPointer)manager_list);

  /* =========================================== */
  /* Create the Describe button                  */
  /* =========================================== */

  XtSetArg(args[1], XtNfromVert, remove);
  XtSetArg(args[2], XtNlabel, "Describe");
  describe = XtCreateManagedWidget("managerButton",
                                   commandWidgetClass,
                                   defclassmanager_form,
                                   args, 3);
  XtAddCallback(describe, XtNcallback, DefclassDescribeCallback, (XtPointer)manager_list);

  /* =========================================== */
  /* Create the Browse button                    */
  /* =========================================== */

  XtSetArg(args[1], XtNfromVert, describe);
  XtSetArg(args[2], XtNlabel, "Browse");
  browse  = XtCreateManagedWidget("managerButton",
                                   commandWidgetClass,
                                   defclassmanager_form,
                                   args, 3);
  XtAddCallback(browse, XtNcallback, DefclassBrowseCallback, (XtPointer)manager_list);

  /* =========================================== */
  /* Create the Pretty Print button              */
  /* =========================================== */

  XtSetArg(args[1], XtNfromVert, browse);
  XtSetArg(args[2], XtNlabel, "Pprint");
  pprint = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 defclassmanager_form,
                                 args, 3);
  XtAddCallback(pprint, XtNcallback, DefclassPprintCallback, (XtPointer)manager_list);

  /* =========================================== */
  /* Create the Message Handler button           */
  /* =========================================== */

  XtSetArg(args[1], XtNfromVert, pprint);
  XtSetArg(args[2], XtNlabel, "Message Handlers...");
  message_handlers = XtCreateManagedWidget("managerButton",
                                           commandWidgetClass,
                                           defclassmanager_form,
                                           args, 3);
  XtAddCallback(message_handlers,XtNcallback, DefclassMessageHandlersCallback,(XtPointer)manager_list);
  /* ======================================================= */
  /* Create the toggle button for the Watch Instance button  */
  /* ======================================================= */

  XtSetArg(args[1], XtNfromVert, message_handlers);
  XtSetArg(args[2], XtNlabel," ");
  CheckBoxes[0] = watch_instances = XtCreateManagedWidget("managerButton",
                                           toggleWidgetClass,
                                           defclassmanager_form,
                                           args, 3);
  XtAddCallback(watch_instances,XtNcallback,WatchInstancesCallback,(XtPointer)manager_list);
  /* ======================================================= */
  /* Create the label button for the Watch Instance button  */
  /* ======================================================= */

  XtSetArg(args[1], XtNfromVert, message_handlers);
  XtSetArg(args[2], XtNlabel,"Watch Instances");
  XtSetArg(args[0],XtNfromHoriz,watch_instances);
  XtSetArg(args[3],XtNborderWidth,0);
  watch_instances_label = XtCreateManagedWidget("checkBoxLabel",
                                           labelWidgetClass,
                                           defclassmanager_form,
                                           args, 4);
  /* ======================================================= */
  /* Create the toggle button for the Watch Slot button      */
  /* ======================================================= */

  XtSetArg(args[0],XtNfromHoriz,defclassmanager_viewport);
  XtSetArg(args[1], XtNfromVert,watch_instances);
  XtSetArg(args[2], XtNlabel," ");
  CheckBoxes[1] = watch_slot =  XtCreateManagedWidget("managerButton",
                                           toggleWidgetClass,
                                           defclassmanager_form,
                                           args, 3);
  XtAddCallback(watch_slot,XtNcallback,WatchSlotCallback,(XtPointer)manager_list);
  /* ======================================================= */
  /* Create the label button for the Watch Slot              */
  /* ======================================================= */

  XtSetArg(args[0],XtNfromHoriz,watch_slot);
  XtSetArg(args[1], XtNfromVert,watch_instances);
  XtSetArg(args[2], XtNlabel,"Watch Slots");
  XtSetArg(args[3], XtNborderWidth,0);
  watch_slot_label =  XtCreateManagedWidget("checkBoxLabel",
                                           labelWidgetClass,
                                           defclassmanager_form,
                                           args,4);
  /* ======================================================= */
  /* Create the command button for the Done                  */
  /* ======================================================= */

  XtSetArg(args[0],XtNfromHoriz,defclassmanager_viewport);
  XtSetArg(args[1], XtNfromVert, watch_slot);
  XtSetArg(args[2], XtNlabel, "Done");
  cancel = XtCreateManagedWidget("managerCancel",
                                 commandWidgetClass,
                                 defclassmanager_form,
                                 args, 3);
  XtAddCallback(cancel, XtNcallback, CancelSelectPrimary, (XtPointer)defclassmanager);

  /* ======================================================= */
  /* Add the callback function to the manager list           */
  /* ======================================================= */

  XtAddCallback(manager_list,XtNcallback,DefclssMngrChckbxCallback,(XtPointer)CheckBoxes);

  XtPopup(defclassmanager, XtGrabNonexclusive);
  }


/*******************************************************************************
          Name:        AgendaManagerCallback
          Description: Pop up the Agenda Manager
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void AgendaManagerCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget agendamanager, agendamanager_form, agendamanager_viewport, remove,
         fire, cancel;
  char buffer[MAX_CHAR_IN_BUF];

  int itemCount = IntGetAgendaList();

  if(item_list == NULL)
    {
    ClearParameters();
    agendamanager_flag = False;
    return;
    }

  agendamanager_flag = True;
  /* =========================================== */
  /* Get the title for agenda manager window */
  /* =========================================== */
  sprintf(buffer,"Agenda Manager - %d Items",itemCount);

  /* ==================================================== */
  /*  Create the Agenda Manager window                    */
  /* ==================================================== */

  agendamanager = XtCreatePopupShell(buffer,
                                     topLevelShellWidgetClass,
                                     toplevel,
                                     NULL, 0);

  agendamanager_form = XtCreateManagedWidget("manager_form",
                                             formWidgetClass,
                                             agendamanager,
                                             NULL, 0);

  XtSetArg(args[0],XtNallowHoriz,True);
  XtSetArg(args[1],XtNallowVert,True);
  agendamanager_viewport = XtCreateManagedWidget("manager_viewport",
                                                 viewportWidgetClass,
                                                 agendamanager_form,
                                                 args, 2);

  XtSetArg(args[0], XtNlist, item_list);
  manager_list = XtCreateManagedWidget("manager_list",
                                       listWidgetClass,
                                       agendamanager_viewport,
                                       args, 1);

  /* ==================================================== */
  /*  Create the Remove button                            */
  /* ==================================================== */

  XtSetArg(args[0], XtNfromHoriz, agendamanager_viewport);
  XtSetArg(args[1], XtNlabel, "Remove");
  remove = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 agendamanager_form,
                                 args, 2);
  XtAddCallback(remove, XtNcallback, AgendaRemove, (XtPointer)manager_list);

  /* ==================================================== */
  /*  Create the Fire button                              */
  /* ==================================================== */

  XtSetArg(args[1], XtNfromVert, remove);
  XtSetArg(args[2], XtNlabel, "Fire");
  fire = XtCreateManagedWidget("managerButton",
                               commandWidgetClass,
                               agendamanager_form,
                               args, 3);
  XtAddCallback(fire, XtNcallback, AgendaFire,(XtPointer)manager_list);

  /* ==================================================== */
  /*  Create the Done button                              */
  /* ==================================================== */

  XtSetArg(args[1], XtNfromVert, fire);
  XtSetArg(args[2], XtNlabel, "Done");
  cancel = XtCreateManagedWidget("managerCancel",
                                 commandWidgetClass,
                                 agendamanager_form,
                                 args, 3);
  XtAddCallback(cancel, XtNcallback, CancelSelectPrimary, (XtPointer) agendamanager);

  XtPopup(agendamanager,  XtGrabNonexclusive);
  }

/*******************************************************************************
          Name:        IntGetAgendaList
          Description: Gets agenda list
          Arguments:  None
          Returns:
*******************************************************************************/
IntGetAgendaList()
  {
  int maxItems = 20,itemCount = 0;
  struct activation *act_ptr;
  char *name;
  char buffer[MAX_CHAR_IN_BUF];

  if((act_ptr = (struct activation *) GetNextActivation(NULL)) == NULL)
    {
      item_list = NULL;
      return(0);
    }
  item_list = (String *)calloc(maxItems,sizeof(String));
  while(act_ptr != NULL)
      {
      /*name = GetActivationName((VOID*)act_ptr);*/
      GetActivationPPForm(buffer,MAX_CHAR_IN_BUF - 1,act_ptr);
      item_list[itemCount] = balloc(strlen(buffer) + 1, char);
      strcpy(item_list[itemCount++],buffer);
      if(itemCount == (maxItems - 1))
        {
          maxItems = maxItems * 2;
          item_list = (String *)realloc(item_list,maxItems*sizeof(String));
        }
      act_ptr = (struct activation *) GetNextActivation(act_ptr);
      }
  item_list[itemCount] = NULL;
  /*sortList(item_list,itemCount);*/
  return(itemCount);
  }

/******************************************************************************
          Name:        AgendaRemove
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void AgendaRemove(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  register int i;
  struct activation *act_ptr;
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  act_ptr = (struct activation *) GetNextActivation(NULL);
  for (i = 0; i < current->list_index ; i++)
    act_ptr = (struct activation *) GetNextActivation(act_ptr);
  DeleteActivation((VOID*)act_ptr);

  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */

  quit_get_event = True; 
  list_change = True;
  }

/******************************************************************************
          Name:        AgendaFire
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void AgendaFire(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  register int i;
  struct activation *act_ptr;
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  act_ptr = (struct activation *) GetNextActivation(NULL);
  for (i = 0; i < current->list_index ; i++)
    act_ptr = (struct activation *) GetNextActivation(act_ptr);
  MoveActivationToTop((VOID*)act_ptr);
  MoveEndOfFile(dialog_text, &event);
  SetCommandString("(run 1)\n");
  PrintCLIPS("wclips","(run 1)\n");

  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */

  quit_get_event = True;
  list_change = True;
  }


/*******************************************************************************
          Name:        FactsWindowCallback
          Description: Called when Facts Window is selected from the Window menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void FactsWindowCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {

  if (Browse_status[FACT_WIN])
    {
    XtSetArg(args[0], XtNleftBitmap, None);
    XtPopdown(facts);
    }
  else if(facts_text != NULL)
   {
    XtPopup(facts,XtGrabNone);
    SetFactListChanged(CLIPS_FALSE);
    PrintChangedFacts();
    XtSetArg(args[0], XtNleftBitmap, checker);
   }
  else
   {
    CreateFactWindow();
    SetFactListChanged(CLIPS_FALSE);
    PrintChangedFacts();
    XtSetArg(args[0], XtNleftBitmap, checker);
    }

  XtSetValues(facts_window, args, 1);
  Browse_status[FACT_WIN] = !Browse_status[FACT_WIN];
  }

/**********************************************************************
 *    CreateFactWindow
 *
 **********************************************************************/

CreateFactWindow()
{
    Dimension height;
    int n = 0;
    char *name,labelBuffer[256];
    struct defmodule* theModule = (struct defmodule *)GetCurrentModule();


 /* Change the name of the window to the current module */

    if(theModule  != NULL)
     {
       name = GetDefmoduleName(theModule);
       strcpy(labelBuffer,"Facts Window(");
       strcat(labelBuffer,name);
       strcat(labelBuffer,")");
     }
     else
     {
       strcpy(labelBuffer,"Facts Window");
     }

    XtSetArg(args[n], XtNheight, &height);n++;
    XtGetValues(dialog, args, n);
    height = (Dimension)(height + 150) / 3;
    n = 0;
    XtSetArg(args[n], XtNheight, height);n++;
    facts = XtCreatePopupShell(labelBuffer,
                               topLevelShellWidgetClass,
                               toplevel,
                               args, n);
    n = 0;
    XtSetArg(args[n], XtNdefaultDistance, 0);n++;
    facts_form = XtCreateManagedWidget("facts_form",
                                       formWidgetClass,
                                       facts,
                                       args, n);
    n = 0;
    XtSetArg(args[n], XtNwidth, 250);n++;
    XtSetArg(args[n], XtNeditType, XawtextAppend);n++;
    XtSetArg(args[n], XtNscrollHorizontal, XawtextScrollAlways);n++;
    XtSetArg(args[n], XtNscrollVertical, XawtextScrollAlways);n++;
    facts_text = XtCreateManagedWidget("facts_text",
                                       asciiTextWidgetClass,
                                       facts_form,
                                       args, n);

    XtOverrideTranslations(facts_text,
                           XtParseTranslationTable(xclips_translation2));

    XtPopup(facts, XtGrabNone);

    if(!AddRouter("xfacts", 10, XclipsQuery, XclipsPrint, NULL, NULL, XclipsExit))
      {
      PrintCLIPS("werror", "Could not allocate xfacts router!\n");
      XclipsExit(0);
      ExitCLIPS(0);
      }

}
/*******************************************************************************
          Name:        AgendaWindowCallback
          Description: Called when Agenda Window is selected from the Window
                       menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void AgendaWindowCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
    Dimension width;
    extern int PrintChangedAgenda();

  if (Browse_status[AGENDA_WIN])
    {
     XtSetArg(args[0], XtNleftBitmap, None);
     XtPopdown(agenda);
    }
  else if(agenda != NULL)
   {
     XtPopup(agenda,XtGrabNone);
     SetAgendaChanged(CLIPS_FALSE);
     PrintChangedAgenda();
     XtSetArg(args[0], XtNleftBitmap,checker);
   }
  else
  {
    CreateAgendaWindow();
    SetAgendaChanged(CLIPS_FALSE);
    PrintChangedAgenda();
    XtSetArg(args[0], XtNleftBitmap, checker);
    }

  XtSetValues(agenda_window, args, 1);

  Browse_status[AGENDA_WIN] = !Browse_status[AGENDA_WIN];
  }
/*******************************************************************************
          Name:
          Description:
          Arguments:  None
          Returns:     None
*******************************************************************************/
CreateAgendaWindow()
{
    Dimension width;
    char *name,labelBuffer[256];
    struct defmodule* theModule = (struct defmodule *)GetCurrentModule();

    XtSetArg(args[0], XtNwidth, &width);
    XtGetValues(dialog, args, 1);
    XtSetArg(args[0], XtNwidth,(Dimension)(2 * width)/3);
    if(theModule  != NULL)
     {
       name = GetDefmoduleName(theModule);
       strcpy(labelBuffer,"Agenda Window(");
       strcat(labelBuffer,name);
       strcat(labelBuffer,")");
     }
    else
     {
       strcpy(labelBuffer,"Agenda Window");
     }
    agenda = XtCreatePopupShell(labelBuffer,
                                topLevelShellWidgetClass,
                                toplevel,
                                args, 1);

    XtSetArg(args[0], XtNdefaultDistance, 0);
    agenda_form = XtCreateManagedWidget("agenda_form",
                                        formWidgetClass,
                                        agenda,
                                        args, 1);

    XtSetArg(args[0], XtNheight, 150);
    XtSetArg(args[1], XtNeditType, XawtextAppend);
    XtSetArg(args[2], XtNscrollHorizontal, XawtextScrollAlways);
    XtSetArg(args[3], XtNscrollVertical, XawtextScrollAlways);
    agenda_text = XtCreateManagedWidget("agenda_text",
                                        asciiTextWidgetClass,
                                        agenda_form,
                                        args, 4);
    XtOverrideTranslations(agenda_text,
                           XtParseTranslationTable(xclips_translation2));

    XtPopup(agenda, XtGrabNone);

    if(!AddRouter("xagenda", 10, XclipsQuery, XclipsPrint,NULL,NULL,XclipsExit))
      {
      PrintCLIPS("werror", "Could not allocate xagenda router!\n");
      XclipsExit(0);
      ExitCLIPS(0);
      }
}
/*******************************************************************************
          Name:
          Description:
          Arguments: 
          Returns:     None
*******************************************************************************/
void FocusWindowCallback(w,client_data,call_data)
Widget w;
XtPointer call_data, client_data;
{
  if (Browse_status[FOCUS_WIN])
    {
    XtSetArg(args[0], XtNleftBitmap, None);
    XtPopdown(focus);
    }
  else if(focus != NULL)
   {
     XtPopup(focus,XtGrabNone);
     SetFocusChanged(CLIPS_FALSE);
     PrintChangedFocus();
     XtSetArg(args[0], XtNleftBitmap,checker);
   }
  else
   {
    CreateFocusWindow();
    SetFocusChanged(CLIPS_FALSE);
    PrintChangedFocus();
    XtSetArg(args[0], XtNleftBitmap, checker);
    }
  XtSetValues(focus_window, args, 1);
  Browse_status[FOCUS_WIN] = !Browse_status[FOCUS_WIN];
}

/*******************************************************************************
          Name:
          Description:
          Arguments:  
          Returns:     None
*******************************************************************************/
CreateFocusWindow()
{
    Dimension width;

    XtSetArg(args[0], XtNwidth, &width);
    XtGetValues(dialog, args, 1);
    XtSetArg(args[0], XtNwidth,width/3);
    focus = XtCreatePopupShell("Focus Window",
                                topLevelShellWidgetClass,
                                toplevel,
                                args, 1);

    XtSetArg(args[0], XtNdefaultDistance, 0);
    focus_form = XtCreateManagedWidget("agenda_form",
                                        formWidgetClass,
                                        focus,
                                        args, 1);

    XtSetArg(args[0], XtNheight, 150);
    XtSetArg(args[1], XtNeditType, XawtextAppend);
    XtSetArg(args[2], XtNscrollHorizontal, XawtextScrollAlways);
    XtSetArg(args[3], XtNscrollVertical, XawtextScrollAlways);
    focus_text = XtCreateManagedWidget("focus_text",
                                        asciiTextWidgetClass,
                                        focus_form,
                                        args, 4);
    XtOverrideTranslations(focus_text,
                           XtParseTranslationTable(xclips_translation2));

    XtPopup(focus, XtGrabNone);

    if(!AddRouter("xfocus", 10, XclipsQuery, XclipsPrint,NULL,NULL,XclipsExit))
      {
      PrintCLIPS("werror", "Could not allocate xfocus router!\n");
      XclipsExit(0);
      ExitCLIPS(0);
      }
}


/*******************************************************************************
          Name:        
          Description: 
          Arguments: 
          Returns:     None
*******************************************************************************/
void InstancesWindowCallback(w,client_data,call_data)
Widget w;
XtPointer call_data, client_data;
{
  if (Browse_status[INSTANCE_WIN])
    {
    XtSetArg(args[0], XtNleftBitmap, None);
    XtPopdown(instances);
    }
  else if(instances != NULL)
   {
     XtPopup(instances,XtGrabNone);
     SetInstancesChanged(CLIPS_FALSE);
     PrintChangedInstances();
     XtSetArg(args[0], XtNleftBitmap,checker);
   }
  else
   {
    CreateInstanceWindow();
    SetInstancesChanged(CLIPS_FALSE);
    PrintChangedInstances();
    XtSetArg(args[0], XtNleftBitmap, checker);
    }
  XtSetValues(instances_window, args, 1);
  Browse_status[INSTANCE_WIN] = !Browse_status[INSTANCE_WIN];
}

/**********************************************************************************
 *    CreateInstanceWindow
 **********************************************************************************/
CreateInstanceWindow()
{
   Dimension height;
   char *name,labelBuffer[256];
   struct defmodule* theModule = (struct defmodule *)GetCurrentModule();


 /* Change the name of the window to the current module */

    if(theModule  != NULL)
     {
       name = GetDefmoduleName(theModule);
       strcpy(labelBuffer,"Instances Window(");
       strcat(labelBuffer,name);
       strcat(labelBuffer,")");
     }
     else
     {
       strcpy(labelBuffer,"Instances Window");
     }

    XtSetArg(args[0], XtNheight, &height);
    XtGetValues(dialog, args, 1);
    height = (Dimension)(height + 150)/3;
    XtSetArg(args[0], XtNheight, height);
    instances = XtCreatePopupShell(labelBuffer,
                               topLevelShellWidgetClass,
                               toplevel,
                               args, 1);

    XtSetArg(args[0], XtNdefaultDistance, 0);
    instances_form = XtCreateManagedWidget("instances_form",
                                       formWidgetClass,
                                       instances,
                                       args, 1);

    XtSetArg(args[0], XtNwidth, 250);
    XtSetArg(args[1], XtNeditType, XawtextAppend);
    XtSetArg(args[2], XtNscrollHorizontal,XawtextScrollAlways);
    XtSetArg(args[3], XtNscrollVertical, XawtextScrollAlways);
    instances_text = XtCreateManagedWidget("instances_text",
                                       asciiTextWidgetClass,
                                       instances_form,
                                       args, 4);

    XtOverrideTranslations(instances_text,
                           XtParseTranslationTable(xclips_translation2));

    XtPopup(instances, XtGrabNone);

    if(!AddRouter("xinstances", 10, XclipsQuery, XclipsPrint, NULL, NULL, XclipsExit))
      {
      PrintCLIPS("werror", "Could not allocate xinstances router!\n");
      XclipsExit(0);
      ExitCLIPS(0);
      }
}

/*******************************************************************************
          Name:
          Description:
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void GlobalsWindowCallback(w,client_data,call_data)
Widget w;
XtPointer call_data, client_data;
{

  if (Browse_status[GLOBAL_WIN])
    {
    XtPopdown(globals);
    XtSetArg(args[0], XtNleftBitmap, None);
    }
  else if(globals != NULL)
   {
     XtPopup(globals,XtGrabNone);
     SetGlobalsChanged(CLIPS_FALSE);
     PrintChangedGlobals();
     XtSetArg(args[0], XtNleftBitmap,checker);
   }
  else
    {
     CreateGlobalWindow();
     SetGlobalsChanged(CLIPS_FALSE);
     PrintChangedGlobals();
     XtSetArg(args[0], XtNleftBitmap, checker);
    }

  XtSetValues(globals_window, args, 1);

  Browse_status[GLOBAL_WIN] = !Browse_status[GLOBAL_WIN];

}
/**********************************************************************************
 *    CreateGlobalWindow
 **********************************************************************************/

CreateGlobalWindow()
{
    
   Dimension height;
   char *name,labelBuffer[256];
   struct defmodule* theModule = (struct defmodule *)GetCurrentModule();


 /* Change the name of the window to the current module */

    if(theModule  != NULL)
     {
       name = GetDefmoduleName(theModule);
       strcpy(labelBuffer,"Globals Window(");
       strcat(labelBuffer,name);
       strcat(labelBuffer,")");
     }
     else
     {
       strcpy(labelBuffer,"Globals Window");
     }

    XtSetArg(args[0], XtNheight, &height);
    XtGetValues(dialog, args, 1);
    height = (Dimension)(height + 150)/3;
    XtSetArg(args[0], XtNheight, height);
    globals = XtCreatePopupShell(labelBuffer,
                               topLevelShellWidgetClass,
                               toplevel,
                               args, 1);

    XtSetArg(args[0], XtNdefaultDistance, 0);
    globals_form = XtCreateManagedWidget("globals_form",
                                       formWidgetClass,
                                       globals,
                                       args, 1);

    XtSetArg(args[0], XtNwidth, 250);
    XtSetArg(args[1], XtNeditType, XawtextAppend);
    XtSetArg(args[2], XtNscrollHorizontal,XawtextScrollAlways);
    XtSetArg(args[3], XtNscrollVertical, XawtextScrollAlways);
    globals_text = XtCreateManagedWidget("globals_text",
                                       asciiTextWidgetClass,
                                       globals_form,
                                       args, 4);

    XtOverrideTranslations(globals_text,
                           XtParseTranslationTable(xclips_translation2));

    XtPopup(globals, XtGrabNone);

    if(!AddRouter("xglobals", 10, XclipsQuery, XclipsPrint, NULL, NULL, XclipsExit))
      {
      PrintCLIPS("werror", "Could not allocate xglobals router!\n");
      XclipsExit(0);
      ExitCLIPS(0);
      }
}

/*******************************************************************************
          Name:		AllWindowsCallback
          Description:	This function turn all the browse flags to True,
                        create all the windows and put the check mark
                        in front of the items of the window menu
          Arguments:
          Returns:
*******************************************************************************/
void AllWindowsCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
    int n = 0;

    /* ============================================================ *
     *  If the fact window has not been created then create it      *
     * ============================================================ */

    if(!Browse_status[FACT_WIN])
     {
       if(facts != NULL)
         XtPopup(facts,XtGrabNone);
       else
         CreateFactWindow();
       XtSetArg(args[n],XtNleftBitmap,checker);n++;
       XtSetValues(facts_window,args,n);       
       Browse_status[FACT_WIN] = !Browse_status[FACT_WIN];
       PrintChangedFacts();
     }
    /* ============================================================ *
     *  If the agenda window has not been created then create it    *
     * ============================================================ */

    if(!Browse_status[AGENDA_WIN])
     {
       if(agenda != NULL)
        XtPopup(agenda,XtGrabNone);
       else
        CreateAgendaWindow();
       XtSetArg(args[n],XtNleftBitmap,checker);n++;
       XtSetValues(agenda_window,args,n);
       Browse_status[AGENDA_WIN] = !Browse_status[AGENDA_WIN];
       PrintChangedAgenda();
     }
    /* ============================================================ *
     *  If the instance window has not been created then create it  *
     * ============================================================ */

    if(!Browse_status[INSTANCE_WIN])
     {
       
       if(instances != NULL)
        XtPopup(instances,XtGrabNone);
       else
        CreateInstanceWindow();
       XtSetArg(args[n],XtNleftBitmap,checker);n++;
       XtSetValues(instances_window,args,n);
       Browse_status[INSTANCE_WIN] = !Browse_status[INSTANCE_WIN];
       PrintChangedInstances();
     }
    /* ============================================================ *
     *  If the global window has not been created then create it    *
     * ============================================================ */

    if(!Browse_status[GLOBAL_WIN])
     {
       if(globals != NULL)
        XtPopup(globals,XtGrabNone);
       else
        CreateGlobalWindow();
       XtSetArg(args[n],XtNleftBitmap,checker);n++;
       XtSetValues(globals_window,args,n);
       Browse_status[GLOBAL_WIN]  = !Browse_status[GLOBAL_WIN];
       PrintChangedGlobals();
    }
    /* ============================================================ *
     *  If the focus window has not been created then create it     *
     * ============================================================ */

    if(!Browse_status[FOCUS_WIN])
    {
       if(focus != NULL)
        XtPopup(focus,XtGrabNone);
       else
        CreateFocusWindow();
       XtSetArg(args[n],XtNleftBitmap,checker);n++;
       XtSetValues(focus_window,args,n);
       Browse_status[FOCUS_WIN] = !Browse_status[FOCUS_WIN];
       PrintChangedFocus();
    }
  }

/*******************************************************************************
          Name:		NoWindowsCallback
          Description:  This fucntion will pop down all of the windows
          Arguments:    Widget w - Unused
                        XtPointer client_data, call_data - Unsused
          Returns:	none
*******************************************************************************/
void NoWindowsCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
    int n = 0;

    XtSetArg(args[n], XtNleftBitmap, None);    n++;

    if (Browse_status[GLOBAL_WIN])
     {
       XtPopdown(globals);
       XtSetValues(globals_window,args,n);
       Browse_status[GLOBAL_WIN] = !Browse_status[GLOBAL_WIN];
     }
    if (Browse_status[INSTANCE_WIN])
     {
       XtPopdown(instances);
       XtSetValues(instances_window,args,n);
       Browse_status[INSTANCE_WIN] = !Browse_status[INSTANCE_WIN];
     }     
    if(Browse_status[FACT_WIN])
     {
       XtPopdown(facts);
       XtSetValues(facts_window,args,n);
       Browse_status[FACT_WIN] = !Browse_status[FACT_WIN]; 
     }
    if(Browse_status[AGENDA_WIN])
     {
       XtPopdown(agenda);
       XtSetValues(agenda_window,args,n);
       Browse_status[AGENDA_WIN] = !Browse_status[AGENDA_WIN];
     }
    if(Browse_status[FOCUS_WIN])
     {
       XtPopdown(focus);
       XtSetValues(focus_window,args,n);
       Browse_status[FOCUS_WIN] = !Browse_status[FOCUS_WIN];
     }

  }


/*******************************************************************************
          Name:        CommandLineCLIPSCallback
          Description: Called when Command Line CLIPS is selected from the
                       Window menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void CommandLineCLIPSCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  system("xterm -e clips &");
  }

/*******************************************************************************
          Name:        ColorUtilityCallback
          Description: Called when Color is selected from the Window menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void ColorUtilityCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  system("color&");
  }

/*******************************************************************************
          Name:        IntGetDefruleLis
          Description: Gets the list of rules
          Arguments:  None
          Returns:
*******************************************************************************/
IntGetDefruleLis()
{
  struct defrule *rule_ptr;
  int maxItems = 20,itemCount = 0;
  char *name;

  if((rule_ptr = (struct defrule *) GetNextDefrule(NULL)) == NULL)
   {
     item_list = NULL;
     return(0);
   }
  item_list = (String *)calloc(maxItems,sizeof(String));
  while(rule_ptr != NULL)
      {
         name = GetDefruleName((VOID*)rule_ptr);
         item_list[itemCount] = balloc(strlen(name) + 1,char);
         strcpy(item_list[itemCount], name);
         itemCount++;
         if(itemCount == (maxItems -1))
          {
            maxItems = 2*maxItems;
            item_list = (String *)realloc(item_list,maxItems * sizeof(String));
          } 
         rule_ptr = (struct defrule *) GetNextDefrule((VOID*)rule_ptr);
      }
  item_list[itemCount] = NULL;
  sortList(item_list,itemCount);
  return(itemCount);
  }

/******************************************************************************
          Name:        DefruleRemoveCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data - not used
          Returns:     None
*******************************************************************************/
static void DefruleRemoveCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);

  if(current->list_index == XAW_LIST_NONE)
    return;
  MoveEndOfFile(dialog_text, &event);
  SetCommandString("(excise ");
  PrintCLIPS("wclips","(excise ");
  AppendCommandString(current->string);
  PrintCLIPS("wclips",current->string);
  AppendCommandString(")\n");
  PrintCLIPS("wclips",")\n");

  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */

  quit_get_event = True;
  list_change = True;
  }

/******************************************************************************
          Name:        DefruleMatchesCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DefruleMatchesCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  MoveEndOfFile(dialog_text, &event);
  SetCommandString("(matches ");
  PrintCLIPS("wclips","(matches ");
  AppendCommandString(current->string);
  PrintCLIPS("wclips",current->string);
  AppendCommandString(")\n");
  PrintCLIPS("wclips",")\n");

  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */

  quit_get_event = True;
  }

/******************************************************************************
          Name:        DefrulePprintCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DefrulePprintCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE ||
     GetDefrulePPForm(FindDefrule(current->string)) == NULL)
      return;
  MoveEndOfFile(dialog_text, &event);
  SetCommandString("(ppdefrule ");
  PrintCLIPS("wclips","(ppdefrule ");
  AppendCommandString(current->string);
  PrintCLIPS("wclips",current->string);
  AppendCommandString(")\n");
  PrintCLIPS("wclips",")\n");

  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */

  quit_get_event = True;
  }

/******************************************************************************
          Name:        DefruleSetBreakCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DefruleSetBreakCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  MoveEndOfFile(dialog_text, &event);
  SetCommandString("(set-break ");
  /*PrintCLIPS("wclips","(set-break ");*/
  AppendCommandString(current->string);
  /*PrintCLIPS("wclips",current->string);*/
  AppendCommandString(")\n");
  /*PrintCLIPS("wclips",")\n");*/

  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */

  quit_get_event = True;
  }

/******************************************************************************
          Name:        DefruleRemoveBreakCallback
          Description: Calls CLIPS remove-break command
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DefruleRemoveBreakCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  MoveEndOfFile(dialog_text, &event);
  SetCommandString("(remove-break ");
  /*PrintCLIPS("wclips","(remove-break ");*/
  AppendCommandString(current->string);
  /*PrintCLIPS("wclips",current->string);*/
  AppendCommandString(")\n");
  /*PrintCLIPS("wclips",")\n");*/

  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */

  quit_get_event = True;
  }

/******************************************************************************
          Name:        DefruleRefreshCallback
          Description: Calls CLIPS refresh command
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DefruleRefreshCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  MoveEndOfFile(dialog_text, &event);
  SetCommandString("(refresh ");
  PrintCLIPS("wclips","(refresh ");
  AppendCommandString(current->string);
  PrintCLIPS("wclips",current->string);
  AppendCommandString(")\n");
  PrintCLIPS("wclips",")\n");
  quit_get_event = True;
  }

/*******************************************************************************
          Name:        IntGetFactList
          Description: Gets list of facts
          Arguments:  None
          Returns:
*******************************************************************************/
IntGetFactList()
{
  struct deffacts *fact_ptr;
  int maxItems = 20,itemCount = 0;
  char *name;

  if((fact_ptr = (struct deffacts *) GetNextDeffacts(NULL)) == NULL)
    {
      item_list = NULL;
      return(0);
    }
  item_list = (String*)calloc(maxItems,sizeof(String));
  while(fact_ptr != NULL)
     {
      name = (char*)GetDeffactsName(fact_ptr);
      item_list[itemCount] = balloc(strlen(name) + 1,char);
      strcpy(item_list[itemCount],name);
      itemCount += 1;
      if(itemCount == (maxItems - 1))
       {
         maxItems = 2 * (maxItems);
         item_list = (String*)realloc(item_list,maxItems * sizeof(String));
       }
      fact_ptr = (struct deffacts *) GetNextDeffacts((VOID*)fact_ptr);
      }
    item_list[itemCount] = NULL;
    sortList(item_list,itemCount);
    return(itemCount);
  }

/******************************************************************************
          Name:        DeffactsRemove
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DeffactsRemove(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  MoveEndOfFile(dialog_text, &event);
  SetCommandString("(undeffacts ");
  PrintCLIPS("wclips","(undeffacts ");
  AppendCommandString(current->string);
  PrintCLIPS("wclips",current->string);
  AppendCommandString(")\n");
  PrintCLIPS("wclips",")\n");

  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */

  quit_get_event = True;
  list_change = True;
  }

/******************************************************************************
          Name:        DeffactsPprint
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DeffactsPprint(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  MoveEndOfFile(dialog_text, &event);
  SetCommandString("(ppdeffacts ");
  PrintCLIPS("wclips","(ppdeffacts ");
  AppendCommandString(current->string);
  PrintCLIPS("wclips",current->string);
  AppendCommandString(")\n");
  PrintCLIPS("wclips",")\n");

  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
  quit_get_event = True;
  }

/*******************************************************************************
          Name:        IntGetDeftemplateList
          Description: Gets list of deftemplates
          Arguments:  None
          Returns:
*******************************************************************************/
IntGetDeftemplateList()
  {

  struct deftemplate *dtmpl_ptr;
  int itemCount = 0,maxItems = 20;
  char *name;

  if((dtmpl_ptr = (struct deftemplate *) GetNextDeftemplate(NULL)) == NULL)
   {
     item_list = NULL;
     return(0);
   }
  item_list = (String *)calloc(maxItems,sizeof(String));
  while(dtmpl_ptr != NULL)
    {
      name = GetDeftemplateName((struct constructHeader *)dtmpl_ptr);
      item_list[itemCount] = balloc(strlen(name) + 1, char);
      strcpy(item_list[itemCount++],name);
      if(itemCount == (maxItems - 1))
       {
         maxItems = 2 * maxItems;
         item_list = (String*)realloc(item_list,maxItems * sizeof(String));
       }
      dtmpl_ptr = (struct deftemplate *) GetNextDeftemplate((VOID*)dtmpl_ptr);
    }
  item_list[itemCount] = NULL;
  sortList(item_list,itemCount);
  return(itemCount); 
  }

/******************************************************************************
          Name:        DeftemplateRemove
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DeftemplateRemove(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  MoveEndOfFile(dialog_text, &event);
  SetCommandString("(undeftemplate ");
  PrintCLIPS("wclips","(undeftemplate ");
  AppendCommandString(current->string);
  PrintCLIPS("wclips",current->string);
  AppendCommandString(")\n");
  PrintCLIPS("wclips",")\n");

  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */

  quit_get_event = True;
  list_change = True;
  }


/******************************************************************************
          Name:        DeftemplatePprint
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DeftemplatePprint(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  MoveEndOfFile(dialog_text, &event);
  SetCommandString("(ppdeftemplate ");
  PrintCLIPS("wclips","(ppdeftemplate ");
  AppendCommandString(current->string);
  PrintCLIPS("wclips",current->string);
  AppendCommandString(")\n");
  PrintCLIPS("wclips",")\n");

  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
  quit_get_event = True;
  }

/*******************************************************************************
          Name:        IntGetDeffunctionList
          Description: Gets list of deffunctions
          Arguments:  None
          Returns:
*******************************************************************************/
IntGetDeffunctionList()
  {
  DEFFUNCTION * dfunc_ptr;
  int itemCount = 0,maxItems = 20;
  char *name;

  if((dfunc_ptr = (DEFFUNCTION *) GetNextDeffunction(NULL)) == NULL)
    {
      item_list = NULL;
      return(0);
    }
  item_list = (String *)calloc(maxItems,sizeof(String));
  while(dfunc_ptr != NULL)
   {
     name = GetDeffunctionName((VOID *) dfunc_ptr);
     item_list[itemCount] = balloc(strlen(name) + 1, char); 
     strcpy(item_list[itemCount++],name);
     if(itemCount == (maxItems - 1))
        {
          maxItems = maxItems * 2;
          item_list = (String *)realloc(item_list,maxItems * sizeof(String));
        }
     dfunc_ptr = (DEFFUNCTION *) GetNextDeffunction((VOID*)dfunc_ptr);  
   }
  item_list[itemCount] = NULL;
  sortList(item_list,itemCount);
  return(itemCount);
  }

/******************************************************************************
          Name:        DeffunctionRemoveCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DeffunctionRemoveCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  MoveEndOfFile(dialog_text, &event);
  SetCommandString("(undeffunction ");
  PrintCLIPS("wclips","(undeffunction ");
  AppendCommandString(current->string);
  PrintCLIPS("wclips",current->string);
  AppendCommandString(")\n");
  PrintCLIPS("wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
  quit_get_event = True;
  list_change = True;
  }


/******************************************************************************
          Name:        DeffunctionPprintCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DeffunctionPprintCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);

  if(current->list_index == XAW_LIST_NONE ||
     GetDeffunctionPPForm(FindDeffunction(current->string)) == NULL)
      return;
  MoveEndOfFile(dialog_text, &event);
  SetCommandString("(ppdeffunction ");
  PrintCLIPS("wclips","(ppdeffunction ");
  AppendCommandString(current->string);
  PrintCLIPS("wclips",current->string);
  AppendCommandString(")\n");
  PrintCLIPS("wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
  quit_get_event = True;
  }

/*******************************************************************************
          Name:        IntGetDefgenericList
          Description: Gets list of defgenerics
          Arguments:  None
          Returns:
*******************************************************************************/
IntGetDefgenericList()
  {
  int maxItems = 20,itemCount = 0;
  struct generic_func *generic_func_ptr;
  char *name;

  if((generic_func_ptr = (struct generic_func *) GetNextDefgeneric(NULL)) == NULL)
    {
       item_list  =  NULL;
       return(0);
    }
  item_list = (String *)calloc(maxItems,sizeof(String));
  while( generic_func_ptr != NULL)
   {
      name = (char*)GetDefgenericName(generic_func_ptr);
      item_list[itemCount] = balloc(strlen(name) + 1, char);
      strcpy(item_list[itemCount++],name);
      if(itemCount == (maxItems - 1))
       {
         maxItems = maxItems * 2;
         item_list = (String *)realloc(item_list,maxItems*sizeof(String));
       }
      generic_func_ptr = (struct generic_func *) GetNextDefgeneric((VOID*)generic_func_ptr);
   }
  item_list[itemCount] = NULL;
  sortList(item_list,itemCount);
  return(itemCount);
  }

/******************************************************************************
          Name:        DefgenericRemoveCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DefgenericRemoveCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  MoveEndOfFile(dialog_text, &event);
  SetCommandString("(undefgeneric ");
  PrintCLIPS("wclips","(undefgeneric ");
  AppendCommandString(current->string);
  PrintCLIPS("wclips",current->string);
  AppendCommandString(")\n");
  PrintCLIPS("wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
  quit_get_event = True;
  list_change = True;
  }


/******************************************************************************
          Name:        DefgenericPprintCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DefgenericPprintCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE ||
     GetDefgenericPPForm(FindDefgeneric(current->string)) == NULL)
      return;
  MoveEndOfFile(dialog_text, &event);
  SetCommandString("(ppdefgeneric ");
  PrintCLIPS("wclips","(ppdefgeneric ");
  AppendCommandString(current->string);
  PrintCLIPS("wclips",current->string);
  AppendCommandString(")\n");
  PrintCLIPS("wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
  quit_get_event = True;
  }

/******************************************************************************
        Name:           DefgenericWatchCallback
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void DefgenericWatchCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
{

  VOID* defgenericPtr = NULL;
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);

  if(current->list_index == XAW_LIST_NONE)
      return;
  defgenericPtr = FindDefgeneric(current->string);
  SetDefgenericWatch(!GetDefgenericWatch(defgenericPtr),defgenericPtr);
}

/******************************************************************************
        Name:           DefgenericMngrCheckBoxCallback
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void DefgenericMngrCheckBoxCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
{
   VOID* defgenericPtr = NULL;
   Widget checkbox = (Widget)client_data;
   XawListReturnStruct *current = XawListShowCurrent(w);

  if(current->list_index == XAW_LIST_NONE)
      return;

  defgenericPtr = FindDefgeneric(current->string);
  XtSetArg(args[0],XtNstate,GetDefgenericWatch(defgenericPtr));
  XtSetValues(checkbox,args,1);

}


/******************************************************************************
          Name:        DefgenericMethodCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DefgenericMethodCallback(w,client_data,call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  
  
  Widget defmethodmanager,  defmethodmanager_form,  defmethodmanager_viewport,
         remove, pprint, cancel,watch_label;
  static Widget watch;
  char title[MAX_CHAR_IN_BUF];
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);
  int itemCount = 0;
   
 
  if(current->list_index == XAW_LIST_NONE)
      return;

  /* ========================================== */
  /*   Get the defmethod list                   */
  /* ========================================== */

  itemCount = IntGetDefmethodList(current->string);
  if(item_list1 == NULL)
    {
      defmethodmanager_flag = False;
      return;
    }
   curr_def_name = balloc(strlen(current->string) +  1,char);
   strcpy(curr_def_name,current->string);
   defmethodmanager_flag = True;
   title[0] = 0;
   sprintf(title,"%s Defmethod Manager - %d Items",curr_def_name,itemCount);
   
   /* =========================================== */
   /*   Create the defmethod manager window       */
   /* =========================================== */

   defmethodmanager = XtCreatePopupShell(title,
                                         topLevelShellWidgetClass,
                                         toplevel,
                                         NULL, 0);

   defmethodmanager_form = XtCreateManagedWidget("manager_form",
                                                 formWidgetClass,
                                                  defmethodmanager,
                                                 NULL, 0);

   XtSetArg(args[0],XtNallowHoriz,True);
   XtSetArg(args[1],XtNallowVert,True);
   defmethodmanager_viewport = XtCreateManagedWidget("manager_viewport",
                                                     viewportWidgetClass,
                                                      defmethodmanager_form,
                                                     args, 2);

   XtSetArg(args[0], XtNlist, item_list1);
   manager_list1 = XtCreateManagedWidget("manager_list",
                                       listWidgetClass,
                                        defmethodmanager_viewport,
                                       args, 1);
  /* ============================================= */
  /*    Create the Pprint button                   */
  /* ============================================= */

  XtSetArg(args[0], XtNfromHoriz,  defmethodmanager_viewport);
  XtSetArg(args[1], XtNlabel, "Remove");
  remove = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                  defmethodmanager_form,
                                 args, 2);
  XtAddCallback(remove,XtNcallback,RemoveDefmethodCallback,(XtPointer)manager_list1);  

  XtSetArg(args[1], XtNfromVert, remove);
  XtSetArg(args[2], XtNlabel, "Pprint");
  pprint = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                  defmethodmanager_form,
                                 args, 3);
  XtAddCallback(pprint,XtNcallback,DefmethodPprintCallback,(XtPointer)manager_list1);

  /* ============================================= */
  /*  Create the Watch button                      */
  /* ============================================= */

  XtSetArg(args[1], XtNfromVert,pprint );
  XtSetArg(args[2], XtNlabel, " ");
  watch = XtCreateManagedWidget("managerButton",
                                 toggleWidgetClass,
                                  defmethodmanager_form,
                                 args, 3);
  XtAddCallback(watch,XtNcallback,DefmethodWatchCallback,(XtPointer)manager_list1);
  XtAddCallback(manager_list1,XtNcallback,DefmethodMngrCheckBoxCallback,(XtPointer)watch);
  XtSetArg(args[2], XtNlabel,"Watch");
  XtSetArg(args[0],XtNfromHoriz,watch);
  watch_label = XtCreateManagedWidget("checkBoxLabel",
                                      labelWidgetClass,
                                      defmethodmanager_form,
                                      args,3);

  /* ============================================= */
  /*   Create the DOne button                      */
  /* ============================================= */

  XtSetArg(args[0], XtNfromHoriz,  defmethodmanager_viewport);
  XtSetArg(args[1], XtNfromVert, watch);
  XtSetArg(args[2], XtNlabel, "Done");
  cancel = XtCreateManagedWidget("managerCancel",
                                 commandWidgetClass,
                                  defmethodmanager_form,
                                 args, 3);
  XtAddCallback(cancel, XtNcallback, CancelSelectSecondary,
                (XtPointer) defmethodmanager);

  XtPopup(defmethodmanager, XtGrabNonexclusive);

  }

/******************************************************************************
          Name:        RemoveDefmethodCallback
          Description: removes aa method from the method list
          Arguments:  w - not used
                       client_data - not used
                       call_data - not used
          Returns:     None
*******************************************************************************/
static void RemoveDefmethodCallback(w,client_data,call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
    char index[5];
    int i;
    Widget aList = (Widget)client_data;
    XawListReturnStruct *current = XawListShowCurrent(aList);

    if(current->list_index == XAW_LIST_NONE)
      return;
    for (i = 0; ('0' <= current->string[i]) && (current->string[i] <= '9');i++)
      index[i] = current->string[i];

    index[i] = 0;
    MoveEndOfFile(dialog_text, &event);
    SetCommandString("(undefmethod  ");
    PrintCLIPS("wclips","(undefmethod ");
    AppendCommandString(curr_def_name);
    PrintCLIPS("wclips",curr_def_name);
    AppendCommandString(" ");
    PrintCLIPS("wclips"," ");
    AppendCommandString(index);
    PrintCLIPS("wclips",index);
    AppendCommandString(")\n");
    PrintCLIPS("wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
    quit_get_event = True;
    list1_change = True;

  }
/******************************************************************************
          Name:        DefmethodPprintCallback
          Description: Print the method
          Arguments:  w - not used
                       client_data - the list widget
                       call_data - not used
          Returns:     None
*******************************************************************************/
static void DefmethodPprintCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;

{
    char index[5];
    int i;
    unsigned methodIndex;
    Widget aList = (Widget)client_data;
    XawListReturnStruct *current = XawListShowCurrent(aList);

    if(current->list_index == XAW_LIST_NONE)
      return;
    for (i = 0; ('0' <= current->string[i]) && (current->string[i] <= '9');i++)
      index[i] = current->string[i];

    index[i] = 0;
    methodIndex = (unsigned)atoi(index);
    if(GetDefmethodPPForm(FindDefgeneric(curr_def_name),methodIndex) == NULL)
      return;
    
    MoveEndOfFile(dialog_text, &event);
    AppendCommandString("(ppdefmethod ");
    PrintCLIPS("wclips","(ppdefmethod ");
    AppendCommandString(curr_def_name);
    PrintCLIPS("wclips",curr_def_name);
    AppendCommandString(" ");
    PrintCLIPS("wclips"," ");
    AppendCommandString(index);
    PrintCLIPS("wclips",index);
    AppendCommandString(")\n");
    PrintCLIPS("wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
    quit_get_event = True;

}

/*******************************************************************************
          Name:        
          Description:
          Arguments: 
          Returns:
*******************************************************************************/
static void DefmethodWatchCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;

{
    char index[5];
    int i;
    unsigned MethodIndex;
    VOID* defgenericPtr = NULL;
    Widget aList = (Widget)client_data;
    XawListReturnStruct *current = XawListShowCurrent(client_data);

    if(current->list_index == XAW_LIST_NONE)
      return;
    for (i = 0; ('0' <= current->string[i]) && (current->string[i] <= '9');i++)
      index[i] = current->string[i];
    index[i] = 0;
    MethodIndex = (unsigned)atoi(index);
    defgenericPtr = FindDefgeneric(curr_def_name);
    SetDefmethodWatch(!GetDefmethodWatch(defgenericPtr,MethodIndex),
                       defgenericPtr,MethodIndex);

}

/*******************************************************************************
          Name:		DefmethodMngrCheckBoxCallback
          Description:
          Arguments:
          Returns:
*******************************************************************************/

static void DefmethodMngrCheckBoxCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;

{
    char index[5];
    int i;
    unsigned MethodIndex;
    VOID* defgenericPtr = NULL;
    Widget aList = (Widget)client_data;
    XawListReturnStruct *current = XawListShowCurrent(w);

    if(current->list_index == XAW_LIST_NONE)
      return;
    for (i = 0; ('0' <= current->string[i]) && (current->string[i] <= '9');i++)
      index[i] = current->string[i];
    index[i] = 0;
    MethodIndex = (unsigned)atoi(index);
    XtSetArg(args[0],XtNstate,GetDefmethodWatch(FindDefgeneric(curr_def_name),MethodIndex));
    XtSetValues((Widget)client_data,args,1);

}
/*******************************************************************************
          Name:        IntGetDefmethodList
          Description: Gets list of defmethods
          Arguments:  None
          Returns:
*******************************************************************************/

IntGetDefmethodList(Aname)
char *Aname;
  {
  VOID *genrc_ptr;
  unsigned index;
  char buf[61];
  register int itemCount = 0;
  int maxItems = 20;

  genrc_ptr = FindDefgeneric(Aname);
  if(item_list1 != NULL)
   free(item_list1);
  if((index = GetNextDefmethod(genrc_ptr,0)) == 0)
   {
     item_list1 = NULL;
     return(0);
   }
  item_list1 = (String*) calloc(maxItems,sizeof(String));
  while (index != 0)
   {
     GetDefmethodDescription(buf,60,genrc_ptr,index);
     item_list1[itemCount] = balloc(strlen(buf) + 1,char);
     strcpy(item_list1[itemCount++],buf);
     if(itemCount == (maxItems - 1))
      {
        maxItems = 2 * maxItems;
        item_list1 = (String*)realloc(item_list1,maxItems*sizeof(String));
      }
     index = GetNextDefmethod(genrc_ptr,index);
   }
  item_list1[itemCount] = NULL;
  sortList(item_list1,itemCount);
  return(itemCount);
  }

/*******************************************************************************
          Name:        IntGetDefinstancesList
          Description: Gets list of definstances
          Arguments:  None
          Returns:
*******************************************************************************/
IntGetDefinstancesList()
{
  int maxItems = 20, itemCount = 0;
  struct definstance *definstance_ptr;
  char *name;


  if((definstance_ptr = (struct definstance *)  GetNextDefinstances(NULL)) == NULL)
    {
      item_list = NULL;
      return(0);
    }
  item_list = (String*)calloc(maxItems,sizeof(String));
  while( definstance_ptr != NULL)
   {
      name = GetDefinstancesName((VOID*)definstance_ptr);
      item_list[itemCount] = balloc(strlen(name) + 1, char);
      strcpy(item_list[itemCount++],name);
      if(itemCount == (maxItems - 1))
       {
         maxItems = maxItems * 2;
         item_list = (String*)realloc(item_list,maxItems * sizeof(String));
       }
      definstance_ptr = (struct definstance *)  GetNextDefinstances((VOID*)definstance_ptr);
   }
  item_list[itemCount] = NULL;
  sortList(item_list,itemCount);
  return(itemCount);
}

/******************************************************************************
          Name:        DefinstancesRemoveCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DefinstancesRemoveCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  MoveEndOfFile(dialog_text, &event);
  SetCommandString("(undefinstances ");
  PrintCLIPS("wclips","(undefinstances ");
  AppendCommandString(current->string);
  PrintCLIPS("wclips",current->string);
  AppendCommandString(")\n");
  PrintCLIPS("wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
  quit_get_event = True;
  list_change = True;
  }


/******************************************************************************
          Name:        DefinstancesPprintCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DefinstancesPprintCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE ||
     GetDefinstancesPPForm(FindDefinstances(current->string)) == NULL)
      return;
  MoveEndOfFile(dialog_text, &event);
  SetCommandString("(ppdefinstances ");
  PrintCLIPS("wclips","(ppdefinstances ");
  AppendCommandString(current->string);
  PrintCLIPS("wclips",current->string);
  AppendCommandString(")\n");
  PrintCLIPS("wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
  quit_get_event = True;
  }

/*******************************************************************************
          Name:        IntGetDefclassList
          Description: Gets list of defclasses
          Arguments:  None
          Returns:
*******************************************************************************/
IntGetDefclassList()
{
  int maxItems = 20, itemCount = 0;
  struct cls *cls_ptr;
  char *name;

  if((cls_ptr = (struct cls *) GetNextDefclass(NULL)) == NULL)
    {
      item_list = NULL;
      return(0);
    }
  item_list = (String*)calloc(maxItems,sizeof(String));
  while( cls_ptr != NULL)
    {
      name = (char*)GetDefclassName(cls_ptr);
      item_list[itemCount] = balloc(strlen(name) + 1, char);
      strcpy(item_list[itemCount++], name);
      if(itemCount == (maxItems - 1))
       {
         maxItems = maxItems * 2;
         item_list = (String*)realloc(item_list,maxItems * sizeof(String));
       }
      cls_ptr = (struct cls *) GetNextDefclass((VOID*)cls_ptr);
    }
  item_list[itemCount] = NULL;
  sortList(item_list,itemCount);  
  return(itemCount);
}

/******************************************************************************
          Name:        DefclassRemoveCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DefclassRemoveCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  MoveEndOfFile(dialog_text, &event);
  SetCommandString("(undefclass ");
  PrintCLIPS("wclips","(undefclass ");
  AppendCommandString(current->string);
  PrintCLIPS("wclips",current->string);
  AppendCommandString(")\n");
  PrintCLIPS("wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
  quit_get_event = True;
  list_change = True;

  }

/******************************************************************************
          Name:        DefclassDescribeCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DefclassDescribeCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  MoveEndOfFile(dialog_text, &event);
  SetCommandString("(describe-class ");
  PrintCLIPS("wclips","(describe-class ");
  AppendCommandString(current->string);
  PrintCLIPS("wclips",current->string);
  AppendCommandString(")\n");
  PrintCLIPS("wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
  quit_get_event = True;
  }

/******************************************************************************
          Name:        DefclassBrowseCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DefclassBrowseCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  MoveEndOfFile(dialog_text, &event);
  SetCommandString("(browse-classes ");
  PrintCLIPS("wclips","(browse-classes ");
  AppendCommandString(current->string);
  PrintCLIPS("wclips",current->string);
  AppendCommandString(")\n");
  PrintCLIPS("wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
  quit_get_event = True;
  }


/******************************************************************************
          Name:        DefclassPprintCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DefclassPprintCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE ||
     GetDefclassPPForm(FindDefclass(current->string)) == NULL)
      return;
  MoveEndOfFile(dialog_text, &event);
  SetCommandString("(ppdefclass ");
  PrintCLIPS("wclips","(ppdefclass ");
  AppendCommandString(current->string);	
  PrintCLIPS("wclips",current->string);
  AppendCommandString(")\n");
  PrintCLIPS("wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
  quit_get_event = True;
  }

/******************************************************************************
          Name:        DefclassMessageHandlersCallback
          Description: Displays a dialog box allowing the message handlers for 
                       the currently selected class to  be browsed.
          Arguments:  w - not used
                       client_data - not used
                       call_data - the list widget containing the list of defclass
          Returns:     None
*******************************************************************************/
static void DefclassMessageHandlersCallback(w,client_data, call_data)
 Widget w;
  XtPointer client_data, call_data;
  {
  char title[MAX_CHAR_IN_BUF];
  Widget defmessHdlrManager, defmessHdlrManager_form, defmessHdlrManager_viewport,
         remove, pprint, cancel,watch,watch_label;
  int itemCount = 0;
 
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);
 

  if(current->list_index == XAW_LIST_NONE)
      return;
  itemCount = IntGetDefmessgHndlerList(current->string);
  if(item_list1 == NULL)
   {
     defmessagehandler_flag = False;
     return;
   }
  curr_def_name = balloc(strlen(current->string) + 1,char);
  strcpy(curr_def_name,current->string);
  title[0] = 0;
  sprintf(title,"%s Defmessage-Handler Manager - %d Items",curr_def_name,itemCount);
  defmessagehandler_flag = True;

  /* ========================================== */
  /*  Create Defmessage Manager window          */
  /* ========================================== */

  defmessHdlrManager =  XtCreatePopupShell(title,
                                         topLevelShellWidgetClass,
                                         toplevel,
                                         NULL, 0);
  defmessHdlrManager_form =  XtCreateManagedWidget("manager_form",
                                                 formWidgetClass,
                                                 defmessHdlrManager,
                                                 NULL, 0);
  XtSetArg(args[0],XtNallowHoriz,True);
  XtSetArg(args[1],XtNallowVert,True);
  defmessHdlrManager_viewport = XtCreateManagedWidget("manager_viewport",
                                                     viewportWidgetClass,
                                                     defmessHdlrManager_form,
                                                     args,2);
   XtSetArg(args[0], XtNlist, item_list1);
   manager_list1 = XtCreateManagedWidget("manager_list",
                                         listWidgetClass,
                                         defmessHdlrManager_viewport,
                                         args, 1);

  /* ========================================== */
  /*  Create the Remove button                  */
  /* ========================================== */

  XtSetArg(args[0], XtNfromHoriz,  defmessHdlrManager_viewport);
  XtSetArg(args[1], XtNlabel, "Remove");
  remove = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 defmessHdlrManager_form,
                                 args, 2);
  XtAddCallback(remove,XtNcallback,RemoveMessageHandlerCallback,(XtPointer)manager_list1); 

  /* ========================================== */
  /*  Create the Pprrint button                 */
  /* ========================================== */

  XtSetArg(args[1], XtNfromVert, remove);
  XtSetArg(args[2], XtNlabel, "Pprint");
  pprint = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 defmessHdlrManager_form,
                                 args, 3);
  XtAddCallback(pprint,XtNcallback,MessageHandlerPprintCallback,(XtPointer)manager_list1); 

  /* ========================================== */
  /*  Create the Watch button                   */
  /* ========================================== */

  XtSetArg(args[1],XtNfromVert,pprint);
  XtSetArg(args[2],XtNlabel," ");
  watch = XtCreateManagedWidget("managerButton",
                                 toggleWidgetClass,
                                 defmessHdlrManager_form,
                                 args, 3);
  XtAddCallback(watch,XtNcallback,DefmessHdlrMngrWatchCallback,(XtPointer)manager_list1);
  XtAddCallback(manager_list1,XtNcallback,DefmessHdlrMngrCheckBoxCallback,(XtPointer)watch);
  XtSetArg(args[0],XtNfromHoriz,watch);
  XtSetArg(args[1],XtNfromVert,pprint);
  XtSetArg(args[2],XtNlabel,"Watch");
  watch_label = XtCreateManagedWidget("checkBoxLabel",
                                 labelWidgetClass,
                                 defmessHdlrManager_form,
                                 args, 3);

  /* ========================================== */
  /*  Create the Cancel button                  */
  /* ========================================== */

  XtSetArg(args[0], XtNfromHoriz,defmessHdlrManager_viewport);
  XtSetArg(args[1], XtNfromVert, watch);
  XtSetArg(args[2], XtNlabel, "Done");
  cancel = XtCreateManagedWidget("managerCancel",
                                 commandWidgetClass,
                                 defmessHdlrManager_form,
                                 args, 3);
  XtAddCallback(cancel, XtNcallback, CancelSelectSecondary,
                (XtPointer) defmessHdlrManager);

  XtPopup(defmessHdlrManager, XtGrabNonexclusive);

  }

/*******************************************************************************
          Name:        IntGetDefmessgHndlerList
          Description: Gets the defmessage-handlers list
          Arguments:  name of the defclass
          Returns:
*******************************************************************************/
IntGetDefmessgHndlerList(name)
char *name;
{
   VOID *defclass_ptr;
   unsigned index;
   char *buf1,*buf2;
   int maxItems = 20,itemCount = 0;
 
   defclass_ptr = FindDefclass(name);
   if(item_list1 != NULL)
     free(item_list1);
   if((index = GetNextDefmessageHandler(defclass_ptr,0)) == 0)
    {
      item_list1 = NULL;
      return(0);
    }
   item_list1 = (String*)calloc(maxItems,sizeof(String));
   while(index != 0)
    {
      buf1 = GetDefmessageHandlerName(defclass_ptr,index);
      buf2 = GetDefmessageHandlerType(defclass_ptr,index);
      item_list1[itemCount] = balloc(strlen(buf1) + strlen(buf2) + 2,char);
      strcpy(item_list1[itemCount],buf1);
      strcat(item_list1[itemCount]," ");
      strcat(item_list1[itemCount++],buf2);
      if(itemCount == (maxItems - 1))
       {
          maxItems = 2 * maxItems;
          item_list1 = (String*)realloc(item_list1,maxItems * sizeof(String));
       }
      index = GetNextDefmessageHandler(defclass_ptr,index);
    }
   item_list1[itemCount] = NULL;
   sortList(item_list1,itemCount);
   return(itemCount);
}
/*******************************************************************************
          Name:        RemoveMessageHandlerCallback
          Description: Take the message-handler out of the list
          Arguments:   w - widget that initiate this call back
                       call_data - not used
                       client_data - the list widget which contains the list
                                     of the defmessage-handlers
          Returns:
*******************************************************************************/
static void RemoveMessageHandlerCallback(w,client_data,call_data)
Widget w;
XtPointer call_data,client_data;
{
    char buf[256];
    int i;
    Widget aList = (Widget)client_data;
    XawListReturnStruct *current = XawListShowCurrent(aList);

    if(current->list_index == XAW_LIST_NONE)
      return;
    for (i = 0; current->string[i] != ' '; i++)
      buf[i] = current->string[i];

    buf[i] = 0;
    MoveEndOfFile(dialog_text, &event);
    AppendCommandString("(undefmessage-handler  ");
    PrintCLIPS("wclips","(undefmessage-handler  ");
    AppendCommandString(curr_def_name);
    PrintCLIPS("wclips",curr_def_name);
    AppendCommandString(" ");
    PrintCLIPS("wclips"," ");
    AppendCommandString(buf);
    PrintCLIPS("wclips",buf);
    strcpy(buf,&(current->string[i]));
    AppendCommandString(" ");
    PrintCLIPS("wclips"," ");
    AppendCommandString(buf);
    PrintCLIPS("wclips",buf);
    AppendCommandString(")\n");
    PrintCLIPS("wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
    quit_get_event = True;
    list1_change = True;    

}
/*******************************************************************************
          Name:        MessageHandlerPprintCallback
          Description: Take the message-handler out of the list
          Arguments:   w - widget that initiate this call back
                       call_data - not used
                       client_data - the list widget which contains the list
                                     of the defmessage-handlers
          Returns:
*******************************************************************************/
static void MessageHandlerPprintCallback(w,client_data,call_data)
Widget w;
XtPointer call_data,client_data;
{
    char buf[256];
    int i;
    unsigned messageIndex;
    Widget aList = (Widget)client_data;
    XawListReturnStruct *current = XawListShowCurrent(aList);

    if(current->list_index == XAW_LIST_NONE)
      return;
    for (i = 0; current->string[i] != ' '; i++)
      buf[i] = current->string[i];
    buf[i] = 0;
    i++;
    messageIndex  = FindDefmessageHandler(FindDefclass(curr_def_name),
                                           buf,&(current->string[i]));
    if(GetDefmessageHandlerPPForm(FindDefclass(curr_def_name),messageIndex) == NULL)
      return;
    MoveEndOfFile(dialog_text, &event);
    AppendCommandString("(ppdefmessage-handler  ");
    PrintCLIPS("wclips","(ppdefmessage-handler  ");
    AppendCommandString(curr_def_name);
    PrintCLIPS("wclips",curr_def_name);
    AppendCommandString(" ");
    PrintCLIPS("wclips"," ");
    AppendCommandString(buf);
    PrintCLIPS("wclips",buf);
    strcpy(buf,&(current->string[i]));
    AppendCommandString(" ");
    PrintCLIPS("wclips"," ");
    AppendCommandString(buf);
    PrintCLIPS("wclips",buf);
    AppendCommandString(")\n");
    PrintCLIPS("wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
    quit_get_event = True;

}
/*******************************************************************************
          Name:        Initialize
          Description:
          Arguents:  list -
          Returns:
*******************************************************************************/
InitializeList(list)
  String list[1000];
  {
  register int i  = 0;

  while(list[i] != NULL)
    {
    release(list[i]);
    list[i++] = NULL;
    }
  }

/*******************************************************************************
          Name:        SetManagerList
          Description:
          Arguments:  widget
          Returns:
*******************************************************************************/
SetManagerList(widget)
Widget widget;
  {
  manager_list  = widget;
  }

/*******************************************************************************
          Name:        GetManagerList
          Description:
          Arguments:  None
          Returns:     manager_list
*******************************************************************************/
Widget GetManagerList()
  {
  return(manager_list);
  }

/*******************************************************************************
          Name:        RefreshMngrList
          Description: Update the manager lists if neccessary
          Arguments:  None
          Returns:
          Notes:       manager_list and manager_list1 are the global variables
                       which store the list widget(s) on the current manager window(s)

*******************************************************************************/
RefreshMngrList()
  {
  int itemCount = 0;
  char buffer[MAX_CHAR_IN_BUF];
  Window theWindow;
  Display *theDisplay;
  
  if(list_change)
   {
    list_change = False;
 
  /* =========================================================== *
   *  Update the rule manager list                               *
   * =========================================================== */
  
    if(defrulemanager_flag)
    {
       itemCount = IntGetDefruleLis();
       if(item_list == NULL)
         {
           XtDestroyWidget(XtParent(XtParent(XtParent(manager_list))));
           ClearParameters();
           defrulemanager_flag = False;
         }
       else
        {
           defrulemanager_flag = True;
           theWindow =  XtWindow(XtParent(XtParent(XtParent(manager_list))));
           theDisplay = XtDisplay(XtParent(XtParent(XtParent(manager_list))));
           sprintf(buffer,"Defrule Manager - %d Items",itemCount);
           XStoreName(theDisplay,theWindow,buffer);
           XawListChange(manager_list,item_list,0,0,False);
        }
    }
  /* =========================================================== *
   *  Update the deffact manager list                            *
   * =========================================================== */

    else if (deffactsmanager_flag)
    {
       itemCount = IntGetFactList();
       if(item_list == NULL)
         {
           XtDestroyWidget(XtParent(XtParent(XtParent(manager_list))));
           ClearParameters();
           deffactsmanager_flag = False;
         }
       else
        {
           deffactsmanager_flag = True;
           theWindow =  XtWindow(XtParent(XtParent(XtParent(manager_list))));
           theDisplay = XtDisplay(XtParent(XtParent(XtParent(manager_list))));
           sprintf(buffer,"Deffacts Manager - %d Items",itemCount);
           XStoreName(theDisplay,theWindow,buffer);
           XawListChange(manager_list,item_list,0,0,False);
        }

    }
  /* =========================================================== *
   *  Update the deftemplate manager list                        *
   * =========================================================== */

    else if (deftemplatemanager_flag)
    {
       itemCount = IntGetDeftemplateList();
       if(item_list == NULL)
         {
           XtDestroyWidget(XtParent(XtParent(XtParent(manager_list))));
           ClearParameters();
           deftemplatemanager_flag = False;
         }
       else
        {
           deftemplatemanager_flag = True;
           theWindow =  XtWindow(XtParent(XtParent(XtParent(manager_list))));
           theDisplay = XtDisplay(XtParent(XtParent(XtParent(manager_list))));
           sprintf(buffer,"Deftemplate Manager - %d Items",itemCount);
           XStoreName(theDisplay,theWindow,buffer);
           XawListChange(manager_list,item_list,0,0,False);
        }
    }
  /* =========================================================== *
   *  Update the deffunction manager list                        *
   * =========================================================== */

   else if (deffunctionmanager_flag)
   {
       itemCount = IntGetDeffunctionList();
       if(item_list == NULL)
         {
           XtDestroyWidget(XtParent(XtParent(XtParent(manager_list))));
           ClearParameters();
           deffunctionmanager_flag = False;
         }
       else
        {
           deffunctionmanager_flag = True;
           theWindow =  XtWindow(XtParent(XtParent(XtParent(manager_list))));
           theDisplay = XtDisplay(XtParent(XtParent(XtParent(manager_list))));
           sprintf(buffer,"Deffunction Manager - %d Items",itemCount);
           XStoreName(theDisplay,theWindow,buffer);
           XawListChange(manager_list,item_list,0,0,False);
        }
   }

  /* =========================================================== *
   *  Update the defglobal manager list                         *
   * =========================================================== */
   else if (defglobalmanager_flag)
   {
       itemCount = IntGetDefglobalList();
       if(item_list == NULL)
         {
           XtDestroyWidget(XtParent(XtParent(XtParent(manager_list))));
           ClearParameters();
           defglobalmanager_flag = False;
         }
       else
        {
           defglobalmanager_flag = True;
           theWindow =  XtWindow(XtParent(XtParent(XtParent(manager_list))));
           theDisplay = XtDisplay(XtParent(XtParent(XtParent(manager_list))));
           sprintf(buffer,"Defglobal Manager - %d Items",itemCount);
           XStoreName(theDisplay,theWindow,buffer);
           XawListChange(manager_list,item_list,0,0,False);
        }
   }

  /* =========================================================== *
   *  Update the defgeneric manager list                         *
   * =========================================================== */

   else if (defgenericmanager_flag)
   {
       itemCount = IntGetDefgenericList();
       if(item_list == NULL)
         {
           XtDestroyWidget(XtParent(XtParent(XtParent(manager_list))));
           ClearParameters();
           defgenericmanager_flag = False;
         }
       else
        {
           /*defgenericmanager_flag = True;*/
           theWindow =  XtWindow(XtParent(XtParent(XtParent(manager_list))));
           theDisplay = XtDisplay(XtParent(XtParent(XtParent(manager_list))));
           sprintf(buffer,"Defgeneric Manager - %d Items",itemCount);
           XStoreName(theDisplay,theWindow,buffer);
           XawListChange(manager_list,item_list,0,0,False);
        }
   }

  /* =========================================================== *
   *  Update the definstances manager list                       *
   * =========================================================== */
 
  else if (definstancesmanager_flag)
  {
       itemCount = IntGetDefinstancesList();
       if(item_list == NULL)
         {
           XtDestroyWidget(XtParent(XtParent(XtParent(manager_list))));
           ClearParameters();
           definstancesmanager_flag = False;
         }
       else
        {
           definstancesmanager_flag = True;
           theWindow =  XtWindow(XtParent(XtParent(XtParent(manager_list))));
           theDisplay = XtDisplay(XtParent(XtParent(XtParent(manager_list))));
           sprintf(buffer,"Definstances Manager - %d Items",itemCount);
           XStoreName(theDisplay,theWindow,buffer);
           XawListChange(manager_list,item_list,0,0,False);
        }
   }
  /* =========================================================== *
   *  Update the defclass manager list                               *
   * =========================================================== */

    else if (defclassmanager_flag)
    {
       itemCount = IntGetDefclassList();
       if(item_list == NULL)
         {
           XtDestroyWidget(XtParent(XtParent(XtParent(manager_list))));
           ClearParameters();
           defclassmanager_flag = False;
         }
       else
        {
           defclassmanager_flag = True;
           theWindow =  XtWindow(XtParent(XtParent(XtParent(manager_list))));
           theDisplay = XtDisplay(XtParent(XtParent(XtParent(manager_list))));
           sprintf(buffer,"Defclass Manager - %d Items",itemCount);
           XStoreName(theDisplay,theWindow,buffer);
           XawListChange(manager_list,item_list,0,0,False);
        }
    }
  /* =========================================================== *
   *  Update the agenda manager list                             *
   * =========================================================== */

    else if (agendamanager_flag)
    {
       itemCount = IntGetAgendaList();
       if(item_list == NULL)
         {
           XtDestroyWidget(XtParent(XtParent(XtParent(manager_list))));
           ClearParameters();
           agendamanager_flag = False;
         }
       else
        {
           agendamanager_flag = True;
           theWindow =  XtWindow(XtParent(XtParent(XtParent(manager_list))));
           theDisplay = XtDisplay(XtParent(XtParent(XtParent(manager_list))));
           sprintf(buffer,"Agenda Manager - %d Items",itemCount);
           XStoreName(theDisplay,theWindow,buffer);
           XawListChange(manager_list,item_list,0,0,False);
        }
     }
   } /* End of list change = True */
  /* =========================================================== *
   *  Update the defmethod manager list                          *
   * =========================================================== */
 
  else if (list1_change)
   {
    if ((defmethodmanager_flag)&&(list1_change))
    {
          
       list1_change = False;
       itemCount = IntGetDefmethodList(curr_def_name);
       if(item_list1 == NULL)
        {
           XtDestroyWidget(XtParent(XtParent(XtParent(manager_list1))));
           defmethodmanager_flag = False;
           release(curr_def_name);
        }
       else
        {
           theWindow =  XtWindow(XtParent(XtParent(XtParent(manager_list1))));
           theDisplay = XtDisplay(XtParent(XtParent(XtParent(manager_list1))));
           sprintf(buffer,"%s Defmethod Manager - %d Items",curr_def_name,itemCount);
           XStoreName(theDisplay,theWindow,buffer); 
           XawListChange(manager_list1,item_list1,0,0,False);
        }
    
     }
  /* =========================================================== *
   *  Update the defmessage handler list                         *
   * =========================================================== */

     else if((defmessagehandler_flag)&&(list1_change))
     {

       list1_change = False;
       itemCount = IntGetDefmessgHndlerList(curr_def_name);
       if(item_list1 == NULL)
        {
           XtDestroyWidget(XtParent(XtParent(XtParent(manager_list1))));
           defmessagehandler_flag = False;
           release(curr_def_name);
        }
       else
        {
          theWindow =  XtWindow(XtParent(XtParent(XtParent(manager_list1))));
           theDisplay = XtDisplay(XtParent(XtParent(XtParent(manager_list1))));
           sprintf(buffer,"%s Defmessage-Handler Manager - %d Items",curr_def_name,itemCount);
           XStoreName(theDisplay,theWindow,buffer);
          XawListChange(manager_list1,item_list1,0,0,False);
        }
     }
   } /* End of list1_change = True */ 
  }

/*******************************************************************************
          Name:        ClearParameters
          Description: Clear the list_change flag t False and reset the manager
		       widget to NULL
          Arguments:  None
          Returns:
*******************************************************************************/
ClearParameters()
  {
  list_change = False;
  SetManagerList((Widget)NULL);
  }

/******************************************************************************
          Name:        CancelSelectPrimary
          Description: Destroys top level popup window for managers
          Arguments:  w - not used
                       client_data - widget to destroy
                       call_data - not used
          Returns:     None
*******************************************************************************/
void CancelSelectPrimary(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget widget = (Widget) client_data;

  XtDestroyWidget(widget);
  ClearParameters();
  free(item_list);
  item_list = NULL;
  defrulemanager_flag = False;
  deffactsmanager_flag = False;
  deftemplatemanager_flag = False;
  deffunctionmanager_flag = False;
  defglobalmanager_flag = False;
  defgenericmanager_flag = False;
  definstancesmanager_flag = False;
  defclassmanager_flag = False;
  agendamanager_flag = False;
  }

/******************************************************************************
          Name:        CancelSelectSecondary
          Description: Destroys second level popup window for managers
          Arguments:  w - not used
                       client_data - widget to destroy
                       call_data - not used
          Returns:     None
*******************************************************************************/
static void CancelSelectSecondary(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget widget = (Widget) client_data;

  XtDestroyWidget(widget);
  list1_change = False;
  manager_list1 = NULL;
  release(curr_def_name);
  free(item_list1);
  item_list1 = NULL;
  defmethodmanager_flag = False;
  defmessagehandler_flag = False;
  }


/******************************************************************************
	Name:		DefruleBreakPointCallback
	Description:	
	Arguments:
	Return:
*******************************************************************************/
static void  DefruleBreakPointCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Boolean OnOff = False;
  VOID *defrulePtr;
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);

  if(current->list_index == XAW_LIST_NONE)
      return;
  defrulePtr = FindDefrule(current->string);
  XtSetArg(args[0],XtNstate,&OnOff);
  XtGetValues(w,args,1);
    if(OnOff == True)
     {
      SetBreak(defrulePtr);
     }
    else
     {
       if (RemoveBreak(defrulePtr) == CLIPS_FALSE)
        {
          PrintCLIPS("werror","Rule ");
          PrintCLIPS("werror",current->string);
          PrintCLIPS("werror"," does not have a breakpoint set.\n");
         }
     }
   }
/******************************************************************************
        Name:		DefruleActivationCallback
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void   DefruleActivationCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Boolean OnOff = False;
  VOID *defrulePtr;
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);

  if(current->list_index == XAW_LIST_NONE)
      return;
  defrulePtr = FindDefrule(current->string);
  SetDefruleWatchActivations(!GetDefruleWatchActivations(defrulePtr),defrulePtr);
  
  }

/******************************************************************************
        Name:		DefruleFiringsCallback
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void   DefruleFiringsCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  VOID *defrulePtr;
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);

  if(current->list_index == XAW_LIST_NONE)
      return;
  defrulePtr = FindDefrule(current->string);
  SetDefruleWatchFirings(!GetDefruleWatchFirings(defrulePtr),defrulePtr);
  
  }

/******************************************************************************
        Name:           DefruleMngrCheckboxesCallback
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void DefruleMngrCheckboxesCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
     VOID* defrulePtr;
     Widget *CheckBoxes = (Widget*)client_data;
     XawListReturnStruct *current = XawListShowCurrent(w);

     if(current->list_index == XAW_LIST_NONE)
            return;
     defrulePtr = FindDefrule(current->string);
     XtSetArg(args[0],XtNstate,DefruleHasBreakpoint(defrulePtr));
     XtSetValues(CheckBoxes[0],args,1);
     XtSetArg(args[0],XtNstate,GetDefruleWatchActivations(defrulePtr));
     XtSetValues(CheckBoxes[1],args,1);
     XtSetArg(args[0],XtNstate,GetDefruleWatchFirings(defrulePtr));
     XtSetValues(CheckBoxes[2],args,1);
  }

/******************************************************************************
        Name:		WatchInstancesCallback
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void   WatchInstancesCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
   VOID *defclassPtr;
   Widget list_widget = (Widget)client_data;
   XawListReturnStruct *current = XawListShowCurrent(list_widget);

   if(current->list_index == XAW_LIST_NONE)
       return;
   defclassPtr = FindDefclass(current->string);
   SetDefclassWatchInstances(!GetDefclassWatchInstances(defclassPtr),defclassPtr); 
  }
/******************************************************************************
        Name:		WatchSlotCallback
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void   WatchSlotCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
   VOID *defclassPtr;
   Widget list_widget = (Widget)client_data;
   XawListReturnStruct *current = XawListShowCurrent(list_widget);

   if(current->list_index == XAW_LIST_NONE)
       return;
   defclassPtr = FindDefclass(current->string);
   SetDefclassWatchSlots(!GetDefclassWatchSlots(defclassPtr),defclassPtr); 
  }


/******************************************************************************
        Name:		DefclssMngrChckbxCallback           
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void DefclssMngrChckbxCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
     VOID* defclassPtr;
     Widget *CheckBoxes = (Widget*)client_data;
     XawListReturnStruct *current = XawListShowCurrent(w);

     if(current->list_index == XAW_LIST_NONE)
            return;
     defclassPtr = FindDefclass(current->string);
     XtSetArg(args[0],XtNstate,GetDefclassWatchInstances(defclassPtr));
     XtSetValues(CheckBoxes[0],args,1);
     XtSetArg(args[0],XtNstate,GetDefclassWatchSlots(defclassPtr));
     XtSetValues(CheckBoxes[1],args,1);
 
  }

/******************************************************************************
        Name:		DeftemplateWatchCallback
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void DeftemplateWatchCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
{
  VOID* deftemplatePtr = NULL;
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);

  if(current->list_index == XAW_LIST_NONE)
      return;
  deftemplatePtr = FindDeftemplate(current->string);
  SetDeftemplateWatch(!GetDeftemplateWatch(deftemplatePtr),deftemplatePtr);

}

/******************************************************************************
        Name:		DeftemplateMngrCheckboxCallback
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void DeftemplateMngrCheckboxCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
{
   VOID* deftemplatePtr = NULL;
   Widget checkbox = (Widget)client_data;
   XawListReturnStruct *current = XawListShowCurrent(w);

  if(current->list_index == XAW_LIST_NONE)
      return;
   
  deftemplatePtr = FindDeftemplate(current->string);
  XtSetArg(args[0],XtNstate,GetDeftemplateWatch(deftemplatePtr));
  XtSetValues(checkbox,args,1); 
}


/******************************************************************************
        Name:		DeffunctionWatchCallback
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void DeffunctionWatchCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
{

  VOID* deffunctionPtr = NULL;
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);

  if(current->list_index == XAW_LIST_NONE)
      return;
  deffunctionPtr = FindDeffunction(current->string);
  SetDeffunctionWatch(!GetDeffunctionWatch(deffunctionPtr),deffunctionPtr);
}



/******************************************************************************
        Name:           DeffunctionMngrCheckBoxCallback
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void DeffunctionMngrCheckboxCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
{
   VOID* deffunctionPtr = NULL;
   Widget checkbox = (Widget)client_data;
   XawListReturnStruct *current = XawListShowCurrent(w);

  if(current->list_index == XAW_LIST_NONE)
      return;

  deffunctionPtr = FindDeffunction(current->string);
  XtSetArg(args[0],XtNstate,GetDeffunctionWatch(deffunctionPtr));
  XtSetValues(checkbox,args,1);

}

/******************************************************************************
        Name:           DefmessHdlrMngrWatchCallback
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void DefmessHdlrMngrWatchCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
{

  VOID* defclassPtr = NULL;
  char *buf1,buf2[20];
  unsigned i,j = 0,index;
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);

  if(current->list_index == XAW_LIST_NONE)
      return;
 /* ======================================= */
  /*  get the name of the defmessage handler */
  /* ======================================  */

  buf1 = (char*)balloc(strlen(current->string),char);
  for (i = 0; current->string[i] != ' '; i++)
    buf1[i] = current->string[i];
  buf1[i] = 0;

  while(current->string[i] == ' ')
    i++;

 /* ======================================= */
 /* Get the handler-type                    */ 
 /* ======================================= */

  while(current->string[i] != 0)
    buf2[j++] = current->string[i++];

  buf2[j] = 0;

  defclassPtr = FindDefclass(curr_def_name);
  index = FindDefmessageHandler(defclassPtr,buf1,buf2);
  SetDefmessageHandlerWatch(
           !GetDefmessageHandlerWatch(defclassPtr,index),defclassPtr,index);
  free(buf1);
}

/******************************************************************************
        Name:           DefmessHdlrMngrCheckBoxCallback
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void DefmessHdlrMngrCheckBoxCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
{
   VOID* defclassPtr = NULL;
   char *buf1;
   char buf2[20];
   unsigned i,j =  0, index;
   Widget checkbox = (Widget)client_data;
   XawListReturnStruct *current = XawListShowCurrent(w);

  if(current->list_index == XAW_LIST_NONE)
      return;

  /* ======================================= */
  /*  get the name of the defmessage handler */
  /* ======================================  */

  buf1 = (char*)balloc(strlen(current->string),char);
  for (i = 0; current->string[i] != ' '; i++)
    buf1[i] = current->string[i];
  buf1[i] = 0;

  while(current->string[i] == ' ')
     i++;

 /* ======================================= */
 /* Get the handler-type                    */
 /* ======================================= */
 
  while(current->string[i] != 0)
    buf2[j++] = current->string[i++];

  buf2[j] = 0;
 
  defclassPtr = FindDefclass(curr_def_name);
  index = FindDefmessageHandler(defclassPtr,buf1,buf2);
  XtSetArg(args[0],XtNstate,GetDefmessageHandlerWatch(defclassPtr,index));
  XtSetValues(checkbox,args,1);
  free(buf1);
}
