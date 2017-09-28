/********************************* xmenu_file.c *******************************/
/*                                                                            */
/******************************************************************************/
/*                                                                            */
/*        Author:  BeBe Ly - NASA/Johnson Space Center                        */
/*                 Daniel J. McCoy - University of Houston-Downtown           */
/*                                 & Loral Space Information Systems          */
/*                                                                            */
/*        This file contains all the callback funtions for file menu          */
/******************************************************************************/

#include <stdio.h>
#include <errno.h>
#include "xsetup.h"
#include "constant.h"
#include "xclips.h"
#include "setup.h"
#include "symbol.h"
#include "scanner.h"

#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>

#if SOLARIS 
#include <dirent.h>
#else
#include <sys/dir.h>
#endif

#ifndef MAX
#define MAX(x, y) ((x) > (y) ? (x) : (y))
#endif

/********** local functions visible outside this file **********/
void LoadBatch();
void LoadBinary();
void LoadTheFacts();
void LoadRules();
void IntDribbleOn();
void Quit();
void Restart();
void IntSave();
void EditCallback();
void LoadRulesCallback();
void LoadBatchCallback();
void LoadBinaryCallback();
void LoadFactsCallback();
void DribbleCallback();
void SaveRulesCallback();
void SaveBinaryCallback();
void SaveFactsCallback();
void QuitCallback();

/********** local functions not visible outside this file **********/
static void ClipsSave();
static void FileToDialog();
static void GetFileForCLIPS();
static char  **GetDirectory();
static void printMatch();
static char *GetBufferFromTextEdit();
static void printMatchForTextEdit();
/********** from CLIPS code **********/
extern int SetCommandString();
extern int PrintCLIPS();
extern int XclipsExit();
/********** external functions from ClipsTextAct.c **********/
extern void MoveEndOfFile();

/********** external functions from xmenu.c **********/
extern void MenuFunc();
extern void CancelPopupSelect();
extern void CancelSelectPrimary();

/********** Global variables **********/
extern Arg args[10];
extern Widget toplevel;
extern Widget dialog,dialog_text;          /* CLIPS dialog window */
extern XEvent event;                /* Current event */
extern Boolean quit_get_event;
extern Boolean Dribble_status;
extern Pixmap checker, clips_logo;
extern Widget agenda_form, agenda_text;
extern Widget facts_form, facts_text;
extern String *item_list;
extern int EvaluatingTopLevelCommand;

/********** local variables that available to the other files **********/
Widget file_dribble;
Widget file, file_list;
char path[255];
char **filenames;
char *completionString = NULL;
int number_entries;
extern int file_item;


/*******************************************************************************
          Name:        EditCallback
          Description: Called when Edit is selected form File menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void EditCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
   file_item = EDIT;
  (void)FileSelect();
  }

/**********************************************************************/
/* GetCommandCompletionString:                                        */
/* Description : Get the string to be completed from the command      */
/* string.                                                            */
/* Arguments : char theString : command string                        */
/*             int maxPosition: Length of string                      */
/* Return    : string to be completed
/**********************************************************************/
char *GetCommandCompletionString(theString,maxPosition)
  char *theString;
  int maxPosition;
  {
   struct token lastToken;
   struct token theToken;
   char lastChar;
   char *rs;
   int length;

   /*=========================*/
   /* Get the command string. */
   /*=========================*/

   if (theString == NULL) return("");

   /*=========================================================================*/
   /* If the last character in the command string is a space, character       */
   /* return, or quotation mark, then the command completion can be anything. */
   /*=========================================================================*/

   lastChar = theString[maxPosition - 1];
   if ((lastChar == ' ') || (lastChar == '"') ||
       (lastChar == '\t') || (lastChar == '\f') ||
       (lastChar == '\n') || (lastChar == '\r'))
     { return(""); }

   /*============================================*/
   /* Find the last token in the command string. */
   /*============================================*/

   OpenTextSource("CommandCompletion",theString,0,maxPosition);
   IgnoreCompletionErrors = CLIPS_TRUE;
   GetToken("CommandCompletion",&theToken);
   CopyToken(&lastToken,&theToken);
   while (theToken.type != STOP)
    {
      CopyToken(&lastToken,&theToken);
      GetToken("CommandCompletion",&theToken);
     }
   CloseStringSource("CommandCompletion");
   IgnoreCompletionErrors = CLIPS_FALSE;

   /*===============================================*/
   /* Determine if the last token can be completed. */
   /*===============================================*/

   if (lastToken.type == SYMBOL)
     {
      rs = ValueToString(lastToken.value);
      if (rs[0] == '[') return (&rs[1]);
      return(ValueToString(lastToken.value));
     }
   else if (lastToken.type == SF_VARIABLE)
     { return(ValueToString(lastToken.value)); }
   else if (lastToken.type == MF_VARIABLE)
     { return(ValueToString(lastToken.value)); }
   else if ((lastToken.type == GBL_VARIABLE) || (lastToken.type == INSTANCE_NAME))
     { return(NULL); }
   else if (lastToken.type == STRING)
     {
      length = strlen(ValueToString(lastToken.value));
      return(GetCommandCompletionString(ValueToString(lastToken.value),length));
     }
   else if ((lastToken.type == FLOAT) || (lastToken.type == INTEGER))
     { return(NULL); }

   return("");
  }
/*******************************************************************************
          Name:        CompletionDialogCallback
          Description: Called when Completion is selected form File menu
          Arguments:  w - menu item that was selected
                       client_data - dialog window or edit window
                       call_data - not used
          Returns:     None
*******************************************************************************/
void CompletionDialogCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
{
  int NumberOfMatches,i,length;
  Boolean tempFlag;
  struct symbolMatch *matches;
  XKeyboardControl value;
  char *commandString;
  extern char* GetCommandString(); 
  extern struct symbolMatch *FindSymbolMatches();
 
  /* ================================================== */
  /* Free the memory of completionString before assign  */
  /* it to the new string.                              */
  /* ================================================== */

  if(completionString != NULL)
   {
     free(completionString);
     completionString = NULL;
   }
  /* =========================================================== */
  /* Get the the uncompleted command string; if there is none    */
  /* sound the bell and exit, else determine if the last token   */
  /* of the string can be complete                               */
  /* =========================================================== */

  commandString = GetCommandString();
  if(commandString != NULL)
   {
    length = strlen(commandString);
    commandString = GetCommandCompletionString(commandString,length);
   }
  if(commandString == NULL)
   {
     XBell(XtDisplay(toplevel),100);
     return;
   }

  /* ============================================================ */
  /* Copy the command string to a global variable for later use.  */
  /* Global completionString has to be used here due to the       */
  /* limitation of the number of arguments could be passed in the */
  /* call back function of in X window  system.                   */
  /* ============================================================ */

  completionString = (char*)malloc(strlen(commandString) + 1);
  strcpy(completionString,commandString);

  /* ============================================================ */
  /* Find the match(es). If there is none, sound the bell and     */
  /* exit; else if there is one match complete the command; else  */
  /* if there are more than one display them                      */
  /* ============================================================ */

  matches = FindSymbolMatches(completionString,&NumberOfMatches,NULL);
  if(NumberOfMatches == 0)
   {
     XBell(XtDisplay(toplevel),100);
     return;
   }
  else if (NumberOfMatches == 1)
   {
      length = strlen(completionString);
      AppendCommandString(&(matches->match->contents[length]));
      PrintCLIPS("stdin",&(matches->match->contents[length]));
   }
  else
   {
      DisplayMatchedList(dialog_text,matches);
   }
}

/*******************************************************************************
          Name:        CompletionEditCallback
          Description: Called when Completion is selected form File menu
                       in the editor.
          Arguments:  w - menu item that was selected
                       client_data - dialog window or edit window
                       call_data - not used
          Returns:     None
*******************************************************************************/
void CompletionEditCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
{
  int NumberOfMatches,i,length;
  Boolean tempFlag;
  struct symbolMatch *matches;
  extern char* GetCommandString(); 
  extern struct symbolMatch *FindSymbolMatches();
  XawTextBlock text;
  char *matchString = NULL;
  Widget source = XawTextGetSource((Widget)client_data);
  XawTextPosition CurrentPosition,EndPosition;

  /* ================================================== */
  /* Free the memory of completionString before assign  */
  /* it to the new string.                              */
  /* ================================================== */

  if(completionString != NULL)
   {
     free(completionString);
     completionString = NULL;
   }
  
  /* =================================================== */
  /* Get the beginning and ending positions of the       */
  /* selection. If there is no selection get the last    */
  /* word from the cursor.                               */
  /* ====================================================*/
  
  XawTextGetSelectionPos((Widget)client_data,&CurrentPosition,&EndPosition);
  if(CurrentPosition == EndPosition)  /* No selection was made */
   {
     matchString = GetBufferFromTextEdit((Widget)client_data);
     length = strlen(matchString);
   }
  else
   {
      XawTextSourceRead(source,CurrentPosition,&text,EndPosition - CurrentPosition);
      XawTextUnsetSelection((Widget)client_data);
      XawTextSetInsertionPoint((Widget)client_data,EndPosition);
      matchString = text.ptr;
      length = text.length;
   }

  /* ======================================= */
  /* Determine if the word can be matched.   */
  /* ======================================= */

  matchString = GetCommandCompletionString(matchString,length);
  if(matchString == NULL)
   {
     XBell(XtDisplay(toplevel),100);
     return;
   }
  completionString = (char*)malloc(strlen(matchString) + 1);
  strcpy(completionString,matchString);
  matches = FindSymbolMatches(completionString,&NumberOfMatches,NULL);
  if(NumberOfMatches == 0)
   {
     XBell(XtDisplay(toplevel),100);
     return;
   }
  else if (NumberOfMatches == 1)
   {
      length = strlen(completionString);
      text.firstPos = 0;
      text.length  = strlen(&(matches->match->contents[length]));   
      text.ptr  = &(matches->match->contents[length]);
      XawTextReplace((Widget)client_data,
                        XawTextGetInsertionPoint((Widget)client_data),
                        XawTextGetInsertionPoint((Widget)client_data),&text);
      XawTextSetInsertionPoint((Widget)client_data,
                     XawTextGetInsertionPoint((Widget)client_data) + text.length);

   }
  else
   {
      DisplayMatchedList((Widget)client_data,matches);
   }
}
/*******************************************************************************
 GetBufferFromTextEdit
 Description : This function will return the last word in the editor
               from the cursor
 *******************************************************************************/
static char * GetBufferFromTextEdit(w)
 Widget w;
{
   XawTextBlock text_return;
   XawTextPosition length;
   char *buffer;

   Widget source = XawTextGetSource(w);
   XawTextPosition NewPos,EndPos = XawTextGetInsertionPoint(w);

   /* ================================================ */
   /*  If Cursor is at the begining return empty       */
   /*  string,orther while move the cursor backward    */
   /*  until it hits the space then read and return    */
   /*  the last word.                                  */
   /* ================================================ */

   if(EndPos == 0)
    return("");
   NewPos = EndPos - 1;
   XawTextSourceRead(source,NewPos,&text_return,1);
   while((text_return.ptr[0] != ' ') && (NewPos != 0))
    {
      NewPos--;      
      XawTextSourceRead(source,NewPos,&text_return,1);
    }
   if(NewPos != 0)
    NewPos++;
   XawTextSourceRead(source,NewPos,&text_return,EndPos - NewPos);
   buffer = (char *)malloc(text_return.length + 1);
   strncpy(buffer,text_return.ptr,text_return.length);
   buffer[text_return.length] = 0;
   return(buffer);
}
/*******************************************************************************
     Name DisplayMatchedList
     Description : Called when there are more than one matches for completion
                   command
     
*******************************************************************************/
DisplayMatchedList(w,matches)
Widget w;
struct symbolMatch *matches;
{
   Widget matchShell,matchForm,matchViewport,
          matchDialog,matchList,cancel;
   int n;

   if(GetMatchList(matches) == 0)
     return(0);
   matchShell = XtCreatePopupShell("Matches",
                                    topLevelShellWidgetClass,
                                    toplevel,
                                    NULL, 0);
   matchForm = XtCreateManagedWidget( "manager_form", formWidgetClass,
                                        matchShell, NULL,0);
  
  XtSetArg(args[0],XtNallowHoriz, True);
  XtSetArg(args[1],XtNallowVert, True); 
  matchViewport = XtCreateManagedWidget("manager_viewport",viewportWidgetClass,
                                        matchForm,NULL, 2);
  n = 0;
  XtSetArg(args[n],XtNlist,item_list);n++;
  matchList = XtCreateManagedWidget("manager_list",
                                       listWidgetClass,
                                       matchViewport,
                                       args,n);
  n = 0;
  XtSetArg(args[n], XtNresizable, True);n++;
  XtSetArg(args[n],XtNlabel,"");n++;
  XtSetArg(args[n], XtNvalue, "");n++;
  XtSetArg(args[n], XtNfromVert, matchViewport);n++;
  XtSetArg(args[n], XtNicon, clips_logo);n++;
  XtSetArg(args[n], XtNleft, XtChainLeft);n++;
  XtSetArg(args[n], XtNright, XtChainRight);n++;
  XtSetArg(args[n], XtNtop, XtChainBottom);n++;
  XtSetArg(args[n], XtNbottom, XtChainBottom);n++;

/* ============================================================= */
/*  If the current active window is clips dialog box then pass   */
/*  the appropriate function to handle the match for the clips   */
/*  dialog; else the funcTion handling the match for the text    */
/*  editor is passed as the callback function.                   */
/* ============================================================= */ 

  if(w == dialog_text)
   {
    matchDialog = XtCreateManagedWidget("match_dialog",
                                      dialogWidgetClass,
                                      matchForm,
                                      args, n);
    XawDialogAddButton(matchDialog, "SELECT",printMatch, (XtPointer)completionString);
   }
  else
   {
     matchDialog = XtCreateManagedWidget("match_editor",
                                      dialogWidgetClass,
                                      matchForm,
                                      args, n);
     XawDialogAddButton(matchDialog, "SELECT",printMatchForTextEdit,(XtPointer)w);
   }
  XawDialogAddButton(matchDialog, "CANCEL", CancelPopupSelect,
                     (XtPointer) matchForm);
  XtAddCallback(matchList, XtNcallback, FileToDialog, (XtPointer) matchDialog);
  XtPopup(matchShell,XtGrabNonexclusive);
}

/*******************************************************************************
          Name:        MatchDialogReturnD
          Description: Called when pressing return key in match dialog widget
          Arguments:  w - Widget that caused action to be called
                       event - Not used
                       params - Dialog widget
                       num_params - Not used
          Returns:     None
*******************************************************************************/
void MatchDialogReturnD(w, event, params, num_params)
  Widget w;
  XEvent *event;
  String *params;
  Cardinal *num_params;
  {
    printMatch(w,(XtPointer)completionString,NULL);
  }

/*******************************************************************************
          Name:        MatchDialogReturnE
          Description: Called when pressing return key in match dialog widget
          Arguments:  w - Widget that caused action to be called
                       event - Not used
                       params - editor widget
                       num_params - Not used
          Returns:     None
          Notes :      Currently, this does not work yet since I
                       do not know how to pass the text editor without
                       make it a global variable.
*******************************************************************************/
void MatchDialogReturnE(w, event, params, num_params)
  Widget w;
  XEvent *event;
  String *params;
  Cardinal *num_params;
  {
    /*printMatchForTextEdit(w,params,NULL);*/
  }

/*******************************************************************************
          Name:        printMatch
          Description: Simulates callbacks for dialog widget
          Arguments:  w - Dialog widget
                       client_data - Dialog widget
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
static void printMatch(w,client_data,call_data)
  Widget w;
  XtPointer client_data, call_data;
{
  Boolean tempFlag;
  String aString = XawDialogGetValueString(XtParent(w));
  int length = strlen((char*)client_data);

  AppendCommandString(&(aString[length]));
  PrintCLIPS("stdin",&(aString[length]));
  XtDestroyWidget(XtParent(XtParent(XtParent(w))));
}

/*******************************************************************************
          Name:        printMatchForTextEdit
          Description: Simulates callbacks for dialog widget
          Arguments:  w - Dialog widget
                       client_data - Dialog widget
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
static void printMatchForTextEdit(w,client_data,call_data)
  Widget w;
  XtPointer client_data, call_data;
{
  XawTextBlock text;
  String aString = XawDialogGetValueString(XtParent(w));
  Widget text_widget = (Widget)client_data;
  int length;

  length = strlen(completionString);
  text.firstPos = 0;
  text.length  = strlen(&(aString[length]));
  text.ptr  = &(aString[length]);
  XawTextReplace(text_widget,
                 XawTextGetInsertionPoint(text_widget),
                 XawTextGetInsertionPoint(text_widget),&text);
  XawTextSetInsertionPoint(text_widget,
                     XawTextGetInsertionPoint(text_widget) + text.length);
  XtDestroyWidget(XtParent(XtParent(XtParent(w))));
}


/*******************************************************************************
          Name:        GetDefruleList
          Description: Gets the list of rules
          Arguments:  None
          Returns:
*******************************************************************************/
GetMatchList(matches)
struct symbolMatch *matches;
{
  int maxItems = 20,itemCount;
  if(matches == NULL)
    return(0);
  if(item_list != NULL)
    {
      free(item_list);
      item_list = NULL;
    }
  item_list = (String *)calloc(maxItems,sizeof(String));
  for(itemCount = 0;matches != NULL;matches = matches->next)
      {
         item_list[itemCount] = balloc(strlen(matches->match->contents) + 1,char);
         strcpy(item_list[itemCount], matches->match->contents);
         itemCount++;
         if(itemCount == (maxItems -1))
          {
            maxItems = 2*maxItems;
            item_list = (String *)realloc(item_list,maxItems * sizeof(String));
          }
      }
  item_list[itemCount] = NULL;
  sortList(item_list,itemCount);
  return(itemCount);
}

/**********************************************************************
 *  sortList
 **********************************************************************/

sortList(list,num)
String *list;
int num;
{
   int i,j,index;
   char *tempString;   

   if(num == 1)
     return(num);
   for(i = 0;i < num;i++)
    {
      tempString = list[i];
      index = i;
      for(j = i + 1;j < num; j++)
       {
         if(strcmp(tempString,list[j]) > 0)
          {
            tempString = list[j];
            index = j;
          }
       }
      if(i != index)
       {
         list[index] = list[i];
         list[i] = tempString;
       }
    }
}

/*******************************************************************************
          Name:        LoadBatchCallback
          Description: Called when Load Batch is selected form File menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void LoadBatchCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
   file_item  = LOADBATCH;
  (void)FileSelect();
  }

/*******************************************************************************
          Name:        LoadBinaryCallback
          Description: Called when Load Binary is selected form File menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void LoadBinaryCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
   file_item  = LOADBINARY;
  (void)FileSelect();
  }

/*******************************************************************************
          Name:        LoadFactsCallback
          Description: Called when Load Facts is selected form File menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void LoadFactsCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
   file_item  = LOADFACTS;
  (void)FileSelect();
  }

/*******************************************************************************
          Name:        LoadRulesCallback
          Description: Called when Load Rules is selected form File menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void LoadRulesCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
   file_item = LOADRULES;
  (void)FileSelect();

  }

/*******************************************************************************
          Name:        DribbleCallback
          Description: Called when Dribble is selected form File menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void DribbleCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  MoveEndOfFile(dialog_text, &event);
  file_item = DRIBBLEON;
  if (Dribble_status)
    {
    XtSetArg(args[0], XtNleftBitmap, None);
    XtSetValues(file_dribble, args, 1);
    SetCommandString("(dribble-off)\n");
    if(!EvaluatingTopLevelCommand)
      PrintCLIPS("wclips","(dribble-off)\n");
    quit_get_event = True;
    Dribble_status = !Dribble_status;
    }
  else
    FileSelect();
  }

/*******************************************************************************
          Name:        SaveBinaryCallback
          Description: Called when Save Binary is selected form File menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void SaveBinaryCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  file_item = SAVEBINARY;
  ClipsSave();
  }

/*******************************************************************************
          Name:        SaveFactsCallback
          Description: Called when Save Facts is selected form File menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void SaveFactsCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {

  file_item = SAVEFACTS;
  ClipsSave();
  }

/*******************************************************************************
          Name:        SaveRulesCallback
          Description: Called when Save Rules is selected form File menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void SaveRulesCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {

  file_item = SAVERULES;
  ClipsSave();
  }

/*******************************************************************************
          Name:        QuitCallback
          Description: Called when Quit is selected form File menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void QuitCallback(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  Widget confirmshell, confirm;

  confirmshell = XtCreatePopupShell("Confirmation",
                                    topLevelShellWidgetClass,
                                    toplevel,
                                    NULL, 0);

  XtSetArg(args[0], XtNlabel, "Quit XCLIPS.\nAre you sure?");
  XtSetArg(args[1], XtNicon, clips_logo);
  confirm = XtCreateManagedWidget("confirm",
                                  dialogWidgetClass,
                                  confirmshell,
                                  args, 2);

  XawDialogAddButton(confirm, "Quit", Quit, (XtPointer) confirm);
  XawDialogAddButton(confirm, "Restart", Restart, (XtPointer) confirm);
  XawDialogAddButton(confirm, "Cancel", CancelPopupSelect, (XtPointer) confirm);

  XtPopup(confirmshell, XtGrabNonexclusive);
  }

/*******************************************************************************
          Name:        ClipsSave
          Description: Prompts for file name to execute CLIPS' bsave,
                       save-facts, or save functions
          Arguments:  None
          Returns:     None
*******************************************************************************/
static void ClipsSave()
  {
  Widget popup, file_dialog;

  popup = XtCreatePopupShell("File",
                             topLevelShellWidgetClass,
                             toplevel,
                             NULL, 0);

  XtSetArg(args[0], XtNlabel, "Enter file name:");
  XtSetArg(args[1], XtNvalue,  "");
  XtSetArg(args[2], XtNicon, clips_logo);
  file_dialog = XtCreateManagedWidget("file_dialog",
                                      dialogWidgetClass,
                                      popup,
                                      args, 3);
  XawDialogAddButton(file_dialog, "Save", IntSave, (XtPointer)NULL);
  XawDialogAddButton(file_dialog, "Cancel", CancelPopupSelect,
                     (XtPointer)file_dialog);

  XtPopup(popup, XtGrabNonexclusive);
  }

/*******************************************************************************
          Name:        IntSave
          Description: Eexecutes CLIPS' bsave, save-facts, or save functions
          Arguments:  w - Dialog Widget
                       client_data - Not Used
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
void IntSave(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  char *filename = XawDialogGetValueString(XtParent(w));

  switch(file_item)
    {
    case SAVEBINARY:
      PrintCLIPS("wclips", "(bsave ");
      SetCommandString("(bsave");
      AppendCommandString("\"");
      PrintCLIPS("wclips", "\"");
      AppendCommandString(filename);
      PrintCLIPS("wclips", filename);
      AppendCommandString("\"");
      PrintCLIPS("wclips", "\"");
      AppendCommandString(")\n");
      PrintCLIPS("wclips", ")\n");
      quit_get_event = True;
    break;

    case SAVEFACTS:
      PrintCLIPS("wclips", "(save-facts ");
      SetCommandString("(save-facts");
      AppendCommandString("\"");
      PrintCLIPS("wclips", "\"");
      AppendCommandString(filename);
      PrintCLIPS("wclips", filename);
      AppendCommandString("\"");
      PrintCLIPS("wclips", "\"");
      AppendCommandString(")\n");
      PrintCLIPS("wclips", ")\n");
      quit_get_event = True;
    break;

    case SAVERULES:
      PrintCLIPS("wclips", "(save ");
      SetCommandString("(save");
      AppendCommandString("\"");
      PrintCLIPS("wclips", "\"");
      AppendCommandString(filename);
      PrintCLIPS("wclips", filename);
      AppendCommandString("\"");
      PrintCLIPS("wclips", "\"");
      AppendCommandString(")\n");
      PrintCLIPS("wclips", ")\n");
      quit_get_event = True;
    break;
    }

  XtDestroyWidget(XtParent(XtParent(w)));
  }

/*******************************************************************************
          Name:        FileSelect
          Description: Pops up window in center of the Dialog Window for file
                       selection by user
          Arguments:  None
          Returns:     None
          Contrubuting Programmers:  Albert Leigh - MacDonnell Douglas
                                     Stan Smith - Barrios
                                     some guy upstairs
******************************************************************************/
FileSelect()
  {
  Widget file_form, file_dialog, view;
  char *getwd();
    

  /*XDefineCursor(XtDisplay(toplevel),toplevel,XC_watch);*/
  file = XtCreatePopupShell("File",
                            topLevelShellWidgetClass,
                            toplevel,
                            NULL, 0);

  file_form = XtCreateManagedWidget("file_form",
                                    formWidgetClass,
                                    file,
                                    NULL, 0);

  XtSetArg(args[0], XtNforceBars, True);
  XtSetArg(args[1], XtNbottom, XtChainBottom);
  XtSetArg(args[2], XtNheight,150);
  XtSetArg(args[3], XtNallowHoriz,True);
  XtSetArg(args[4],XtNallowVert,True);
  view = XtCreateManagedWidget("view",
                               viewportWidgetClass,
                               file_form,
                               args, 5);

  /* =============================================================== *
   *  Create the Select/Cancel dialog box in the file selection      *
   *  dialog box.                                                    *
   * =============================================================== */

  XtSetArg(args[0], XtNresizable, True);
  XtSetArg(args[1], XtNlabel, "Enter File Name");
  XtSetArg(args[2], XtNvalue, "");
  XtSetArg(args[3], XtNfromVert, view);
  XtSetArg(args[4], XtNicon, clips_logo);
  XtSetArg(args[5], XtNleft, XtChainLeft);
  XtSetArg(args[6], XtNright, XtChainRight);
  XtSetArg(args[7], XtNtop, XtChainBottom);
  XtSetArg(args[8], XtNbottom, XtChainBottom);
  file_dialog = XtCreateManagedWidget("file_dialog",
                                      dialogWidgetClass,
                                      file_form,
                                      args, 9);
  XawDialogAddButton(file_dialog, "SELECT", MenuFunc, (XtPointer) file_dialog);
  XawDialogAddButton(file_dialog, "CANCEL", CancelPopupSelect,
                     (XtPointer) file_form);

  XtSetArg(args[0], XtNfromHoriz, file_dialog);
  XtSetArg(args[1], XtNfromVert, view);

  /* =============================================================== *
   *  Get the path of the current dirrectory                         *
   * =============================================================== */

  if(getwd(path) == NULL)
    printf("Error getting current working directory '%s'\n", path);

  if(path[strlen(path) - 1] != '/')
    strcat(path, "/");

  /* =============================================================== *
   *  Create the file dialog list box                                *
   * =============================================================== */

  XtSetArg(args[0], XtNdefaultColumns, 4);
  XtSetArg(args[1], XtNlist, GetDirectory(path));
  XtSetArg(args[2], XtNforceColumns, False);
  XtSetArg(args[3], XtNverticalList, True);
  XtSetArg(args[4], XtNinternalWidth, 10);
  file_list = XtCreateManagedWidget("file_dialog",
                                    listWidgetClass,
                                    view,
                                    args, 5);
  XtAddCallback(file_list, XtNcallback, FileToDialog, (XtPointer) file_dialog);

  XtPopup(file, XtGrabNonexclusive);
  /*XDefineCursor(XtDisplay(toplevel),toplevel,None);*/
  }

/*******************************************************************************
          Name:        GetDirectory
          Description: used with FileSelect to create list of filenames in a
                       specific directory
          Arguments:  None
          Returns:     None
          Contrubuting Programmers:  Albert Leigh - MacDonnell Douglas
                                     Lac Nguyen - Computer Science Corp.
                                     Stan Smith - Barrios
                                     some guy upstairs
*******************************************************************************/
static char **GetDirectory()
  {
  int fcount;
  char *fullpath;
  DIR *dirp;

#if SOLARIS
   struct dirent *entry;
#else
   struct direct *entry;
#endif

  int namelength = 0;

  if ((dirp = opendir(path)) == NULL)
    {
    number_entries = 1;
    filenames = (char **)calloc(1, sizeof(char **));
    filenames[0] = (char *)malloc(sizeof(char) * 14);
    strcpy(filenames[0], ".."); 
    return(filenames);
    }
  /* =============================================================== *
   * Determine the number of filenames in the directory              *
   * =============================================================== */

  fcount = 0;

  while ((entry = readdir(dirp)) != NULL)
    {
    namelength = MAX(namelength, (unsigned int)strlen(entry->d_name));
    fcount++;
    }
  /* =============================================================== *
   * Make sure the memory allocated for the filename will contain it *
   * =============================================================== */

  namelength = MAX((namelength + 2), 14);
  if(strcmp(path,"/") == 0)
   fcount--;
  number_entries = fcount - 1;

  rewinddir(dirp);  

  filenames = (char **)calloc(fcount, sizeof(char *));
  filenames[0] = (char *)malloc(sizeof(char *) * fcount * namelength);
  fullpath  = (char *)malloc(sizeof(char)*(strlen(path)+namelength));
  fcount = 0;

  /* ============================================================== *
   *  Get the list of file or directory names in a directory        *
   * ============================================================== */

  while ((entry = readdir(dirp)) != NULL)
    {
    if (strcmp(entry->d_name, "."))
      {
       if((strcmp(path,"/") != 0)|| (strcmp (entry->d_name,"..") != 0))
        {
          filenames[fcount] = *filenames + (fcount * namelength);
          strcpy(filenames[fcount], entry->d_name);

          sprintf(fullpath, "%s%s", path, entry->d_name);

          if (IsDirectory(fullpath))
          strcat(filenames[fcount], "/");
          fcount++;
        }	
      }
    }

  close(dirp);

  qsort(*filenames, fcount, (sizeof(char) * namelength), strcmp);
 
  return(filenames);
  }

/*******************************************************************************
          Name:        IsDirectory
          Description: used with FileSelect to test for directory
          Arguments:  temppath - directory temppath to check for
          Returns:     0 - for directory
                       1 - for not directory
          Contrubuting Programmers:  Albert Leigh - MacDonnell Douglas
                                     Stan Smith - Barrios
                                     some guy upstairs
*******************************************************************************/
int IsDirectory(temppath)
  char *temppath;
  {
  struct stat sbuf;

  if(!stat(temppath, &sbuf) && ((sbuf.st_mode & S_IFMT) == S_IFDIR))
    return(1);

  else
    return(0);
  }

/*******************************************************************************
          Name:        FileToDialog
          Description: copies selected list string to dialog's asciitext window
          Arguments:  w - list widget
                       client_data - dialog widget
                       call_data - list string selected
          Returns:     None
          Contrubuting Programmers:  Albert Leigh - MacDonnell Douglas
                                     Stan Smith - Barrios
                                     some guy upstairs
*******************************************************************************/
static void FileToDialog(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  XawListReturnStruct *item = (XawListReturnStruct *)call_data;
  char *ptr;
  int i,rtn;

  if (!strcmp(item->string, "../"))
    {
    path[strlen(path) - 1] = '\0';
    ptr = strrchr(path, '/');
    ptr++;
    *(ptr) = '\0';

/* bug reported by Victor Sanchez */
/*                                */
/* remove next 2 lines and replace*/
/* by following 1 line            */
/*    for(i = 0; i < number_entries; i++)
      free(filenames[i]);
*/
    free(filenames[0]);
    free(filenames);

    XawListChange(w, GetDirectory(), 0, 0, True);

    }

  else if (strrchr(item->string, '/'))
    {
    strcat(path, item->string);
    XawListChange(w, GetDirectory(), 0, 0, True);
    }

  else
    {
    XtSetArg(args[0], XtNvalue, item->string);
    XtSetValues((Widget)client_data, args, 1);
    }  
  }

/*******************************************************************************
          Name:        LoadBatch
          Description: Loads batch file into CLIPS
          Arguments:  str - file to load
          Returns:     None
*******************************************************************************/
void LoadBatch(str)
  char *str;
  {
  PrintCLIPS("wclips", "(batch ");
  SetCommandString("(batch");
  GetFileForCLIPS(str);
  PrintCLIPS("wclips", ")\n");
  AppendCommandString(")\n");
  quit_get_event = True;
  }

/*******************************************************************************
          Name:        LoadBinary
          Description: Loads binary file into CLIPS
          Arguments:  str - file to load
          Returns:     None
*******************************************************************************/
void LoadBinary(str)
  char *str;
  {
  PrintCLIPS("wclips", "(bload ");
  SetCommandString("(bload");
  GetFileForCLIPS(str);
  PrintCLIPS("wclips", ")\n");
  AppendCommandString(")\n");
  quit_get_event = True;
  }

/*******************************************************************************
          Name:        LoadTheFacts
          Description: Loads facts file into CLIPS
          Arguments:  str - file to load
          Returns:     None
*******************************************************************************/
void LoadTheFacts(str)
  char *str;
  {
  PrintCLIPS("wclips", "(load-facts ");
  SetCommandString("(load-facts");
  GetFileForCLIPS(str);
  PrintCLIPS("wclips", ")\n");
  AppendCommandString(")\n");
  quit_get_event = True;
  }

/*******************************************************************************
          Name:        LoadRules
          Description: Loads rules into CLIPS
          Arguments:  str - file to load
          Returns:     None
*******************************************************************************/
void LoadRules(str)
  char *str;
  {
  PrintCLIPS("wclips", "(load ");
  SetCommandString("(load");
  GetFileForCLIPS(str);
  PrintCLIPS("wclips", ")\n");
  AppendCommandString(")\n");
  quit_get_event = True;
  }

/*******************************************************************************
          Name:        IntDribbleOn
          Description: Turns on dribble
          Arguments:  str - file to dribble to
          Returns:     None
*******************************************************************************/
void IntDribbleOn(str)
  String str;
  {
  if(!EvaluatingTopLevelCommand)
    PrintCLIPS("wclips", "(dribble-on ");
  SetCommandString("(dribble-on");
  GetFileForCLIPS(str);
  if(!EvaluatingTopLevelCommand)
    PrintCLIPS("wclips", ")\n");
  AppendCommandString(")\n");
  quit_get_event = True;
  if (((access(str, 02) == 0) || (access(str, 00))) && (strcmp(str, "\0") != 0))
    {
    Dribble_status = True;
    XtSetArg(args[0], XtNleftBitmap, checker);
    XtSetValues(file_dribble, args, 1);
    }
  }

/*******************************************************************************
          Name:        GetFileForCLIPS
          Description: Gets file for CLIPS to load
          Arguments:  file - File to get
          Returns:     None
*******************************************************************************/
static void GetFileForCLIPS(file)
  char *file;
  {
  AppendCommandString("\"");
  AppendCommandString(file);
  AppendCommandString("\"");
  if(!EvaluatingTopLevelCommand)
   {
    PrintCLIPS("wclips", "\"");
    PrintCLIPS("wclips", file);
    PrintCLIPS("wclips", "\"");
   }
  }

/******************************************************************************
          Name:        Restart
          Description: Restarts CLIPS
          Arguments:  w - not used
                       client_data - popup widget
                       call_data - not used
          Returns:     None
*******************************************************************************/
void Restart(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
  {
  /* following added at NRC to use the actual program name of this system */
  extern char *ThisProgramName;
  char PGMname[256];
  
  strcpy(PGMname, ThisProgramName);
  strcat(PGMname, "&");
  system(PGMname);
/*   system("xclips&"); */
  XclipsExit(0);
  }

/******************************************************************************
          Name:        Quit
          Description: Quits CLIPS
          Arguments:  None
          Returns:     None
******************************************************************************/
void Quit()
  {
  XclipsExit(0);
  }

