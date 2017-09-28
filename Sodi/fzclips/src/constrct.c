   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00 08/11/94             */
   /*                                                     */
   /*                  CONSTRUCT MODULE                   */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides basic functionality for creating new    */
/*   types of constructs, saving constructs to a file, and   */
/*   adding new functionality to the clear and reset         */
/*   commands.                                               */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#define _CONSTRCT_SOURCE_

#include <stdio.h>
#define _CLIPS_STDIO_
#include <string.h>

#include "setup.h"

#include "constant.h"
#include "clipsmem.h"
#include "router.h"
#include "scanner.h"
#include "watch.h"
#include "prcdrfun.h"
#include "prcdrpsr.h"
#include "argacces.h"
#include "exprnpsr.h"
#include "multifld.h"
#include "moduldef.h"
#include "utility.h"
#include "commline.h"

#include "constrct.h"

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   globle int                   ClearInProgress = CLIPS_FALSE;
   globle int                   ResetInProgress = CLIPS_FALSE;

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

#if (! RUN_TIME) && (! BLOAD_ONLY)
   static struct callFunctionItem   *ListOfSaveFunctions = NULL;
   static BOOLEAN                    PrintWhileLoading = CLIPS_FALSE;
   static BOOLEAN                    WatchCompilations = ON;
#endif

   static struct construct          *ListOfConstructs = NULL;
   static struct callFunctionItem   *ListOfResetFunctions = NULL;
   static struct callFunctionItem   *ListOfClearFunctions = NULL;
   static struct callFunctionItem   *ListOfClearReadyFunctions = NULL;
   static int                        Executing = CLIPS_FALSE;
#if ANSI_COMPILER
   static int                      (*BeforeResetFunction)(void) = NULL;
#else
   static int                      (*BeforeResetFunction)() = NULL;
#endif

#if (! RUN_TIME) && (! BLOAD_ONLY)

/*************************************************/
/* FindConstruct: Determines whether a construct */
/*   type is in the ListOfConstructs.            */
/*************************************************/
globle struct construct *FindConstruct(name)
  char *name;
  {
   struct construct *currentPtr;

   for (currentPtr = ListOfConstructs;
        currentPtr != NULL;
        currentPtr = currentPtr->next)
     {
      if (strcmp(name,currentPtr->constructName) == 0)
        { return(currentPtr); }
     }

   return(NULL);
  }

/***********************************************************/
/* RemoveConstruct: Removes a construct and its associated */
/*   parsing function from the ListOfConstructs. Returns   */
/*   TRUE if the construct type was removed, otherwise     */
/*   FALSE.                                                */
/***********************************************************/
globle int RemoveConstruct(name)
  char *name;
  {
   struct construct *currentPtr, *lastPtr = NULL;
   
   for (currentPtr = ListOfConstructs;
        currentPtr != NULL;
        currentPtr = currentPtr->next)
     {
      if (strcmp(name,currentPtr->constructName) == 0)
        {
         if (lastPtr == NULL)
           { ListOfConstructs = currentPtr->next; }
         else
           { lastPtr->next = currentPtr->next; }
         rtn_struct(construct,currentPtr);
         return(CLIPS_TRUE);
        }
        
      lastPtr = currentPtr;
     }

   return(CLIPS_FALSE);
  }

/************************************************/
/* Save: C access routine for the save command. */
/************************************************/
globle int Save(fileName)
  char *fileName;
  {
   struct callFunctionItem *saveFunction;
   FILE *filePtr;

   /*=====================*/
   /* Open the save file. */
   /*=====================*/
   
   if ((filePtr = fopen(fileName,"w")) == NULL)
     { return(CLIPS_FALSE); }
     
   /*===========================*/
   /* Bypass the router system. */
   /*===========================*/
   
   SetFastSave(filePtr);

   /*======================*/
   /* Save the constructs. */
   /*======================*/

   for (saveFunction = ListOfSaveFunctions;
        saveFunction != NULL;
        saveFunction = saveFunction->next)
     {
#if ANSI_COMPILER
      ((* (VOID (*)(char *)) saveFunction->func))((char *) filePtr);
#else
      (*saveFunction->func)((char *) filePtr);
#endif
     }

   /*======================*/
   /* Close the save file. */
   /*======================*/
   
   fclose(filePtr);
   
   /*===========================*/
   /* Remove the router bypass. */
   /*===========================*/
   
   SetFastSave(NULL);

   /*=========================*/
   /* Return TRUE to indicate */
   /* successful completion.  */
   /*=========================*/
   
   return(CLIPS_TRUE);
  }

/*******************************************************/
/* RemoveSaveFunction: Removes a function from the     */
/*   ListOfSaveFunctions. Returns TRUE if the function */
/*   was successfully removed, otherwise FALSE.        */
/*******************************************************/
globle BOOLEAN RemoveSaveFunction(name)
  char *name;
  {
   int found;

   ListOfSaveFunctions = 
     RemoveFunctionFromCallList(name,ListOfSaveFunctions,&found);

   if (found) return(CLIPS_TRUE);
   
   return(CLIPS_FALSE);
  }

/**********************************/
/* SetCompilationsWatch: Sets the */
/*   value of WatchCompilations.  */
/**********************************/
globle VOID SetCompilationsWatch(value)
  int value;
  {
   WatchCompilations = value;
  }

/*************************************/
/* GetCompilationsWatch: Returns the */
/*   value of WatchCompilations.     */
/*************************************/
globle BOOLEAN GetCompilationsWatch()
  { return(WatchCompilations); }

/**********************************/
/* SetPrintWhileLoading: Sets the */
/*   value of PrintWhileLoading.  */
/**********************************/
globle VOID SetPrintWhileLoading(value)
  BOOLEAN value;
  {
   PrintWhileLoading = value;
  }

/*************************************/
/* GetPrintWhileLoading: Returns the */
/*   value of PrintWhileLoading.     */
/*************************************/
globle BOOLEAN GetPrintWhileLoading()
  {
   return(PrintWhileLoading);
  }
#endif

/*************************************/
/* InitializeConstructs: Initializes */
/*   the Construct Manager.          */
/*************************************/
globle VOID InitializeConstructs()
  {
#if (! RUN_TIME)
   DefineFunction2("clear",   'v', PTIF ClearCommand,   "ClearCommand", "00");
   DefineFunction2("reset",   'v', PTIF ResetCommand,   "ResetCommand", "00");
#endif

#if DEBUGGING_FUNCTIONS && (! RUN_TIME) && (! BLOAD_ONLY)
   AddWatchItem("compilations",0,&WatchCompilations,30,NULL,NULL);
#endif
  }
  
/**************************************/
/* ClearCommand: CLIPS access routine */
/*   for the clear command.           */
/**************************************/
globle VOID ClearCommand()
  {
   if (ArgCountCheck("clear",EXACTLY,0) == -1) return;
   Clear();
   return;
  }

/**************************************/
/* ResetCommand: CLIPS access routine */
/*   for the reset command.           */
/**************************************/
globle VOID ResetCommand()
  {
   if (ArgCountCheck("reset",EXACTLY,0) == -1) return;
   Reset();
   return;
  }

/****************************/
/* Reset: C access routine  */
/*   for the reset command. */
/****************************/
globle VOID Reset()
  {
   struct callFunctionItem *resetPtr;

   /*=====================================*/
   /* The reset command can't be executed */
   /* while a reset is in progress.       */
   /*=====================================*/
   
   if (ResetInProgress) return;

   ResetInProgress = CLIPS_TRUE;
   
   /*================================================*/
   /* If the reset is performed from the top level   */
   /* command prompt, reset the halt execution flag. */
   /*================================================*/
   
   if (CurrentEvaluationDepth == 0) SetHaltExecution(CLIPS_FALSE);

   /*=======================================================*/
   /* Call the before reset function to determine if the    */
   /* reset should continue. [Used by the some of the       */
   /* windowed interfaces to query the user whether a       */
   /* reset should proceed with activations on the agenda.] */
   /*=======================================================*/
   
   if ((BeforeResetFunction != NULL) ? ((*BeforeResetFunction)() == CLIPS_FALSE) : 
                                       CLIPS_FALSE)
     {
      ResetInProgress = CLIPS_FALSE;
      return;
     }
   
   /*===========================*/
   /* Call each reset function. */
   /*===========================*/
   
   for (resetPtr = ListOfResetFunctions;
        (resetPtr != NULL) && (GetHaltExecution() == CLIPS_FALSE);
        resetPtr = resetPtr->next)
     { (*resetPtr->func)(); }

   /*============================================*/
   /* Set the current module to the MAIN module. */
   /*============================================*/
   
   SetCurrentModule((VOID *) FindDefmodule("MAIN"));
   
   /*===========================================*/
   /* Perform periodic cleanup if the reset was */
   /* issued from an embedded controller.       */
   /*===========================================*/
   
   if ((CurrentEvaluationDepth == 0) && (! EvaluatingTopLevelCommand) &&
       (CurrentExpression == NULL))
     { PeriodicCleanup(CLIPS_TRUE,CLIPS_FALSE); }

   /*===================================*/
   /* A reset is no longer in progress. */
   /*===================================*/
   
   ResetInProgress = CLIPS_FALSE;
  }

/************************************/
/* SetBeforeResetFunction: Sets the */
/*  value of BeforeResetFunction.   */
/************************************/
#if ! MAC_MCW /* 03-04-96 */
globle int (*SetBeforeResetFunction(theFunction))(VOID_ARG)
  int (*theFunction)(VOID_ARG);
#else
globle int (*SetBeforeResetFunction(theFunction))()
  int (*theFunction)(VOID_ARG);
#endif
  {
   int (*tempFunction)(VOID_ARG);

   tempFunction = BeforeResetFunction;
   BeforeResetFunction = theFunction;
   return(tempFunction);
  }

/*************************************/
/* AddResetFunction: Adds a function */
/*   to ListOfResetFunctions.        */
/*************************************/
globle BOOLEAN AddResetFunction(name,functionPtr,priority)
  char *name;
#if ANSI_COMPILER
  VOID (*functionPtr)(void);
#else
  VOID (*functionPtr)();
#endif
  int priority;
  {
   ListOfResetFunctions = AddFunctionToCallList(name,priority,
                                                functionPtr,
                                                ListOfResetFunctions);
   return(CLIPS_TRUE);
  }

/*******************************************/
/* RemoveResetFunction: Removes a function */
/*   from the ListOfResetFunctions.        */
/*******************************************/
globle BOOLEAN RemoveResetFunction(name)
  char *name;
  {
   int found;

   ListOfResetFunctions = 
      RemoveFunctionFromCallList(name,ListOfResetFunctions,&found);

   if (found) return(CLIPS_TRUE);
   
   return(CLIPS_FALSE);
  }

/**************************************************/
/* Clear: C access routine for the clear command. */
/**************************************************/
globle VOID Clear()
  {
   struct callFunctionItem *theFunction;
   
   /*==========================================*/
   /* Activate the watch router which captures */
   /* trace output so that it is not displayed */
   /* during a clear.                          */
   /*==========================================*/
   
#if DEBUGGING_FUNCTIONS
   ActivateRouter(WTRACE);
#endif

   /*===================================*/
   /* Determine if a clear is possible. */ 
   /*===================================*/

   if (ClearReady() == CLIPS_FALSE)
     {
      PrintErrorID("CONSTRCT",1,CLIPS_FALSE);
      PrintCLIPS(WERROR,"Some constructs are still in use. Clear cannot continue.\n");
#if DEBUGGING_FUNCTIONS
      DeactivateRouter(WTRACE);
#endif
      return;
     }

   /*===========================*/
   /* Call all clear functions. */
   /*===========================*/
   
   ClearInProgress = CLIPS_TRUE;

   for (theFunction = ListOfClearFunctions;
        theFunction != NULL;
        theFunction = theFunction->next)
     { (*theFunction->func)(); }

   /*=============================*/
   /* Deactivate the watch router */
   /* for capturing output.       */
   /*=============================*/

#if DEBUGGING_FUNCTIONS
   DeactivateRouter(WTRACE);
#endif
   
   /*===========================================*/
   /* Perform periodic cleanup if the clear was */
   /* issued from an embedded controller.       */
   /*===========================================*/

   if ((CurrentEvaluationDepth == 0) && (! EvaluatingTopLevelCommand) &&
       (CurrentExpression == NULL))     /* 03-04-96 */
     { PeriodicCleanup(CLIPS_TRUE,CLIPS_FALSE); }

   /*===========================*/
   /* Clear has been completed. */
   /*===========================*/
   
   ClearInProgress = CLIPS_FALSE;
  }

/*********************************************************/
/* ClearReady: Returns TRUE if a clear can be performed, */
/*   otherwise FALSE. Note that this is destructively    */
/*   determined (e.g. facts will be deleted as part of   */
/*   the determination).                                 */ 
/*********************************************************/
globle BOOLEAN ClearReady()
  {
   struct callFunctionItem *theFunction;
   int (*tempFunction)(VOID_ARG);

   for (theFunction = ListOfClearReadyFunctions;
        theFunction != NULL;
        theFunction = theFunction->next)
     {
      tempFunction = (int (*)(VOID_ARG)) theFunction->func;
      if ((*tempFunction)() == CLIPS_FALSE)
        { return(CLIPS_FALSE); }
     }

   return(CLIPS_TRUE);
  }
  
/******************************************/
/* AddClearReadyFunction: Adds a function */
/*   to ListOfClearReadyFunctions.        */
/******************************************/
globle BOOLEAN AddClearReadyFunction(name,functionPtr,priority)
  char *name;
#if ANSI_COMPILER
  int (*functionPtr)(void);
#else
  int (*functionPtr)();
#endif
  int priority;
  {
   ListOfClearReadyFunctions = 
     AddFunctionToCallList(name,priority,
#if ANSI_COMPILER
                           (VOID (*)(void)) functionPtr,
#else
                           (VOID (*)()) functionPtr,
#endif
                           ListOfClearReadyFunctions);
   return(1);
  }

/************************************************/
/* RemoveClearReadyFunction: Removes a function */
/*   from the ListOfClearReadyFunctions.        */
/************************************************/
globle BOOLEAN RemoveClearReadyFunction(name)
  char *name;
  {
   int found;

   ListOfClearReadyFunctions = 
      RemoveFunctionFromCallList(name,ListOfClearReadyFunctions,&found);

   if (found) return(CLIPS_TRUE);
   
   return(CLIPS_FALSE);
  }

/*************************************/
/* AddClearFunction: Adds a function */
/*   to ListOfClearFunctions.        */
/*************************************/
globle BOOLEAN AddClearFunction(name,functionPtr,priority)
  char *name;
#if ANSI_COMPILER
  VOID (*functionPtr)(void);
#else
  VOID (*functionPtr)();
#endif
  int priority;
  {
   ListOfClearFunctions = 
      AddFunctionToCallList(name,priority,
#if ANSI_COMPILER
                            (VOID (*)(void)) functionPtr,
#else
                            (VOID (*)()) functionPtr,
#endif
                            ListOfClearFunctions);
   return(1);
  }

/*******************************************/
/* RemoveClearFunction: Removes a function */
/*    from the ListOfClearFunctions.       */
/*******************************************/
globle BOOLEAN RemoveClearFunction(name)
  char *name;
  {
   int found;

   ListOfClearFunctions = 
     RemoveFunctionFromCallList(name,ListOfClearFunctions,&found);

   if (found) return(CLIPS_TRUE);
   
   return(CLIPS_FALSE);
  }

/***********************************************/
/* ExecutingConstruct: Returns CLIPS_TRUE if a */
/*   construct is currently being executed,    */
/*   otherwise CLIPS_FALSE.                    */
/***********************************************/
globle int ExecutingConstruct()
  { return(Executing); }

/********************************************/
/* SetExecutingConstruct: Sets the value of */
/*   the executing variable indicating that */
/*   actions such as reset, clear, etc      */
/*   should not be performed.               */
/********************************************/
globle VOID SetExecutingConstruct(value)
  int value;
  {
   Executing = value;
  }

/************************************************************/
/* OldGetConstructList: Returns a list of all the construct */
/*   names in a multifield value. It doesn't check the      */
/*   number of arguments. It assumes that the restriction   */
/*   string in DefineFunction2 call was "00".               */
/************************************************************/
globle VOID OldGetConstructList(returnValue,nextFunction,nameFunction)
  DATA_OBJECT_PTR returnValue;
#if ANSI_COMPILER
  VOID *(*nextFunction)(VOID *);
  char *(*nameFunction)(VOID *);
#else
  VOID *(*nextFunction)();
  char *(*nameFunction)();
#endif
  {
   VOID *theConstruct;
   long count = 0;
   struct multifield *theList;

   /*====================================*/
   /* Determine the number of constructs */
   /* of the specified type.             */
   /*====================================*/
   
   for (theConstruct = (*nextFunction)(NULL);
        theConstruct != NULL;
        theConstruct = (*nextFunction)(theConstruct))
     { count++; }
   
   /*===========================*/
   /* Create a multifield large */
   /* enough to store the list. */
   /*===========================*/
   
   SetpType(returnValue,MULTIFIELD);
   SetpDOBegin(returnValue,1);
   SetpDOEnd(returnValue,count);
   theList = (struct multifield *) CreateMultifield((int) count);
   SetpValue(returnValue,(VOID *) theList);
   
   /*====================================*/
   /* Store the names in the multifield. */
   /*====================================*/
   
   for (theConstruct = (*nextFunction)(NULL), count = 1;
        theConstruct != NULL;
        theConstruct = (*nextFunction)(theConstruct), count++)
     {
      if (HaltExecution == CLIPS_TRUE)
        {
         SetMultifieldErrorValue(returnValue);
         return;
        }
      SetMFType(theList,count,SYMBOL);
      SetMFValue(theList,count,AddSymbol((*nameFunction)(theConstruct)));
     }
  }
  
/*******************************************************/
/* DeinstallConstructHeader: Decrements the busy count */
/*   of a construct name and frees its pretty print    */
/*   representation string (both of which are stored   */
/*   in the generic construct header).                 */
/*******************************************************/
globle VOID DeinstallConstructHeader(theHeader)
  struct constructHeader *theHeader;
  {   
   DecrementSymbolCount(theHeader->name);
   if (theHeader->ppForm != NULL)
     {
      rm(theHeader->ppForm,
         (int) sizeof(char) * ((int) strlen(theHeader->ppForm) + 1));
      theHeader->ppForm = NULL;
     }
  }

/*****************************************************/
/* AddConstruct: Adds a construct and its associated */
/*   parsing function to the ListOfConstructs.       */
/*****************************************************/
globle struct construct *AddConstruct(name,pluralName,
                                      parseFunction,findFunction,
                                      getConstructNameFunction,getPPFormFunction,
                                      getModuleItemFunction,getNextItemFunction,
                                      setNextItemFunction,isConstructDeletableFunction,
                                      deleteFunction,freeFunction)
  char *name, *pluralName;
#if ANSI_COMPILER
  int (*parseFunction)(char *);
  VOID *(*findFunction)(char *);
  SYMBOL_HN *(*getConstructNameFunction)(struct constructHeader *);
  char *(*getPPFormFunction)(struct constructHeader *);
  struct defmoduleItemHeader *(*getModuleItemFunction)(struct constructHeader *);
  VOID *(*getNextItemFunction)(VOID *);
  VOID (*setNextItemFunction)(struct constructHeader *,struct constructHeader *);
  BOOLEAN (*isConstructDeletableFunction)(VOID *);
  int (*deleteFunction)(VOID *);
  VOID (*freeFunction)(VOID *);
#else
  int (*parseFunction)();
  VOID *(*findFunction)();
  SYMBOL_HN *(*getConstructNameFunction)();
  char *(*getPPFormFunction)();
  struct defmoduleItemHeader *(*getModuleItemFunction)();
  VOID *(*getNextItemFunction)();
  VOID (*setNextItemFunction)();
  BOOLEAN (*isConstructDeletableFunction)();
  int (*deleteFunction)();
  VOID (*freeFunction)();
#endif
  {
   struct construct *newPtr;

   /*=============================*/
   /* Allocate and initialize the */
   /* construct data structure.   */
   /*=============================*/
   
   newPtr = get_struct(construct);

   newPtr->constructName = name;
   newPtr->pluralName = pluralName;
   newPtr->parseFunction = parseFunction;
   newPtr->findFunction = findFunction;
   newPtr->getConstructNameFunction = getConstructNameFunction;
   newPtr->getPPFormFunction = getPPFormFunction;
   newPtr->getModuleItemFunction = getModuleItemFunction;
   newPtr->getNextItemFunction = getNextItemFunction;
   newPtr->setNextItemFunction = setNextItemFunction;
   newPtr->isConstructDeletableFunction = isConstructDeletableFunction;
   newPtr->deleteFunction = deleteFunction;
   newPtr->freeFunction = freeFunction;   
   
   /*===============================*/
   /* Add the construct to the list */
   /* of constructs and return it.  */
   /*===============================*/
   
   newPtr->next = ListOfConstructs;
   ListOfConstructs = newPtr;
   return(newPtr);
  }
  
/************************************/
/* AddSaveFunction: Adds a function */
/*   to the ListOfSaveFunctions.    */
/************************************/
globle BOOLEAN AddSaveFunction(name,functionPtr,priority)
  char *name;
#if ANSI_COMPILER
  VOID (*functionPtr)(char *);
#else
  VOID (*functionPtr)();
#endif
  int priority;
  {
#if (MAC_MPW || MAC_MCW) && (RUN_TIME || BLOAD_ONLY) /*added 03-04-96*/
#pragma unused(name)
#pragma unused(functionPtr)
#pragma unused(priority)
#endif
#if (! RUN_TIME) && (! BLOAD_ONLY)
   ListOfSaveFunctions = 
     AddFunctionToCallList(name,priority,
#if ANSI_COMPILER
                           (VOID (*)(void)) functionPtr,
#else
                           (VOID (*)()) functionPtr,
#endif
                           ListOfSaveFunctions);
#endif

   return(1);
  }
