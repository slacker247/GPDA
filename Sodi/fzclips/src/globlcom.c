   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.01  10/29/93            */
   /*                                                     */
   /*              DEFGLOBAL COMMANDS MODULE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides the show-defglobals, set-reset-globals, */
/*   and get-reset-globals commands.                         */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#define _GLOBLCOM_SOURCE_

#include "setup.h"

#if DEFGLOBAL_CONSTRUCT

#include "extnfunc.h"
#include "argacces.h"
#include "prntutil.h"
#include "router.h"

#include "globldef.h"

#include "globlcom.h"

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static BOOLEAN               ResetGlobals = CLIPS_TRUE;

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if DEBUGGING_FUNCTIONS
#if ANSI_COMPILER
   static VOID                       PrintDefglobalValueForm(char *,VOID *);
#else
   static VOID                       PrintDefglobalValueForm();
#endif
#endif

/************************************************************/
/* DefglobalCommandDefinitions: Defines defglobal commands. */
/************************************************************/
globle VOID DefglobalCommandDefinitions()
  {
#if ! RUN_TIME
   DefineFunction2("set-reset-globals",'b',
                  SetResetGlobalsCommand,"SetResetGlobalsCommand", "11");
   DefineFunction2("get-reset-globals",'b',
                   GetResetGlobalsCommand,"GetResetGlobalsCommand", "00");
                   
#if DEBUGGING_FUNCTIONS
   DefineFunction2("show-defglobals",'v',
                   PTIF ShowDefglobalsCommand,"ShowDefglobalsCommand", "01w");
#endif

#endif
  }

/************************************************/
/* SetResetGlobalsCommand: CLIPS access routine */
/*   for the get-reset-globals command.         */
/************************************************/
globle int SetResetGlobalsCommand()
  {
   int oldValue;
   DATA_OBJECT arg_ptr;

   /*===========================================*/
   /* Remember the old value of this attribute. */
   /*===========================================*/
   
   oldValue = GetResetGlobals();

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/
   
   if (ArgCountCheck("set-reset-globals",EXACTLY,1) == -1)
     { return(oldValue); }

   /*===========================================*/
   /* Determine the new value of the attribute. */
   /*===========================================*/
   
   RtnUnknown(1,&arg_ptr);

   if ((arg_ptr.value == CLIPSFalseSymbol) && (arg_ptr.type == SYMBOL))
     { SetResetGlobals(CLIPS_FALSE); }
   else
     { SetResetGlobals(CLIPS_TRUE); }

   /*========================================*/
   /* Return the old value of the attribute. */
   /*========================================*/
   
   return(oldValue);
  }
  
/*****************************************/
/* SetResetGlobals: C access routine for */
/*   the set-reset-globals command.      */
/*****************************************/
globle BOOLEAN SetResetGlobals(value)
  int value;
  {
   int ov;

   ov = ResetGlobals;
   ResetGlobals = value;
   return(ov);
  }
  
/************************************************/
/* GetResetGlobalsCommand: CLIPS access routine */
/*   for the get-reset-globals command.         */
/************************************************/
globle int GetResetGlobalsCommand()
  {
   int oldValue;

   oldValue = GetResetGlobals();

   if (ArgCountCheck("get-reset-globals",EXACTLY,0) == -1)
     { return(oldValue); }

   return(oldValue);
  }
  
/*****************************************/
/* GetResetGlobals: C access routine for */
/*   the get-reset-globals command.      */
/*****************************************/
globle BOOLEAN GetResetGlobals()
  { return(ResetGlobals); }

#if DEBUGGING_FUNCTIONS

/***********************************************/
/* ShowDefglobalsCommand: CLIPS access routine */
/*   for the show-defglobals command.          */
/***********************************************/
globle VOID ShowDefglobalsCommand()
  {
   struct defmodule *theModule;
   int numArgs, error;
   
   if ((numArgs = ArgCountCheck("show-defglobals",NO_MORE_THAN,1)) == -1) return;

   if (numArgs == 1)
     {
      theModule = GetModuleName("show-defglobals",1,&error);
      if (error) return;
     }
   else
     { theModule = ((struct defmodule *) GetCurrentModule()); }
   
   ShowDefglobals(WDISPLAY,theModule);
  }

/**************************************/
/* ShowDefglobals: C access routine   */
/*   for the show-defglobals command. */
/**************************************/
globle VOID ShowDefglobals(logicalName,vTheModule)
  char *logicalName;
  VOID *vTheModule;
  {
   struct defmodule *theModule = (struct defmodule *) vTheModule;
   struct constructHeader *constructPtr;
   int allModules = CLIPS_FALSE;
   struct defmoduleItemHeader *theModuleItem;
   
   /*=======================================*/
   /* If the module specified is NULL, then */
   /* list all constructs in all modules.   */
   /*=======================================*/
  
   if (theModule == NULL)
     { 
      theModule = (struct defmodule *) GetNextDefmodule(NULL);
      allModules = CLIPS_TRUE;
     }
     
   /*======================================================*/
   /* Print out the constructs in the specified module(s). */
   /*======================================================*/
   
   for (;
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      /*===========================================*/
      /* Print the module name before every group  */
      /* of defglobals listed if we're listing the */
      /* defglobals from every module.             */
      /*===========================================*/
      
      if (allModules) 
        {
         PrintCLIPS(logicalName,GetDefmoduleName(theModule));
         PrintCLIPS(logicalName,":\n");
        }
      
      /*=====================================*/
      /* Print every defglobal in the module */
      /* currently being examined.           */
      /*=====================================*/
      
      theModuleItem = (struct defmoduleItemHeader *) GetModuleItem(theModule,DefglobalModuleIndex);

      for (constructPtr = theModuleItem->firstItem;
           constructPtr != NULL;
           constructPtr = constructPtr->next)
        {
         if (HaltExecution == CLIPS_TRUE) return;
              
         if (allModules) PrintCLIPS(logicalName,"   ");
         PrintDefglobalValueForm(logicalName,(VOID *) constructPtr);
         PrintCLIPS(logicalName,"\n");
        }
        
      /*===================================*/
      /* If we're only listing the globals */
      /* for one module, then return.      */
      /*===================================*/
      
      if (! allModules) return;
     }
  }
  
/*****************************************************/
/* PrintDefglobalValueForm: Prints the value form of */
/*   a defglobal (the current value). For example,   */
/*   ?*x* = 3                                        */
/*****************************************************/
static VOID PrintDefglobalValueForm(logicalName,vTheGlobal)
  char *logicalName;
  VOID *vTheGlobal;
  {
   struct defglobal *theGlobal = (struct defglobal *) vTheGlobal;

   PrintCLIPS(logicalName,"?*");
   PrintCLIPS(logicalName,ValueToString(theGlobal->header.name));
   PrintCLIPS(logicalName,"* = ");
   PrintDataObject(logicalName,&theGlobal->current);
  }
  
#endif /* DEBUGGING_FUNCTIONS */

#endif /* DEFGLOBAL_CONSTRUCT */

