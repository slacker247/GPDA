   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.10 07/22/94             */
   /*                                                     */
   /*              CONSTRUCT COMMANDS MODULE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains generic routines for deleting, pretty   */
/*   printing, finding, obtaining module information,        */
/*   obtaining lists of constructs, listing constructs, and  */
/*   manipulation routines.                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#define _CSTRCCOM_SOURCE_

#include <string.h>

#include "setup.h"

#include "constant.h"
#include "clipsmem.h"
#include "moduldef.h"
#include "argacces.h"
#include "multifld.h"
#include "modulutl.h"
#include "router.h"
#include "utility.h"
#include "commline.h"

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
#include "bload.h"
#endif

#if (! BLOAD_ONLY) && (! RUN_TIME)
#include "cstrcpsr.h"
#endif

#include "cstrccom.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if ANSI_COMPILER

#if DEBUGGING_FUNCTIONS
   static VOID                    ConstructPrintWatch(char *,struct construct *,VOID *,
                                                      BOOLEAN (*)(VOID *));
   static BOOLEAN                 ConstructWatchSupport(struct construct *,char *,
                                                        char *,EXPRESSION *,BOOLEAN,
                                                        BOOLEAN,BOOLEAN (*)(VOID *),
                                                        VOID (*)(BOOLEAN,VOID *));
#endif

#else

#if DEBUGGING_FUNCTIONS
   static VOID                    ConstructPrintWatch();
   static BOOLEAN                 ConstructWatchSupport();
#endif

#endif

#if (! RUN_TIME)

/************************************/
/* AddConstructToModule: Adds a     */
/* construct to the current module. */
/************************************/
globle VOID AddConstructToModule(theConstruct)
  struct constructHeader *theConstruct;
  {
   if (theConstruct->whichModule->lastItem == NULL)
     { theConstruct->whichModule->firstItem = theConstruct; }
   else
     { theConstruct->whichModule->lastItem->next = theConstruct; }
   
   theConstruct->whichModule->lastItem = theConstruct;
   theConstruct->next = NULL;
  }

#endif /* (! RUN_TIME) */

/****************************************************/
/* DeleteNamedConstruct: Generic driver routine for */
/*   deleting a specific construct from a module.   */
/****************************************************/
globle BOOLEAN DeleteNamedConstruct(constructName,constructClass)
  char *constructName;
  struct construct *constructClass;
  {
#if (! BLOAD_ONLY)
   VOID *constructPtr;

   /*=============================*/
   /* Constructs can't be deleted */
   /* while a bload is in effect. */
   /*=============================*/
   
#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
   if (Bloaded() == CLIPS_TRUE) return(CLIPS_FALSE);
#endif

   /*===============================*/
   /* Look for the named construct. */
   /*===============================*/
   
   constructPtr = (*constructClass->findFunction)(constructName);

   /*========================================*/
   /* If the construct was found, delete it. */
   /*========================================*/
   
   if (constructPtr != NULL)
     { return((*constructClass->deleteFunction)(constructPtr)); }
 
   /*========================================*/
   /* If the construct wasn't found, but the */
   /* special symbol * was used, then delete */
   /* all constructs of the specified type.  */
   /*========================================*/
   
   if (strcmp("*",constructName) == 0)
     {
      (*constructClass->deleteFunction)(NULL);
      return(CLIPS_TRUE);
     }

   /*===============================*/
   /* Otherwise, return FALSE to    */
   /* indicate no deletion occured. */
   /*===============================*/
              
   return(CLIPS_FALSE);
#else
   return(CLIPS_FALSE);
#endif
  }

/*******************************************/
/* FindNamedConstruct: Generic routine for */
/*   searching for a specified construct.  */
/*******************************************/
globle VOID *FindNamedConstruct(constructName,constructClass)
  char *constructName;
  struct construct *constructClass;
  {
   VOID *theConstruct;
   SYMBOL_HN *findValue; 
   
   /*==========================*/
   /* Save the current module. */
   /*==========================*/
   
   SaveCurrentModule();
     
   /*=========================================================*/
   /* Extract the construct name. If a module was specified,  */
   /* then ExtractModuleAndConstructName will set the current */
   /* module to the module specified in the name.             */
   /*=========================================================*/
   
   constructName = ExtractModuleAndConstructName(constructName);

   /*=================================================*/
   /* If a valid construct name couldn't be extracted */
   /* or the construct name isn't in the symbol table */
   /* (which means the construct doesn't exist), then */
   /* return NULL to indicate the specified construct */
   /* couldn't be found.                              */
   /*=================================================*/
   
   if ((constructName == NULL) ?
       CLIPS_TRUE :
       ((findValue = (SYMBOL_HN *) FindSymbol(constructName)) == NULL))
     {
      RestoreCurrentModule();
      return(NULL);
     }

   /*===============================================*/
   /* Loop through every construct of the specified */
   /* class in the current module checking to see   */
   /* if the construct's name matches the construct */
   /* being sought. If found, restore the current   */
   /* module and return a pointer to the construct. */
   /*===============================================*/
   
   for (theConstruct = (*constructClass->getNextItemFunction)(NULL);
        theConstruct != NULL;
        theConstruct = (*constructClass->getNextItemFunction)(theConstruct))
     { 
      if (findValue == (*constructClass->getConstructNameFunction)(theConstruct)) 
        { 
         RestoreCurrentModule();
         return (theConstruct);
        }
     } 

   /*=============================*/
   /* Restore the current module. */
   /*=============================*/
   RestoreCurrentModule();
   
   /*====================================*/
   /* Return NULL to indicated the named */
   /* construct was not found.           */
   /*====================================*/
   
   return(NULL);
  }

/*****************************************/
/* UndefconstructCommand: Driver routine */
/*   for the undef<construct> commands.  */
/*****************************************/
globle VOID UndefconstructCommand(command,constructClass)
  char *command;
  struct construct *constructClass;
  {
   char *constructName;
   char buffer[80];
   
   /*==============================================*/
   /* Get the name of the construct to be deleted. */
   /*==============================================*/
   
   sprintf(buffer,"%s name",constructClass->constructName);

   constructName = GetConstructName(command,buffer);
   if (constructName == NULL) return;

#if (! RUN_TIME) && (! BLOAD_ONLY)

   /*=============================================*/
   /* Check to see if the named construct exists. */
   /*=============================================*/
   
   if (((*constructClass->findFunction)(constructName) == CLIPS_FALSE) && 
       (strcmp("*",constructName) != 0))
     {
      CantFindItemErrorMessage(constructClass->constructName,constructName);
      return;
     }
     
   /*===============================================*/
   /* If the construct does exist, try deleting it. */
   /*===============================================*/
   
   else if (DeleteNamedConstruct(constructName,constructClass) == CLIPS_FALSE)
     {
      CantDeleteItemErrorMessage(constructClass->constructName,constructName);
      return;
     }

   return;
#else
   /*=====================================*/
   /* Constructs can't be deleted in a    */
   /* run-time or bload only environment. */
   /*=====================================*/
   
   CantDeleteItemErrorMessage(constructClass->constructName,constructName);
   return;
#endif
  }
   
/******************************************/
/* PPConstructCommand: Driver routine for */
/*   the ppdef<construct> commands.       */
/******************************************/
globle VOID PPConstructCommand(command,constructClass)
  char *command;
  struct construct *constructClass;
  {
   char *constructName;
   char buffer[80];
   
   /*===============================*/
   /* Get the name of the construct */
   /* to be "pretty printed."       */
   /*===============================*/
   
   sprintf(buffer,"%s name",constructClass->constructName);

   constructName = GetConstructName(command,buffer);
   if (constructName == NULL) return;

   /*================================*/
   /* Call the driver routine for    */
   /* pretty printing the construct. */
   /*================================*/
   
   if (PPConstruct(constructName,WDISPLAY,constructClass) == CLIPS_FALSE)
     { CantFindItemErrorMessage(constructClass->constructName,constructName); }
  }
  
/***********************************/
/* PPConstruct: Driver routine for */
/*   pretty printing a construct.  */
/***********************************/
globle int PPConstruct(constructName,logicalName,constructClass)
  char *constructName;
  char *logicalName;
  struct construct *constructClass;
  {
   VOID *constructPtr;

   /*==================================*/
   /* Use the construct's name to find */
   /* a pointer to actual construct.   */
   /*==================================*/
   
   constructPtr = (*constructClass->findFunction)(constructName);
   if (constructPtr == NULL) return(CLIPS_FALSE);

   /*==============================================*/
   /* If the pretty print form is NULL (because of */ 
   /* conserve-mem), return TRUE (which indicates  */
   /* the construct was found).                    */
   /*==============================================*/
   
   if ((*constructClass->getPPFormFunction)(constructPtr) == NULL) 
     { return(CLIPS_TRUE); }
   
   /*============================================*/
   /* Print the pretty print string in smaller   */
   /* chunks. (VMS had a bug that didn't allow   */
   /* printing a string greater than 512 bytes.) */
   /*============================================*/
   
   PrintInChunks(logicalName,(*constructClass->getPPFormFunction)(constructPtr));
   
   /*=======================================*/
   /* Return TRUE to indicate the construct */
   /* was found and pretty printed.         */
   /*=======================================*/
   
   return(CLIPS_TRUE);
  }

/*********************************************/
/* GetConstructModuleCommand: Driver routine */
/*   for def<construct>-module routines      */
/*********************************************/
globle SYMBOL_HN *GetConstructModuleCommand(command,constructClass)
  char *command;
  struct construct *constructClass;
  {
   char *constructName;
   char buffer[80];
   struct defmodule *constructModule;
   
   /*=========================================*/
   /* Get the name of the construct for which */
   /* we want to determine its module.        */
   /*=========================================*/
   
   sprintf(buffer,"%s name",constructClass->constructName);

   constructName = GetConstructName(command,buffer);
   if (constructName == NULL) return((SYMBOL_HN *) CLIPSFalseSymbol);

   /*==========================================*/
   /* Get a pointer to the construct's module. */
   /*==========================================*/
   
   constructModule = GetConstructModule(constructName,constructClass);
   if (constructModule == NULL)
     {
      CantFindItemErrorMessage(constructClass->constructName,constructName);
      return((SYMBOL_HN *) CLIPSFalseSymbol);
     }
     
   /*============================================*/
   /* Return the name of the construct's module. */
   /*============================================*/
   
   return(constructModule->name);
  }
  
/******************************************/
/* GetConstructModule: Driver routine for */
/*   getting the module for a construct   */
/******************************************/
globle struct defmodule *GetConstructModule(constructName,constructClass)
  char *constructName;
  struct construct *constructClass;
  {
   struct constructHeader *constructPtr;
   int count, position;
   SYMBOL_HN *theName;
   
   /*====================================================*/
   /* If the construct name contains a module specifier, */
   /* then get a pointer to the defmodule associated     */
   /* with the specified name.                           */
   /*====================================================*/
   
   if ((position = FindModuleSeparator(constructName)) != CLIPS_FALSE)
     {
      theName = ExtractModuleName(position,constructName);
      if (theName != NULL) 
        { return((struct defmodule *) FindDefmodule(ValueToString(theName))); }
     }
     
   /*============================================*/
   /* No module was specified, so search for the */
   /* named construct in the current module and  */
   /* modules from which it imports.             */
   /*============================================*/
   
   constructPtr = (struct constructHeader *) 
                  FindImportedConstruct(constructClass->constructName,NULL,constructName,
                                        &count,CLIPS_TRUE,NULL);
   if (constructPtr == NULL) return(NULL);

   return(constructPtr->whichModule->theModule);
  }

/*************************************/
/* Undefconstruct: Generic C routine */
/*   for deleting a construct.       */
/*************************************/
globle BOOLEAN Undefconstruct(theConstruct,constructClass)
  VOID *theConstruct;
  struct construct *constructClass;
  {
#if BLOAD_ONLY || RUN_TIME
#if MAC_MPW || MAC_MCW   /* added 03-05-96 */
#pragma unused(theConstruct)
#pragma unused(constructClass)
#endif
   return(CLIPS_FALSE);
#else
   VOID *currentConstruct,*nextConstruct;
   BOOLEAN success;
   
   /*================================================*/
   /* Delete all constructs of the specified type if */
   /* the construct pointer is the NULL pointer.     */
   /*================================================*/

   if (theConstruct == NULL)
     {
      success = CLIPS_TRUE;
      
      /*===================================================*/
      /* Loop through all of the constructs in the module. */
      /*===================================================*/
      
      currentConstruct = (*constructClass->getNextItemFunction)(NULL);
      while (currentConstruct != NULL)
        {
         /*==============================*/
         /* Remember the next construct. */
         /*==============================*/
         
         nextConstruct = (*constructClass->getNextItemFunction)(currentConstruct);
         
         /*=============================*/
         /* Try deleting the construct. */
         /*=============================*/
         
         if ((*constructClass->isConstructDeletableFunction)(currentConstruct))
           {
            RemoveConstructFromModule((struct constructHeader *) currentConstruct);
            (*constructClass->freeFunction)(currentConstruct);
           }
         else
           {
            CantDeleteItemErrorMessage(constructClass->constructName,
                        ValueToString((*constructClass->getConstructNameFunction)(currentConstruct)));
            success = CLIPS_FALSE;
           }
           
         /*================================*/
         /* Move on to the next construct. */
         /*================================*/
         
         currentConstruct = nextConstruct;
        }
        
      /*================================================*/
      /* Perform periodic cleanup if CLIPS is embedded. */
      /*================================================*/
      
      if ((CurrentEvaluationDepth == 0) && (! EvaluatingTopLevelCommand) &&
          (CurrentExpression == NULL))
        { PeriodicCleanup(CLIPS_TRUE,CLIPS_FALSE); }

      /*============================================*/
      /* Return TRUE if all constructs successfully */
      /* deleted, otherwise FALSE.                  */
      /*============================================*/
      
      return(success);
     }
     
   /*==================================================*/
   /* Return FALSE if the construct cannot be deleted. */
   /*==================================================*/

   if ((*constructClass->isConstructDeletableFunction)(theConstruct) == CLIPS_FALSE)
     { return(CLIPS_FALSE); }

   /*===========================*/
   /* Remove the construct from */
   /* the list in its module.   */
   /*===========================*/
   
   RemoveConstructFromModule((struct constructHeader *) theConstruct);
   
   /*=======================*/
   /* Delete the construct. */
   /*=======================*/
   
   (*constructClass->freeFunction)(theConstruct);
   
   /*================================================*/
   /* Perform periodic cleanup if CLIPS is embedded. */
   /*================================================*/

   if ((CurrentEvaluationDepth == 0) && (! EvaluatingTopLevelCommand) &&
       (CurrentExpression == NULL))
     { PeriodicCleanup(CLIPS_TRUE,CLIPS_FALSE); }

   /*=============================*/
   /* Return TRUE to indicate the */
   /* construct was deleted.      */
   /*=============================*/
   
   return(CLIPS_TRUE);
#endif
  }
  
/***********************************/
/* SaveConstruct: Generic routine  */
/*   for saving a construct class. */
/***********************************/
globle VOID SaveConstruct(logicalName,constructClass)
  char *logicalName;
  struct construct *constructClass;
  {
   struct defmodule *theModule;
   char *ppform;
   struct constructHeader *theConstruct;

   /*==========================*/
   /* Save the current module. */
   /*==========================*/
   
   SaveCurrentModule();
   
   /*===========================*/
   /* Loop through each module. */
   /*===========================*/
   
   for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      /*===========================*/
      /* Set the current module to */
      /* the one we're examining.  */
      /*===========================*/
      
      SetCurrentModule((VOID *) theModule);
      
      /*==============================================*/
      /* Loop through each construct of the specified */
      /* construct class in the module.               */
      /*==============================================*/
      
      for (theConstruct = (struct constructHeader *)
                          (*constructClass->getNextItemFunction)(NULL);
           theConstruct != NULL;
           theConstruct = (struct constructHeader *)
                          (*constructClass->getNextItemFunction)(theConstruct))
        {
         /*==========================================*/
         /* Print the construct's pretty print form. */
         /*==========================================*/
         
         ppform = (*constructClass->getPPFormFunction)(theConstruct);    
         if (ppform != NULL)
           {
            PrintInChunks(logicalName,ppform);
            PrintCLIPS(logicalName,"\n");
           }
        }
     }
     
   /*=============================*/
   /* Restore the current module. */
   /*=============================*/
   
   RestoreCurrentModule();
  }
  
/*********************************************************/
/* GetConstructModuleName: Generic routine for returning */
/*   the name of the module to which a construct belongs */
/*********************************************************/
globle char *GetConstructModuleName(theConstruct)
  struct constructHeader *theConstruct;
  { return(GetDefmoduleName((VOID *) theConstruct->whichModule->theModule)); }

/*********************************************************/
/* GetConstructNameString: Generic routine for returning */
/*   the name string of a construct.                     */
/*********************************************************/
globle char *GetConstructNameString(theConstruct)
  struct constructHeader *theConstruct;
  { return(ValueToString(theConstruct->name)); }

/**********************************************************/
/* GetConstructNamePointer: Generic routine for returning */
/*   the name pointer of a construct.                     */
/**********************************************************/
globle SYMBOL_HN *GetConstructNamePointer(theConstruct)
  struct constructHeader *theConstruct;
  { return(theConstruct->name); }

/***************************************************/
/* GetConstructListFunction: Generic CLIPS Routine */
/*   for retrieving the constructs in a module.    */
/***************************************************/
globle VOID GetConstructListFunction(functionName,returnValue,constructClass)
  char *functionName;
  DATA_OBJECT_PTR returnValue;
  struct construct *constructClass;
  {
   struct defmodule *theModule;
   DATA_OBJECT result;
   int numArgs;
   
   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/
   
   if ((numArgs = ArgCountCheck(functionName,NO_MORE_THAN,1)) == -1)
     {      
      SetMultifieldErrorValue(returnValue);
      return;
     }
     
   /*====================================*/
   /* If an argument was given, check to */
   /* see that it's a valid module name. */
   /*====================================*/

   if (numArgs == 1)
     {
      /*======================================*/
      /* Only symbols are valid module names. */
      /*======================================*/
      
      RtnUnknown(1,&result);
      if (GetType(result) != SYMBOL)
        {
         SetMultifieldErrorValue(returnValue);
         ExpectedTypeError1(functionName,1,"defmodule name");
         return;
        }
        
      /*===========================================*/
      /* Verify that the named module exists or is */
      /* the symbol * (for obtaining the construct */
      /* list for all modules).                    */
      /*===========================================*/
                   
      if ((theModule = (struct defmodule *) FindDefmodule(DOToString(result))) == NULL)
        {
         if (strcmp("*",DOToString(result)) != 0) 
           {
            SetMultifieldErrorValue(returnValue);
            ExpectedTypeError1(functionName,1,"defmodule name");
            return;
           }
           
         theModule = NULL;
        }
     }
     
   /*=====================================*/
   /* Otherwise use the current module to */
   /* generate the construct list.        */
   /*=====================================*/
   
   else
     { theModule = ((struct defmodule *) GetCurrentModule()); }
  
   /*=============================*/
   /* Call the driver routine to  */
   /* get the list of constructs. */
   /*=============================*/
   
   GetConstructList(returnValue,constructClass,theModule);
  }
  
/********************************************/
/* GetConstructList: Generic C Routine for  */
/*   retrieving the constructs in a module. */
/********************************************/
globle VOID GetConstructList(returnValue,constructClass,theModule)
  DATA_OBJECT_PTR returnValue;
  struct construct *constructClass;
  struct defmodule *theModule;
  { 
   VOID *theConstruct;
   long count = 0;
   struct multifield *theList;
   SYMBOL_HN *theName;
   struct defmodule *loopModule;
   int allModules = CLIPS_FALSE;
   
   /*==========================*/
   /* Save the current module. */
   /*==========================*/
   
   SaveCurrentModule();
      
   /*=======================================*/
   /* If the module specified is NULL, then */
   /* get all constructs in all modules.    */
   /*=======================================*/

   if (theModule == NULL)
     {
      theModule = (struct defmodule *) GetNextDefmodule(NULL);
      allModules = CLIPS_TRUE;
     }
     
   /*================================*/
   /* Count the number of constructs */
   /* to be retrieved.               */
   /*================================*/
   
   loopModule = theModule;
   while (loopModule != NULL)
     {
      SetCurrentModule((VOID *) loopModule);
      theConstruct = NULL;
      while ((theConstruct = (*constructClass->getNextItemFunction)(theConstruct)) != NULL) 
        { count++; }
        
      if (allModules) loopModule = (struct defmodule *) GetNextDefmodule(loopModule);
      else loopModule = NULL;
     }
   
   /*================================*/
   /* Create the multifield value to */
   /* store the construct names.     */
   /*================================*/
   
   SetpType(returnValue,MULTIFIELD);
   SetpDOBegin(returnValue,1);
   SetpDOEnd(returnValue,count);
   theList = (struct multifield *) CreateMultifield((int) count);
   SetpValue(returnValue,(VOID *) theList);
   
   /*===========================*/
   /* Store the construct names */
   /* in the multifield value.  */
   /*===========================*/
   
   loopModule = theModule;
   count = 1;
   while (loopModule != NULL)
     {
      /*============================*/
      /* Set the current module to  */
      /* the module being examined. */
      /*============================*/
      
      SetCurrentModule((VOID *) loopModule);
      
      /*===============================*/
      /* Add each construct name found */
      /* in the module to the list.    */
      /*===============================*/
      
      theConstruct = NULL;
      while ((theConstruct = (*constructClass->getNextItemFunction)(theConstruct)) != NULL) 
        {
         theName = (*constructClass->getConstructNameFunction)(theConstruct);
         SetMFType(theList,count,SYMBOL);
         if (allModules)
           {
            char buffer[512];
            
            strcpy(buffer,GetDefmoduleName(loopModule));
            strcat(buffer,"::");
            strcat(buffer,ValueToString(theName));
            SetMFValue(theList,count,AddSymbol(buffer));
           }
         else
           { SetMFValue(theList,count,AddSymbol(ValueToString(theName))); }
         count++;
        } 
 
      /*==================================*/
      /* Move on to the next module (if   */
      /* the list is to contain the names */
      /* of constructs from all modules). */
      /*==================================*/
      
      if (allModules) loopModule = (struct defmodule *) GetNextDefmodule(loopModule);
      else loopModule = NULL;
     }
   
   /*=============================*/
   /* Restore the current module. */
   /*=============================*/
   
   RestoreCurrentModule();
  }

/***********************************************/
/* ListConstructCommand: Generic CLIPS Routine */
/*   for listing the constructs in a module.   */
/***********************************************/
globle VOID ListConstructCommand(functionName,constructClass)
  char *functionName;
  struct construct *constructClass;
  {
   struct defmodule *theModule;
   DATA_OBJECT result;
   int numArgs;
   
   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/
   
   if ((numArgs = ArgCountCheck(functionName,NO_MORE_THAN,1)) == -1) return;

   /*====================================*/
   /* If an argument was given, check to */
   /* see that it's a valid module name. */
   /*====================================*/

   if (numArgs == 1)
     {
      /*======================================*/
      /* Only symbols are valid module names. */
      /*======================================*/

      RtnUnknown(1,&result);
      if (GetType(result) != SYMBOL)
        {
         ExpectedTypeError1(functionName,1,"defmodule name");
         return;
        }
        
      /*===========================================*/
      /* Verify that the named module exists or is */
      /* the symbol * (for obtaining the construct */
      /* list for all modules).                    */
      /*===========================================*/

      if ((theModule = (struct defmodule *) FindDefmodule(DOToString(result))) == NULL)
        {
         if (strcmp("*",DOToString(result)) != 0) 
           {
            ExpectedTypeError1(functionName,1,"defmodule name");
            return;
           }
           
         theModule = NULL;
        }
     }
   
   /*=====================================*/
   /* Otherwise use the current module to */
   /* generate the construct list.        */
   /*=====================================*/

   else
     { theModule = ((struct defmodule *) GetCurrentModule()); }
  
   /*=========================*/
   /* Call the driver routine */
   /* to list the constructs. */
   /*=========================*/
   
   ListConstruct(constructClass,WDISPLAY,theModule);
  }
  
/*****************************************/
/* ListConstruct: Generic C Routine for  */
/*   listing the constructs in a module. */
/*****************************************/
globle VOID ListConstruct(constructClass,logicalName,theModule)
  char *logicalName;
  struct construct *constructClass;
  struct defmodule *theModule;
  {
   VOID *constructPtr;
   SYMBOL_HN *constructName;
   long count = 0;
   int allModules = CLIPS_FALSE;
        
   /*==========================*/
   /* Save the current module. */
   /*==========================*/
   
   SaveCurrentModule();

   /*=======================================*/
   /* If the module specified is NULL, then */
   /* list all constructs in all modules.   */
   /*=======================================*/
  
   if (theModule == NULL)
     { 
      theModule = (struct defmodule *) GetNextDefmodule(NULL);
      allModules = CLIPS_TRUE;
     }
     
   /*==================================*/
   /* Loop through all of the modules. */
   /*==================================*/
   
   while (theModule != NULL)
     {
      /*========================================*/
      /* If we're printing the construct in all */
      /* modules, then preface each module      */
      /* listing with the name of the module.   */
      /*========================================*/
               
      if (allModules) 
        {
         PrintCLIPS(logicalName,GetDefmoduleName(theModule));
         PrintCLIPS(logicalName,":\n");
        }
        
      /*===============================*/
      /* Set the current module to the */
      /* module we're examining.       */
      /*===============================*/
      
      SetCurrentModule((VOID *) theModule);
      
      /*===========================================*/
      /* List all of the constructs in the module. */
      /*===========================================*/
      
      for (constructPtr = (*constructClass->getNextItemFunction)(NULL);
           constructPtr != NULL;
           constructPtr = (*constructClass->getNextItemFunction)(constructPtr))
        {
         if (HaltExecution == CLIPS_TRUE) return;
  
         constructName = (*constructClass->getConstructNameFunction)(constructPtr);
           
         if (constructName != NULL)
           {
            if (allModules) PrintCLIPS(WDISPLAY,"   ");
            PrintCLIPS(logicalName,ValueToString(constructName));
            PrintCLIPS(logicalName,"\n");
           }
            
         count++;
        }
        
      /*====================================*/
      /* Move on to the next module (if the */
      /* listing is to contain the names of */
      /* constructs from all modules).      */
      /*====================================*/
      
      if (allModules) theModule = (struct defmodule *) GetNextDefmodule(theModule);
      else theModule = NULL;
     }

   /*=================================================*/
   /* Print the tally and restore the current module. */
   /*=================================================*/
   
   PrintTally(WDISPLAY,count,constructClass->constructName,
                             constructClass->pluralName);
   
   RestoreCurrentModule();
  }
  
/**********************************************************/
/* SetNextConstruct: Sets the next field of one construct */
/*   to point to another construct of the same type.      */
/**********************************************************/
globle VOID SetNextConstruct(theConstruct,targetConstruct)
  struct constructHeader *theConstruct, *targetConstruct;
  { theConstruct->next = targetConstruct; }
  
/********************************************************************/
/* GetConstructModuleItem: Returns the construct module for a given */
/*   construct (note that this is a pointer to a data structure     */
/*   like the deffactsModule, not a pointer to an environment       */
/*   module which contains a number of types of constructs.         */
/********************************************************************/
globle struct defmoduleItemHeader *GetConstructModuleItem(theConstruct)
  struct constructHeader *theConstruct;
  { return(theConstruct->whichModule); }
  
/***************************************************************/
/* GetConstructPPForm: Returns the pretty print representation */
/*   for the specified construct.                              */
/***************************************************************/
globle char *GetConstructPPForm(theConstruct)
  struct constructHeader *theConstruct;
  { return(theConstruct->ppForm); }

/****************************************************/
/* GetNextConstructItem: Returns the next construct */
/*   items from a list of constructs.               */
/****************************************************/
globle struct constructHeader *GetNextConstructItem(theConstruct,moduleIndex)
  struct constructHeader *theConstruct;
  int moduleIndex;
  {   
   struct defmoduleItemHeader *theModuleItem;
   
   if (theConstruct == NULL)
     {
      theModuleItem = (struct defmoduleItemHeader *) 
                      GetModuleItem(NULL,moduleIndex);
      if (theModuleItem == NULL) return(NULL);
      return(theModuleItem->firstItem);
     }
   
   return(theConstruct->next);
  }

/*******************************************************/
/* GetConstructModuleItemByIndex: Returns a pointer to */
/*  the defmodule item for the specified construct. If */
/*  theModule is NULL, then the construct module item  */
/*  for the current module is returned, otherwise the  */
/*  construct module item for the specified construct  */
/*  is returned.                                       */
/*******************************************************/
globle struct defmoduleItemHeader *GetConstructModuleItemByIndex(theModule,moduleIndex)
  struct defmodule *theModule;
  int moduleIndex;
  { 
   if (theModule != NULL)
     { 
      return((struct defmoduleItemHeader *)
             GetModuleItem(theModule,moduleIndex));
     }
     
   return((struct defmoduleItemHeader *)
          GetModuleItem(((struct defmodule *) GetCurrentModule()),moduleIndex));
  }
  
/******************************************/
/* FreeConstructHeaderModule: Deallocates */
/*   the data structures associated with  */
/*   the construct module item header.    */ 
/******************************************/
globle VOID FreeConstructHeaderModule(theModuleItem,constructClass)
  struct defmoduleItemHeader *theModuleItem;
  struct construct *constructClass;
  {
   struct constructHeader *thisOne, *nextOne;
   
   thisOne = theModuleItem->firstItem;
   
   while (thisOne != NULL)
     { 
      nextOne = thisOne->next;
      (*constructClass->freeFunction)(thisOne); 
      thisOne = nextOne;
     }
  } 

/**********************************************/
/* DoForAllConstructs: Executes an action for */
/*   all constructs of a specified class.     */
/**********************************************/
globle long DoForAllConstructs(actionFunction,moduleItemIndex,interruptable,userBuffer)
#if ANSI_COMPILER
  VOID (*actionFunction)(struct constructHeader *,VOID *);
#else
  VOID (*actionFunction)();
#endif
  int moduleItemIndex;
  int interruptable;
  VOID *userBuffer;
  {
   struct constructHeader *theConstruct;   
   struct defmoduleItemHeader *theModuleItem;
   VOID *theModule;
   long moduleCount = 0L;
        
   /*==========================*/
   /* Save the current module. */
   /*==========================*/
   
   SaveCurrentModule();
   
   /*==================================*/
   /* Loop through all of the modules. */
   /*==================================*/
   
   for (theModule = GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = GetNextDefmodule(theModule), moduleCount++)
     {
      /*=============================*/
      /* Set the current module to   */
      /* the module we're examining. */
      /*=============================*/
      
      SetCurrentModule((VOID *) theModule);

      /*================================================*/
      /* Perform the action for each of the constructs. */
      /*================================================*/
      
      theModuleItem = (struct defmoduleItemHeader *)
                      GetModuleItem(theModule,moduleItemIndex);
   
      for (theConstruct = theModuleItem->firstItem;
           theConstruct != NULL;
           theConstruct = theConstruct->next)
        {
         if (interruptable)
           { 
            if (GetHaltExecution() == CLIPS_TRUE) 
              {
               RestoreCurrentModule();
               return(-1L); 
              }
           }
            
         (*actionFunction)(theConstruct,userBuffer);
        }
     }
     
   /*=============================*/
   /* Restore the current module. */
   /*=============================*/
   
   RestoreCurrentModule();
   
   /*=========================================*/
   /* Return the number of modules traversed. */
   /*=========================================*/
   
   return(moduleCount);
  }
  
/*****************************************************/
/* InitializeConstructHeader: Initializes construct  */
/*   header info, including to which module item the */
/*   new construct belongs                           */
/*****************************************************/
globle VOID InitializeConstructHeader(constructType,theConstruct,theConstructName)
  char *constructType;
  struct constructHeader *theConstruct;
  SYMBOL_HN *theConstructName;
  {
   struct moduleItem *theModuleItem;
   struct defmoduleItemHeader *theItemHeader;
   
   theModuleItem = FindModuleItem(constructType);
   theItemHeader = (struct defmoduleItemHeader *) 
                   GetModuleItem(NULL,theModuleItem->moduleIndex);
                   
   theConstruct->whichModule = theItemHeader;
   theConstruct->name = theConstructName;
   theConstruct->ppForm = NULL;
   theConstruct->bsaveID = 0L;
   theConstruct->next = NULL;
  }

/*************************************************/
/* SetConstructPPForm: Sets a construct's pretty */
/*   print form and deletes the old one.         */
/*************************************************/
globle VOID SetConstructPPForm(theConstruct,ppForm)
  struct constructHeader *theConstruct;
  char *ppForm;
  {
   if (theConstruct->ppForm != NULL)
     {
      rm((VOID *) theConstruct->ppForm,
         (int) ((strlen(theConstruct->ppForm) + 1) * sizeof(char)));
     }
   theConstruct->ppForm = ppForm;
  }

#if DEBUGGING_FUNCTIONS

/******************************************************/
/* ConstructPrintWatchAccess: Provides an interface   */
/*   to the list-watch-items function for a construct */
/******************************************************/
globle BOOLEAN ConstructPrintWatchAccess(constructClass,log,argExprs,
                                         getWatchFunc,setWatchFunc)
  struct construct *constructClass;
  char *log;
  EXPRESSION *argExprs;
#if ANSI_COMPILER
  BOOLEAN (*getWatchFunc)(VOID *);
  VOID (*setWatchFunc)(BOOLEAN,VOID *);
#else
  BOOLEAN (*getWatchFunc)();
  VOID (*setWatchFunc)();
#endif
  {
   return(ConstructWatchSupport(constructClass,"list-watch-items",log,argExprs,
                                CLIPS_FALSE,CLIPS_FALSE,getWatchFunc,setWatchFunc));
  }

/**************************************************/
/* ConstructSetWatchAccess: Provides an interface */
/*   to the watch function for a construct        */
/**************************************************/
globle BOOLEAN ConstructSetWatchAccess(constructClass,newState,argExprs,
                                       getWatchFunc,setWatchFunc)
  struct construct *constructClass;
  BOOLEAN newState;
  EXPRESSION *argExprs;
#if ANSI_COMPILER
  BOOLEAN (*getWatchFunc)(VOID *);
  VOID (*setWatchFunc)(BOOLEAN,VOID *);
#else
  BOOLEAN (*getWatchFunc)();
  VOID (*setWatchFunc)();
#endif
  {
   return(ConstructWatchSupport(constructClass,"watch",WERROR,argExprs,
                                CLIPS_TRUE,newState,getWatchFunc,setWatchFunc));
  }

/******************************************************/
/* ConstructWatchSupport: Generic construct interface */
/*   into watch and list-watch-items.                 */
/******************************************************/
static BOOLEAN ConstructWatchSupport(constructClass,funcName,log,argExprs,
                                     setFlag,newState,getWatchFunc,setWatchFunc)
  struct construct *constructClass;
  char *funcName,*log;
  EXPRESSION *argExprs;
  BOOLEAN setFlag,newState;
#if ANSI_COMPILER
  BOOLEAN (*getWatchFunc)(VOID *);
  VOID (*setWatchFunc)(BOOLEAN,VOID *);
#else
  BOOLEAN (*getWatchFunc)();
  VOID (*setWatchFunc)();
#endif
  {
   struct defmodule *theModule;
   VOID *theConstruct;
   DATA_OBJECT constructName;
   int argIndex = 2;
   
   /*========================================*/
   /* If no constructs are specified, then   */
   /* show/set the trace for all constructs. */
   /*========================================*/
   
   if (argExprs == NULL)
     {
      /*==========================*/
      /* Save the current module. */
      /*==========================*/
      
      SaveCurrentModule();
      
      /*===========================*/
      /* Loop through each module. */
      /*===========================*/
      
      for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
           theModule != NULL;
           theModule = (struct defmodule *) GetNextDefmodule((VOID *) theModule))
        {
         /*============================*/
         /* Set the current module to  */
         /* the module being examined. */
         /*============================*/
         
         SetCurrentModule((VOID *) theModule);
         
         /*====================================================*/
         /* If we're displaying the names of constructs with   */
         /* watch flags enabled, then preface each module      */
         /* listing of constructs with the name of the module. */
         /*====================================================*/
         
         if (setFlag == CLIPS_FALSE)
           {
            PrintCLIPS(log,GetDefmoduleName((VOID *) theModule));
            PrintCLIPS(log,":\n");
           }
           
         /*============================================*/
         /* Loop through each construct in the module. */
         /*============================================*/
         
         for (theConstruct = (*constructClass->getNextItemFunction)(NULL);
              theConstruct != NULL;
              theConstruct = (*constructClass->getNextItemFunction)(theConstruct))
           {
            /*=============================================*/
            /* Either set the watch flag for the construct */
            /* or display its current state.               */
            /*=============================================*/
            
            if (setFlag)
              { (*setWatchFunc)(newState,theConstruct); }
            else
              {
               PrintCLIPS(log,"   ");
               ConstructPrintWatch(log,constructClass,theConstruct,getWatchFunc);
              }
           }
        }
        
      /*=============================*/
      /* Restore the current module. */
      /*=============================*/
      
      RestoreCurrentModule();
      
      /*====================================*/
      /* Return TRUE to indicate successful */
      /* completion of the command.         */
      /*====================================*/
      
      return(CLIPS_TRUE);
     }
     
   /*==================================================*/
   /* Show/set the trace for each specified construct. */
   /*==================================================*/
   
   while (argExprs != NULL)
     {
      /*==========================================*/
      /* Evaluate the argument that should be a   */
      /* construct name. Return FALSE is an error */
      /* occurs when evaluating the argument.     */
      /*==========================================*/
      
      if (EvaluateExpression(argExprs,&constructName))
        { return(CLIPS_FALSE); }
      
      /*================================================*/
      /* Check to see that it's a valid construct name. */
      /*================================================*/
      
      if ((constructName.type != SYMBOL) ? CLIPS_TRUE :
          ((theConstruct = LookupConstruct(constructClass,
                                           DOToString(constructName),CLIPS_TRUE)) == NULL))
        {
         ExpectedTypeError1(funcName,argIndex,constructClass->constructName);
         return(CLIPS_FALSE);
        }
       
      /*=============================================*/
      /* Either set the watch flag for the construct */
      /* or display its current state.               */
      /*=============================================*/

      if (setFlag)
        { (*setWatchFunc)(newState,theConstruct); }
      else
        { ConstructPrintWatch(log,constructClass,theConstruct,getWatchFunc); }
        
      /*===============================*/
      /* Move on to the next argument. */
      /*===============================*/
      
      argIndex++;
      argExprs = GetNextArgument(argExprs);
     }
      
   /*====================================*/
   /* Return TRUE to indicate successful */
   /* completion of the command.         */
   /*====================================*/
     
   return(CLIPS_TRUE);
  }
  
/*************************************************/
/* ConstructPrintWatch: Displays the trace value */
/*   of a construct for list-watch-items         */
/*************************************************/
static VOID ConstructPrintWatch(log,constructClass,theConstruct,getWatchFunc)
  char *log;
  struct construct *constructClass;
  VOID *theConstruct;
#if ANSI_COMPILER
  BOOLEAN (*getWatchFunc)(VOID *);
#else
  BOOLEAN (*getWatchFunc)();
#endif
  {
   PrintCLIPS(log,ValueToString((*constructClass->getConstructNameFunction)(theConstruct)));
   PrintCLIPS(log,(*getWatchFunc)(theConstruct) ? " = on\n" : " = off\n");
  }
   
#endif /* DEBUGGING_FUNCTIONS */

/*****************************************************/
/* LookupConstruct: Finds a construct in the current */
/*   or imported modules. If specified, will also    */
/*   look for construct in a non-imported module.    */
/*****************************************************/
globle VOID *LookupConstruct(constructClass,constructName,moduleNameAllowed)
  struct construct *constructClass;
  char *constructName;
  BOOLEAN moduleNameAllowed;
  {
   VOID *theConstruct;
   char *constructType;
   int moduleCount;
   
   /*============================================*/
   /* Look for the specified construct in the    */
   /* current module or in any imported modules. */
   /*============================================*/
   
   constructType = constructClass->constructName;
   theConstruct = FindImportedConstruct(constructType,NULL,constructName,
                                        &moduleCount,CLIPS_TRUE,NULL);

   /*===========================================*/
   /* Return NULL if the reference is ambiguous */
   /* (it was found in more than one module).   */
   /*===========================================*/
   
   if (theConstruct != NULL)
     {
      if (moduleCount > 1)
        {
         AmbiguousReferenceErrorMessage(constructType,constructName);
         return(NULL);
        }
      return(theConstruct);
     }
     
   /*=============================================*/
   /* If specified, check to see if the construct */
   /* is in a non-imported module.                */
   /*=============================================*/
   
   if (moduleNameAllowed && FindModuleSeparator(constructName))
     { theConstruct = (*constructClass->findFunction)(constructName); }
     
   /*====================================*/
   /* Return a pointer to the construct. */
   /*====================================*/
   
   return(theConstruct);
  }

