   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  10/29/93            */
   /*                                                     */
   /*              DEFGLOBAL PARSER MODULE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: Parses the defglobal construct.                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#define _GLOBLPSR_SOURCE_

#include "setup.h"

#if DEFGLOBAL_CONSTRUCT

#include <string.h>

#include "pprint.h"
#include "router.h"
#include "clipsmem.h"
#include "scanner.h"
#include "evaluatn.h"
#include "exprnpsr.h"
#include "constrct.h"
#include "multifld.h"
#include "watch.h"
#include "modulutl.h"
#include "modulpsr.h"
#include "cstrcpsr.h"
#include "globldef.h"
#include "globlbsc.h"

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
#include "bload.h"
#endif

#include "globlpsr.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if (! RUN_TIME) && (! BLOAD_ONLY)
#if ANSI_COMPILER
   static BOOLEAN                 GetVariableDefinition(char *,int *,int,struct token *);
   static VOID                    AddDefglobal(SYMBOL_HN *,DATA_OBJECT_PTR,struct expr *);
#else
   static BOOLEAN                 GetVariableDefinition();
   static VOID                    AddDefglobal();
#endif
#endif

/*********************************************************************/
/* ParseDefglobal: Coordinates all actions necessary for the parsing */
/*   and creation of a defglobal into the current environment.       */
/*********************************************************************/
globle BOOLEAN ParseDefglobal(readSource)
  char *readSource;
  {
   int defglobalError = CLIPS_FALSE;
#if (MAC_MPW || MAC_MCW) && (RUN_TIME || BLOAD_ONLY)  /* added 03-07-96 */
#pragma unused(readSource)
#endif
   
#if (! RUN_TIME) && (! BLOAD_ONLY)

   struct token theToken;
   int tokenRead = CLIPS_TRUE;
   struct defmodule *theModule;

   /*=====================================*/
   /* Pretty print buffer initialization. */
   /*=====================================*/
   
   SetPPBufferStatus(ON);
   FlushPPBuffer();
   SetIndentDepth(3);
   SavePPBuffer("(defglobal ");

   /*=================================================*/
   /* Individual defglobal constructs can't be parsed */
   /* while a binary load is in effect.               */
   /*=================================================*/
   
#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
   if (Bloaded() == CLIPS_TRUE)
     {
      CannotLoadWithBloadMessage("defglobal");
      return(CLIPS_TRUE);
     }
#endif

   /*===========================*/
   /* Look for the module name. */
   /*===========================*/
   
   GetToken(readSource,&theToken);
   if (theToken.type == SYMBOL)
     {
      /*=================================================*/
      /* The optional module name can't contain a module */
      /* separator like other constructs. For example,   */
      /* (defrule X::foo is OK for rules, but the right  */
      /* syntax for defglobals is (defglobal X ?*foo*.   */
      /*=================================================*/
      
      tokenRead = CLIPS_FALSE;
      if (FindModuleSeparator(ValueToString(theToken.value)))
        {
         SyntaxErrorMessage("defglobal");
         return(CLIPS_TRUE);
        }      
      
      /*=================================*/
      /* Determine if the module exists. */
      /*=================================*/
      
      theModule = (struct defmodule *) FindDefmodule(ValueToString(theToken.value));
      if (theModule == NULL)
        {
         CantFindItemErrorMessage("defmodule",ValueToString(theToken.value));
         return(CLIPS_TRUE);
        }
        
      /*=========================================*/
      /* If the module name was OK, then set the */
      /* current module to the specified module. */
      /*=========================================*/
      
      SavePPBuffer(" ");
      SetCurrentModule((VOID *) theModule);
     }
     
   /*===========================================*/
   /* If the module name wasn't specified, then */
   /* use the current module's name in the      */
   /* defglobal's pretty print representation.  */
   /*===========================================*/
   
   else
     { 
      PPBackup();
      SavePPBuffer(GetDefmoduleName(((struct defmodule *) GetCurrentModule())));
      SavePPBuffer(" ");
      SavePPBuffer(theToken.printForm);
     }

   /*======================*/
   /* Parse the variables. */
   /*======================*/

   while (GetVariableDefinition(readSource,&defglobalError,tokenRead,&theToken))
     { 
      tokenRead = CLIPS_FALSE;
      
      FlushPPBuffer();
      SavePPBuffer("(defglobal ");
      SavePPBuffer(GetDefmoduleName(((struct defmodule *) GetCurrentModule())));
      SavePPBuffer(" ");
     }

#endif

   /*==================================*/
   /* Return the parsing error status. */
   /*==================================*/
   
   return(defglobalError);
  }

#if (! RUN_TIME) && (! BLOAD_ONLY)

/***************************************************************/
/* GetVariableDefinition: Parses and evaluates a single global */
/*   variable in a defglobal construct. Returns TRUE if the    */
/*   variable was successfully parsed and FALSE if a right     */
/*   parenthesis is encountered (signifying the end of the     */
/*   defglobal construct) or an error occurs. The error status */
/*   flag is also set if an error occurs.                      */
/***************************************************************/
static BOOLEAN GetVariableDefinition(readSource,defglobalError,tokenRead,theToken)
  char *readSource;
  int *defglobalError;
  int tokenRead;
  struct token *theToken;
  {
   SYMBOL_HN *variableName;
   struct expr *assignPtr;
   DATA_OBJECT assignValue;

   /*========================================*/
   /* Get next token, which should either be */
   /* a closing parenthesis or a variable.   */
   /*========================================*/

   if (! tokenRead) GetToken(readSource,theToken);
   if (theToken->type == RPAREN) return(CLIPS_FALSE);

   if (theToken->type == SF_VARIABLE)
     {
      SyntaxErrorMessage("defglobal");
      *defglobalError = CLIPS_TRUE;
      return(CLIPS_FALSE);
     }
   else if (theToken->type != GBL_VARIABLE)
     {
      SyntaxErrorMessage("defglobal");
      *defglobalError = CLIPS_TRUE;
      return(CLIPS_FALSE);
     }

   variableName = (SYMBOL_HN *) theToken->value;

   SavePPBuffer(" ");

   /*================================*/
   /* Print out compilation message. */
   /*================================*/

#if DEBUGGING_FUNCTIONS
   if ((GetWatchItem("compilations") == ON) && GetPrintWhileLoading())
     {
      if (QFindDefglobal(variableName) != NULL) PrintCLIPS(WDIALOG,"Redefining defglobal: ?");
      else PrintCLIPS(WDIALOG,"Defining defglobal: ");
      PrintCLIPS(WDIALOG,ValueToString(variableName));
      PrintCLIPS(WDIALOG,"\n");
     }
   else 
#endif
     { if (GetPrintWhileLoading()) PrintCLIPS(WDIALOG,":"); }
     
   /*==================================================================*/
   /* Check for import/export conflicts from the construct definition. */
   /*==================================================================*/
     
#if DEFMODULE_CONSTRUCT
   if (FindImportExportConflict("defglobal",((struct defmodule *) GetCurrentModule()),ValueToString(variableName)))
     {
      ImportExportConflictMessage("defglobal",ValueToString(variableName),NULL,NULL);
      *defglobalError = CLIPS_TRUE;
      return(CLIPS_FALSE);
     }
#endif

   /*==============================*/
   /* The next token must be an =. */
   /*==============================*/

   GetToken(readSource,theToken);
   if (strcmp(theToken->printForm,"=") != 0)
     {
      SyntaxErrorMessage("defglobal");
      *defglobalError = CLIPS_TRUE;
      return(CLIPS_FALSE);
     }

   SavePPBuffer(" ");

   /*======================================================*/
   /* Parse the expression to be assigned to the variable. */
   /*======================================================*/

   assignPtr = ParseAtomOrExpression(readSource,NULL);
   if (assignPtr == NULL)
     {
      *defglobalError = CLIPS_TRUE;
      return(CLIPS_FALSE);
     }

   /*==========================*/
   /* Evaluate the expression. */
   /*==========================*/

   SetEvaluationError(CLIPS_FALSE);
   if (EvaluateExpression(assignPtr,&assignValue))
     {
      ReturnExpression(assignPtr);
      *defglobalError = CLIPS_TRUE;
      return(CLIPS_FALSE);
     }

   SavePPBuffer(")");
   
   /*======================================*/
   /* Add the variable to the global list. */
   /*======================================*/

   AddDefglobal(variableName,&assignValue,assignPtr);
   
   /*==================================================*/
   /* Return TRUE to indicate that the global variable */
   /* definition was successfully parsed.              */
   /*==================================================*/
   
   return(CLIPS_TRUE);
  }

/*********************************************************/
/* AddDefglobal: Adds a defglobal to the current module. */
/*********************************************************/
static VOID AddDefglobal(name,vPtr,ePtr)
  SYMBOL_HN *name;
  DATA_OBJECT_PTR vPtr;
  struct expr *ePtr;
  {
   struct defglobal *defglobalPtr;
   BOOLEAN new = CLIPS_FALSE;
#if DEBUGGING_FUNCTIONS
   int GlobalHadWatch = CLIPS_FALSE;
#endif

   /*========================================================*/
   /* If the defglobal is already defined, then use the old  */
   /* data structure and substitute new values. If it hasn't */
   /* been defined, then create a new data structure.        */
   /*========================================================*/

   defglobalPtr = QFindDefglobal(name);
   if (defglobalPtr == NULL)
     {
      new = CLIPS_TRUE;
      defglobalPtr = get_struct(defglobal);
     }
   else
     {
      DeinstallConstructHeader(&defglobalPtr->header);
#if DEBUGGING_FUNCTIONS
      GlobalHadWatch = defglobalPtr->watch;
#endif
     }

   /*===========================================*/
   /* Remove the old values from the defglobal. */
   /*===========================================*/

   if (new == CLIPS_FALSE)
     {
      ValueDeinstall(&defglobalPtr->current);
      if (defglobalPtr->current.type == MULTIFIELD)
        { ReturnMultifield(defglobalPtr->current.value); }
      
      RemoveHashedExpression(defglobalPtr->initial);
     }

   /*=======================================*/
   /* Copy the new values to the defglobal. */
   /*=======================================*/

   defglobalPtr->current.type = vPtr->type;
   if (vPtr->type != MULTIFIELD) defglobalPtr->current.value = vPtr->value;
   else DuplicateMultifield(&defglobalPtr->current,vPtr);
   ValueInstall(&defglobalPtr->current);

   defglobalPtr->initial = AddHashedExpression(ePtr);
   ReturnExpression(ePtr);
   ChangeToGlobals = CLIPS_TRUE;

   /*=================================*/
   /* Restore the old watch value to  */
   /* the defglobal if redefined.     */
   /*=================================*/
   
#if DEBUGGING_FUNCTIONS
   defglobalPtr->watch = GlobalHadWatch ? CLIPS_TRUE : WatchGlobals;
#endif

   /*======================================*/
   /* Save the name and pretty print form. */
   /*======================================*/
   
   defglobalPtr->header.name = name;
   IncrementSymbolCount(name);

   SavePPBuffer("\n");
   if (GetConserveMemory() == CLIPS_TRUE)
     { defglobalPtr->header.ppForm = NULL; }
   else
     { defglobalPtr->header.ppForm = CopyPPBuffer(); }
     
   defglobalPtr->inScope = CLIPS_TRUE;

   /*=============================================*/
   /* If the defglobal was redefined, we're done. */
   /*=============================================*/

   if (new == CLIPS_FALSE) return;

   /*===================================*/
   /* Copy the defglobal variable name. */
   /*===================================*/

   defglobalPtr->busyCount = 0;
   defglobalPtr->header.whichModule = (struct defmoduleItemHeader *)
                               GetModuleItem(NULL,FindModuleItem("defglobal")->moduleIndex);

   /*=============================================*/
   /* Add the defglobal to the list of defglobals */
   /* for the current module.                     */
   /*=============================================*/
      
   AddConstructToModule(&defglobalPtr->header);
  }
  
/*****************************************************************/
/* ReplaceGlobalVariable: Replaces a global variable found in an */
/*   expression with the appropriate primitive data type which   */
/*   can later be used to retrieve the global variable's value.  */
/*****************************************************************/
globle BOOLEAN ReplaceGlobalVariable(ePtr)
  struct expr *ePtr;
  {
   struct defglobal *theGlobal;
   int count;
   
   /*=================================*/
   /* Search for the global variable. */
   /*=================================*/
   
   theGlobal = (struct defglobal *) 
               FindImportedConstruct("defglobal",NULL,ValueToString(ePtr->value),
                                     &count,CLIPS_TRUE,NULL);
               
   /*=============================================*/
   /* If it wasn't found, print an error message. */
   /*=============================================*/
   
   if (theGlobal == NULL)
     {
      GlobalReferenceErrorMessage(ValueToString(ePtr->value));
      return(CLIPS_FALSE);
     }
     
   /*========================================================*/
   /* The current implementation of the defmodules shouldn't */
   /* allow a construct to be defined which would cause an   */
   /* ambiguous reference, but we'll check for it anyway.    */
   /*========================================================*/
   
   if (count > 1) 
     {
      AmbiguousReferenceErrorMessage("defglobal",ValueToString(ePtr->value));
      return(CLIPS_FALSE);
     }

   /*==============================================*/
   /* Replace the symbolic reference of the global */
   /* variable with a direct pointer reference.    */
   /*==============================================*/
   
   ePtr->type = DEFGLOBAL_PTR;
   ePtr->value = (VOID *) theGlobal;

   return(CLIPS_TRUE);
  }

/*****************************************************************/
/* GlobalReferenceErrorMessage: Prints an error message when a   */
/*   symbolic reference to a global variable cannot be resolved. */
/*****************************************************************/
globle VOID GlobalReferenceErrorMessage(variableName)
  char *variableName;
  {
   PrintErrorID("GLOBLPSR",1,CLIPS_TRUE);
   PrintCLIPS(WERROR,"\nGlobal variable ?*");
   PrintCLIPS(WERROR,variableName);
   PrintCLIPS(WERROR,"* was referenced, but is not defined.\n");
  }
  
#endif /* (! RUN_TIME) && (! BLOAD_ONLY) */

#endif /* DEFGLOBAL_CONSTRUCT */



