   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  02/24/94            */
   /*                                                     */
   /*                FACT COMMANDS MODULE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides the facts, assert, retract, save-facts, */
/*   load-facts, set-fact-duplication, get-fact-duplication, */
/*   assert-string, and fact-index commands and functions.   */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Bob Orchard (NRCC - Nat'l Research Council of Canada)*/
/*                  (Fuzzy reasoning extensions)             */
/*                  (certainty factors for facts and rules)  */
/*                  (extensions to run command)              */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#include <stdio.h>
#define _CLIPS_STDIO_
#include <string.h>

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT

#define _FACTCOM_SOURCE_

#include "clipsmem.h"
#include "exprnpsr.h"
#include "factmngr.h"
#include "argacces.h"
#include "match.h"
#include "router.h"
#include "scanner.h"
#include "constant.h"
#include "factrhs.h"
#include "factmch.h"
#include "extnfunc.h"
#include "tmpltpsr.h"
#include "tmpltutl.h"  /* added 03-06-96 */
#include "facthsh.h"
#include "modulutl.h"

#include "tmpltdef.h"
#include "tmpltfun.h"

#if CERTAINTY_FACTORS   /* added 03-06-96 */
#include "cfdef.h"
#endif

#if FUZZY_DEFTEMPLATES  /* added 03-06-96 */
#include "fuzzyutl.h"
#endif

#if BLOAD_AND_BSAVE || BLOAD || BLOAD_ONLY
#include "bload.h"
#endif

#include "factcom.h"

#define INVALID     -2L
#define UNSPECIFIED -1L

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if ANSI_COMPILER
#if (! RUN_TIME)
   static struct expr            *AssertParse(struct expr *,char *);
#endif
#if DEBUGGING_FUNCTIONS
   static long int                GetFactsArgument(int,int);
#endif
   static struct expr            *StandardLoadFact(char *,struct token *);
   static DATA_OBJECT_PTR         GetSaveFactsDeftemplateNames(struct expr *,int,int *,int *);
#else
#if (! RUN_TIME)
   static struct expr            *AssertParse();
#endif
#if DEBUGGING_FUNCTIONS
   static long int                GetFactsArgument();
#endif
   static struct expr            *StandardLoadFact();
   static DATA_OBJECT_PTR         GetSaveFactsDeftemplateNames();
#endif

/***************************************/
/* FactCommandDefinitions: Initializes */
/*   fact commands and functions.      */
/***************************************/
globle VOID FactCommandDefinitions()
  {
#if ! RUN_TIME
#if DEBUGGING_FUNCTIONS
   DefineFunction2("facts", 'v', PTIF FactsCommand,        "FactsCommand", "*4iu");
#endif

   DefineFunction("assert", 'u', PTIF AssertCommand,  "AssertCommand");
   DefineFunction2("retract", 'v', PTIF RetractCommand, "RetractCommand","1*z");
   DefineFunction2("assert-string", 'u', PTIF AssertStringFunction,   "AssertStringFunction", "11s");
   DefineFunction2("str-assert", 'u', PTIF AssertStringFunction,   "AssertStringFunction", "11s");

   DefineFunction2("get-fact-duplication",'b',
                   GetFactDuplicationCommand,"GetFactDuplicationCommand", "00");
   DefineFunction2("set-fact-duplication",'b',
                   SetFactDuplicationCommand,"SetFactDuplicationCommand", "11");

   DefineFunction2("save-facts", 'b', PTIF SaveFactsCommand, "SaveFactsCommand", "1*wk");
   DefineFunction2("load-facts", 'b', PTIF LoadFactsCommand, "LoadFactsCommand", "11k");
   DefineFunction2("fact-index", 'l', PTIF FactIndexFunction,"FactIndexFunction", "11y");
#endif

#if (! RUN_TIME)
   AddFunctionParser("assert",AssertParse);
   FuncSeqOvlFlags("assert",CLIPS_FALSE,CLIPS_FALSE);
#endif
  }

/***************************************/
/* AssertCommand: CLIPS access routine */
/*   for the assert function.          */
/***************************************/
globle VOID AssertCommand(rv)
  DATA_OBJECT_PTR rv;
  {
   struct deftemplate *theDeftemplate;
   struct field *theField;
   DATA_OBJECT theValue;
   struct expr *theExpression;
   struct templateSlot *slotPtr;
   struct fact *newFact;
   int error = CLIPS_FALSE;
   int i;
   struct fact *theFact;

#if FUZZY_DEFTEMPLATES
   char str[128];
#endif

   /*===================================================*/
   /* Set the default return value to the symbol FALSE. */
   /*===================================================*/

   SetpType(rv,SYMBOL);
   SetpValue(rv,CLIPSFalseSymbol);

   /*================================*/
   /* Get the deftemplate associated */
   /* with the fact being asserted.  */
   /*================================*/

   theExpression = GetFirstArgument();
   theDeftemplate = (struct deftemplate *) theExpression->value;
      
   /*=======================================*/
   /* Create the fact and store the name of */
   /* the deftemplate as the 1st field.     */
   /*=======================================*/
   
   if (theDeftemplate->implied == CLIPS_FALSE) 
     { 
      newFact = CreateFactBySize((int) theDeftemplate->numberOfSlots);
      slotPtr = theDeftemplate->slotList;
     }
   else 
     {
      newFact = CreateFactBySize(1);
      if (theExpression->nextArg == NULL)  
        { 
         newFact->theProposition.theFields[0].type = MULTIFIELD;
         newFact->theProposition.theFields[0].value = CreateMultifield2(0L);
        }
      slotPtr = NULL;
     }
   
   newFact->whichDeftemplate = theDeftemplate;
   
   /*===================================================*/
   /* Evaluate the expression associated with each slot */
   /* and store the result in the appropriate slot of   */
   /* the newly created fact.                           */
   /*===================================================*/
   
   theField = newFact->theProposition.theFields;

#if CERTAINTY_FACTORS   /* added 03-06-96 */
   /* get the CF from the argList of the DEFTEMPLATE_PTR expr struct currently
      pointed at be theExpression

      NOTE WELL -- cannot just return the fact structure and return on error...
                   MUST let theFields get set up correctly with value and type
                   or the ReturnFact may potentially try to ReturnMultiField
                   erroneously !!
   */
   if (theExpression->argList == NULL)
      newFact->factCF = 1.0;
   else
      {
        EvaluateExpression(theExpression->argList,&theValue);
        if (theValue.type != FLOAT && theValue.type != INTEGER)
          {
            cfNonNumberError();
            error = CLIPS_TRUE;
          }
        else
          {
            newFact->factCF = (theValue.type == FLOAT) ? 
		                  ValueToDouble(theValue.value) :
		                  (double)ValueToLong(theValue.value);
								 
            if (newFact->factCF < 0.0 || newFact->factCF > 1.0)
              {
                cfRangeError();
                error = CLIPS_TRUE;
              }
           }
       }
#endif
   
   for (theExpression = theExpression->nextArg, i = 0;
        theExpression != NULL;
        theExpression = theExpression->nextArg, i++)
     {
      /*===================================================*/
      /* Evaluate the expression to be stored in the slot. */
      /*===================================================*/
      
      EvaluateExpression(theExpression,&theValue);   
      
#if FUZZY_DEFTEMPLATES
      /* 
         If the deftemplate associated with the fuzzy value to be
         asserted in the slot does NOT match the deftemplate associated
         the slot --- this is an error

         Fuzzy Deftemplates - have a value in the fuzzyTemplate field
                            - have a slotlist BUT the slot has NO constraints
         Fuzzy Slots in a non Fuzzy Deftemplate
                            - have constraints in the slot 

         A fuzzy value can ONLY be put into a fuzzy slot (fuzzy deftemplate or
         fuzzy slot of a standard deftemplate)

         The code below could stand some simplification!!

         NOTE: cannot ReturnFact until the Fields have been set up!!

      */
      if (slotPtr != NULL) /* implied facts have a null slot list */
       {
        if (theValue.type != FUZZY_VALUE)
          {
            if (theDeftemplate->fuzzyTemplate != NULL ||
                (slotPtr->constraints != NULL &&
                 slotPtr->constraints->fuzzyValuesAllowed == CLIPS_TRUE
                )
               )
              {
               sprintf(str,"**** Error in asserting fact %s, slot %s\n", 
                           ValueToString(theDeftemplate->header.name),
                           ValueToString(slotPtr->slotName));
               PrintCLIPS(WERROR,str);
               PrintCLIPS(WERROR,"Fuzzy Slot must have a Fuzzy Value\n");
                          theValue.type = SYMBOL;
                          theValue.value = CLIPSFalseSymbol;
               error = CLIPS_TRUE;
              }
          }
        else /* type is FUZZY_VALUE */
          {

           if (theValue.value == NULL)
            {
             /* do this because the getConstant function that evaluates fuzzy
                values may get out of range values for the fuzzy sets and it is 
                necessary to NOT assert the fact with a NULL fuzzy value.
             */
             theValue.type = SYMBOL;
             theValue.value = CLIPSFalseSymbol;
             error = CLIPS_TRUE;
            }
           else 
            { /* is the fuzzy value of the right type? */
              struct deftemplate *fvDeftemplate;
              struct deftemplate *slotDeftemplate;

              if (theDeftemplate->fuzzyTemplate == NULL &&
                 slotPtr->constraints->fuzzyValuesAllowed == CLIPS_FALSE)
               {
                 sprintf(str,"**** Error in asserting fact %s, slot %s\n", 
                         ValueToString(theDeftemplate->header.name),
                         ValueToString(slotPtr->slotName));
                 PrintCLIPS(WERROR,str);
                 PrintCLIPS(WERROR,"Fuzzy Value can only be inserted in a Fuzzy Slot\n");
                 theValue.type = SYMBOL;
                 theValue.value = CLIPSFalseSymbol;
	         error = CLIPS_TRUE;
               }
              else
               {
                 fvDeftemplate = (ValueToFuzzyValue(theValue.value))->whichDeftemplate;
                 /* the slot for a fuzzy deftemplate will not have any
                    entry in the restrictionList -- it is not necessary to
                    check that the slot's associated deftemplate is the one
                    for the fuzzy value since it can only be itself
                 */

                 if (theDeftemplate->fuzzyTemplate == NULL &&
                     slotPtr->constraints->fuzzyValueRestriction)
                    slotDeftemplate = (struct deftemplate *)
                                       slotPtr->constraints->restrictionList->value;
                 else
                    /* a fuzzy deftemplate */
                    slotDeftemplate = theDeftemplate;
                  
                 if (slotDeftemplate != fvDeftemplate)
                  {
                   sprintf(str,"**** Error in asserting fact %s, slot %s\n", 
                           ValueToString(theDeftemplate->header.name),
                           ValueToString(slotPtr->slotName));
                   PrintCLIPS(WERROR,str);
                   PrintCLIPS(WERROR,"Fuzzy Value must match type required in fuzzy slot\n");
                   theValue.type = SYMBOL;
                   theValue.value = CLIPSFalseSymbol;
                   error = CLIPS_TRUE;
                  }
               }
             
            }
          }
       }  /* end of   if (slotPtr != NULL)   */

#endif
      /*============================================================*/
      /* A multifield value can't be stored in a single field slot. */
      /*============================================================*/
      
      if ((slotPtr != NULL) ?
          (slotPtr->multislot == CLIPS_FALSE) && (theValue.type == MULTIFIELD) :
          CLIPS_FALSE)
        {
         MultiIntoSingleFieldSlotError(slotPtr,theDeftemplate);
         theValue.type = SYMBOL;
         theValue.value = CLIPSFalseSymbol;
         error = CLIPS_TRUE;
        }
        
      /*==============================*/
      /* Store the value in the slot. */
      /*==============================*/
      
      theField[i].type = (short) theValue.type;
      theField[i].value = theValue.value;
      
      /*========================================*/
      /* Get the information for the next slot. */
      /*========================================*/
      
      if (slotPtr != NULL) slotPtr = slotPtr->next;
     }

   /*============================================*/
   /* If an error occured while generating the   */
   /* fact's slot values, then abort the assert. */
   /*============================================*/
            
   if (error) 
     {
      ReturnFact(newFact);
      return;
     }
     
   /*================================*/
   /* Add the fact to the fact-list. */
   /*================================*/

   theFact = (struct fact *) Assert((VOID *) newFact);

   /*========================================*/
   /* The asserted fact is the return value. */
   /*========================================*/

   if (theFact != NULL)
     {
      SetpType(rv,FACT_ADDRESS);
      SetpValue(rv,(VOID *) theFact);
     }
     
   return;
  }
  
/****************************************/
/* RetractCommand: CLIPS access routine */
/*   for the retract command.           */
/****************************************/
globle VOID RetractCommand()
  {
   long int factIndex;
   struct fact *ptr;
   struct expr *theArgument;
   DATA_OBJECT theResult;
   int argNumber;

   /*================================*/
   /* Iterate through each argument. */
   /*================================*/
   
   for (theArgument = GetFirstArgument(), argNumber = 1;
        theArgument != NULL;
        theArgument = GetNextArgument(theArgument), argNumber++)
     {
      /*========================*/
      /* Evaluate the argument. */
      /*========================*/
      
      EvaluateExpression(theArgument,&theResult);
      
      /*===============================================*/
      /* If the argument evaluates to an integer, then */
      /* it's assumed to be the fact index of the fact */
      /* to be retracted.                              */
      /*===============================================*/
      
      if (theResult.type == INTEGER)
        {
         /*==========================================*/
         /* A fact index must be a positive integer. */
         /*==========================================*/
         
         factIndex = ValueToLong(theResult.value);
         if (factIndex < 0)
           {            
            ExpectedTypeError1("retract",argNumber,"fact-address, fact-index, or the symbol *");
            return;
           }
         
         /*================================================*/
         /* See if a fact with the specified index exists. */
         /*================================================*/
         
         ptr = FindIndexedFact(factIndex);
         
         /*=====================================*/
         /* If the fact exists then retract it, */
         /* otherwise print an error message.   */
         /*=====================================*/
         
         if (ptr != NULL)
           { Retract((VOID *) ptr); }
         else
           {
            char tempBuffer[20];
            sprintf(tempBuffer,"f-%ld",factIndex);
            CantFindItemErrorMessage("fact",tempBuffer);
           }
        }
        
      /*===============================================*/
      /* Otherwise if the argument evaluates to a fact */
      /* address, we can directly retract it.          */
      /*===============================================*/
      
      else if (theResult.type == FACT_ADDRESS)
        { Retract(theResult.value); }
        
      /*============================================*/
      /* Otherwise if the argument evaluates to the */
      /* symbol *, then all facts are retracted.    */
      /*============================================*/
      
      else if ((theResult.type == SYMBOL) ?
               (strcmp(ValueToString(theResult.value),"*") == 0) : CLIPS_FALSE)
        {
         RemoveAllFacts();
         return;
        }
        
      /*============================================*/
      /* Otherwise the argument has evaluated to an */
      /* illegal value for the retract command.     */
      /*============================================*/
      
      else
        {
         ExpectedTypeError1("retract",argNumber,"fact-address, fact-index, or the symbol *");
         SetEvaluationError(TRUE);
        }
     }
  }

/***************************************************/
/* SetFactDuplicationCommand: CLIPS access routine */
/*   for the set-fact-duplication command.         */
/***************************************************/
globle int SetFactDuplicationCommand()
  {
   int oldValue;
   DATA_OBJECT theValue;

   /*=====================================================*/
   /* Get the old value of the fact duplication behavior. */
   /*=====================================================*/
   
   oldValue = GetFactDuplication();

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/
   
   if (ArgCountCheck("set-fact-duplication",EXACTLY,1) == -1)
     { return(oldValue); }

   /*========================*/
   /* Evaluate the argument. */
   /*========================*/
   
   RtnUnknown(1,&theValue);

   /*===============================================================*/
   /* If the argument evaluated to FALSE, then the fact duplication */
   /* behavior is disabled, otherwise it is enabled.                */
   /*===============================================================*/
   
   if ((theValue.value == CLIPSFalseSymbol) && (theValue.type == SYMBOL))
     { SetFactDuplication(CLIPS_FALSE); }
   else
     { SetFactDuplication(CLIPS_TRUE); }

   /*========================================================*/
   /* Return the old value of the fact duplication behavior. */
   /*========================================================*/
   
   return(oldValue);
  }

/***************************************************/
/* GetFactDuplicationCommand: CLIPS access routine */
/*   for the get-fact-duplication command.         */
/***************************************************/
globle int GetFactDuplicationCommand()
  {
   int currentValue;

   /*=========================================================*/
   /* Get the current value of the fact duplication behavior. */
   /*=========================================================*/
   
   currentValue = GetFactDuplication();

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/   

   if (ArgCountCheck("get-fact-duplication",EXACTLY,0) == -1)
     { return(currentValue); }
     
   /*============================================================*/
   /* Return the current value of the fact duplication behavior. */
   /*============================================================*/

   return(currentValue);
  }

/*******************************************/
/* FactIndexFunction: CLIPS access routine */
/*   for the fact-index function.          */
/*******************************************/
globle long int FactIndexFunction()
  {
   DATA_OBJECT item;

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/
   
   if (ArgCountCheck("fact-index",EXACTLY,1) == -1) return(-1L);

   /*========================*/
   /* Evaluate the argument. */
   /*========================*/
   
   RtnUnknown(1,&item);

   /*======================================*/
   /* The argument must be a fact address. */
   /*======================================*/
   
   if (GetType(item) != FACT_ADDRESS)
     {
      ExpectedTypeError1("fact-index",1,"fact-address");
      return(-1L);
     }
     
   /*================================================*/
   /* Return the fact index associated with the fact */
   /* address. If the fact has been retracted, then  */
   /* return -1 for the fact index.                  */
   /*================================================*/

   if (((struct fact *) GetValue(item))->garbage) return(-1L);
   
   return (FactIndex(GetValue(item))); 
  }

#if DEBUGGING_FUNCTIONS

/**************************************/
/* FactsCommand: CLIPS access routine */
/*   for the facts command.           */
/**************************************/
globle VOID FactsCommand()
  {
   int argumentCount;
   long int start = UNSPECIFIED, end = UNSPECIFIED, max = UNSPECIFIED;
   struct defmodule *theModule;
   DATA_OBJECT theValue;
   int argOffset;

   /*=========================================================*/
   /* Determine the number of arguments to the facts command. */
   /*=========================================================*/
   
   if ((argumentCount = ArgCountCheck("facts",NO_MORE_THAN,4)) == -1) return;
   
   /*==================================*/
   /* The default module for the facts */
   /* command is the current module.   */
   /*==================================*/
   
   theModule = ((struct defmodule *) GetCurrentModule());
   
   /*==========================================*/
   /* If no arguments were specified, then use */
   /* the default values to list the facts.    */
   /*==========================================*/
   
   if (argumentCount == 0)
     {
      Facts(WDISPLAY,theModule,(long) start,(long) end,(long) max);
      return;
     }
     
   /*========================================================*/
   /* Since there are one or more arguments, see if a module */
   /* or start index was specified as the first argument.    */
   /*========================================================*/
   
   RtnUnknown(1,&theValue);
   
   /*===============================================*/
   /* If the first argument is a symbol, then check */
   /* to see that a valid module was specified.     */
   /*===============================================*/
   
   if (theValue.type == SYMBOL)
     {
      theModule = (struct defmodule *) FindDefmodule(ValueToString(theValue.value));
      if ((theModule == NULL) && (strcmp(ValueToString(theValue.value),"*") != 0))
        {
         SetEvaluationError(CLIPS_TRUE);
         CantFindItemErrorMessage("defmodule",ValueToString(theValue.value));
         return;
        }
        
      if ((start = GetFactsArgument(2,argumentCount)) == INVALID) return;
        
      argOffset = 1;
     }
   
   /*================================================*/
   /* Otherwise if the first argument is an integer, */
   /* check to see that a valid index was specified. */
   /*================================================*/
   
   else if (theValue.type == INTEGER)
     {   
      start = DOToLong(theValue);
      if (start < 0)
        {
         ExpectedTypeError1("facts",1,"symbol or positive number");
         SetHaltExecution(CLIPS_TRUE);
         SetEvaluationError(CLIPS_TRUE);
         return;
        }
      argOffset = 0;
     }
   
   /*==========================================*/
   /* Otherwise the first argument is invalid. */
   /*==========================================*/
   
   else
     {  
      ExpectedTypeError1("facts",1,"symbol or positive number");
      SetHaltExecution(CLIPS_TRUE);
      SetEvaluationError(CLIPS_TRUE);
      return;
     }

   /*==========================*/
   /* Get the other arguments. */
   /*==========================*/
   
   if ((end = GetFactsArgument(2 + argOffset,argumentCount)) == INVALID) return;
   if ((max = GetFactsArgument(3 + argOffset,argumentCount)) == INVALID) return;

   /*=================*/
   /* List the facts. */
   /*=================*/
   
   Facts(WDISPLAY,theModule,(long) start,(long) end,(long) max);
  }

/**************************************************/
/* Facts: C access routine for the facts command. */
/**************************************************/
globle VOID Facts(logicalName,vTheModule,start,end,max)
  char *logicalName;
  VOID *vTheModule;
  long start, end, max;
  {
   struct fact *factPtr;
   long count = 0;
   struct defmodule *oldModule, *theModule = (struct defmodule *) vTheModule;
   int allModules = CLIPS_FALSE;

   /*==========================*/
   /* Save the current module. */
   /*==========================*/
   
   oldModule = ((struct defmodule *) GetCurrentModule());
   
   /*=========================================================*/
   /* Determine if facts from all modules are to be displayed */
   /* or just facts from the current module.                  */
   /*=========================================================*/
   
   if (theModule == NULL) allModules = CLIPS_TRUE;
   else SetCurrentModule((VOID *) theModule);
   
   /*=====================================*/
   /* Get the first fact to be displayed. */
   /*=====================================*/
   
   if (allModules) factPtr = (struct fact *) GetNextFact(NULL);
   else factPtr = (struct fact *) GetNextFactInScope(NULL);
   
   /*===============================*/
   /* Display facts until there are */
   /* no more facts to display.     */
   /*===============================*/
   
   while (factPtr != NULL)
     {
      /*==================================================*/
      /* Abort the display of facts if the Halt Execution */
      /* flag has been set (normally by user action).     */
      /*==================================================*/
      
      if (GetHaltExecution() == CLIPS_TRUE) 
        {
         SetCurrentModule((VOID *) oldModule);
         return;
        }
        
      /*===============================================*/
      /* If the maximum fact index of facts to display */
      /* has been reached, then stop displaying facts. */
      /*===============================================*/
      
      if ((factPtr->factIndex > end) && (end != UNSPECIFIED))
        {
         PrintTally(logicalName,count,"fact","facts");
         SetCurrentModule((VOID *) oldModule);
         return;
        }

      /*================================================*/
      /* If the maximum number of facts to be displayed */
      /* has been reached, then stop displaying facts.  */
      /*================================================*/
      
      if (max == 0)
        {
         PrintTally(logicalName,count,"fact","facts");
         SetCurrentModule((VOID *) oldModule);
         return;
        }

      /*======================================================*/
      /* If the index of the fact is greater than the minimum */
      /* starting fact index, then display the fact.          */
      /*======================================================*/
      
      if (factPtr->factIndex >= start)
        {
         PrintFactWithIdentifier(WDISPLAY,factPtr);
         PrintCLIPS(logicalName,"\n");
         count++;
         if (max > 0) max--;
        }

      /*========================================*/
      /* Proceed to the next fact to be listed. */
      /*========================================*/
      
      if (allModules) factPtr = (struct fact *) GetNextFact(factPtr);
      else factPtr = (struct fact *) GetNextFactInScope(factPtr);
     }

   /*===================================================*/
   /* Print the total of the number of facts displayed. */
   /*===================================================*/
   
   PrintTally(logicalName,count,"fact","facts");
   
   /*=============================*/
   /* Restore the current module. */
   /*=============================*/
   
   SetCurrentModule((VOID *) oldModule);
  }
  
/****************************************************************/
/* GetFactsArgument: Returns an argument for the facts command. */
/*  A return value of -1 indicates that no value was specified. */
/*  A return value of -2 indicates that the value specified is  */
/*  invalid.                                                    */
/****************************************************************/
static long int GetFactsArgument(whichOne,argumentCount)
  int whichOne, argumentCount;
  {
   long int factIndex;
   DATA_OBJECT theValue;

   if (whichOne > argumentCount) return(UNSPECIFIED);

   if (ArgTypeCheck("facts",whichOne,INTEGER,&theValue) == CLIPS_FALSE) return(INVALID);
   
   factIndex = DOToLong(theValue);
   
   if (factIndex < 0)
     {
      ExpectedTypeError1("facts",whichOne,"positive number");
      SetHaltExecution(CLIPS_TRUE);
      SetEvaluationError(CLIPS_TRUE);
      return(INVALID);
     }
     
   return(factIndex);
  }

#endif /* DEBUGGING_FUNCTIONS */

/**********************************************/
/* AssertStringFunction: CLIPS access routine */
/*   for the assert-string function.          */
/**********************************************/
globle VOID AssertStringFunction(returnValue)
  DATA_OBJECT_PTR returnValue;
  {
   DATA_OBJECT argPtr;
   struct fact *theFact;
   
   /*===================================================*/
   /* Set the default return value to the symbol FALSE. */
   /*===================================================*/

   SetpType(returnValue,SYMBOL);
   SetpValue(returnValue,CLIPSFalseSymbol);

   /*=====================================================*/
   /* Check for the correct number and type of arguments. */
   /*=====================================================*/
   
   if (ArgCountCheck("assert-string",EXACTLY,1) == -1) return;

   if (ArgTypeCheck("assert-string",1,STRING,&argPtr) == CLIPS_FALSE)
     { return; }

   /*==========================================*/
   /* Call the driver routine for converting a */
   /* string to a fact and then assert it.     */
   /*==========================================*/
   
   theFact = (struct fact *) AssertString(DOToString(argPtr));
   if (theFact != NULL)
     {
      SetpType(returnValue,FACT_ADDRESS);
      SetpValue(returnValue,(VOID *) theFact);
     }
     
   return;
  }

/******************************************/
/* SaveFactsCommand: CLIPS access routine */
/*   for the save-facts command.          */
/******************************************/
globle int SaveFactsCommand()
  {
   char *fileName;
   int numArgs, saveCode = LOCAL_SAVE;
   char *argument;
   DATA_OBJECT theValue;
   struct expr *theList = NULL;

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/
   
   if ((numArgs = ArgCountCheck("save-facts",AT_LEAST,1)) == -1) return(CLIPS_FALSE);
   
   /*=================================================*/
   /* Get the file name to which facts will be saved. */
   /*=================================================*/
   
   if ((fileName = GetFileName("save-facts",1)) == NULL) return(CLIPS_FALSE);
   
   /*=============================================================*/
   /* If specified, the second argument to save-facts indicates   */
   /* whether just facts local to the current module or all facts */
   /* visible to the current module will be saved.                */
   /*=============================================================*/
   
   if (numArgs > 1)
     {
      if (ArgTypeCheck("save-facts",2,SYMBOL,&theValue) == CLIPS_FALSE) return(CLIPS_FALSE);

      argument = DOToString(theValue);

      if (strcmp(argument,"local") == 0)
        { saveCode = LOCAL_SAVE; }
      else if (strcmp(argument,"visible") == 0)
        { saveCode = VISIBLE_SAVE; }
      else
        {
         ExpectedTypeError1("save-facts",2,"symbol with value local or visible");
         return(CLIPS_FALSE);
        }
     }
   
   /*======================================================*/
   /* Subsequent arguments indicate that only those facts  */
   /* associated with the specified deftemplates should be */
   /* saved to the file.                                   */
   /*======================================================*/
   
   if (numArgs > 2) theList = GetFirstArgument()->nextArg->nextArg;

   /*====================================*/
   /* Call the SaveFacts driver routine. */
   /*====================================*/
   
   if (SaveFacts(fileName,saveCode,theList) == CLIPS_FALSE)
     { return(CLIPS_FALSE); }

   return(CLIPS_TRUE);
  }

/******************************************/
/* LoadFactsCommand: CLIPS access routine */
/*   for the load-facts command.          */
/******************************************/
globle int LoadFactsCommand()
  {
   char *fileName;

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/
   
   if (ArgCountCheck("load-facts",EXACTLY,1) == -1) return(CLIPS_FALSE);

   /*====================================================*/
   /* Get the file name from which facts will be loaded. */
   /*====================================================*/

   if ((fileName = GetFileName("load-facts",1)) == NULL) return(CLIPS_FALSE);

   /*====================================*/
   /* Call the LoadFacts driver routine. */
   /*====================================*/
   
   if (LoadFacts(fileName) == CLIPS_FALSE) return(CLIPS_FALSE);

   return(CLIPS_TRUE);
  }

/***********************************************************/
/* SaveFacts: C access routine for the save-facts command. */
/***********************************************************/
globle BOOLEAN SaveFacts(fileName,saveCode,theList)
  char *fileName;
  int saveCode;
  struct expr *theList;
  {
   int tempValue1, tempValue2, tempValue3;
#if FUZZY_DEFTEMPLATES /* addeed 03-06-96 */
   int tempValue4;
#endif
   struct fact *theFact; /* changed 03-06-96 */
   FILE *filePtr;
   struct defmodule *theModule;
   DATA_OBJECT_PTR theDOArray;
   int count, i, printFact, error;

   /*======================================================*/
   /* Open the file. Use either "fast save" or I/O Router. */
   /*======================================================*/

   if ((filePtr = fopen(fileName,"w")) == NULL)
     {
      OpenErrorMessage("save-facts",fileName);
      return(CLIPS_FALSE);
     }

   SetFastSave(filePtr);

   /*===========================================*/
   /* Set the print flags so that addresses and */
   /* strings are printed properly to the file. */
   /*===========================================*/

   tempValue1 = PreserveEscapedCharacters;
   PreserveEscapedCharacters = CLIPS_TRUE;
   tempValue2 = AddressesToStrings;
   AddressesToStrings = CLIPS_TRUE;
   tempValue3 = InstanceAddressesToNames;
   InstanceAddressesToNames = CLIPS_TRUE;

#if FUZZY_DEFTEMPLATES   /* added 03-06-96 */ 
   tempValue4 = saveFactsInProgress;
   saveFactsInProgress = CLIPS_TRUE;
#endif

   /*===================================================*/
   /* Determine the list of specific facts to be saved. */
   /*===================================================*/
   
   theDOArray = GetSaveFactsDeftemplateNames(theList,saveCode,&count,&error);
   
   if (error)
     {
      PreserveEscapedCharacters = tempValue1;
      AddressesToStrings = tempValue2;
      InstanceAddressesToNames = tempValue3;
#if FUZZY_DEFTEMPLATES   /* added 03-06-96 */
      saveFactsInProgress = tempValue4;
#endif
      fclose(filePtr);
      SetFastSave(NULL);
      return(CLIPS_FALSE);
     }
     
   /*=================*/
   /* Save the facts. */
   /*=================*/
      
   theModule = ((struct defmodule *) GetCurrentModule());
   
   for (theFact = (struct fact *) GetNextFactInScope(NULL);
        theFact != NULL;
        theFact = (struct fact *) GetNextFactInScope(theFact))
     {
      /*===========================================================*/
      /* If we're doing a local save and the facts's corresponding */
      /* deftemplate isn't in the current module, then don't save  */
      /* the fact.                                                 */
      /*===========================================================*/
      
      if ((saveCode == LOCAL_SAVE) &&
          (theFact->whichDeftemplate->header.whichModule->theModule != theModule))
        { printFact = CLIPS_FALSE; }
        
      /*=====================================================*/
      /* Otherwise, if the list of facts to be printed isn't */
      /* restricted, then set the print flag to TRUE.        */
      /*=====================================================*/
      
      else if (theList == NULL)
        { printFact = CLIPS_TRUE; }
        
      /*=======================================================*/
      /* Otherwise see if the fact's corresponding deftemplate */
      /* is in the list of deftemplates whose facts are to be  */
      /* saved. If it's in the list, then set the print flag   */
      /* to TRUE, otherwise set it to FALSE.                   */
      /*=======================================================*/
      
      else 
        {
         printFact = CLIPS_FALSE;
         for (i = 0; i < count; i++)
           {
            if (theDOArray[i].value == (VOID *) theFact->whichDeftemplate)
              {
               printFact = CLIPS_TRUE;
               break;
              }
           }
        }
         
      /*===================================*/
      /* If the print flag is set to TRUE, */
      /* then save the fact to the file.   */
      /*===================================*/
      
      if (printFact)
        { 
         PrintFact((char *) filePtr,theFact);
         PrintCLIPS((char *) filePtr,"\n");
        }
     }
     
   /*==========================*/
   /* Restore the print flags. */
   /*==========================*/
   
   PreserveEscapedCharacters = tempValue1;
   AddressesToStrings = tempValue2;
   InstanceAddressesToNames = tempValue3;
#if FUZZY_DEFTEMPLATES
   saveFactsInProgress = tempValue4;
#endif
   
   /*=================*/
   /* Close the file. */
   /*=================*/

   fclose(filePtr);
   SetFastSave(NULL);
   
   /*==================================*/
   /* Free the deftemplate name array. */
   /*==================================*/
   
   if (theList != NULL) rm3(theDOArray,(long) sizeof(DATA_OBJECT) * count);

   /*===================================*/
   /* Return TRUE to indicate no errors */
   /* occurred while saving the facts.  */
   /*===================================*/
   
   return(CLIPS_TRUE);
  }
  
/*******************************************************************/
/* GetSaveFactsDeftemplateNames: Retrieves the list of deftemplate */
/*   names for saving specific facts with the save-facts command.  */
/*******************************************************************/
static DATA_OBJECT_PTR GetSaveFactsDeftemplateNames(theList,saveCode,count,error)
  struct expr *theList;
  int saveCode;
  int *count;
  int *error;
  {
   struct expr *tempList;
   DATA_OBJECT_PTR theDOArray;
   int i, tempCount;
   struct deftemplate *theDeftemplate;
   
   /*=============================*/
   /* Initialize the error state. */
   /*=============================*/
   
   *error = CLIPS_FALSE;
   
   /*=====================================================*/
   /* If no deftemplate names were specified as arguments */
   /* then the deftemplate name list is empty.            */
   /*=====================================================*/
   
   if (theList == NULL) 
     {
      *count = 0;
      return(NULL);
     }
   
   /*======================================*/
   /* Determine the number of deftemplate  */
   /* names to be stored in the name list. */
   /*======================================*/
               
   for (tempList = theList, *count = 0;
        tempList != NULL;
        tempList = tempList->nextArg, (*count)++)
     { /* Do Nothing */ }
        
   /*=========================================*/
   /* Allocate the storage for the name list. */
   /*=========================================*/
   
   theDOArray = (DATA_OBJECT_PTR) gm3((long) sizeof(DATA_OBJECT) * *count);
   
   /*=====================================*/
   /* Loop through each of the arguments. */
   /*=====================================*/
   
   for (tempList = theList, i = 0; 
        i < *count; 
        tempList = tempList->nextArg, i++)
     {
      /*========================*/
      /* Evaluate the argument. */
      /*========================*/
      
      EvaluateExpression(tempList,&theDOArray[i]);
      
      if (EvaluationError)
        {
         *error = CLIPS_TRUE;
         rm3(theDOArray,(long) sizeof(DATA_OBJECT) * *count);
         return(NULL);
        }
     
      /*======================================*/
      /* A deftemplate name must be a symbol. */
      /*======================================*/
      
      if (theDOArray[i].type != SYMBOL)
        { 
         *error = CLIPS_TRUE;
         ExpectedTypeError1("save-facts",3+i,"symbol");
         rm3(theDOArray,(long) sizeof(DATA_OBJECT) * *count);
         return(NULL);
        }
         
      /*===================================================*/
      /* Find the deftemplate. For a local save, look only */
      /* in the current module. For a visible save, look   */
      /* in all visible modules.                           */
      /*===================================================*/
      
      if (saveCode == LOCAL_SAVE)
        {
         theDeftemplate = (struct deftemplate *)
                         FindDeftemplate(ValueToString(theDOArray[i].value));
         if (theDeftemplate == NULL)
           {
            *error = CLIPS_TRUE;
            ExpectedTypeError1("save-facts",3+i,"local deftemplate name");
            rm3(theDOArray,(long) sizeof(DATA_OBJECT) * *count);
            return(NULL);
           }
        }
      else if (saveCode == VISIBLE_SAVE)
        {
         theDeftemplate = (struct deftemplate *)
           FindImportedConstruct("deftemplate",NULL,
                                 ValueToString(theDOArray[i].value),
                                 &tempCount,CLIPS_TRUE,NULL);
         if (theDeftemplate == NULL)
           {
            *error = CLIPS_TRUE;
            ExpectedTypeError1("save-facts",3+i,"visible deftemplate name");
            rm3(theDOArray,(long) sizeof(DATA_OBJECT) * *count);
            return(NULL);
           }
        }
        
      /*==================================*/
      /* Add a pointer to the deftemplate */
      /* to the array being created.      */
      /*==================================*/
               
      theDOArray[i].type = DEFTEMPLATE_PTR;
      theDOArray[i].value = (VOID *) theDeftemplate;
     }
     
   /*===================================*/
   /* Return the array of deftemplates. */
   /*===================================*/
   
   return(theDOArray);
  }
  
/***********************************************************/
/* LoadFacts: C access routine for the load-facts command. */
/***********************************************************/
globle BOOLEAN LoadFacts(fileName)
  char *fileName;
  {
   FILE *filePtr;
   struct token theToken;
   struct expr *testPtr;
   DATA_OBJECT rv;

   /*======================================================*/
   /* Open the file. Use either "fast save" or I/O Router. */
   /*======================================================*/

   if ((filePtr = fopen(fileName,"r")) == NULL)
     {
      OpenErrorMessage("load-facts",fileName);
      return(CLIPS_FALSE);
     }

   SetFastLoad(filePtr);

   /*=================*/
   /* Load the facts. */
   /*=================*/

   theToken.type = LPAREN;
   while (theToken.type != STOP)
     {
      testPtr = StandardLoadFact((char *) filePtr,&theToken);
      if (testPtr == NULL) theToken.type = STOP;
      else EvaluateExpression(testPtr,&rv);
      ReturnExpression(testPtr);
     }

   /*=================*/
   /* Close the file. */
   /*=================*/

   SetFastLoad(NULL);
   fclose(filePtr);

#if CERTAINTY_FACTORS   /* added 03-06-96 */
   /* GetRHSPattern called in StandardLoadFact may have left a token 
      in the lookahead Token (theUnToken)  -- see GetRHSPattern and
	  Scanner.c
   */
   ClearTheUnToken();
#endif

   /*================================================*/
   /* Return TRUE if no error occurred while loading */
   /* the facts, otherwise return FALSE.             */
   /*================================================*/
   
   if (EvaluationError) return(CLIPS_FALSE);
   return(CLIPS_TRUE);
  }

/**************************************************************************/
/* StandardLoadFact: Loads a single fact from the specified logical name. */
/**************************************************************************/
static struct expr *StandardLoadFact(logicalName,theToken)
  char *logicalName;
  struct token *theToken;
  {
   int error = CLIPS_FALSE;
   struct expr *temp;

   GetToken(logicalName,theToken);
   if (theToken->type != LPAREN) return(NULL);

   temp = GenConstant(FCALL,FindFunction("assert"));
   temp->argList = GetRHSPattern(logicalName,theToken,&error,
                                  CLIPS_TRUE,CLIPS_FALSE,CLIPS_TRUE,RPAREN);

   if (error == CLIPS_TRUE)
     {
      PrintCLIPS(WERROR,"Function load-facts encountered an error\n");
      SetEvaluationError(CLIPS_TRUE);
      ReturnExpression(temp);
      return(NULL);
     }

   if (ExpressionContainsVariables(temp,CLIPS_TRUE))
     {
      ReturnExpression(temp);
      return(NULL);
     }

   return(temp);
  }

#if (! RUN_TIME)

/****************************************************************/
/* AssertParse: Driver routine for parsing the assert function. */
/****************************************************************/
static struct expr *AssertParse(top,logicalName)
  struct expr *top;
  char *logicalName;
  {
   int error;
   struct expr *rv;
   struct token theToken;

   ReturnExpression(top);
   SavePPBuffer(" ");
   IncrementIndentDepth(8);
   rv = BuildRHSAssert(logicalName,&theToken,&error,CLIPS_TRUE,CLIPS_TRUE,"assert command");
   DecrementIndentDepth(8);
   return(rv);
  }

#endif /* (! RUN_TIME) */

#endif /* DEFTEMPLATE_CONSTRUCT */


