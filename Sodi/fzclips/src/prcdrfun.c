   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  05/12/93            */
   /*                                                     */
   /*             PROCEDURAL FUNCTIONS MODULE             */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for several procedural         */
/*   functions including if, while, loop-for-count, bind,    */
/*   progn, return, break, and switch                        */
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

#define _PRCDRFUN_SOURCE_

#include <stdio.h>
#define _CLIPS_STDIO_

#include "setup.h"

#include "clipsmem.h"
#include "router.h"
#include "argacces.h"
#include "constrnt.h"
#include "cstrnchk.h"
#include "cstrnops.h"
#include "multifld.h"
#include "exprnpsr.h"
#include "scanner.h"
#include "utility.h"
#include "prcdrpsr.h"
#include "prcdrfun.h"

#if DEFGLOBAL_CONSTRUCT 
#include "globldef.h" 
#endif

typedef struct loopCounterStack
  {
   long loopCounter;
   struct loopCounterStack *nxt;
  } LOOP_COUNTER_STACK;
  
/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   globle int                 ReturnFlag = CLIPS_FALSE;
   globle int                 BreakFlag = CLIPS_FALSE;

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static LOOP_COUNTER_STACK *LoopCounterStack = NULL;
   static struct dataObject  *BindList = NULL;

#if ! RUN_TIME
/**********************************************/
/* ProceduralFunctionDefinitions: Initializes */
/*   the procedural functions.                */
/**********************************************/
globle VOID ProceduralFunctionDefinitions()
  {
   DefineFunction2("if", 'u', PTIF IfFunction, "IfFunction", NULL);
   DefineFunction2("while", 'u', PTIF WhileFunction, "WhileFunction", NULL);
   DefineFunction2("loop-for-count",'u', PTIF LoopForCountFunction, "LoopForCountFunction", NULL);
   DefineFunction2("(get-loop-count)",'l', PTIF GetLoopCount, "GetLoopCount", NULL);
   DefineFunction2("bind", 'u', PTIF BindFunction, "BindFunction", NULL);
   DefineFunction2("progn", 'u', PTIF PrognFunction, "PrognFunction", NULL);
   DefineFunction2("return", 'u', PTIF ReturnFunction, "ReturnFunction",NULL);
   DefineFunction2("break", 'v', PTIF BreakFunction, "BreakFunction",NULL);
   DefineFunction2("switch", 'u', PTIF SwitchFunction, "SwitchFunction",NULL);

   ProceduralFunctionParsers();

   FuncSeqOvlFlags("progn",CLIPS_FALSE,CLIPS_FALSE);
   FuncSeqOvlFlags("if",CLIPS_FALSE,CLIPS_FALSE);
   FuncSeqOvlFlags("while",CLIPS_FALSE,CLIPS_FALSE);
   FuncSeqOvlFlags("loop-for-count",CLIPS_FALSE,CLIPS_FALSE);
   FuncSeqOvlFlags("return",CLIPS_FALSE,CLIPS_FALSE);
   FuncSeqOvlFlags("switch",CLIPS_FALSE,CLIPS_FALSE);
  }
#endif

/***************************************/
/* WhileFunction: CLIPS access routine */
/*   for the while function.           */
/***************************************/
globle VOID WhileFunction(returnValue)
  DATA_OBJECT_PTR returnValue;  /* changed 03-11-96 */
  {
   DATA_OBJECT theResult;       /* changed 03-11-96 */

   /*====================================================*/
   /* Evaluate the body of the while loop as long as the */
   /* while condition evaluates to a non-FALSE value.    */
   /*====================================================*/
   
   CurrentEvaluationDepth++;
   RtnUnknown(1,&theResult);
   while (((theResult.value != CLIPSFalseSymbol) ||
           (theResult.type != SYMBOL)) &&
           (HaltExecution != CLIPS_TRUE))
     {
      if ((BreakFlag == CLIPS_TRUE) || (ReturnFlag == CLIPS_TRUE))
        break;
      RtnUnknown(2,&theResult);
      CurrentEvaluationDepth--;
      PeriodicCleanup(CLIPS_FALSE,CLIPS_TRUE);
      CurrentEvaluationDepth++;
      if ((BreakFlag == CLIPS_TRUE) || (ReturnFlag == CLIPS_TRUE))
        break;
      RtnUnknown(1,&theResult);
     }
   CurrentEvaluationDepth--;

   /*=====================================================*/
   /* Reset the break flag. The return flag is not reset  */
   /* because the while loop is probably contained within */
   /* a deffunction or RHS of a rule which needs to be    */
   /* returned from as well.                              */
   /*=====================================================*/
   
   BreakFlag = CLIPS_FALSE;
   
   /*====================================================*/
   /* If the return command was issued, then return that */
   /* value, otherwise return the symbol FALSE.          */
   /*====================================================*/
   
   if (ReturnFlag == CLIPS_TRUE)
     {
      returnValue->type = theResult.type;
      returnValue->value = theResult.value;
      returnValue->begin = theResult.begin;
      returnValue->end = theResult.end;
     }
   else
     {
      returnValue->type = SYMBOL;
      returnValue->value = CLIPSFalseSymbol;
     }
  }
  
/**********************************************/
/* LoopForCountFunction: CLIPS access routine */
/*   for the loop-for-count function.         */
/**********************************************/
globle VOID LoopForCountFunction(loopResult)
  DATA_OBJECT_PTR loopResult;
  {
   DATA_OBJECT arg_ptr;
   long iterationEnd;
   LOOP_COUNTER_STACK *tmpCounter;

   tmpCounter = get_struct(loopCounterStack);
   tmpCounter->loopCounter = 0L;
   tmpCounter->nxt = LoopCounterStack;
   LoopCounterStack = tmpCounter;
   if (ArgTypeCheck("loop-for-count",1,INTEGER,&arg_ptr) == CLIPS_FALSE)
     {
      loopResult->type = SYMBOL;
      loopResult->value = CLIPSFalseSymbol;
      LoopCounterStack = tmpCounter->nxt;
      rtn_struct(loopCounterStack,tmpCounter);
      return;
     }
   tmpCounter->loopCounter = DOToLong(arg_ptr);
   if (ArgTypeCheck("loop-for-count",2,INTEGER,&arg_ptr) == CLIPS_FALSE)
     {
      loopResult->type = SYMBOL;
      loopResult->value = CLIPSFalseSymbol;
      LoopCounterStack = tmpCounter->nxt;
      rtn_struct(loopCounterStack,tmpCounter);
      return;
     }
   iterationEnd = DOToLong(arg_ptr);
   while ((tmpCounter->loopCounter <= iterationEnd) &&
          (HaltExecution != CLIPS_TRUE))
     {
      if ((BreakFlag == CLIPS_TRUE) || (ReturnFlag == CLIPS_TRUE))
        break;
      CurrentEvaluationDepth++;
      RtnUnknown(3,&arg_ptr);
      CurrentEvaluationDepth--;
      PeriodicCleanup(CLIPS_FALSE,CLIPS_TRUE);
      if ((BreakFlag == CLIPS_TRUE) || (ReturnFlag == CLIPS_TRUE))
        break;
      tmpCounter->loopCounter++;
     }

   BreakFlag = CLIPS_FALSE;
   if (ReturnFlag == CLIPS_TRUE)
     {
      loopResult->type = arg_ptr.type;
      loopResult->value = arg_ptr.value;
      loopResult->begin = arg_ptr.begin;
      loopResult->end = arg_ptr.end; 
     }
   else
     {
      loopResult->type = SYMBOL;
      loopResult->value = CLIPSFalseSymbol;
     }
   LoopCounterStack = tmpCounter->nxt;
   rtn_struct(loopCounterStack,tmpCounter);
  }

/************************************************/
/* GetLoopCount                                 */
/************************************************/
globle long GetLoopCount()
  {
   int depth;
   LOOP_COUNTER_STACK *tmpCounter;
   
   depth = ValueToInteger(GetFirstArgument()->value);
   tmpCounter = LoopCounterStack;
   while (depth > 0)
     {
      tmpCounter = tmpCounter->nxt;
      depth--;
     }
   return(tmpCounter->loopCounter);
  }

/************************************/
/* IfFunction: CLIPS access routine */
/*   for the if function.           */
/************************************/
globle VOID IfFunction(returnValue)
  DATA_OBJECT_PTR returnValue;
  {
   int numArgs;    /* changed 03-11-96 */

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/
   
   if ((numArgs = ArgRangeCheck("if",2,3)) == -1)
     {
      returnValue->type = SYMBOL;
      returnValue->value = CLIPSFalseSymbol;
      return;
     }

   /*=========================*/
   /* Evaluate the condition. */
   /*=========================*/
   
   RtnUnknown(1,returnValue);
   if ((BreakFlag == CLIPS_TRUE) || (ReturnFlag == CLIPS_TRUE))
     { return; }

   /*=========================================*/
   /* If the condition evaluated to FALSE and */
   /* an "else" portion exists, evaluate it   */
   /* and return the value.                   */
   /*=========================================*/

   if ((returnValue->value == CLIPSFalseSymbol) &&
       (returnValue->type == SYMBOL) &&
       (numArgs == 3))
     {
      RtnUnknown(3,returnValue);
      return;
     }
   
   /*===================================================*/
   /* Otherwise if the symbol evaluated to a non-FALSE  */
   /* value, evaluate the "then" portion and return it. */
   /*===================================================*/
   
   else if ((returnValue->value != CLIPSFalseSymbol) ||
            (returnValue->type != SYMBOL))
     {
      RtnUnknown(2,returnValue);
      return;
     }

   /*=========================================*/
   /* Return FALSE if the condition evaluated */
   /* to FALSE and there is no "else" portion */
   /* of the if statement.                    */
   /*=========================================*/
   
   returnValue->type = SYMBOL;
   returnValue->value = CLIPSFalseSymbol;
   return;
  }

/**************************************/
/* BindFunction: CLIPS access routine */
/*   for the bind function.           */
/**************************************/
globle VOID BindFunction(returnValue)
  DATA_OBJECT_PTR returnValue;
  {
   DATA_OBJECT *theBind, *lastBind;
   int found = CLIPS_FALSE,
       unbindVar = CLIPS_FALSE;
   SYMBOL_HN *variableName = NULL;
#if DEFGLOBAL_CONSTRUCT
   struct defglobal *theGlobal = NULL;
#endif
   
   /*===============================================*/
   /* Determine the name of the variable to be set. */
   /*===============================================*/
   
#if DEFGLOBAL_CONSTRUCT
   if (GetFirstArgument()->type == DEFGLOBAL_PTR)
     { theGlobal = (struct defglobal *) GetFirstArgument()->value; }
   else 
#endif
     {
      EvaluateExpression(GetFirstArgument(),returnValue);
      variableName = (SYMBOL_HN *) DOPToPointer(returnValue);
     }
      
   /*===========================================*/
   /* Determine the new value for the variable. */
   /*===========================================*/

   if (GetFirstArgument()->nextArg == NULL)
     { unbindVar = CLIPS_TRUE; }
   else if (GetFirstArgument()->nextArg->nextArg == NULL)
     { EvaluateExpression(GetFirstArgument()->nextArg,returnValue); }
   else
     { StoreInMultifield(returnValue,GetFirstArgument()->nextArg,CLIPS_TRUE); }
     
   /*==================================*/
   /* Bind a defglobal if appropriate. */
   /*==================================*/

#if DEFGLOBAL_CONSTRUCT
   if (theGlobal != NULL)
     {
      QSetDefglobalValue(theGlobal,returnValue,unbindVar);
      return;
     }
#endif

   /*===============================================*/
   /* Search for the variable in the list of binds. */
   /*===============================================*/

   theBind = BindList;     /* changed 03-11-96 */
   lastBind = NULL;        /* changed 03-11-96 */

   while ((theBind != NULL) && (found == CLIPS_FALSE))
     {
      if (theBind->supplementalInfo == (VOID *) variableName)
        { found = CLIPS_TRUE; }
      else
        {
         lastBind = theBind;         /* changed 03-11-96 */
         theBind = theBind->next;    /* changed 03-11-96 */
        }
     }

   /*========================================================*/
   /* If variable was not in the list of binds, then add it. */
   /* Make sure that this operation preserves the bind list  */
   /* as a stack.                                            */
   /*========================================================*/

   if (found == CLIPS_FALSE)
     {
      if (unbindVar == CLIPS_FALSE)
        {
         theBind = get_struct(dataObject);
         theBind->supplementalInfo = (VOID *) variableName;
         theBind->next = NULL;
         if (lastBind == NULL)
           { BindList = theBind; }
         else
           { lastBind->next = theBind; }
        }
      else
        {
         returnValue->type = SYMBOL;
         returnValue->value = CLIPSFalseSymbol;
         return;
        }
     }
   else
     { ValueDeinstall(theBind); }

   /*================================*/
   /* Set the value of the variable. */
   /*================================*/
   
   if (unbindVar == CLIPS_FALSE)
     {
      theBind->type = returnValue->type;
      theBind->value = returnValue->value;
      theBind->begin = returnValue->begin;
      theBind->end = returnValue->end;
      ValueInstall(returnValue);
     }
   else
     {
      if (lastBind == NULL) BindList = theBind->next;
      else lastBind->next = theBind->next;
      rtn_struct(dataObject,theBind);
      returnValue->type = SYMBOL;
      returnValue->value = CLIPSFalseSymbol;
     }
  }
  
/*******************************************/
/* GetBoundVariable: Searches the BindList */
/*   for a specified variable.             */
/*******************************************/
globle BOOLEAN GetBoundVariable(vPtr,varName)
  DATA_OBJECT_PTR vPtr;
  SYMBOL_HN *varName;
  {
   DATA_OBJECT_PTR bindPtr;

   for (bindPtr = BindList; bindPtr != NULL; bindPtr = bindPtr->next)
     {
      if (bindPtr->supplementalInfo == (VOID *) varName)
        {
         vPtr->type = bindPtr->type;
         vPtr->value = bindPtr->value;
         vPtr->begin = bindPtr->begin;
         vPtr->end = bindPtr->end;
         return(CLIPS_TRUE);
        }
     }
     
   return(CLIPS_FALSE);
  }
  
/*************************************************/
/* FlushBindList: Removes all variables from the */
/*   list of currently bound local variables.    */
/*************************************************/
globle VOID FlushBindList()
  {
   ReturnValues(BindList);
   BindList = NULL;
  }
  
/***************************************/
/* PrognFunction: CLIPS access routine */
/*   for the progn function.           */
/***************************************/
globle VOID PrognFunction(returnValue)
  DATA_OBJECT_PTR returnValue;
  {
   int numa, i;

   numa = RtnArgCount();

   if (numa == 0)
     {
      returnValue->type = SYMBOL;
      returnValue->value = CLIPSFalseSymbol;
      return;
     }

   i = 1;
   while ((i <= numa) && (GetHaltExecution() != CLIPS_TRUE))
     {
      RtnUnknown(i,returnValue);
      if ((BreakFlag == CLIPS_TRUE) || (ReturnFlag == CLIPS_TRUE))
        break;
      i++;
     }

   if (GetHaltExecution() == CLIPS_TRUE)
     {
      returnValue->type = SYMBOL;
      returnValue->value = CLIPSFalseSymbol;
      return;
     }

   return;
  }

/*****************************************************************/
/* ReturnFunction: CLIPS access routine for the return function. */
/*****************************************************************/
globle VOID ReturnFunction(result)
  DATA_OBJECT_PTR result;
  {
   if (RtnArgCount() == 0)
     {
      result->type = RVOID;
      result->value = CLIPSFalseSymbol;
     }
   else
     RtnUnknown(1,result);
   ReturnFlag = CLIPS_TRUE;
  }

/***************************************************************/
/* BreakFunction: CLIPS access routine for the break function. */   
/***************************************************************/
globle VOID BreakFunction()
  {
   BreakFlag = CLIPS_TRUE;
  }

/*****************************************************************/
/* SwitchFunction: CLIPS access routine for the switch function. */
/*****************************************************************/
globle VOID SwitchFunction(result)
  DATA_OBJECT_PTR result;
  {
   DATA_OBJECT switch_val,case_val;
   EXPRESSION *exp;
   
   result->type = SYMBOL;
   result->value = CLIPSFalseSymbol;
   
   /* ==========================
      Get the value to switch on
      ========================== */
   EvaluateExpression(GetFirstArgument(),&switch_val);
   if (EvaluationError)
     return;
   for (exp = GetFirstArgument()->nextArg ; exp != NULL ; exp = exp->nextArg->nextArg)
     {
      /* =================================================
         RVOID is the default case (if any) for the switch
         ================================================= */
      if (exp->type == RVOID)
        {
         EvaluateExpression(exp->nextArg,result);
         return;
        }
        
      /* ====================================================
         If the case matches, evaluate the actions and return
         ==================================================== */
      EvaluateExpression(exp,&case_val);
      if (EvaluationError)
        return;
      if (switch_val.type == case_val.type)
        {
         if ((case_val.type == MULTIFIELD) ? MultifieldDOsEqual(&switch_val,&case_val) :
             (switch_val.value == case_val.value))
           {
            EvaluateExpression(exp->nextArg,result);
            return;
           }
        }
     }
  }


  


