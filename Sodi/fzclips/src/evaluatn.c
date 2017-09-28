   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  03/09/94            */
   /*                                                     */
   /*                  EVALUATION MODULE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides routines for evaluating expressions.    */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Donnell                                     */
/*      Bob Orchard (NRCC - Nat'l Research Council of Canada)*/
/*                  (Fuzzy reasoning extensions)             */
/*                  (certainty factors for facts and rules)  */
/*                  (extensions to run command)              */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#define _EVALUATN_SOURCE_

#include <stdio.h>
#define _CLIPS_STDIO_
#include <string.h>
#include <ctype.h>

#include "setup.h"

#if FUZZY_DEFTEMPLATES    /* added 03-06-96 */
#include "symbol.h"
#include "fuzzyrhs.h"
#include "fuzzypsr.h"
#endif

#include "constant.h"
#include "clipsmem.h"
#include "router.h"
#include "extnfunc.h"
#include "prcdrfun.h"
#include "multifld.h"
#include "factmngr.h"
#include "prntutil.h"
#include "exprnpsr.h"
#include "utility.h"
#include "commline.h"

#if DEFFUNCTION_CONSTRUCT
#include "dffnxfun.h"
#endif

#if DEFGENERIC_CONSTRUCT
#include "genrccom.h"
#endif

#if OBJECT_SYSTEM
#include "object.h"
#endif

#include "evaluatn.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if ANSI_COMPILER
   static VOID                    PropagateReturnAtom(int,VOID *);
#else
   static VOID                    PropagateReturnAtom();
#endif

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   globle struct expr          *CurrentExpression = NULL;
   globle int                   EvaluationError = CLIPS_FALSE;
   globle int                   HaltExecution = CLIPS_FALSE;
   globle int                   CurrentEvaluationDepth = 0;
#if FUZZY_DEFTEMPLATES    /* added 03-06-96 */
   globle struct entityRecord  *PrimitivesArray[PRIMITIVES_ARRAY_SIZE];
#else
   globle struct entityRecord  *PrimitivesArray[70];
#endif

/*******************************************************************/
/* EvaluateExpression: Evaluates a CLIPS expression. Returns FALSE */
/*   if no errors occurred during evaluation, otherwise TRUE.      */
/*******************************************************************/
globle int EvaluateExpression(problem,returnValue)
  struct expr *problem;
  DATA_OBJECT_PTR returnValue;
  {
   struct expr *oldArgument;
   struct FunctionDefinition *fptr;
        
   if (problem == NULL)
     {
      returnValue->type = SYMBOL;
      returnValue->value = CLIPSFalseSymbol;
      return(EvaluationError);
     }

   switch (problem->type)
     {
      case STRING:
      case SYMBOL:
      case FLOAT:
      case INTEGER:
#if OBJECT_SYSTEM
      case INSTANCE_NAME:
      case INSTANCE_ADDRESS:
#endif
#if FUZZY_DEFTEMPLATES /* added 03-06-96 */
      case FUZZY_VALUE:
#endif
      case EXTERNAL_ADDRESS:
        returnValue->type = problem->type;
        returnValue->value = problem->value;
        break;

#if FUZZY_DEFTEMPLATES  /* added 03-06-96 */
      case S_FUNCTION:
      case PI_FUNCTION:
      case Z_FUNCTION:
      case SINGLETON_EXPRESSION:
	     /* At some time it may be worthwhile making this into an FCALL
		    but only when we allow user's to create functions that return
			fuzzy values -- this may not happen
	     */
		{
		  struct fuzzy_value *fvptr;
		  fvptr = getConstantFuzzyValue(problem, &EvaluationError);
          returnValue->type = FUZZY_VALUE;
		  if (fvptr != NULL)
		    {
              returnValue->value = (VOID *)AddFuzzyValue(fvptr);
		      /* AddFuzzyValue makes a copy of the fuzzy value -- so remove this one */
              rtnFuzzyValue(fvptr);
			}
	      else
                  {
                   returnValue->type = RVOID;
                   returnValue->value = CLIPSFalseSymbol;
                   SetEvaluationError(CLIPS_TRUE);
                  }
        }
        break;
#endif

      case FCALL:
        {
         oldArgument = CurrentExpression;
         CurrentExpression = problem;
         fptr = (struct FunctionDefinition *) problem->value;
         switch(fptr->returnValueType)
           {
            case 'v' :
              (* (VOID (*)(VOID_ARG)) fptr->functionPointer)();
              returnValue->type = RVOID;
              returnValue->value = CLIPSFalseSymbol;
              break;
            case 'b' :
              returnValue->type = SYMBOL;
              if ((* (int (*)(VOID_ARG)) fptr->functionPointer)())
                returnValue->value = CLIPSTrueSymbol;
              else
                returnValue->value = CLIPSFalseSymbol;
              break;
            case 'a' :
              returnValue->type = EXTERNAL_ADDRESS;
              returnValue->value =
                             (* (VOID *(*)(VOID_ARG)) fptr->functionPointer)();
              break;
            case 'i' :
              returnValue->type = INTEGER;
              returnValue->value = (VOID *)
                AddLong((long) (* (int (*)(VOID_ARG)) fptr->functionPointer)());
              break;
            case 'l' :
              returnValue->type = INTEGER;
              returnValue->value = (VOID *)
                 AddLong((* (long int (*)(VOID_ARG)) fptr->functionPointer)());
              break;
#if FUZZY_DEFTEMPLATES   /* added 03-06-96 */
            case 'F' :  
	      { 
                struct fuzzy_value *fvPtr;
 
                fvPtr = (* (struct fuzzy_value * (*)(VOID_ARG)) fptr->functionPointer)();
                if (fvPtr != NULL)
                  {
                   returnValue->type = FUZZY_VALUE;
                   returnValue->value = (VOID *)AddFuzzyValue( fvPtr );
                   /* AddFuzzyValue makes a copy of fv .. so return it */
                   rtnFuzzyValue( fvPtr );
                  }
                else
                  {
                   returnValue->type = RVOID;
                   returnValue->value = CLIPSFalseSymbol;
                  }
               }
              break;
#endif
            case 'f' :
              returnValue->type = FLOAT;
              returnValue->value = (VOID *)
                 AddDouble((double) (* (float (*)(VOID_ARG)) fptr->functionPointer)());
              break;
            case 'd' :
              returnValue->type = FLOAT;
              returnValue->value = (VOID *)
                 AddDouble((* (double (*)(VOID_ARG)) fptr->functionPointer)());
              break;
            case 's' :
              returnValue->type = STRING;
              returnValue->value = (VOID *)
                (* (SYMBOL_HN *(*)(VOID_ARG)) fptr->functionPointer)();
              break;
            case 'w' :
              returnValue->type = SYMBOL;
              returnValue->value = (VOID *)
                (* (SYMBOL_HN *(*)(VOID_ARG)) fptr->functionPointer)();
              break;
#if OBJECT_SYSTEM
            case 'x' :
              returnValue->type = INSTANCE_ADDRESS;
              returnValue->value =
                             (* (VOID *(*)(VOID_ARG)) fptr->functionPointer)();
              break;
            case 'o' :
              returnValue->type = INSTANCE_NAME;
              returnValue->value = (VOID *)
                (* (SYMBOL_HN *(*)(VOID_ARG)) fptr->functionPointer)();
              break;
#endif
            case 'c' :
              {
               char cbuff[2];

               cbuff[0] = (* (char (*)(VOID_ARG)) fptr->functionPointer)();
               cbuff[1] = EOS;
               returnValue->type = SYMBOL;
               returnValue->value = (VOID *) AddSymbol(cbuff);
               break;
              }
              
            case 'j' :
            case 'k' :
            case 'm' :
            case 'n' :
            case 'u' :
#if ANSI_COMPILER
              (* (VOID (*)(DATA_OBJECT_PTR)) fptr->functionPointer)(returnValue);
#else
              (* (VOID (*)()) fptr->functionPointer)(returnValue);
#endif
              break;

            default :
               CLIPSSystemError("EVALUATN",2);
               ExitCLIPS(5);
               break;
            }

        CurrentExpression = oldArgument;
        break;
        }

     case MULTIFIELD:
        returnValue->type = MULTIFIELD;
        returnValue->value = ((DATA_OBJECT_PTR) (problem->value))->value;
        returnValue->begin = ((DATA_OBJECT_PTR) (problem->value))->begin;
        returnValue->end = ((DATA_OBJECT_PTR) (problem->value))->end;
        break;

     case MF_VARIABLE:
     case SF_VARIABLE:
        if (GetBoundVariable(returnValue,(SYMBOL_HN *) problem->value) == CLIPS_FALSE)
          {   
           PrintErrorID("EVALUATN",1,CLIPS_FALSE);
           PrintCLIPS(WERROR,"Variable ");
           PrintCLIPS(WERROR,ValueToString(problem->value));
           PrintCLIPS(WERROR," is unbound\n");
           returnValue->type = SYMBOL;
           returnValue->value = CLIPSFalseSymbol;
           SetEvaluationError(CLIPS_TRUE);
          }
        break;
        
      default:
        if (PrimitivesArray[problem->type] == NULL)
          {
           CLIPSSystemError("EVALUATN",3);
           ExitCLIPS(5);
          }
          
        if (PrimitivesArray[problem->type]->copyToEvaluate)
          {
           returnValue->type = problem->type;
           returnValue->value = problem->value;
           break;
          }
          
        if (PrimitivesArray[problem->type]->evaluateFunction == NULL)
          {
           CLIPSSystemError("EVALUATN",4);
           ExitCLIPS(5);
          }
          
        oldArgument = CurrentExpression;
        CurrentExpression = problem;
        (*PrimitivesArray[problem->type]->evaluateFunction)(problem->value,returnValue);
        CurrentExpression = oldArgument;
        break;
     }
     
   PropagateReturnValue(returnValue);
   return(EvaluationError);
  }

/******************************************/
/* InstallPrimitive: Installs a primitive */
/*   data type in the primitives array.   */
/******************************************/
globle VOID InstallPrimitive(thePrimitive,whichPosition)
  struct entityRecord *thePrimitive;
  int whichPosition;
  {
   if (PrimitivesArray[whichPosition] != NULL)
     {
      CLIPSSystemError("EVALUATN",5);
      ExitCLIPS(7);
     }
     
   PrimitivesArray[whichPosition] = thePrimitive;
  }
  
/******************************************************/
/* SetEvaluationError: Sets the EvaluationError flag. */
/******************************************************/
globle VOID SetEvaluationError(value)
  int value;
  {
   EvaluationError = value;
   if (value == CLIPS_TRUE) HaltExecution = CLIPS_TRUE;
  }

/*********************************************************/
/* GetEvaluationError: Returns the EvaluationError flag. */
/*********************************************************/
globle int GetEvaluationError()
  {
   return(EvaluationError);
  }

/**************************************************/
/* SetHaltExecution: Sets the HaltExecution flag. */
/**************************************************/
globle VOID SetHaltExecution(value)
  int value;
  { HaltExecution = value; }

/*****************************************************/
/* GetHaltExecution: Returns the HaltExecution flag. */
/*****************************************************/
globle int GetHaltExecution()
  {
   return(HaltExecution);
  }

/******************************************************/
/* ReturnValues: Returns a linked list of DATA_OBJECT */
/*   structures to the pool of free memory.           */
/******************************************************/
globle VOID ReturnValues(garbagePtr)
  DATA_OBJECT_PTR garbagePtr;
  {
   DATA_OBJECT_PTR nextPtr;

   while (garbagePtr != NULL)
     {
      nextPtr = garbagePtr->next;
      ValueDeinstall(garbagePtr);
      rtn_struct(dataObject,garbagePtr);
      garbagePtr = nextPtr;
     }
  }

/***************************************************/
/* PrintDataObject: Prints a DATA_OBJECT structure */
/*   to the specified logical name.                */
/***************************************************/
globle VOID PrintDataObject(fileid,argPtr)
  char *fileid;
  DATA_OBJECT_PTR argPtr;
  {
   switch(argPtr->type)
     {
      case RVOID:
      case SYMBOL:
      case STRING:
      case INTEGER:
      case FLOAT:
      case EXTERNAL_ADDRESS:
      case FACT_ADDRESS:
#if OBJECT_SYSTEM
      case INSTANCE_NAME:
      case INSTANCE_ADDRESS:
#endif
#if FUZZY_DEFTEMPLATES  /* added 03-06-96 */
     case FUZZY_VALUE:
#endif
        PrintAtom(fileid,argPtr->type,argPtr->value);
        break;
      case MULTIFIELD:
        PrintMultifield(fileid,(struct multifield *) argPtr->value,
                        argPtr->begin,argPtr->end,CLIPS_TRUE);
        break;

      default:
        PrintCLIPS(fileid,"<UnknownPrintType");
        PrintLongInteger(fileid,(long int) argPtr->type);
        PrintCLIPS(fileid,">");
        SetHaltExecution(CLIPS_TRUE);
        SetEvaluationError(CLIPS_TRUE);
        break;
     }
  }

/*************************************************/
/* SetMultifieldErrorValue: Creates a multifield */
/*   value of length zero for error returns.     */
/*************************************************/
globle VOID SetMultifieldErrorValue(returnValue)
  DATA_OBJECT_PTR returnValue;
  {
   returnValue->type = MULTIFIELD;
   returnValue->value = CreateMultifield(0L);
   returnValue->begin = 1;
   returnValue->end = 0;
  }

/**************************************************/
/* ValueInstall: Increments the appropriate count */
/*   (in use) values for a DATA_OBJECT structure. */
/**************************************************/
globle VOID ValueInstall(vPtr)
  DATA_OBJECT *vPtr;
  {
   if (vPtr->type == MULTIFIELD) MultifieldInstall((struct multifield *) vPtr->value);
   else AtomInstall(vPtr->type,vPtr->value);
  }

/****************************************************/
/* ValueDeinstall: Decrements the appropriate count */
/*   (in use) values for a DATA_OBJECT structure.   */
/****************************************************/
globle VOID ValueDeinstall(vPtr)
  DATA_OBJECT *vPtr;
  {
   if (vPtr->type == MULTIFIELD) MultifieldDeinstall((struct multifield *) vPtr->value);
   else AtomDeinstall(vPtr->type,vPtr->value);
  }
  
/*****************************************/
/* AtomInstall: Increments the reference */
/*   count of an atomic data type.       */
/*****************************************/
globle VOID AtomInstall(type,vPtr)
  int type;
  VOID *vPtr;
  {
   switch (type)
     {
      case SYMBOL:
      case STRING:
#if DEFGLOBAL_CONSTRUCT
      case GBL_VARIABLE:
#endif
#if OBJECT_SYSTEM
      case INSTANCE_NAME:
#endif
        IncrementSymbolCount(vPtr);
        break;

      case FLOAT:
        IncrementFloatCount(vPtr);
        break;

      case INTEGER:
        IncrementIntegerCount(vPtr);
        break;

      case MULTIFIELD:
        MultifieldInstall(vPtr);
        break;

#if FUZZY_DEFTEMPLATES /* added 03-06-96 */
      /* fuzzy values have a name which is a symbol */
      case FUZZY_VALUE:
	    InstallFuzzyValue(vPtr);
        break;
#endif
        
      case RVOID:
        break;

      default:
        if (PrimitivesArray[type] == NULL) break;
        if (PrimitivesArray[type]->bitMap) IncrementBitMapCount(vPtr);
        else if (PrimitivesArray[type]->incrementBusyCount)
          { (*PrimitivesArray[type]->incrementBusyCount)(vPtr); }
        break;
     }
  }

/*******************************************/
/* AtomDeinstall: Decrements the reference */
/*   count of an atomic data type.         */
/*******************************************/
globle VOID AtomDeinstall(type,vPtr)
  int type;
  VOID *vPtr;
  {
   switch (type)
     {
      case SYMBOL:
      case STRING:
#if DEFGLOBAL_CONSTRUCT
      case GBL_VARIABLE:
#endif
#if OBJECT_SYSTEM
      case INSTANCE_NAME:
#endif
        DecrementSymbolCount(vPtr);
        break;

      case FLOAT:
        DecrementFloatCount(vPtr);
        break;

      case INTEGER:
        DecrementIntegerCount(vPtr);
        break;

#if FUZZY_DEFTEMPLATES /* added 03-06-96 */
      case FUZZY_VALUE:
	    DeinstallFuzzyValue(vPtr);
        break;
#endif

      case MULTIFIELD:
        MultifieldDeinstall(vPtr);
        break;
        
      case RVOID:
        break;
        
      default:
        if (PrimitivesArray[type] == NULL) break;
        if (PrimitivesArray[type]->bitMap) DecrementBitMapCount(vPtr);
        else if (PrimitivesArray[type]->decrementBusyCount)
          { (*PrimitivesArray[type]->decrementBusyCount)(vPtr); }
     }
  }
  
/*********************************************************************/
/* PropagateReturnValue: Decrements the associated depth for a value */
/*   stored in a DATA_OBJECT structure. In effect, the values        */
/*   returned by certain evaluations (such as a deffunction call)    */
/*   are passed up to the previous depth of evaluation. The return   */
/*   value's depth is decremented so that it will not be garbage     */
/*   collected along with other items that are no longer needed from */
/*   the evaluation that generated the return value.                 */
/*********************************************************************/
globle VOID PropagateReturnValue(vPtr)
  DATA_OBJECT *vPtr;
  {
   long i; /* 6.04 Bug Fix */
   struct multifield *theSegment;
   struct field HUGE_ADDR *theMultifield;

   if (vPtr->type != MULTIFIELD)
     PropagateReturnAtom(vPtr->type,vPtr->value);
   else
     {
      theSegment = (struct multifield *) vPtr->value;
      if (theSegment->depth > CurrentEvaluationDepth)
        theSegment->depth = (short) CurrentEvaluationDepth;
      theMultifield = theSegment->theFields;
      i = vPtr->begin;
      while (i <= vPtr->end)
        {
         PropagateReturnAtom(theMultifield[i].type,theMultifield[i].value);
         i++;
        }
     }
  }

/*****************************************/
/* PropagateReturnAtom: Support function */
/*   for PropagateReturnValue.           */
/*****************************************/
static VOID PropagateReturnAtom(type,value)
  int type;
  VOID *value;
  {
   switch (type)
     {
      case INTEGER         :
      case FLOAT           :
      case SYMBOL          :
      case STRING          :

#if OBJECT_SYSTEM
      case INSTANCE_NAME   :
#endif
        if (((SYMBOL_HN *) value)->depth > CurrentEvaluationDepth)
          { ((SYMBOL_HN *) value)->depth = CurrentEvaluationDepth; }
        break;
        
#if FUZZY_DEFTEMPLATES 
      case FUZZY_VALUE     :
        {       
          if (((FUZZY_VALUE_HN *) value)->depth > CurrentEvaluationDepth)
            { ((FUZZY_VALUE_HN *) value)->depth = CurrentEvaluationDepth; }
        }
          
        break;
#endif

#if OBJECT_SYSTEM
      case INSTANCE_ADDRESS : 
        if (((INSTANCE_TYPE *) value)->depth > CurrentEvaluationDepth)
          { ((INSTANCE_TYPE *) value)->depth = CurrentEvaluationDepth; }
        break;
#endif
      case FACT_ADDRESS :
        if (((int) ((struct fact *) value)->depth) > CurrentEvaluationDepth)
          { ((struct fact *) value)->depth = (unsigned) CurrentEvaluationDepth; }
        break;
     }
  }
  
#if DEFFUNCTION_CONSTRUCT || DEFGENERIC_CONSTRUCT

/************************************************/
/* CLIPSFunctionCall: Allows CLIPS Deffunctions */
/*   and Generic Functions to be called from C. */
/*   Allows only constants as arguments.        */
/************************************************/
globle int CLIPSFunctionCall(name,args,result)
  char *name,*args;
  DATA_OBJECT *result;
  {
#if DEFGENERIC_CONSTRUCT
   VOID *gfunc;
#endif
#if DEFFUNCTION_CONSTRUCT
   VOID *dptr;
#endif
   struct FunctionDefinition *fptr;
   EXPRESSION *argexps, *top;
   int error = CLIPS_FALSE;
   
   /*=============================================*/
   /* Force periodic cleanup if the function call */
   /* was executed from an embedded application.  */
   /*=============================================*/

   if ((CurrentEvaluationDepth == 0) && (! EvaluatingTopLevelCommand) &&
       (CurrentExpression == NULL))
     { PeriodicCleanup(CLIPS_TRUE,CLIPS_FALSE); } 

   /*========================*/
   /* Reset the error state. */
   /*========================*/
   
   if (CurrentEvaluationDepth == 0) SetHaltExecution(CLIPS_FALSE);
   EvaluationError = CLIPS_FALSE;

   /*======================================*/
   /* Initialize the default return value. */
   /*======================================*/
   
   result->type = SYMBOL;
   result->value = CLIPSFalseSymbol;

   /*============================*/
   /* Parse the argument string. */
   /*============================*/
    
   argexps = ParseConstantArguments(args,&error);
   if (error == CLIPS_TRUE) return(CLIPS_TRUE);

   /*=====================================================*/
   /* Check to see if the function call is a deffunction. */
   /*=====================================================*/
   
#if DEFFUNCTION_CONSTRUCT
   if ((dptr = (VOID *) LookupDeffunctionInScope(name)) != NULL)
     {
      top = GenConstant(PCALL,dptr);
      top->argList = argexps;
      error = EvaluateExpression(top,result);
     }
   else
#endif

   /*====================================================*/
   /* Check to see if the function call is a defgeneric. */
   /*====================================================*/
   
#if DEFGENERIC_CONSTRUCT
   if ((gfunc = (VOID *) LookupDefgenericInScope(name)) != NULL)
     {
      top = GenConstant(GCALL,gfunc);
      top->argList = argexps;
      error = EvaluateExpression(top,result);
     }
   else
#endif

   /*======================================*/
   /* Check to see if the function call is */
   /* a system or user defined function.   */
   /*======================================*/
   
   if ((fptr = FindFunction(name)) != NULL)
     {
      top = GenConstant(FCALL,fptr);
      top->argList = argexps;
      error = EvaluateExpression(top,result);
     }
     
   /*=========================================================*/
   /* Otherwise signal an error if a deffunction, defgeneric, */
   /* or user defined function doesn't exist that matches     */
   /* the specified function name.                            */
   /*=========================================================*/
   
   else
     {   
      PrintErrorID("EVALUATN",2,CLIPS_FALSE);
      PrintCLIPS(WERROR,"No function, generic function or deffunction of name ");
      PrintCLIPS(WERROR,name);
      PrintCLIPS(WERROR," exists for external call.\n");
      top = argexps;
      error = CLIPS_TRUE;
     }

   /*========================*/
   /* Return the expression. */
   /*========================*/
   
   ReturnExpression(top);
   
   /*==========================*/
   /* Return the error status. */
   /*==========================*/
   
   return(error);
  }

#endif

/***************************************************/
/* CopyDataObject: Copies the values from a source */
/*   DATA_OBJECT to a destination DATA_OBJECT.     */
/***************************************************/
globle VOID CopyDataObject(dst,src,garbageMultifield)
  DATA_OBJECT *dst,*src;
  int garbageMultifield;
  {
   if (src->type != MULTIFIELD)
     {
      dst->type = src->type;
      dst->value = src->value;
     }
   else
     {
      DuplicateMultifield(dst,src);
      if (garbageMultifield)
        { AddToMultifieldList((struct multifield *) dst->value); }
     }
  }

/************************************************************************/
/* ConvertValueToExpression: Converts the value stored in a data object */
/*   into an expression. For multifield values, a chain of expressions  */
/*   is generated and the chain is linked by the nextArg field. For a   */
/*   single field value, a single expression is created.                */
/************************************************************************/
globle struct expr *ConvertValueToExpression(theValue)
  DATA_OBJECT *theValue;
  {
   long i; /* 6.04 Bug Fix */
   struct expr *head = NULL, *last = NULL, *new;
   
   if (GetpType(theValue) != MULTIFIELD)
     { return(GenConstant(GetpType(theValue),GetpValue(theValue))); }
   
   for (i = GetpDOBegin(theValue); i <= GetpDOEnd(theValue); i++)
     {
      new = GenConstant(GetMFType(GetpValue(theValue),i),
                        GetMFValue(GetpValue(theValue),i));
      if (last == NULL) head = new;
      else last->nextArg = new;
      last = new;
     }
   
   if (head == NULL)
     return(GenConstant(FCALL,(VOID *) FindFunction("create$")));
     
   return(head);
  }

/****************************************/
/* GetAtomicHashValue: Returns the hash */
/*   value for an atomic data type.     */
/****************************************/
unsigned int GetAtomicHashValue(type,value,position)
  int type;
  VOID *value;
  int position;
  {
   unsigned int tvalue;
   union
     {
      double fv;
      unsigned int liv;
     } fis;
   
   switch (type)
     {
      case FLOAT:
        fis.fv = ValueToDouble(value);
        tvalue = fis.liv;
        break;
        
      case INTEGER:
        tvalue = (unsigned int) ValueToLong(value);
        break;
        
      case FACT_ADDRESS:
#if OBJECT_SYSTEM
      case INSTANCE_ADDRESS:
#endif
      case EXTERNAL_ADDRESS:
         tvalue = (unsigned int) value;
         break;
         
      case STRING:
#if OBJECT_SYSTEM
      case INSTANCE_NAME:
#endif
      case SYMBOL:
        tvalue = ((SYMBOL_HN *) value)->bucket;
        break;

      default:
        tvalue = type;
     }
   
   if (position < 0) return(tvalue);
   
   return((unsigned int) (tvalue * (position + 29)));
  }
