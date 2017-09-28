   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  05/12/93            */
   /*                                                     */
   /*             MULTIFIELD FUNCTIONS MODULE             */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for several multifield         */
/*   functions including first$, rest$, subseq$, delete$,    */
/*   replace$, insert$, explode$, implode$, nth$, member$,   */
/*   subsetp, progn$, str-implode, str-explode, subset, nth, */
/*   mv-replace, member, mv-subseq, and mv-delete.           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian Donnell                                        */
/*      Barry Cameron                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#define _MULTIFUN_SOURCE_

#include "setup.h"

#if MULTIFIELD_FUNCTIONS || OBJECT_SYSTEM

#include <stdio.h>
#define _CLIPS_STDIO_
#include <string.h>

#include "clipsmem.h"
#include "argacces.h"
#include "multifld.h"
#include "router.h"
#include "multifun.h"
#include "exprnpsr.h"
#include "prcdrpsr.h"
#include "prcdrfun.h"
#if (! BLOAD_ONLY) && (! RUN_TIME)
#include "scanner.h"
#endif

#if OBJECT_SYSTEM
#include "object.h"
#endif

/**************/
/* STRUCTURES */
/**************/

typedef struct fieldVarStack
  {
   int type;
   VOID *value;
   long index;
   struct fieldVarStack *nxt;
  } FIELD_VAR_STACK;
  
/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if ANSI_COMPILER
#if MULTIFIELD_FUNCTIONS
   static int                     FindItemInSegment(int,VOID *,DATA_OBJECT_PTR);
#if (! BLOAD_ONLY) && (! RUN_TIME)
   static struct expr            *MultifieldPrognParser(struct expr *,char *);
   static VOID                    ReplaceMvPrognFieldVars(SYMBOL_HN *,struct expr *,int);
#endif
#endif
   static VOID                    MVRangeError(long,long,long,char *);
#else
#if MULTIFIELD_FUNCTIONS
   static int                     FindItemInSegment();
#if (! BLOAD_ONLY) && (! RUN_TIME)
   static struct expr            *MultifieldPrognParser();
   static VOID                    ReplaceMvPrognFieldVars();
#endif
#endif
   static VOID                    MVRangeError();
#endif
#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

#if MULTIFIELD_FUNCTIONS

   static FIELD_VAR_STACK    *FieldVarStack = NULL;

#if ! RUN_TIME
/**********************************************/
/* MultifieldFunctionDefinitions: Initializes */
/*   the multifield functions.                */
/**********************************************/
globle VOID MultifieldFunctionDefinitions()
  {   
   DefineFunction2("first$", 'm', PTIF FirstFunction, "FirstFunction", "11m");
   DefineFunction2("rest$", 'm', PTIF RestFunction, "RestFunction", "11m");
   DefineFunction2("subseq$", 'm', PTIF SubseqFunction, "SubseqFunction", "33im");
   DefineFunction2("delete$", 'm', PTIF DeleteFunction, "DeleteFunction", "33im");
   DefineFunction2("replace$", 'm', PTIF ReplaceFunction, "ReplaceFunction","4**mii");
   DefineFunction2("insert$", 'm', PTIF InsertFunction, "InsertFunction", "3**mi");
   DefineFunction2("explode$", 'm', PTIF ExplodeFunction, "ExplodeFunction", "11s");
   DefineFunction2("implode$", 's', PTIF ImplodeFunction, "ImplodeFunction", "11m");
   DefineFunction2("nth$", 'u', PTIF NthFunction, "NthFunction", "22*im");
   DefineFunction2("member$", 'u', PTIF MemberFunction, "MemberFunction", "22*um");
   DefineFunction2("subsetp", 'b', PTIF SubsetpFunction, "SubsetpFunction", "22*mm");
   DefineFunction2("progn$", 'u', PTIF MultifieldPrognFunction, "MultifieldPrognFunction", NULL);
   DefineFunction2("str-implode", 's', PTIF ImplodeFunction, "ImplodeFunction", "11m");
   DefineFunction2("str-explode", 'm', PTIF ExplodeFunction, "ExplodeFunction", "11s");
   DefineFunction2("subset", 'b', PTIF SubsetpFunction, "SubsetpFunction", "22*mm");
   DefineFunction2("nth", 'u', PTIF NthFunction, "NthFunction", "22*im");
   DefineFunction2("mv-replace", 'm', PTIF MVReplaceFunction, "MVReplaceFunction","33*im");
   DefineFunction2("member", 'u', PTIF MemberFunction, "MemberFunction", "22*um");
   DefineFunction2("mv-subseq", 'm', PTIF MVSubseqFunction, "MVSubseqFunction", "33*iim");
   DefineFunction2("mv-delete", 'm', PTIF MVDeleteFunction,"MVDeleteFunction", "22*im");
#if ! BLOAD_ONLY
   AddFunctionParser("progn$",MultifieldPrognParser);
#endif
   FuncSeqOvlFlags("progn$",CLIPS_FALSE,CLIPS_FALSE);
   DefineFunction2("(get-progn$-field)", 'u', PTIF GetMvPrognField, "GetMvPrognField", "00");
   DefineFunction2("(get-progn$-index)", 'l', PTIF GetMvPrognIndex, "GetMvPrognIndex", "00");
  }
#endif

/****************************************/
/* DeleteFunction: CLIPS access routine */
/*   for the delete$ function.          */
/****************************************/
globle VOID DeleteFunction(returnValue)
  DATA_OBJECT_PTR returnValue;
  {
   DATA_OBJECT value1, value2, value3;  /* changed 03-11-96 */

   /*=======================================*/
   /* Check for the correct argument types. */
   /*=======================================*/
   
   if ((ArgTypeCheck("delete$",1,MULTIFIELD,&value1) == CLIPS_FALSE) ||
       (ArgTypeCheck("delete$",2,INTEGER,&value2) == CLIPS_FALSE) ||
       (ArgTypeCheck("delete$",3,INTEGER,&value3) == CLIPS_FALSE))
     {
      SetEvaluationError(CLIPS_TRUE);
      SetMultifieldErrorValue(returnValue);
      return;
     }

   /*=================================================*/
   /* Delete the section out of the multifield value. */
   /*=================================================*/
   
   if (DeleteMultiValueField(returnValue,&value1,
            DOToLong(value2),DOToLong(value3),"delete$") == CLIPS_FALSE)
     {
      SetEvaluationError(CLIPS_TRUE);
      SetMultifieldErrorValue(returnValue);
     }
  }
  
/******************************************/
/* MVDeleteFunction: CLIPS access routine */
/*   for the mv-delete function.          */
/******************************************/
globle VOID MVDeleteFunction(returnValue)
  DATA_OBJECT_PTR returnValue;
  {
   DATA_OBJECT value1, value2;

   /*=======================================*/
   /* Check for the correct argument types. */
   /*=======================================*/
   
   if ((ArgTypeCheck("mv-delete",1,INTEGER,&value1) == CLIPS_FALSE) ||
       (ArgTypeCheck("mv-delete",2,MULTIFIELD,&value2) == CLIPS_FALSE))
     {
      SetEvaluationError(CLIPS_TRUE);
      SetMultifieldErrorValue(returnValue);
      return;
     }

   /*=================================================*/
   /* Delete the section out of the multifield value. */
   /*=================================================*/
   
   if (DeleteMultiValueField(returnValue,&value2,
            DOToLong(value1),DOToLong(value1),"mv-delete") == CLIPS_FALSE)
     {
      SetEvaluationError(CLIPS_TRUE);
      SetMultifieldErrorValue(returnValue);
     }
  }

/*****************************************/
/* ReplaceFunction: CLIPS access routine */
/*   for the replace$ function.          */
/*****************************************/
globle VOID ReplaceFunction(returnValue)
  DATA_OBJECT_PTR returnValue;
  {
   DATA_OBJECT value1, value2, value3, value4; /* changed 03-11-96 */
   EXPRESSION *fieldarg;

   /*=======================================*/
   /* Check for the correct argument types. */
   /*=======================================*/
   
   if ((ArgTypeCheck("replace$",1,MULTIFIELD,&value1) == CLIPS_FALSE) ||
       (ArgTypeCheck("replace$",2,INTEGER,&value2) == CLIPS_FALSE) ||
       (ArgTypeCheck("replace$",3,INTEGER,&value3) == CLIPS_FALSE))
     {
      SetEvaluationError(CLIPS_TRUE);
      SetMultifieldErrorValue(returnValue);
      return;
     }
     
   /*===============================*/
   /* Create the replacement value. */
   /*===============================*/
   
   fieldarg = GetFirstArgument()->nextArg->nextArg->nextArg;
   if (fieldarg->nextArg != NULL)
     { StoreInMultifield(&value4,fieldarg,CLIPS_TRUE); }
   else
     { EvaluateExpression(fieldarg,&value4); }
     
   /*==============================================*/
   /* Replace the section in the multifield value. */
   /*==============================================*/

   if (ReplaceMultiValueField(returnValue,&value1,DOToInteger(value2),
                   DOToInteger(value3),&value4,"replace$") == CLIPS_FALSE)
     {
      SetEvaluationError(CLIPS_TRUE);
      SetMultifieldErrorValue(returnValue);
     }
  }
  
/*******************************************/
/* MVReplaceFunction: CLIPS access routine */
/*   for the mv-replace function.          */
/*******************************************/
globle VOID MVReplaceFunction(returnValue)
  DATA_OBJECT_PTR returnValue;
  {
   DATA_OBJECT value1, value2, value3;  /* changed 03-11-96 */

   /*=======================================*/
   /* Check for the correct argument types. */
   /*=======================================*/
   
   if ((ArgTypeCheck("mv-replace",1,INTEGER,&value1) == CLIPS_FALSE) ||
       (ArgTypeCheck("mv-replace",2,MULTIFIELD,&value2) == CLIPS_FALSE))
     {
      SetEvaluationError(CLIPS_TRUE);
      SetMultifieldErrorValue(returnValue);
      return;
     }
   
   /*===============================*/
   /* Create the replacement value. */
   /*===============================*/
   
   EvaluateExpression(GetFirstArgument()->nextArg->nextArg,&value3);

   /*==============================================*/
   /* Replace the section in the multifield value. */
   /*==============================================*/
   
   if (ReplaceMultiValueField(returnValue,&value2,DOToInteger(value1),
                   DOToInteger(value1),&value3,"mv-replace") == CLIPS_FALSE)
     {
      SetEvaluationError(CLIPS_TRUE);
      SetMultifieldErrorValue(returnValue);
     }
  }
  
/****************************************/
/* InsertFunction: CLIPS access routine */
/*   for the insert$ function.          */
/****************************************/
globle VOID InsertFunction(returnValue)
  DATA_OBJECT_PTR returnValue;
  {
   DATA_OBJECT value1, value2, value3;  /* changed 03-11-96 */
   EXPRESSION *fieldarg;

   /*=======================================*/
   /* Check for the correct argument types. */
   /*=======================================*/
   
   if ((ArgTypeCheck("insert$",1,MULTIFIELD,&value1) == CLIPS_FALSE) ||
       (ArgTypeCheck("insert$",2,INTEGER,&value2) == CLIPS_FALSE))
     {
      SetEvaluationError(CLIPS_TRUE);
      SetMultifieldErrorValue(returnValue);
      return;
     }
     
   /*=============================*/
   /* Create the insertion value. */
   /*=============================*/
   
   fieldarg = GetFirstArgument()->nextArg->nextArg;
   if (fieldarg->nextArg != NULL)
     StoreInMultifield(&value3,fieldarg,CLIPS_TRUE);
   else
     EvaluateExpression(fieldarg,&value3);
     
   /*===========================================*/
   /* Insert the value in the multifield value. */
   /*===========================================*/

   if (InsertMultiValueField(returnValue,&value1,DOToLong(value2),
                             &value3,"insert$") == CLIPS_FALSE)
     {
      SetEvaluationError(CLIPS_TRUE);
      SetMultifieldErrorValue(returnValue);
     }
  }
  
/*****************************************/
/* ExplodeFunction: CLIPS access routine */
/*   for the explode$ function.          */
/*****************************************/
globle VOID ExplodeFunction(str_value)
  DATA_OBJECT_PTR str_value;
  {
   DATA_OBJECT value;
   struct multifield *theMultifield;
   long end; /* 6.04 Bug Fix */
   
   /*=====================================*/
   /* Explode$ expects a single argument. */
   /*=====================================*/

   if (ArgCountCheck("explode$",EXACTLY,1) == -1)
     {
      SetHaltExecution(CLIPS_TRUE);
      SetEvaluationError(CLIPS_TRUE);
      SetMultifieldErrorValue(str_value);
      return;
     }
     
   /*==================================*/
   /* The argument should be a string. */
   /*==================================*/

   if (ArgTypeCheck("explode$",1,STRING,&value) == CLIPS_FALSE)
     {
      SetHaltExecution(CLIPS_TRUE);
      SetEvaluationError(CLIPS_TRUE);
      SetMultifieldErrorValue(str_value);
      return;
     }

   /*=====================================*/
   /* Convert the string to a multifield. */
   /*=====================================*/
  
   theMultifield = StringToMultifield(DOToString(value));
   if (theMultifield == NULL)
     {
      theMultifield = (struct multifield *) CreateMultifield(0L);
      end = 0;
     }
   else
     { end = GetMFLength(theMultifield); }

   /*========================*/
   /* Return the multifield. */
   /*========================*/

   SetpType(str_value,MULTIFIELD);
   SetpDOBegin(str_value,1);
   SetpDOEnd(str_value,end);
   SetpValue(str_value,(VOID *) theMultifield);
   return;
  }

/*****************************************/
/* ImplodeFunction: CLIPS access routine */
/*   for the implode$ function.          */
/*****************************************/
globle VOID *ImplodeFunction()
  {
   DATA_OBJECT value;   /* changed 03-11-96 */
   long strsize = 0;
   long i, j;
   char *tmp_str;
   char *ret_str;
   struct multifield *theMultifield;   /* changed 03-11-96 */
   VOID *rv;
   
   /*=====================================*/
   /* Implode$ expects a single argument. */
   /*=====================================*/

   if (ArgCountCheck("implode$",EXACTLY,1) == -1)
     { return(AddSymbol("")); }
     
   /*======================================*/
   /* The argument should be a multifield. */
   /*======================================*/

   if (ArgTypeCheck("implode$",1,MULTIFIELD,&value) == CLIPS_FALSE)
     { return(AddSymbol("")); }

   /*===================================================*/
   /* Determine the size of the string to be allocated. */
   /*===================================================*/

   theMultifield = (struct multifield *) GetValue(value);
   for (i = GetDOBegin(value) ; i <= GetDOEnd(value) ; i++)
     {
      if (GetMFType(theMultifield,i) == FLOAT)
        {
         tmp_str = FloatToString(ValueToDouble(GetMFValue(theMultifield,i)));
         strsize += strlen(tmp_str) + 1;
        }
      else if (GetMFType(theMultifield,i) == INTEGER)
        {
         tmp_str = LongIntegerToString(ValueToLong(GetMFValue(theMultifield,i)));
         strsize += strlen(tmp_str) + 1;
        }
      else if (GetMFType(theMultifield,i) == STRING)
        {
         strsize += strlen(ValueToString(GetMFValue(theMultifield,i))) + 3;
         tmp_str = ValueToString(GetMFValue(theMultifield,i));
         while(*tmp_str)
           {
            if(*tmp_str == '"')
              { strsize++; }
            tmp_str++;
           }
        }
#if OBJECT_SYSTEM
      else if (GetMFType(theMultifield,i) == INSTANCE_NAME)
        { strsize += strlen(ValueToString(GetMFValue(theMultifield,i))) + 3; }
      else if (GetMFType(theMultifield,i) == INSTANCE_ADDRESS)
        { strsize += strlen(ValueToString(((INSTANCE_TYPE *)
                            GetMFValue(theMultifield,i))->name)) + 3; }
#endif

      else
        { strsize += strlen(ValueToString(GetMFValue(theMultifield,i))) + 1; }
     }

   /*=============================================*/
   /* Allocate the string and copy all components */
   /* of the MULTIFIELD variable to it.             */
   /*=============================================*/

   if (strsize == 0) return(AddSymbol(""));
   ret_str = (char *) gm2(strsize);
   for(j=0, i=GetDOBegin(value); i <= GetDOEnd(value) ; i++)
     {

      /*============================*/
      /* Convert numbers to strings */
      /*============================*/

      if (GetMFType(theMultifield,i) == FLOAT)
        {
         tmp_str = FloatToString(ValueToDouble(GetMFValue(theMultifield,i)));
         while(*tmp_str)
           {
            *(ret_str+j) = *tmp_str;
            j++, tmp_str++;
           }
        }
      else if (GetMFType(theMultifield,i) == INTEGER)
        {
         tmp_str = LongIntegerToString(ValueToLong(GetMFValue(theMultifield,i)));
         while(*tmp_str)
           {
            *(ret_str+j) = *tmp_str;
            j++, tmp_str++;
           }
        }

      /*=======================================*/
      /* Enclose strings in quotes and preceed */
      /* imbedded quotes with a backslash      */
      /*=======================================*/

      else if (GetMFType(theMultifield,i) == STRING)
        {
         tmp_str = ValueToString(GetMFValue(theMultifield,i));
         *(ret_str+j) = '"';
         j++;
         while(*tmp_str)
           {
            if(*tmp_str == '"')
              {
               *(ret_str+j) = '\\';
               j++;
              }
            *(ret_str+j) = *tmp_str;
            j++, tmp_str++;
           }
         *(ret_str+j) = '"';
         j++;
        }
#if OBJECT_SYSTEM
      else if (GetMFType(theMultifield,i) == INSTANCE_NAME)
        {
         tmp_str = ValueToString(GetMFValue(theMultifield,i));
         *(ret_str + j++) = '[';
         while(*tmp_str)
           {
            *(ret_str+j) = *tmp_str;
            j++, tmp_str++;
           }
         *(ret_str + j++) = ']';
        }
      else if (GetMFType(theMultifield,i) == INSTANCE_ADDRESS)
        {
         tmp_str = ValueToString(((INSTANCE_TYPE *) GetMFValue(theMultifield,i))->name);
         *(ret_str + j++) = '[';
         while(*tmp_str)
           {
            *(ret_str+j) = *tmp_str;
            j++, tmp_str++;
           }
         *(ret_str + j++) = ']';
        }
#endif
      else
        {
         tmp_str = ValueToString(GetMFValue(theMultifield,i));
         while(*tmp_str)
           {
            *(ret_str+j) = *tmp_str;
            j++, tmp_str++;
           }
         }
      *(ret_str+j) = ' ';
      j++;
     }
   *(ret_str+j-1) = '\0';

   /*====================*/
   /* Return the string. */
   /*====================*/

   rv = AddSymbol(ret_str);
   rm(ret_str,strsize);
   return(rv);
  }

/****************************************/
/* SubseqFunction: CLIPS access routine */
/*   for the subseq$ function.          */
/****************************************/
globle VOID SubseqFunction(sub_value)
  DATA_OBJECT_PTR sub_value;
  {
   DATA_OBJECT value;
   struct multifield *theList;
   long offset, start, end, length; /* 6.04 Bug Fix */

   /*===================================*/
   /* Get the segment to be subdivided. */
   /*===================================*/
  
   if (ArgTypeCheck("subseq$",1,MULTIFIELD,&value) == CLIPS_FALSE)
     {
      SetMultifieldErrorValue(sub_value);
      return;
     }
   theList = (struct multifield *) DOToPointer(value);
   offset = GetDOBegin(value);
   
   /*=============================================*/
   /* Get range arguments. If they are not within */
   /* appropriate ranges, return a null segment.  */
   /*=============================================*/
   
   if (ArgTypeCheck("subseq$",2,INTEGER,&value) == CLIPS_FALSE)
     {
      SetMultifieldErrorValue(sub_value);
      return;
     }
   start = DOToInteger(value);

   if (ArgTypeCheck("subseq$",3,INTEGER,&value) == CLIPS_FALSE)
     {
      SetMultifieldErrorValue(sub_value);
      return;
     }
   end = DOToInteger(value);

   if ((end < 1) || (end < start))
     {
      SetMultifieldErrorValue(sub_value);
      return;
     }

   /*===================================================*/
   /* Adjust lengths  to conform to segment boundaries. */
   /*===================================================*/

   length = GetMFLength(theList);
   if (start > length)
     {
      SetMultifieldErrorValue(sub_value);
      return;
     }
   if (end > length) end = length;
   if (start < 1) start = 1;

   /*=========================*/
   /* Return the new segment. */
   /*=========================*/

   SetpType(sub_value,MULTIFIELD);
   SetpValue(sub_value,theList);
   SetpDOEnd(sub_value,offset + end - 1);
   SetpDOBegin(sub_value,offset + start - 1);
  }

/******************************************/
/* MVSubseqFunction: CLIPS access routine */
/*   for the mv-subseq function.          */
/******************************************/
globle VOID MVSubseqFunction(sub_value)
  DATA_OBJECT_PTR sub_value;
  {
   DATA_OBJECT value;
   struct multifield *theList;
   long offset, start, end, length; /* 6.04 Bug Fix */
   
   /*=============================================*/
   /* Get range arguments. If they are not within */
   /* appropriate ranges, return a null segment.  */
   /*=============================================*/
   
   if (ArgTypeCheck("mv-subseq",1,INTEGER,&value) == CLIPS_FALSE)
     {
      SetMultifieldErrorValue(sub_value);
      return;
     }
   start = DOToInteger(value);

   if (ArgTypeCheck("mv-subseq",2,INTEGER,&value) == CLIPS_FALSE)
     {
      SetMultifieldErrorValue(sub_value);
      return;
     }
   end = DOToInteger(value);

   if ((end < 1) || (end < start))
     {
      SetMultifieldErrorValue(sub_value);
      return;
     }

   /*===================================*/
   /* Get the segment to be subdivided. */
   /*===================================*/
  
   if (ArgTypeCheck("mv-subseq",3,MULTIFIELD,&value) == CLIPS_FALSE)
     {
      SetMultifieldErrorValue(sub_value);
      return;
     }
   theList = (struct multifield *) DOToPointer(value);
   offset = GetDOBegin(value);
   
   /*===================================================*/
   /* Adjust lengths  to conform to segment boundaries. */
   /*===================================================*/

   length = GetMFLength(theList);
   if (start > length)
     {
      SetMultifieldErrorValue(sub_value);
      return;
     }
   if (end > length) end = length;
   if (start < 1) start = 1;

   /*=========================*/
   /* Return the new segment. */
   /*=========================*/

   SetpType(sub_value,MULTIFIELD);
   SetpValue(sub_value,theList);
   SetpDOEnd(sub_value,offset + end - 1);
   SetpDOBegin(sub_value,offset + start - 1);
  }
  
/***************************************/
/* FirstFunction: CLIPS access routine */
/*   for the first$ function.          */
/***************************************/
globle VOID FirstFunction(returnValue)
  DATA_OBJECT_PTR returnValue;
  {
   DATA_OBJECT theValue;
   struct multifield *theList;

   /*===================================*/
   /* Get the segment to be subdivided. */
   /*===================================*/
   
   if (ArgTypeCheck("first$",1,MULTIFIELD,&theValue) == CLIPS_FALSE)
     {
      SetMultifieldErrorValue(returnValue);
      return;
     }
     
   theList = (struct multifield *) DOToPointer(theValue);

   /*=========================*/
   /* Return the new segment. */
   /*=========================*/

   SetpType(returnValue,MULTIFIELD);
   SetpValue(returnValue,theList);
   if (GetDOEnd(theValue) >= GetDOBegin(theValue))
     { SetpDOEnd(returnValue,GetDOBegin(theValue)); }
   else
     { SetpDOEnd(returnValue,GetDOEnd(theValue)); }
   SetpDOBegin(returnValue,GetDOBegin(theValue));
  }
  
/**************************************/
/* RestFunction: CLIPS access routine */
/*   for the rest$ function.          */
/**************************************/
globle VOID RestFunction(returnValue)
  DATA_OBJECT_PTR returnValue;
  {
   DATA_OBJECT theValue;
   struct multifield *theList;

   /*===================================*/
   /* Get the segment to be subdivided. */
   /*===================================*/
   
   if (ArgTypeCheck("rest$",1,MULTIFIELD,&theValue) == CLIPS_FALSE)
     {
      SetMultifieldErrorValue(returnValue);
      return;
     }
     
   theList = (struct multifield *) DOToPointer(theValue);

   /*=========================*/
   /* Return the new segment. */
   /*=========================*/

   SetpType(returnValue,MULTIFIELD);
   SetpValue(returnValue,theList);
   if (GetDOBegin(theValue) > GetDOEnd(theValue))
     { SetpDOBegin(returnValue,GetDOBegin(theValue)); }
   else
     { SetpDOBegin(returnValue,GetDOBegin(theValue) + 1); }
   SetpDOEnd(returnValue,GetDOEnd(theValue));
  }
  
/*************************************/
/* NthFunction: CLIPS access routine */
/*   for the nth$ function.          */
/*************************************/
globle VOID NthFunction(nth_value)
  DATA_OBJECT_PTR nth_value;
  {
   DATA_OBJECT value1, value2;
   struct multifield *elm_ptr;
   long n; /* 6.04 Bug Fix */

   if (ArgCountCheck("nth$",EXACTLY,2) == -1)
     {
      SetpType(nth_value,SYMBOL);
      SetpValue(nth_value,(VOID *) AddSymbol("nil"));
      return;
     }

   if ((ArgTypeCheck("nth$",1,INTEGER,&value1) == CLIPS_FALSE) ||
       (ArgTypeCheck("nth$",2,MULTIFIELD,&value2) == CLIPS_FALSE))
     {
      SetpType(nth_value,SYMBOL);
      SetpValue(nth_value,(VOID *) AddSymbol("nil"));
      return;
     }

   n = DOToLong(value1); /* 6.04 Bug Fix */
   if ((n > GetDOLength(value2)) || (n < 1))
     {
      SetpType(nth_value,SYMBOL);
      SetpValue(nth_value,(VOID *) AddSymbol("nil"));
      return;
     }

   elm_ptr = (struct multifield *) GetValue(value2);
   SetpType(nth_value,GetMFType(elm_ptr,n + GetDOBegin(value2) - 1));
   SetpValue(nth_value,GetMFValue(elm_ptr,n + GetDOBegin(value2) - 1));
  }

/* ------------------------------------------------------------------
 *    SubsetFunction:
 *               This function compares two multi-field variables
 *               to see if the first is a subset of the second. It
 *               does not consider order.
 *
 *    INPUTS:    Two arguments via CLIPS stack. First is the sublist
 *               multi-field variable, the second is the list to be
 *               compared to. Both should be of type MULTIFIELD.
 *
 *    OUTPUTS:   One floating point number, 1.0 if the first list
 *               is a subset of the second, else 0.0 if it is not.
 *
 *    NOTES:     This function is called from CLIPS with the subset
 *               command. Repeated values in the sublist must also
 *               be repeated in the main list.
 * ------------------------------------------------------------------
 */

globle BOOLEAN SubsetpFunction()
  {
   DATA_OBJECT item1, item2;
   long i; /* 6.04 Bug Fix */

   if (ArgCountCheck("subsetp",EXACTLY,2) == -1)
     return(CLIPS_FALSE);

   if (ArgTypeCheck("subsetp",1,MULTIFIELD,&item1) == CLIPS_FALSE)
     return(CLIPS_FALSE);

   if (ArgTypeCheck("subsetp",2,MULTIFIELD,&item2) == CLIPS_FALSE)
     return(CLIPS_FALSE);

   for (i = GetDOBegin(item1) ; i <= GetDOEnd(item1) ; i++)
     {
      if (FindItemInSegment(GetMFType((struct multifield *) GetValue(item1),i),
                            GetMFValue((struct multifield *) GetValue(item1),i),&item2) == 0)
        { return(CLIPS_FALSE); }
     }

   return(CLIPS_TRUE);
  }

/****************************************/
/* MemberFunction: CLIPS access routine */
/*   for the member$ function.          */
/****************************************/
globle VOID MemberFunction(result)
  DATA_OBJECT_PTR result;
  {
   DATA_OBJECT item1, item2;
   int pos;

   result->type = SYMBOL;
   result->value = CLIPSFalseSymbol;

   if (ArgCountCheck("member$",EXACTLY,2) == -1) return;

   RtnUnknown(1,&item1);
   if ((GetType(item1) != SYMBOL) &&
       (GetType(item1) != STRING) &&
       (GetType(item1) != INTEGER) &&
       (GetType(item1) != EXTERNAL_ADDRESS) &&
#if OBJECT_SYSTEM
       (GetType(item1) != INSTANCE_NAME) &&
       (GetType(item1) != INSTANCE_ADDRESS) &&
#endif
       (GetType(item1) != FLOAT))
     {
      ExpectedTypeError1("member$",1,"primitive data type");
      SetEvaluationError(CLIPS_TRUE);
      return;
     }

   if (ArgTypeCheck("member$",2,MULTIFIELD,&item2) == CLIPS_FALSE) return;

   pos = FindItemInSegment(item1.type,item1.value,&item2);

   if (pos != 0L)
     {
      result->type = INTEGER;
      result->value = (VOID *) AddLong((long) pos);
     }
  }

/***************************************/
/* FindItemInSegment:                  */
/***************************************/
static int FindItemInSegment(searchType,searchValue,value)
  int searchType;
  VOID *searchValue;
  DATA_OBJECT_PTR value;   /* changed 03-11-96 */
  {
   long mul_length, i; /* 6.04 Bug Fix */

   mul_length = GetpDOLength(value);
   for (i = 0 ; i < mul_length ; i++)
     {
      if ((searchValue == GetMFValue((struct multifield *) GetpValue(value),i + GetpDOBegin(value))) &&
          (searchType == GetMFType((struct multifield *) GetpValue(value),i + GetpDOBegin(value))))
        return(i+1);
     }

   return(CLIPS_FALSE);
  }

#if (! BLOAD_ONLY) && (! RUN_TIME)

/******************************************************/
/* MultifieldPrognParser: Parses the progn$ function. */
/******************************************************/
static struct expr *MultifieldPrognParser(top,infile)
  struct expr *top;
  char *infile;
  {
   struct BindInfo *oldBindList,*newBindList,*prev;
   struct token tkn;
   struct expr *tmp;
   SYMBOL_HN *fieldVar = NULL;
   
   SavePPBuffer(" ");
   GetToken(infile,&tkn);
   
   /* ================================
      Simple form: progn$ <mf-exp> ...
      ================================ */
   if (tkn.type != LPAREN)
     {
      top->argList = ParseAtomOrExpression(infile,&tkn);
      if (top->argList == NULL)
        {
         ReturnExpression(top);
         return(NULL);
        }
     }
   else
     {
      GetToken(infile,&tkn);
      if (tkn.type != SF_VARIABLE)
        {
         if (tkn.type != SYMBOL)
           goto MvPrognParseError;
         top->argList = Function2Parse(infile,ValueToString(tkn.value));
         if (top->argList == NULL)
           {
            ReturnExpression(top);
            return(NULL);
           }
        }

      /* =========================================
         Complex form: progn$ (<var> <mf-exp>) ...
         ========================================= */
      else
        {
         fieldVar = (SYMBOL_HN *) tkn.value;
         SavePPBuffer(" ");
         top->argList = ParseAtomOrExpression(infile,NULL);
         if (top->argList == NULL)
           {
            ReturnExpression(top);
            return(NULL);
           }
         GetToken(infile,&tkn);
         if (tkn.type != RPAREN)
           goto MvPrognParseError;
         PPBackup();
         /* PPBackup(); */
         SavePPBuffer(tkn.printForm);
         SavePPBuffer(" ");
        }
     }

   if (CheckArgumentAgainstRestriction(top->argList,(int) 'm'))
     goto MvPrognParseError;
   oldBindList = GetParsedBindNames();
   SetParsedBindNames(NULL);
   IncrementIndentDepth(3);
   BreakContext = CLIPS_TRUE;
   ReturnContext = svContexts->rtn;
   PPCRAndIndent();
   top->argList->nextArg = GroupActions(infile,&tkn,CLIPS_TRUE,NULL);
   DecrementIndentDepth(3);
   PPBackup();
   PPBackup();
   SavePPBuffer(tkn.printForm);
   if (top->argList->nextArg == NULL)
     {
      SetParsedBindNames(oldBindList);
      ReturnExpression(top);
      return(NULL);
     }
   tmp = top->argList->nextArg;
   top->argList->nextArg = tmp->argList;
   tmp->argList = NULL;
   ReturnExpression(tmp);
   newBindList = GetParsedBindNames();
   prev = NULL;
   while (newBindList != NULL)
     {
      if ((fieldVar == NULL) ? CLIPS_FALSE :
          (strcmp(ValueToString(newBindList->name),ValueToString(fieldVar)) == 0))
        {
         ClearParsedBindNames();
         SetParsedBindNames(oldBindList);
         PrintErrorID("MULTIFUN",2,CLIPS_FALSE);
         PrintCLIPS(WERROR,"Cannot rebind field variable in function progn$.\n");
         ReturnExpression(top);
         return(NULL);
        }
      prev = newBindList;
      newBindList = newBindList->next;
     }
   if (prev == NULL)
     SetParsedBindNames(oldBindList);
   else
     prev->next = oldBindList;
   if (fieldVar != NULL)
     ReplaceMvPrognFieldVars(fieldVar,top->argList->nextArg,0);
   return(top);
   
MvPrognParseError:
   SyntaxErrorMessage("progn$");
   ReturnExpression(top);
   return(NULL);
  }

/**********************************************/
/* ReplaceMvPrognFieldVars: Replaces variable */
/*   references found in the progn$ function. */
/**********************************************/
static VOID ReplaceMvPrognFieldVars(fieldVar,exp,depth)
  SYMBOL_HN *fieldVar;
  struct expr *exp;
  int depth;
  {
   int flen;
   
   flen = strlen(ValueToString(fieldVar));
   while (exp != NULL)
     {
      if ((exp->type != SF_VARIABLE) ? CLIPS_FALSE :
          (strncmp(ValueToString(exp->value),ValueToString(fieldVar),
                   (CLIPS_STD_SIZE) flen) == 0))
        {
         if (ValueToString(exp->value)[flen] == '\0')
           {
            exp->type = FCALL;
            exp->value = (VOID *) FindFunction((VOID *) "(get-progn$-field)");
            exp->argList = GenConstant(INTEGER,AddLong((long) depth));
           }
         else if (strcmp(ValueToString(exp->value) + flen,"-index") == 0)
           {
            exp->type = FCALL;
            exp->value = (VOID *) FindFunction((VOID *) "(get-progn$-index)");
            exp->argList = GenConstant(INTEGER,AddLong((long) depth));
           }
        }
      else if (exp->argList != NULL)
        {
         if ((exp->type == FCALL) && (exp->value == (VOID *) FindFunction("progn$")))
           ReplaceMvPrognFieldVars(fieldVar,exp->argList,depth+1);
         else
           ReplaceMvPrognFieldVars(fieldVar,exp->argList,depth);
        }
      exp = exp->nextArg;
     }
  }

#endif

/*****************************************/
/* MultifieldPrognFunction: CLIPS access */
/*   routine for the progn$ function.    */
/*****************************************/
globle VOID MultifieldPrognFunction(result)
  DATA_OBJECT_PTR result;
  {
   EXPRESSION *exp;
   DATA_OBJECT argval;
   long i, end; /* 6.04 Bug Fix */
   FIELD_VAR_STACK *tmpField;
   
   tmpField = get_struct(fieldVarStack);
   tmpField->type = SYMBOL;
   tmpField->value = CLIPSFalseSymbol;
   tmpField->nxt = FieldVarStack;
   FieldVarStack = tmpField;
   result->type = SYMBOL;
   result->value = CLIPSFalseSymbol;
   if (ArgTypeCheck("progn$",1,MULTIFIELD,&argval) == CLIPS_FALSE)
     {
      FieldVarStack = tmpField->nxt;
      rtn_struct(fieldVarStack,tmpField);
      return;
     }
   end = GetDOEnd(argval);
   for (i = GetDOBegin(argval) ; i <= end ; i++)
     {
      tmpField->type = GetMFType(argval.value,i);
      tmpField->value = GetMFValue(argval.value,i);
      tmpField->index = i;
      for (exp = GetFirstArgument()->nextArg ; exp != NULL ; exp = exp->nextArg)
        {
         EvaluateExpression(exp,result);
         if (HaltExecution || BreakFlag || ReturnFlag)
           {
            BreakFlag = CLIPS_FALSE;
            if (HaltExecution)
              {
               result->type = SYMBOL;
               result->value = CLIPSFalseSymbol;
              }
            FieldVarStack = tmpField->nxt;
            rtn_struct(fieldVarStack,tmpField);
            return;
           }
        }
     }
   BreakFlag = CLIPS_FALSE;
   FieldVarStack = tmpField->nxt;
   rtn_struct(fieldVarStack,tmpField);
  }

/***************************************************/
/* GetMvPrognField                                 */
/***************************************************/
globle VOID GetMvPrognField(result)
  DATA_OBJECT_PTR result;
  {
   int depth;
   FIELD_VAR_STACK *tmpField;
   
   depth = ValueToInteger(GetFirstArgument()->value);
   tmpField = FieldVarStack;
   while (depth > 0)
     {
      tmpField = tmpField->nxt;
      depth--;
     }
   result->type = tmpField->type;
   result->value = tmpField->value;
  }

/***************************************************/
/* GetMvPrognField                                 */
/***************************************************/
globle long GetMvPrognIndex()
  {
   int depth;
   FIELD_VAR_STACK *tmpField;
   
   depth = ValueToInteger(GetFirstArgument()->value);
   tmpField = FieldVarStack;
   while (depth > 0)
     {
      tmpField = tmpField->nxt;
      depth--;
     }
   return(tmpField->index);
  }

#endif

#if OBJECT_SYSTEM || MULTIFIELD_FUNCTIONS

/**************************************************************************
  NAME         : ReplaceMultiValueField
  DESCRIPTION  : Performs a replace on the src multi-field value
                   storing the results in the dst multi-field value
  INPUTS       : 1) The destination value buffer
                 2) The source value (can be NULL)
                 3) Beginning of index range
                 4) End of range
                 5) The new field value
  RETURNS      : CLIPS_TRUE if successful, CLIPS_FALSE otherwise
  SIDE EFFECTS : Allocates and sets a ephemeral segment (even if new
                   number of fields is 0)
                 Src value segment is not changed
  NOTES        : index is NOT guaranteed to be valid
                 src is guaranteed to be a multi-field variable or NULL
 **************************************************************************/
globle int ReplaceMultiValueField(dst,src,rb,re,field,funcName)
  DATA_OBJECT *dst,*src,*field;
  long rb,re;
  char *funcName;
  {
   long i,j,k; 
   struct field *deptr;
   struct field *septr;
   long srclen,dstlen;  

   srclen = (src != NULL) ? (src->end - src->begin + 1) : 0;
   if ((re < rb) ||
       (rb < 1) || (re < 1) ||
       (rb > srclen) || (re > srclen))
     {
      MVRangeError(rb,re,srclen,funcName);
      return(CLIPS_FALSE);
     }
   rb = src->begin + rb - 1;
   re = src->begin + re - 1;
   if (field->type == MULTIFIELD)
     dstlen = srclen + GetpDOLength(field) - (re-rb+1);
   else
     dstlen = srclen + 1 - (re-rb+1);
   dst->type = MULTIFIELD;
   dst->begin = 0;
   dst->value = CreateMultifield(dstlen);
   dst->end = dstlen-1;
   for (i = 0 , j = src->begin ; j < rb ; i++ , j++)
     {
      deptr = &((struct multifield *) dst->value)->theFields[i];
      septr = &((struct multifield *) src->value)->theFields[j];
      deptr->type = septr->type;
      deptr->value = septr->value;
     }
   if (field->type != MULTIFIELD)
     {
      deptr = &((struct multifield *) dst->value)->theFields[i++];
      deptr->type = (short) field->type;
      deptr->value = field->value;
     }
   else
     {
      for (k = field->begin ; k <= field->end ; k++ , i++)
        {
         deptr = &((struct multifield *) dst->value)->theFields[i];
         septr = &((struct multifield *) field->value)->theFields[k];
         deptr->type = septr->type;
         deptr->value = septr->value;
        }
     }
   while (j < re)
     j++;
   for (j++ ; i < dstlen ; i++ , j++)
     {
      deptr = &((struct multifield *) dst->value)->theFields[i];
      septr = &((struct multifield *) src->value)->theFields[j];
      deptr->type = septr->type;
      deptr->value = septr->value;
     }
   return(CLIPS_TRUE);
  }

/**************************************************************************
  NAME         : InsertMultiValueField
  DESCRIPTION  : Performs an insert on the src multi-field value
                   storing the results in the dst multi-field value
  INPUTS       : 1) The destination value buffer
                 2) The source value (can be NULL)
                 3) The index for the change
                 4) The new field value
  RETURNS      : CLIPS_TRUE if successful, CLIPS_FALSE otherwise
  SIDE EFFECTS : Allocates and sets a ephemeral segment (even if new
                   number of fields is 0)
                 Src value segment is not changed
  NOTES        : index is NOT guaranteed to be valid
                 src is guaranteed to be a multi-field variable or NULL
 **************************************************************************/
globle int InsertMultiValueField(dst,src,index,field,funcName)
  DATA_OBJECT *dst,*src,*field;
  long index;   /* changed 03-11-96 */
  char *funcName;
  {
   register long i,j,k;   /* changed 03-11-96 */
   register FIELD *deptr, *septr;
   long srclen,dstlen; /* 6.04 Bug Fix */

   srclen = (src != NULL) ? (src->end - src->begin + 1) : 0;
   if (index < 1)
     {
      MVRangeError(index,index,srclen+1,funcName);
      return(CLIPS_FALSE);
     }
   if (index > (srclen + 1))
     index = srclen + 1;
   dst->type = MULTIFIELD;
   dst->begin = 0;
   if (src == NULL)
     {
      if (field->type == MULTIFIELD)
        {
         DuplicateMultifield(dst,field);
         AddToMultifieldList((struct multifield *) dst->value);
        }
      else
        {
         dst->value = CreateMultifield(0L);
         dst->end = 0;
         deptr = &((struct multifield *) dst->value)->theFields[0];
         deptr->type = (short) field->type;
         deptr->value = field->value;
        }
      return(CLIPS_TRUE);
     }
   dstlen = (field->type == MULTIFIELD) ? GetpDOLength(field) + srclen : srclen + 1;
   dst->value = CreateMultifield(dstlen);
   dst->end = dstlen-1;
   index--;
   for (i = 0 , j = src->begin ; j < index ; i++ , j++)
     {
      deptr = &((struct multifield *) dst->value)->theFields[i];
      septr = &((struct multifield *) src->value)->theFields[j];
      deptr->type = septr->type;
      deptr->value = septr->value;
     }
   if (field->type != MULTIFIELD)
     {
      deptr = &((struct multifield *) dst->value)->theFields[index];
      deptr->type = (short) field->type;
      deptr->value = field->value;
      i++;
     }
   else
     {
      for (k = field->begin ; k <= field->end ; k++ , i++)
        {
         deptr = &((struct multifield *) dst->value)->theFields[i];
         septr = &((struct multifield *) field->value)->theFields[k];
         deptr->type = septr->type;
         deptr->value = septr->value;
        }
     }
   for ( ; j <= src->end ; i++ , j++)
     {
      deptr = &((struct multifield *) dst->value)->theFields[i];
      septr = &((struct multifield *) src->value)->theFields[j];
      deptr->type = septr->type;
      deptr->value = septr->value;
     }
   return(CLIPS_TRUE);
  }

/*******************************************************
  NAME         : MVRangeError
  DESCRIPTION  : Prints out an error messages for index
                   out-of-range errors in multi-field
                   access functions
  INPUTS       : 1) The bad range start
                 2) The bad range end
                 3) The max end of the range (min is
                     assumed to be 1)
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 ******************************************************/
static VOID MVRangeError(brb,bre,max,funcName)
  long brb,bre,max;
  char *funcName;
  {
   PrintErrorID("MULTIFUN",1,CLIPS_FALSE);
   PrintCLIPS(WERROR,"Multifield index ");
   if (brb == bre)
     PrintLongInteger(WERROR,(long) brb);
   else
     {
      PrintCLIPS(WERROR,"range ");
      PrintLongInteger(WERROR,(long) brb);
      PrintCLIPS(WERROR,"..");
      PrintLongInteger(WERROR,(long) bre);
     }
   PrintCLIPS(WERROR," out of range 1..");
   PrintLongInteger(WERROR,(long) max);
   if (funcName != NULL)
     {
      PrintCLIPS(WERROR," in function ");
      PrintCLIPS(WERROR,funcName);
     }
   PrintCLIPS(WERROR,".\n");
  }

/**************************************************************************
  NAME         : DeleteMultiValueField
  DESCRIPTION  : Performs a modify on the src multi-field value
                   storing the results in the dst multi-field value
  INPUTS       : 1) The destination value buffer
                 2) The source value (can be NULL)
                 3) The beginning index for deletion
                 4) The ending index for deletion
  RETURNS      : CLIPS_TRUE if successful, CLIPS_FALSE otherwise
  SIDE EFFECTS : Allocates and sets a ephemeral segment (even if new
                   number of fields is 0)
                 Src value segment is not changed
  NOTES        : index is NOT guaranteed to be valid
                 src is guaranteed to be a multi-field variable or NULL
 **************************************************************************/
globle int DeleteMultiValueField(dst,src,rb,re,funcName)
  DATA_OBJECT *dst,*src;
  long rb,re;
  char *funcName;
  {
   register long i,j;
   register FIELD_PTR deptr,septr;
   long srclen, dstlen;

   srclen = (src != NULL) ? (src->end - src->begin + 1) : 0;
   if ((re < rb) ||
       (rb < 1) || (re < 1) ||
       (rb > srclen) || (re > srclen))
     {
      MVRangeError(rb,re,srclen,funcName);
      return(CLIPS_FALSE);
     }
   dst->type = MULTIFIELD;
   dst->begin = 0;
   if (srclen == 0)
    {
     dst->value = CreateMultifield(0L);
     dst->end = -1;
     return(CLIPS_TRUE);
    }
   rb = src->begin + rb -1;
   re = src->begin + re -1;
   dstlen = srclen-(re-rb+1);
   dst->end = dstlen-1;
   dst->value = CreateMultifield(dstlen);
   for (i = 0 , j = src->begin ; j < rb ; i++ , j++)
     {
      deptr = &((struct multifield *) dst->value)->theFields[i];
      septr = &((struct multifield *) src->value)->theFields[j];
      deptr->type = septr->type;
      deptr->value = septr->value;
     }
   while (j < re)
     j++;
   for (j++ ; i <= dst->end ; j++ , i++)
     {
      deptr = &((struct multifield *) dst->value)->theFields[i];
      septr = &((struct multifield *) src->value)->theFields[j];
      deptr->type = septr->type;
      deptr->value = septr->value;
     }
   return(CLIPS_TRUE);
  }

#endif


