   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  05/12/93            */
   /*                                                     */
   /*                  MULTIFIELD MODULE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
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

#define _MULTIFLD_SOURCE_

#include <stdio.h>
#define _CLIPS_STDIO_

#include "setup.h"

#include "constant.h"
#include "clipsmem.h"
#include "evaluatn.h"
#include "scanner.h"
#include "router.h"
#include "strngrtr.h"
#include "utility.h"
#include "multifld.h"

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static struct multifield  *ListOfMultifields = NULL;
  
/***********************************************************/
/* CreateMultifield2:       */
/***********************************************************/
globle VOID *CreateMultifield2(size)
  long size; /* 6.04 Bug Fix */
  {
   struct multifield *theSegment;
   long newSize = size;  /* 6.04 Bug Fix */

   if (size <= 0) newSize = 1;
   
   theSegment = get_var_struct2(multifield,(long) sizeof(struct field) * (newSize - 1L));   
   
   theSegment->multifieldLength = size;
   theSegment->depth = (short) CurrentEvaluationDepth;
   theSegment->busyCount = 0;
   theSegment->next = NULL;

   return((VOID *) theSegment);
  }

/*****************************************************************/
/* ReturnMultifield:                                             */
/*****************************************************************/
globle VOID ReturnMultifield(theSegment)
  struct multifield *theSegment;
  {
   long newSize; /* 6.04 Bug Fix */
   
   if (theSegment == NULL) return;
   
   if (theSegment->multifieldLength == 0) newSize = 1;
   else newSize = theSegment->multifieldLength;
   
   rtn_var_struct2(multifield,sizeof(struct field) * (newSize - 1),theSegment);
  }

/******************************/
/* MultifieldInstall:            */
/******************************/
globle VOID MultifieldInstall(theSegment)
  struct multifield *theSegment;
  {
   long length, i; /* 6.04 Bug Fix */
   struct field HUGE_ADDR *theFields;

   if (theSegment == NULL) return;
   
   length = theSegment->multifieldLength;
   
   theSegment->busyCount++;
   theFields = theSegment->theFields;

   for (i = 0 ; i < length ; i++)
     { AtomInstall(theFields[i].type,theFields[i].value); }
  }

/******************************/
/* MultifieldDeinstall:       */
/******************************/
globle VOID MultifieldDeinstall(theSegment)
  struct multifield *theSegment;
  {
   long length, i; /* 6.04 Bug Fix */
   struct field HUGE_ADDR *theFields;

   if (theSegment == NULL) return;
   
   length = theSegment->multifieldLength;
   theSegment->busyCount--;
   theFields = theSegment->theFields;

   for (i = 0 ; i < length ; i++)
     { AtomDeinstall(theFields[i].type,theFields[i].value); }
  }

/*******************************************************/
/* StringToMultifield:  Returns a multifield structure */
/*    that represents the string sent as the argument. */
/*******************************************************/
globle struct multifield *StringToMultifield(theString)
  char *theString;
  {
   struct token theToken;
   struct multifield *theSegment;
   struct field HUGE_ADDR *theFields;
   long numberOfFields = 0; /* 6.04 Bug Fix */
   struct expr *topAtom = NULL, *lastAtom = NULL, *theAtom;

   /*====================================================*/
   /* Open the string as an input source and read in the */
   /* list of values to be stored in the multifield.     */
   /*====================================================*/
   
   OpenStringSource("multifield-str",theString,0);

   GetToken("multifield-str",&theToken);
   while (theToken.type != STOP)
     {
      if ((theToken.type == SYMBOL) || (theToken.type == STRING) ||
          (theToken.type == FLOAT) || (theToken.type == INTEGER) ||
          (theToken.type == INSTANCE_NAME))
        { theAtom = GenConstant(theToken.type,theToken.value); }
      else
        { theAtom = GenConstant(STRING,AddSymbol(theToken.printForm)); }

      numberOfFields++;
      if (topAtom == NULL) topAtom = theAtom;
      else lastAtom->nextArg = theAtom;

      lastAtom = theAtom;
      GetToken("multifield-str",&theToken);
     }

   CloseStringSource("multifield-str");

   /*====================================================================*/
   /* Create a multifield of the appropriate size for the values parsed. */
   /*====================================================================*/
   
   theSegment = (struct multifield *) CreateMultifield(numberOfFields);
   theFields = theSegment->theFields;

   /*====================================*/
   /* Copy the values to the multifield. */
   /*====================================*/
   
   theAtom = topAtom;
   numberOfFields = 0;
   while (theAtom != NULL)
     {
      theFields[numberOfFields].type = theAtom->type;
      theFields[numberOfFields].value = theAtom->value;
      numberOfFields++;
      theAtom = theAtom->nextArg;
     }

   /*===========================*/
   /* Return the parsed values. */
   /*===========================*/
   
   ReturnExpression(topAtom);

   /*============================*/
   /* Return the new multifield. */
   /*============================*/
   
   return(theSegment);
  }

/***********************************************************/
/* CreateMultifield: Creates a multifield of the specified */
/*   size and adds it to the list of segments.             */
/***********************************************************/
globle VOID *CreateMultifield(size)
  long size; /* 6.04 Bug Fix */
  {
   struct multifield *theSegment;
   long newSize; /* 6.04 Bug Fix */

   if (size <= 0) newSize = 1;
   else newSize = size;

   theSegment = get_var_struct2(multifield,(long) sizeof(struct field) * (newSize - 1L));   
   
   theSegment->multifieldLength = size;
   theSegment->depth = (short) CurrentEvaluationDepth;
   theSegment->busyCount = 0;
   theSegment->next = NULL;
     
   theSegment->next = ListOfMultifields;
   ListOfMultifields = theSegment;

   EphemeralItemCount++;
   EphemeralItemSize += sizeof(struct multifield) + (sizeof(struct field) * newSize);

   return((VOID *) theSegment);
  }
  
/*********************************************************************/
/* DOToMultifield:    */
/*********************************************************************/
globle VOID *DOToMultifield(theValue)
  DATA_OBJECT *theValue;
  {
   struct multifield *dst, *src;
   
   if (theValue->type != MULTIFIELD) return(NULL);
   
   dst = (struct multifield *) CreateMultifield2(GetpDOLength(theValue));
   
   src = (struct multifield *) theValue->value;
   CopyMemory(struct field,dst->multifieldLength,
              &(dst->theFields[0]),&(src->theFields[GetpDOBegin(theValue) - 1]));

   return((VOID *) dst);
  }
  
/***********************************************************/
/* AddToMultifieldList:                                       */
/***********************************************************/
globle VOID AddToMultifieldList(theSegment)
  struct multifield *theSegment;
  {
   theSegment->depth = (short) CurrentEvaluationDepth;
   theSegment->next = ListOfMultifields;
   ListOfMultifields = theSegment;

   EphemeralItemCount++;
   EphemeralItemSize += sizeof(struct multifield) + (sizeof(struct field) * theSegment->multifieldLength);
  }

/***********************************************************/
/* FlushMultifields:                                         */
/***********************************************************/
globle VOID FlushMultifields()
  {
   struct multifield *theSegment, *nextPtr, *lastPtr = NULL;
   long newSize; /* 6.04 Bug Fix */

   theSegment = ListOfMultifields;
   while (theSegment != NULL)
     {
      nextPtr = theSegment->next;
      if ((theSegment->depth > CurrentEvaluationDepth) && (theSegment->busyCount == 0))
        {
         EphemeralItemCount--;
         EphemeralItemSize -= sizeof(struct multifield) + 
                              (sizeof(struct field) * theSegment->multifieldLength);
         if (theSegment->multifieldLength == 0) newSize = 1;
         else newSize = theSegment->multifieldLength;
         rtn_var_struct2(multifield,sizeof(struct field) * (newSize - 1),theSegment);
         if (lastPtr == NULL) ListOfMultifields = nextPtr;
         else lastPtr->next = nextPtr;
        }
      else
        { lastPtr = theSegment; }

      theSegment = nextPtr;
     }
  }

/*********************************************************************/
/* DuplicateMultifield: Allocates a new segment and copies results from */
/*                  old value to new - NOT put on ListOfMultifields!!   */
/*********************************************************************/
globle VOID DuplicateMultifield(dst,src)
  DATA_OBJECT_PTR dst, src;
  {
   dst->type = MULTIFIELD;
   dst->begin = 0;
   dst->end = src->end - src->begin;
   dst->value = (VOID *) CreateMultifield2(dst->end + 1);
   CopyMemory(struct field,dst->end + 1,&((struct multifield *) dst->value)->theFields[0],
                                        &((struct multifield *) src->value)->theFields[src->begin]);
  }
  
/*********************************************************************/
/* CopyMultifield:    */
/*********************************************************************/
globle VOID *CopyMultifield(src)
  struct multifield *src;
  {
   struct multifield *dst;
   
   dst = (struct multifield *) CreateMultifield2(src->multifieldLength);
   CopyMemory(struct field,src->multifieldLength,&(dst->theFields[0]),&(src->theFields[0]));
   return((VOID *) dst);
  }
  
/**********************************************************/
/* PrintMultifield: Prints out a multifield               */
/**********************************************************/
globle VOID PrintMultifield(fileid,segment,begin,end,printParens)
  char *fileid;
  struct multifield *segment;
  long begin, end;
  int printParens;
  {
   struct field HUGE_ADDR *theMultifield;
   int i;

   theMultifield = segment->theFields;
   if (printParens)
     PrintCLIPS(fileid,"(");
   i = begin;
   while (i <= end)
     {
      PrintAtom(fileid,theMultifield[i].type,theMultifield[i].value);
      i++;
      if (i <= end) PrintCLIPS(fileid," ");
     }
   if (printParens)
     PrintCLIPS(fileid,")");
  }

/*****************************************************/
/* StoreInMultifield:  Append function for segments. */
/*****************************************************/
globle VOID StoreInMultifield(returnValue,expptr,garbageSegment)
  DATA_OBJECT *returnValue;
  EXPRESSION *expptr;
  int garbageSegment;
  {
   DATA_OBJECT val_ptr;
   DATA_OBJECT HUGE_ADDR *val_arr;
   struct multifield HUGE_ADDR *theMultifield; 
   struct multifield *orig_ptr;
   long start, end, i,j, k, seg_size, argCount;

   argCount = CountArguments(expptr);

   /*=========================================*/
   /* If no arguments are given return a NULL */
   /* multifield of length zero.              */
   /*=========================================*/

   if (argCount == 0)
     {
      SetpType(returnValue,MULTIFIELD);
      SetpDOBegin(returnValue,1);
      SetpDOEnd(returnValue,0);
      if (garbageSegment) theMultifield = (struct multifield HUGE_ADDR *) CreateMultifield(0L);
      else theMultifield = (struct multifield HUGE_ADDR *) CreateMultifield2(0L);
      SetpValue(returnValue,(VOID *) theMultifield);
      return;
     }

   else
     {
      /*========================================*/
      /* Get a new segment with length equal to */
      /* the total length of all the arguments. */
      /*========================================*/

      val_arr = (DATA_OBJECT HUGE_ADDR *) gm3((long) sizeof(DATA_OBJECT) * argCount);
      seg_size = 0;
      for(i = 1 ; i <= argCount ; i++ , expptr = expptr->nextArg)
        {
         EvaluateExpression(expptr,&val_ptr);
         if (EvaluationError)
           {
            SetpType(returnValue,MULTIFIELD);
            SetpDOBegin(returnValue,1);
            SetpDOEnd(returnValue,0);
            if (garbageSegment) 
              { theMultifield = (struct multifield HUGE_ADDR *) CreateMultifield(0L); }
            else theMultifield = (struct multifield HUGE_ADDR *) CreateMultifield2(0L);
            SetpValue(returnValue,(VOID *) theMultifield);
            rm3(val_arr,(long) sizeof(DATA_OBJECT) * argCount);
            return;
           }
         SetpType(val_arr+i-1,GetType(val_ptr));
         if (GetType(val_ptr) == MULTIFIELD)
           {
            SetpValue(val_arr+i-1,GetpValue(&val_ptr));
            start = GetDOBegin(val_ptr);
            end = GetDOEnd(val_ptr);
           }
         else if (GetType(val_ptr) == RVOID)
           {
            SetpValue(val_arr+i-1,GetValue(val_ptr));
            start = 1;
            end = 0;
           }
         else
           {
            SetpValue(val_arr+i-1,GetValue(val_ptr));
            start = end = -1;
           }

         seg_size += end - start + 1;
         SetpDOBegin(val_arr+i-1,start);
         SetpDOEnd(val_arr+i-1,end);
        }
      
      if (garbageSegment) 
        { theMultifield = (struct multifield HUGE_ADDR *) CreateMultifield(seg_size); }
      else theMultifield = (struct multifield HUGE_ADDR *) CreateMultifield2(seg_size);
      
      /*========================================*/
      /* Copy each argument into new segment.  */
      /*========================================*/

      for(k=0,j=1; k < argCount;k++)
        {
         if (GetpType(val_arr+k) == MULTIFIELD)
           {
            start = GetpDOBegin(val_arr+k);
            end = GetpDOEnd(val_arr+k);
            orig_ptr = (struct multifield *) GetpValue(val_arr+k);
            for(i=start; i< end + 1; i++,j++)
              {
               SetMFType(theMultifield,j,(GetMFType(orig_ptr,i)));
               SetMFValue(theMultifield,j,(GetMFValue(orig_ptr,i)));
              }
           }
         else if (GetpType(val_arr+k) != MULTIFIELD)
           {
            SetMFType(theMultifield,j,(short) (GetpType(val_arr+k)));
            SetMFValue(theMultifield,j,(GetpValue(val_arr+k)));
            j++;
           }
        }

      /*=========================*/
      /* Return the new segment. */
      /*=========================*/
      
      SetpType(returnValue,MULTIFIELD);
      SetpDOBegin(returnValue,1);
      SetpDOEnd(returnValue,seg_size);
      SetpValue(returnValue,(VOID *) theMultifield);
      rm3(val_arr,(long) sizeof(DATA_OBJECT) * argCount);
      return;
     }
  }

/*************************************************************/
/* MultifieldDOsEqual: determines if two segments are equal. */
/*************************************************************/
globle BOOLEAN MultifieldDOsEqual(dobj1,dobj2)
  DATA_OBJECT_PTR dobj1,dobj2;
  {
   long extent1,extent2; /* 6.04 Bug Fix */
   FIELD_PTR e1,e2;
   
   extent1 = GetpDOLength(dobj1);
   extent2 = GetpDOLength(dobj2);
   if (extent1 != extent2)
     { return(CLIPS_FALSE); }

   e1 = (FIELD_PTR) GetMFPtr(GetpValue(dobj1),GetpDOBegin(dobj1));
   e2 = (FIELD_PTR) GetMFPtr(GetpValue(dobj2),GetpDOBegin(dobj2));
   while (extent1 != 0)
     {
      if (e1->type != e2->type)
        { return(CLIPS_FALSE); }

      if (e1->value != e2->value)
        { return(CLIPS_FALSE); }

      extent1--;

      if (extent1 > 0)
        {
         e1++;
         e2++;
        }
     }
   return(CLIPS_TRUE);
  }

/******************************************************************/
/* MultifieldsEqual: Determines if two multifields are identical. */
/******************************************************************/
globle int MultifieldsEqual(segment1,segment2)
  struct multifield *segment1, *segment2;
  {
   struct field HUGE_ADDR *elem1; 
   struct field HUGE_ADDR *elem2;
   long length, i = 0; /* 6.04 Bug Fix */

   length = segment1->multifieldLength;
   if (length != (int) segment2->multifieldLength)
     { return(CLIPS_FALSE); }

   elem1 = segment1->theFields;
   elem2 = segment2->theFields;

   /*==================================================*/
   /* Compare each field of both facts until the facts */
   /* match completely or the facts mismatch.          */
   /*==================================================*/

   while (i < length)
     {
      if (elem1[i].type != elem2[i].type)
        { return(CLIPS_FALSE); }

#if FUZZY_DEFTEMPLATES  /* added 03-11-96 */
      /* fuzzy value fields must only match on the type
         to be considered equal
      */
      if (elem1[i].type == FUZZY_VALUE)
        { i++; continue; }
#endif

      if (elem1[i].type == MULTIFIELD)
        {
         if (MultifieldsEqual(elem1[i].value,elem2[i].value) == CLIPS_FALSE)
          { return(CLIPS_FALSE); }
        }
      else if (elem1[i].value != elem2[i].value)
        { return(CLIPS_FALSE); }

      i++;
     }
   return(CLIPS_TRUE);
  }
