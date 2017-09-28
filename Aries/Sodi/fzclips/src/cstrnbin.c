   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  03/21/94            */
   /*                                                     */
   /*            CONSTRAINT BLOAD/BSAVE MODULE            */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the binary save/load feature for      */
/*    constraint records.                                    */
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

#define _CSTRNBIN_SOURCE_

#include "setup.h"

#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME)

#include "constant.h"
#include "clipsmem.h"
#include "router.h"
#include "bload.h"
  
#if BLOAD_AND_BSAVE
#include "bsave.h"
#endif

#include "cstrnbin.h"

/*******************/
/* DATA STRUCTURES */
/*******************/

struct bsaveConstraintRecord
  {      
   unsigned int anyAllowed : 1;
   unsigned int symbolsAllowed : 1;
   unsigned int stringsAllowed : 1;
   unsigned int floatsAllowed : 1;
   unsigned int integersAllowed : 1;
   unsigned int instanceNamesAllowed : 1;
   unsigned int instanceAddressesAllowed : 1;
   unsigned int externalAddressesAllowed : 1;
   unsigned int factAddressesAllowed : 1;
#if FUZZY_DEFTEMPLATES
   unsigned int fuzzyValuesAllowed : 1;
   unsigned int fuzzyValueRestriction : 1;
#endif
   unsigned int anyRestriction : 1;
   unsigned int symbolRestriction : 1;
   unsigned int stringRestriction : 1;
   unsigned int numberRestriction : 1;
   unsigned int floatRestriction : 1;
   unsigned int integerRestriction : 1;
   unsigned int instanceNameRestriction : 1;
   unsigned int multifieldsAllowed : 1;
   unsigned int singlefieldsAllowed : 1; 
   long restrictionList;
   long minValue;
   long maxValue; 
   long minFields;
   long maxFields;       
  };      
  
typedef struct bsaveConstraintRecord BSAVE_CONSTRAINT_RECORD;

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if ANSI_COMPILER
#if BLOAD_AND_BSAVE
   static VOID                    CopyToBsaveConstraintRecord(CONSTRAINT_RECORD *,BSAVE_CONSTRAINT_RECORD *);
#endif
   static VOID                    CopyFromBsaveConstraintRecord(VOID *,long);
#else
#if BLOAD_AND_BSAVE
   static VOID                    CopyToBsaveConstraintRecord();
#endif
   static VOID                    CopyFromBsaveConstraintRecord();
#endif

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   globle struct constraintRecord HUGE_ADDR * ConstraintArray;
   
/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static long int                 NumberOfConstraints;

#if BLOAD_AND_BSAVE

/**************************************************/
/* WriteNeededConstraints: Writes the constraints */
/*   in the constraint table to the binary image  */
/*   currently being saved.                       */
/**************************************************/
globle VOID WriteNeededConstraints(fp)
  FILE *fp;
  {
   int i;
   unsigned short theIndex = 0;
   unsigned long int numberOfUsedConstraints = 0;
   CONSTRAINT_RECORD *tmpPtr;
   BSAVE_CONSTRAINT_RECORD bsaveConstraints;
#if FUZZY_DEFTEMPLATES
   unsigned long int numberOfFuzzyValueConstraints = 0;
   int saveOnlyFuzzyValueConstraints = CLIPS_FALSE;
#endif

   /*================================*/
   /* Get the number of constraints. */
   /*================================*/

   for (i = 0; i < SIZE_CONSTRAINT_HASH; i++)
     {
      for (tmpPtr = ConstraintHashtable[i];
           tmpPtr != NULL;
           tmpPtr = tmpPtr->next)
        {
         tmpPtr->bsaveIndex = theIndex++;
         numberOfUsedConstraints++;
#if FUZZY_DEFTEMPLATES
         if (tmpPtr->fuzzyValuesAllowed)
            numberOfFuzzyValueConstraints++;
#endif
        }
     }
     
   /*=============================================*/
   /* If dynamic constraint checking is disabled, */
   /* then no constraints are saved.              */
   /*=============================================*/
   
   if ((! GetDynamicConstraintChecking()) && (numberOfUsedConstraints != 0))
     {
#if FUZZY_DEFTEMPLATES
      /* Fuzzy Value constraints MUST always be kept!! */
      numberOfUsedConstraints = numberOfFuzzyValueConstraints;
      saveOnlyFuzzyValueConstraints = CLIPS_TRUE;
      theIndex = 0;
      for (i = 0 ; i < SIZE_CONSTRAINT_HASH; i++)
        {
         tmpPtr = ConstraintHashtable[i];
         while (tmpPtr != NULL)
           {
            if (tmpPtr->fuzzyValuesAllowed)
               tmpPtr->bsaveIndex = theIndex++;
            else
               tmpPtr->bsaveIndex = -1L;
            tmpPtr = tmpPtr->next;
           }
        }
#else
      numberOfUsedConstraints = 0;
#endif
      PrintWarningID("CSTRNBIN",1,CLIPS_FALSE);
      PrintCLIPS(WWARNING,"Constraints are not saved with a binary image\n");
      PrintCLIPS(WWARNING,"  when dynamic constraint checking is disabled.\n");
#if FUZZY_DEFTEMPLATES
      PrintCLIPS(WWARNING,"  (except Fuzzy Value constraints are always saved)\n");
#endif
     }
     
   /*============================================*/
   /* Write out the number of constraints in the */
   /* constraint table followed by each of the   */
   /* constraints in the constraint table.       */
   /*============================================*/

   GenWrite(&numberOfUsedConstraints,(unsigned long) sizeof(unsigned long int),fp);
   if (numberOfUsedConstraints == 0) return;
   
   for (i = 0 ; i < SIZE_CONSTRAINT_HASH; i++)
     {
      for (tmpPtr = ConstraintHashtable[i];
           tmpPtr != NULL;
           tmpPtr = tmpPtr->next)
        {
#if FUZZY_DEFTEMPLATES
         if (!saveOnlyFuzzyValueConstraints ||
             (saveOnlyFuzzyValueConstraints && tmpPtr->fuzzyValuesAllowed)
            )
           {
#endif
         CopyToBsaveConstraintRecord(tmpPtr,&bsaveConstraints);
         GenWrite(&bsaveConstraints,
                  (unsigned long) sizeof(BSAVE_CONSTRAINT_RECORD),fp);
#if FUZZY_DEFTEMPLATES
           }
#endif
        }
     }
  }
  
/****************************************************/
/* CopyToBsaveConstraintRecord: Copies a constraint */
/*   record to the data structure used for storing  */
/*   constraints in a binary image.                 */
/****************************************************/
static VOID CopyToBsaveConstraintRecord(constraints,bsaveConstraints)
  CONSTRAINT_RECORD *constraints;
  BSAVE_CONSTRAINT_RECORD *bsaveConstraints;
  {   
   bsaveConstraints->anyAllowed = constraints->anyAllowed;
   bsaveConstraints->symbolsAllowed = constraints->symbolsAllowed;
   bsaveConstraints->stringsAllowed = constraints->stringsAllowed;
   bsaveConstraints->floatsAllowed = constraints->floatsAllowed;
   bsaveConstraints->integersAllowed = constraints->integersAllowed;
   bsaveConstraints->instanceNamesAllowed = constraints->instanceNamesAllowed;
   bsaveConstraints->instanceAddressesAllowed = constraints->instanceAddressesAllowed;
   bsaveConstraints->externalAddressesAllowed = constraints->externalAddressesAllowed;
   bsaveConstraints->multifieldsAllowed = constraints->multifieldsAllowed;
   bsaveConstraints->singlefieldsAllowed = constraints->singlefieldsAllowed; 
   bsaveConstraints->factAddressesAllowed = constraints->factAddressesAllowed;
#if FUZZY_DEFTEMPLATES
   bsaveConstraints->fuzzyValuesAllowed = constraints->fuzzyValuesAllowed;  
   bsaveConstraints->fuzzyValueRestriction = constraints->fuzzyValueRestriction;
#endif  
   bsaveConstraints->anyRestriction = constraints->anyRestriction;
   bsaveConstraints->symbolRestriction = constraints->symbolRestriction;
   bsaveConstraints->stringRestriction = constraints->stringRestriction;
   bsaveConstraints->floatRestriction = constraints->floatRestriction;
   bsaveConstraints->integerRestriction = constraints->integerRestriction;
   bsaveConstraints->instanceNameRestriction = constraints->instanceNameRestriction;
     
   bsaveConstraints->restrictionList = HashedExpressionIndex(constraints->restrictionList);
   bsaveConstraints->minValue = HashedExpressionIndex(constraints->minValue);
   bsaveConstraints->maxValue = HashedExpressionIndex(constraints->maxValue);
   bsaveConstraints->minFields = HashedExpressionIndex(constraints->minFields);
   bsaveConstraints->maxFields = HashedExpressionIndex(constraints->maxFields);
  }

#endif /* BLOAD_AND_BSAVE */
   
/********************************************************/
/* ReadNeededConstraints: Reads in the constraints used */
/*   by the binary image currently being loaded.        */
/********************************************************/
globle VOID ReadNeededConstraints()
  {
   GenRead((VOID *) &NumberOfConstraints,(unsigned long) 
                                         sizeof(unsigned long int));
   if (NumberOfConstraints == 0) return;

   ConstraintArray = (CONSTRAINT_RECORD HUGE_ADDR *)
           genlongalloc((unsigned long) (sizeof(CONSTRAINT_RECORD) * 
                                        NumberOfConstraints));

   BloadandRefresh(NumberOfConstraints,sizeof(BSAVE_CONSTRAINT_RECORD),
                   CopyFromBsaveConstraintRecord);
  }
  
/*****************************************************/
/* CopyFromBsaveConstraintRecord: Copies values to a */
/*   constraint record from the data structure used  */
/*   for storing constraints in a binary image.      */
/*****************************************************/
static VOID CopyFromBsaveConstraintRecord(buf,index)
  VOID *buf;
  long index; /* changed (ci) 03-05-96 */
  {
   BSAVE_CONSTRAINT_RECORD *bsaveConstraints;
   CONSTRAINT_RECORD *constraints;
   
   bsaveConstraints = (BSAVE_CONSTRAINT_RECORD *) buf;
   constraints = (CONSTRAINT_RECORD *) &ConstraintArray[index];
   
   constraints->anyAllowed = bsaveConstraints->anyAllowed;
   constraints->symbolsAllowed = bsaveConstraints->symbolsAllowed;
   constraints->stringsAllowed = bsaveConstraints->stringsAllowed;
   constraints->floatsAllowed = bsaveConstraints->floatsAllowed;
   constraints->integersAllowed = bsaveConstraints->integersAllowed;
   constraints->instanceNamesAllowed = bsaveConstraints->instanceNamesAllowed;
   constraints->instanceAddressesAllowed = bsaveConstraints->instanceAddressesAllowed;
   constraints->externalAddressesAllowed = bsaveConstraints->externalAddressesAllowed;
   constraints->multifieldsAllowed = bsaveConstraints->multifieldsAllowed;
   constraints->singlefieldsAllowed = bsaveConstraints->singlefieldsAllowed; 
   constraints->factAddressesAllowed = bsaveConstraints->factAddressesAllowed;
#if FUZZY_DEFTEMPLATES
   constraints->fuzzyValuesAllowed = bsaveConstraints->fuzzyValuesAllowed;  
   constraints->fuzzyValueRestriction = bsaveConstraints->fuzzyValueRestriction;
#endif  
   constraints->anyRestriction = bsaveConstraints->anyRestriction;
   constraints->symbolRestriction = bsaveConstraints->symbolRestriction;
   constraints->stringRestriction = bsaveConstraints->stringRestriction;
   constraints->floatRestriction = bsaveConstraints->floatRestriction;
   constraints->integerRestriction = bsaveConstraints->integerRestriction;
   constraints->instanceNameRestriction = bsaveConstraints->instanceNameRestriction;
   
   constraints->restrictionList = HashedExpressionPointer(bsaveConstraints->restrictionList);
   constraints->minValue = HashedExpressionPointer(bsaveConstraints->minValue);
   constraints->maxValue = HashedExpressionPointer(bsaveConstraints->maxValue);
   constraints->minFields = HashedExpressionPointer(bsaveConstraints->minFields);
   constraints->maxFields = HashedExpressionPointer(bsaveConstraints->maxFields);
   constraints->multifield = NULL;
  }
  
/********************************************************/
/* ClearBloadedConstraints: Releases memory associated  */
/*   with constraints loaded from binary image          */
/********************************************************/
globle VOID ClearBloadedConstraints()
  {
   if (NumberOfConstraints != 0)
     {
      genlongfree((VOID *) ConstraintArray,
                  (unsigned long) (sizeof(CONSTRAINT_RECORD) *
                                  NumberOfConstraints));
      NumberOfConstraints = 0;
     }
  }
  
#endif /* (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME) */





