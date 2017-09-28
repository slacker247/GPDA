   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  02/25/94            */
   /*                                                     */
   /*                 FACT HASHING MODULE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides routines for maintaining a fact hash    */
/*   table so that duplication of facts can quickly be       */
/*   determined.                                             */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Bob Orchard (NRCC - Nat'l Research Council of Canada)*/
/*                  (Fuzzy reasoning extensions)             */
/*                  (certainty factors for facts and rules)  */
/*                  (extensions to run command)              */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#define _FACTHSH_SOURCE_

#include <stdio.h>
#define _CLIPS_STDIO_

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT

#if FUZZY_DEFTEMPLATES     /* added 03-06-96 */
#include "symbol.h"
#include "fuzzyutl.h"
#endif

#include "constant.h"
#include "clipsmem.h"
#include "router.h"

#if DEFRULE_CONSTRUCT
#include "lgcldpnd.h"
#endif

#include "facthsh.h"

#if CERTAINTY_FACTORS    /* added 03-06-96 */
#include "cfdef.h"
#endif

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if ANSI_COMPILER
   static int                     HashFact(struct fact *);
   static int                     HashMultifield(struct multifield *);
   static struct fact            *FactExists(struct fact *,int);
#else
   static int                     HashFact();
   static int                     HashMultifield();
   static struct fact            *FactExists();
#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static struct factHashEntry  **FactHashTable;
   static BOOLEAN                 FactDuplication = CLIPS_FALSE;

/************************************************/
/* HashFact: Returns the hash value for a fact. */
/************************************************/
static int HashFact(theFact)
  struct fact *theFact;
  {
   int count = 0;
   int hashValue;
   
   /*============================================*/
   /* Get a hash value for the deftemplate name. */
   /*============================================*/
   
   count += HashSymbol(ValueToString(theFact->whichDeftemplate->header.name),
                       SIZE_FACT_HASH);

#if FUZZY_DEFTEMPLATES   /* added 03-06-96 */
   /* fuzzy facts are only different in their template name */
   /* I.E. there can be ONLY 1 fact at a time for any fuzzy */
   /*      template  OR any fact with fuzzy Slots           */
   /* so just use the name of the fact template (could      */
   /* probably use the hash value of the fuzzy value found  */
   /* in theFact->theProposition->theFields[0] (Hashnode)   */
   /* BUT why bother -- just adds to overhead of calculating*/
   /* the hash value and I doubt we get any better          */
   /* distribution in the hash table - most of the hash     */
   /* calculations seem to be far too complex!!             */

   if (theFact->whichDeftemplate->hasFuzzySlots)
      return(count);
#endif
   
   /*=================================================*/
   /* Add in the hash value for the rest of the fact. */
   /*=================================================*/
   
   count += (int) HashMultifield(&theFact->theProposition);
   
   /*================================*/
   /* Make sure the hash value falls */
   /* in the appropriate range.      */
   /*================================*/
   
   hashValue = (int) (count % SIZE_FACT_HASH);
   if (hashValue < 0) hashValue = - hashValue;
   
   /*========================*/
   /* Return the hash value. */
   /*========================*/
   
   return(hashValue);
  }
     
/************************************************************/
/* HashMultifield: Returns the hash value for a multifield. */
/************************************************************/
static int HashMultifield(theSegment)
  struct multifield *theSegment;
  {
   long length, i;
   unsigned int tvalue;
   unsigned int count;
   struct field *fieldPtr;
   union
     {
      double fv;
      unsigned int liv;
     } fis;

   /*================================================*/
   /* Initialize variables for computing hash value. */
   /*================================================*/
   
   count = 0;
   length = theSegment->multifieldLength;
   fieldPtr = theSegment->theFields;
   
   /*====================================================*/
   /* Loop through each value in the multifield, compute */
   /* its hash value, and add it to the running total.   */
   /*====================================================*/
   
   for (i = 0;
        i < length;
        i++)
     {
      switch(fieldPtr[i].type)
         {
          case MULTIFIELD:
            count += HashMultifield((struct multifield *) fieldPtr[i].value);
            break;
            
          case FLOAT:
            fis.fv = ValueToDouble(fieldPtr[i].value);
            count += (fis.liv * (i + 29));
            break;
            
          case INTEGER:
            count += (int) (((int) ValueToLong(fieldPtr[i].value)) * (i + 29));
            break;
            
          case FACT_ADDRESS:
          case EXTERNAL_ADDRESS:
#if OBJECT_SYSTEM
          case INSTANCE_ADDRESS:
#endif
            count += (int) (((int) fieldPtr[i].value) * (i + 29));
            break;
            
          case SYMBOL:
          case STRING:
#if OBJECT_SYSTEM
          case INSTANCE_NAME:
#endif
            tvalue = (unsigned) HashSymbol(ValueToString(fieldPtr[i].value),SIZE_FACT_HASH);
            count += (unsigned) (tvalue * (i + 29));
            break;
         }
     }
     
   /*========================*/
   /* Return the hash value. */
   /*========================*/

   return(count);
  }

/**********************************************/
/* FactExists: Determines if a specified fact */
/*   already exists in the fact hash table.   */
/**********************************************/
static struct fact *FactExists(theFact,hashValue)
  struct fact *theFact;
  int hashValue;
  {
   struct factHashEntry *theFactHash;

   for (theFactHash = FactHashTable[hashValue];
        theFactHash != NULL;
        theFactHash = theFactHash->next)
     {
      if ((theFact->whichDeftemplate == theFactHash->theFact->whichDeftemplate) ?
          MultifieldsEqual(&theFact->theProposition,
                           &theFactHash->theFact->theProposition) : CLIPS_FALSE)
#if CERTAINTY_FACTORS  /* added 03-06-96 */
          /* standard facts -- same templates means same fact --
             perform global contribution calculation for CF and return 
             ptr to existing fact indicating fact exists
          */
        { changeCFofExistingFact(theFact,theFactHash->theFact);
          return(theFactHash->theFact); 
        }
#else
        { return(theFactHash->theFact); }
#endif
      }

   return(NULL);
  }

/************************************************************/
/* AddHashedFact: Adds a fact entry to the fact hash table. */
/************************************************************/
globle VOID AddHashedFact(theFact,hashValue)
  struct fact *theFact;
  int hashValue;
  {
   struct factHashEntry *newhash, *temp;

   newhash = get_struct(factHashEntry);
   newhash->theFact = theFact;

   temp = FactHashTable[hashValue];
   FactHashTable[hashValue] = newhash;
   newhash->next = temp;
  }

/******************************************/
/* RemoveHashedFact: Removes a fact entry */
/*   from the fact hash table.            */
/******************************************/
globle BOOLEAN RemoveHashedFact(theFact)
  struct fact *theFact;
  {
   int hashValue;
   struct factHashEntry *hptr, *prev;

   hashValue = HashFact(theFact);

   for (hptr = FactHashTable[hashValue], prev = NULL;
        hptr != NULL;
        hptr = hptr->next)
     {
      if (hptr->theFact == theFact)
        {
         if (prev == NULL)
           {
            FactHashTable[hashValue] = hptr->next;
            rtn_struct(factHashEntry,hptr);
            return(1);
           }
         else
           {
            prev->next = hptr->next;
            rtn_struct(factHashEntry,hptr);
            return(1);
           }
        }
      prev = hptr;
     }
     
   return(0);
  }

/* added the HandleExistingFuzzyFact 03-06-96 */
#if FUZZY_DEFTEMPLATES
/*******************************************************/
/* HandleExistingFuzzyFact: Determines if new fact to  */
/*   be added to the fact-list is existing fuzzy fact  */
/*   and if so modifies it accordingly.  Will also     */
/*   retract the existing fact if it finds one         */
/*                                                     */
/*   Returns hashvalue for the NEW fact                */
/*                                                     */
/* NOTE: a fact with fuzzy slots exists already if the */
/*       non-fuzzy slots are identical in value and    */
/*       the fuzzy slots all have the same FUZZY_VALUE */
/*       type -- i.e. both temperature deftemplates    */
/*******************************************************/
globle int HandleExistingFuzzyFact(theFact)
  VOID **theFact;
  {
   struct fact *tempFact;
   struct factHashEntry *theFactHash;
   int hashValue;
   struct fact *theFactPtr = (struct fact *)*theFact;

   hashValue = HashFact((VOID *)theFactPtr);

   /* Fuzzy facts never get duplicated ... they just get modified if they
      already exist ... always allow duplication for them... 
      do the required modification to the fact if it already exists
   */
   
   theFactHash = FactHashTable[hashValue];
   tempFact = NULL;

   while (theFactHash != NULL)
     {
      if (theFactPtr->whichDeftemplate == theFactHash->theFact->whichDeftemplate)
        { /* same template definitions for fact */
          if (MultifieldsEqual(&theFactPtr->theProposition,
                               &theFactHash->theFact->theProposition))
            { /* MultiFieldsEqual will compare fuzzy fields to be equal
                 if they are both of the same FUZZY_VALUE type
              */
              tempFact = theFactHash->theFact; 
#if CERTAINTY_FACTORS
              /* If facts are the same we need to perform
                 global contribution calculation for CF 
              */
              changeCFofNewVsExistingFact(theFactPtr, tempFact);
#endif

              /* fuzzy facts (ie. any fuzzy slots) perform global
                 contribution calculation(s) for each fuzzy slot
              */
              changeValueOfFuzzySlots(tempFact, theFactPtr);
			  break;
            }
        }
      theFactHash = theFactHash->next;
     }
	 
   if (tempFact != NULL) /* existing fact! retract it before new one gets asserted */
       Retract(tempFact);

   return(hashValue);
  }

#endif

/*****************************************************/
/* HandleFactDuplication: Determines if a fact to be */
/*   added to the fact-list is a duplicate entry and */
/*   takes appropriate action based on the current   */
/*   setting of the fact-duplication flag.           */
/*****************************************************/
globle int HandleFactDuplication(theFact)
  VOID *theFact;
  {
   struct fact *tempPtr;
   int hashValue;

   hashValue = HashFact(theFact);

   if (FactDuplication) return(hashValue);
   
   tempPtr = FactExists(theFact,hashValue);
   if (tempPtr == NULL) return(hashValue);

   ReturnFact(theFact);
#if LOGICAL_DEPENDENCIES && DEFRULE_CONSTRUCT
   AddLogicalDependencies((struct patternEntity *) tempPtr,CLIPS_TRUE);
#endif
   return(-1);
  }

/********************************************/
/* GetFactDuplication: C access routine for */
/*   the get-fact-duplication command.      */
/********************************************/
globle BOOLEAN GetFactDuplication()
  { return(FactDuplication); }

/********************************************/
/* SetFactDuplication: C access routine for */
/*   the set-fact-duplication command.      */
/********************************************/
globle BOOLEAN SetFactDuplication(value)
  int value;
  {
   int ov;

   ov = FactDuplication;
   FactDuplication = value;
   return(ov);
  }

/**************************************************/
/* InitializeFactHashTable: Initializes the table */
/*   entries in the fact hash table to NULL.      */
/**************************************************/
globle VOID InitializeFactHashTable()
   {
    int i;

    FactHashTable = (struct factHashEntry **) 
                    gm2((int) sizeof (struct factHashEntry *) * SIZE_FACT_HASH);

    if (FactHashTable == NULL) ExitCLIPS(1);

    for (i = 0; i < SIZE_FACT_HASH; i++) FactHashTable[i] = NULL;
   }

#if DEVELOPER

/*****************************************************/
/* ShowFactHashTable: Displays the number of entries */
/*   in each slot of the fact hash table.            */
/*****************************************************/
globle VOID ShowFactHashTable()
   {
    int i, count;
    struct factHashEntry *theEntry;
    char buffer[20];

    for (i = 0; i < SIZE_FACT_HASH; i++) 
      {
       for (theEntry =  FactHashTable[i], count = 0;
            theEntry != NULL;
            theEntry = theEntry->next,count++);
            
       if (count != 0) 
         {
          sprintf(buffer,"%4d: %4d\n",i,count);
          PrintCLIPS(WDISPLAY,buffer);
         }
      }
   }
   
#endif /* DEVELOPER */

#endif /* DEFTEMPLATE_CONSTRUCT */

