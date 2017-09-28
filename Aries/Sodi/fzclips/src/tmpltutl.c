   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  02/23/94            */
   /*                                                     */
   /*            DEFTEMPLATE UTILITIES MODULE             */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides utility routines for deftemplates.      */
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

#define  _TMPLTUTL_SOURCE_

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT

#include <stdio.h>

#define _CLIPS_STDIO_

#include <string.h>

#include "extnfunc.h"
#include "clipsmem.h"
#include "constrct.h"
#include "router.h"
#include "argacces.h"
#include "cstrnchk.h"
#include "tmpltfun.h"
#include "tmpltpsr.h"
#include "modulutl.h"
#include "watch.h"
#include "tmpltbsc.h"
#include "tmpltdef.h"

#include "tmpltutl.h"

#if FUZZY_DEFTEMPLATES
#include "fuzzypsr.h"
#include "fuzzyutl.h"
#include "symbol.h"
#endif

#if CERTAINTY_FACTORS  
#include "cfdef.h"
#endif

/********************************************************/
/* InvalidDeftemplateSlotMessage: Generic error message */
/*   for use when a specified slot name isn't defined   */
/*   in its corresponding deftemplate.                  */
/********************************************************/
globle VOID InvalidDeftemplateSlotMessage(slotName,deftemplateName)
  char *slotName;
  char *deftemplateName;
  {
   PrintErrorID("TMPLTDEF",1,CLIPS_TRUE);
   PrintCLIPS(WERROR,"Invalid slot ");
   PrintCLIPS(WERROR,slotName);
   PrintCLIPS(WERROR," not defined in corresponding deftemplate ");
   PrintCLIPS(WERROR,deftemplateName);
   PrintCLIPS(WERROR,".\n");
  }
  
/**********************************************************/
/* SingleFieldSlotCardinalityError: Generic error message */
/*   used when an attempt is made to placed a multifield  */
/*   value into a single field slot.                      */
/**********************************************************/
globle VOID SingleFieldSlotCardinalityError(slotName)
  char *slotName;
  {
   PrintErrorID("TMPLTDEF",2,CLIPS_TRUE);
   PrintCLIPS(WERROR,"The single field slot ");
   PrintCLIPS(WERROR,slotName);
   PrintCLIPS(WERROR," can only contain a single field value.\n");
  }
  
/**********************************************************************/
/* MultiIntoSingleFieldSlotError: Determines if a multifield value is */
/*   being placed into a single field slot of a deftemplate fact.     */
/**********************************************************************/
globle VOID MultiIntoSingleFieldSlotError(theSlot,theDeftemplate)
  struct templateSlot *theSlot;
  struct deftemplate *theDeftemplate;
  {
   PrintErrorID("TMPLTFUN",2,CLIPS_TRUE);
   PrintCLIPS(WERROR,"Attempted to assert a multifield value \n");
   PrintCLIPS(WERROR,"into the single field slot ");
   if (theSlot != NULL) PrintCLIPS(WERROR,theSlot->slotName->contents);
   else PrintCLIPS(WERROR,"<<unknown>>");
   PrintCLIPS(WERROR," of deftemplate ");
   if (theDeftemplate != NULL) PrintCLIPS(WERROR,theDeftemplate->header.name->contents);
   else PrintCLIPS(WERROR,"<<unknown>>");
   PrintCLIPS(WERROR,".\n");
   
   SetEvaluationError(CLIPS_TRUE);
  }

/**************************************************************/
/* CheckTemplateFact: Checks a fact to see if it violates any */
/*   deftemplate type, allowed-..., or range specifications.  */
/**************************************************************/
globle VOID CheckTemplateFact(theFact)
  struct fact *theFact;
  {
   struct field *sublist;
   int i;
   struct deftemplate *theDeftemplate;
   struct templateSlot *slotPtr;
   DATA_OBJECT theData;
   char thePlace[20];
   int rv;

   if (! GetDynamicConstraintChecking()) return;

   sublist = theFact->theProposition.theFields;

   /*========================================================*/
   /* If the deftemplate corresponding to the first field of */
   /* of the fact cannot be found, then the fact cannot be   */
   /* checked against the deftemplate format.                */
   /*========================================================*/

   theDeftemplate = theFact->whichDeftemplate;
   if (theDeftemplate == NULL) return;
   if (theDeftemplate->implied) return;

   /*=============================================*/
   /* Check each of the slots of the deftemplate. */
   /*=============================================*/

   i = 0;
   for (slotPtr = theDeftemplate->slotList; 
        slotPtr != NULL;
        slotPtr = slotPtr->next)
     {
      /*================================================*/
      /* Store the slot value in the appropriate format */
      /* for a call to the constraint checking routine. */
      /*================================================*/
      
      if (slotPtr->multislot == CLIPS_FALSE)
        {
         theData.type = sublist[i].type;
         theData.value = sublist[i].value;
         i++;
        }
      else
        { 
         theData.type = MULTIFIELD;
         theData.value = (VOID *) sublist[i].value;
         theData.begin = 0;
         theData.end = ((struct multifield *) sublist[i].value)->multifieldLength-1;
         i++;
        }
        
      /*=============================================*/
      /* Call the constraint checking routine to see */
      /* if a constraint violation occurred.         */
      /*=============================================*/
      
      rv = ConstraintCheckDataObject(&theData,slotPtr->constraints);
      if (rv != NO_VIOLATION)
        {
         sprintf(thePlace,"fact f-%-5ld ",theFact->factIndex);
     
         PrintErrorID("CSTRNCHK",1,CLIPS_TRUE);
         PrintCLIPS(WERROR,"Slot value ");
         PrintDataObject(WERROR,&theData);
         PrintCLIPS(WERROR," ");
         ConstraintViolationErrorMessage(NULL,thePlace,CLIPS_FALSE,0,slotPtr->slotName,
                                         0,rv,slotPtr->constraints,CLIPS_TRUE);
         SetHaltExecution(CLIPS_TRUE);
         return;
        }
     }

   return;
  }
  
/***********************************************************************/
/* CheckRHSSlotTypes: Checks the validity of a change to a slot as the */
/*   result of an assert, modify, or duplicate command. This checking  */
/*   is performed statically (i.e. when the command is being parsed).  */
/***********************************************************************/
globle BOOLEAN CheckRHSSlotTypes(rhsSlots,slotPtr,thePlace)
  struct expr *rhsSlots;
  struct templateSlot *slotPtr;
  char *thePlace;
  {
   int rv;
   char *theName;

   if (GetStaticConstraintChecking() == CLIPS_FALSE) return(CLIPS_TRUE);
      rv = ConstraintCheckExpressionChain(rhsSlots,slotPtr->constraints);
      if (rv != NO_VIOLATION)
        {
         if (rv != CARDINALITY_VIOLATION) theName = "A literal slot value";
         else theName = "Literal slot values";
         ConstraintViolationErrorMessage(theName,thePlace,CLIPS_TRUE,0,
                                         slotPtr->slotName,0,rv,slotPtr->constraints,CLIPS_TRUE);
         return(0);
        }
        
   return(1);
  }  
  
/*********************************************************/
/* GetNthSlot: Given a deftemplate and an integer index, */
/*   returns the nth slot of a deftemplate.              */
/*********************************************************/
globle struct templateSlot *GetNthSlot(theDeftemplate,position)
  struct deftemplate *theDeftemplate;
  int position;
  {
   struct templateSlot *slotPtr;
   int i = 0;
   
   slotPtr = theDeftemplate->slotList;
   while (slotPtr != NULL)
     {
      if (i == position) return(slotPtr);
      slotPtr = slotPtr->next;
      i++;
     }

   return(NULL);
  }
  
/*******************************************************/
/* FindSlotPosition: Finds the position of a specified */
/*   slot in a deftemplate structure.                  */
/*******************************************************/
globle int FindSlotPosition(theDeftemplate,name)
  struct deftemplate *theDeftemplate;
  SYMBOL_HN *name;
  {
   struct templateSlot *slotPtr;
   int position;

   for (slotPtr = theDeftemplate->slotList, position = 1;
        slotPtr != NULL;
        slotPtr = slotPtr->next, position++)
     {
      if (slotPtr->slotName == name)
        { return(position); }
     }

   return(0);
  }
  
/*******************************************************************/
/* PrintTemplateFact: Prints a fact using the deftemplate format.  */
/*   Returns CLIPS_TRUE if the fact was printed using this format, */
/*   otherwise CLIPS_FALSE.                                        */
/*******************************************************************/
globle VOID PrintTemplateFact(logicalName,theFact)
  char *logicalName;
  struct fact *theFact;
  {
   struct field *sublist;
   int i;
   struct deftemplate *theDeftemplate;
   struct templateSlot *slotPtr;

   /*==============================*/
   /* Initialize some information. */
   /*==============================*/
   
   theDeftemplate = theFact->whichDeftemplate;
   sublist = theFact->theProposition.theFields;

   /*=============================================*/
   /* Print the relation name of the deftemplate. */
   /*=============================================*/

   PrintCLIPS(logicalName,"("); 
   PrintCLIPS(logicalName,theDeftemplate->header.name->contents);

#if FUZZY_DEFTEMPLATES
   if (theDeftemplate->fuzzyTemplate != NULL)  /* fuzzy template */
      {
        PrintFuzzyTemplateFact(logicalName, 
                      (struct fuzzy_value *)ValueToFuzzyValue((sublist[0].value))
#if CERTAINTY_FACTORS  
                      ,theFact->factCF   
#endif
							   );
        return;
      }
#endif

   if (theDeftemplate->slotList != NULL) PrintCLIPS(logicalName," ");

   /*===================================================*/
   /* Print each of the field slots of the deftemplate. */
   /*===================================================*/

   slotPtr = theDeftemplate->slotList;

   i = 0;
   while (slotPtr != NULL)
     {
      /*===========================================*/
      /* Print the closing parenthesis of the slot */
      /* and the slot name.                        */
      /*===========================================*/

      PrintCLIPS(logicalName,"(");
      PrintCLIPS(logicalName,slotPtr->slotName->contents);

      /*======================================================*/
      /* Print the value of the slot for a single field slot. */
      /*======================================================*/

      if (slotPtr->multislot == CLIPS_FALSE)
        { 
         PrintCLIPS(logicalName," ");

#if FUZZY_DEFTEMPLATES
         /* for a fuzzy value printed during a fact save
            we need to look for the 'xxx' linguistic value --
            if it is xxx then print the set as singletons
         */
         if (saveFactsInProgress && 
             sublist[i].type == FUZZY_VALUE
            )
	   { struct fuzzy_value *fv;

             fv =  ValueToFuzzyValue(sublist[i].value);
             if (strcmp("???", fv->name) == 0)
               PrintFuzzySet(logicalName, fv);
             else
               PrintCLIPS(logicalName, fv->name);
           }
         else
#endif

         PrintAtom(logicalName,sublist[i].type,sublist[i].value); 
        }

      /*==========================================================*/
      /* Else print the value of the slot for a multi field slot. */
      /*==========================================================*/

      else
        { 
         struct multifield *theSegment;
         
         theSegment = (struct multifield *) sublist[i].value;
         if (theSegment->multifieldLength > 0)
           {
            PrintCLIPS(logicalName," ");
            PrintMultifield(logicalName,sublist[i].value,0,theSegment->multifieldLength-1,CLIPS_FALSE);
           }
        }

      /*============================================*/
      /* Print the closing parenthesis of the slot. */
      /*============================================*/

      i++;
      PrintCLIPS(logicalName,")");
      slotPtr = slotPtr->next;
      if (slotPtr != NULL) PrintCLIPS(logicalName," ");
     }
     
   PrintCLIPS(logicalName,")");

#if CERTAINTY_FACTORS  
   printCF(logicalName,theFact->factCF);   
#endif

#if FUZZY_DEFTEMPLATES
   /* There may be some fuzzy value slots in the fact -- if so just
      print out the fuzzy sets for them on next lines
      ... UNLESS we are doing a fact save operation!
   */
   if (!saveFactsInProgress)
     for (i=0; i<(unsigned int)theDeftemplate->numberOfSlots; i++)
       {
        if (sublist[i].type == FUZZY_VALUE)
          {
           PrintCLIPS(logicalName,"\n\t( ");
           PrintFuzzySet(logicalName, ValueToFuzzyValue(sublist[i].value));
           PrintCLIPS(logicalName," )");
          }
       }
#endif
  }

/***************************************************************************/
/* UpdateDeftemplateScope: Updates the scope flag of all the deftemplates. */
/***************************************************************************/
globle VOID UpdateDeftemplateScope()
  {
   struct deftemplate *theDeftemplate;
   int moduleCount;
   struct defmodule *theModule;
   struct defmoduleItemHeader *theItem;
   
   /*==================================*/
   /* Loop through all of the modules. */
   /*==================================*/
   
   for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      /*======================================================*/
      /* Loop through each of the deftemplates in the module. */
      /*======================================================*/
      
      theItem = (struct defmoduleItemHeader *)
                GetModuleItem(theModule,DeftemplateModuleIndex);
      
      for (theDeftemplate = (struct deftemplate *) theItem->firstItem;
           theDeftemplate != NULL ;
           theDeftemplate = (struct deftemplate *) GetNextDeftemplate(theDeftemplate))
        {  
         /*=======================================*/
         /* If the deftemplate can be seen by the */
         /* current module, then it is in scope.  */
         /*=======================================*/
         
         if (FindImportedConstruct("deftemplate",theModule,
                                   ValueToString(theDeftemplate->header.name),
                                   &moduleCount,CLIPS_TRUE,NULL) != NULL)
           { theDeftemplate->inScope = CLIPS_TRUE; }
         else
           { theDeftemplate->inScope = CLIPS_FALSE; }
        }  
     }
  }
  
/****************************************************************/
/* FindSlot: Finds a specified slot in a deftemplate structure. */
/****************************************************************/
globle struct templateSlot *FindSlot(theDeftemplate,name,whichOne)
  struct deftemplate *theDeftemplate;
  SYMBOL_HN *name;
  int *whichOne;
  {
   struct templateSlot *slotPtr;

   *whichOne = 1;
   slotPtr = theDeftemplate->slotList;
   while (slotPtr != NULL)
     {
      if (slotPtr->slotName == name)
        { return(slotPtr); }
      (*whichOne)++;
      slotPtr = slotPtr->next;
     }

   *whichOne = -1;
   return(NULL);
  } 

#if (! RUN_TIME) && (! BLOAD_ONLY)

/************************************************************/
/* CreateImpliedDeftemplate: Creates an implied deftemplate */
/*   and adds it to the list of deftemplates.               */
/************************************************************/
globle struct deftemplate *CreateImpliedDeftemplate(deftemplateName,setFlag)
  SYMBOL_HN *deftemplateName;
  int setFlag;
  {
   struct deftemplate *newDeftemplate;

   newDeftemplate = get_struct(deftemplate);
   newDeftemplate->header.name = deftemplateName;
   newDeftemplate->header.ppForm = NULL;
   newDeftemplate->slotList = NULL;
   newDeftemplate->implied = setFlag;
   newDeftemplate->numberOfSlots = 0;
   newDeftemplate->inScope = 1;
   newDeftemplate->patternNetwork = NULL;
   newDeftemplate->busyCount = 0;
   newDeftemplate->watch = CLIPS_FALSE;
   newDeftemplate->header.next = NULL;
#if FUZZY_DEFTEMPLATES
   newDeftemplate->hasFuzzySlots = CLIPS_FALSE;
   newDeftemplate->fuzzyTemplate = NULL;
#endif

#if DEBUGGING_FUNCTIONS
   if (GetWatchItem("facts"))
     { SetDeftemplateWatch(ON,(VOID *) newDeftemplate); }
#endif

   newDeftemplate->header.whichModule = (struct defmoduleItemHeader *) 
                                        GetModuleItem(NULL,DeftemplateModuleIndex);

   AddConstructToModule(&newDeftemplate->header);
   InstallDeftemplate(newDeftemplate);
   
   return(newDeftemplate);
  }
 
#endif

#endif /* DEFTEMPLATE_CONSTRUCT */
