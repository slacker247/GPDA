   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00 02/18/94             */
   /*                                                     */
   /*              DEFTEMPLATE PARSER MODULE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Parses the deftemplate construct.                */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#define _TMPLTPSR_SOURCE_

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT

#include <stdio.h>
#define _CLIPS_STDIO_
#include <string.h>

#include "constant.h"
#include "clipsmem.h"
#include "symbol.h"
#include "scanner.h"
#include "exprnpsr.h"
#include "router.h"
#include "constrct.h"
#include "factmngr.h"
#include "cstrnchk.h"
#include "cstrnpsr.h"
#include "cstrcpsr.h"
#if BLOAD || BLOAD_AND_BSAVE
#include "bload.h"
#endif
#include "default.h"
#include "pattern.h" 
#include "watch.h"
#include "cstrnutl.h"

#include "tmpltdef.h"
#include "tmpltbsc.h"

#include "tmpltpsr.h"


#if FUZZY_DEFTEMPLATES
#include "fuzzypsr.h"
#endif

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if (! RUN_TIME) && (! BLOAD_ONLY)
#if ANSI_COMPILER
   static struct templateSlot    *SlotDeclarations(char *,struct token *);
   static struct templateSlot    *ParseSlot(char *,struct token *,struct templateSlot *);
   static struct templateSlot    *DefinedSlots(char *,SYMBOL_HN *,int,struct token *);
#else
   static struct templateSlot    *SlotDeclarations();
   static struct templateSlot    *ParseSlot();
   static struct templateSlot    *DefinedSlots();
#endif
#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

#if (! RUN_TIME) && (! BLOAD_ONLY)
   static int              DeftemplateError;
#endif

/*******************************************************/
/* ParseDeftemplate: Parses the deftemplate construct. */
/*******************************************************/
globle int ParseDeftemplate(readSource)
  char *readSource;
  {
#if (MAC_MPW || MAC_MCW) && (RUN_TIME || BLOAD_ONLY)/*added 3-11-96*/
#pragma unused(readSource)
#endif

#if (! RUN_TIME) && (! BLOAD_ONLY)
   SYMBOL_HN *deftemplateName;
   struct deftemplate *newDeftemplate;
   struct templateSlot *slots;

#if FUZZY_DEFTEMPLATES
/********************************************************/
/*  A fuzzy deftemplate has a template with a different */
/*  syntax than a regular template                      */
/********************************************************/
   struct fuzzyLv *fzTemplate;     
   struct templateSlot *slotPtr;
#endif

   struct token inputToken;

   /*================================================*/
   /* Initialize pretty print and error information. */
   /*================================================*/

   DeftemplateError = CLIPS_FALSE;
   SetPPBufferStatus(ON);
   FlushPPBuffer();
   SavePPBuffer("(deftemplate ");
   
   /*==============================================================*/
   /* Deftemplates can not be added when a binary image is loaded. */
   /*==============================================================*/
   
#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded() == CLIPS_TRUE)
     {
      CannotLoadWithBloadMessage("deftemplate");
      return(CLIPS_TRUE);
     }
#endif

   /*=======================================================*/
   /* Parse the name and comment fields of the deftemplate. */
   /*=======================================================*/

#if DEBUGGING_FUNCTIONS
   DeletedTemplateDebugFlags = 0;
#endif 

   deftemplateName = GetConstructNameAndComment(readSource,&inputToken,"deftemplate",
                                                FindDeftemplate,Undeftemplate,"%",
                                                CLIPS_TRUE,CLIPS_TRUE,CLIPS_TRUE);
   if (deftemplateName == NULL) return(CLIPS_TRUE);
      
   if (ReservedPatternSymbol(ValueToString(deftemplateName),"deftemplate"))
     {
      ReservedPatternSymbolErrorMsg(ValueToString(deftemplateName),"a deftemplate name");
      return(CLIPS_TRUE);
     }
     
   /*===========================================*/
   /* Parse the slot fields of the deftemplate. */
   /* OR the fuzzy variable description         */
   /*===========================================*/

#if FUZZY_DEFTEMPLATES
   if (inputToken.type == FLOAT || inputToken.type == INTEGER)  
     {
      /* if next token is a number could be a fuzzy definition */
      fzTemplate = ParseFuzzyTemplate(readSource, &inputToken, &DeftemplateError);

      if (DeftemplateError == CLIPS_TRUE) return(CLIPS_TRUE);

      /*===========================*/
      /* Build the slot container. */
      /*===========================*/

      slots = get_struct(templateSlot);
      slots->slotName = (SYMBOL_HN *)AddSymbol("GenericFuzzySlot");
      slots->defaultList = NULL;
      slots->constraints = GetConstraintRecord();
      slots->constraints->anyAllowed = CLIPS_FALSE; 
      slots->constraints->fuzzyValuesAllowed = CLIPS_TRUE; 
      slots->constraints->fuzzyValueRestriction = CLIPS_FALSE; 
      slots->constraints->restrictionList = NULL;
      slots->noDefault = CLIPS_TRUE;
      slots->defaultPresent = CLIPS_FALSE;
      slots->defaultDynamic = CLIPS_FALSE;
      slots->multislot = CLIPS_FALSE;
      slots->next = NULL;
       
     }
   else
     { /* otherwise it should be a standard deftemplate description */
      slots = SlotDeclarations(readSource,&inputToken);
      fzTemplate = NULL;
     }
#else
   slots = SlotDeclarations(readSource,&inputToken);
#endif

   if (DeftemplateError == CLIPS_TRUE) return(CLIPS_TRUE);

   /*=====================================*/
   /* Create a new deftemplate structure. */
   /*=====================================*/

   newDeftemplate = get_struct(deftemplate);
   newDeftemplate->header.name =  deftemplateName;
   newDeftemplate->header.next = NULL;  
   newDeftemplate->slotList = slots;
#if FUZZY_DEFTEMPLATES
   /* if any fuzzy value slots set hasFuzzySlots to TRUE */
   newDeftemplate->hasFuzzySlots = CLIPS_FALSE;
   for (slotPtr = slots; slotPtr != NULL; slotPtr = slotPtr->next)
     {
       if (slotPtr->constraints->fuzzyValuesAllowed)
         {
           newDeftemplate->hasFuzzySlots = CLIPS_TRUE;
           break;
         }
     }

   /* if a Fuzzy Deftemplate set ptr to fuzzyLv description */
   newDeftemplate->fuzzyTemplate = fzTemplate;
#endif
   newDeftemplate->implied = CLIPS_FALSE;
   newDeftemplate->numberOfSlots = 0;
   newDeftemplate->busyCount = 0;
   newDeftemplate->watch = 0;
   newDeftemplate->inScope = CLIPS_TRUE;
   newDeftemplate->patternNetwork = NULL;
   newDeftemplate->header.whichModule = (struct defmoduleItemHeader *)
                                        GetModuleItem(NULL,DeftemplateModuleIndex);

   /*================================*/
   /* Determine the number of slots. */
   /*================================*/
   
   while (slots != NULL)
     {
      newDeftemplate->numberOfSlots++;
      slots = slots->next;
     }

#if FUZZY_DEFTEMPLATES
   if (fzTemplate != NULL)  /* fuzzy set is like a single slot */
     newDeftemplate->numberOfSlots = 1; 
#endif

   /*====================================*/
   /* Store pretty print representation. */
   /*====================================*/

   if (GetConserveMemory() == CLIPS_TRUE)
     { newDeftemplate->header.ppForm = NULL; }
   else
     { newDeftemplate->header.ppForm = CopyPPBuffer(); }
   
   /*=======================================================================*/
   /* If a template is redefined, then we want to restore its watch status. */
   /*=======================================================================*/

#if DEBUGGING_FUNCTIONS
   if ((BitwiseTest(DeletedTemplateDebugFlags,0)) || GetWatchItem("facts"))
     { SetDeftemplateWatch(ON,(VOID *) newDeftemplate); }
#endif

   /*==============================================*/
   /* Add deftemplate to the list of deftemplates. */
   /*==============================================*/

   AddConstructToModule(&newDeftemplate->header);

   InstallDeftemplate(newDeftemplate);
   
#endif

   return(CLIPS_FALSE);
  }
  
#if (! RUN_TIME) && (! BLOAD_ONLY)
  
/**************************************************************/
/* InstallDeftemplate: Increments all occurrences in the hash */
/*   table of symbols found in an deftemplate and adds it to  */
/*   the hash table.                                          */
/**************************************************************/
globle VOID InstallDeftemplate(theDeftemplate)
  struct deftemplate *theDeftemplate;
  {
   struct templateSlot *slotPtr;
   struct expr *tempExpr;
   
   IncrementSymbolCount(theDeftemplate->header.name);

   for (slotPtr = theDeftemplate->slotList;
        slotPtr != NULL;
        slotPtr = slotPtr->next)
     {
      IncrementSymbolCount(slotPtr->slotName);
      tempExpr = AddHashedExpression(slotPtr->defaultList);
      ReturnExpression(slotPtr->defaultList);
      slotPtr->defaultList = tempExpr;
      slotPtr->constraints = AddConstraint(slotPtr->constraints);
     }
#if FUZZY_DEFTEMPLATES
   if (theDeftemplate->fuzzyTemplate != NULL)
     {
       InstallFuzzyTemplate(theDeftemplate);
     }
#endif
  }
  
/********************************************************************/
/* SlotDeclarations: Parses the slot declarations of a deftemplate. */
/********************************************************************/
static struct templateSlot *SlotDeclarations(readSource,inputToken)
  char *readSource;
  struct token *inputToken;
  {
   struct templateSlot *newSlot, *slotList = NULL, *lastSlot = NULL;
   struct templateSlot *multiSlot = NULL;

   while (inputToken->type != RPAREN)
     {
      /*====================================================*/
      /* Slots begin with a '(' followed by a slot keyword. */
      /*====================================================*/

      if (inputToken->type != LPAREN)
        {
         SyntaxErrorMessage("deftemplate");
         ReturnSlots(slotList);
         ReturnSlots(multiSlot);
         DeftemplateError = CLIPS_TRUE;
         return(NULL);
        }

      GetToken(readSource,inputToken);
      if (inputToken->type != SYMBOL)
        {
         SyntaxErrorMessage("deftemplate");
         ReturnSlots(slotList);
         ReturnSlots(multiSlot);
         DeftemplateError = CLIPS_TRUE;
         return(NULL);
        }

      /*=================*/
      /* Parse the slot. */
      /*=================*/

      newSlot = ParseSlot(readSource,inputToken,slotList);
      if (DeftemplateError == CLIPS_TRUE)
        {
         ReturnSlots(newSlot);
         ReturnSlots(slotList);
         ReturnSlots(multiSlot);
         return(NULL);
        }

      /*===========================================*/
      /* Attach the new slot to the list of slots. */
      /*===========================================*/

      if (newSlot != NULL)
        {
         if (lastSlot == NULL)
           { slotList = newSlot; }
         else
           { lastSlot->next = newSlot; }
         lastSlot = newSlot;
        }

      /*================================*/
      /* Check for closing parenthesis. */
      /*================================*/

      GetToken(readSource,inputToken);
      if (inputToken->type != RPAREN)
        {
         PPBackup();
         SavePPBuffer("\n   ");
         SavePPBuffer(inputToken->printForm);
        }
     }

  SavePPBuffer("\n");

  /*=======================*/
  /* Return the slot list. */
  /*=======================*/

  return(slotList);
 }

/*****************************************************/
/* ParseSlot: Parses a single slot of a deftemplate. */
/*****************************************************/
static struct templateSlot *ParseSlot(readSource,inputToken,slotList)
  char *readSource;
  struct token *inputToken;
  struct templateSlot *slotList;
  {
   int parsingMultislot;   /* changed 03-11-96 */
   SYMBOL_HN *slotName;
   struct templateSlot *newSlot;
   int rv;

   /*=====================================================*/
   /* Slots must  begin with keyword field or multifield. */
   /*=====================================================*/

   if ((strcmp(ValueToString(inputToken->value),"field") != 0) &&
       (strcmp(ValueToString(inputToken->value),"multifield") != 0) &&
       (strcmp(ValueToString(inputToken->value),"slot") != 0) &&
       (strcmp(ValueToString(inputToken->value),"multislot") != 0))
     {
      SyntaxErrorMessage("deftemplate");
      DeftemplateError = CLIPS_TRUE;
      return(NULL);
     }

   /*===============================================*/
   /* Determine if multifield slot is being parsed. */
   /*===============================================*/

   if ((strcmp(ValueToString(inputToken->value),"multifield") == 0) ||
       (strcmp(ValueToString(inputToken->value),"multislot") == 0))
     { parsingMultislot = CLIPS_TRUE; }
   else
     { parsingMultislot = CLIPS_FALSE; }

   /*========================================*/
   /* The name of the slot must be a symbol. */
   /*========================================*/

   SavePPBuffer(" ");
   GetToken(readSource,inputToken);
   if (inputToken->type != SYMBOL)
     {
      SyntaxErrorMessage("deftemplate");
      DeftemplateError = CLIPS_TRUE;
      return(NULL);
     }

   slotName = (SYMBOL_HN *) inputToken->value;

   /*================================================*/
   /* Determine if the slot has already been parsed. */
   /*================================================*/
   
   while (slotList != NULL)
     {
      if (slotList->slotName == slotName)
        {
         AlreadyParsedErrorMessage("slot ",ValueToString(slotList->slotName));
         DeftemplateError = CLIPS_TRUE;
         return(NULL);
        }
        
      slotList = slotList->next;
     }
     
   /*===================================*/
   /* Parse the attributes of the slot. */
   /*===================================*/

   newSlot = DefinedSlots(readSource,slotName,parsingMultislot,inputToken);
   if (newSlot == NULL)
     {
      DeftemplateError = CLIPS_TRUE;
      return(NULL);
     }

   /*=================================*/
   /* Check for slot conflict errors. */
   /*=================================*/

   if (CheckConstraintParseConflicts(newSlot->constraints) == CLIPS_FALSE)
     {
      ReturnSlots(newSlot);
      DeftemplateError = CLIPS_TRUE;
      return(NULL);
     }
         
   if ((newSlot->defaultPresent) || (newSlot->defaultDynamic))
     { rv = ConstraintCheckExpressionChain(newSlot->defaultList,newSlot->constraints); }
   else
     { rv = NO_VIOLATION; }
     
   if ((rv != NO_VIOLATION) && GetStaticConstraintChecking())
     {
      char *temp;
      if (newSlot->defaultDynamic) temp = "the default-dynamic attribute";
      else temp = "the default attribute";
      ConstraintViolationErrorMessage("An expression",temp,CLIPS_FALSE,0,
                                      newSlot->slotName,0,rv,newSlot->constraints,CLIPS_TRUE);
      ReturnSlots(newSlot);
      DeftemplateError = CLIPS_TRUE;
      return(NULL);
     }

   /*==================*/
   /* Return the slot. */
   /*==================*/

   return(newSlot);
  }

/**************************************************************/
/* DefinedSlots: Parses a field or multifield slot attribute. */
/**************************************************************/
static struct templateSlot *DefinedSlots(readSource,slotName,multifieldSlot,inputToken)
  char *readSource;
  SYMBOL_HN *slotName;
  int multifieldSlot;
  struct token *inputToken;
  {
   struct templateSlot *newSlot;
   struct expr *defaultList;
   int defaultFound = CLIPS_FALSE;
   int noneSpecified, deriveSpecified;
   CONSTRAINT_PARSE_RECORD parsedConstraints;

   /*===========================*/
   /* Build the slot container. */
   /*===========================*/

   newSlot = get_struct(templateSlot);
   newSlot->slotName = slotName;
   newSlot->defaultList = NULL;
   newSlot->constraints = GetConstraintRecord();
   if (multifieldSlot)
     { newSlot->constraints->multifieldsAllowed = CLIPS_TRUE; }
   newSlot->multislot = multifieldSlot;
   newSlot->noDefault = CLIPS_FALSE;
   newSlot->defaultPresent = CLIPS_FALSE;
   newSlot->defaultDynamic = CLIPS_FALSE;
   newSlot->next = NULL;
   
   /*========================================*/
   /* Parse the primitive slot if it exists. */
   /*========================================*/

   InitializeConstraintParseRecord(&parsedConstraints);
   GetToken(readSource,inputToken);

   while (inputToken->type != RPAREN)
     {
      PPBackup();
      SavePPBuffer(" ");
      SavePPBuffer(inputToken->printForm);

      /*================================================*/
      /* Slot attributes begin with a left parenthesis. */
      /*================================================*/

      if (inputToken->type != LPAREN)
        {
         SyntaxErrorMessage("deftemplate");
         ReturnSlots(newSlot);
         DeftemplateError = CLIPS_TRUE;
         return(NULL);
        }

      /*=============================================*/
      /* The name of the attribute must be a symbol. */
      /*=============================================*/

      GetToken(readSource,inputToken);
      if (inputToken->type != SYMBOL)
        {
         SyntaxErrorMessage("deftemplate");
         ReturnSlots(newSlot);
         DeftemplateError = CLIPS_TRUE;
         return(NULL);
        }

      /*================================================================*/
      /* Determine if the attribute is one of the standard constraints. */
      /*================================================================*/

      if (StandardConstraint(ValueToString(inputToken->value)))
        {
         if (ParseStandardConstraint(readSource,(ValueToString(inputToken->value)),
                                     newSlot->constraints,&parsedConstraints,
                                     multifieldSlot) == CLIPS_FALSE)
           {
            DeftemplateError = CLIPS_TRUE;
            ReturnSlots(newSlot);
            return(NULL);
           }
        }

      /*=================================================*/
      /* else if the attribute is the default attribute, */
      /* then get the default list for this slot.        */
      /*=================================================*/

      else if ((strcmp(ValueToString(inputToken->value),"default") == 0) ||
               (strcmp(ValueToString(inputToken->value),"default-dynamic") == 0)) 
        {
#if FUZZY_DEFTEMPLATES
         if (newSlot->constraints->fuzzyValuesAllowed)
           { /* can't have defaults if FUZZY-VALUE type */
            SyntaxErrorMessage("default attribute \n(can't have defaults with FUZZY-VALUE type slot)");
            DeftemplateError = CLIPS_TRUE;
            ReturnSlots(newSlot);
            return(NULL);
           }
#endif
         /*======================================================*/
         /* Check to see if the default has already been parsed. */
         /*======================================================*/
         
         if (defaultFound)
           {
            AlreadyParsedErrorMessage("default attribute",NULL);
            DeftemplateError = CLIPS_TRUE;
            ReturnSlots(newSlot);
            return(NULL);
           }
         
         newSlot->noDefault = CLIPS_FALSE;
           
         /*=====================================================*/
         /* Determine whether the default is dynamic or static. */
         /*=====================================================*/
         
         if (strcmp(ValueToString(inputToken->value),"default") == 0) 
           { 
            newSlot->defaultPresent = CLIPS_TRUE;
            newSlot->defaultDynamic = CLIPS_FALSE;
           }
         else 
           { 
            newSlot->defaultPresent = CLIPS_FALSE;
            newSlot->defaultDynamic = CLIPS_TRUE;
           }
         
         /*===================================*/
         /* Parse the list of default values. */
         /*===================================*/
         
         defaultList = ParseDefault(readSource,multifieldSlot,(int) newSlot->defaultDynamic,
                                  CLIPS_TRUE,&noneSpecified,&deriveSpecified,&DeftemplateError);
         if (DeftemplateError == CLIPS_TRUE)
           {
            ReturnSlots(newSlot);
            return(NULL);
           }
         
         /*==================================*/
         /* Store the default with the slot. */
         /*==================================*/
         
         defaultFound = CLIPS_TRUE;
         if (deriveSpecified) newSlot->defaultPresent = CLIPS_FALSE;
         else if (noneSpecified)
           {
            newSlot->noDefault = CLIPS_TRUE;
            newSlot->defaultPresent = CLIPS_FALSE;
           }
         newSlot->defaultList = defaultList;
        }

      /*============================================*/
      /* Otherwise the attribute is an invalid one. */
      /*============================================*/

      else
        {
         SyntaxErrorMessage("slot attributes");
         ReturnSlots(newSlot);
         DeftemplateError = CLIPS_TRUE;
         return(NULL);
        }

      /*===================================*/
      /* Begin parsing the next attribute. */
      /*===================================*/

      GetToken(readSource,inputToken);
     }

#if FUZZY_DEFTEMPLATES
   /*============================================*/
   /* Fuzzy slots cannot have defaults           */
   /*============================================*/
   if (newSlot->constraints->fuzzyValuesAllowed)
     { /* can't have defaults if FUZZY-VALUE type */
       newSlot->noDefault = CLIPS_TRUE;
       newSlot->defaultPresent = CLIPS_FALSE;
       newSlot->defaultDynamic = CLIPS_FALSE;
     }
#endif

   /*============================*/
   /* Return the attribute list. */
   /*============================*/

   return(newSlot);
  }

#endif /* (! RUN_TIME) && (! BLOAD_ONLY) */

#endif /* DEFTEMPLATE_CONSTRUCT */


