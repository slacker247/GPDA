   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00 02/11/94             */
   /*                                                     */
   /*                DEFTEMPLATE LHS MODULE               */
   /*******************************************************/

/*************************************************************/
/* Purpose: Parses LHS deftemplate patterns.                 */
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

#define _TMPLTLHS_SOURCE_

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT && DEFRULE_CONSTRUCT && (! RUN_TIME) && (! BLOAD_ONLY)

#include <stdio.h>
#define _CLIPS_STDIO_
#include <string.h>

#include "constant.h"
#include "clipsmem.h"
#include "symbol.h"
#include "scanner.h"
#include "exprnpsr.h"
#include "router.h"
#include "constrnt.h"
#include "constrct.h"
#include "reorder.h"
#include "pattern.h"
#include "factrhs.h"
#include "modulutl.h"
#include "tmpltutl.h"  /* added 03-11-96 */
#include "tmpltdef.h"

#include "tmpltlhs.h"

#if FUZZY_DEFTEMPLATES    /* added 03-11-96 */
#include "fuzzylhs.h"
#include "fuzzypsr.h"
#endif

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if ANSI_COMPILER
   static struct lhsParseNode    *GetLHSSlots(char *,struct token *,struct deftemplate *,int *);
   static struct lhsParseNode    *GetSingleLHSSlot(char *,struct token *,
                                                   struct templateSlot *,int *,int);
#if FUZZY_DEFTEMPLATES     /* added 03-11-96 */
   static struct lhsParseNode    *GetFuzzySingleLHSSlot(char *,struct token *,
                                                   struct deftemplate *,int *,int);
#endif
   static BOOLEAN                 MultiplyDefinedLHSSlots(struct lhsParseNode *,SYMBOL_HN *);
#else
   static struct lhsParseNode    *GetLHSSlots();
   static struct lhsParseNode    *GetSingleLHSSlot();
#if FUZZY_DEFTEMPLATES
   static struct lhsParseNode    *GetFuzzySingleLHSSlot();
#endif
   static BOOLEAN                 MultiplyDefinedLHSSlots();
#endif

/**************************************************/
/* Locally defined global variables               */
/**************************************************/

#if FUZZY_DEFTEMPLATES       /* added 03-11-96 */

/* this is only used in this file and pattern.c (routine
   LiteralRestrictionParse)
*/
globle struct deftemplate *FuzzyDeftemplate;

#endif

/*********************************************/
/* DeftemplateLHSParse: Parses a LHS pattern */
/*   that uses the deftemplate format.       */
/*********************************************/
globle struct lhsParseNode *DeftemplateLHSParse(readSource,theDeftemplate)
  char *readSource;
  struct deftemplate *theDeftemplate;
  {
   struct lhsParseNode *head, *firstSlot;
   struct token theToken;
   int error;

   /*===============================================================*/
   /* Make sure the deftemplate name is not connected to subfields. */
   /*===============================================================*/

   GetToken(readSource,&theToken);
   if ((theToken.type == OR_CONSTRAINT) || (theToken.type == AND_CONSTRAINT))
     {
      SyntaxErrorMessage("deftemplate patterns");
      return(NULL);
     }

   /*===================================================*/
   /* Create the pattern node for the deftemplate name. */
   /*===================================================*/

   head = GetLHSParseNode();
   head->type = SF_WILDCARD;
   head->negated = CLIPS_FALSE;
   head->index = 0;
   head->slotNumber = 1;
   head->bottom = GetLHSParseNode();
   head->bottom->type = SYMBOL;
   head->bottom->negated = CLIPS_FALSE;
   head->bottom->value = (VOID *) theDeftemplate->header.name;

   /*==========================================*/
   /* Get the other fields in the deftemplate. */
   /*==========================================*/

   error = CLIPS_FALSE;

#if FUZZY_DEFTEMPLATES    /* added 03-11-96 */
   /*======================================*/
   /* May be a Fuzzy deftemplate.          */
   /*======================================*/

   /* global variable -- used here and in LiteralRestrictionParse ONLY ...
      Probably better to pass theDeftemplate thru a long series of
      routines than to add this global BUT ...
   */
    
   FuzzyDeftemplate = NULL; 
   
   if (theDeftemplate->fuzzyTemplate != NULL)
     { 		   
       FuzzyDeftemplate = theDeftemplate;

       /*=======================================*/
       /* Put a space between the template name */
       /* and the fuzzy set definition.         */
       /*=======================================*/

       PPBackup();
       SavePPBuffer(" ");
       SavePPBuffer(theToken.printForm);

       firstSlot = GetFuzzySingleLHSSlot(readSource,&theToken,theDeftemplate,&error,2);
     }
   else
     {
   
#endif /* FUZZY_DEFTEMPLATES */

   firstSlot = GetLHSSlots(readSource,&theToken,theDeftemplate,&error);

#if FUZZY_DEFTEMPLATES    /* added 03-11-96 */
     }
#endif /* FUZZY_DEFTEMPLATES */
   if (error)
     {
      ReturnLHSParseNodes(firstSlot);
      ReturnLHSParseNodes(head);
      return(NULL);
     }

   /*=========================*/
   /* Return the LHS pattern. */
   /*=========================*/

   head->right = firstSlot;
   return(head);
  }

/******************************************/
/* GetLHSSlots: Retrieves all of the slot */
/*   values used in a LHS pattern.        */
/******************************************/
static struct lhsParseNode *GetLHSSlots(readSource,tempToken,theDeftemplate,error)
  char *readSource;
  struct token *tempToken;
  struct deftemplate *theDeftemplate;
  int *error;
  {
   struct lhsParseNode *firstSlot = NULL, *nextSlot, *lastSlot = NULL;
   struct templateSlot *slotPtr;
   int position;

   /*=======================================================*/
   /* Continue parsing slot definitions until the pattern's */
   /* closing right parenthesis is encountered.             */
   /*=======================================================*/

   while (tempToken->type != RPAREN)
     {
      PPBackup();
      SavePPBuffer(" ");
      SavePPBuffer(tempToken->printForm);

      /*=================================================*/
      /* Slot definitions begin with a left parenthesis. */
      /*=================================================*/

      if (tempToken->type != LPAREN)
        {
         *error = CLIPS_TRUE;
         SyntaxErrorMessage("deftemplate patterns");
         ReturnLHSParseNodes(firstSlot);
         return(NULL);
        }

      /*====================*/
      /* Get the slot name. */
      /*====================*/

      GetToken(readSource,tempToken);
      if (tempToken->type != SYMBOL)
        {
         *error = CLIPS_TRUE;
         SyntaxErrorMessage("deftemplate patterns");
         ReturnLHSParseNodes(firstSlot);
         return(NULL);
        }

      /*==========================================================*/
      /* Determine if the slot name is valid for the deftemplate. */
      /*==========================================================*/

      if ((slotPtr = FindSlot(theDeftemplate,tempToken->value,&position)) == NULL)
        {
         *error = CLIPS_TRUE;
         InvalidDeftemplateSlotMessage(ValueToString(tempToken->value),
                                       ValueToString(theDeftemplate->header.name));
         ReturnLHSParseNodes(firstSlot);
         return(NULL);
        }

      /*============================================*/
      /* Determine if the slot is multiply defined. */
      /*============================================*/
      
      if (MultiplyDefinedLHSSlots(firstSlot,tempToken->value) == CLIPS_TRUE)
        {
         *error = CLIPS_TRUE;
         ReturnLHSParseNodes(firstSlot);
         return(NULL);
        }
        
      /*==============================================================*/
      /* Get the pattern matching values used in the slot definition. */
      /*==============================================================*/

      nextSlot = GetSingleLHSSlot(readSource,tempToken,slotPtr,error,position+1);
      if (*error)
        {
         ReturnLHSParseNodes(firstSlot);
         ReturnLHSParseNodes(nextSlot);
         return(NULL);
        }

      /*=====================================*/
      /* Add the slot definition to the list */
      /* of slot definitions already parsed. */
      /*=====================================*/

      if (lastSlot == NULL)
        { firstSlot = nextSlot; }
      else
        { lastSlot->right = nextSlot; }
        
      while (nextSlot->right != NULL) nextSlot = nextSlot->right;
      lastSlot = nextSlot;

      /*==============================*/
      /* Begin parsing the next slot. */
      /*==============================*/

      GetToken(readSource,tempToken);
     }

   /*===========================================================*/
   /* Return all the slot definitions found in the lhs pattern. */
   /*===========================================================*/

   return(firstSlot);
  }

/*****************************************************/
/* GetSingleLHSSlot: Get the pattern matching values */
/*   to be associated with a slot name.              */
/*****************************************************/
static struct lhsParseNode *GetSingleLHSSlot(readSource,tempToken,slotPtr,error,position)
  char *readSource;
  struct token *tempToken;
  struct templateSlot *slotPtr;
  int *error;
  int position;
  {
   struct lhsParseNode *nextSlot;
   SYMBOL_HN *slotName;

   /*================================================*/
   /* Get the slot name and read in the first token. */
   /*================================================*/
   
   slotName = (SYMBOL_HN *) tempToken->value;
   SavePPBuffer(" ");
   GetToken(readSource,tempToken);

   /*====================================*/
   /* Get value for a single field slot. */
   /*====================================*/

   if (slotPtr->multislot == CLIPS_FALSE)
     {
      /*=======================*/
      /* Get the single value. */
      /*=======================*/

      nextSlot = RestrictionParse(readSource,tempToken,CLIPS_FALSE,
                                  slotPtr->slotName,position - 1,slotPtr->constraints,0);
      if (nextSlot == NULL)
        {
         *error = CLIPS_TRUE;
         return(NULL);
        }
                     
      /*======================================*/
      /* Multi field wildcards and variables  */
      /* not allowed in a single field slot.  */
      /*======================================*/

      if ((nextSlot->type == MF_VARIABLE) ||
          (nextSlot->type == MULTIFIELD))
        {
         SingleFieldSlotCardinalityError(slotPtr->slotName->contents);
         *error = CLIPS_TRUE;
         ReturnLHSParseNodes(nextSlot);
         return(NULL);
        }
     }

   /*===================================*/
   /* Get values for a multifield slot. */
   /*===================================*/

   else
     {
      nextSlot = RestrictionParse(readSource,tempToken,CLIPS_TRUE,slotName,position - 1,
                                  slotPtr->constraints,1);
      if (nextSlot == NULL)
        {
         *error = CLIPS_TRUE;
         return(NULL);
        }
     }

   /*========================================================*/
   /* The slot definition must end with a right parenthesis. */
   /*========================================================*/

   if (tempToken->type != RPAREN)
     {
      PPBackup();                        
      SavePPBuffer(" ");                  
      SavePPBuffer(tempToken->printForm); 
      SyntaxErrorMessage("deftemplate patterns");
      *error = CLIPS_TRUE;
      ReturnLHSParseNodes(nextSlot);
      return(NULL);
     }
   
   /*===============================================*/
   /* Fix the pretty print output if the multifield */
   /* slot contained no restrictions.               */
   /*===============================================*/
   
   if ((nextSlot->bottom == NULL) && slotPtr->multislot)
     {
      PPBackup();  
      PPBackup();                        
      SavePPBuffer(")");  
     } 

   /*=================================*/
   /* Add the slot values to the slot */
   /* structure and return it.        */
   /*=================================*/

   return(nextSlot);
  }

#if FUZZY_DEFTEMPLATES      /* added 03-11-96 */

/*****************************************************/
/* GetFuzzySingleLHSSlot: Get the pattern matching   */
/*   values for a fuzzy deftemplate pattern slot     */
/*                                                   */
/*  e.g.   (temp very cold)   or (temp ?)  or        */
/*         (temp ?x&very cold)                       */
/*****************************************************/
static struct lhsParseNode *GetFuzzySingleLHSSlot(readSource,tempToken,theDeftemplate,error,position)
  char *readSource;
  struct token *tempToken;
  struct deftemplate *theDeftemplate;
  int *error;
  int position;
  {
   struct lhsParseNode *nextSlot;
   struct templateSlot *slotPtr = theDeftemplate->slotList;

   /*====================================*/
   /* Get value for a single field slot. */
   /*====================================*/

   /*=======================*/
   /* Get the single value. */
   /*=======================*/

   nextSlot = RestrictionParse(readSource,tempToken,CLIPS_FALSE,
                               slotPtr->slotName,position - 1,
                               slotPtr->constraints,0);

  if (nextSlot == NULL)
     {
      *error = CLIPS_TRUE;
      return(NULL);
     }
                     
   /*======================================*/
   /* Multi field wildcards and variables  */
   /* not allowed in a single field slot.  */
   /*======================================*/

   if ((nextSlot->type == MF_VARIABLE) ||
       (nextSlot->type == MULTIFIELD))
     {
      SingleFieldSlotCardinalityError(slotPtr->slotName->contents);
      *error = CLIPS_TRUE;
      ReturnLHSParseNodes(nextSlot);
      return(NULL);
     }

   /*========================================================*/
   /* The slot definition must end with a right parenthesis. */
   /*========================================================*/

   if (tempToken->type != RPAREN)
     {
      PPBackup();                        
      SavePPBuffer(" ");                  
      SavePPBuffer(tempToken->printForm); 
      SyntaxErrorMessage("deftemplate patterns");
      *error = CLIPS_TRUE;
      ReturnLHSParseNodes(nextSlot);
      return(NULL);
     }

   /*=================================*/
   /* Add the slot values to the slot */
   /* structure and return it.        */
   /*=================================*/

   return(nextSlot);
  }

#endif   /* FUZZY_DEFTEMPLATES */


/******************************************************/
/* MultiplyDefinedLHSSlots: Determines if a slot name */
/*   was used more than once in a LHS pattern.        */
/******************************************************/
static BOOLEAN MultiplyDefinedLHSSlots(theSlots,slotName)
  struct lhsParseNode *theSlots;
  SYMBOL_HN *slotName;
  {
   for (;
        theSlots != NULL;
        theSlots = theSlots->right)
     {
      if (theSlots->slot == slotName)
        {         
         AlreadyParsedErrorMessage("slot ",ValueToString(slotName));
         return(CLIPS_TRUE);
        }
     }

   return(CLIPS_FALSE);
  }

#endif /* DEFTEMPLATE_CONSTRUCT && DEFRULE_CONSTRUCT && (! RUN_TIME) && (! BLOAD_ONLY) */


