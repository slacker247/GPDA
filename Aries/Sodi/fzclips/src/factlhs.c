   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  02/25/94            */
   /*                                                     */
   /*            FACT LHS PATTERN PARSING MODULE          */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains routines for integration of ordered and */
/*   deftemplate fact patterns with the defrule LHS pattern  */
/*   parser including routines for recognizing fact          */
/*   patterns, parsing ordered fact patterns, initiating the */
/*   parsing of deftemplate fact patterns, and creating the  */
/*   default initial-fact fact pattern.                      */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#define _FACTLHS_SOURCE_

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT && DEFRULE_CONSTRUCT && (! RUN_TIME) && (! BLOAD_ONLY)

#include <stdio.h>
#define _CLIPS_STDIO_

#include "router.h"
#include "reorder.h"
#include "pattern.h"
#include "tmpltpsr.h"
#include "tmpltdef.h"
#include "tmpltlhs.h"
#include "tmpltutl.h"     /* added 03-06-96 */
#include "modulutl.h"
#include "modulpsr.h"
#include "cstrcpsr.h"

#include "factlhs.h"

/***********************************************/
/* SequenceRestrictionParse: Parses an ordered */
/*   fact pattern conditional element.         */
/*                                             */
/*   <ordered-fact-pattern-CE>                 */
/*             ::= (<symbol> <constraint>+)    */
/***********************************************/
globle struct lhsParseNode *SequenceRestrictionParse(readSource,theToken)
  char *readSource;
  struct token *theToken;
  {
   struct lhsParseNode *topNode;
   struct lhsParseNode *nextField;
   
   /*================================================*/
   /* Create the pattern node for the relation name. */
   /*================================================*/

   topNode = GetLHSParseNode();
   topNode->type = SF_WILDCARD;
   topNode->negated = CLIPS_FALSE;
   topNode->index = -1;
   topNode->slotNumber = 1;
   topNode->bottom = GetLHSParseNode();
   topNode->bottom->type = SYMBOL;
   topNode->bottom->negated = CLIPS_FALSE;
   topNode->bottom->value = (VOID *) theToken->value;
   
   /*======================================================*/
   /* Connective constraints cannot be used in conjunction */
   /* with the first field of a pattern.                   */
   /*======================================================*/
   
   SavePPBuffer(" "); 
   GetToken(readSource,theToken);
   if ((theToken->type == OR_CONSTRAINT) || (theToken->type == AND_CONSTRAINT))
     {
      ReturnLHSParseNodes(topNode);
      SyntaxErrorMessage("the first field of a pattern");
      return(NULL);
     }   

   /*============================================================*/
   /* Treat the remaining constraints of an ordered fact pattern */
   /* as if they were contained in a multifield slot.            */
   /*============================================================*/
   
   nextField = RestrictionParse(readSource,theToken,CLIPS_TRUE,NULL,1,NULL,1);
   if (nextField == NULL)
     {
      ReturnLHSParseNodes(topNode);
      return(NULL);
     }
   topNode->right = nextField;
   
   /*================================================*/
   /* The pattern must end with a right parenthesis. */
   /*================================================*/
   
   if (theToken->type != RPAREN)
     {
      PPBackup();                        
      SavePPBuffer(" ");                  
      SavePPBuffer(theToken->printForm); 
      SyntaxErrorMessage("fact patterns");
      ReturnLHSParseNodes(topNode);
      return(NULL);
     }
     
   /*====================================*/
   /* Fix the pretty print output if the */
   /* slot contained no restrictions.    */
   /*====================================*/
   
   if (nextField->bottom == NULL)
     {
      PPBackup();  
      PPBackup();                        
      SavePPBuffer(")");  
     } 
     
   /*===================================*/
   /* If no errors, return the pattern. */
   /*===================================*/
   
   return(topNode);
  }

/****************************************************************/
/* CreateInitialFactPattern: Creates the pattern (initial-fact) */
/*   for use in rules which have no LHS patterns.               */
/****************************************************************/
globle struct lhsParseNode *CreateInitialFactPattern()
  {
   struct lhsParseNode *topNode;
   struct deftemplate *theDeftemplate;
   int count;

   /*==================================*/
   /* If the initial-fact deftemplate  */
   /* doesn't exist, then create it.   */
   /*==================================*/
   
   theDeftemplate = (struct deftemplate *) 
                    FindImportedConstruct("deftemplate",NULL,"initial-fact",
                                          &count,CLIPS_TRUE,NULL);
   if (theDeftemplate == NULL)
     { 
      PrintWarningID("FACTLHS",1,CLIPS_FALSE);
      PrintCLIPS(WWARNING,"Creating implied initial-fact deftemplate in module ");
      PrintCLIPS(WWARNING,GetDefmoduleName(GetCurrentModule()));
      PrintCLIPS(WWARNING,".\n");
      PrintCLIPS(WWARNING,"  You probably want to import this deftemplate from the MAIN module.\n");
      CreateImpliedDeftemplate(AddSymbol("initial-fact"),CLIPS_FALSE); 
     }

   /*====================================*/
   /* Create the (initial-fact) pattern. */
   /*====================================*/
   
   topNode = GetLHSParseNode();
   topNode->type = SF_WILDCARD;
   topNode->index = 0;
   topNode->slotNumber = 1;
   
   topNode->bottom = GetLHSParseNode();
   topNode->bottom->type = SYMBOL;
   topNode->bottom->value = (VOID *) AddSymbol("initial-fact");
   
   /*=====================*/
   /* Return the pattern. */
   /*=====================*/
   
   return(topNode);
  }
  
/**********************************************************************/
/* FactPatternParserFind: This function is the pattern find function  */
/*   for facts. It tells the pattern parsing code that the specified  */
/*   pattern can be parsed as a fact pattern. By default, any pattern */
/*   beginning with a symbol can be parsed as a fact pattern. Since   */
/*   all patterns begin with a symbol, it follows that all patterns   */
/*   can be parsed as a fact pattern.                                 */
/**********************************************************************/
#if IBM_TBC
#pragma argsused
#endif
globle int FactPatternParserFind(theRelation)
  SYMBOL_HN *theRelation;
  {
#if MAC_MPW || MAC_MCW  /* added 03-07-96 */
#pragma unused(theRelation)
#endif
   return(CLIPS_TRUE);
  }
  
/******************************************************/
/* FactPatternParse: This function is called to parse */
/*  both deftemplate and ordered fact patterns.       */
/******************************************************/
globle struct lhsParseNode *FactPatternParse(readSource,theToken)
  char *readSource;
  struct token *theToken;
  {
   struct deftemplate *theDeftemplate;
   int count;
   
   /*=========================================*/
   /* A module separator can not be included  */
   /* as part of the pattern's relation name. */
   /*=========================================*/
   
   if (FindModuleSeparator(ValueToString(theToken->value)))
     {
      IllegalModuleSpecifierMessage();
      return(NULL);
     }
     
   /*=========================================================*/
   /* Find the deftemplate associated with the relation name. */
   /*=========================================================*/
   
   theDeftemplate = (struct deftemplate *) 
                    FindImportedConstruct("deftemplate",NULL,ValueToString(theToken->value),
                                          &count,CLIPS_TRUE,NULL);
  
   if (count > 1)
     {
      AmbiguousReferenceErrorMessage("deftemplate",ValueToString(theToken->value));
      return(NULL);
     }
     
   /*======================================================*/
   /* If no deftemplate exists with the specified relation */
   /* name, then create an implied deftemplate.            */
   /*======================================================*/
   
   if (theDeftemplate == NULL)
     { 
#if DEFMODULE_CONSTRUCT
      if (FindImportExportConflict("deftemplate",((struct defmodule *) GetCurrentModule()),ValueToString(theToken->value)))
        {
         ImportExportConflictMessage("implied deftemplate",ValueToString(theToken->value),NULL,NULL);
         return(NULL);
        }
#endif /* DEFMODULE_CONSTRUCT */

      theDeftemplate = CreateImpliedDeftemplate((SYMBOL_HN *) theToken->value,CLIPS_TRUE);
     }
     
   /*===============================================*/
   /* If an explicit deftemplate exists, then parse */
   /* the pattern as a deftemplate pattern.         */
   /*===============================================*/
   
   if (theDeftemplate->implied == CLIPS_FALSE) 
     { return(DeftemplateLHSParse(readSource,theDeftemplate)); }
   
   /*================================*/
   /* Parse an ordered fact pattern. */
   /*================================*/
   
   return(SequenceRestrictionParse(readSource,theToken));
  }
  
#endif /* DEFTEMPLATE_CONSTRUCT && DEFRULE_CONSTRUCT && (! RUN_TIME) && (! BLOAD_ONLY) */



