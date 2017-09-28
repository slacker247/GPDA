   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */ 
   /*                                                     */
   /*             CLIPS Version 6.00  06/17/94            */
   /*                                                     */
   /*                 RULE PATTERN MODULE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides the mechanism for recognizing and       */
/*   parsing the various types of patterns that can be used  */
/*   in the LHS of a rule. In CLIPS 6.0, the only pattern    */
/*   types provided are for deftemplate and instance         */   
/*   patterns.                                               */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#define _PATTERN_SOURCE_

#include "setup.h"

#include <stdio.h>
#define _CLIPS_STDIO_

#if DEFRULE_CONSTRUCT

#include "constant.h"
#include "clipsmem.h"
#include "match.h"
#include "reteutil.h"
#include "constrnt.h"
#include "exprnpsr.h"
#include "router.h"
#include "cstrnchk.h"
#include "cstrnutl.h"
#include "rulecmp.h"

#include "pattern.h"

#if FUZZY_DEFTEMPLATES   /* added 03-11-96 */
#include "tmpltdef.h"
#include "tmpltlhs.h"
#include "fuzzylhs.h"
#include "fuzzypsr.h"
#endif

#define MAX_POSITIONS 8

/**************/
/* STRUCTURES */
/**************/

struct reservedSymbol
  {
   char *theSymbol;
   char *reservedBy;
   struct reservedSymbol *next;
  };

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if (! RUN_TIME) && (! BLOAD_ONLY)
#if ANSI_COMPILER
#if FUZZY_DEFTEMPLATES  /* added 03-11-96 */
   static struct lhsParseNode    *ConjunctiveRestrictionParse(char *,struct token *,int *,
                                                             CONSTRAINT_RECORD *);
   static struct lhsParseNode    *LiteralRestrictionParse(char *,struct token *,int *,
                                                             CONSTRAINT_RECORD *);
#else
   static struct lhsParseNode    *ConjunctiveRestrictionParse(char *,struct token *,int *);
   static struct lhsParseNode    *LiteralRestrictionParse(char *,struct token *,int *);
#endif
   static int                     CheckForVariableMixing(struct lhsParseNode *);
   static VOID                    TallyFieldTypes(struct lhsParseNode *);
#else
   static struct lhsParseNode    *ConjunctiveRestrictionParse();
   static struct lhsParseNode    *LiteralRestrictionParse();
   static int                     CheckForVariableMixing();
   static VOID                    TallyFieldTypes();
#endif
#endif

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   globle struct patternParser *ListOfPatternParsers = NULL;

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static struct patternParser  *PatternParserArray[MAX_POSITIONS];
   static int                    nextPosition = 0;
   static struct reservedSymbol *ListOfReservedPatternSymbols = NULL;
  
/******************************************************************/
/* AddReservedPatternSymbol: Adds a symbol to the list of symbols */
/*  that are restricted for use in patterns. For example, the     */
/*  deftemplate construct cannot use the symbol "object", since   */
/*  this needs to be reserved for object patterns. Some symbols,  */
/*  such as "exists" are completely reserved and can not be used  */
/*  to start any type of pattern CE.                              */
/******************************************************************/
VOID AddReservedPatternSymbol(theSymbol,reservedBy)
  char *theSymbol;
  char *reservedBy;
  {
   struct reservedSymbol *newSymbol;
   
   newSymbol = get_struct(reservedSymbol);
   newSymbol->theSymbol = theSymbol;
   newSymbol->reservedBy = reservedBy;
   newSymbol->next = ListOfReservedPatternSymbols;
   ListOfReservedPatternSymbols = newSymbol;
  }
  
/******************************************************************/
/* ReservedPatternSymbol: Returns TRUE if the specified symbol is */
/*   a reserved pattern symbol, otherwise FALSE is returned. If   */
/*   the construct which is trying to use the symbol is the same  */
/*   construct that reserved the symbol, then FALSE is returned.  */
/******************************************************************/
BOOLEAN ReservedPatternSymbol(theSymbol,checkedBy)
  char *theSymbol;
  char *checkedBy;
  {
   struct reservedSymbol *currentSymbol;
   
   for (currentSymbol = ListOfReservedPatternSymbols;
        currentSymbol != NULL;
        currentSymbol = currentSymbol->next)
     {
      if (strcmp(theSymbol,currentSymbol->theSymbol) == 0)
        {
         if ((currentSymbol->reservedBy == NULL) || (checkedBy ==  NULL))
           { return(CLIPS_TRUE); }
           
         if (strcmp(checkedBy,currentSymbol->reservedBy) == 0) return(CLIPS_FALSE);
         
         return(CLIPS_TRUE);
        }
     }
   
   return(CLIPS_FALSE);
  }

/********************************************************/
/* ReservedPatternSymbolErrorMsg: Generic error message */
/*   for attempting to use a reserved pattern symbol.   */
/********************************************************/
VOID ReservedPatternSymbolErrorMsg(theSymbol,usedFor)
  char *theSymbol;
  char *usedFor;
  {
   PrintErrorID("PATTERN",1,CLIPS_TRUE);
   PrintCLIPS(WERROR,"The symbol ");
   PrintCLIPS(WERROR,theSymbol);
   PrintCLIPS(WERROR," has special meaning\n");
   PrintCLIPS(WERROR,"and may not be used as ");
   PrintCLIPS(WERROR,usedFor);
   PrintCLIPS(WERROR,".\n");
  }
  
/************************************************************/
/* GetNextEntity: Utility routine for accessing all of the  */
/*   data entities that can match patterns. Currently facts */
/*   and instances are the only data entities available.    */
/************************************************************/
globle VOID GetNextPatternEntity(theParser,theEntity)
  struct patternParser **theParser;
  struct patternEntity **theEntity;
  {
   /*=============================================================*/
   /* If the current parser is NULL, then we want to retrieve the */
   /* very first data entity. The traversal of entities is done   */
   /* by entity type (e.g. all facts are traversed followed by    */
   /* all instances). To get the first entity type to traverse,   */
   /* the current parser is set to the first parser on the list   */
   /* of pattern parsers.                                         */
   /*=============================================================*/
        
   if (*theParser == NULL) 
     { 
      *theParser = ListOfPatternParsers;
      *theEntity = NULL;
     }
     
   /*================================================================*/
   /* Otherwise try to retrieve the next entity following the entity */
   /* returned by the last call to GetNextEntity. If that entity was */
   /* the last of its data type, then move on to the next pattern    */
   /* parser, otherwise return that entity as the next one.          */
   /*================================================================*/
   
   else if (theEntity != NULL)
     { 
      *theEntity = (struct patternEntity *) 
                   (*(*theParser)->entityType->base.getNextFunction)(*theEntity);

      if ((*theEntity) != NULL) return;
      
      *theParser = (*theParser)->next;
     } 
     
   /*===============================================================*/
   /* Otherwise, we encountered a situation which should not occur. */
   /* Once a NULL entity is returned from GetNextEntity, it should  */
   /* not be passed back to GetNextEntity.                          */
   /*===============================================================*/
   
   else
     {
      CLIPSSystemError("PATTERN",1);
      ExitCLIPS(4);
     }
   
   /*================================================*/
   /* Keep looping through the lists of entities and */
   /* pattern parsers until an entity is found.      */
   /*================================================*/
   
   while ((*theEntity == NULL) && (*theParser != NULL))
     {
      *theEntity = (struct patternEntity *) 
                   (*(*theParser)->entityType->base.getNextFunction)(*theEntity);
                   
      if (*theEntity != NULL) return;
      
      *theParser = (*theParser)->next;
     }
     
   return;
  }
  
/**************************************************************/
/* DetachPattern: Detaches a pattern from the pattern network */
/*   by calling the appropriate function for the data type    */
/*   associated with the pattern.                             */
/**************************************************************/
VOID DetachPattern(rhsType,theHeader)
  int rhsType;
  struct patternNodeHeader *theHeader;
  {
   if (PatternParserArray[rhsType] != NULL)
     {
      FlushAlphaBetaMemory(theHeader->alphaMemory);
      (*PatternParserArray[rhsType]->removePatternFunction)(theHeader);
     }
  }
   
/**************************************************/
/* AddPatternParser: Adds a pattern type to the   */
/*   list of pattern parsers used to detect valid */
/*   patterns in the LHS of a rule.               */
/**************************************************/
globle BOOLEAN AddPatternParser(name,priority,entityType,
                      recognizeFunction,parseFunction,
                      postAnalysisFunction,addPatternFunction,removePatternFunction,
                      genJNConstantFunction,replaceGetJNValueFunction,
                      genGetJNValueFunction,genCompareJNValuesFunction,
                      genPNConstantFunction,replaceGetPNValueFunction,
                      genGetPNValueFunction,genComparePNValuesFunction,
                      returnUserDataFunction,copyUserDataFunction,
                      markIRPatternFunction,incrementalResetFunction,
                      initialPatternFunction,
                      codeReferenceFunction)
  char *name;
  int priority;
  struct patternEntityRecord *entityType;
#if ANSI_COMPILER
  int (*recognizeFunction)(SYMBOL_HN *);
  struct lhsParseNode *(*parseFunction)(char *,struct token *);
  int (*postAnalysisFunction)(struct lhsParseNode *);
  struct patternNodeHeader *(*addPatternFunction)(struct lhsParseNode *);
  VOID (*removePatternFunction)(struct patternNodeHeader *);
  struct expr *(*genJNConstantFunction)(struct lhsParseNode *);
  VOID (*replaceGetJNValueFunction)(struct expr *,struct lhsParseNode *); /* 10 */
  struct expr *(*genGetJNValueFunction)(struct lhsParseNode *);
  struct expr *(*genCompareJNValuesFunction)(struct lhsParseNode *,struct lhsParseNode *);
  struct expr *(*genPNConstantFunction)(struct lhsParseNode *);
  VOID (*replaceGetPNValueFunction)(struct expr *,struct lhsParseNode *);
  struct expr *(*genGetPNValueFunction)(struct lhsParseNode *);
  struct expr *(*genComparePNValuesFunction)(struct lhsParseNode *,struct lhsParseNode *);
  VOID (*returnUserDataFunction)(VOID *);
  VOID *(*copyUserDataFunction)(VOID *);
  VOID (*markIRPatternFunction)(struct patternNodeHeader *,int);
  VOID (*incrementalResetFunction)(VOID);
  struct lhsParseNode *(*initialPatternFunction)(VOID);
  VOID (*codeReferenceFunction)(VOID *,FILE *,int,int);
#else
  int (*recognizeFunction)();
  struct lhsParseNode *(*parseFunction)();
  int (*postAnalysisFunction)();
  struct patternNodeHeader *(*addPatternFunction)();
  VOID (*removePatternFunction)();
  struct expr *(*genJNConstantFunction)();
  VOID (*replaceGetJNValueFunction)();
  struct expr *(*genGetJNValueFunction)();
  struct expr *(*genCompareJNValuesFunction)();
  struct expr *(*genPNConstantFunction)();
  VOID (*replaceGetPNValueFunction)();
  struct expr *(*genGetPNValueFunction)();
  struct expr *(*genComparePNValuesFunction)();
  VOID (*returnUserDataFunction)();
  VOID *(*copyUserDataFunction)();
  VOID (*markIRPatternFunction)();
  VOID (*incrementalResetFunction)();
  struct lhsParseNode *(*initialPatternFunction)();
  VOID (*codeReferenceFunction)();
#endif
  {
   struct patternParser *newPtr, *currentPtr, *lastPtr = NULL;

   /*============================================*/
   /* Check to see that the limit for the number */
   /* of pattern parsers has not been exceeded.  */
   /*============================================*/
   
   if (nextPosition >= MAX_POSITIONS) return(CLIPS_FALSE);
   
   /*================================*/
   /* Create the new pattern parser. */
   /*================================*/
   
   newPtr = get_struct(patternParser);

   newPtr->name = name;
   newPtr->entityType = entityType;
   newPtr->recognizeFunction = recognizeFunction;
   newPtr->parseFunction = parseFunction;
   newPtr->postAnalysisFunction = postAnalysisFunction;
   newPtr->addPatternFunction = addPatternFunction;
   newPtr->removePatternFunction = removePatternFunction;
   newPtr->genJNConstantFunction = genJNConstantFunction;
   newPtr->replaceGetJNValueFunction = replaceGetJNValueFunction;
   newPtr->genGetJNValueFunction = genGetJNValueFunction;
   newPtr->genCompareJNValuesFunction = genCompareJNValuesFunction;
   newPtr->genPNConstantFunction = genPNConstantFunction;
   newPtr->replaceGetPNValueFunction = replaceGetPNValueFunction;
   newPtr->genGetPNValueFunction = genGetPNValueFunction;
   newPtr->genComparePNValuesFunction = genComparePNValuesFunction;
   newPtr->returnUserDataFunction = returnUserDataFunction;
   newPtr->copyUserDataFunction = copyUserDataFunction;
   newPtr->markIRPatternFunction = markIRPatternFunction;
   newPtr->incrementalResetFunction = incrementalResetFunction;
   newPtr->initialPatternFunction = initialPatternFunction;
   newPtr->codeReferenceFunction = codeReferenceFunction;
   newPtr->priority = priority;
   newPtr->positionInArray = nextPosition;
   PatternParserArray[nextPosition] = newPtr;
   nextPosition++;

   /*================================*/
   /* Add the parser to the list of  */
   /* parsers based on its priority. */
   /*================================*/
   
   if (ListOfPatternParsers == NULL)
     {
      newPtr->next = NULL;
      ListOfPatternParsers = newPtr;
      return(CLIPS_TRUE);
     }

   currentPtr = ListOfPatternParsers;
   while ((currentPtr != NULL) ? (priority < currentPtr->priority) : CLIPS_FALSE)
     {
      lastPtr = currentPtr;
      currentPtr = currentPtr->next;
     }

   if (lastPtr == NULL)
     {
      newPtr->next = ListOfPatternParsers;
      ListOfPatternParsers = newPtr;
     }
   else
     {
      newPtr->next = currentPtr;
      lastPtr->next = newPtr;
     }

   return(CLIPS_TRUE);
  }
  
/****************************************************/
/* FindPatternParser: Searches for a pattern parser */
/*  that can parse a pattern beginning with the     */
/*  specified keyword (e.g. "object").              */
/****************************************************/
globle struct patternParser *FindPatternParser(name)
  char *name;
  {
   struct patternParser *tempParser;

   for (tempParser = ListOfPatternParsers;
        tempParser != NULL;
        tempParser = tempParser->next)
     { if (strcmp(tempParser->name,name) == 0) return(tempParser); }
     
   return(NULL);
  }

/******************************************************/
/* GetPatternParser: Returns a pointer to the pattern */
/*    parser for the specified data entity.           */
/******************************************************/
struct patternParser *GetPatternParser(rhsType)
  int rhsType;
  {
   return(PatternParserArray[rhsType]);
  }

#if CONSTRUCT_COMPILER && (! RUN_TIME)

/*************************************************************/
/* PatternNodeHeaderToCode: Writes the C code representation */
/*   of a patternNodeHeader data structure.                  */
/*************************************************************/
globle VOID PatternNodeHeaderToCode(fp,theHeader,imageID,maxIndices)
  FILE *fp;
  struct patternNodeHeader *theHeader;
  int imageID;
  int maxIndices;
  {
   fprintf(fp,"{NULL,NULL,");
   
   if (theHeader->entryJoin == NULL)
     { fprintf(fp,"NULL,"); }
   else
     {
      fprintf(fp,"&%s%d_%d[%d],",
                 JoinPrefix(),imageID,
                 (((int) theHeader->entryJoin->bsaveID) / maxIndices) + 1,
                 ((int) theHeader->entryJoin->bsaveID) % maxIndices);
     }
   
   fprintf(fp,"%d,%d,%d,0,0,%d,%d}",theHeader->singlefieldNode,
                                     theHeader->multifieldNode,
                                     theHeader->stopNode,
                                     theHeader->beginSlot,
                                     theHeader->endSlot);
  }

#endif /* CONSTRUCT_COMPILER && (! RUN_TIME) */

#if (! RUN_TIME) && (! BLOAD_ONLY)

/****************************************************************/
/* PostPatternAnalysis: Calls the post analysis routines for    */
/*   each of the pattern parsers to allow additional processing */
/*   by the pattern parser after the variable analysis routines */
/*   have analyzed the LHS patterns.                            */
/****************************************************************/
globle BOOLEAN PostPatternAnalysis(theLHS)
  struct lhsParseNode *theLHS;
  {
   struct lhsParseNode *patternPtr;
   struct patternParser *tempParser;
   
   for (patternPtr = theLHS; patternPtr != NULL; patternPtr = patternPtr->bottom)
     {
      if ((patternPtr->type == PATTERN_CE) && (patternPtr->patternType != NULL))
        {
         tempParser = patternPtr->patternType;
         if (tempParser->postAnalysisFunction != NULL)
           { if ((*tempParser->postAnalysisFunction)(patternPtr)) return(CLIPS_TRUE); }
        }
     }  
      
   return(CLIPS_FALSE);
  }

/******************************************************************/
/* RestrictionParse: Parses a single field within a pattern. This */
/*    field may either be a single field wildcard, a multifield   */
/*    wildcard, a single field variable, a multifield variable,   */
/*    or a series of connected constraints.                       */
/*                                                                */
/* <constraint> ::= ? |                                           */
/*                  $? |                                          */
/*                  <connected-constraint>                        */
/******************************************************************/
struct lhsParseNode *RestrictionParse(readSource,theToken,multifieldSlot,
                                      theSlot,slotNumber,theConstraints,
                                      position)
  char *readSource;
  struct token *theToken;
  int multifieldSlot;
  struct symbolHashNode *theSlot;
  int slotNumber;
  CONSTRAINT_RECORD *theConstraints;
  int position;
  {
   struct lhsParseNode *topNode = NULL, *lastNode = NULL, *nextNode;
   int numberOfSingleFields = 0;
   int numberOfMultifields = 0;
   int startPosition = position;
   int error = CLIPS_FALSE;
   CONSTRAINT_RECORD *tempConstraints;
   
   /*==================================================*/
   /* Keep parsing fields until a right parenthesis is */
   /* encountered. This will either indicate the end   */
   /* of an instance or deftemplate slot or the end of */
   /* an ordered fact.                                 */
   /*==================================================*/
   
   while (theToken->type != RPAREN)
     {
      /*========================================*/
      /* Look for either a single or multifield */
      /* wildcard or a conjuctive restriction.  */
      /*========================================*/
      
      if ((theToken->type == SF_WILDCARD) ||
          (theToken->type == MF_WILDCARD))
        {
         nextNode = GetLHSParseNode();
         nextNode->type = theToken->type;
         nextNode->negated = CLIPS_FALSE;
         GetToken(readSource,theToken);
        }
      else
        {
#if FUZZY_DEFTEMPLATES  /* added 03-11-96 */
         nextNode = ConjunctiveRestrictionParse(readSource,theToken,&error,theConstraints);
#else 
         nextNode = ConjunctiveRestrictionParse(readSource,theToken,&error);
#endif
         if (nextNode == NULL)
           {
            ReturnLHSParseNodes(topNode);
            return(NULL);
           }
        }
              
      /*========================================================*/
      /* Fix up the pretty print representation of a multifield */
      /* slot so that the fields don't run together.            */
      /*========================================================*/
      
      if ((theToken->type != RPAREN) && (multifieldSlot == CLIPS_TRUE))
        {
         PPBackup();
         SavePPBuffer(" ");
         SavePPBuffer(theToken->printForm);
        }
      
      /*========================================*/
      /* Keep track of the number of single and */
      /* multifield restrictions encountered.   */
      /*========================================*/
      
      if ((nextNode->type == SF_WILDCARD) || (nextNode->type == SF_VARIABLE))
        { numberOfSingleFields++; }
      else
        { numberOfMultifields++; }
        
      /*===================================*/
      /* Assign the slot name and indices. */
      /*===================================*/
      
      nextNode->slot = theSlot;
      nextNode->slotNumber = slotNumber;
      nextNode->index = position++;
      
      /*==============================================*/
      /* If we're not dealing with a multifield slot, */
      /* attach the constraints directly to the node  */
      /* and return.                                  */
      /*==============================================*/
      
      if (! multifieldSlot)
        {
         if (theConstraints == NULL) 
           { 
            if (nextNode->type == SF_VARIABLE)
              { nextNode->constraints = GetConstraintRecord(); }
            else nextNode->constraints = NULL;
           }
         else nextNode->constraints = theConstraints; 
         return(nextNode);
        }
        
      /*====================================================*/
      /* Attach the restriction to the list of restrictions */
      /* already parsed for this slot or ordered fact.      */
      /*====================================================*/
      
      if (lastNode == NULL) topNode = nextNode;
      else lastNode->right = nextNode;
      
      lastNode = nextNode;
     }

   /*=====================================================*/
   /* Once we're through parsing, check to make sure that */
   /* a single field slot was given a restriction. If the */
   /* following test fails, then we know we're dealing    */
   /* with a multifield slot.                             */
   /*=====================================================*/

   if ((topNode == NULL) && (! multifieldSlot))
     {
      SyntaxErrorMessage("defrule");
      return(NULL);
     }
      
   /*===============================================*/
   /* Loop through each of the restrictions in the  */
   /* list of restrictions for the multifield slot. */
   /*===============================================*/
    
   for (nextNode = topNode; nextNode != NULL; nextNode = nextNode->right)
     {
      /*===================================================*/
      /* Assign a constraint record to each constraint. If */
      /* the slot has an explicit constraint, then copy    */
      /* this and store it with the constraint. Otherwise, */
      /* create a constraint record for a single field     */
      /* constraint and skip the constraint modifications  */
      /* for a multifield constraint.                      */
      /*===================================================*/
          
      if (theConstraints == NULL)
        {
         if (nextNode->type == SF_VARIABLE)
           { nextNode->constraints = GetConstraintRecord(); }
         else
           { continue; }
        }
      else
        { nextNode->constraints = CopyConstraintRecord(theConstraints); }
        
      /*==========================================*/
      /* Remove the min and max field constraints */
      /* for the entire slot from the constraint  */
      /* record for this single constraint.       */
      /*==========================================*/
      
      ReturnExpression(nextNode->constraints->minFields);
      ReturnExpression(nextNode->constraints->maxFields);
      nextNode->constraints->minFields = GenConstant(SYMBOL,NegativeInfinity);
      nextNode->constraints->maxFields = GenConstant(SYMBOL,PositiveInfinity);
      nextNode->derivedConstraints = CLIPS_TRUE;
          
      /*====================================================*/
      /* If we're not dealing with a multifield constraint, */
      /* then no further modifications are needed to the    */
      /* min and max constraints for this constraint.       */
      /*====================================================*/
      
      if ((nextNode->type != MF_WILDCARD) && (nextNode->type != MF_VARIABLE))
        { continue; }
      
      /*==========================================================*/
      /* Create a separate constraint record to keep track of the */
      /* cardinality information for this multifield constraint.  */
      /*==========================================================*/
      
      tempConstraints = GetConstraintRecord();
      SetConstraintType(MULTIFIELD,tempConstraints);
      tempConstraints->singlefieldsAllowed = CLIPS_FALSE;
      tempConstraints->multifield = nextNode->constraints;
      nextNode->constraints = tempConstraints;
         
      /*=====================================================*/
      /* Adjust the min and max field values for this single */
      /* multifield constraint based on the min and max      */
      /* fields for the entire slot and the number of single */
      /* field values contained in the slot.                 */
      /*=====================================================*/
      
      if (theConstraints->maxFields->value != PositiveInfinity)
        {
         ReturnExpression(tempConstraints->maxFields);
         tempConstraints->maxFields = GenConstant(INTEGER,AddLong(ValueToLong(theConstraints->maxFields->value) - numberOfSingleFields));
        }
            
      if ((numberOfMultifields == 1) && (theConstraints->minFields->value != NegativeInfinity))
        {
         ReturnExpression(tempConstraints->minFields);
         tempConstraints->minFields = GenConstant(INTEGER,AddLong(ValueToLong(theConstraints->minFields->value) - numberOfSingleFields));
        }
     }
      
   /*================================================*/
   /* If a multifield slot is being parsed, place a  */
   /* node on top of the list of constraints parsed. */
   /*================================================*/
   
   if (multifieldSlot)
     {
      nextNode = GetLHSParseNode();
      nextNode->type = MF_WILDCARD;
      nextNode->multifieldSlot = CLIPS_TRUE;
      nextNode->bottom = topNode;
      nextNode->slot = theSlot;
      nextNode->slotNumber = slotNumber;
      nextNode->index = startPosition;
      nextNode->constraints = theConstraints;
      topNode = nextNode;
      TallyFieldTypes(topNode->bottom);
     }
     
   /*=================================*/
   /* Return the list of constraints. */
   /*=================================*/
   
   return(topNode);
  }

/***************************************************************/
/* TallyFieldTypes: Determines the number of single field and  */
/*   multifield variables and wildcards that appear before and */
/*   after each restriction found in a multifield slot.        */
/***************************************************************/
static VOID TallyFieldTypes(theRestrictions)
  struct lhsParseNode *theRestrictions;
  {
   struct lhsParseNode *tempNode1, *tempNode2, *tempNode3;
   int totalSingleFields = 0, totalMultiFields = 0;
   int runningSingleFields = 0, runningMultiFields = 0;
   
   /*========================================*/
   /* Compute the total number of single and */
   /* multifield variables and wildcards.    */
   /*========================================*/
   
   for (tempNode1 = theRestrictions; tempNode1 != NULL; tempNode1 = tempNode1->right)
     {
      if ((tempNode1->type == SF_VARIABLE) || (tempNode1->type == SF_WILDCARD))
        { totalSingleFields++; }
      else
        { totalMultiFields++; }
     }
     
   /*======================================================*/
   /* Loop through each constraint tallying the numbers of */
   /* the variable types before and after along the way.   */
   /*======================================================*/
   
   for (tempNode1 = theRestrictions; tempNode1 != NULL; tempNode1 = tempNode1->right)
     {
      /*===================================*/
      /* Assign the values to the "binding */
      /* variable" node of the constraint. */
      /*===================================*/
      
      tempNode1->singleFieldsBefore = runningSingleFields;
      tempNode1->multiFieldsBefore = runningMultiFields;
      tempNode1->withinMultifieldSlot = CLIPS_TRUE;
      
      if ((tempNode1->type == SF_VARIABLE) || (tempNode1->type == SF_WILDCARD))
        { 
         tempNode1->singleFieldsAfter = totalSingleFields - (runningSingleFields + 1);
         tempNode1->multiFieldsAfter = totalMultiFields - runningMultiFields;
        }
      else
        { 
         tempNode1->singleFieldsAfter = totalSingleFields - runningSingleFields;
         tempNode1->multiFieldsAfter = totalMultiFields - (runningMultiFields + 1);
        }
         
      /*=====================================================*/
      /* Assign the values to each of the and (&) and or (|) */
      /* connected constraints within the constraint.        */
      /*=====================================================*/
      
      for (tempNode2 = tempNode1->bottom; tempNode2 != NULL; tempNode2 = tempNode2->bottom)
        {
         for (tempNode3 = tempNode2; tempNode3 != NULL; tempNode3 = tempNode3->right)
           {
            tempNode3->singleFieldsBefore = tempNode1->singleFieldsBefore;
            tempNode3->singleFieldsAfter = tempNode1->singleFieldsAfter;
            tempNode3->multiFieldsBefore = tempNode1->multiFieldsBefore;
            tempNode3->multiFieldsAfter = tempNode1->multiFieldsAfter;
            tempNode3->withinMultifieldSlot = CLIPS_TRUE;
           }
        }
        
      /*=======================================*/
      /* Calculate the running total of single */
      /* and multifield constraints.           */
      /*=======================================*/
      
      if ((tempNode1->type == SF_VARIABLE) || (tempNode1->type == SF_WILDCARD))
        { runningSingleFields++; }
      else
        { runningMultiFields++; }
     }
  }
  
/*******************************************************************/
/* ConjunctiveRestrictionParse: Parses a single constraint field in */
/*   a pattern that is not a single field wildcard, multifield     */
/*   wildcard, or multifield variable. The field may consist of a  */
/*   number of subfields tied together using the & connective      */
/*   constraint and/or the | connective constraint.                */
/*                                                                 */
/* <connected-constraint>                                          */
/*            ::= <single-constraint> |                            */
/*                <single-constraint> & <connected-constraint> |   */
/*                <single-constraint> | <connected-constraint>     */
/*******************************************************************/
#if FUZZY_DEFTEMPLATES   /* added 03-11-96 */
static struct lhsParseNode *ConjunctiveRestrictionParse(readSource,theToken,error,theConstraints)
#else
static struct lhsParseNode *ConjunctiveRestrictionParse(readSource,theToken,error)
#endif
  char *readSource;
  struct token *theToken;
  int *error;
#if FUZZY_DEFTEMPLATES    /* added 03-11-96 */
  CONSTRAINT_RECORD *theConstraints;
#endif
  {
   struct lhsParseNode *bindNode;
   struct lhsParseNode *theNode, *nextOr, *nextAnd;
   int connectorType;
#if FUZZY_DEFTEMPLATES   /* added 03-11-96 */
   int fuzzyType;

   fuzzyType = (theConstraints != NULL) ? theConstraints->fuzzyValuesAllowed
                                        : CLIPS_FALSE;
#endif

   /*=====================================*/
   /* Get the first node and determine if */
   /* it is a binding variable.           */
   /*=====================================*/

#if !FUZZY_DEFTEMPLATES    /* added 03-11-96 */
   theNode = LiteralRestrictionParse(readSource,theToken,error);
#else
   theNode = LiteralRestrictionParse(readSource,theToken,error,theConstraints);

   if (*error == CLIPS_TRUE)
     { return(NULL); }

   if (fuzzyType && theNode->type != FUZZY_VALUE && theNode->type != SF_VARIABLE)
     {
       /* error -- fuzzy slot can only have fuzzy expression, SF variable 
          (possible connect with ANDS (&), or a SF wildcard (which is taken
          care of in RestrictionParse
       */       
       SyntaxErrorMessage("Fuzzy Value slot\n(only ?, ?var, or linguistic expression allowed)");
       *error = CLIPS_TRUE;
       return( NULL );
     }
#endif

#if FUZZY_DEFTEMPLATES   /* added 03-11-96 */
   /* Fuzzy Value processing eats up the ')' so don't get another token */
   if (theNode->type != FUZZY_VALUE)
      GetToken(readSource,theToken);
#else
   GetToken(readSource,theToken);
#endif
   
   if (((theNode->type == SF_VARIABLE) || (theNode->type == MF_VARIABLE)) &&
       (theNode->negated == CLIPS_FALSE) &&
       (theToken->type != OR_CONSTRAINT))
     {
      theNode->bindingVariable = CLIPS_TRUE;
      bindNode = theNode;
      nextOr = NULL;
      nextAnd = NULL;
     }
   else
     {
      bindNode = GetLHSParseNode();
      if (theNode->type == MF_VARIABLE) bindNode->type = MF_WILDCARD;
      else bindNode->type = SF_WILDCARD;
      bindNode->negated = CLIPS_FALSE;
      bindNode->bottom = theNode;
      nextOr = theNode;
      nextAnd = theNode;
     }

   /*===================================*/
   /* Process the connected constraints */
   /* within the constraint             */
   /*===================================*/

#if FUZZY_DEFTEMPLATES   /* added 03-11-96 */
   while ((theToken->type == OR_CONSTRAINT && !fuzzyType) ||
          (theToken->type == AND_CONSTRAINT)
         )
#else
   while ((theToken->type == OR_CONSTRAINT) || (theToken->type == AND_CONSTRAINT))
#endif
     {
      /*==========================*/
      /* Get the next constraint. */
      /*==========================*/

      connectorType = theToken->type;

      GetToken(readSource,theToken);
#if FUZZY_DEFTEMPLATES   /* added 03-11-96 */
      theNode = LiteralRestrictionParse(readSource,theToken,error,theConstraints);
#else
      theNode = LiteralRestrictionParse(readSource,theToken,error);
#endif

      if (*error == CLIPS_TRUE)
        {
         ReturnLHSParseNodes(bindNode);
         return(NULL);
        }

      /*=======================================*/
      /* Attach the new constraint to the list */
      /* of constraints for this field.        */
      /*=======================================*/

      if (connectorType == OR_CONSTRAINT)
        {
         if (nextOr == NULL)
           { bindNode->bottom = theNode; }
         else
           { nextOr->bottom = theNode; }
         nextOr = theNode;
         nextAnd = theNode;
        }
      else if (connectorType == AND_CONSTRAINT)
        {
         if (nextAnd == NULL)
           {
            bindNode->bottom = theNode;
            nextOr = theNode;
           }
         else
           { nextAnd->right = theNode; }
         nextAnd = theNode;
        }
      else
        {
         CLIPSSystemError("RULEPSR",1);
         ExitCLIPS(4);
        }

      /*==================================================*/
      /* Determine if any more restrictions are connected */
      /* to the current list of restrictions.             */
      /*==================================================*/

#if FUZZY_DEFTEMPLATES   /* added 03-11-96 */
      /* Fuzzy Value processing eats up the ')' so don't get another token */
      if (theNode->type != FUZZY_VALUE)
         GetToken(readSource,theToken);
#else
      GetToken(readSource,theToken);
#endif

     }

   /*==========================================*/
   /* Check for illegal mixing of single and   */
   /* multifield values within the constraint. */
   /*==========================================*/
   
   if (CheckForVariableMixing(bindNode))
     { 
      *error = CLIPS_TRUE;
      ReturnLHSParseNodes(bindNode);
      return(NULL);
     }
     
   /*========================*/
   /* Return the constraint. */
   /*========================*/
   
   return(bindNode);
  }
  
/*****************************************************/
/* CheckForVariableMixing: Checks a field constraint */
/*   to determine if single and multifield variables */
/*   are illegally mixed within it.                  */
/*****************************************************/
static int CheckForVariableMixing(theRestriction)
  struct lhsParseNode *theRestriction;
  {
   struct lhsParseNode *tempRestriction;
   CONSTRAINT_RECORD *theConstraint;
   int multifield = CLIPS_FALSE;
   int singlefield = CLIPS_FALSE;
   int constant = CLIPS_FALSE;
   int singleReturnValue = CLIPS_FALSE;
   int multiReturnValue = CLIPS_FALSE;
   
   /*================================================*/
   /* If the constraint contains a binding variable, */
   /* determine whether it is a single field or      */
   /* multifield variable.                           */
   /*================================================*/
   
   if (theRestriction->type == SF_VARIABLE) singlefield = CLIPS_TRUE;
   else if (theRestriction->type == MF_VARIABLE) multifield = CLIPS_TRUE;
   
   /*===========================================*/
   /* Loop through each of the or (|) connected */
   /* constraints within the constraint.        */
   /*===========================================*/
   
   for (theRestriction = theRestriction->bottom;
        theRestriction != NULL;
        theRestriction = theRestriction->bottom)
     {
      /*============================================*/
      /* Loop through each of the and (&) connected */
      /* constraints within the or (|) constraint.  */
      /*============================================*/
      
      for (tempRestriction = theRestriction;
           tempRestriction != NULL;
           tempRestriction = tempRestriction->right)
        {
         /*=====================================================*/
         /* Determine if the constraint contains a single field */
         /* variable, multifield variable, constant (a single   */
         /* field), a return value constraint of a function     */
         /* returning a single field value, or a return value   */
         /* constraint of a function returning a multifield     */
         /* value.                                              */
         /*=====================================================*/
         
         if (tempRestriction->type == SF_VARIABLE) singlefield = CLIPS_TRUE;
         else if (tempRestriction->type == MF_VARIABLE) multifield = CLIPS_TRUE;
         else if (ConstantType(tempRestriction->type)) constant = CLIPS_TRUE;
         else if (tempRestriction->type == RETURN_VALUE_CONSTRAINT)
           {
            theConstraint = FunctionCallToConstraintRecord(tempRestriction->expression->value);
            if (theConstraint->anyAllowed) { /* Do nothing. */ }
            else if (theConstraint->multifieldsAllowed) multiReturnValue = CLIPS_TRUE;
            else singleReturnValue = CLIPS_TRUE;
            RemoveConstraint(theConstraint);
           }
        }
     }
     
   /*================================================================*/
   /* Using a single field value (a single field variable, constant, */
   /* or function returning a single field value) together with a    */
   /* multifield value (a multifield variable or function returning  */
   /* a multifield value) is illegal. Return TRUE if this occurs.    */
   /*================================================================*/
   
   if ((singlefield || constant || singleReturnValue) &&
       (multifield || multiReturnValue))
       
     {
      PrintErrorID("PATTERN",2,CLIPS_TRUE);
      PrintCLIPS(WERROR,"Single and multifield constraints cannot be mixed in a field constraint\n");
      return(CLIPS_TRUE);
     }
     
   /*=======================================*/
   /* Otherwise return FALSE to indicate no */
   /* illegal variable mixing was detected. */
   /*=======================================*/
   
   return(CLIPS_FALSE);
  }

/***********************************************************/
/* LiteralRestrictionParse: Parses a single constraint.    */
/*   The constraint may be a literal constraint, a         */
/*   predicate constraint, a return value constraint, or a */
/*   variable constraint. The constraints may also be      */
/*   negated using the ~ connective constraint.            */
/*                                                         */
/* <single-constraint>     ::= <term> | ~<term>            */
/*                                                         */
/*  <term>                 ::= <constant> |                */
/*                             <single-field-variable> |   */
/*                             <multi-field-variable> |    */
/*                             :<function-call> |          */
/*                             =<function-call>            */
/***********************************************************/
#if FUZZY_DEFTEMPLATES    /* added 03-11-96 */
static struct lhsParseNode *LiteralRestrictionParse(readSource,theToken,error,theConstraints)
#else
static struct lhsParseNode *LiteralRestrictionParse(readSource,theToken,error)
#endif
  char *readSource;
  struct token *theToken;
  int *error;
#if FUZZY_DEFTEMPLATES   /* added 03-11-96 */
  CONSTRAINT_RECORD *theConstraints;
#endif
  {
   struct lhsParseNode *topNode;
   struct expr *theExpression;
#if FUZZY_DEFTEMPLATES   /* added 03-11-96 */
   int fuzzyType;

   fuzzyType = (theConstraints != NULL) ? theConstraints->fuzzyValuesAllowed : CLIPS_FALSE;
#endif

   /*============================================*/
   /* Create a node to represent the constraint. */
   /*============================================*/
   
   topNode = GetLHSParseNode();

   /*=================================================*/
   /* Determine if the constraint has a '~' preceding */
   /* it. If it  does, then the field is negated      */
   /* (e.g. ~red means "not the constant red."        */
   /*=================================================*/
#if FUZZY_DEFTEMPLATES   /* added 03-11-96 */
   if (theToken->type == NOT_CONSTRAINT && !fuzzyType)
#else
   if (theToken->type == NOT_CONSTRAINT)
#endif
     {
      GetToken(readSource,theToken);
      topNode->negated = CLIPS_TRUE;
     }
   else
     { topNode->negated = CLIPS_FALSE; }

   /*===========================================*/
   /* Determine if the constraint is one of the */ 
   /* recognized types. These are ?variables,   */
   /* symbols, strings, numbers, :(expression), */
   /* and =(expression).                        */
   /*===========================================*/

   topNode->type = theToken->type;

   /*============================================*/
   /* Any symbol is valid, but an = signifies a  */
   /* return value constraint and an : signifies */
   /* a predicate constraint.                    */
   /*============================================*/
   
   if (theToken->type == SYMBOL)
     {
#if FUZZY_DEFTEMPLATES   /* added 03-11-96 */
      if (strcmp(ValueToString(theToken->value),"=") == 0 && !fuzzyType)
#else
      /*==============================*/
      /* If the symbol is an =, parse */
      /* a return value constraint.   */
      /*==============================*/
      
      if (strcmp(ValueToString(theToken->value),"=") == 0)
#endif
        {
         theExpression = Function0Parse(readSource);
         if (theExpression == NULL)
           {
            *error = CLIPS_TRUE;
            ReturnLHSParseNodes(topNode);
            return(NULL);
           }
         topNode->type = RETURN_VALUE_CONSTRAINT;
         topNode->expression = ExpressionToLHSParseNodes(theExpression);
         ReturnExpression(theExpression);
        }
#if FUZZY_DEFTEMPLATES   /* added 03-11-96 */
      else if (strcmp(ValueToString(theToken->value),":") == 0 && !fuzzyType)
#else        
      /*=============================*/
      /* If the symbol is a :, parse */
      /* a predicate constraint.     */
      /*=============================*/
      
      else if (strcmp(ValueToString(theToken->value),":") == 0)
#endif
        {
         theExpression = Function0Parse(readSource);
         if (theExpression == NULL)
           {
            *error = CLIPS_TRUE;
            ReturnLHSParseNodes(topNode);
            return(NULL);
           }
         topNode->type = PREDICATE_CONSTRAINT;
         topNode->expression = ExpressionToLHSParseNodes(theExpression);
         ReturnExpression(theExpression);
        }
      
      /*==============================================*/
      /* Otherwise, treat the constraint as a symbol. */
      /*==============================================*/
      
      else
#if FUZZY_DEFTEMPLATES   /* added 03-11-96 */
        {
          if (fuzzyType)
            {
              struct fuzzy_value *fvPtr;
              struct deftemplate *theDeftemplate;

              /* FuzzyDeftemplate is 'global' that holds the current
                deftemplate ptr IF it is a fuzzy deftemplate -- this is
                set up in routine DeftemplateLHSParse in tmpltlhs.c.
                Need to do this because FuzzyDeftemplate fuzzy values 
                do NOT have a restrictionList -- could have set up a lot
                of routines to pass this value thru to here BUT...
              */
              if (theConstraints->fuzzyValueRestriction)
                 theDeftemplate = (struct deftemplate *)theConstraints->restrictionList->value;
              else
                 theDeftemplate = FuzzyDeftemplate;

              fvPtr = GetFuzzyLHSPattern(readSource,theToken,
                                         theDeftemplate->fuzzyTemplate,error);
              if (*error)
                {
                 ReturnLHSParseNodes(topNode);
                 return(NULL);
                }
              topNode->type = FUZZY_VALUE;
              topNode->value = (VOID *)AddFuzzyValue(fvPtr);
              rtnFuzzyValue(fvPtr); /* copy of fv made by AddFuzzyValue! */
            }
          else
#endif
        { topNode->value = theToken->value; }
#if FUZZY_DEFTEMPLATES   /* added 03-11-96 */
        }
#endif
     }
     
   /*=====================================================*/
   /* Single and multifield variables and float, integer, */
   /* string, and instance name constants are also valid. */
   /*=====================================================*/
   
   else if ((theToken->type == SF_VARIABLE)  ||
            (theToken->type == MF_VARIABLE)  ||
            (theToken->type == FLOAT) ||
            (theToken->type == INTEGER) ||
            (theToken->type == STRING) ||
            (theToken->type == INSTANCE_NAME))
     { topNode->value = theToken->value; }
     
   /*===========================*/
   /* Anything else is invalid. */
   /*===========================*/
   
   else
     {
      SyntaxErrorMessage("defrule");
      *error = CLIPS_TRUE;
      ReturnLHSParseNodes(topNode);
      return(NULL);
     }

   /*===============================*/
   /* Return the parsed constraint. */
   /*===============================*/
   
   return(topNode);
  }

#endif /* (! RUN_TIME) && (! BLOAD_ONLY) */

#endif /* DEFRULE_CONSTRUCT */




