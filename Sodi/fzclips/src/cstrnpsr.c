   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  06/23/94            */
   /*                                                     */
   /*               CONSTRAINT PARSER MODULE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides functions for parsing constraint        */
/*   declarations.                                           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian Donnell                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#define _CSTRNPSR_SOURCE_

#include <stdio.h>
#define _CLIPS_STDIO_
#if ANSI_COMPILER
#include <stdlib.h>
#endif

#include "setup.h"

#include "constant.h"
#include "clipsmem.h"
#include "router.h"
#include "scanner.h"
#include "cstrnutl.h"
#include "cstrnchk.h"

#include "cstrnpsr.h"

#if FUZZY_DEFTEMPLATES
#include "tmpltdef.h"
#include "moduldef.h"
#include "modulutl.h"
#endif

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if (! RUN_TIME) && (! BLOAD_ONLY)
#if ANSI_COMPILER
   static BOOLEAN                 ParseRangeCardinalityAttribute(char *,CONSTRAINT_RECORD *,
                                                      CONSTRAINT_PARSE_RECORD *,char *,int);
#if FUZZY_DEFTEMPLATES
   static BOOLEAN                 ParseTypeAttribute(char *,CONSTRAINT_RECORD *,
                                                     CONSTRAINT_PARSE_RECORD *, int);
#else
   static BOOLEAN                 ParseTypeAttribute(char *,CONSTRAINT_RECORD *);
#endif
   static VOID                    AddToRestrictionList(int,CONSTRAINT_RECORD *,
                                                       CONSTRAINT_RECORD *);
   static BOOLEAN                 ParseAllowedValuesAttribute(char *,char *,
                                                              CONSTRAINT_RECORD *,
                                                              CONSTRAINT_PARSE_RECORD *);
   static int                     GetConstraintTypeFromAllowedName(char *);
   static int                     GetConstraintTypeFromTypeName(char *);
   static int                     GetAttributeParseValue(char *,CONSTRAINT_PARSE_RECORD *);
   static VOID                    SetRestrictionFlag(int,CONSTRAINT_RECORD *,int);
   static VOID                    SetParseFlag(CONSTRAINT_PARSE_RECORD *,char *);
   static VOID                    NoConjunctiveUseError(char *,char *);
#else
   static BOOLEAN                 ParseRangeCardinalityAttribute();
   static BOOLEAN                 ParseTypeAttribute();
   static VOID                    AddToRestrictionList();
   static BOOLEAN                 ParseAllowedValuesAttribute();
   static int                     GetConstraintTypeFromAllowedName();
   static int                     GetConstraintTypeFromTypeName();
   static int                     GetAttributeParseValue();
   static VOID                    SetRestrictionFlag();
   static VOID                    SetParseFlag();
   static VOID                    NoConjunctiveUseError();
#endif
#endif
  
/********************************************************************/
/* CheckConstraintParseConflicts: Determines if a constraint record */
/*   has any conflicts in the attribute specifications. Returns     */
/*   TRUE if no conflicts were detected, otherwise FALSE.           */
/********************************************************************/
globle BOOLEAN CheckConstraintParseConflicts(constraints)
  CONSTRAINT_RECORD *constraints;
  {
   /*===================================================*/
   /* Check to see if any of the allowed-... attributes */
   /* conflict with the type attribute.                 */
   /*===================================================*/
   
   if (constraints->anyAllowed == CLIPS_TRUE)
     { /* Do Nothing */ }
   else if (constraints->symbolRestriction && 
            (constraints->symbolsAllowed == CLIPS_FALSE))
     {
      AttributeConflictErrorMessage("type","allowed-symbols");
      return(CLIPS_FALSE);
     }
   else if (constraints->stringRestriction && 
            (constraints->stringsAllowed == CLIPS_FALSE))
     {
      AttributeConflictErrorMessage("type","allowed-strings");
      return(CLIPS_FALSE);
     }
   else if (constraints->integerRestriction && 
            (constraints->integersAllowed == CLIPS_FALSE))
     {
      AttributeConflictErrorMessage("type","allowed-integers/numbers");
      return(CLIPS_FALSE);
     }
   else if (constraints->floatRestriction && 
            (constraints->floatsAllowed == CLIPS_FALSE))
     {         
      AttributeConflictErrorMessage("type","allowed-floats/numbers");
      return(CLIPS_FALSE);
     }
   else if (constraints->instanceNameRestriction && 
            (constraints->instanceNamesAllowed == CLIPS_FALSE))
     {         
      AttributeConflictErrorMessage("type","allowed-instance-names");
      return(CLIPS_FALSE);
     }
   else if (constraints->anyRestriction)
     {
      struct expr *exp;
      
      for (exp = constraints->restrictionList; 
           exp != NULL;
           exp = exp->nextArg)
        {
         if (ConstraintCheckValue(exp->type,exp->value,constraints) != NO_VIOLATION)
           {
            AttributeConflictErrorMessage("type","allowed-values");
            return(CLIPS_FALSE);
           }
        }
     }
#if FUZZY_DEFTEMPLATES
   else if (constraints->fuzzyValueRestriction && (constraints->fuzzyValuesAllowed == CLIPS_FALSE))
     {/* this should never happen?? */
      AttributeConflictErrorMessage("FUZZY VALUE type","FUZZY VALUE Restriction");
      return(CLIPS_FALSE);
     }
#endif

   /*================================================================*/
   /* Check to see if range attribute conflicts with type attribute. */
   /*================================================================*/

   if ((constraints->maxValue != NULL) &&
       (constraints->anyAllowed == CLIPS_FALSE))
     {
      if (((constraints->maxValue->type == INTEGER) &&
          (constraints->integersAllowed == CLIPS_FALSE)) ||
          ((constraints->maxValue->type == FLOAT) &&
           (constraints->floatsAllowed == CLIPS_FALSE)))
        {
         AttributeConflictErrorMessage("type","range");
         return(CLIPS_FALSE);
        }
     }

   if ((constraints->minValue != NULL) && 
       (constraints->anyAllowed == CLIPS_FALSE))
     {
      if (((constraints->minValue->type == INTEGER) &&
          (constraints->integersAllowed == CLIPS_FALSE)) ||
          ((constraints->minValue->type == FLOAT) &&
           (constraints->floatsAllowed == CLIPS_FALSE)))
        {
         AttributeConflictErrorMessage("type","range");
         return(CLIPS_FALSE);
        }
     }
     
   /*=====================================================*/
   /* Return TRUE to indicate no conflicts were detected. */
   /*=====================================================*/
   
   return(CLIPS_TRUE);
  }  

/********************************************************/
/* AttributeConflictErrorMessage: Generic error message */
/*   for a constraint attribute conflict.               */
/********************************************************/
globle VOID AttributeConflictErrorMessage(attribute1,attribute2)
  char *attribute1, *attribute2;
  {
   PrintErrorID("CSTRNPSR",1,CLIPS_TRUE);
   PrintCLIPS(WERROR,"The ");
   PrintCLIPS(WERROR,attribute1);
   PrintCLIPS(WERROR," attribute conflicts with the ");
   PrintCLIPS(WERROR,attribute2);
   PrintCLIPS(WERROR," attribute.\n");
  }

#if (! RUN_TIME) && (! BLOAD_ONLY)

/***************************************************************************/
/* InitializeConstraintParseRecord: Initializes the values of a constraint */
/*   parse record which is used to determine whether one of the standard   */
/*   constraint specifications has already been parsed.                    */
/***************************************************************************/
globle VOID InitializeConstraintParseRecord(parsedConstraints)
  CONSTRAINT_PARSE_RECORD *parsedConstraints;
  {   
   parsedConstraints->type = CLIPS_FALSE;
   parsedConstraints->range = CLIPS_FALSE;
   parsedConstraints->allowedSymbols = CLIPS_FALSE;
   parsedConstraints->allowedStrings = CLIPS_FALSE;
   parsedConstraints->allowedLexemes = CLIPS_FALSE;
   parsedConstraints->allowedIntegers = CLIPS_FALSE;
   parsedConstraints->allowedFloats = CLIPS_FALSE;
   parsedConstraints->allowedNumbers = CLIPS_FALSE;
   parsedConstraints->allowedValues = CLIPS_FALSE;
   parsedConstraints->allowedInstanceNames = CLIPS_FALSE;
   parsedConstraints->cardinality = CLIPS_FALSE;
  }
  
/************************************************************************/
/* StandardConstraint: Returns TRUE if the specified name is one of the */
/*   standard constraints parseable by the routines in this module.     */
/************************************************************************/
globle BOOLEAN StandardConstraint(constraintName)
  char *constraintName;
  {   
   if ((strcmp(constraintName,"type") == 0) ||
       (strcmp(constraintName,"range") == 0) ||
       (strcmp(constraintName,"cardinality") == 0) ||
       (strcmp(constraintName,"allowed-symbols") == 0) ||
       (strcmp(constraintName,"allowed-strings") == 0) ||
       (strcmp(constraintName,"allowed-lexemes") == 0) ||
       (strcmp(constraintName,"allowed-integers") == 0) ||
       (strcmp(constraintName,"allowed-floats") == 0) ||
       (strcmp(constraintName,"allowed-numbers") == 0) ||
       (strcmp(constraintName,"allowed-instance-names") == 0) ||
       (strcmp(constraintName,"allowed-values") == 0))
       
     { return(CLIPS_TRUE); }
   
   return(CLIPS_FALSE);
  }
  
/***********************************************************************/
/* ParseStandardConstraint: Parses a standard constraint. Returns TRUE */
/*   if the constraint was successfully parsed, otherwise FALSE.       */
/***********************************************************************/
globle BOOLEAN ParseStandardConstraint(readSource,constraintName,
                                       constraints,parsedConstraints,
                                       multipleValuesAllowed)
  char *readSource;
  char *constraintName;
  CONSTRAINT_RECORD *constraints;
  CONSTRAINT_PARSE_RECORD *parsedConstraints;
  int multipleValuesAllowed;
  {   
   int rv = CLIPS_FALSE;
   
   /*=====================================================*/
   /* Determine if the attribute has already been parsed. */
   /*=====================================================*/
   
   if (GetAttributeParseValue(constraintName,parsedConstraints))
     {
      AlreadyParsedErrorMessage(constraintName," attribute");
      return(CLIPS_FALSE);
     }

   /*==========================================*/
   /* If specified, parse the range attribute. */
   /*==========================================*/
   
   if (strcmp(constraintName,"range") == 0) 
     {
      rv = ParseRangeCardinalityAttribute(readSource,constraints,parsedConstraints,
                                          constraintName,multipleValuesAllowed);
     }
     
   /*================================================*/
   /* If specified, parse the cardinality attribute. */
   /*================================================*/
   
   else if (strcmp(constraintName,"cardinality") == 0) 
     {
      rv = ParseRangeCardinalityAttribute(readSource,constraints,parsedConstraints,
                                          constraintName,multipleValuesAllowed);
     }
     
   /*=========================================*/
   /* If specified, parse the type attribute. */
   /*=========================================*/
   
   else if (strcmp(constraintName,"type") == 0) 
     {
#if FUZZY_DEFTEMPLATES
      rv = ParseTypeAttribute(readSource,constraints,parsedConstraints,multipleValuesAllowed);
#else
 rv = ParseTypeAttribute(readSource,constraints); 
#endif
     }
     
   /*================================================*/
   /* If specified, parse the allowed-... attribute. */
   /*================================================*/
   
   else if ((strcmp(constraintName,"allowed-symbols") == 0) ||
            (strcmp(constraintName,"allowed-strings") == 0) ||
            (strcmp(constraintName,"allowed-lexemes") == 0) ||
            (strcmp(constraintName,"allowed-integers") == 0) ||
            (strcmp(constraintName,"allowed-floats") == 0) ||
            (strcmp(constraintName,"allowed-numbers") == 0) ||
            (strcmp(constraintName,"allowed-instance-names") == 0) ||
            (strcmp(constraintName,"allowed-values") == 0))
     {
      rv = ParseAllowedValuesAttribute(readSource,constraintName,
                                         constraints,parsedConstraints);
     }
     
   /*=========================================*/
   /* Remember which constraint attribute was */
   /* parsed and return the error status.     */
   /*=========================================*/
   
   SetParseFlag(parsedConstraints,constraintName);
   return(rv);
  }

/***********************************************************/
/* OverlayConstraint: Overlays fields of source constraint */
/* record on destination based on which fields are set in  */
/* the parsed constraint record. Assumes AddConstraint has */
/* not yet been called for the destination constraint      */
/* record.                                                 */
/***********************************************************/
globle VOID OverlayConstraint(pc,cdst,csrc)
  CONSTRAINT_PARSE_RECORD *pc;
  CONSTRAINT_RECORD *cdst,*csrc;
  {
   if (pc->type == 0)
     {
      cdst->anyAllowed = csrc->anyAllowed;
      cdst->symbolsAllowed = csrc->symbolsAllowed;
      cdst->stringsAllowed = csrc->stringsAllowed;
      cdst->floatsAllowed = csrc->floatsAllowed;
      cdst->integersAllowed = csrc->integersAllowed;
      cdst->instanceNamesAllowed = csrc->instanceNamesAllowed;
      cdst->instanceAddressesAllowed = csrc->instanceAddressesAllowed;
      cdst->externalAddressesAllowed = csrc->externalAddressesAllowed;
      cdst->factAddressesAllowed = csrc->factAddressesAllowed;
#if FUZZY_DEFTEMPLATES
      /* When the type constraint is a FUZZY-VALUE the type is the only 
         constraint allowed and the fuzzyValuesAllowed flag and the
         fuzzyValueRestriction is set...also the RestrictionList will
         contain 1 item -- a DEFTEMPLATE_PTR which is a reference to the
         Deftemplate used by the fuzzy value. Therefore we must set 
         the destination constraints to be the same as the source-->
         if the fuzzyValueRestriction is 1 then the list will have
         a DEFTEMPLATE_PTR type on it.
       
         NOTE: for fuzzy values associated with a fuzzy deftemplate 
               only the fuzzyValuesAllowed flag will be set.
      */
      cdst->fuzzyValuesAllowed = csrc->fuzzyValuesAllowed;
      cdst->fuzzyValueRestriction = csrc->fuzzyValueRestriction;
      if (cdst->fuzzyValueRestriction)
            AddToRestrictionList(DEFTEMPLATE_PTR,cdst,csrc);
#endif
     }
     
   if (pc->range == 0)
     {
      ReturnExpression(cdst->minValue);
      ReturnExpression(cdst->maxValue);
      cdst->minValue = CopyExpression(csrc->minValue);
      cdst->maxValue = CopyExpression(csrc->maxValue);
     }
     
   if (pc->allowedValues == 0)
     {
      if ((pc->allowedSymbols == 0) &&
          (pc->allowedStrings == 0) &&
          (pc->allowedLexemes == 0) &&
          (pc->allowedIntegers == 0) &&
          (pc->allowedFloats == 0) &&
          (pc->allowedNumbers == 0) &&
          (pc->allowedInstanceNames == 0))
        {
         cdst->anyRestriction = csrc->anyRestriction;
         cdst->symbolRestriction = csrc->symbolRestriction;
         cdst->stringRestriction = csrc->stringRestriction;
         cdst->floatRestriction = csrc->floatRestriction;
         cdst->integerRestriction = csrc->integerRestriction;
         cdst->instanceNameRestriction = csrc->instanceNameRestriction;
         cdst->restrictionList = CopyExpression(csrc->restrictionList);
        }
      else
        {
         if ((pc->allowedSymbols == 0) && csrc->symbolRestriction)
           {
            cdst->symbolRestriction = 1;
            AddToRestrictionList(SYMBOL,cdst,csrc);
           }
         if ((pc->allowedStrings == 0) && csrc->stringRestriction)
           {
            cdst->stringRestriction = 1;
            AddToRestrictionList(STRING,cdst,csrc);
           }
         if ((pc->allowedLexemes == 0) && csrc->symbolRestriction && csrc->stringRestriction)
           {
            cdst->symbolRestriction = 1;
            cdst->stringRestriction = 1;
            AddToRestrictionList(SYMBOL,cdst,csrc);
            AddToRestrictionList(STRING,cdst,csrc);
           }
         if ((pc->allowedIntegers == 0) && csrc->integerRestriction)
           {
            cdst->integerRestriction = 1;
            AddToRestrictionList(INTEGER,cdst,csrc);
           }
         if ((pc->allowedFloats == 0) && csrc->floatRestriction)
           {
            cdst->floatRestriction = 1;
            AddToRestrictionList(FLOAT,cdst,csrc);
           }
         if ((pc->allowedNumbers == 0) && csrc->integerRestriction && csrc->floatRestriction)
           {
            cdst->integerRestriction = 1;
            cdst->floatRestriction = 1;
            AddToRestrictionList(INTEGER,cdst,csrc);
            AddToRestrictionList(FLOAT,cdst,csrc);
           }
         if ((pc->allowedInstanceNames == 0) && csrc->instanceNameRestriction)
           {
            cdst->instanceNameRestriction = 1;
            AddToRestrictionList(INSTANCE_NAME,cdst,csrc);
           }
        }
     }
     
   if (pc->cardinality == 0)
     {
      ReturnExpression(cdst->minFields);
      ReturnExpression(cdst->maxFields);
      cdst->minFields = CopyExpression(csrc->minFields);
      cdst->maxFields = CopyExpression(csrc->maxFields);
     }
  }

/**********************************************/
/* OverlayConstraintParseRecord: Performs a   */
/*   field-wise "or" of the destination parse */
/*   record with the source parse record.     */
/**********************************************/ /* added 03-05-96 */
globle VOID OverlayConstraintParseRecord(dst,src)
  CONSTRAINT_PARSE_RECORD *dst,*src;
  {
   if (src->type) dst->type = CLIPS_TRUE;
   if (src->range) dst->range = CLIPS_TRUE;
   if (src->allowedSymbols) dst->allowedSymbols = CLIPS_TRUE;
   if (src->allowedStrings) dst->allowedStrings = CLIPS_TRUE;
   if (src->allowedLexemes) dst->allowedLexemes = CLIPS_TRUE;
   if (src->allowedIntegers) dst->allowedIntegers = CLIPS_TRUE;
   if (src->allowedFloats) dst->allowedFloats = CLIPS_TRUE;
   if (src->allowedNumbers) dst->allowedNumbers = CLIPS_TRUE;
   if (src->allowedValues) dst->allowedValues = CLIPS_TRUE;
   if (src->allowedInstanceNames) dst->allowedInstanceNames = CLIPS_TRUE;
   if (src->cardinality) dst->cardinality = CLIPS_TRUE;
  }

/************************************************************/
/* AddToRestrictionList: Prepends atoms of the specified    */
/* type from the source restriction list to the destination */
/************************************************************/
static VOID AddToRestrictionList(type,cdst,csrc)
  int type;
  CONSTRAINT_RECORD *cdst,*csrc;
  {
   struct expr *exp,*tmp;
   
   for (exp = csrc->restrictionList; exp != NULL; exp = exp->nextArg)
     {
      if (exp->type == type)
        {
         tmp = GenConstant(exp->type,exp->value);
         tmp->nextArg = cdst->restrictionList;
         cdst->restrictionList = tmp;
        }
     }
  } 
  
/*******************************************************************/
/* ParseAllowedValuesAttribute: Parses the allowed-... attributes. */
/*******************************************************************/
static BOOLEAN ParseAllowedValuesAttribute(readSource,constraintName,
                                           constraints,parsedConstraints)
  char *readSource;
  char *constraintName;
  CONSTRAINT_RECORD *constraints;
  CONSTRAINT_PARSE_RECORD *parsedConstraints;
  {
   struct token inputToken;
   int expectedType, error = CLIPS_FALSE;
   struct expr *newValue, *lastValue;
   int constantParsed = CLIPS_FALSE, variableParsed = CLIPS_FALSE;
   char *tempPtr;

#if FUZZY_DEFTEMPLATES
   /*======================================================*/
   /* The allowed-values attribute is not allowed if a     */
   /* FUZZY-VALUE type constraint has already been parsed. */
   /*======================================================*/
   if (constraints->fuzzyValuesAllowed)
     {
      NoConjunctiveUseError("allowed-...","type FUZZY-VALUE");
      return(CLIPS_FALSE);       
     }

#endif
        
   /*======================================================*/
   /* The allowed-values attribute is not allowed if other */
   /* allowed-... attributes have already been parsed.     */
   /*======================================================*/

   if ((strcmp(constraintName,"allowed-values") == 0) &&
       ((parsedConstraints->allowedSymbols) ||
        (parsedConstraints->allowedStrings) ||
        (parsedConstraints->allowedLexemes) ||
        (parsedConstraints->allowedIntegers) ||
        (parsedConstraints->allowedFloats) ||
        (parsedConstraints->allowedNumbers) ||
        (parsedConstraints->allowedInstanceNames)))
     {
      if (parsedConstraints->allowedSymbols) tempPtr = "allowed-symbols";
      else if (parsedConstraints->allowedStrings) tempPtr = "allowed-strings";
      else if (parsedConstraints->allowedLexemes) tempPtr = "allowed-lexemes";
      else if (parsedConstraints->allowedIntegers) tempPtr = "allowed-integers";
      else if (parsedConstraints->allowedFloats) tempPtr = "allowed-floats";
      else if (parsedConstraints->allowedNumbers) tempPtr = "allowed-numbers";
      else if (parsedConstraints->allowedInstanceNames) tempPtr = "allowed-instance-names";
      NoConjunctiveUseError("allowed-values",tempPtr);
      return(CLIPS_FALSE);
     }
     
   /*=======================================================*/
   /* The allowed-values/numbers/integers/floats attributes */
   /* are not allowed with the range attribute.             */
   /*=======================================================*/

   if (((strcmp(constraintName,"allowed-values") == 0) ||
        (strcmp(constraintName,"allowed-numbers") == 0) ||
        (strcmp(constraintName,"allowed-integers") == 0) ||
        (strcmp(constraintName,"allowed-floats") == 0)) &&
       (parsedConstraints->range))
     {      
      NoConjunctiveUseError(constraintName,"range");
      return(CLIPS_FALSE);
     }
        
   /*===================================================*/
   /* The allowed-... attributes are not allowed if the */
   /* allowed-values attribute has already been parsed. */
   /*===================================================*/

   if ((strcmp(constraintName,"allowed-values") != 0) && 
            (parsedConstraints->allowedValues))
     {
      NoConjunctiveUseError(constraintName,"allowed-values");
      return(CLIPS_FALSE);
     }

   /*==================================================*/
   /* The allowed-numbers attribute is not allowed if  */
   /* the allowed-integers or allowed-floats attribute */
   /* has already been parsed.                         */
   /*==================================================*/

   if ((strcmp(constraintName,"allowed-numbers") == 0) && 
       ((parsedConstraints->allowedFloats) || (parsedConstraints->allowedIntegers)))
     {
      if (parsedConstraints->allowedFloats) tempPtr = "allowed-floats";
      else tempPtr = "allowed-integers";
      NoConjunctiveUseError("allowed-numbers",tempPtr);
      return(CLIPS_FALSE);
     }
   
   /*============================================================*/
   /* The allowed-integers/floats attributes are not allowed if  */
   /* the allowed-numbers attribute has already been parsed.     */
   /*============================================================*/

   if (((strcmp(constraintName,"allowed-integers") == 0) ||
        (strcmp(constraintName,"allowed-floats") == 0)) &&
       (parsedConstraints->allowedNumbers))
     {
      NoConjunctiveUseError(constraintName,"allowed-number");
      return(CLIPS_FALSE);
     }
     
   /*==================================================*/
   /* The allowed-lexemes attribute is not allowed if  */
   /* the allowed-symbols or allowed-strings attribute */
   /* has already been parsed.                         */
   /*==================================================*/

   if ((strcmp(constraintName,"allowed-lexemes") == 0) && 
       ((parsedConstraints->allowedSymbols) || (parsedConstraints->allowedStrings)))
     {
      if (parsedConstraints->allowedSymbols) tempPtr = "allowed-symbols";
      else tempPtr = "allowed-strings";
      NoConjunctiveUseError("allowed-lexemes",tempPtr);
      return(CLIPS_FALSE);
     }
   
   /*===========================================================*/
   /* The allowed-symbols/strings attributes are not allowed if */
   /* the allowed-lexemes attribute has already been parsed.    */
   /*===========================================================*/

   if (((strcmp(constraintName,"allowed-symbols") == 0) ||
        (strcmp(constraintName,"allowed-strings") == 0)) &&
       (parsedConstraints->allowedLexemes))
     {
      NoConjunctiveUseError(constraintName,"allowed-lexemes");
      return(CLIPS_FALSE);
     }

   /*========================*/
   /* Get the expected type. */
   /*========================*/
 
   expectedType = GetConstraintTypeFromAllowedName(constraintName);
   SetRestrictionFlag(expectedType,constraints,CLIPS_TRUE);

   /*=================================================*/
   /* Get the last value in the restriction list (the */
   /* allowed values will be appended there).         */
   /*=================================================*/
   
   lastValue = constraints->restrictionList;
   if (lastValue != NULL)
     { while (lastValue->nextArg != NULL) lastValue = lastValue->nextArg; }
   
   /*==================================================*/
   /* Read the allowed values and add them to the list */
   /* until a right parenthesis is encountered.        */
   /*==================================================*/

   SavePPBuffer(" ");
   GetToken(readSource,&inputToken);
     
   while (inputToken.type != RPAREN)
     {
      SavePPBuffer(" ");

      /*=============================================*/
      /* Determine the type of the token just parsed */
      /* and if it is an appropriate value.          */
      /*=============================================*/

      switch(inputToken.type)
        {
         case INTEGER:
           if ((expectedType != UNKNOWN) && 
               (expectedType != INTEGER) &&
               (expectedType != INTEGER_OR_FLOAT)) error = CLIPS_TRUE;
           constantParsed = CLIPS_TRUE; /*changed(type..)03-05-96*/
           break;
         
         case FLOAT:
           if ((expectedType != UNKNOWN) && 
               (expectedType != FLOAT) &&
               (expectedType != INTEGER_OR_FLOAT)) error = CLIPS_TRUE;
           constantParsed = CLIPS_TRUE;
           break;
         
         case STRING:
           if ((expectedType != UNKNOWN) && 
               (expectedType != STRING) &&
               (expectedType != SYMBOL_OR_STRING)) error = CLIPS_TRUE;
           constantParsed = CLIPS_TRUE;
           break;
         
         case SYMBOL:
           if ((expectedType != UNKNOWN) && 
               (expectedType != SYMBOL) &&
               (expectedType != SYMBOL_OR_STRING)) error = CLIPS_TRUE;
           constantParsed = CLIPS_TRUE;
           break;
         
#if OBJECT_SYSTEM
         case INSTANCE_NAME:
           if ((expectedType != UNKNOWN) && 
               (expectedType != INSTANCE_NAME)) error = CLIPS_TRUE;
           constantParsed = CLIPS_TRUE;
           break;
#endif

         case SF_VARIABLE:
           if (strcmp(inputToken.printForm,"?VARIABLE") == 0) 
             { variableParsed = CLIPS_TRUE; }
           else 
             {
              char tempBuffer[120];
              sprintf(tempBuffer,"%s attribute",constraintName);
              SyntaxErrorMessage(tempBuffer);
              return(CLIPS_FALSE);
             }
             
           break;

         default:
           {
            char tempBuffer[120];
            sprintf(tempBuffer,"%s attribute",constraintName);
            SyntaxErrorMessage(tempBuffer);
           }
           return(CLIPS_FALSE);
        }
        
      /*=====================================*/
      /* Signal an error if an inappropriate */
      /* value was found.                    */
      /*=====================================*/
      
      if (error)
        {
         PrintErrorID("CSTRNPSR",4,CLIPS_TRUE);
         PrintCLIPS(WERROR,"Value does not match the expected type for the ");
         PrintCLIPS(WERROR,constraintName);
         PrintCLIPS(WERROR," attribute\n");
         return(CLIPS_FALSE);
        }

      /*======================================*/
      /* The ?VARIABLE argument can't be used */
      /* in conjunction with constants.       */
      /*======================================*/
      
      if (constantParsed && variableParsed)
        {
         char tempBuffer[120];
         sprintf(tempBuffer,"%s attribute",constraintName);
         SyntaxErrorMessage(tempBuffer);
         return(CLIPS_FALSE);
        }      
        
      /*===========================================*/
      /* Add the constant to the restriction list. */
      /*===========================================*/

      newValue = GenConstant(inputToken.type,inputToken.value);
      if (lastValue == NULL)
        { constraints->restrictionList = newValue; }
      else 
        { lastValue->nextArg = newValue; }
      lastValue = newValue;

      /*=======================================*/
      /* Begin parsing the next allowed value. */
      /*=======================================*/

      GetToken(readSource,&inputToken);
     }

   /*======================================================*/
   /* There must be at least one value for this attribute. */
   /*======================================================*/

   if ((! constantParsed) && (! variableParsed))
     {
      char tempBuffer[120];
      sprintf(tempBuffer,"%s attribute",constraintName);
      SyntaxErrorMessage(tempBuffer);
      return(CLIPS_FALSE);
     }

   /*======================================*/
   /* If ?VARIABLE was parsed, then remove */
   /* the restrictions for the type being  */
   /* restricted.                          */
   /*======================================*/

   if (variableParsed)
     {
      switch(expectedType)
        {
         case UNKNOWN:
           constraints->anyRestriction = CLIPS_FALSE;
           break;

         case SYMBOL:
           constraints->symbolRestriction = CLIPS_FALSE;
           break;

         case STRING:
           constraints->stringRestriction = CLIPS_FALSE;
           break;

         case INTEGER:
           constraints->integerRestriction = CLIPS_FALSE;
           break;

         case FLOAT:
           constraints->floatRestriction = CLIPS_FALSE;
           break;

         case INTEGER_OR_FLOAT:
           constraints->floatRestriction = CLIPS_FALSE;
           constraints->integerRestriction = CLIPS_FALSE;
           break;
           
         case SYMBOL_OR_STRING:
           constraints->symbolRestriction = CLIPS_FALSE;
           constraints->stringRestriction = CLIPS_FALSE;
           break;
           
         case INSTANCE_NAME:
           constraints->instanceNameRestriction = CLIPS_FALSE;
           break;
        }
     }

   /*=====================================*/
   /* Fix up pretty print representation. */
   /*=====================================*/

   PPBackup();
   PPBackup();
   SavePPBuffer(")");

   /*=======================================*/
   /* Return TRUE to indicate the attribute */
   /* was successfully parsed.              */
   /*=======================================*/
   
   return(CLIPS_TRUE);
  }     

/***********************************************************/
/* NoConjunctiveUseError: Generic error message indicating */
/*   that two attributes can't be used in conjunction.     */
/***********************************************************/
static VOID NoConjunctiveUseError(attribute1,attribute2)
  char *attribute1;
  char *attribute2;
  {
   PrintErrorID("CSTRNPSR",3,CLIPS_TRUE);
   PrintCLIPS(WERROR,"The ");
   PrintCLIPS(WERROR,attribute1);
   PrintCLIPS(WERROR," attribute cannot be used\n");
   PrintCLIPS(WERROR,"in conjunction with the ");
   PrintCLIPS(WERROR,attribute2);
   PrintCLIPS(WERROR," attribute.\n");
  }
      
/**************************************************/
/* ParseTypeAttribute: Parses the type attribute. */
/**************************************************/
#if FUZZY_DEFTEMPLATES
static BOOLEAN ParseTypeAttribute(readSource,constraints,parsedConstraints,multipleValuesAllowed)
#else
static BOOLEAN ParseTypeAttribute(readSource,constraints)
#endif
  char *readSource;
  CONSTRAINT_RECORD *constraints;
#if FUZZY_DEFTEMPLATES
  CONSTRAINT_PARSE_RECORD *parsedConstraints;
  int multipleValuesAllowed;
#endif
  {
   int typeParsed = CLIPS_FALSE;
   int variableParsed = CLIPS_FALSE;
   int theType;
   struct token inputToken;

   /*======================================*/
   /* Continue parsing types until a right */
   /* parenthesis is encountered.          */
   /*======================================*/

   SavePPBuffer(" ");
   for (GetToken(readSource,&inputToken);
        inputToken.type != RPAREN;
        GetToken(readSource,&inputToken))
     {
      SavePPBuffer(" ");

      /*==================================*/
      /* If the token is a symbol then... */
      /*==================================*/
      
      if (inputToken.type == SYMBOL)
        {
         /*==============================================*/
         /* ?VARIABLE can't be used with type constants. */
         /*==============================================*/

         if (variableParsed == CLIPS_TRUE)
           {
            SyntaxErrorMessage("type attribute");
            return(CLIPS_FALSE);
           }
         
         /*========================================*/
         /* Check for an appropriate type constant */
         /* (e.g. SYMBOL, FLOAT, INTEGER, etc.).   */
         /*========================================*/
         
         theType = GetConstraintTypeFromTypeName(ValueToString(inputToken.value));

#if FUZZY_DEFTEMPLATES /* added 03-05-96 */
         /* if the type is FUZZY-VALUE then expect the name of the fuzzy
            deftemplate to follow. If not error. The name must be the name
            of an already defined fuzzy deftemplate!! Set the flags for
            fuzzyValuesAllowed and fuzzyValueRestriction and add the
            ptr to the deftemplate (DEFTEMPLATE_PTR type) to the RestrictionList.
            NOTE: 1. no other type is allowed.
                  2. no other constraint (attribute) such as 
                     range/cardinality/allowed-... is allowed.
                  3. cannot be in a multifield slot
         */
         if (theType == FUZZY_VALUE)
           {
             int count;
             char *moduleName;
             struct defmodule *theModule;
             struct deftemplate *deftPtr;

             if (multipleValuesAllowed)
               {
                 SyntaxErrorMessage("type FUZZY-VALUE (not allowed in multifield)");
                 return(CLIPS_FALSE);
               }
             if (parsedConstraints->range || parsedConstraints->cardinality ||
                 parsedConstraints->allowedSymbols  || parsedConstraints->allowedStrings  ||
                 parsedConstraints->allowedLexemes  || parsedConstraints->allowedFloats  ||
                 parsedConstraints->allowedIntegers  || parsedConstraints->allowedNumbers  ||
                 parsedConstraints->allowedInstanceNames  || 
parsedConstraints->allowedValues 
                )
               {
                 SyntaxErrorMessage("The 'type FUZZY-VALUE' attribute cannot be used \nwith any other attribute");
                 return(CLIPS_FALSE);
               }
             /* The next test may not be needed -- duplicates last one?? */
             if (constraints->symbolsAllowed || constraints->stringsAllowed  || 
                 constraints->floatsAllowed || constraints->integersAllowed  || 
                 constraints->instanceNamesAllowed || constraints->instanceAddressesAllowed  || 
                 constraints->externalAddressesAllowed || constraints->factAddressesAllowed ||
                 constraints->restrictionList != NULL
                )
               {
                 SyntaxErrorMessage("'type FUZZY-VALUE' \n(attribute cannot be used with any other attribute");
                 return(CLIPS_FALSE);
               }
             
             GetToken(readSource,&inputToken);

             /* token should be name of a fuzzy deftemplate */
             if (inputToken.type != SYMBOL)
               {
                 SyntaxErrorMessage("type attribute \n(expecting fuzzy deftemplate name)");
                 return(CLIPS_FALSE);
               }

             if (FindModuleSeparator(ValueToString(inputToken.value)))
               {
                 deftPtr = (struct deftemplate *)
                      FindDeftemplate(ValueToString(inputToken.value));
               }
              else
               {
                 deftPtr = (struct deftemplate *)
                       FindImportedConstruct("deftemplate",NULL,ValueToString(inputToken.value),
                                             &count,CLIPS_TRUE,NULL);

                 if (count > 1)
                   {
                     AmbiguousReferenceErrorMessage("deftemplate",ValueToString(inputToken.value));
                     return(CLIPS_FALSE);
                   }
                 if (deftPtr != NULL)
                   { /* form the fully qualified name -- e.g.  MAIN::tmpltname */
                     PPBackup();
                     theModule = deftPtr->header.whichModule->theModule;
                     moduleName = ValueToString(theModule->name);
                     SavePPBuffer(moduleName);
                     SavePPBuffer("::");
                     SavePPBuffer(ValueToString(inputToken.value));
                   }
                }

	      if ((deftPtr == NULL) || (SetConstraintType(theType,constraints)))
               {
                 if (deftPtr == NULL)
                    SyntaxErrorMessage("type attribute \n(expecting fuzzy deftemplate name)");
                 else
                    SyntaxErrorMessage("type attribute \n(type FUZZY-VALUE cannot be specified more than once)");
                 return(CLIPS_FALSE);
               }

             SetRestrictionFlag(theType,constraints,CLIPS_TRUE);
             constraints->anyAllowed = CLIPS_FALSE;
             constraints->restrictionList = GenConstant(DEFTEMPLATE_PTR, (VOID *)deftPtr);
             typeParsed = CLIPS_TRUE;

             GetToken(readSource,&inputToken);
             SavePPBuffer(" ");
             if (inputToken.type != RPAREN)
               {
                 SyntaxErrorMessage("type attribute \n(expecting ')' after fuzzy deftemplate name)");
                 return(CLIPS_FALSE);
               }
             break;  /* exit from the 'while (inputToken.type != RPAREN)' loop */
           }
#endif

         if (theType < 0)
           {
            SyntaxErrorMessage("type attribute");
            return(CLIPS_FALSE);
           }
             
         /*==================================================*/
         /* Change the type restriction flags to reflect the */
         /* type restriction. If the type restriction was    */
         /* already specified, then a error is generated.    */
         /*==================================================*/
         
         if (SetConstraintType(theType,constraints))
           {
            SyntaxErrorMessage("type attribute");
            return(CLIPS_FALSE);
           }

         constraints->anyAllowed = CLIPS_FALSE;
         
         /*===========================================*/
         /* Remember that a type constant was parsed. */
         /*===========================================*/

         typeParsed = CLIPS_TRUE;
        }
        
      /*==============================================*/
      /* Otherwise if the token is a variable then... */
      /*==============================================*/

      else if (inputToken.type == SF_VARIABLE)
        {
         /*========================================*/
         /* The only variable allowd is ?VARIABLE. */
         /*========================================*/
         
         if (strcmp(inputToken.printForm,"?VARIABLE") != 0)
           {
            SyntaxErrorMessage("type attribute");
            return(CLIPS_FALSE);
           }
           
         /*===================================*/
         /* ?VARIABLE can't be used more than */
         /* once or with type constants.      */
         /*===================================*/
         
         if (typeParsed || variableParsed)
           {
            SyntaxErrorMessage("type attribute");
            return(CLIPS_FALSE);
           }

         /*======================================*/
         /* Remember that a variable was parsed. */
         /*======================================*/
         
         variableParsed = CLIPS_TRUE;
        }
         
      /*====================================*/
      /* Otherwise this is an invalid value */
      /* for the type attribute.            */
      /*====================================*/
      
       else
        {
         SyntaxErrorMessage("type attribute");
         return(CLIPS_FALSE);
        }
     }

   /*=====================================*/
   /* Fix up pretty print representation. */
   /*=====================================*/

   PPBackup();
   PPBackup();
   SavePPBuffer(")");

   /*=======================================*/
   /* The type attribute must have a value. */
   /*=======================================*/

   if ((! typeParsed) && (! variableParsed))
     {
      SyntaxErrorMessage("type attribute");
      return(CLIPS_FALSE);
     }

   /*===========================================*/
   /* Return TRUE indicating the type attibuted */
   /* was successfully parsed.                  */
   /*===========================================*/
   
   return(CLIPS_TRUE);
  }
  
/***************************************************************************/
/* ParseRangeCardinalityAttribute: Parses the range/cardinality attribute. */
/***************************************************************************/
static BOOLEAN ParseRangeCardinalityAttribute(readSource,constraints,parsedConstraints,
                                              constraintName,multipleValuesAllowed)
  char *readSource;
  CONSTRAINT_RECORD *constraints;
  CONSTRAINT_PARSE_RECORD *parsedConstraints;
  char *constraintName;
  int multipleValuesAllowed;
  {
   struct token inputToken;
   int range;
   char *tempPtr;

#if FUZZY_DEFTEMPLATES
   /*======================================================*/
   /* The Range/Cardinality attribute is not allowed if a  */
   /* FUZZY-VALUE type constraint has already been parsed. */
   /*======================================================*/
   if (constraints->fuzzyValuesAllowed)
     {
      NoConjunctiveUseError("range/cardinality","type FUZZY-VALUE");
      return(CLIPS_FALSE);       
     }

#endif

   /*=================================*/
   /* Determine if we're parsing the  */
   /* range or cardinality attribute. */
   /*=================================*/
   
   if (strcmp(constraintName,"range") == 0) 
     {
      parsedConstraints->range = CLIPS_TRUE;
      range = CLIPS_TRUE;
     }
   else
     {
      parsedConstraints->cardinality = CLIPS_TRUE;
      range = CLIPS_FALSE;
     }
   
   /*===================================================================*/
   /* The cardinality attribute can only be used with multifield slots. */
   /*===================================================================*/
   
   if ((range == CLIPS_FALSE) &&
       (multipleValuesAllowed == CLIPS_FALSE))
     {
      PrintErrorID("CSTRNPSR",5,CLIPS_TRUE);
      PrintCLIPS(WERROR,"The cardinality attribute ");
      PrintCLIPS(WERROR,"can only be used with multifield slots.\n");
      return(CLIPS_FALSE);
     }
     
   /*====================================================*/
   /* The range attribute is not allowed with the        */
   /* allowed-values/numbers/integers/floats attributes. */
   /*====================================================*/

   if ((range == CLIPS_TRUE) && 
       (parsedConstraints->allowedValues ||
        parsedConstraints->allowedNumbers ||
        parsedConstraints->allowedIntegers ||
        parsedConstraints->allowedFloats))
     {
      if (parsedConstraints->allowedValues) tempPtr = "allowed-values";
      else if (parsedConstraints->allowedIntegers) tempPtr = "allowed-integers";
      else if (parsedConstraints->allowedFloats) tempPtr = "allowed-floats";
      else if (parsedConstraints->allowedNumbers) tempPtr = "allowed-numbers";
      NoConjunctiveUseError("range",tempPtr);
      return(CLIPS_FALSE);
     }

   /*==========================*/
   /* Parse the minimum value. */
   /*==========================*/

   SavePPBuffer(" ");
   GetToken(readSource,&inputToken);
   if ((inputToken.type == INTEGER) || ((inputToken.type == FLOAT) && range))
     { 
      if (range)
        {
         ReturnExpression(constraints->minValue);
         constraints->minValue = GenConstant(inputToken.type,inputToken.value); 
        }
      else
        {
         ReturnExpression(constraints->minFields);
         constraints->minFields = GenConstant(inputToken.type,inputToken.value); 
        }
     }
   else if ((inputToken.type == SF_VARIABLE) && (strcmp(inputToken.printForm,"?VARIABLE") == 0))
     { /* Do nothing. */ }
   else
     {
      char tempBuffer[120];
      sprintf(tempBuffer,"%s attribute",constraintName);
      SyntaxErrorMessage(tempBuffer);
      return(CLIPS_FALSE);
     }

   /*==========================*/
   /* Parse the maximum value. */
   /*==========================*/

   SavePPBuffer(" ");
   GetToken(readSource,&inputToken);
   if ((inputToken.type == INTEGER) || ((inputToken.type == FLOAT) && range))
     {
      if (range)
        {
         ReturnExpression(constraints->maxValue);
         constraints->maxValue = GenConstant(inputToken.type,inputToken.value); 
        }
      else
        {
         ReturnExpression(constraints->maxFields);
         constraints->maxFields = GenConstant(inputToken.type,inputToken.value); 
        }
     }
   else if ((inputToken.type == SF_VARIABLE) && (strcmp(inputToken.printForm,"?VARIABLE") == 0))
     { /* Do nothing. */ }
   else
     {
      char tempBuffer[120];
      sprintf(tempBuffer,"%s attribute",constraintName);
      SyntaxErrorMessage(tempBuffer);
      return(CLIPS_FALSE);
     }

   /*================================*/
   /* Parse the closing parenthesis. */
   /*================================*/

   GetToken(readSource,&inputToken);
   if (inputToken.type != RPAREN)
     {
      SyntaxErrorMessage("range attribute");
      return(CLIPS_FALSE);
     }

   /*====================================================*/
   /* Minimum value must be less than the maximum value. */
   /*====================================================*/

   if (range)
     {
      if (CompareNumbers(constraints->minValue->type,
                         constraints->minValue->value,
                         constraints->maxValue->type,
                         constraints->maxValue->value) == GREATER_THAN)
        {
         PrintErrorID("CSTRNPSR",2,CLIPS_TRUE);
         PrintCLIPS(WERROR,"Minimum range value must be less than\n");
         PrintCLIPS(WERROR,"or equal to the maximum range value\n");
         return(CLIPS_FALSE);
        }
     }
   else
     {
      if (CompareNumbers(constraints->minFields->type,
                         constraints->minFields->value,
                         constraints->maxFields->type,
                         constraints->maxFields->value) == GREATER_THAN)
        {
         PrintErrorID("CSTRNPSR",2,CLIPS_TRUE);
         PrintCLIPS(WERROR,"Minimum cardinality value must be less than\n");
         PrintCLIPS(WERROR,"or equal to the maximum cardinality value\n");
         return(CLIPS_FALSE);
        }
     }
     
   /*====================================*/
   /* Return TRUE to indicate that the   */
   /* attribute was successfully parsed. */
   /*====================================*/
   
   return(CLIPS_TRUE);
  }

/******************************************************************/
/* GetConstraintTypeFromAllowedName: Returns the type restriction */
/*   associated with an allowed-... attribute.                    */
/******************************************************************/
static int GetConstraintTypeFromAllowedName(constraintName)
  char *constraintName;
  {
   if (strcmp(constraintName,"allowed-values") == 0) return(UNKNOWN);
   else if (strcmp(constraintName,"allowed-symbols") == 0) return(SYMBOL);
   else if (strcmp(constraintName,"allowed-strings") == 0) return(STRING);
   else if (strcmp(constraintName,"allowed-lexemes") == 0) return(SYMBOL_OR_STRING);
   else if (strcmp(constraintName,"allowed-integers") == 0) return(INTEGER);
   else if (strcmp(constraintName,"allowed-numbers") == 0) return(INTEGER_OR_FLOAT);
   else if (strcmp(constraintName,"allowed-instance-names") == 0) return(INSTANCE_NAME);
   else if (strcmp(constraintName,"allowed-floats") == 0) return(FLOAT);

   return(-1);
  }
  
/*******************************************************/
/* GetConstraintTypeFromTypeName: Converts a type name */
/*   to its equivalent integer type restriction.       */
/*******************************************************/
static int GetConstraintTypeFromTypeName(constraintName)
  char *constraintName;
  {
   if (strcmp(constraintName,"SYMBOL") == 0) return(SYMBOL);
   else if (strcmp(constraintName,"STRING") == 0) return(STRING);
   else if (strcmp(constraintName,"LEXEME") == 0) return(SYMBOL_OR_STRING);
   else if (strcmp(constraintName,"INTEGER") == 0) return(INTEGER);
   else if (strcmp(constraintName,"FLOAT") == 0) return(FLOAT);
   else if (strcmp(constraintName,"NUMBER") == 0) return(INTEGER_OR_FLOAT);
   else if (strcmp(constraintName,"INSTANCE-NAME") == 0) return(INSTANCE_NAME);
   else if (strcmp(constraintName,"INSTANCE-ADDRESS") == 0) return(INSTANCE_ADDRESS);
   else if (strcmp(constraintName,"INSTANCE") == 0) return(INSTANCE_OR_INSTANCE_NAME);
   else if (strcmp(constraintName,"EXTERNAL-ADDRESS") == 0) return(EXTERNAL_ADDRESS);
   else if (strcmp(constraintName,"FACT-ADDRESS") == 0) return(FACT_ADDRESS);
#if FUZZY_DEFTEMPLATES /* added 03-05-96 */
   else if (strcmp(constraintName,"FUZZY-VALUE") == 0) return(FUZZY_VALUE);
#endif

   return(-1);
  }

/**************************************************************/
/* GetAttributeParseValue: Returns a boolean value indicating */
/*   whether a specific attribute has already been parsed.    */
/**************************************************************/
static int GetAttributeParseValue(constraintName,parsedConstraints)
  char *constraintName;
  CONSTRAINT_PARSE_RECORD *parsedConstraints;
  {
   if (strcmp(constraintName,"type") == 0) 
     { return(parsedConstraints->type); }
   else if (strcmp(constraintName,"range") == 0) 
     { return(parsedConstraints->range); }
   else if (strcmp(constraintName,"cardinality") == 0) 
     { return(parsedConstraints->cardinality); }
   else if (strcmp(constraintName,"allowed-values") == 0) 
     { return(parsedConstraints->allowedValues); }
   else if (strcmp(constraintName,"allowed-symbols") == 0)
     { return(parsedConstraints->allowedSymbols); }
   else if (strcmp(constraintName,"allowed-strings") == 0) 
     { return(parsedConstraints->allowedStrings); }
   else if (strcmp(constraintName,"allowed-lexemes") == 0) 
     { return(parsedConstraints->allowedLexemes); }
   else if (strcmp(constraintName,"allowed-instance-names") == 0) 
     { return(parsedConstraints->allowedInstanceNames); }
   else if (strcmp(constraintName,"allowed-integers") == 0)
     { return(parsedConstraints->allowedIntegers); }
   else if (strcmp(constraintName,"allowed-floats") == 0) 
     { return(parsedConstraints->allowedFloats); }
   else if (strcmp(constraintName,"allowed-numbers") == 0) 
     { return(parsedConstraints->allowedNumbers); }
   
   return(CLIPS_TRUE);
  }
  
/**********************************************************/
/* SetRestrictionFlag: Sets the restriction flag of a     */
/*   constraint record indicating whether a specific      */
/*   type has an associated allowed-... restriction list. */
/**********************************************************/
static VOID SetRestrictionFlag(restriction,constraints,value)
  int restriction;
  CONSTRAINT_RECORD *constraints;
  int value;
  {
   switch (restriction)
     {
      case UNKNOWN:
         constraints->anyRestriction = value;
         break;
         
      case SYMBOL:
         constraints->symbolRestriction = value;
         break;
         
      case STRING:
         constraints->stringRestriction = value;
         break;
         
      case INTEGER:
         constraints->integerRestriction = value;
         break;
         
      case FLOAT:
         constraints->floatRestriction = value;
         break;
         
      case INTEGER_OR_FLOAT:
         constraints->integerRestriction = value;
         constraints->floatRestriction = value; 
         break; 
         
      case SYMBOL_OR_STRING:
         constraints->symbolRestriction = value;
         constraints->stringRestriction = value; 
         break;   
         
      case INSTANCE_NAME:
         constraints->instanceNameRestriction = value;
         break; 

#if FUZZY_DEFTEMPLATES    /* added 03-05-96 */
      case FUZZY_VALUE:
         constraints->fuzzyValueRestriction = value;
         break;      

#endif
     
     }
  }
  
/********************************************************************/
/* SetParseFlag: Sets the flag in a parsed constraints data         */
/*  structure indicating that a specific attribute has been parsed. */
/********************************************************************/
static VOID SetParseFlag(parsedConstraints,constraintName)
  CONSTRAINT_PARSE_RECORD *parsedConstraints;
  char *constraintName;
  {
   if (strcmp(constraintName,"range") == 0) 
     { parsedConstraints->range = CLIPS_TRUE; }
   else if (strcmp(constraintName,"type") == 0) 
     { parsedConstraints->type = CLIPS_TRUE; }
   else if (strcmp(constraintName,"cardinality") == 0) 
     { parsedConstraints->cardinality = CLIPS_TRUE; }
   else if (strcmp(constraintName,"allowed-symbols") == 0)
     { parsedConstraints->allowedSymbols = CLIPS_TRUE; }
   else if (strcmp(constraintName,"allowed-strings") == 0) 
     { parsedConstraints->allowedStrings = CLIPS_TRUE; }
   else if (strcmp(constraintName,"allowed-lexemes") == 0)
     { parsedConstraints->allowedLexemes = CLIPS_TRUE; }
   else if (strcmp(constraintName,"allowed-integers") == 0) 
     { parsedConstraints->allowedIntegers = CLIPS_TRUE; }
   else if (strcmp(constraintName,"allowed-floats") == 0) 
     { parsedConstraints->allowedFloats = CLIPS_TRUE; }
   else if (strcmp(constraintName,"allowed-numbers") == 0)
     { parsedConstraints->allowedNumbers = CLIPS_TRUE; }
   else if (strcmp(constraintName,"allowed-values") == 0) 
     { parsedConstraints->allowedValues = CLIPS_TRUE; }
  }
  
#endif /* (! RUN_TIME) && (! BLOAD_ONLY) */

