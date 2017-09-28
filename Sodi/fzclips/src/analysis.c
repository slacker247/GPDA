   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  10/05/93            */
   /*                                                     */
   /*                  ANALYSIS MODULE                    */
   /*******************************************************/

/*************************************************************/
/* Purpose: Analyzes LHS patterns to check for semantic      */
/*   errors and to determine variable comparisons and other  */
/*   tests which must be performed either in the pattern or  */
/*   join networks.                                          */
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

#define _ANALYSIS_SOURCE_

#include "setup.h"

#if (! RUN_TIME) && (! BLOAD_ONLY) && DEFRULE_CONSTRUCT

#include <stdio.h>
#define _CLIPS_STDIO_

#include "constant.h"
#include "symbol.h"
#include "clipsmem.h"
#include "exprnpsr.h"
#include "reorder.h"
#include "generate.h"
#include "pattern.h"
#include "router.h"
#include "ruledef.h"
#include "cstrnchk.h"
#include "cstrnutl.h"
#include "cstrnops.h"
#include "rulecstr.h"
#include "modulutl.h"
#include "analysis.h"

#if DEFGLOBAL_CONSTRUCT
#include "globldef.h"
#endif

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/
 
#if ANSI_COMPILER
   static int                     GetVariables(struct lhsParseNode *);
   static BOOLEAN                 UnboundVariablesInPattern(struct lhsParseNode *,int);
   static int                     PropagateVariableToNodes(struct lhsParseNode *,
                                                           int,
                                                           struct symbolHashNode *,
                                                           struct lhsParseNode *,
                                                           int,int,int);
   static struct lhsParseNode    *CheckExpression(struct lhsParseNode *,
                                                  struct lhsParseNode *,
                                                  int,
                                                  struct symbolHashNode *,
                                                  int);
   static VOID                    VariableReferenceErrorMessage(struct symbolHashNode *,
                                                                struct lhsParseNode *,
                                                                int,
                                                                struct symbolHashNode *,
                                                                int);
   static int                     ProcessField(struct lhsParseNode *,
                                               struct lhsParseNode *,
                                               struct lhsParseNode *);
   static int                     ProcessVariable(struct lhsParseNode *,
                                               struct lhsParseNode *,
                                               struct lhsParseNode *);
   static VOID                    VariableMixingErrorMessage(struct symbolHashNode *);
   static int                     PropagateVariableDriver(struct lhsParseNode *,
                                                          struct lhsParseNode *,
                                                          struct lhsParseNode *,
                                                          int,struct symbolHashNode *,
                                                          struct lhsParseNode *,
                                                          int);
#else
   static int                     GetVariables();
   static BOOLEAN                 UnboundVariablesInPattern();
   static int                     PropagateVariableToNodes();
   static struct lhsParseNode    *CheckExpression();
   static VOID                    VariableReferenceErrorMessage();
   static int                     ProcessField();
   static int                     ProcessVariable();
   static VOID                    VariableMixingErrorMessage();
   static int                     PropagateVariableDriver();
#endif

#if FUZZY_DEFTEMPLATES   /* added 03-14-96 */
/******************************************************************/
/* FuzzySlotAnalysis: Count number of FuzzySlots in the patterns  */
/*       of the rule is returned.                                 */
/* If Fuzzy Pattern (pattern->bottom->type is FUZZY_VALUE)        */
/* then create an 'expr' of type SCALL_PN_FUZZY_VALUE with argList*/
/* slot pointing to a FUZZY_VALUE expression slot (FUZZY_VALUE_HN)*/
/* and 2nd arg is the slot number                                 */
/*                                                                */ 
/* NOTE: We should have an SF_WILDCARD pattern connected to the   */
/*       FUZZY_VALUE pattern node                                 */
/*       After we just have the SF_WILDCARD node with expr created*/
/*       in its networkTest slot                                  */
/* NOTE: For fuzzy variables we use this analysis phase to        */
/*       determine if the LHS of the rule is FUZZY or CRISP and to*/
/*       count the number of fuzzy slots in LHS  - this info is   */
/*       used later when the rule is created to add pattern-fv..[]*/
/*       array to end of the rule structure for LHS's which are   */
/*       fuzzy. We store ptrs to fuzzy values (hash nodes) there  */
/*       for use in quicker calc of the degree of matching between*/
/*       these fuzzy patterns and the actual facts so that the    */
/*       conclusion can be adjusted accordingly.                  */
/******************************************************************/

globle unsigned int FuzzySlotAnalysis(patternPtr)
struct lhsParseNode *patternPtr;
{
  unsigned int numFuzzySlots = 0;
  struct lhsParseNode *lhsPNPtr, *lhsSlotPtr;
  struct expr *fz_expr;

  lhsPNPtr = patternPtr;

  while (lhsPNPtr != NULL)
    {
      /* check all slots on the pattern for a FUZZY_VALUE */
      lhsSlotPtr = lhsPNPtr->right;
      while (lhsSlotPtr != NULL)
        {
          if (lhsSlotPtr->bottom != NULL &&
	      lhsSlotPtr->bottom->type == FUZZY_VALUE)
            {
             numFuzzySlots++;

             fz_expr = get_struct(expr);
             fz_expr->type = SCALL_PN_FUZZY_VALUE;
             fz_expr->value = NULL;
             fz_expr->argList = get_struct(expr);
             fz_expr->argList->type = FUZZY_VALUE;
             fz_expr->argList->argList = NULL;
             fz_expr->argList->value = (VOID *)lhsSlotPtr->bottom->value;
             fz_expr->argList->nextArg = 
                   GenConstant(INTEGER,(VOID *)AddLong((long) (lhsSlotPtr->slotNumber)-1));
	     fz_expr->nextArg = NULL;
             lhsSlotPtr->bottom->value = NULL; 

             lhsSlotPtr->networkTest = 
                   CombineExpressions(fz_expr,lhsSlotPtr->networkTest);
             ReturnLHSParseNodes(lhsSlotPtr->bottom);
             lhsSlotPtr->bottom = NULL;
            }

          /* on to next slot in pattern */
          lhsSlotPtr = lhsSlotPtr->right;
        }

      /* on to next pattern */
      lhsPNPtr = lhsPNPtr->bottom;
    }

  return( numFuzzySlots );
}

#endif
  
/******************************************************************/
/* VariableAnalysis: Propagates variables references to other     */
/*   variables in the LHS and determines if there are any illegal */
/*   variable references (e.g. referring to an unbound variable). */
/*   The propagation of variable references simply means all      */
/*   subsequent references of a variable are made to "point" back */
/*   to the variable being propagated.                            */
/******************************************************************/
globle int VariableAnalysis(patternPtr)
  struct lhsParseNode *patternPtr;
  {
   struct lhsParseNode *rv, *theList, *tempList;
   int errorFlag = CLIPS_FALSE;

   /*======================================================*/
   /* Loop through all of the CEs in the rule to determine */
   /* which variables refer to other variables and whether */
   /* any semantic errors exist when refering to variables */
   /* (such as referring to a variable that was not        */
   /* previously bound).                                   */
   /*======================================================*/

   while (patternPtr != NULL)
     {
      /*=========================================================*/
      /* If a pattern CE is encountered, propagate any variables */
      /* found in the pattern and note any illegal references to */
      /* other variables.                                        */
      /*=========================================================*/
      
      if (patternPtr->type == PATTERN_CE)
        {         
         /*====================================================*/
         /* Determine if the fact address associated with this */
         /* pattern illegally refers to other variables.       */
         /*====================================================*/
         
         if ((patternPtr->value != NULL) && 
             (patternPtr->referringNode != NULL))
           {
            errorFlag = CLIPS_TRUE;
            if (patternPtr->referringNode->index == -1)
              {
               PrintErrorID("ANALYSIS",1,CLIPS_TRUE);
               PrintCLIPS(WERROR,"Duplicate pattern-address ?");
               PrintCLIPS(WERROR,ValueToString(patternPtr->value));
               PrintCLIPS(WERROR," found in CE #");
               PrintLongInteger(WERROR,(long) patternPtr->whichCE);
               PrintCLIPS(WERROR,".\n");
              }
            else
              {
               PrintErrorID("ANALYSIS",2,CLIPS_TRUE);
               PrintCLIPS(WERROR,"Pattern-address ?");
               PrintCLIPS(WERROR,ValueToString(patternPtr->value));
               PrintCLIPS(WERROR," used in CE #");
               PrintLongInteger(WERROR,(long) patternPtr->whichCE);
               PrintCLIPS(WERROR," was previously bound within a pattern CE.\n");
              }
           }
         
         /*====================================================*/
         /* Propagate the pattern and field location of bound  */
         /* variables found in this pattern to other variables */
         /* in the same semantic scope as the bound variable.  */
         /*====================================================*/
         
         if (GetVariables(patternPtr)) return(CLIPS_TRUE); 
        }
        
      /*==============================================================*/
      /* If a test CE is encountered, make sure that all references   */
      /* to variables have been previously bound. If they are bound   */
      /* then replace the references to variables with function calls */
      /* to retrieve the variables.                                   */
      /*==============================================================*/
       
      else if (patternPtr->type == TEST_CE)
        { 
         /*=====================================================*/
         /* Verify that all variables were referenced properly. */
         /*=====================================================*/
         
         rv = CheckExpression(patternPtr->expression,NULL,(int) patternPtr->whichCE,NULL,0); 
         
         /*=========================================================*/
         /* Determine the type and value constraints implied by the */
         /* expression and propagate these constraints to other     */
         /* variables in the LHS. For example, the expression       */
         /* (+ ?x 1) implies that ?x is a number.                   */
         /*=========================================================*/
         
         theList = GetExpressionVarConstraints(patternPtr->expression);
         for (tempList = theList; tempList != NULL; tempList = tempList->right)
            {
             if (PropagateVariableDriver(patternPtr,patternPtr,NULL,SF_VARIABLE,
                                         tempList->value,tempList,CLIPS_FALSE))
               { 
                ReturnLHSParseNodes(theList);
                return(CLIPS_TRUE);
               }
            }
         ReturnLHSParseNodes(theList);
         
         /*========================================================*/
         /* If the variables in the expression were all referenced */
         /* properly, then create the expression to use in the     */
         /* join network.                                          */
         /*========================================================*/
         
         if (rv != NULL)
           { errorFlag = CLIPS_TRUE; }
         else
           { patternPtr->networkTest = GetvarReplace(patternPtr->expression); }
        }

      /*=====================================================*/
      /* Move on to the next pattern in the LHS of the rule. */
      /*=====================================================*/
      
      patternPtr = patternPtr->bottom;
     }
     
   /*==========================================*/
   /* Return the error status of the analysis. */
   /*==========================================*/
   
   return(errorFlag);
  }
  
/****************************************************************/
/* GetVariables: Loops through each field/slot within a pattern */
/*   and propagates the pattern and field location of bound     */
/*   variables found in the pattern to other variables within   */
/*   the same semantic scope as the bound variables.            */
/****************************************************************/
static int GetVariables(thePattern)
  struct lhsParseNode *thePattern;
  {
   struct lhsParseNode *patternHead = thePattern;
   struct lhsParseNode *multifieldHeader = NULL;
     
   /*======================================================*/
   /* Loop through all the fields/slots found in a pattern */
   /* looking for binding instances of variables.          */
   /*======================================================*/
   
   while (thePattern != NULL)
     {
      /*================================================*/
      /* A multifield slot contains a sublist of fields */
      /* that must be traversed and checked.            */
      /*================================================*/
      
      if (thePattern->multifieldSlot)
        {
         multifieldHeader = thePattern;
         thePattern = thePattern->bottom;
        }
        
      /*==================================================*/
      /* Propagate the binding occurences of single field */
      /* variables, multifield variables, and fact        */
      /* addresses to other occurences of the variable.   */
      /* If an error is encountered, return TRUE.         */
      /*==================================================*/
        
      if (thePattern != NULL)
        {
         if ((thePattern->type == SF_VARIABLE) ||
             (thePattern->type == MF_VARIABLE) ||
             ((thePattern->type == PATTERN_CE) && (thePattern->value != NULL)))
           {
            if (ProcessVariable(thePattern,multifieldHeader,patternHead))
              { return(CLIPS_TRUE); }
           }
         else
           {   
            if (ProcessField(thePattern,multifieldHeader,patternHead))
              { return(CLIPS_TRUE); }
           }
        }
        
      /*===============================================*/
      /* Move on to the next field/slot in the pattern */
      /* or to the next field in a multifield slot.    */
      /*===============================================*/
      
      if (thePattern == NULL) 
        { thePattern = multifieldHeader; }
      else if ((thePattern->right == NULL) && (multifieldHeader != NULL))
        {
         thePattern = multifieldHeader;
         multifieldHeader = NULL;
        }
        
      thePattern = thePattern->right;
     }
     
   /*===============================*/
   /* Return FALSE to indicate that */
   /* no errors were detected.      */
   /*===============================*/
   
   return(CLIPS_FALSE);
  }
  
/******************************************************/
/* ProcessVariable: Processes a single occurence of a */
/*   variable by propagating references to it.        */                     
/******************************************************/
static int ProcessVariable(thePattern,multifieldHeader,patternHead)
  struct lhsParseNode *thePattern, *multifieldHeader, *patternHead;
  {
   int theType;
   struct symbolHashNode *theVariable;
   struct constraintRecord *theConstraints;
   
   /*=============================================================*/
   /* If a pattern address is being propagated, then treat it as  */
   /* a single field pattern variable and create a constraint     */
   /* which indicates that is must be a fact or instance address. */
   /* This code will have to be modified for new data types which */
   /* can match patterns.                                         */
   /*=============================================================*/
   
   if (thePattern->type == PATTERN_CE) 
     {
      theType = SF_VARIABLE;
      theVariable = (struct symbolHashNode *) thePattern->value;
      if (thePattern->derivedConstraints) RemoveConstraint(thePattern->constraints);
      theConstraints = GetConstraintRecord();
      thePattern->constraints = theConstraints;
      thePattern->constraints->anyAllowed = CLIPS_FALSE;
      thePattern->constraints->instanceAddressesAllowed = CLIPS_TRUE;
      thePattern->constraints->factAddressesAllowed = CLIPS_TRUE;
      thePattern->derivedConstraints = CLIPS_TRUE;
     }
   
   /*===================================================*/
   /* Otherwise a pattern variable is being propagated. */
   /*===================================================*/

   else 
     {
      theType = thePattern->type;
      theVariable = (struct symbolHashNode *) thePattern->value;
     }
       
   /*===================================================*/
   /* Propagate the variable location to any additional */
   /* fields associated with the binding variable.      */
   /*===================================================*/
   
   if (thePattern->type != PATTERN_CE)
     {
      PropagateVariableToNodes(thePattern->bottom,theType,theVariable,
                               thePattern,patternHead->beginNandDepth,
                               CLIPS_TRUE,CLIPS_FALSE);  
                               
      if (ProcessField(thePattern,multifieldHeader,patternHead))
        { return(CLIPS_TRUE); }
     }
   
   /*=================================================================*/
   /* Propagate the constraints to other fields, slots, and patterns. */
   /*=================================================================*/
   
   return(PropagateVariableDriver(patternHead,thePattern,multifieldHeader,theType,
                                  theVariable,thePattern,CLIPS_TRUE));
  }
  
/*******************************************/
/* PropagateVariableDriver: Driver routine */
/*   for propagating variable references.  */
/*******************************************/
static int PropagateVariableDriver(patternHead,theNode,multifieldHeader,
                                   theType,variableName,theReference,assignReference)
  struct lhsParseNode *patternHead, *theNode, *multifieldHeader;
  int theType;
  struct symbolHashNode *variableName;
  struct lhsParseNode *theReference;
  int assignReference;
  {
   /*===================================================*/
   /* Propagate the variable location to any additional */
   /* constraints associated with the binding variable. */
   /*===================================================*/
   
   if (multifieldHeader != NULL)
     {
      if (PropagateVariableToNodes(multifieldHeader->right,theType,variableName,
                                   theReference,patternHead->beginNandDepth,assignReference,CLIPS_FALSE))
        {
         VariableMixingErrorMessage(variableName);
         return(CLIPS_TRUE);
        }
     }
             
   /*========================================================*/
   /* Propagate the variable location to fields/slots in the */
   /* same pattern which appear after the binding variable.  */
   /*========================================================*/
   
   if (PropagateVariableToNodes(theNode->right,theType,variableName,theReference,
                                patternHead->beginNandDepth,assignReference,CLIPS_FALSE))
     {
      VariableMixingErrorMessage(variableName);
      return(CLIPS_TRUE);
     }
   
   /*======================================================*/
   /* Propagate values to other patterns if the pattern in */
   /* which the variable is found is not a "not" CE or the */
   /* last pattern within a nand CE.                       */
   /*======================================================*/
   
   if (((patternHead->type == PATTERN_CE) || (patternHead->type == TEST_CE)) && 
       (patternHead->negated == CLIPS_FALSE) &&
       (patternHead->beginNandDepth <= patternHead->endNandDepth))
     {
      int ignoreVariableMixing;
      
      /*============================================================*/
      /* If the variables are propagated from a test CE, then don't */
      /* check for mixing of single and multifield variables (since */
      /* previously bound multifield variables typically have the $ */
      /* removed when passed as an argument to a function unless    */
      /* sequence expansion is desired).                            */
      /*============================================================*/
      
      if (patternHead->type == TEST_CE) ignoreVariableMixing = CLIPS_TRUE;
      else ignoreVariableMixing = CLIPS_FALSE;
      
      /*==========================*/
      /* Propagate the reference. */
      /*==========================*/
      
      if (PropagateVariableToNodes(patternHead->bottom,theType,variableName,theReference,
                                   patternHead->beginNandDepth,assignReference,
                                   ignoreVariableMixing))
       {
         VariableMixingErrorMessage(variableName);
         return(CLIPS_TRUE);
        }
     }
     
   /*==============================================*/
   /* Return FALSE to indicate that no errors were */
   /* generated by the variable propagation.       */
   /*==============================================*/
   
   return(CLIPS_FALSE);
  }
   
/********************************************************/
/* ProcessField: Processes a field or slot of a pattern */
/*   which does not contain a binding variable.         */
/********************************************************/
static int ProcessField(thePattern,multifieldHeader,patternHead)
  struct lhsParseNode *thePattern, *multifieldHeader, *patternHead;
  {
   struct lhsParseNode *theList, *tempList;
   
   /*====================================================*/
   /* Nothing needs to be done for the node representing */
   /* the entire pattern. Return FALSE to indicate that  */
   /* no errors were generated.                          */
   /*====================================================*/
      
   if (thePattern->type == PATTERN_CE) return(CLIPS_FALSE);

   /*====================================================================*/
   /* Derive a set of constraints based on values found in the slot or   */
   /* field. For example, if a slot can only contain the values 1, 2, or */
   /* 3, the field constraint ~2 would generate a constraint record that */
   /* only allows the value 1 or 3. Once generated, the constraints are  */
   /* propagated to other slots and fields.                              */
   /*====================================================================*/
   
   theList = DeriveVariableConstraints(thePattern);
   for (tempList = theList; tempList != NULL; tempList = tempList->right)
     {
      if (PropagateVariableDriver(patternHead,thePattern,multifieldHeader,tempList->type,
                                  tempList->value,tempList,CLIPS_FALSE))
        { 
         ReturnLHSParseNodes(theList);
         return(CLIPS_TRUE);
        }
     }
   ReturnLHSParseNodes(theList);

   /*===========================================================*/
   /* Check for "variable referenced, but not previously bound" */
   /* errors. Return TRUE if this type of error is detected.    */
   /*===========================================================*/
          
   if (UnboundVariablesInPattern(thePattern,(int) patternHead->whichCE))
     { return(CLIPS_TRUE); }
         
   /*==================================================*/
   /* Check for constraint errors for this slot/field. */
   /* If the slot/field has unmatchable constraints    */
   /* then return TRUE to indicate a semantic error.   */
   /*==================================================*/
    
   if (ProcessConnectedConstraints(thePattern,multifieldHeader,patternHead))
     { return(CLIPS_TRUE); }

   /*==============================================================*/
   /* Convert the slot/field constraint to a series of expressions */
   /* that will be used in the pattern and join networks.          */
   /*==============================================================*/
   
   FieldConversion(thePattern,patternHead); 
   
   /*=========================================================*/
   /* Return FALSE to indicate that no errors were generated. */
   /*=========================================================*/
   
   return(CLIPS_FALSE);
  }
  
/*************************************************************/
/* PropagateVariableToNodes: Propagates variable references  */
/*  to all other variables within the semantic scope of the  */
/*  bound variable. That is, a variable reference cannot be  */
/*  beyond an enclosing not/and CE combination. The          */
/*  restriction of propagating variables beyond an enclosing */
/*  not CE is handled within the GetVariables function.      */
/*************************************************************/
static int PropagateVariableToNodes(theNode,theType,variableName,
                                    theReference,startDepth,
                                    assignReference,ignoreVariableTypes)
  struct lhsParseNode *theNode;
  int theType;
  struct symbolHashNode *variableName;
  struct lhsParseNode *theReference;
  int startDepth;
  int assignReference;
  int ignoreVariableTypes;
  {
   struct constraintRecord *tempConstraints;
   
   /*===========================================*/
   /* Traverse the nodes using the bottom link. */
   /*===========================================*/
   
   while (theNode != NULL)
     {
      /*==================================================*/
      /* If the field/slot contains a predicate or return */
      /* value constraint, then propagate the variable to */
      /* the expression associated with that constraint.  */
      /*==================================================*/
      
      if (theNode->expression != NULL)
        { 
         PropagateVariableToNodes(theNode->expression,theType,variableName,
                                  theReference,startDepth,assignReference,CLIPS_TRUE);
        }
        
      /*======================================================*/
      /* If the field/slot is a single or multifield variable */
      /* with the same name as the propagated variable,       */
      /* then propagate the variable location to this node.   */
      /*======================================================*/
      
      else if (((theNode->type == SF_VARIABLE) || (theNode->type == MF_VARIABLE)) &&
               (theNode->value == (VOID *) variableName))
        {
         /*======================================================*/
         /* Check for mixing of single and multifield variables. */
         /*======================================================*/
         
         if (ignoreVariableTypes == CLIPS_FALSE)
           {
            if (((theType == SF_VARIABLE) && (theNode->type == MF_VARIABLE)) ||
                ((theType == MF_VARIABLE) && (theNode->type == SF_VARIABLE)))
              { return(CLIPS_TRUE); }
           }
                  
         /*======================================================*/
         /* Intersect the propagated variable's constraints with */
         /* the current constraints for this field/slot.         */
         /*======================================================*/
         
         if ((theReference->constraints != NULL) && (! theNode->negated))
           {
            tempConstraints = theNode->constraints;
            theNode->constraints = IntersectConstraints(theReference->constraints,
                                                        tempConstraints);
            if (theNode->derivedConstraints)
              { RemoveConstraint(tempConstraints); }
             
            theNode->derivedConstraints = CLIPS_TRUE;
           }
         
            
         /*=====================================================*/
         /* Don't propagate the variable if it originates from  */
         /* a different type of pattern object and the variable */
         /* reference has already been resolved.                */
         /*=====================================================*/
         
         if (assignReference)
           {
            if (theNode->referringNode == NULL) 
              { theNode->referringNode = theReference; }
            else if (theReference->pattern == theNode->pattern)
              { theNode->referringNode = theReference; }
            else if (theReference->patternType == theNode->patternType)
              { theNode->referringNode = theReference; }
           }
        }
        
      /*========================================================*/
      /* If the field/slot is the node representing the entire  */
      /* pattern, then propagate the variable location to the   */
      /* fact address associated with the pattern (if it is the */
      /* same variable name).                                   */
      /*========================================================*/
      
      else if ((theNode->type == PATTERN_CE) && 
               (theNode->value == (VOID *) variableName) &&
               (assignReference == CLIPS_TRUE))
        {
         if (theType == MF_VARIABLE) return(CLIPS_TRUE);
         
         theNode->referringNode = theReference;
        }
        
      /*=====================================================*/
      /* Propagate the variable to other fields contained    */
      /* within the same & field constraint or same pattern. */
      /*=====================================================*/
       
      if (theNode->right != NULL) 
        {
         if (PropagateVariableToNodes(theNode->right,theType,variableName,
                                      theReference,startDepth,assignReference,ignoreVariableTypes))
           { return(CLIPS_TRUE); }
        }
                                  
      /*============================================================*/
      /* Propagate the variable to other patterns within the same   */
      /* semantic scope (if dealing with the node for an entire     */
      /* pattern) or to the next | field constraint within a field. */
      /*============================================================*/
      
      if ((theNode->type == PATTERN_CE) || (theNode->type == TEST_CE))
        { 
         if (theNode->endNandDepth < startDepth) theNode = NULL;
         else theNode = theNode->bottom;
        }
      else
        { theNode = theNode->bottom; }
     }
     
   /*========================================================*/
   /* Return FALSE to indicate that no errors were detected. */
   /*========================================================*/
   
   return(CLIPS_FALSE);
  }

/*************************************************************/
/* UnboundVariablesInPattern: Verifies that variables within */
/*   a slot/field have been referenced properly (i.e. that   */
/*   variables have been previously bound if they are not a  */
/*   binding occurrence).                                    */
/*************************************************************/
static BOOLEAN UnboundVariablesInPattern(theSlot,pattern)
  struct lhsParseNode *theSlot;
  int pattern;
  {
   struct lhsParseNode *andField;
   struct lhsParseNode *rv;
   int result;
   struct lhsParseNode *orField;
   struct symbolHashNode *slotName;
   CONSTRAINT_RECORD *theConstraints;
   int theField;

   /*===================================================*/
   /* If a multifield slot is being checked, then check */
   /* each of the fields grouped with the multifield.   */
   /*===================================================*/
   
   if (theSlot->multifieldSlot)
     {
      theSlot = theSlot->bottom;
      while (theSlot != NULL)
        {
         if (UnboundVariablesInPattern(theSlot,pattern))
           { return(CLIPS_TRUE); }
         theSlot = theSlot->right;
        }
        
      return(CLIPS_FALSE);
     }
    
   /*=======================*/
   /* Check a single field. */
   /*=======================*/
   
   slotName = theSlot->slot;
   theField = theSlot->index;
   theConstraints = theSlot->constraints;
   
   /*===========================================*/
   /* Loop through each of the '|' constraints. */
   /*===========================================*/
   
   for (orField = theSlot->bottom;
        orField != NULL;
        orField = orField->bottom)
     {   
      /*===========================================*/
      /* Loop through each of the fields connected */
      /* by the '&' within the '|' constraint.     */
      /*===========================================*/

      for (andField = orField;
           andField != NULL;
           andField = andField->right)
        {
         /*=======================================================*/
         /* If this is not a binding occurence of a variable and  */
         /* there is no previous binding occurence of a variable, */
         /* then generate an error message for a variable that is */
         /* referred to but not bound.                            */
         /*=======================================================*/
         
         if (((andField->type == SF_VARIABLE) || (andField->type == MF_VARIABLE)) &&
             (andField->referringNode == NULL))
           {
            VariableReferenceErrorMessage((SYMBOL_HN *) andField->value,NULL,pattern,
                                          slotName,theField);
            return(CLIPS_TRUE);
           }
           
         /*==============================================*/
         /* Check predicate and return value constraints */
         /* to insure that all variables used within the */
         /* constraint have been previously bound.       */
         /*==============================================*/
             
         else if ((andField->type == PREDICATE_CONSTRAINT) ||
                  (andField->type == RETURN_VALUE_CONSTRAINT))
           {
            rv = CheckExpression(andField->expression,NULL,pattern,slotName,theField); 
            if (rv != NULL) return(CLIPS_TRUE);
           }
           
         /*========================================================*/
         /* If static constraint checking is being performed, then */
         /* determine if constant values have violated the set of  */
         /* derived constraints for the slot/field (based on the   */
         /* deftemplate definition and propagated constraints).    */
         /*========================================================*/
         
         else if (((andField->type == INTEGER) || (andField->type == FLOAT) ||
                   (andField->type == SYMBOL) || (andField->type == STRING) ||
                   (andField->type == INSTANCE_NAME)) &&
                  GetStaticConstraintChecking())
           {
            result = ConstraintCheckValue(andField->type,andField->value,theConstraints);
            if (result != NO_VIOLATION)
              {
               ConstraintViolationErrorMessage("A literal restriction value",
                                               NULL,CLIPS_FALSE,pattern,
                                               slotName,theField,result,
                                               theConstraints,CLIPS_TRUE);
               return(CLIPS_TRUE);
              }
           }
        }
     }

   /*===============================*/
   /* Return FALSE to indicate that */
   /* no errors were detected.      */
   /*===============================*/
   
   return(CLIPS_FALSE);
  }

/******************************************************************/
/* CheckExpression: Verifies that variables within an expression  */
/*   have been referenced properly. All variables within an       */
/*   expression must have been previously bound.                  */
/******************************************************************/
static struct lhsParseNode *CheckExpression(exprPtr,lastOne,whichCE,slotName,theField)
  struct lhsParseNode *exprPtr, *lastOne;
  int whichCE;
  struct symbolHashNode *slotName;
  int theField;
  {
   struct lhsParseNode *rv;
   int i = 1;

   while (exprPtr != NULL)
     {
      /*===============================================================*/
      /* Check that single field variables contained in the expression */
      /* were previously defined in the LHS. Also check to see if the  */
      /* variable has unmatchable constraints.                         */
      /*===============================================================*/
      
      if (exprPtr->type == SF_VARIABLE) 
        { 
         if (exprPtr->referringNode == NULL)
           {
            VariableReferenceErrorMessage((SYMBOL_HN *) exprPtr->value,lastOne,
                                          whichCE,slotName,theField);
            return(exprPtr);
           }
         else if ((UnmatchableConstraint(exprPtr->constraints)) &&
                  GetStaticConstraintChecking())
           {
            ConstraintReferenceErrorMessage((SYMBOL_HN *) exprPtr->value,lastOne,i,
                                            whichCE,slotName,theField);
            return(exprPtr); 
           }
        }      
      
      /*==================================================*/
      /* Check that multifield variables contained in the */
      /* expression were previously defined in the LHS.   */
      /*==================================================*/

      else if ((exprPtr->type == MF_VARIABLE) && (exprPtr->referringNode == NULL))
        { 
         VariableReferenceErrorMessage((SYMBOL_HN *) exprPtr->value,lastOne,
                                       whichCE,slotName,theField);
         return(exprPtr); 
        }
      
      /*=====================================================*/
      /* Check that global variables are referenced properly */
      /* (i.e. if you reference a global variable, it must   */
      /* already be defined by a defglobal construct).       */
      /*=====================================================*/

#if DEFGLOBAL_CONSTRUCT
      else if (exprPtr->type == GBL_VARIABLE)
        {
         int count;
         
         if (FindImportedConstruct("defglobal",NULL,ValueToString(exprPtr->value),
                                   &count,CLIPS_TRUE,NULL) == NULL)
           {
            VariableReferenceErrorMessage((SYMBOL_HN *) exprPtr->value,lastOne,
                                          whichCE,slotName,theField);
            return(exprPtr);
           }
        }
#endif

      /*============================================*/
      /* Recursively check other function calls to  */
      /* insure variables are referenced correctly. */
      /*============================================*/
      
      else if (((exprPtr->type == FCALL)
#if DEFGENERIC_CONSTRUCT
             || (exprPtr->type == GCALL)
#endif
#if DEFFUNCTION_CONSTRUCT
             || (exprPtr->type == PCALL)
#endif
         ) && (exprPtr->bottom != NULL))
        {
         if ((rv = CheckExpression(exprPtr->bottom,exprPtr,whichCE,slotName,theField)) != NULL)
           { return(rv); }
        }
      
      /*=============================================*/
      /* Move on to the next part of the expression. */
      /*=============================================*/
      
      i++;
      exprPtr = exprPtr->right;
     }

   /*================================================*/
   /* Return NULL to indicate no error was detected. */
   /*================================================*/
   
   return(NULL);
  }
 
/********************************************************/
/* VariableReferenceErrorMessage: Generic error message */
/*   for referencing a variable before it is defined.   */
/********************************************************/
static VOID VariableReferenceErrorMessage(theVariable,theExpression,whichCE,slotName,theField)
  struct symbolHashNode *theVariable;
  struct lhsParseNode *theExpression;
  int whichCE;
  struct symbolHashNode *slotName;
  int theField;
  {
   struct expr *temprv;
   
   /*=============================*/
   /* Print the error message ID. */
   /*=============================*/
   
   PrintErrorID("ANALYSIS",4,CLIPS_TRUE);

   /*=================================*/
   /* Print the name of the variable. */
   /*=================================*/
   
   PrintCLIPS(WERROR,"Variable ?");
   PrintCLIPS(WERROR,ValueToString(theVariable));
   PrintCLIPS(WERROR," ");
   
   /*=================================================*/
   /* If the variable was found inside an expression, */
   /* then print the expression.                      */
   /*=================================================*/
   
   if (theExpression != NULL)
     {
      temprv = LHSParseNodesToExpression(theExpression);
      ReturnExpression(temprv->nextArg);
      temprv->nextArg = NULL;
      PrintCLIPS(WERROR,"found in the expression ");
      PrintExpression(WERROR,temprv);
      PrintCLIPS(WERROR,"\n");
      ReturnExpression(temprv);
     }
   
   /*====================================================*/
   /* Print the CE in which the variable was referenced. */
   /*====================================================*/
   
   PrintCLIPS(WERROR,"was referenced in CE #");
   PrintLongInteger(WERROR,(long int) whichCE);
   
   /*=====================================*/
   /* Identify the slot or field in which */
   /* the variable was found.             */
   /*=====================================*/
   
   if (slotName == NULL)
     {
      if (theField > 0)
        {
         PrintCLIPS(WERROR," field #");
         PrintLongInteger(WERROR,(long int) theField);
        }
     }
   else
     {
      PrintCLIPS(WERROR," slot ");
      PrintCLIPS(WERROR,ValueToString(slotName));
     }
           
   PrintCLIPS(WERROR," before being defined.\n");
  }
  
/************************************************************/
/* VariableMixingErrorMessage: Prints the error message for */
/*   the illegal mixing of single and multifield variables  */
/*   on the LHS of a rule.                                  */
/************************************************************/
static VOID VariableMixingErrorMessage(theVariable)
  struct symbolHashNode *theVariable;
  {
   PrintErrorID("ANALYSIS",3,CLIPS_TRUE);
   PrintCLIPS(WERROR,"Variable ?");
   PrintCLIPS(WERROR,ValueToString(theVariable));
   PrintCLIPS(WERROR," is used as both a single and multifield variable in the LHS\n");
  }

#endif /* (! RUN_TIME) && (! BLOAD_ONLY) && DEFRULE_CONSTRUCT */


