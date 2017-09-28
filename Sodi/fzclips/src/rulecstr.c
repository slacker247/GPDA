   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  06/20/94            */
   /*                                                     */
   /*              RULE CONSTRAINTS MODULE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides routines for detecting constraint       */
/*   conflicts in the LHS and RHS of rules.                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#define _RULECSTR_SOURCE_

#include "setup.h"

#if (! RUN_TIME) && (! BLOAD_ONLY) && DEFRULE_CONSTRUCT

#include <stdio.h>
#define _CLIPS_STDIO_

#include "router.h"
#include "reorder.h"
#include "cstrnchk.h"
#include "cstrnops.h"
#include "extnfunc.h"
#include "analysis.h"
#include "prcdrpsr.h"
#include "cstrnutl.h"
#include "rulepsr.h"

#include "rulecstr.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/
 
#if ANSI_COMPILER
   static BOOLEAN                 CheckForUnmatchableConstraints(struct lhsParseNode *,int);
   static BOOLEAN                 MultifieldCardinalityViolation(struct lhsParseNode *);
   static struct lhsParseNode    *UnionVariableConstraints(struct lhsParseNode *,
                                                     struct lhsParseNode *);
   static struct lhsParseNode    *AddToVariableConstraints(struct lhsParseNode *,
                                                    struct lhsParseNode *);
   static VOID                    ConstraintConflictMessage(struct symbolHashNode *,
                                                            int,int,struct symbolHashNode *);
   static BOOLEAN                 CheckArgumentForConstraintError(struct expr *,struct expr*,
                                                                  int,struct FunctionDefinition *,
                                                                  struct lhsParseNode *);
#else
   static BOOLEAN                 CheckForUnmatchableConstraints();
   static BOOLEAN                 MultifieldCardinalityViolation();
   static struct lhsParseNode    *UnionVariableConstraints();
   static struct lhsParseNode    *AddToVariableConstraints();
   static VOID                    ConstraintConflictMessage();
   static BOOLEAN                 CheckArgumentForConstraintError();
#endif
  
/***********************************************************/
/* CheckForUnmatchableConstraints: Determines if a LHS CE  */
/*   node contains unmatchable constraints. Return TRUE if */
/*   there are unmatchable constraints, otherwise FALSE.   */
/***********************************************************/
static BOOLEAN CheckForUnmatchableConstraints(theNode,whichCE)
  struct lhsParseNode *theNode;
  int whichCE;
  {   
   if (GetStaticConstraintChecking() == CLIPS_FALSE) return(CLIPS_FALSE);
      
   if (UnmatchableConstraint(theNode->constraints))
     {
      ConstraintConflictMessage(theNode->value,whichCE,
                                theNode->index,theNode->slot);
      return(CLIPS_TRUE);
     }
     
   return(CLIPS_FALSE);
  }

/******************************************************/
/* ConstraintConflictMessage: Error message used when */
/*   a constraint restriction for a slot prevents any */
/*   value from matching the pattern constraint.      */
/******************************************************/
static VOID ConstraintConflictMessage(variableName,thePattern,theField,theSlot)
  struct symbolHashNode *variableName;
  int thePattern, theField;
  struct symbolHashNode *theSlot;
  {
   /*=========================*/
   /* Print the error header. */
   /*=========================*/
   
   PrintErrorID("RULECSTR",1,CLIPS_TRUE);
   
   /*======================================================*/
   /* Print the variable name (if available) and CE number */
   /* for which the constraint violation occurred.         */
   /*======================================================*/
   
   if (variableName != NULL)
     {
      PrintCLIPS(WERROR,"Variable ?");
      PrintCLIPS(WERROR,ValueToString(variableName));
      PrintCLIPS(WERROR," in CE #");
      PrintLongInteger(WERROR,(long int) thePattern);
     }
   else
     {
      PrintCLIPS(WERROR,"Pattern #");
      PrintLongInteger(WERROR,(long int) thePattern);
     }
     
   /*=======================================*/
   /* Print the slot name or field position */
   /* in which the violation occurred.      */
   /*=======================================*/
   
   if (theSlot == NULL)
     {
      PrintCLIPS(WERROR," field #");
      PrintLongInteger(WERROR,(long int) theField);
     }
   else
     {
      PrintCLIPS(WERROR," slot ");
      PrintCLIPS(WERROR,ValueToString(theSlot));
     }
     
   /*======================================*/
   /* Print the rest of the error message. */
   /*======================================*/
   
   PrintCLIPS(WERROR,"\nhas constraint conflicts which make the pattern unmatchable.\n");
  }
  
/***************************************************************/
/* MultifieldCardinalityViolation: Determines if a cardinality */
/*   violation has occurred for a LHS CE node.                 */
/***************************************************************/
static BOOLEAN MultifieldCardinalityViolation(theNode)
  struct lhsParseNode *theNode;
  {
   struct lhsParseNode *tmpNode;
   struct expr *tmpMax;
   long minFields = 0;
   long maxFields = 0;
   int positiveInfinity = CLIPS_FALSE;
   CONSTRAINT_RECORD *newConstraint, *tempConstraint;
   
   /*================================*/
   /* A single field slot can't have */
   /* a cardinality violation.       */
   /*================================*/
   
   if (theNode->multifieldSlot == CLIPS_FALSE) return(CLIPS_FALSE);
   
   /*=============================================*/
   /* Determine the minimum and maximum number of */
   /* fields the slot could contain based on the  */
   /* slot constraints found in the pattern.      */
   /*=============================================*/
   
   for (tmpNode = theNode->bottom;
        tmpNode != NULL;
        tmpNode = tmpNode->right)
     {
      /*====================================================*/
      /* A single field variable increases both the minimum */
      /* and maximum number of fields by one.               */
      /*====================================================*/
      
      if ((tmpNode->type == SF_VARIABLE) ||
          (tmpNode->type == SF_WILDCARD))
        {
         minFields++;
         maxFields++;
        }
        
      /*=================================================*/
      /* Otherwise a multifield wildcard or variable has */
      /* been encountered. If it is constrained then use */
      /* minimum and maximum number of fields constraint */
      /* associated with this LHS node.                  */
      /*=================================================*/
      
      else if (tmpNode->constraints != NULL)
        {
         /*=======================================*/
         /* The lowest minimum of all the min/max */
         /* pairs will be the first in the list.  */
         /*=======================================*/
         
         if (tmpNode->constraints->minFields->value != NegativeInfinity)
           { minFields += ValueToLong(tmpNode->constraints->minFields->value); }
           
         /*=========================================*/
         /* The greatest maximum of all the min/max */
         /* pairs will be the last in the list.     */
         /*=========================================*/
         
         tmpMax = tmpNode->constraints->maxFields;
         while (tmpMax->nextArg != NULL) tmpMax = tmpMax->nextArg;
         if (tmpMax->value == PositiveInfinity)
           { positiveInfinity = CLIPS_TRUE; }
         else
           { maxFields += ValueToLong(tmpMax->value); }
        }
        
      /*================================================*/
      /* Otherwise an unconstrained multifield wildcard */
      /* or variable increases the maximum number of    */
      /* fields to positive infinity.                   */
      /*================================================*/
      
      else
        { positiveInfinity = CLIPS_TRUE; }
     }
     
   /*==================================================================*/
   /* Create a constraint record for the cardinality of the sum of the */
   /* cardinalities of the restrictions inside the multifield slot.    */
   /*==================================================================*/
   
   if (theNode->constraints == NULL) tempConstraint = GetConstraintRecord();
   else tempConstraint = CopyConstraintRecord(theNode->constraints);
   ReturnExpression(tempConstraint->minFields);
   ReturnExpression(tempConstraint->maxFields);
   tempConstraint->minFields = GenConstant(INTEGER,AddLong((long) minFields));
   if (positiveInfinity) tempConstraint->maxFields = GenConstant(SYMBOL,PositiveInfinity);
   else tempConstraint->maxFields = GenConstant(INTEGER,AddLong((long) maxFields));
   
   /*================================================================*/
   /* Determine the final cardinality for the multifield slot by     */
   /* intersecting the cardinality sum of the restrictions within    */
   /* the multifield slot with the original cardinality of the slot. */
   /*================================================================*/
   
   newConstraint = IntersectConstraints(theNode->constraints,tempConstraint);
   if (theNode->derivedConstraints) RemoveConstraint(theNode->constraints);
   RemoveConstraint(tempConstraint);
   theNode->constraints = newConstraint;
   theNode->derivedConstraints = CLIPS_TRUE;
   
   /*===================================================================*/
   /* Determine if the final cardinality for the slot can be satisfied. */
   /*===================================================================*/
   
   if (GetStaticConstraintChecking() == CLIPS_FALSE) return(CLIPS_FALSE);
   if (UnmatchableConstraint(newConstraint)) return(CLIPS_TRUE);
     
   return(CLIPS_FALSE);
  }
  
/***************************************************/
/* ProcessConnectedConstraints: Examines a single  */
/*   connected constraint searching for constraint */
/*   violations.                                   */
/***************************************************/
globle BOOLEAN ProcessConnectedConstraints(theNode,multifieldHeader,patternHead)
  struct lhsParseNode *theNode, *multifieldHeader, *patternHead;
  {
   struct constraintRecord *orConstraints = NULL, *andConstraints;
   struct constraintRecord *tmpConstraints, *rvConstraints;
   struct lhsParseNode *orNode, *andNode;
   struct expr *tmpExpr;
     
   /*============================================*/
   /* Loop through all of the or (|) constraints */
   /* found in the connected constraint.         */
   /*============================================*/     
   
   for (orNode = theNode->bottom; orNode != NULL; orNode = orNode->bottom)
     {
      /*=================================================*/
      /* Intersect all of the &'ed constraints together. */
      /*=================================================*/
   
      andConstraints = NULL;
      for (andNode = orNode; andNode != NULL; andNode = andNode->right)
        {
         if (! andNode->negated)
           {
            if (andNode->type == RETURN_VALUE_CONSTRAINT)
              {
               if (andNode->expression->type == FCALL)
                 {
                  rvConstraints = FunctionCallToConstraintRecord(andNode->expression->value);
                  tmpConstraints = andConstraints;
                  andConstraints = IntersectConstraints(andConstraints,rvConstraints);
                  RemoveConstraint(tmpConstraints);
                  RemoveConstraint(rvConstraints);
                 }
              }
            else if (ConstantType(andNode->type))
              {
               tmpExpr = GenConstant(andNode->type,andNode->value);
               rvConstraints = ExpressionToConstraintRecord(tmpExpr);
               tmpConstraints = andConstraints;
               andConstraints = IntersectConstraints(andConstraints,rvConstraints);
               RemoveConstraint(tmpConstraints);
               RemoveConstraint(rvConstraints);
               ReturnExpression(tmpExpr); 
              }
            else if (andNode->constraints != NULL)
              {
               tmpConstraints = andConstraints;
               andConstraints = IntersectConstraints(andConstraints,andNode->constraints);
               RemoveConstraint(tmpConstraints);
              }
           }
        }
        
      /*===========================================================*/
      /* Intersect the &'ed constraints with the slot constraints. */
      /*===========================================================*/
      
      tmpConstraints = andConstraints;
      andConstraints = IntersectConstraints(andConstraints,theNode->constraints);
      RemoveConstraint(tmpConstraints);
               
      /*===============================================================*/
      /* Remove any negated constants from the list of allowed values. */
      /*===============================================================*/
      
      for (andNode = orNode; andNode != NULL; andNode = andNode->right)
        {
         if ((andNode->negated) && ConstantType(andNode->type))
             { RemoveConstantFromConstraint(andNode->type,andNode->value,andConstraints); }
        }
        
      /*=======================================================*/
      /* Union the &'ed constraints with the |'ed constraints. */
      /*=======================================================*/
      
      tmpConstraints = orConstraints;
      orConstraints = UnionConstraints(orConstraints,andConstraints);
      RemoveConstraint(tmpConstraints);
      RemoveConstraint(andConstraints);
     }
     
   /*===============================================*/
   /* Replace the constraints for the slot with the */
   /* constraints derived from the connected        */
   /* constraints (which should be a subset.        */
   /*===============================================*/
   
   if (orConstraints != NULL) 
     {
      if (theNode->derivedConstraints) RemoveConstraint(theNode->constraints);
      theNode->constraints = orConstraints;
      theNode->derivedConstraints = CLIPS_TRUE;
     }
   
   /*==================================*/
   /* Check for constraint violations. */
   /*==================================*/
   
   if (CheckForUnmatchableConstraints(theNode,(int) patternHead->whichCE)) 
     { return(CLIPS_TRUE); }
          
   /*=========================================*/
   /* If the constraints are for a multifield */
   /* slot, check for cardinality violations. */
   /*=========================================*/
   
   if ((multifieldHeader != NULL) && (theNode->right == NULL))
     {
      if (MultifieldCardinalityViolation(multifieldHeader)) 
        { 
         ConstraintViolationErrorMessage("The group of restrictions",
                                                  NULL,CLIPS_FALSE,
                                                  (int) patternHead->whichCE,
                                                  multifieldHeader->slot,
                                                  multifieldHeader->index,
                                                  CARDINALITY_VIOLATION,
                                                  multifieldHeader->constraints,CLIPS_TRUE);
          return(CLIPS_TRUE);
         }
      }
      
   /*=======================================*/
   /* Return FALSE indicating no constraint */
   /* violations were detected.             */
   /*=======================================*/
   
   return(CLIPS_FALSE);
  }

/**************************************************/
/* ConstraintReferenceErrorMessage: Generic error */
/*   message for LHS constraint violation errors  */
/*   that occur within an expression.             */
/**************************************************/
globle VOID ConstraintReferenceErrorMessage(theVariable,theExpression,whichArgument,
                                            whichCE,slotName,theField)
  struct symbolHashNode *theVariable;
  struct lhsParseNode *theExpression;
  int whichArgument;
  int whichCE;
  struct symbolHashNode *slotName;
  int theField;
  {
   struct expr *temprv;
   
   PrintErrorID("RULECSTR",2,CLIPS_TRUE);

   /*==========================*/
   /* Print the variable name. */
   /*==========================*/
   
   PrintCLIPS(WERROR,"Previous variable bindings of ?");
   PrintCLIPS(WERROR,ValueToString(theVariable));
   PrintCLIPS(WERROR," caused the type restrictions");
   
   /*============================*/
   /* Print the argument number. */
   /*============================*/
   
   PrintCLIPS(WERROR,"\nfor argument #");
   PrintLongInteger(WERROR,(long int) whichArgument);
   
   /*=======================*/
   /* Print the expression. */
   /*=======================*/
   
   PrintCLIPS(WERROR," of the expression ");
   temprv = LHSParseNodesToExpression(theExpression);
   ReturnExpression(temprv->nextArg);
   temprv->nextArg = NULL;
   PrintExpression(WERROR,temprv);
   PrintCLIPS(WERROR,"\n");
   ReturnExpression(temprv);
   
   /*========================================*/
   /* Print out the index of the conditional */
   /* element and the slot name or field     */
   /* index where the violation occured.     */
   /*========================================*/
         
   PrintCLIPS(WERROR,"found in CE #");
   PrintLongInteger(WERROR,(long int) whichCE);
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
           
   PrintCLIPS(WERROR," to be violated.\n");
  }
  
/********************************************************/
/* AddToVariableConstraints: Adds the constraints for a */
/*   variable to a list of constraints. If the variable */
/*   is already in the list, the constraints for the    */
/*   variable are intersected with the new constraints. */
/********************************************************/
static struct lhsParseNode *AddToVariableConstraints(oldList,newItems)
  struct lhsParseNode *oldList, *newItems;
  {
   CONSTRAINT_RECORD *newConstraints;
   struct lhsParseNode *temp, *trace;
      
   /*=================================================*/
   /* Loop through each of the new constraints adding */
   /* it to the list if it's not already present or   */
   /* modifying the constraint if it is.              */
   /*=================================================*/
   
   while (newItems != NULL)
     {
      /*==========================================*/
      /* Get the next item since the next pointer */
      /* value (right) needs to be set to NULL.   */
      /*==========================================*/
      
      temp = newItems->right;
      newItems->right = NULL;
            
      /*===================================*/
      /* Search the list for the variable. */
      /*===================================*/
      
      for (trace = oldList; trace != NULL; trace = trace->right)
        {
         /*=========================================*/
         /* If the variable is already in the list, */
         /* modify the constraint already there to  */
         /* include the new constraint.             */
         /*=========================================*/
         
         if (trace->value == newItems->value)
           {
            newConstraints = IntersectConstraints(trace->constraints,
                                                  newItems->constraints);
            RemoveConstraint(trace->constraints);
            trace->constraints = newConstraints;
            ReturnLHSParseNodes(newItems);
            break;
           }
        }
        
      /*=================================*/
      /* Add the variable constraints to */
      /* the list if it wasn't found.    */
      /*=================================*/
      
      if (trace == NULL)
        {
         newItems->right = oldList;   /* changed 03-12-96 */
         oldList = newItems;          /* changed 03-12-96 */
        }
        
      /*===========================*/
      /* Move on to the next item. */
      /*===========================*/
      
      newItems = temp;
     }
     
   return(oldList);
  }
  
/***********************************************************/
/* UnionVariableConstraints: Unions two lists of variable  */
/*   constraints. If a variable appears in one list  but   */
/*   not the other, then the variable is unconstrained and */
/*   thus not included in the unioned list.                */
/***********************************************************/
static struct lhsParseNode *UnionVariableConstraints(list1,list2)
  struct lhsParseNode *list1, *list2;
  {
   struct lhsParseNode *list3 = NULL, *trace, *temp;
   
   /*===================================*/
   /* Loop through all of the variables */
   /* in the first list.                */
   /*===================================*/
   
   while (list1 != NULL)
     {
      /*=============================================*/
      /* Search for the variable in the second list. */
      /*=============================================*/
      
      for (trace = list2; trace != NULL; trace = trace->right)
        {
         /*============================================*/
         /* If the variable is found in both lists,    */
         /* union the constraints and add the variable */
         /* to the new list being constructed.         */
         /*============================================*/
         
         if (list1->value == trace->value)
           {
            temp = GetLHSParseNode();
            temp->derivedConstraints = CLIPS_TRUE;
            temp->value = list1->value;
            temp->constraints = UnionConstraints(list1->constraints,trace->constraints);
            temp->right = list3;
            list3 = temp;
            break;
           }
        }
        
      /*==============================*/
      /* Move on to the next variable */
      /* in the first list.           */
      /*==============================*/
      
      temp = list1->right;
      list1->right = NULL;
      ReturnLHSParseNodes(list1);
      list1 = temp;
     }
     
   /*====================================*/
   /* Free the items in the second list. */
   /*====================================*/
   
   ReturnLHSParseNodes(list2);

   /*======================*/
   /* Return the new list. */
   /*======================*/
   
   return(list3);
  }
  
/*****************************************************************/
/* GetExpressionVarConstraints: Given an expression stored using */
/*   the LHS parse node data structures, determines and returns  */
/*   the constraints on variables caused by that expression. For */
/*   example, the expression (+ ?x 1) would imply a numeric type */
/*   constraint for the variable ?x since the addition function  */
/*   expects numeric arguments.                                  */
/*****************************************************************/
globle struct lhsParseNode *GetExpressionVarConstraints(theExpression)
  struct lhsParseNode *theExpression;
  {
   struct lhsParseNode *list1 = NULL, *list2;
   
   for (; theExpression != NULL; theExpression = theExpression->bottom)
     {
      if (theExpression->right != NULL)
        { 
         list2 = GetExpressionVarConstraints(theExpression->right); 
         list1 = AddToVariableConstraints(list2,list1);
        }
      
      if (theExpression->type == SF_VARIABLE)
        {
         list2 = GetLHSParseNode();
         if (theExpression->referringNode != NULL)
           { list2->type = theExpression->referringNode->type; }
         else
           { list2->type = SF_VARIABLE; }
         list2->value = theExpression->value;
         list2->derivedConstraints = CLIPS_TRUE;
         list2->constraints = CopyConstraintRecord(theExpression->constraints);
         list1 = AddToVariableConstraints(list2,list1);
        }
     }
     
   return(list1);   
  }
  
/***********************************************/
/* DeriveVariableConstraints: Derives the list */
/*   of variable constraints associated with a */
/*   single connected constraint.              */
/***********************************************/
globle struct lhsParseNode *DeriveVariableConstraints(theNode)
  struct lhsParseNode *theNode;
  {
   struct lhsParseNode *orNode, *andNode;
   struct lhsParseNode *list1, *list2, *list3 = NULL;
   int first = CLIPS_TRUE;
     
   /*===============================*/
   /* Process the constraints for a */
   /* single connected constraint.  */
   /*===============================*/     
   
   for (orNode = theNode->bottom; orNode != NULL; orNode = orNode->bottom)
     {
      /*=================================================*/
      /* Intersect all of the &'ed constraints together. */
      /*=================================================*/
     
      list2 = NULL;
      for (andNode = orNode; andNode != NULL; andNode = andNode->right)
        {
         if ((andNode->type == RETURN_VALUE_CONSTRAINT) ||
             (andNode->type == PREDICATE_CONSTRAINT))
           {
            list1 = GetExpressionVarConstraints(andNode->expression);
            list2 = AddToVariableConstraints(list2,list1);
           }
        }
        
      if (first)
        {
         list3 = list2;
         first = CLIPS_FALSE;
        }
      else
        { list3 = UnionVariableConstraints(list3,list2); }
     }
     
   return(list3);
  }
  
/*******************************************/
/* CheckRHSForConstraintErrors: Checks the */
/*   RHS of a rule for constraint errors.  */
/*******************************************/
globle BOOLEAN CheckRHSForConstraintErrors(expressionList,theLHS)
  struct expr *expressionList;
  struct lhsParseNode *theLHS;
  {
   struct FunctionDefinition *theFunction;
   int i;
   struct expr *lastOne, *checkList, *tmpPtr;
   
   if (expressionList == NULL) return(CLIPS_FALSE);
      
   for (checkList = expressionList; 
        checkList != NULL;
        checkList = checkList->nextArg)
      {
       expressionList = checkList->argList;
       i = 1;
       if (checkList->type == FCALL)
         {
          lastOne = checkList;
          theFunction = (struct FunctionDefinition *) checkList->value;
         }
       else
         { theFunction = NULL; }
   
       while (expressionList != NULL)
         {
          if (CheckArgumentForConstraintError(expressionList,lastOne,i,
                                              theFunction,theLHS))
            { return(CLIPS_TRUE); }
        
          i++;   
          tmpPtr = expressionList->nextArg;
          expressionList->nextArg = NULL;
          if (CheckRHSForConstraintErrors(expressionList,theLHS)) return(CLIPS_TRUE);
          expressionList->nextArg = tmpPtr;
          expressionList = expressionList->nextArg;
         }
      }
     
   return(CLIPS_FALSE);
  }

/*************************************************************/
/* CheckArgumentForConstraintError: Checks a single argument */
/*   found in the RHS of a rule for constraint errors.       */
/*   Returns TRUE if an error is detected, otherwise FALSE.  */
/*************************************************************/
static BOOLEAN      /* added 03-12-96 */ CheckArgumentForConstraintError(expressionList,lastOne,i,
                                               theFunction,theLHS)
  struct expr *expressionList;
  struct expr *lastOne;
  int i;  
  struct FunctionDefinition *theFunction;
  struct lhsParseNode *theLHS;
  {
   int theRestriction;
   CONSTRAINT_RECORD *constraint1, *constraint2, *constraint3, *constraint4;
   struct lhsParseNode *theVariable;
   struct expr *tmpPtr;
   int rv = CLIPS_FALSE;
   
   /*=============================================================*/
   /* Skip anything that isn't a variable or isn't an argument to */
   /* a user defined function (i.e. deffunctions and generic have */
   /* no constraint information so they aren't checked).          */
   /*=============================================================*/
   
   if ((expressionList->type != SF_VARIABLE) || (theFunction == NULL)) 
     { return (rv); }
   
   /*===========================================*/
   /* Get the restrictions for the argument and */
   /* convert them to a constraint record.      */
   /*===========================================*/
   
   theRestriction = GetNthRestriction(theFunction,i);
   constraint1 = ArgumentTypeToConstraintRecord(theRestriction);
         
   /*================================================*/
   /* Look for the constraint record associated with */
   /* binding the variable in the LHS of the rule.   */
   /*================================================*/
   
   theVariable = FindVariable((SYMBOL_HN *) expressionList->value,theLHS);
   if (theVariable != NULL)
     {
      if (theVariable->type == MF_VARIABLE)
        {
         constraint2 = GetConstraintRecord();
         SetConstraintType(MULTIFIELD,constraint2);
        }
      else if (theVariable->constraints == NULL)
        { constraint2 = GetConstraintRecord(); }
      else
        { constraint2 = CopyConstraintRecord(theVariable->constraints); }
     }
   else
     { constraint2 = NULL; }
           
   /*================================================*/
   /* Look for the constraint record associated with */
   /* binding the variable on the RHS of the rule.   */
   /*================================================*/
   
   constraint3 = FindBindConstraints((SYMBOL_HN *) expressionList->value);
   
   /*====================================================*/
   /* Union the LHS and RHS variable binding constraints */
   /* (the variable must satisfy one or the other).      */
   /*====================================================*/
   
   constraint3 = UnionConstraints(constraint3,constraint2);
   
   /*====================================================*/
   /* Intersect the LHS/RHS variable binding constraints */
   /* with the function argument restriction constraints */
   /* (the variable must satisfy both).                  */
   /*====================================================*/
   
   constraint4 = IntersectConstraints(constraint3,constraint1);
         
   /*====================================*/
   /* Check for unmatchable constraints. */
   /*====================================*/

   if (UnmatchableConstraint(constraint4) && GetStaticConstraintChecking())
     {
      PrintErrorID("RULECSTR",3,CLIPS_TRUE);
      PrintCLIPS(WERROR,"Previous variable bindings of ?");
      PrintCLIPS(WERROR,ValueToString((SYMBOL_HN *) expressionList->value));
      PrintCLIPS(WERROR," caused the type restrictions");
      PrintCLIPS(WERROR,"\nfor argument #");
      PrintLongInteger(WERROR,(long int) i);
      PrintCLIPS(WERROR," of the expression ");
      tmpPtr = lastOne->nextArg;
      lastOne->nextArg = NULL;
      PrintExpression(WERROR,lastOne);
      lastOne->nextArg = tmpPtr;
      PrintCLIPS(WERROR,"\nfound in the rule's RHS to be violated.\n");
            
      rv = CLIPS_TRUE;
     }
           
   /*===========================================*/
   /* Free the temporarily created constraints. */
   /*===========================================*/
   
   RemoveConstraint(constraint1);
   RemoveConstraint(constraint2);
   RemoveConstraint(constraint3);
   RemoveConstraint(constraint4);
   
   /*========================================*/
   /* Return TRUE if unmatchable constraints */
   /* were detected, otherwise FALSE.        */
   /*========================================*/
   
   return(rv);
  }
  
#endif /* (! RUN_TIME) && (! BLOAD_ONLY) && DEFRULE_CONSTRUCT */
