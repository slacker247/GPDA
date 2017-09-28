   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  03/08/94            */
   /*                                                     */
   /*           FACT RETE PRINT FUNCTIONS MODULE          */
   /*******************************************************/
           
/*************************************************************/
/* Purpose: Print routines for the fact rete primitives.     */
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

#define _FACTPRT_SOURCE_

#include <stdio.h>
#define _CLIPS_STDIO_

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT && DEFRULE_CONSTRUCT

#include "symbol.h"
#include "router.h"
#include "factgen.h"

#include "factprt.h"
  
/***************************************/
/* PrintFactJNCompVars1: Print routine */
/*   for the FactJNCompVars1 function. */
/***************************************/
#if IBM_TBC && (! DEVELOPER)
#pragma argsused
#endif
globle VOID PrintFactJNCompVars1(logicalName,theValue)
  char *logicalName;
  VOID *theValue;
  {
#if DEVELOPER
   struct factCompVarsJN1Call *hack;

   hack = (struct factCompVarsJN1Call *) ValueToBitMap(theValue);
   PrintCLIPS(logicalName,"(fact-jn-cmp-vars1 ");
   if (hack->pass) PrintCLIPS(logicalName,"p ");
   else PrintCLIPS(logicalName,"n ");
   PrintLongInteger(logicalName,(long) hack->slot1);
   PrintCLIPS(logicalName," ");
   PrintLongInteger(logicalName,(long) hack->pattern2);
   PrintCLIPS(logicalName," ");
   PrintLongInteger(logicalName,(long) hack->slot2);
   PrintCLIPS(logicalName,")");
#else
#if MAC_MPW || MAC_MCW     /* added 03-07-96 */
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }  
  
/***************************************/
/* PrintFactJNCompVars2: Print routine */
/*   for the FactJNCompVars2 function. */
/***************************************/
#if IBM_TBC && (! DEVELOPER)
#pragma argsused
#endif
globle VOID PrintFactJNCompVars2(logicalName,theValue)
  char *logicalName;
  VOID *theValue;
  {
#if DEVELOPER
   struct factCompVarsJN2Call *hack;

   hack = (struct factCompVarsJN2Call *) ValueToBitMap(theValue);
   PrintCLIPS(logicalName,"(fact-jn-cmp-vars2 ");
   if (hack->pass) PrintCLIPS(logicalName,"p ");
   else PrintCLIPS(logicalName,"n ");
   
   PrintCLIPS(logicalName,"s");
   PrintLongInteger(logicalName,(long) hack->slot1);
   PrintCLIPS(logicalName," ");
   
   if (hack->fromBeginning1) PrintCLIPS(logicalName,"b ");
   else PrintCLIPS(logicalName,"e ");
   
   PrintCLIPS(logicalName,"f");
   PrintLongInteger(logicalName,(long) hack->offset1);
   PrintCLIPS(logicalName," ");
   
   PrintCLIPS(logicalName,"p");
   PrintLongInteger(logicalName,(long) hack->pattern2);
   PrintCLIPS(logicalName," ");
   
   PrintCLIPS(logicalName,"s");
   PrintLongInteger(logicalName,(long) hack->slot2);
   PrintCLIPS(logicalName," ");
   
   if (hack->fromBeginning2) PrintCLIPS(logicalName,"b ");
   else PrintCLIPS(logicalName,"e ");
   
   PrintCLIPS(logicalName,"f");
   PrintLongInteger(logicalName,(long) hack->offset2);
   PrintCLIPS(logicalName,")");
#else
#if MAC_MPW || MAC_MCW   /* added 03-07-96 */
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }  
  
/***************************************/
/* PrintFactPNCompVars1: Print routine */
/*   for the FactPNCompVars1 function. */
/***************************************/
#if IBM_TBC && (! DEVELOPER)
#pragma argsused
#endif
globle VOID PrintFactPNCompVars1(logicalName,theValue)
  char *logicalName;
  VOID *theValue;
  {
#if DEVELOPER
   struct factCompVarsPN1Call *hack;

   hack = (struct factCompVarsPN1Call *) ValueToBitMap(theValue);
   PrintCLIPS(logicalName,"(fact-pn-cmp-vars ");
   if (hack->pass) PrintCLIPS(logicalName,"p ");
   else PrintCLIPS(logicalName,"n ");
   PrintLongInteger(logicalName,(long) hack->field1);
   PrintCLIPS(logicalName," ");
   PrintLongInteger(logicalName,(long) hack->field2);
   PrintCLIPS(logicalName,")");
#else
#if MAC_MPW || MAC_MCW
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

/**************************************/
/* PrintFactSlotLength: Print routine */
/*   for the FactSlotLength function. */
/**************************************/
#if IBM_TBC && (! DEVELOPER)
#pragma argsused
#endif
globle VOID PrintFactSlotLength(logicalName,theValue)
  char *logicalName;
  VOID *theValue;
  {
#if DEVELOPER   
   struct factCheckLengthPNCall *hack;

   hack = (struct factCheckLengthPNCall *) ValueToBitMap(theValue);
   
   PrintCLIPS(logicalName,"(slot-length ");
   PrintLongInteger(logicalName,(long) hack->whichSlot);
   PrintCLIPS(logicalName," ");
   if (hack->exactly) PrintCLIPS(logicalName,"= ");
   else PrintCLIPS(logicalName,">= ");
   PrintLongInteger(logicalName,(long) hack->minLength);
   PrintCLIPS(logicalName,")");
#else
#if MAC_MPW || MAC_MCW     /* added 03-07-96 */
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }
  
/*************************************/
/* PrintFactJNGetVar1: Print routine */
/*   for the FactJNGetvar1 function. */
/*************************************/
#if IBM_TBC && (! DEVELOPER)
#pragma argsused
#endif
globle VOID PrintFactJNGetVar1(logicalName,theValue)
  char *logicalName;
  VOID *theValue;
  {  
#if DEVELOPER
   struct factGetVarJN1Call *hack;

   hack = (struct factGetVarJN1Call *) ValueToBitMap(theValue);
   PrintCLIPS(logicalName,"(fact-jn-getvar-1 ");
   if (hack->factAddress) PrintCLIPS(logicalName,"t ");
   else PrintCLIPS(logicalName,"f ");
   if (hack->allFields) PrintCLIPS(logicalName,"t ");
   else PrintCLIPS(logicalName,"f ");
   
   PrintLongInteger(logicalName,(long) hack->whichPattern);
   PrintCLIPS(logicalName," ");
   PrintLongInteger(logicalName,(long) hack->whichField);
   PrintCLIPS(logicalName," ");
   PrintLongInteger(logicalName,(long) hack->whichSlot);
   PrintCLIPS(logicalName,")");
#else
#if MAC_MPW || MAC_MCW    /* added 03-07-96 */
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }
  
/*************************************/
/* PrintFactJNGetVar2: Print routine */
/*   for the FactJNGetvar2 function. */
/*************************************/
#if IBM_TBC && (! DEVELOPER)
#pragma argsused
#endif
globle VOID PrintFactJNGetVar2(logicalName,theValue)
  char *logicalName;
  VOID *theValue;
  {  
#if DEVELOPER
   struct factGetVarJN2Call *hack;

   hack = (struct factGetVarJN2Call *) ValueToBitMap(theValue);
   PrintCLIPS(logicalName,"(fact-jn-getvar-2 ");
   
   PrintLongInteger(logicalName,(long) hack->whichPattern);
   PrintCLIPS(logicalName," ");
   PrintLongInteger(logicalName,(long) hack->whichSlot);
   PrintCLIPS(logicalName,")");
#else
#if MAC_MPW || MAC_MCW    /* added 03-07-96 */
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }
  
/*************************************/
/* PrintFactJNGetVar3: Print routine */
/*   for the FactJNGetVar3 function. */
/*************************************/
#if IBM_TBC && (! DEVELOPER)
#pragma argsused
#endif
globle VOID PrintFactJNGetVar3(logicalName,theValue)
  char *logicalName;
  VOID *theValue;
  {  
#if DEVELOPER
   struct factGetVarJN3Call *hack;

   hack = (struct factGetVarJN3Call *) ValueToBitMap(theValue);
   PrintCLIPS(logicalName,"(fact-jn-getvar-3 ");
   if (hack->fromBeginning) PrintCLIPS(logicalName,"t ");
   else PrintCLIPS(logicalName,"f ");
   if (hack->fromEnd) PrintCLIPS(logicalName,"t ");
   else PrintCLIPS(logicalName,"f ");
         
   PrintLongInteger(logicalName,(long) hack->beginOffset);
   PrintCLIPS(logicalName," ");
   PrintLongInteger(logicalName,(long) hack->endOffset);
   PrintCLIPS(logicalName," ");
   PrintLongInteger(logicalName,(long) hack->whichSlot);
   PrintCLIPS(logicalName,")");
#else
#if MAC_MPW || MAC_MCW    /* added 03-07-96 */
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }
  
/*************************************/
/* PrintFactPNGetVar1: Print routine */
/*   for the FactPNGetvar1 function. */
/*************************************/
#if IBM_TBC && (! DEVELOPER)
#pragma argsused
#endif
globle VOID PrintFactPNGetVar1(logicalName,theValue)
  char *logicalName;
  VOID *theValue;
  {  
#if DEVELOPER
   struct factGetVarPN1Call *hack;

   hack = (struct factGetVarPN1Call *) ValueToBitMap(theValue);
   PrintCLIPS(logicalName,"(fact-pn-getvar-1 ");
   if (hack->factAddress) PrintCLIPS(logicalName,"t ");
   else PrintCLIPS(logicalName,"f ");
   if (hack->allFields) PrintCLIPS(logicalName,"t F");
   else PrintCLIPS(logicalName,"f F");
   
   PrintLongInteger(logicalName,(long) hack->whichField);
   PrintCLIPS(logicalName," S");
   PrintLongInteger(logicalName,(long) hack->whichSlot);
   PrintCLIPS(logicalName,")");
#else
#if MAC_MPW || MAC_MCW    /* added 03-07-96 */ 
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

/*************************************/
/* PrintFactPNGetVar2: Print routine */
/*   for the FactPNGetvar2 function. */
/*************************************/
#if IBM_TBC && (! DEVELOPER)
#pragma argsused
#endif
globle VOID PrintFactPNGetVar2(logicalName,theValue)
  char *logicalName;
  VOID *theValue;
  {  
#if DEVELOPER
   struct factGetVarPN2Call *hack;

   hack = (struct factGetVarPN2Call *) ValueToBitMap(theValue);;
   PrintCLIPS(logicalName,"(fact-pn-getvar-2 S");
   PrintLongInteger(logicalName,(long) hack->whichSlot);
   PrintCLIPS(logicalName,")");
#else
#if MAC_MPW || MAC_MCW    /* added 03-07-96 */
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }
  
/*************************************/
/* PrintFactPNGetVar3: Print routine */
/*   for the FactPNGetvar3 function. */
/*************************************/
#if IBM_TBC && (! DEVELOPER)
#pragma argsused
#endif
globle VOID PrintFactPNGetVar3(logicalName,theValue)
  char *logicalName;
  VOID *theValue;
  {  
#if DEVELOPER
   struct factGetVarPN3Call *hack;

   hack = (struct factGetVarPN3Call *) ValueToBitMap(theValue);
   PrintCLIPS(logicalName,"(fact-pn-getvar-3 ");
   
   if (hack->fromBeginning) PrintCLIPS(logicalName,"t ");
   else PrintCLIPS(logicalName,"f ");
   if (hack->fromEnd) PrintCLIPS(logicalName,"t B");
   else PrintCLIPS(logicalName,"f B");
   
   PrintLongInteger(logicalName,(long) hack->beginOffset);
   PrintCLIPS(logicalName," E");
   PrintLongInteger(logicalName,(long) hack->endOffset);
   PrintCLIPS(logicalName," S");
   PrintLongInteger(logicalName,(long) hack->whichSlot);
   PrintCLIPS(logicalName,")");
#else
#if MAC_MPW || MAC_MCW    /* added 03-07-96 */
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  } 
  
/***************************************/
/* PrintFactPNConstant1: Print routine */
/*   for the FactPNConstant1 function. */
/***************************************/
#if IBM_TBC && (! DEVELOPER)
#pragma argsused
#endif
globle VOID PrintFactPNConstant1(logicalName,theValue)
  char *logicalName;
  VOID *theValue;
  {  
#if DEVELOPER
   struct factConstantPN1Call *hack;

   hack = (struct factConstantPN1Call *) ValueToBitMap(theValue);
   
   PrintCLIPS(logicalName,"(fact-pn-constant1 ");
   
   PrintLongInteger(logicalName,(long) hack->whichSlot);
   
   if (hack->testForEquality) PrintCLIPS(logicalName," = ");
   else PrintCLIPS(logicalName," != ");
   
   PrintAtom(logicalName,GetFirstArgument()->type,GetFirstArgument()->value);
   PrintCLIPS(logicalName,")");
#else
#if MAC_MPW || MAC_MCW    /* added 03-07-96 */
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  } 
  
/***************************************/
/* PrintFactPNConstant2: Print routine */
/*   for the FactPNConstant2 function. */
/***************************************/
#if IBM_TBC && (! DEVELOPER)
#pragma argsused
#endif
globle VOID PrintFactPNConstant2(logicalName,theValue)
  char *logicalName;
  VOID *theValue;
  {  
#if DEVELOPER
   struct factConstantPN2Call *hack;

   hack = (struct factConstantPN2Call *) ValueToBitMap(theValue);
   
   PrintCLIPS(logicalName,"(fact-pn-constant2 ");
   
   PrintLongInteger(logicalName,(long) hack->whichSlot);
   
   PrintCLIPS(logicalName," ");
   
   PrintLongInteger(logicalName,(long) hack->offset);
   
   if (hack->testForEquality) PrintCLIPS(logicalName," = ");
   else PrintCLIPS(logicalName," != ");
   
   PrintAtom(logicalName,GetFirstArgument()->type,GetFirstArgument()->value);
   PrintCLIPS(logicalName,")");
#else
#if MAC_MPW || MAC_MCW    /* added 03-07-96 */
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  } 

#if FUZZY_DEFTEMPLATES
  
/***********************************************************************/
/* PrintPNFUZZY_VALUE: Print routine for the PN_FUZZY_VALUE function.  */
/***********************************************************************/
#if IBM_TBC && (! DEVELOPER)
#pragma argsused
#endif
globle VOID PrintPNFUZZY_VALUE(logicalName,theValue)
  char *logicalName;
  VOID *theValue;
  {  
#if DEVELOPER
   
   PrintCLIPS(logicalName,"(fact-pn-fuzzy_value ");
         
   PrintAtom(logicalName,GetFirstArgument()->type,GetFirstArgument()->value);
   PrintCLIPS(logicalName,")");
#else
#if MAC_MPW
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  } 
  
#endif
  
#endif /* DEFTEMPLATE_CONSTRUCT && DEFRULE_CONSTRUCT */


