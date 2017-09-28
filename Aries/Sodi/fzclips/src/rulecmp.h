   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  03/22/94            */
   /*                                                     */
   /*        DEFRULE CONSTRUCT COMPILER HEADER FILE       */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the constructs-to-c feature for the   */
/*    defrule construct.                                     */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/* Contributing Programmer(s):                               */
/*      Bob Orchard (NRCC - Nat'l Research Council of Canada)*/
/*                  (Fuzzy reasoning extensions)             */
/*                  (certainty factors for facts and rules)  */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_rulecmp
#define _H_rulecmp

#include "conscomp.h"
#ifndef _H_extnfunc
#include "extnfunc.h"
#endif

#define JoinPrefix() ArbitraryPrefix(DefruleCodeItem,2)

#if FUZZY_DEFTEMPLATES   /* added 03-12-96 */
#define PatternFvPrefix() ArbitraryPrefix(DefruleCodeItem,3)
#endif

#ifdef LOCALE
#undef LOCALE
#endif
  
#ifdef _RULECMP_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ANSI_COMPILER  
   LOCALE VOID                     DefruleCompilerSetup(void);
   LOCALE VOID                     DefruleCModuleReference(FILE *,int,int,int);
#else
   LOCALE VOID                     DefruleCompilerSetup();
   LOCALE VOID                     DefruleCModuleReference();
#endif 

#ifndef _RULECMP_SOURCE_
extern struct CodeGeneratorItem *DefruleCodeItem;
#endif

#endif




