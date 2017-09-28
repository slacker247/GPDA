   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  03/22/94            */
   /*                                                     */
   /*           DEFRULE LHS PARSING HEADER FILE           */
   /*******************************************************/

/*************************************************************/
/* Purpose: Coordinates parsing of the LHS conditional       */
/*   elements of a rule.                                     */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_rulelhs
#define _H_rulelhs

#ifndef _H_expressn
#include "expressn.h"
#endif
#ifndef _H_reorder
#include "reorder.h"
#endif
#ifndef _H_scanner
#include "scanner.h"
#endif
#ifndef _H_pattern
#include "pattern.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _RULELHS_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif
  
#if ANSI_COMPILER
LOCALE struct lhsParseNode           *ParseRuleLHS(char *,struct token *,char *);
LOCALE VOID                           PropagatePatternType(struct lhsParseNode *,struct patternParser *);
#else
LOCALE struct lhsParseNode           *ParseRuleLHS();
LOCALE VOID                           PropagatePatternType();
#endif


#ifndef _RULELHS_SOURCE_
   extern int                     GlobalSalience;
   extern int                     GlobalAutoFocus;
   extern struct expr            *SalienceExpression;
#if CERTAINTY_FACTORS   /* added 03-12-96 */
   extern double                  GlobalCF;
   extern struct expr            *CFExpression;
#endif
#endif

#endif




