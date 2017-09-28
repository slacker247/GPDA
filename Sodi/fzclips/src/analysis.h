   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  10/05/93            */
   /*                                                     */
   /*                ANALYSIS HEADER FILE                 */
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
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_analysis

#define _H_analysis

#ifndef _H_expressn
#include "expressn.h"
#endif
#ifndef _H_reorder
#include "reorder.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif
#ifdef _ANALYSIS_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ANSI_COMPILER

#if FUZZY_DEFTEMPLATES     /* added 03-14-96 */
   LOCALE unsigned int     FuzzySlotAnalysis(struct lhsParseNode *patternPtr);
#endif

   LOCALE BOOLEAN                        VariableAnalysis(struct lhsParseNode *);
#else

#if FUZZY_DEFTEMPLATES    /* added 03-14-96 */
   LOCALE unsigned int     FuzzySlotAnalysis();
#endif

   LOCALE BOOLEAN                        VariableAnalysis();
#endif

#endif

