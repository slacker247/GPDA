   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  11/15/93            */
   /*                                                     */
   /*                RETRACT HEADER FILE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose:  Handles join network activity associated with   */
/*   with the removal of a data entity such as a fact or     */
/*   instance.                                               */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_retract
#define _H_retract

#ifndef _H_match
#include "match.h" 
#endif
#ifndef _H_network
#include "network.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _RETRACT_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ANSI_COMPILER
LOCALE VOID                           NetworkRetract(struct patternMatch *);
LOCALE VOID                           PosEntryRetract(struct joinNode *,struct alphaMatch *,struct partialMatch *,int,int);
LOCALE VOID                           ReturnPartialMatch(struct partialMatch *);
LOCALE VOID                           FlushGarbagePartialMatches(void);
LOCALE VOID                           NegEntryRetract(struct joinNode *,struct partialMatch *,int);

#else
LOCALE VOID                           NetworkRetract();
LOCALE VOID                           PosEntryRetract();
LOCALE VOID                           ReturnPartialMatch();
LOCALE VOID                           FlushGarbagePartialMatches();
LOCALE VOID                           NegEntryRetract();
#endif

#ifndef _RETRACT_SOURCE_
   extern struct partialMatch        *GarbagePartialMatches;
   extern struct alphaMatch          *GarbageAlphaMatches;
#endif

#endif



