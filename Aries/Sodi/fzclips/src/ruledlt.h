   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  03/22/94            */
   /*                                                     */
   /*          RULE DELETION MODULE HEADER FILE           */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides routines for deleting a rule including  */
/*   freeing the defrule data structures and removing the    */
/*   appropriate joins from the join network.                */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_ruledlt

#define _H_ruledlt

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _RULEDLT_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ANSI_COMPILER
   LOCALE VOID                           ReturnDefrule(VOID *);
#else
   LOCALE VOID                           ReturnDefrule();
#endif

#ifndef _RULEDLT_SOURCE_
#if (! RUN_TIME) && (! BLOAD_ONLY) && DEBUGGING_FUNCTIONS
   extern int                            DeletedRuleDebugFlags;
#endif
#endif

#endif






