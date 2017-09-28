   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  10/13/93            */
   /*                                                     */
   /*          LOGICAL DEPENDENCIES HEADER FILE           */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provide support routines for managing truth      */
/*   maintenance using the logical conditional element.      */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_lgcldpnd

#define _H_lgcldpnd

#ifndef _H_match
#include "match.h"
#endif
#ifndef _H_pattern
#include "pattern.h"
#endif

struct dependency
  {
   VOID *dPtr;
   struct dependency *next;
  };
  
#ifdef LOCALE
#undef LOCALE
#endif
#ifdef _LGCLDPND_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ANSI_COMPILER
   LOCALE BOOLEAN                        AddLogicalDependencies(struct patternEntity *,int);
   LOCALE VOID                           RemoveEntityDependencies(struct patternEntity *);
   LOCALE VOID                           RemovePMDependencies(struct partialMatch *);
   LOCALE VOID                           RemoveLogicalSupport(struct partialMatch *);
   LOCALE VOID                           ForceLogicalRetractions(void);
   LOCALE VOID                           Dependencies(struct patternEntity *);
   LOCALE VOID                           Dependents(struct patternEntity *);
   LOCALE VOID                           DependenciesCommand(void);
   LOCALE VOID                           DependentsCommand(void);
#else
   LOCALE BOOLEAN                        AddLogicalDependencies();
   LOCALE VOID                           RemoveEntityDependencies();
   LOCALE VOID                           RemovePMDependencies();
   LOCALE VOID                           RemoveLogicalSupport();
   LOCALE VOID                           ForceLogicalRetractions();
   LOCALE VOID                           Dependencies();
   LOCALE VOID                           Dependents();
   LOCALE VOID                           DependenciesCommand();
   LOCALE VOID                           DependentsCommand();
#endif

#endif





