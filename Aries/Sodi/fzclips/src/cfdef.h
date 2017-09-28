   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  05/12/93            */
   /*                                                     */
   /*             CERTAINTY FACTOR HEADER FILE            */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Donnell                                     */
/*      Bob Orchard (NRCC - Nat'l Research Council of Canada)*/
/*	                (Fuzzy reasoning extensions)             */
/*	                (certainty factors for facts and rules)  */
/*	                (extensions to run command)              */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/


#ifndef _H_cfdef
#define _H_cfdef


#ifndef _H_factmngr
#include "factmngr.h"
#endif


#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _CFDEF_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ANSI_COMPILER
   LOCALE double                possibility( struct fuzzy_value *f1, struct fuzzy_value *f2 );
   LOCALE double                necessity( struct fuzzy_value *f1, struct fuzzy_value *f2 );
   LOCALE double                similarity( struct fuzzy_value *f1, struct fuzzy_value *f2 );
   LOCALE VOID                  InitializeCF();
   LOCALE struct expr           *ParseDeclareUncertainty(char *readSource,
                                                        char *ruleName,
                                                        int *error,
                                                        double *cfVALUE);
   LOCALE double                computeStdConclCF(double theRuleCF,
                                                  struct partialMatch *binds);
#if FUZZY_DEFTEMPLATES
   LOCALE double                computeFuzzyCrispConclCF(struct defrule *theRule,
                                                  struct partialMatch *binds);
#endif
   LOCALE VOID                  changeCFofNewFact(struct fact *newfact);
   LOCALE VOID                  changeCFofExistingFact(struct fact *fact1,struct fact *fact2);
   LOCALE VOID                  changeCFofNewVsExistingFact(struct fact *fact1,struct fact *fact2);
   LOCALE VOID                  cfInformationError(char *);
   LOCALE VOID                  cfRangeError();
   LOCALE VOID                  cfNonNumberError();
   LOCALE VOID                  printCF(char *logicalName, double cf);
   LOCALE double                getcf();
   LOCALE VOID                  set_threshold();
   LOCALE VOID                  unthreshold();
   LOCALE double                get_threshold();
#else
   LOCALE double                possibility( );
   LOCALE double                necessity( );
   LOCALE double                similarity( );
   LOCALE VOID                  InitializeCF();
   LOCALE struct expr           *ParseDeclareUncertainty();
   LOCALE double                computeStdConclCF();
#if FUZZY_DEFTEMPLATES
   LOCALE double                computeFuzzyCrispConclCF();
#endif
   LOCALE VOID                  changeCFofNewFact();
   LOCALE VOID                  changeCFofExistingFact();
   LOCALE VOID                  changeCFofNewVsExistingFact();
   LOCALE VOID                  cfInformationError();
   LOCALE VOID                  cfRangeError();
   LOCALE VOID                  cfNonNumberError();
   LOCALE VOID                  printCF();
   LOCALE double                getcf();
   LOCALE VOID                  set_threshold();
   LOCALE VOID                  unthreshold();
   LOCALE double                get_threshold();

#endif

#ifndef _CFDEF_SOURCE_
   extern double               Threshold_CF;
#endif


#endif


