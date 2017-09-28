   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  05/12/93            */
   /*                                                     */
   /*             FUZZY UTILITY HEADER                    */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Donnell                                     */
/*      Bob Orchard (NRCC - Nat'l Research Council of Canada)*/
/*                  (Fuzzy reasoning extensions)             */
/*                  (certainty factors for facts and rules)  */
/*                  (extensions to run command)              */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/



#ifndef _H_fuzzyutl
#define _H_fuzzyutl


#ifndef _H_factmngr
#include "factmngr.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _FUZZYUTL_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ANSI_COMPILER
   LOCALE VOID                 fcompliment( struct fuzzy_value *fv );
   LOCALE struct fuzzy_value  *funion( struct fuzzy_value *f1, struct fuzzy_value *f2 );
   LOCALE int                  nonintersectiontest( double *Ax, double *Ay, 
                                                    double *Bx, double *By,
                                                    int Asize, int Bsize );
   LOCALE VOID                 computeFuzzyConsequence( struct fact *new_fact );
   LOCALE VOID                 changeValueOfFuzzySlots( struct fact *fact1, struct fact *fact2 );
   LOCALE VOID                 PrintFuzzyTemplateFact(char *logName, struct fuzzy_value *fv
#if CERTAINTY_FACTORS
                                                     ,double CF
#endif
                                                     );
   LOCALE VOID                 PrintFuzzySet(char *logName, struct fuzzy_value *fv);
   LOCALE double               maxmin_intersect(struct fuzzy_value *f1,
                                                struct fuzzy_value *f2, 
                                                int DoIntersect,
                                                struct fuzzy_value **intersectSet);
   LOCALE struct fuzzy_value  *fintersect(struct fuzzy_value *f1, struct fuzzy_value *f2);
   LOCALE double               max_of_min(struct fuzzy_value *f1, struct fuzzy_value *f2);
   LOCALE int                  FZ_EQUAL(double f1, double f2);

#else
   LOCALE VOID                 fcompliment( );
   LOCALE struct fuzzy_value  *funion( );
   LOCALE int                  nonintersectiontest( );
   LOCALE VOID                 computeFuzzyConsequence( );
   LOCALE VOID                 changeValueOfFuzzySlots( );
   LOCALE VOID                 PrintFuzzyTemplateFact( );
   LOCALE VOID                 PrintFuzzySet( );
   LOCALE double               maxmin_intersect( );
   LOCALE struct fuzzy_value  *fintersect();
   LOCALE double               max_of_min();
   LOCALE int                  FZ_EQUAL();
   
#endif




#ifndef _FUZZYUTL_SOURCE_
   extern int                 saveFactsInProgress;
#endif


#endif

