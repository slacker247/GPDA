   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  05/12/93            */
   /*                                                     */
   /*             FUZZY RHS PARSE HEADER FILE             */
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



#ifndef _H_fuzzyrhs
#define _H_fuzzyrhs


#ifndef _H_tmpltdef
#include "tmpltdef.h"
#endif

#ifndef _H_fuzzymod
#include "fuzzymod.h"
#endif


#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _FUZZYRHS_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif


#if ANSI_COMPILER
   LOCALE struct expr        *ParseAssertFuzzyFact(char *readSource, struct token *tempToken,
                                              int *error, int endType, int constantsOnly,
                                              struct deftemplate *theDeftemplate,
                                              int variablesAllowed);
   LOCALE struct fuzzy_value *ParseLinguisticExpr(char *readSource,
                                              struct token *tempToken,
                                              struct fuzzyLv *lvp,
                                              int *error);
   LOCALE struct fuzzy_value *CopyFuzzyValue(struct fuzzy_value *fv);
   LOCALE VOID                CompactFuzzyValue(struct fuzzy_value *fv);
   LOCALE struct fuzzy_value *getConstantFuzzyValue(struct expr *top, int *error);
   LOCALE VOID                ModifyFuzzyValue(struct modifierListItem *mptr,
                                               struct fuzzy_value *elem);
   LOCALE double             *FgetArray ( int length );
   LOCALE VOID                FrtnArray ( double *p, int length );
   LOCALE int                *IgetArray ( int length );
   LOCALE VOID                IrtnArray ( int *p, int length );
   LOCALE struct expr        *tokenToFloatExpression(char *readSource,
                                                struct token *tempToken,
                                                int  *error,
                                                int constantsOnly);

#else
   LOCALE struct expr        *ParseAssertFuzzyFact();
   LOCALE struct fuzzy_value *ParseLinguisticExpr();
   LOCALE struct fuzzy_value *CopyFuzzyValue();
   LOCALE VOID                CompactFuzzyValue();
   LOCALE struct fuzzy_value *getConstantFuzzyValue();
   LOCALE VOID                ModifyFuzzyValue();
   LOCALE double             *FgetArray ();
   LOCALE VOID                FrtnArray ();
   LOCALE int                *IgetArray ();
   LOCALE VOID                IrtnArray ();
   LOCALE struct expr 		 *tokenToFloatExpression();

#endif


#endif

