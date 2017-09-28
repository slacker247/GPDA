   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  03/18/94            */
   /*                                                     */
   /*        SYMBOL CONSTRUCT COMPILER HEADER FILE        */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the constructs-to-c feature for       */
/*   atomic data values: symbols, integers, floats, and      */
/*   bit maps.                                               */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_symblcmp
#define _H_symblcmp

#ifndef _CLIPS_STDIO_
#define _CLIPS_STDIO_
#include <stdio.h>
#endif

#ifndef _H_symbol
#include "symbol.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _SYMBLCMP_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif


               
#if ANSI_COMPILER
   LOCALE VOID                     PrintSymbolReference(FILE *,SYMBOL_HN *);
   LOCALE VOID                     PrintFloatReference(FILE *,FLOAT_HN *);
   LOCALE VOID                     PrintIntegerReference(FILE *,INTEGER_HN *);
   LOCALE VOID                     PrintBitMapReference(FILE *,BITMAP_HN *);
#if FUZZY_DEFTEMPLATES     /* added 03-12-96 */
   LOCALE VOID                     PrintFuzzyValueReference(FILE *,FUZZY_VALUE_HN *);
#endif
   LOCALE VOID                     AtomicValuesToCode(char *);
#else
   LOCALE VOID                     PrintSymbolReference();
   LOCALE VOID                     PrintFloatReference();
   LOCALE VOID                     PrintIntegerReference();
   LOCALE VOID                     PrintBitMapReference();
#if FUZZY_DEFTEMPLATES     /* added 03-12-96 */
   LOCALE VOID                     PrintFuzzyValueReference();
#endif
   LOCALE VOID                     AtomicValuesToCode();
#endif

#endif







