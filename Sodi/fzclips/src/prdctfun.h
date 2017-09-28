   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00 10/18/93             */
   /*                                                     */
   /*            PREDICATE FUNCTIONS HEADER FILE          */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_prdctfun

#define _H_prdctfun

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _PRDCTFUN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ANSI_COMPILER
   LOCALE VOID                           PredicateFunctionDefinitions(void);
   LOCALE BOOLEAN                        EqFunction(void);
   LOCALE BOOLEAN                        NeqFunction(void);
   LOCALE BOOLEAN                        StringpFunction(void);
   LOCALE BOOLEAN                        SymbolpFunction(void);
   LOCALE BOOLEAN                        LexemepFunction(void);
   LOCALE BOOLEAN                        NumberpFunction(void);
   LOCALE BOOLEAN                        FloatpFunction(void);
   LOCALE BOOLEAN                        IntegerpFunction(void);
   LOCALE BOOLEAN                        MultifieldpFunction(void);
   LOCALE BOOLEAN                        PointerpFunction(void);
   LOCALE BOOLEAN                        NotFunction(void);
   LOCALE BOOLEAN                        AndFunction(void);
   LOCALE BOOLEAN                        OrFunction(void);
   LOCALE BOOLEAN                        LessThanOrEqualFunction(void);
   LOCALE BOOLEAN                        GreaterThanOrEqualFunction(void);
   LOCALE BOOLEAN                        LessThanFunction(void);
   LOCALE BOOLEAN                        GreaterThanFunction(void);
   LOCALE BOOLEAN                        NumericEqualFunction(void);
   LOCALE BOOLEAN                        NumericNotEqualFunction(void);
   LOCALE BOOLEAN                        OddpFunction(void);
   LOCALE BOOLEAN                        EvenpFunction(void);
#else
   LOCALE VOID                           PredicateFunctionDefinitions();
   LOCALE BOOLEAN                        EqFunction();
   LOCALE BOOLEAN                        NeqFunction();
   LOCALE BOOLEAN                        StringpFunction();
   LOCALE BOOLEAN                        SymbolpFunction();
   LOCALE BOOLEAN                        LexemepFunction();
   LOCALE BOOLEAN                        NumberpFunction();
   LOCALE BOOLEAN                        FloatpFunction();
   LOCALE BOOLEAN                        IntegerpFunction();
   LOCALE BOOLEAN                        MultifieldpFunction();
   LOCALE BOOLEAN                        PointerpFunction();
   LOCALE BOOLEAN                        NotFunction();
   LOCALE BOOLEAN                        AndFunction();
   LOCALE BOOLEAN                        OrFunction();
   LOCALE BOOLEAN                        LessThanOrEqualFunction();
   LOCALE BOOLEAN                        GreaterThanOrEqualFunction();
   LOCALE BOOLEAN                        LessThanFunction();
   LOCALE BOOLEAN                        GreaterThanFunction();
   LOCALE BOOLEAN                        NumericEqualFunction();
   LOCALE BOOLEAN                        NumericNotEqualFunction();
   LOCALE BOOLEAN                        OddpFunction();
   LOCALE BOOLEAN                        EvenpFunction();
#endif


#endif



