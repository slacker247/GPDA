   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00 12/30/93             */
   /*                                                     */
   /*          EXTENDED MATH FUNCTIONS HEADER FILE        */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for numerous extended math     */
/*   functions including cos, sin, tan, sec, csc, cot, acos, */
/*   asin, atan, asec, acsc, acot, cosh, sinh, tanh, sech,   */
/*   csch, coth, acosh, asinh, atanh, asech, acsch, acoth,   */
/*   mod, exp, log, log10, sqrt, pi, deg-rad, rad-deg,       */
/*   deg-grad, grad-deg, **, and round.                      */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_emathfun

#define _H_emathfun

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _EMATHFUN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ANSI_COMPILER
   LOCALE VOID                           ExtendedMathFunctionDefinitions(void);
#if EX_MATH
   LOCALE double                         CosFunction(void);
   LOCALE double                         SinFunction(void);
   LOCALE double                         TanFunction(void);
   LOCALE double                         SecFunction(void);
   LOCALE double                         CscFunction(void);
   LOCALE double                         CotFunction(void);
   LOCALE double                         AcosFunction(void);
   LOCALE double                         AsinFunction(void);
   LOCALE double                         AtanFunction(void);
   LOCALE double                         AsecFunction(void);
   LOCALE double                         AcscFunction(void);
   LOCALE double                         AcotFunction(void);
   LOCALE double                         CoshFunction(void);
   LOCALE double                         SinhFunction(void);
   LOCALE double                         TanhFunction(void);
   LOCALE double                         SechFunction(void);
   LOCALE double                         CschFunction(void);
   LOCALE double                         CothFunction(void);
   LOCALE double                         AcoshFunction(void);
   LOCALE double                         AsinhFunction(void);
   LOCALE double                         AtanhFunction(void);
   LOCALE double                         AsechFunction(void);
   LOCALE double                         AcschFunction(void);
   LOCALE double                         AcothFunction(void);
   LOCALE long                           RoundFunction(void);
   LOCALE VOID                           ModFunction(DATA_OBJECT_PTR);
   LOCALE double                         ExpFunction(void);
   LOCALE double                         LogFunction(void);
   LOCALE double                         Log10Function(void);
   LOCALE double                         SqrtFunction(void);
   LOCALE double                         PiFunction(void);
   LOCALE double                         DegRadFunction(void);
   LOCALE double                         RadDegFunction(void);
   LOCALE double                         DegGradFunction(void);
   LOCALE double                         GradDegFunction(void);
   LOCALE double                         PowFunction(void);
#endif
#else   
   LOCALE VOID                           ExtendedMathFunctionDefinitions();
#if EX_MATH
   LOCALE double                         CosFunction();
   LOCALE double                         SinFunction();
   LOCALE double                         TanFunction();
   LOCALE double                         SecFunction();
   LOCALE double                         CscFunction();
   LOCALE double                         CotFunction();
   LOCALE double                         AcosFunction();
   LOCALE double                         AsinFunction();
   LOCALE double                         AtanFunction();
   LOCALE double                         AsecFunction();
   LOCALE double                         AcscFunction();
   LOCALE double                         AcotFunction();
   LOCALE double                         CoshFunction();
   LOCALE double                         SinhFunction();
   LOCALE double                         TanhFunction();
   LOCALE double                         SechFunction();
   LOCALE double                         CschFunction();
   LOCALE double                         CothFunction();
   LOCALE double                         AcoshFunction();
   LOCALE double                         AsinhFunction();
   LOCALE double                         AtanhFunction();
   LOCALE double                         AsechFunction();
   LOCALE double                         AcschFunction();
   LOCALE double                         AcothFunction();
   LOCALE long                           RoundFunction();
   LOCALE VOID                           ModFunction();
   LOCALE double                         ExpFunction();
   LOCALE double                         LogFunction();
   LOCALE double                         Log10Function();
   LOCALE double                         SqrtFunction();
   LOCALE double                         PiFunction();
   LOCALE double                         DegRadFunction();
   LOCALE double                         RadDegFunction();
   LOCALE double                         DegGradFunction();
   LOCALE double                         GradDegFunction();
   LOCALE double                         PowFunction();
#endif
#endif


#endif



