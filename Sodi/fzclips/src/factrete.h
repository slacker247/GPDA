   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  05/12/93            */
   /*                                                     */
   /*        FACT RETE ACCESS FUNCTIONS HEADER FILE       */
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

#ifndef _H_factrete

#define _H_factrete

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _FACTRETE_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ANSI_COMPILER
   LOCALE BOOLEAN                        FactPNGetVar1(VOID *,DATA_OBJECT_PTR);
   LOCALE BOOLEAN                        FactPNGetVar2(VOID *,DATA_OBJECT_PTR);
   LOCALE BOOLEAN                        FactPNGetVar3(VOID *,DATA_OBJECT_PTR);
   LOCALE BOOLEAN                        FactJNGetVar1(VOID *,DATA_OBJECT_PTR);
   LOCALE BOOLEAN                        FactJNGetVar2(VOID *,DATA_OBJECT_PTR);
   LOCALE BOOLEAN                        FactJNGetVar3(VOID *,DATA_OBJECT_PTR);
   LOCALE BOOLEAN                        FactSlotLength(VOID *,DATA_OBJECT_PTR);
   LOCALE int                            FactJNCompVars1(VOID *,DATA_OBJECT_PTR);
   LOCALE int                            FactJNCompVars2(VOID *,DATA_OBJECT_PTR);
   LOCALE int                            FactPNCompVars1(VOID *,DATA_OBJECT_PTR);
   LOCALE BOOLEAN                        FactPNConstant1(VOID *,DATA_OBJECT_PTR);
   LOCALE BOOLEAN                        FactPNConstant2(VOID *,DATA_OBJECT_PTR);
   LOCALE int                            FactStoreMultifield(VOID *,DATA_OBJECT_PTR);
   LOCALE int                            AdjustFieldPosition(struct multifieldMarker *,int,int,int *);
#if FUZZY_DEFTEMPLATES
   LOCALE BOOLEAN                        FactFuzzyValuePNFunction(VOID *, DATA_OBJECT_PTR);
#endif   /* added 03-07-96 */

#else   
   LOCALE BOOLEAN                        FactPNGetVar1();
   LOCALE BOOLEAN                        FactPNGetVar2();
   LOCALE BOOLEAN                        FactPNGetVar3();
   LOCALE BOOLEAN                        FactJNGetVar1();
   LOCALE BOOLEAN                        FactJNGetVar2();
   LOCALE BOOLEAN                        FactJNGetVar3();
   LOCALE BOOLEAN                        FactSlotLength();
   LOCALE int                            FactJNCompVars1();
   LOCALE int                            FactJNCompVars2();
   LOCALE int                            FactPNCompVars1();
   LOCALE BOOLEAN                        FactPNConstant1();
   LOCALE BOOLEAN                        FactPNConstant2();
   LOCALE int                            FactStoreMultifield();
   LOCALE int                            AdjustFieldPosition();
#if FUZZY_DEFTEMPLATES
   LOCALE BOOLEAN                        FactFuzzyValuePNFunction();
#endif   /* added 03-07-96 */

#endif 

#endif






