   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  05/12/93            */
   /*                                                     */
   /*         FACT RETE PRINT FUNCTIONS HEADER FILE       */
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

#ifndef _H_factprt

#define _H_factprt

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _FACTPRT_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ANSI_COMPILER
   LOCALE VOID                           PrintFactJNCompVars1(char *,VOID *);
   LOCALE VOID                           PrintFactJNCompVars2(char *,VOID *);
   LOCALE VOID                           PrintFactPNCompVars1(char *,VOID *);
   LOCALE VOID                           PrintFactJNGetVar1(char *,VOID *);
   LOCALE VOID                           PrintFactJNGetVar2(char *,VOID *);
   LOCALE VOID                           PrintFactJNGetVar3(char *,VOID *);
   LOCALE VOID                           PrintFactPNGetVar1(char *,VOID *);
   LOCALE VOID                           PrintFactPNGetVar2(char *,VOID *);
   LOCALE VOID                           PrintFactPNGetVar3(char *,VOID *);
   LOCALE VOID                           PrintFactSlotLength(char *,VOID *);
   LOCALE VOID                           PrintFactPNConstant1(char *,VOID *);
   LOCALE VOID                           PrintFactPNConstant2(char *,VOID *);
#if FUZZY_DEFTEMPLATES
   LOCALE VOID                           PrintPNFUZZY_VALUE(char *,VOID *);
#endif
#else   
   LOCALE VOID                           PrintFactJNCompVars1();
   LOCALE VOID                           PrintFactJNCompVars2();
   LOCALE VOID                           PrintFactPNCompVars1();
   LOCALE VOID                           PrintFactJNGetVar1();
   LOCALE VOID                           PrintFactJNGetVar2();
   LOCALE VOID                           PrintFactJNGetVar3();
   LOCALE VOID                           PrintFactPNGetVar1();
   LOCALE VOID                           PrintFactPNGetVar2();
   LOCALE VOID                           PrintFactPNGetVar3();
   LOCALE VOID                           PrintFactSlotLength();
   LOCALE VOID                           PrintFactPNConstant1();
   LOCALE VOID                           PrintFactPNConstant2();
#if FUZZY_DEFTEMPLATES
   LOCALE VOID                           PrintPNFUZZY_VALUE();
#endif
#endif 

#endif






