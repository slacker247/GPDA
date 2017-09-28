   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  12/06/93            */
   /*                                                     */
   /*              FACT FUNCTIONS HEADER FILE             */
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

#ifndef _H_factfun
#define _H_factfun

#ifndef _H_factmngr
#include "factmngr.h"
#endif
  
#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _FACTFUN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ANSI_COMPILER 
   LOCALE VOID                           FactFunctionDefinitions(void);
   LOCALE VOID                          *FactRelationFunction(void);
   LOCALE VOID                          *FactRelation(VOID *);
   LOCALE long int                       FactExistpFunction(void);
   LOCALE long int                       FactExistp(VOID *);
   LOCALE VOID                           FactSlotValueFunction(DATA_OBJECT *);
   LOCALE VOID                           FactSlotValue(VOID *,char *,DATA_OBJECT *);
   LOCALE VOID                           FactSlotNamesFunction(DATA_OBJECT *);
   LOCALE VOID                           FactSlotNames(VOID *,DATA_OBJECT *);
   LOCALE VOID                           GetFactListFunction(DATA_OBJECT *);
   LOCALE VOID                           GetFactList(DATA_OBJECT *,VOID *);
   LOCALE struct fact                   *GetFactAddressOrIndexArgument(char *,int,int);
#else  
   LOCALE VOID                           FactFunctionDefinitions();
   LOCALE VOID                          *FactRelationFunction();
   LOCALE VOID                          *FactRelation();
   LOCALE long int                       FactExistpFunction();
   LOCALE long int                       FactExistp();
   LOCALE VOID                           FactSlotValueFunction();
   LOCALE VOID                           FactSlotValue();
   LOCALE VOID                           FactSlotNamesFunction();
   LOCALE VOID                           FactSlotNames();
   LOCALE VOID                           GetFactListFunction();
   LOCALE VOID                           GetFactList();
   LOCALE struct fact                   *GetFactAddressOrIndexArgument();
#endif

#endif


