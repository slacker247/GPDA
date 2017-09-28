   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  05/12/93            */
   /*                                                     */
   /*           MULTIFIELD FUNCTIONS HEADER FILE          */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary Riley and Brian Donnell                         */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_multifun
#define _H_multifun

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _MULTIFUN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ANSI_COMPILER
#if ! RUN_TIME
   LOCALE VOID                    MultifieldFunctionDefinitions(void);
#endif
#if MULTIFIELD_FUNCTIONS
   LOCALE VOID                    DeleteFunction(DATA_OBJECT_PTR);
   LOCALE VOID                    MVDeleteFunction(DATA_OBJECT_PTR);
   LOCALE VOID                    ReplaceFunction(DATA_OBJECT_PTR);
   LOCALE VOID                    MVReplaceFunction(DATA_OBJECT_PTR);
   LOCALE VOID                    InsertFunction(DATA_OBJECT_PTR);
   LOCALE VOID                    ExplodeFunction(DATA_OBJECT_PTR);
   LOCALE VOID                   *ImplodeFunction(void);
   LOCALE VOID                    SubseqFunction(DATA_OBJECT_PTR);
   LOCALE VOID                    MVSubseqFunction(DATA_OBJECT_PTR);
   LOCALE VOID                    FirstFunction(DATA_OBJECT_PTR);
   LOCALE VOID                    RestFunction(DATA_OBJECT_PTR);
   LOCALE VOID                    NthFunction(DATA_OBJECT_PTR);
   LOCALE BOOLEAN                 SubsetpFunction(void);
   LOCALE VOID                    MemberFunction(DATA_OBJECT_PTR);
   LOCALE VOID                    MultifieldPrognFunction(DATA_OBJECT_PTR);
   LOCALE VOID                    GetMvPrognField(DATA_OBJECT_PTR);
   LOCALE long                    GetMvPrognIndex(void);
#endif
   LOCALE int                     ReplaceMultiValueField(struct dataObject *,
                                                         struct dataObject *,
                                                         long,long,struct dataObject *,char *);
   LOCALE int                     InsertMultiValueField(struct dataObject *,
                                                        struct dataObject *,
                                                        long,struct dataObject *,char *);
   LOCALE int                     DeleteMultiValueField(struct dataObject *,struct dataObject *,
                                                        long,long,char *);

#else
#if ! RUN_TIME
   LOCALE VOID                    MultifieldFunctionDefinitions();
#endif
#if MULTIFIELD_FUNCTIONS
   LOCALE VOID                    DeleteFunction();
   LOCALE VOID                    MVDeleteFunction();
   LOCALE VOID                    ReplaceFunction();
   LOCALE VOID                    MVReplaceFunction();
   LOCALE VOID                    InsertFunction();
   LOCALE VOID                    ExplodeFunction();
   LOCALE VOID                   *ImplodeFunction();
   LOCALE VOID                    SubseqFunction();
   LOCALE VOID                    MVSubseqFunction();
   LOCALE VOID                    FirstFunction();
   LOCALE VOID                    RestFunction();
   LOCALE VOID                    NthFunction();
   LOCALE BOOLEAN                 SubsetpFunction();
   LOCALE VOID                    MemberFunction();
   LOCALE VOID                    MultifieldPrognFunction();
   LOCALE VOID                    GetMvPrognField();
   LOCALE long                    GetMvPrognIndex();
#endif
   LOCALE int                     ReplaceMultiValueField();
   LOCALE int                     InsertMultiValueField();
   LOCALE int                     DeleteMultiValueField();
#endif

#endif





