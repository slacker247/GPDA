   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  02/11/94            */
   /*                                                     */
   /*          DEFTEMPLATE UTILITIES HEADER FILE          */
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

#ifndef _H_tmpltutl

#define _H_tmpltutl

#ifndef _H_expressn
#include "expressn.h"
#endif
#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_factmngr
#include "factmngr.h"
#endif
#ifndef _H_constrnt
#include "constrnt.h"
#endif
#ifndef _H_symbol
#include "symbol.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _TMPLTUTL_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ANSI_COMPILER
   LOCALE VOID                           InvalidDeftemplateSlotMessage(char *,char *);
   LOCALE VOID                           SingleFieldSlotCardinalityError(char *);
   LOCALE VOID                           MultiIntoSingleFieldSlotError(struct templateSlot *,struct deftemplate *);
   LOCALE VOID                           CheckTemplateFact(struct fact *);
   LOCALE BOOLEAN                        CheckRHSSlotTypes(struct expr *,struct templateSlot *,char *);
   LOCALE struct templateSlot           *GetNthSlot(struct deftemplate *,int);
   LOCALE int                            FindSlotPosition(struct deftemplate *,struct symbolHashNode *);
   LOCALE VOID                           PrintTemplateFact(char *,struct fact *);
   LOCALE VOID                           UpdateDeftemplateScope(void);
   LOCALE struct templateSlot           *FindSlot(struct deftemplate *,struct symbolHashNode *,int *);
   LOCALE struct deftemplate            *CreateImpliedDeftemplate(SYMBOL_HN *,int);
#else
   LOCALE VOID                           InvalidDeftemplateSlotMessage();
   LOCALE VOID                           SingleFieldSlotCardinalityError();
   LOCALE VOID                           MultiIntoSingleFieldSlotError();
   LOCALE VOID                           CheckTemplateFact();
   LOCALE BOOLEAN                        CheckRHSSlotTypes();
   LOCALE struct templateSlot           *GetNthSlot();
   LOCALE int                            FindSlotPosition();
   LOCALE VOID                           PrintTemplateFact();
   LOCALE VOID                           UpdateDeftemplateScope();
   LOCALE struct templateSlot           *FindSlot();
   LOCALE struct deftemplate            *CreateImpliedDeftemplate();
#endif

#endif





