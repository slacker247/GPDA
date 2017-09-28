   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  05/12/93            */
   /*                                                     */
   /*             FUZZY MODIFIER HEADER FILE              */
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



#ifndef _H_fuzzymod
#define _H_fuzzymod

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _FUZZYMOD_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif


#ifndef _H_setup
#include "setup.h"
#endif


#ifndef _H_fuzzyval
#include "fuzzyval.h"
#endif

#ifndef _H_dffnxfun
#include "dffnxfun.h"
#endif


#ifndef _H_extnfunc
#include "extnfunc.h"
#endif


/* structures for the list of modifier functions */

struct modifierListItem
  {
     char *name;   /* name of the modifier */
#if ANSI_COMPILER
     VOID (*modfunc)(struct fuzzy_value *fv);
#else
     VOID (*modfunc)();
#endif
     struct FunctionDefinition *modClipsfunc;
#if DEFFUNCTION_CONSTRUCT
     DEFFUNCTION *modDeffunc;
#endif      
     struct modifierListItem *next;
  };



/* routines globally accessible and defined in fuzzycom.c */

#if ANSI_COMPILER
     LOCALE VOID initFuzzyModifierList();
     LOCALE VOID executeModifyFunction(struct fuzzy_value *, struct modifierListItem *);
     LOCALE int  AddFuzzyModifier(char *, VOID (*)(struct fuzzy_value *),
                                  struct FunctionDefinition *
#if DEFFUNCTION_CONSTRUCT
                                  ,DEFFUNCTION *
#endif
                                 );
     LOCALE VOID RemoveFuzzyModifier(char *);
     LOCALE struct modifierListItem *FindModifier(char *mod_name);
#else
     LOCALE VOID initFuzzyModifierList();
     LOCALE VOID executeModifyFunction();
     LOCALE int  AddFuzzyModifier();
     LOCALE VOID RemoveFuzzyModifier();
     LOCALE struct modifierListItem *FindModifier();
#endif
 




#endif /* _H_fuzzymod */
