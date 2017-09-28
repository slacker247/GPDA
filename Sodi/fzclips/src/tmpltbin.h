   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  05/12/93            */
   /*                                                     */
   /*         DEFTEMPLATE BSAVE/BLOAD HEADER FILE         */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Donnell                                     */
/*      Bob Orchard (NRCC - Nat'l Research Council of Canada)*/
/*                  (Fuzzy reasoning extensions)             */
/*                  (certainty factors for facts and rules)  */
/*                  (extensions to run command)              */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#if (! RUN_TIME)
#ifndef _H_tmpltbin

#define _H_tmpltbin

struct bsaveTemplateSlot
  {
   unsigned long slotName;
   unsigned int multislot : 1;
   unsigned int noDefault : 1;
   unsigned int defaultPresent : 1;
   unsigned int defaultDynamic : 1;
   long constraints;
   long defaultList;
   long next;
  };

struct bsaveDeftemplate;
struct bsaveDeftemplateModule;

#ifndef _H_cstrcbin
#include "cstrcbin.h"
#endif  

struct bsaveDeftemplate
  {
   struct bsaveConstructHeader header;
   long slotList;  
   unsigned int implied : 1;
   unsigned int numberOfSlots : 15; 
   long patternNetwork;  
#if FUZZY_DEFTEMPLATES
   unsigned int hasFuzzySlots : 1;
   long fuzzyTemplateList;  
#endif
};


#if FUZZY_DEFTEMPLATES

struct bsaveLvPlusUniverse
  {
    double from;
    double to;
    long   unitsName;
    long   ptPtr;
  };
  
struct bsaveFuzzyPrimaryTerm
  {
    long fuzzyValue;
    long next;
  };
  
#endif


#ifndef _H_modulbin
#include "modulbin.h"
#endif

struct bsaveDeftemplateModule
  {
   struct bsaveDefmoduleItemHeader header;
  }; 
  
#ifndef _H_tmpltdef
#include "tmpltdef.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _TMPLTBIN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ANSI_COMPILER   
   LOCALE VOID                           DeftemplateBinarySetup(void);
   LOCALE VOID                          *BloadDeftemplateModuleReference(int);
#else
   LOCALE VOID                           DeftemplateBinarySetup();
   LOCALE VOID                          *BloadDeftemplateModuleReference();
#endif

#ifndef _TMPLTBIN_SOURCE_
   extern struct deftemplate HUGE_ADDR  *DeftemplateArray;
#endif

#define DeftemplatePointer(i) ((struct deftemplate HUGE_ADDR *) (&DeftemplateArray[i]))


#endif
#endif







