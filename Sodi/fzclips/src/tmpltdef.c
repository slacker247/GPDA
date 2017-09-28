   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  02/11/94            */
   /*                                                     */
   /*                 DEFTEMPLATE MODULE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: Defines basic deftemplate primitive functions    */
/*   such as allocating and deallocating, traversing, and    */
/*   finding deftemplate data structures.                    */
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

#define _TMPLTDEF_SOURCE_

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT

#include <stdio.h>
#define _CLIPS_STDIO_

#include "clipsmem.h"
#include "exprnops.h"
#include "cstrccom.h"
#include "network.h"
#include "tmpltpsr.h"
#include "tmpltbsc.h"
#include "tmpltutl.h"   /* added 03-11-96 */
#include "tmpltfun.h"   /* added 03-11-96 */
#include "router.h"
#include "modulpsr.h"
#include "modulutl.h"
#include "cstrnchk.h"   /* added 03-11-96 */

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
#include "bload.h"
#include "tmpltbin.h"
#endif

#if CONSTRUCT_COMPILER && (! RUN_TIME)
#include "tmpltcmp.h"
#endif

#include "tmpltdef.h"

#if FUZZY_DEFTEMPLATES     /* added 03-11-96 */
#include "fuzzypsr.h"
#include "fuzzyutl.h"
#include "symbol.h"
#endif
  
/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if ANSI_COMPILER
   static VOID                   *AllocateModule(void);
   static VOID                    FreeModule(VOID *);
   static VOID                    ReturnDeftemplate(VOID *);
   static VOID                    InitializeDeftemplateModules(void);
   static VOID                    IncrementDeftemplateBusyCount(VOID *);
   static VOID                    DecrementDeftemplateBusyCount(VOID *);
#else
   static VOID                   *AllocateModule();
   static VOID                    FreeModule();
   static VOID                    ReturnDeftemplate();
   static VOID                    InitializeDeftemplateModules();
   static VOID                    IncrementDeftemplateBusyCount();
   static VOID                    DecrementDeftemplateBusyCount();
#endif

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   globle struct construct       *DeftemplateConstruct;
   globle int                     DeftemplateModuleIndex;
   globle struct entityRecord     DeftemplatePtrRecord = { DEFTEMPLATE_PTR,1,0,0,
                                                           NULL,
                                                           NULL,NULL, 
                                                           NULL,
                                                           NULL,
                                                           DecrementDeftemplateBusyCount,
                                                           IncrementDeftemplateBusyCount,
                                                           NULL,NULL,NULL,NULL };

/******************************************************************/
/* InitializeDeftemplates: Initializes the deftemplate construct. */
/******************************************************************/
globle VOID InitializeDeftemplates()
  {
   InitializeFacts();
   
   InitializeDeftemplateModules(); 

   DeftemplateBasicCommands();
   
   DeftemplateFunctions();

   DeftemplateConstruct = 
      AddConstruct("deftemplate","deftemplates",ParseDeftemplate,FindDeftemplate,
                   GetConstructNamePointer,GetConstructPPForm,
                   GetConstructModuleItem,GetNextDeftemplate,SetNextConstruct,
                   IsDeftemplateDeletable,Undeftemplate,ReturnDeftemplate);
                   
   InstallPrimitive((ENTITY_RECORD_PTR) &DeftemplatePtrRecord,DEFTEMPLATE_PTR);
  }

/*************************************************************/
/* InitializeDeftemplateModules: Initializes the deftemplate */
/*   construct for use with the defmodule construct.         */
/*************************************************************/
static VOID InitializeDeftemplateModules()
  {
   DeftemplateModuleIndex = RegisterModuleItem("deftemplate",
                                    AllocateModule,
                                    FreeModule,
#if BLOAD_AND_BSAVE || BLOAD || BLOAD_ONLY
                                    BloadDeftemplateModuleReference,
#else
                                    NULL,
#endif
#if CONSTRUCT_COMPILER && (! RUN_TIME)
                                    DeftemplateCModuleReference,
#else
                                    NULL,
#endif
                                    FindDeftemplate);
                                    
#if (! BLOAD_ONLY) && (! RUN_TIME) && DEFMODULE_CONSTRUCT
   AddPortConstructItem("deftemplate",SYMBOL);
#endif
  }

/***************************************************/
/* AllocateModule: Allocates a deftemplate module. */
/***************************************************/
static VOID *AllocateModule()
  { return((VOID *) get_struct(deftemplateModule)); }
  
/*************************************************/
/* FreeModule: Deallocates a deftemplate module. */ 
/*************************************************/
static VOID FreeModule(theItem)
  VOID *theItem;
  {
   FreeConstructHeaderModule(theItem,DeftemplateConstruct);
   rtn_struct(deftemplateModule,theItem);
  } 

/****************************************************************/
/* GetDeftemplateModuleItem: Returns a pointer to the defmodule */
/*  item for the specified deftemplate or defmodule.            */
/****************************************************************/
globle struct deftemplateModule *GetDeftemplateModuleItem(theModule)
  struct defmodule *theModule;
  { return((struct deftemplateModule *) GetConstructModuleItemByIndex(theModule,DeftemplateModuleIndex)); }
    
/***********************************************************/
/* FindDeftemplate: Searches for a deftemplate in the list */
/*   of deftemplates. Returns a pointer to the deftemplate */
/*   if found, otherwise NULL.                             */
/***********************************************************/
globle VOID *FindDeftemplate(deftemplateName)
  char *deftemplateName;
  { return(FindNamedConstruct(deftemplateName,DeftemplateConstruct)); }

/***********************************************************************/
/* GetNextDeftemplate: If passed a NULL pointer, returns the first     */
/*   deftemplate in the ListOfDeftemplates. Otherwise returns the next */
/*   deftemplate following the deftemplate passed as an argument.      */
/***********************************************************************/
globle VOID *GetNextDeftemplate(deftemplatePtr)
  VOID *deftemplatePtr;
  { return((VOID *) GetNextConstructItem(deftemplatePtr,DeftemplateModuleIndex)); }

/**********************************************************/
/* IsDeftemplateDeletable: Returns TRUE if a particular   */
/*   deftemplate can be deleted, otherwise returns FALSE. */
/**********************************************************/
globle BOOLEAN IsDeftemplateDeletable(vTheDeftemplate)
  VOID *vTheDeftemplate;
  {
#if (MAC_MPW || MAC_MCW) && (RUN_TIME || BLOAD_ONLY)/*added 3-11-96*/
#pragma unused(vTheDeftemplate)
#endif

#if BLOAD_ONLY || RUN_TIME
   return(FALSE);
#else
   struct deftemplate *theDeftemplate = (struct deftemplate *) vTheDeftemplate;
#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded()) return(FALSE);
#endif

   if (theDeftemplate->busyCount > 0) return(CLIPS_FALSE);
   if (theDeftemplate->patternNetwork != NULL) return(CLIPS_FALSE);
   
   return(CLIPS_TRUE);
#endif
  }
  
/**************************************************************/
/* ReturnDeftemplate: Returns the data structures associated  */
/*   with a deftemplate construct to the pool of free memory. */
/**************************************************************/
static VOID ReturnDeftemplate(vTheConstruct)
  VOID *vTheConstruct;
  {
#if (MAC_MPW || MAC_MCW) && (RUN_TIME || BLOAD_ONLY)/*added 3-11-96*/
#pragma unused(vTheConstruct)
#endif

#if (! BLOAD_ONLY) && (! RUN_TIME)
   struct deftemplate *theConstruct = (struct deftemplate *) vTheConstruct;
   struct templateSlot *slotPtr;
   
   if (theConstruct == NULL) return;
   
   /*====================================================================*/
   /* If a template is redefined, then we want to save its debug status. */
   /*====================================================================*/

#if DEBUGGING_FUNCTIONS
   DeletedTemplateDebugFlags = 0;
   if (theConstruct->watch) BitwiseSet(DeletedTemplateDebugFlags,0);
#endif

#if FUZZY_DEFTEMPLATES
   /*===========================================*/
   /* Free storage used by the fuzzy template.  */
   /*                                           */
   /* NOTE: no DeinstallDeftemplate - this is it*/
   /*===========================================*/
   
   if (theConstruct->fuzzyTemplate != NULL)
     {
       DeinstallFuzzyTemplate(theConstruct->fuzzyTemplate);
       /* for a fuzzy deftemplate the call to RemoveConstraint below
          for its single slot will decrement the busyCount for the
          deftemplate (restrictionList has ptr to this deftemplate)
          so it should be incremented or the count will go negative.
       */
       if ( theConstruct->slotList->constraints != NULL)
          theConstruct->busyCount++;
      }
#endif

   /*===========================================*/
   /* Free storage used by the templates slots. */
   /*===========================================*/
   
   slotPtr = theConstruct->slotList;
   while (slotPtr != NULL)
     {
      DecrementSymbolCount(slotPtr->slotName);
      RemoveHashedExpression(slotPtr->defaultList);
      slotPtr->defaultList = NULL;
      RemoveConstraint(slotPtr->constraints);
      slotPtr->constraints = NULL;
      slotPtr = slotPtr->next;
     }
     
   ReturnSlots(theConstruct->slotList);
   
   /*==================================*/
   /* Free storage used by the header. */
   /*==================================*/
   
   DeinstallConstructHeader(&theConstruct->header);
     
   rtn_struct(deftemplate,theConstruct);
#endif
  }

/***********************************************/
/* ReturnSlots: Returns the slot structures of */
/*   a deftemplate to free memory.             */
/***********************************************/
globle VOID ReturnSlots(slotPtr)
  struct templateSlot *slotPtr;
  {
#if (MAC_MPW || MAC_MCW) && (RUN_TIME || BLOAD_ONLY)
#pragma unused(slotPtr)
#endif

#if (! BLOAD_ONLY) && (! RUN_TIME)
   struct templateSlot *nextSlot;

   while (slotPtr != NULL)
     {
      nextSlot = slotPtr->next;
      ReturnExpression(slotPtr->defaultList);
      RemoveConstraint(slotPtr->constraints);
      rtn_struct(templateSlot,slotPtr);
      slotPtr = nextSlot;
     }
#endif
  }

/*************************************************/
/* DecrementDeftemplateBusyCount: Decrements the */
/*   busy count of a deftemplate data structure. */
/*************************************************/
static VOID DecrementDeftemplateBusyCount(vTheTemplate)
  VOID *vTheTemplate;
  {
   struct deftemplate *theTemplate = (struct deftemplate *) vTheTemplate;
   
   if (! ClearInProgress) theTemplate->busyCount--;
  }

/*************************************************/
/* IncrementDeftemplateBusyCount: Increments the */
/*   busy count of a deftemplate data structure. */
/*************************************************/
static VOID IncrementDeftemplateBusyCount(vTheTemplate)
  VOID *vTheTemplate;
  {
   struct deftemplate *theTemplate = (struct deftemplate *) vTheTemplate;
   
   theTemplate->busyCount++;
  } 

#endif /* DEFTEMPLATE_CONSTRUCT */


