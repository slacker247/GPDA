   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  10/13/93            */
   /*                                                     */
   /*             LOGICAL DEPENDENCIES MODULE             */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provide support routines for managing truth      */
/*   maintenance using the logical conditional element.      */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#define _LGCLDPND_SOURCE_

#include <stdio.h>
#define _CLIPS_STDIO_

#include "setup.h"

#if DEFRULE_CONSTRUCT && LOGICAL_DEPENDENCIES

#include "clipsmem.h"
#include "router.h"
#include "evaluatn.h"
#include "engine.h"
#include "reteutil.h"
#include "pattern.h"
#include "argacces.h"
#include "factmngr.h"

#if OBJECT_SYSTEM
#include "insfun.h"
#endif

#include "lgcldpnd.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if ANSI_COMPILER
   static struct partialMatch    *FindLogicalBind(struct joinNode *,struct partialMatch *);
   static int                     FindEntityInPartialMatch(struct patternEntity *,struct partialMatch *);
   static struct dependency      *DetachAssociatedDependencies(struct dependency *,VOID *);
#if DEBUGGING_FUNCTIONS
   static VOID                   *GetFactOrInstanceArgument(DATA_OBJECT *,char *);
#endif
#else
   static struct partialMatch    *FindLogicalBind();
   static int                     FindEntityInPartialMatch();
   static struct dependency      *DetachAssociatedDependencies();
#if DEBUGGING_FUNCTIONS
   static VOID                   *GetFactOrInstanceArgument();
#endif
#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static struct dependency     *UnsupportedDataEntities = NULL; /* 03-07-96 */

/***********************************************************************/
/* AddLogicalDependencies: Adds the logical dependency links between a */
/*   data entity (such as a fact or instance) and the partial match    */
/*   which logically supports that data entity. If a data entity is    */
/*   unconditionally asserted (i.e. the global variable TheLogicalJoin */
/*   is NULL), then existing logical support for the data entity is no */
/*   longer needed and it is removed. If a data entity is already      */
/*   unconditionally supported and that data entity is conditionally   */
/*   asserted (i.e. the global variable TheLogicalJoin is not NULL),   */
/*   then the logical support is ignored. Otherwise, the partial match */
/*   is linked to the data entity and the data entity is linked to the */
/*   partial match. Note that the word assert is used to refer to      */
/*   creating a fact with the assert command and creating an instance  */
/*   with the make-instance command.                                   */
/***********************************************************************/
globle BOOLEAN AddLogicalDependencies(theEntity,existingEntity) 
  struct patternEntity *theEntity;
  int existingEntity;
  {
   struct partialMatch *theBinds;
   struct dependency *newDependency;

   /*==============================================*/
   /* If the rule has no logical patterns, then no */
   /* dependencies have to be established.         */
   /*==============================================*/

   if (TheLogicalJoin == NULL)
     {
      if (existingEntity) RemoveEntityDependencies(theEntity);
      return(CLIPS_TRUE);
     }
   else if (existingEntity && (theEntity->dependents == NULL))
     { return(CLIPS_TRUE); }

   /*============================================================*/
   /* Find the partial match in the logical join associated with */
   /* activation partial match. If the partial match cannot be   */
   /* found, then the partial match must have been deleted by a  */
   /* previous RHS action and the dependency link should not be  */
   /* added.                                                     */
   /*============================================================*/

   theBinds = FindLogicalBind(TheLogicalJoin,GlobalLHSBinds);
   if (theBinds == NULL) return(CLIPS_FALSE);

   /*==============================================================*/
   /* Add a dependency link between the partial match and the data */
   /* entity. The dependency links are stored in the partial match */
   /* behind the data entities stored in the partial match and the */
   /* activation link, if any.                                     */
   /*==============================================================*/

   newDependency = get_struct(dependency);
   newDependency->dPtr = (VOID *) theEntity;
   newDependency->next = (struct dependency *) 
                         theBinds->binds[theBinds->bcount + theBinds->activationf].gm.theValue;
   theBinds->binds[theBinds->bcount + theBinds->activationf].gm.theValue = (VOID *) newDependency;
   
   /*================================================================*/
   /* Add a dependency link between the entity and the partialMatch. */
   /*================================================================*/
   
   newDependency = get_struct(dependency);
   newDependency->dPtr = (VOID *) theBinds;
   newDependency->next = (struct dependency *) theEntity->dependents;
   theEntity->dependents = (VOID *) newDependency;

   /*==================================================================*/
   /* Return true to indicate that the data entity should be asserted. */
   /*==================================================================*/

   return(TRUE);
  }

/************************************************************************/
/* FindLogicalBind: Finds the partial match associated with the logical */
/*   CE which will provide logical support for a data entity asserted   */
/*   from the currently executing rule. The function is called when     */
/*   creating logical support links between the data entity and         */
/*   supporting partial matches. It compares each partial match found   */
/*   at a specified join to the partial match associated with a rule    */
/*   activation until it finds the partial match that generated the     */
/*   rule activation.                                                   */
/************************************************************************/
static struct partialMatch *FindLogicalBind(theJoin,theBinds) 
  struct joinNode *theJoin;
  struct partialMatch *theBinds;
  {
   struct partialMatch *compPtr;
   unsigned int i;
   int found;

   /*==================================*/
   /* Loop through each of the partial */
   /* matches in the beta memory.      */
   /*==================================*/
   
   for (compPtr = theJoin->beta;
        compPtr != NULL;
        compPtr = compPtr->next)
     {
      /*==================================================*/
      /* Compare each of the data entities in the partial */
      /* match being examined and the partial match used  */
      /* in the dependency link.                          */
      /*==================================================*/
      
      found = CLIPS_TRUE;

      for (i = 0; i < compPtr->bcount; i++)
        {
         if (compPtr->binds[i].gm.theMatch != theBinds->binds[i].gm.theMatch) 
           {
            found = CLIPS_FALSE;
            break;
           }
        }

      /*========================================================*/
      /* If all of the data entities in the partial match are   */
      /* identical to the partial match in the dependency link, */
      /* then this is the partial match we're looking for.      */
      /*========================================================*/
                                  
      if (found) return(compPtr);
     }

   /*========================================*/
   /* The partial match corresponding to the */
   /* logical dependency couldn't be found.  */
   /*========================================*/
   
   return(NULL);
  }

/*********************************************************************/
/* RemoveEntityDependencies: Removes all logical support links from  */
/*   a pattern entity that point to partial matches or other pattern */
/*   entities. Also removes the associated links from the partial    */
/*   matches or pattern entities which point back to the pattern     */
/*   entities.                                                       */
/*********************************************************************/
globle VOID RemoveEntityDependencies(theEntity) 
  struct patternEntity *theEntity;
  {
   struct dependency *fdPtr, *nextPtr, *theList;
   struct partialMatch *theBinds;

   /*===============================*/
   /* Get the list of dependencies. */
   /*===============================*/
   
   fdPtr = (struct dependency *) theEntity->dependents;

   /*========================================*/
   /* Loop through each of the dependencies. */
   /*========================================*/
   
   while (fdPtr != NULL)
     {
      /*===============================*/
      /* Remember the next dependency. */
      /*===============================*/
      
      nextPtr = fdPtr->next;

      /*================================================================*/
      /* Remove the link between the data entity and the partial match. */
      /*================================================================*/
      
      theBinds = (struct partialMatch *) fdPtr->dPtr;
      theList = (struct dependency *) 
                theBinds->binds[theBinds->bcount + theBinds->activationf].gm.theValue;
      theList = DetachAssociatedDependencies(theList,(VOID *) theEntity);
      theBinds->binds[theBinds->bcount + theBinds->activationf].gm.theValue = (VOID *) theList;

      /*========================*/
      /* Return the dependency. */
      /*========================*/
      
      rtn_struct(dependency,fdPtr);
      
      /*=================================*/
      /* Move on to the next dependency. */
      /*=================================*/
      
      fdPtr = nextPtr;
     }

   /*=====================================================*/
   /* Set the dependency list of the data entity to NULL. */
   /*=====================================================*/
   
   theEntity->dependents = NULL;
  }

/*******************************************************************/
/* DetachAssociatedDependencies: Removes all logical support links */
/*   which pointer to a pattern entity from a list of dependencies */
/*   (which may be associated with either a partial match or       */
/*   another pattern entity). Does not remove links which point in */
/*   the other direction.                                          */
/*******************************************************************/
static struct dependency *DetachAssociatedDependencies(theList,theEntity) 
  struct dependency *theList;
  VOID *theEntity;
  {
   struct dependency *fdPtr, *nextPtr, *lastPtr = NULL;

   fdPtr = theList;

   while (fdPtr != NULL)
     {
      if (fdPtr->dPtr == theEntity)
        {
         nextPtr = fdPtr->next;
         if (lastPtr == NULL) theList = nextPtr;
         else lastPtr->next = nextPtr;
         rtn_struct(dependency,fdPtr);
         fdPtr = nextPtr;
        }
      else
        {
         lastPtr = fdPtr;
         fdPtr = fdPtr->next;
        }
     }
     
   return(theList);
  }
  
/**************************************************************************/
/* RemovePMDependencies: Removes all logical support links from a partial */
/*   match that point to any data entities. Also removes the associated   */
/*   links from the data entities which point back to the partial match.  */
/**************************************************************************/
globle VOID RemovePMDependencies(theBinds) 
  struct partialMatch *theBinds;
  {
   struct dependency *fdPtr, *nextPtr, *theList;
   struct patternEntity *theEntity;

   fdPtr = (struct dependency *) theBinds->binds[theBinds->bcount + theBinds->activationf].gm.theValue;

   while (fdPtr != NULL)
     {
      nextPtr = fdPtr->next;

      theEntity = (struct patternEntity *) fdPtr->dPtr;

      theList = (struct dependency *) theEntity->dependents;
      theList = DetachAssociatedDependencies(theList,(VOID *) theBinds);
      theEntity->dependents = (VOID *) theList;
      
      rtn_struct(dependency,fdPtr);
      fdPtr = nextPtr;
     }

   theBinds->binds[theBinds->bcount + theBinds->activationf].gm.theValue = NULL;
  }

/************************************************************************/
/* RemoveLogicalSupport: Removes the dependency links between a partial */
/*   match and the data entities it logically supports. Also removes    */
/*   the associated links from the data entities which point back to    */
/*   the partial match by calling DetachAssociatedEntityDependencies.   */
/*   If an entity has all of its logical support removed as a result of */
/*   this procedure, the dependency link from the partial match is      */
/*   added to the list of unsupported data entities so that the entity  */
/*   will be deleted as a result of losing its logical support.         */
/************************************************************************/
globle VOID RemoveLogicalSupport(theBinds)
  struct partialMatch *theBinds;
  {
   struct dependency *dlPtr, *tempPtr, *theList;
   struct patternEntity *theEntity;

   /*========================================*/
   /* If the partial match has no associated */
   /* dependencies, then return.             */
   /*========================================*/
   
   if (theBinds->dependentsf == CLIPS_FALSE) return;
   
   /*=======================================*/
   /* Loop through each of the dependencies */
   /* attached to the partial match.        */
   /*=======================================*/
   
   dlPtr = (struct dependency *) theBinds->binds[theBinds->bcount + theBinds->activationf].gm.theValue;

   while (dlPtr != NULL)
     {
      /*===============================*/
      /* Remember the next dependency. */
      /*===============================*/
      
      tempPtr = dlPtr->next;

      /*==========================================================*/
      /* Determine the data entity associated with the dependency */
      /* structure and delete its dependency references to this   */
      /* partial match.                                           */
      /*==========================================================*/
      
      theEntity = (struct patternEntity *) dlPtr->dPtr;
      
      theList = (struct dependency *) theEntity->dependents;
      theList = DetachAssociatedDependencies(theList,(VOID *) theBinds);
      theEntity->dependents = (VOID *) theList;
      
      /*==============================================================*/
      /* If the data entity has lost all of its logical support, then */
      /* add the dependency structure from the partial match to the   */
      /* list of unsupported data entities to be deleted. Otherwise,  */
      /* just delete the dependency structure.                        */
      /*==============================================================*/
      
      if (theEntity->dependents == NULL)
        {
 	 (*theEntity->theInfo->base.incrementBusyCount)(theEntity);  /* bug fixed 06-07-96 */
         dlPtr->next = UnsupportedDataEntities;   /* changed 03-07-96 */
         UnsupportedDataEntities = dlPtr;
        }
      else
        { rtn_struct(dependency,dlPtr); }

      /*==================================*/
      /* Move on to the next dependency.  */
      /*==================================*/
      
      dlPtr = tempPtr;
     }

   /*=====================================*/
   /* The partial match no longer has any */
   /* dependencies associated with it.    */
   /*=====================================*/
   
   theBinds->binds[theBinds->bcount + theBinds->activationf].gm.theValue = NULL;
  }
  
/********************************************************************/
/* ForceLogicalRetractions: Deletes the data entities found on the  */
/*   list of items that have lost their logical support. The delete */
/*   function associated with each data entity is called to delete  */
/*   that data entity. Calling the delete function may in turn      */
/*   add more data entities to the list of data entities which have */
/*   lost their logical support.                                    */
/********************************************************************/
globle VOID ForceLogicalRetractions()
  {
   struct dependency *tempPtr;
   struct patternEntity *theEntity;
   static int alreadyEntered = CLIPS_FALSE;

   /*===================================================*/
   /* Don't reenter this function once it's called. Any */
   /* new additions to the list of items to be deleted  */
   /* as a result of losing their logical support will  */
   /* be handled properly.                              */
   /*===================================================*/
   
   if (alreadyEntered) return;
   alreadyEntered = CLIPS_TRUE;
   
   /*=======================================================*/
   /* Continue to delete the first item on the list as long */
   /* as one exists. This is done because new items may be  */
   /* placed at the beginning of the list as other data     */
   /* entities are deleted.                                 */
   /*=======================================================*/
   
   while (UnsupportedDataEntities != NULL)   /* changed 03-07-96 */
     {
      /*==========================================*/
      /* Determine the data entity to be deleted. */
      /*==========================================*/
      
      theEntity = (struct patternEntity *) UnsupportedDataEntities->dPtr;

      /*================================================*/
      /* Remove the dependency structure from the list. */
      /*================================================*/
      
      tempPtr = UnsupportedDataEntities;   /* changed 03-07-96 */
      UnsupportedDataEntities = UnsupportedDataEntities->next;
      rtn_struct(dependency,tempPtr);
      
      /*=========================*/
      /* Delete the data entity. */
      /*=========================*/
      
      (*theEntity->theInfo->base.decrementBusyCount)(theEntity);
      (*theEntity->theInfo->base.deleteFunction)(theEntity);
     }
     
   /*============================================*/
   /* Deletion of items on the list is complete. */
   /*============================================*/
   
   alreadyEntered = CLIPS_FALSE;
  }

/****************************************************************/
/* Dependencies: C access routine for the dependencies command. */
/****************************************************************/
globle VOID Dependencies(theEntity)
  struct patternEntity *theEntity;
  {
   struct dependency *fdPtr;
   
   /*=========================================*/
   /* If the data entity has no dependencies, */
   /* then print "None" and return.           */
   /*=========================================*/
   
   if (theEntity->dependents == NULL)
     {
      PrintCLIPS(WDISPLAY,"None\n");
      return;
     }

   /*============================================*/
   /* Loop through the list of the data entities */
   /* dependencies and print them.               */
   /*============================================*/
   
   for (fdPtr = (struct dependency *) theEntity->dependents;
        fdPtr != NULL;
        fdPtr = fdPtr->next)
     {
      PrintPartialMatch(WDISPLAY,(struct partialMatch *) fdPtr->dPtr);
      PrintCLIPS(WDISPLAY,"\n");
     }
  }

/************************************************************/
/* Dependents: C access routine for the dependents command. */
/************************************************************/
globle VOID Dependents(theEntity)   /* changed 03-07-96 */
  struct patternEntity *theEntity;
  {
   struct patternEntity *entityPtr = NULL;
   struct patternParser *theParser = NULL;
   struct dependency *fdPtr;
   struct partialMatch *theBinds;
   int found = CLIPS_FALSE;

   /*=================================*/
   /* Loop through every data entity. */
   /*=================================*/
   
   for (GetNextPatternEntity(&theParser,&entityPtr);
        entityPtr != NULL;
        GetNextPatternEntity(&theParser,&entityPtr))
     {
      /*====================================*/
      /* Loop through every dependency link */
      /* associated with the data entity.   */
      /*====================================*/
      
      for (fdPtr = (struct dependency *) entityPtr->dependents;
           fdPtr != NULL;
           fdPtr = fdPtr->next)
        {
         /*=====================================================*/
         /* If the data entity which was the argument passed to */
         /* the dependents command is contained in one of the   */
         /* partial matches of the data entity currently being  */
         /* examined, then the data entity being examined is a  */
         /* dependent. Print the data entity and then move on   */
         /* to the next data entity.                            */
         /*=====================================================*/
         
         theBinds = (struct partialMatch *) fdPtr->dPtr;
         if (FindEntityInPartialMatch(theEntity,theBinds) == CLIPS_TRUE)
           {
            if (found) PrintCLIPS(WDISPLAY,",");
            (*entityPtr->theInfo->base.shortPrintFunction)(WDISPLAY,entityPtr);
            found = CLIPS_TRUE;
            break;
           }
        }
     }
     
   /*=================================================*/
   /* If no dependents were found, then print "None." */
   /* Otherwise print a carriage return after the     */
   /* list of dependents.                             */
   /*=================================================*/

   if (! found) PrintCLIPS(WDISPLAY,"None\n");
   else PrintCLIPS(WDISPLAY,"\n");
  }
  
/******************************************************/
/* FindEntityInPartialMatch: Searches for a specified */
/*   data entity in a partial match.                  */
/******************************************************/
static int FindEntityInPartialMatch(theEntity,thePartialMatch)
  struct patternEntity *theEntity;
  struct partialMatch *thePartialMatch;
  {
   short int i;

   for (i = 0 ; i < (int) thePartialMatch->bcount; i++)
     {
      if (thePartialMatch->binds[i].gm.theMatch->matchingItem == theEntity)
        { return(CLIPS_TRUE); }
     }

   return(CLIPS_FALSE);
  }

#if DEBUGGING_FUNCTIONS

/*********************************************/
/* DependenciesCommand: CLIPS access routine */
/*   for the dependencies command.           */
/*********************************************/
globle VOID DependenciesCommand()
  {
   DATA_OBJECT item;
   VOID *ptr;

   if (ArgCountCheck("dependencies",EXACTLY,1) == -1) return;

   ptr = GetFactOrInstanceArgument(&item,"dependencies");
   
   if (ptr == NULL) return;
   
#if DEFRULE_CONSTRUCT
   Dependencies((struct patternEntity *) ptr);   /* changed 03-07-96 */
#else
   PrintCLIPS(WDISPLAY,"None\n");
#endif
  }
        
/*******************************************/
/* DependentsCommand: CLIPS access routine */
/*   for the dependents command.           */
/*******************************************/
globle VOID DependentsCommand()
  {
   DATA_OBJECT item;
   VOID *ptr;

   if (ArgCountCheck("dependents",EXACTLY,1) == -1) return;

   ptr = GetFactOrInstanceArgument(&item,"dependents");
   
   if (ptr == NULL) return;
   
#if DEFRULE_CONSTRUCT
   Dependents((struct patternEntity *) ptr);  /* changed 03-07-96 */
#else
   PrintCLIPS(WDISPLAY,"None\n");
#endif
  }
        
/**************************************************************/
/* GetFactOrInstanceArgument: Utility routine for retrieving  */
/*   an argument suitable for the dependents and dependencies */
/*   commands.                                                */
/**************************************************************/
static VOID *GetFactOrInstanceArgument(item,functionName)
  DATA_OBJECT *item;
  char *functionName;
  {
   VOID *ptr;

   /*==============================*/
   /* Retrieve the first argument. */
   /*==============================*/
   
   RtnUnknown(1,item);

   /*==================================================*/
   /* Fact and instance addresses are valid arguments. */
   /*==================================================*/
   
   if ((GetpType(item) == FACT_ADDRESS) ||
       (GetpType(item) == INSTANCE_ADDRESS))
     { return(GetpValue(item)); }
     
   /*==================================================*/
   /* An integer is a valid argument if it corresponds */
   /* to the fact index of an existing fact.           */
   /*==================================================*/
   
#if DEFTEMPLATE_CONSTRUCT
   else if (GetpType(item) == INTEGER)
     {
      if ((ptr = (VOID *) FindIndexedFact(DOPToLong(item))) == NULL)
        {
         char tempBuffer[20];
         sprintf(tempBuffer,"f-%ld",DOPToLong(item));
         CantFindItemErrorMessage("fact",tempBuffer);
        }
      return(ptr);
     }
#endif

   /*================================================*/
   /* Instance names and symbols are valid arguments */
   /* if they correspond to an existing instance.    */
   /*================================================*/
   
#if OBJECT_SYSTEM
   else if ((GetpType(item) == INSTANCE_NAME) || (GetpType(item) == SYMBOL))
     {
      if ((ptr = (VOID *) FindInstanceBySymbol((SYMBOL_HN *) GetpValue(item))) == NULL)
        {
         CantFindItemErrorMessage("instance",ValueToString(GetpValue(item)));
        }
      return(ptr);
     }
#endif

   /*========================================*/
   /* Any other type is an invalid argument. */
   /*========================================*/
   
   ExpectedTypeError2(functionName,1);
   return(NULL);
  }

#endif /* DEBUGGING_FUNCTIONS */

#endif /* DEFRULE_CONSTRUCT && LOGICAL_DEPENDENCIES */

