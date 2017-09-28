   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  10/12/93            */
   /*                                                     */
   /*                    ENGINE MODULE                    */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides functionality primarily associated with */
/*   the run and focus commands.                             */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Bebe Ly                                              */
/*      Brian L. Donnell                                     */
/*      Bob Orchard (NRCC - Nat'l Research Council of Canada)*/
/*                  (Fuzzy reasoning extensions)             */
/*                  (certainty factors for facts and rules)  */
/*                  (extensions to run command)              */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#define _ENGINE_SOURCE_

#include <stdio.h>
#define _CLIPS_STDIO_
#include <string.h>

#include "setup.h"

#if DEFRULE_CONSTRUCT

#include "constant.h"
#include "clipsmem.h"
#include "watch.h"
#include "router.h"
#include "argacces.h"
#include "reteutil.h"
#include "utility.h"
#include "retract.h"
#include "ruledlt.h"
#include "prccode.h"
#include "prcdrfun.h"
#include "inscom.h"
#include "factmngr.h" 
#include "sysdep.h"
#include "agenda.h"
#include "modulutl.h"     /* added 03-06-96 */

#include "engine.h"

#if CERTAINTY_FACTORS     /* added 03-06-96 */
#include "cfdef.h"
#endif

#if EXTENDED_RUN_OPTIONS  /* added 03-06-96 */
#include "fuzzyutl.h"
#endif
        
/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if ANSI_COMPILER
   static struct activation      *NextActivationToFire(void);
   static struct defmodule       *RemoveFocus(struct defmodule *);
#else
   static struct activation      *NextActivationToFire();
   static struct defmodule       *RemoveFocus();
#endif

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   globle struct defrule      *ExecutingRule = NULL;
   globle BOOLEAN              HaltRules = CLIPS_FALSE;
#if LOGICAL_DEPENDENCIES
   globle struct joinNode     *TheLogicalJoin = NULL;
#endif

#if CERTAINTY_FACTORS  /* added 03-06-96 */
/* need to have access to concludingCF for the activation so correct
   CF can be calulated for any asserted facts -- NOTE: 2 types
   of concluding CFs calculated - 1 for use with FUZZY asserts and for
   use with CRISP asserts when no FUZZY patterns on LHS -  other for
   CRISP asserts when FUZZY patterns on LHS
   These values are stored on the current activation so we need to
   make this available to changeCFofNewFact in cfdef.c.
*/
   globle ACTIVATION           *theCurrentActivation = NULL;
#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/
   
   static struct callFunctionItem   
                              *ListOfRunFunctions = NULL;
#if EXTENDED_RUN_OPTIONS /* added 03-06-96 */
   static struct callFunctionItem 
                             *ListOfRunStartFunctions = NULL;
   static struct callFunctionItem  
                             *ListOfRunStopFunctions = NULL;
#endif
   static struct focus        *CurrentFocus = NULL;
   static int                  FocusChanged = CLIPS_FALSE;
#if DEBUGGING_FUNCTIONS
   static int                  WatchStatistics = OFF;
   static int                  WatchFocus = OFF;
#endif

#if EXTENDED_RUN_OPTIONS /* added 03-06-96 */
/**************************************************************/
/*  NOTE: Run                                                 */
/*   *** modified at NRC to allow (run -2) to keep            */
/*       cycling even though the agenda is empty              */
/*       This is very useful when external events             */
/*       may cause facts to be added to the system            */
/*       which in turn may cause rules to be fired!           */
/**************************************************************/
#endif
  
/**********************************************/
/* Run: C access routine for the run command. */
/**********************************************/
globle long int Run(runLimit)
  long int runLimit;
  {
   long int rulesFired = 0;
   DATA_OBJECT result;
   struct callFunctionItem *theRunFunction;
#if DEBUGGING_FUNCTIONS
   long int maxActivations = 0, sumActivations = 0;
#if DEFTEMPLATE_CONSTRUCT
   long int maxFacts = 0, sumFacts = 0;
#endif
#if OBJECT_SYSTEM
   long int maxInstances = 0, sumInstances = 0;
#endif
   double endTime, startTime = 0.0;
   long int tempValue;
#endif
   unsigned int i;
   static int alreadyRunning = CLIPS_FALSE;
   struct patternEntity *theMatchingItem;
   struct partialMatch *theBasis;
   ACTIVATION *theActivation;
   char *ruleFiring;
   
#if EXTENDED_RUN_OPTIONS /* added 03-06-96 */

   struct callFunctionItem *runstart_ptr;
   struct callFunctionItem *runstop_ptr;
   int runForever = CLIPS_TRUE;  /* TRUE if to run continuously --
                                    stop if error in rule or an exec function
                                    sets execution error to TRUE */
   int runTillAgendaEmpty = CLIPS_TRUE; /* TRUE if to stop when agenda is empty */
   
   /*=====================================================*/
   /* Fire rules until the agenda is empty, the run limit */
   /* has been reached, or a rule execution error occurs. */
   /*                                                     */
   /*   *** modified at NRC to allow (run -2) to keep     */
   /*       cycling even though the agenda is empty       */
   /*       This is very useful when external events      */
   /*       may cause facts to be added to the system     */
   /*       which in turn may cause rules to be fired!    */
   /*       If (run n) where n is less than -2 then the   */
   /*       keep cycling even though the agenda may be-   */
   /*       empty but stop if abs(n) rules complete!      */
   /*                                                     */
   /*   for (run n)                                       */
   /*                                                     */
   /*   n           >=0    -1     -2    <-2               */
   /*                                                     */
   /*  runForever    F      T      T     F                */
   /*                                                     */
   /*  runTillEmpty  T      T      F     F                */
   /*                                                     */
   /*                                                     */
   /*  If runforever is true then the count of rules      */
   /*  executed is ignored. If runtillempty is true then  */
   /*  stop if the AGENDA becomes empty.                  */
   /*                                                     */
   /*=====================================================*/

#endif   

   /*=====================================================*/
   /* Make sure the run command is not already executing. */
   /*=====================================================*/

   if (alreadyRunning) return(0);
   alreadyRunning = CLIPS_TRUE;

   /*================================*/
   /* Set up statistics information. */
   /*================================*/

#if DEBUGGING_FUNCTIONS
   if (WatchStatistics)
     {
#if DEFTEMPLATE_CONSTRUCT
      maxFacts = GetNumberOfFacts();
      sumFacts = maxFacts;
#endif
#if OBJECT_SYSTEM
      maxInstances = GetGlobalNumberOfInstances();
      sumInstances = maxInstances;
#endif
      maxActivations = GetNumberOfActivations();
      sumActivations = maxActivations;
      startTime = gentime();
     }
#endif

   /*=============================*/
   /* Set up execution variables. */
   /*=============================*/

   if (CurrentEvaluationDepth == 0) SetHaltExecution(CLIPS_FALSE);
   HaltRules = CLIPS_FALSE;
     
   /*=====================================================*/
   /* Fire rules until the agenda is empty, the run limit */
   /* has been reached, or a rule execution error occurs. */
   /*=====================================================*/
   
   theActivation = NextActivationToFire();

#if EXTENDED_RUN_OPTIONS   /* added 03-06-96 */

   if (runLimit != -1)
     if (runLimit == -2)
       runTillAgendaEmpty = CLIPS_FALSE;
     else if (runLimit < -2)
       { runLimit = -runLimit;
         runForever = CLIPS_FALSE;
         runTillAgendaEmpty = CLIPS_FALSE;
       }
     else
       runForever = CLIPS_FALSE;


   /*==================================================*/
   /* added at NRC ******                              */
   /*                                                  */
   /* Execute any functions necessary for run starting */
   /*==================================================*/
   
   for (runstart_ptr = ListOfRunStartFunctions;
        runstart_ptr != NULL;
        runstart_ptr = runstart_ptr->next)
     { (*runstart_ptr->func)(); }
	 
  while ((HaltExecution == CLIPS_FALSE) &&
          (HaltRules == CLIPS_FALSE))
     {      
       if (runTillAgendaEmpty && (theActivation == NULL))
	      break;
      else if (!runForever && (runLimit == 0))
	      break;

      if (theActivation != NULL) /* if idle loop (runforever & no agenda) */
      {

#else
     
   while ((theActivation != NULL) &&
          (runLimit != 0) &&
          (HaltExecution == CLIPS_FALSE) &&
          (HaltRules == CLIPS_FALSE))
     {

#endif
      
      /*===========================================*/
      /* Detach the activation from the agenda and */
      /* determine which rule is firing.           */
      /*===========================================*/

      DetachActivation(theActivation);
      ruleFiring = GetActivationName(theActivation);
      theBasis = (struct partialMatch *) GetActivationBasis(theActivation);
      ExecutingRule = (struct defrule *) GetActivationRule(theActivation);

#if CERTAINTY_FACTORS /* added 03-06-96 */
      /* For certainty factors we need to check that the rule should     */
      /* really be executed when threshold limit is > 0.0. The           */
      /* thresholdCF is compared to the Rule's concluding certainty.     */
      /*                                                                 */
      /* StdConcludingCF = RuleCF * minimum(all fact CF's matched on LHS)*/
      /*                                                                 */
      /* Also the CF's of any asserted facts depend on the rule's        */
      /* concluding certainty so this is added to the activation record  */
      /*                                                                 */
      /* Must store the StdConcludingCF in theActivation record so it    */
      /* can be used when asserting facts without having to calculate it */
      /* again.                                                          */

      if (Threshold_CF == 0.0 || 
          (Threshold_CF <= 
                (theActivation->StdConcludingCF =
                          computeStdConclCF(theActivation->CF, theBasis))
          )
         )
      {
#endif	
      
      /*=============================================*/
      /* Update the number of rules that have fired. */
      /*=============================================*/
      
      rulesFired++;
      if (runLimit > 0) { runLimit--; }

      /*==================================*/
      /* If rules are being watched, then */
      /* print an information message.    */
      /*==================================*/
      
#if DEBUGGING_FUNCTIONS
      if (ExecutingRule->watchFiring)
        {
         char printSpace[60];

         sprintf(printSpace,"FIRE %4ld ",rulesFired);
         PrintCLIPS(WTRACE,printSpace);
         PrintCLIPS(WTRACE,ruleFiring);
         PrintCLIPS(WTRACE,": ");
         PrintPartialMatch(WTRACE,theBasis);
         PrintCLIPS(WTRACE,"\n");
        }
#endif
      
      /*=================================================*/
      /* Remove the link between the activation and the  */
      /* completed match for the rule. Set the busy flag */
      /* for the completed match to TRUE (so the match   */
      /* upon which our RHS variables are dependent is   */
      /* not deleted while our rule is firing). Set up   */
      /* the global pointers to the completed match for  */
      /* routines which do variable extractions.         */
      /*=================================================*/

      theBasis->binds[theBasis->bcount].gm.theValue = NULL;
      theBasis->busy = CLIPS_TRUE;

      GlobalLHSBinds = theBasis;
      GlobalRHSBinds = NULL;

      /*===================================================================*/
      /* Increment the count for each of the facts/objects associated with */
      /* the rule activation so that the facts/objects cannot be deleted   */
      /* by garbage collection while the rule is executing.                */
      /*===================================================================*/

      for (i = 0; i < theBasis->bcount; i++)
        {
         theMatchingItem = theBasis->binds[i].gm.theMatch->matchingItem;
         if (theMatchingItem != NULL)
           { (*theMatchingItem->theInfo->incrementBasisCount)(theMatchingItem); }
        }

      /*====================================================*/
      /* Execute the rule's right hand side actions. If the */
      /* rule has logical CEs, set up the pointer to the    */
      /* rules logical join so the assert command will      */
      /* attach the appropriate dependencies to the facts.  */
      /*====================================================*/

#if LOGICAL_DEPENDENCIES
      TheLogicalJoin = ExecutingRule->logicalJoin;
#endif
      CurrentEvaluationDepth++;
      SetEvaluationError(CLIPS_FALSE);
      ExecutingRule->executing = CLIPS_TRUE;

#if FUZZY_DEFTEMPLATES
      /* Need min of maxmins for FUZZY_FUZZY type rules.                   */
      /* Asserts (FUZZY) on RHS use it to calc fuzzy consequence.          */
      /* computeFuzzyConsequence (fuzzyutl.c) will call computeMinOfMaxmins*/
      /* to calculate the actual min_of_maxmins when a fuzzy fact is       */ 
      /* asserted from a rule and the rule's LHS is FUZZY.                 */
      /* Set ExecutingRule->min_of_maxmins is -1.0 -- this is so it only   */
      /* gets calculated once per rule execution and not for every fuzzy   */
      /* fact that is asserted!                                            */

      ExecutingRule->min_of_maxmins = -1.0;
#endif

#if CERTAINTY_FACTORS  /* added 03-06-96 */
      /* global needed when asserts are done on RHS of rule */
      theCurrentActivation = theActivation;
#endif
      EvaluateProcActions(ExecutingRule->header.whichModule->theModule,
                          ExecutingRule->actions,ExecutingRule->localVarCnt,
                          &result,NULL);
      ExecutingRule->executing = CLIPS_FALSE;
      SetEvaluationError(CLIPS_FALSE);
      CurrentEvaluationDepth--;
#if LOGICAL_DEPENDENCIES
      TheLogicalJoin = NULL;
#endif
        
      /*=====================================================*/
      /* If rule execution was halted, then print a message. */
      /*=====================================================*/
      
#if DEBUGGING_FUNCTIONS
      if ((HaltExecution) || (HaltRules && ExecutingRule->watchFiring))
#else
      if ((HaltExecution) || (HaltRules))
#endif

        {
         PrintErrorID("PRCCODE",4,CLIPS_FALSE);
         PrintCLIPS(WERROR,"Execution halted during the actions of defrule ");
         PrintCLIPS(WERROR,ruleFiring);
         PrintCLIPS(WERROR,".\n");
        }

      /*===================================================================*/
      /* Decrement the count for each of the facts/objects associated with */
      /* the rule activation. If the last match for the activation         */
      /* is from a not CE, then we need to make sure that the last         */
      /* match is an actual match for the CE and not a counter.            */
      /*===================================================================*/

      theBasis->busy = CLIPS_FALSE;
      
      for (i = 0; i < (theBasis->bcount - 1); i++)
        {
         theMatchingItem = theBasis->binds[i].gm.theMatch->matchingItem;
         if (theMatchingItem != NULL)
           { (*theMatchingItem->theInfo->decrementBasisCount)(theMatchingItem); }
        }
        
      i = theBasis->bcount - 1;
      if (theBasis->counterf == CLIPS_FALSE)
        {
         theMatchingItem = theBasis->binds[i].gm.theMatch->matchingItem;
         if (theMatchingItem != NULL)
           { (*theMatchingItem->theInfo->decrementBasisCount)(theMatchingItem); }
        }

      /*========================================*/
      /* Return the agenda node to free memory. */
      /*========================================*/

      RemoveActivation(theActivation,CLIPS_FALSE,CLIPS_FALSE);

      /*======================================*/
      /* Get rid of partial matches discarded */
      /* while executing the rule's RHS.      */
      /*======================================*/
      
      FlushGarbagePartialMatches();

      /*==================================*/
      /* Get rid of other garbage created */
      /* while executing the rule's RHS.  */
      /*==================================*/
      
      PeriodicCleanup(CLIPS_FALSE,CLIPS_TRUE);

      /*==========================*/
      /* Keep up with statistics. */
      /*==========================*/
      
#if DEBUGGING_FUNCTIONS
      if (WatchStatistics)
        {
#if DEFTEMPLATE_CONSTRUCT
         tempValue = GetNumberOfFacts();
         if (tempValue > maxFacts) maxFacts = tempValue;
         sumFacts += tempValue;
#endif
#if OBJECT_SYSTEM
         tempValue = GetGlobalNumberOfInstances();
         if (tempValue > maxInstances) maxInstances = tempValue;
         sumInstances += tempValue;
#endif
         tempValue = GetNumberOfActivations();
         if (tempValue > maxActivations) maxActivations = tempValue;
         sumActivations += tempValue;
        }
#endif

      /*==================================*/
      /* Update saliences if appropriate. */
      /*==================================*/

#if DYNAMIC_SALIENCE
      if (GetSalienceEvaluation() == EVERY_CYCLE) RefreshAgenda(NULL);
#endif

#if CERTAINTY_FACTORS /* added 03-06-96 */
      } /* end of   if (Threshold_CF == 0.0 || ...  */
      else
      {
        /*================================================*/
        /* even if below threshold and not executing the  */
        /* rule we need to clean up a few things          */
        /*================================================*/

        /*=================================================*/
        /* Remove the link between the activation and the  */
        /* completed match for the rule.                   */
        /*=================================================*/

        theBasis->binds[theBasis->bcount].gm.theValue = NULL;

        /*=================================================*/
        /*  must Return the agenda node to free memory     */
        /*=================================================*/

        RemoveActivation(theActivation,CLIPS_FALSE,CLIPS_FALSE);
      }
#endif

#if EXTENDED_RUN_OPTIONS  /* added 03-06-96 */
      } /* end of test for theActivation != NULL */
     else
      {
       /*==================================*/
       /* Just do the cleanup if idleing   */
       /*==================================*/
      
       PeriodicCleanup(CLIPS_FALSE,CLIPS_TRUE);
      }
#endif

      /*========================================*/
      /* Execute the list of functions that are */
      /* to be called after each rule firing.   */
      /*========================================*/

      for (theRunFunction = ListOfRunFunctions;
           theRunFunction != NULL;
           theRunFunction = theRunFunction->next)
        { (*theRunFunction->func)(); }

      /*========================================*/
      /* If a return was issued on the RHS of a */
      /* rule, then remove *that* rule's module */
      /* from the focus stack                   */
      /*========================================*/
      
      if (ReturnFlag == CLIPS_TRUE)
        { RemoveFocus(ExecutingRule->header.whichModule->theModule); }
      ReturnFlag = CLIPS_FALSE;
      
      /*========================================*/
      /* Determine the next activation to fire. */
      /*========================================*/
      
      theActivation = (struct activation *) NextActivationToFire();
      
      /*==============================*/
      /* Check for a rule breakpoint. */
      /*==============================*/

      if (theActivation != NULL)
        {
         if (((struct defrule *) GetActivationRule(theActivation))->afterBreakpoint)
           {
            HaltRules = CLIPS_TRUE;
            PrintCLIPS(WDIALOG,"Breaking on rule ");
            PrintCLIPS(WDIALOG,GetActivationName(theActivation));
            PrintCLIPS(WDIALOG,".\n");
           }
        }
     }

#if EXTENDED_RUN_OPTIONS /* added 03-06-96 */
   /*==================================================*/
   /* added at NRC ******                              */
   /*                                                  */
   /* Execute any functions necessary for run stopping */
   /*==================================================*/
   
   for (runstop_ptr = ListOfRunStopFunctions;
        runstop_ptr != NULL;
        runstop_ptr = runstop_ptr->next)
     { (*runstop_ptr->func)(); }

#endif

   /*======================================================*/
   /* If rule execution was halted because the rule firing */
   /* limit was reached, then print a message.             */
   /*======================================================*/
   
   if (runLimit == rulesFired)
     { PrintCLIPS(WDIALOG,"rule firing limit reached\n"); }

   /*==============================*/
   /* Restore execution variables. */
   /*==============================*/

   ExecutingRule = NULL;
   HaltRules = CLIPS_FALSE;
#if CERTAINTY_FACTORS /* added 03-06-96 */
   /* global needed when asserts are done on RHS of rule */
   theCurrentActivation = NULL;
#endif

   /*=================================================*/
   /* Print out statistics if they are being watched. */
   /*=================================================*/
   
#if DEBUGGING_FUNCTIONS
   if (WatchStatistics)
     {
      char printSpace[60];

      endTime = gentime();

      PrintLongInteger(WDIALOG,rulesFired);
      PrintCLIPS(WDIALOG," rules fired");

#if (! GENERIC)
      if (startTime != endTime)
        {
         PrintCLIPS(WDIALOG,"        Run time is ");
         PrintFloat(WDIALOG,endTime - startTime);
         PrintCLIPS(WDIALOG," seconds.\n");
         PrintFloat(WDIALOG,(double) rulesFired / (endTime - startTime));
         PrintCLIPS(WDIALOG," rules per second.\n");
        }
      else
        { PrintCLIPS(WDIALOG,"\n"); }
#endif

#if DEFTEMPLATE_CONSTRUCT
      sprintf(printSpace,"%ld mean number of facts (%ld maximum).\n",
                          (long) (((double) sumFacts / (rulesFired + 1)) + 0.5),
                          maxFacts);
      PrintCLIPS(WDIALOG,printSpace);
#endif

#if OBJECT_SYSTEM
      sprintf(printSpace,"%ld mean number of instances (%ld maximum).\n",
                          (long) (((double) sumInstances / (rulesFired + 1)) + 0.5),
                          maxInstances);
      PrintCLIPS(WDIALOG,printSpace);
#endif

      sprintf(printSpace,"%ld mean number of activations (%ld maximum).\n",
                          (long) (((double) sumActivations / (rulesFired + 1)) + 0.5),
                          maxActivations);
      PrintCLIPS(WDIALOG,printSpace);
     }
#endif

   /*==========================================*/
   /* The current module should be the current */
   /* focus when the run finishes.             */
   /*==========================================*/
   
   if (CurrentFocus != NULL)
     {
      if (CurrentFocus->theModule != ((struct defmodule *) GetCurrentModule()))
        { SetCurrentModule((VOID *) CurrentFocus->theModule); }
     }
        
   /*===================================*/
   /* Return the number of rules fired. */
   /*===================================*/

   alreadyRunning = CLIPS_FALSE;
   return(rulesFired);
  }
  
/***********************************************************/
/* NextActivationToFire: Returns the next activation which */
/*   should be executed based on the current focus.        */
/***********************************************************/
static struct activation *NextActivationToFire()
  {
   struct activation *theActivation;
   struct defmodule *theModule;
   
   /*====================================*/
   /* If there is no current focus, then */
   /* focus on the MAIN module.          */
   /*====================================*/
                     
   if (CurrentFocus == NULL)
     { 
      theModule = (struct defmodule *) FindDefmodule("MAIN");
      Focus(theModule);
     }
     
   /*===========================================================*/
   /* Determine the top activation on the agenda of the current */
   /* focus. If the current focus has no activations on its     */
   /* agenda, then pop the focus off the focus stack until      */
   /* a focus that has an activation on its agenda is found.    */
   /*===========================================================*/
   
   theActivation = CurrentFocus->theDefruleModule->agenda;
   while ((theActivation == NULL) && (CurrentFocus != NULL))
     {
      if (CurrentFocus != NULL) PopFocus(); 
      if (CurrentFocus != NULL) theActivation = CurrentFocus->theDefruleModule->agenda;
     }
     
   /*=========================================*/
   /* Return the next activation to be fired. */
   /*=========================================*/
   
   return(theActivation);
  }
  
/***************************************************/
/* RemoveFocus: Removes the first occurence of the */
/*   specified module from the focus stack.        */
/***************************************************/
static struct defmodule *RemoveFocus(theModule)
  struct defmodule *theModule;
  {
   struct focus *tempFocus,*prevFocus, *nextFocus;
   int found = CLIPS_FALSE;
   int currentFocusRemoved = CLIPS_FALSE;

   /*====================================*/
   /* Return NULL if there is nothing on */
   /* the focus stack to remove.         */
   /*====================================*/
   
   if (CurrentFocus == NULL) return(NULL);

   /*=============================================*/
   /* Remove the first occurence of the specified */
   /* module from the focus stack.                */
   /*=============================================*/
   
   prevFocus = NULL;
   tempFocus = CurrentFocus;
   while ((tempFocus != NULL) && (! found))
     {
      if (tempFocus->theModule == theModule)
        {
         found = CLIPS_TRUE;

         nextFocus = tempFocus->next;
         rtn_struct(focus,tempFocus);
         tempFocus = nextFocus;

         if (prevFocus == NULL)
           {
            currentFocusRemoved = CLIPS_TRUE;
            CurrentFocus = tempFocus;
           }
         else
           { prevFocus->next = tempFocus; }
        }
      else
        {
         prevFocus = tempFocus;
         tempFocus = tempFocus->next;
        }
     }
   
   /*=========================================*/
   /* If the given module is not in the focus */
   /* stack, simply return the current focus  */
   /*=========================================*/
   
   if (! found) return(CurrentFocus->theModule);
   
   /*========================================*/
   /* If the current focus is being watched, */
   /* then print an informational message.   */
   /*========================================*/

#if DEBUGGING_FUNCTIONS
   if (WatchFocus)
     {
      PrintCLIPS(WTRACE,"<== Focus ");
      PrintCLIPS(WTRACE,ValueToString(theModule->name));
      
      if ((CurrentFocus != NULL) && currentFocusRemoved)
        {
         PrintCLIPS(WTRACE," to ");
         PrintCLIPS(WTRACE,ValueToString(CurrentFocus->theModule->name));
        }
        
      PrintCLIPS(WTRACE,"\n");
     }
#endif
   
   /*======================================================*/
   /* Set the current module to the module associated with */
   /* the current focus (if it changed) and set a boolean  */
   /* flag indicating that the focus has changed.          */
   /*======================================================*/
   
   if ((CurrentFocus != NULL) && currentFocusRemoved)
     { SetCurrentModule((VOID *) CurrentFocus->theModule); }
   FocusChanged = CLIPS_TRUE;
   
   /*====================================*/
   /* Return the module that was removed */
   /* from the focus stack.              */
   /*====================================*/
   
   return(theModule);
  }
  
/**********************************************************/
/* PopFocus: C access routine for the pop-focus function. */
/**********************************************************/
globle VOID *PopFocus()
  {
   if (CurrentFocus == NULL) return(NULL);
   return((VOID *) RemoveFocus(CurrentFocus->theModule));
  }
  
/*************************************************************/
/* GetNextFocus: Returns the next focus on the focus stack. */
/*************************************************************/
globle VOID *GetNextFocus(theFocus)
  VOID *theFocus;
  {
   /*==================================================*/
   /* If NULL is passed as an argument, return the top */
   /* focus on the focus stack (the current focus).    */
   /*==================================================*/
   
   if (theFocus == NULL) return((VOID *) CurrentFocus);
   
   /*=======================================*/
   /* Otherwise, return the focus following */
   /* the focus passed as an argument.      */
   /*=======================================*/
   
   return((VOID *) ((struct focus *) theFocus)->next);
  }
  
/***************************************************/
/* Focus: C access routine for the focus function. */
/***************************************************/
globle VOID Focus(vTheModule)
  VOID *vTheModule;
  {
   struct defmodule *theModule = (struct defmodule *) vTheModule;
   struct focus *tempFocus;
   
   /*==================================================*/
   /* Make the specified module be the current module. */
   /* If the specified module is the current focus,    */
   /* then no further action is needed.                */
   /*==================================================*/
   
   SetCurrentModule((VOID *) theModule);
   if (CurrentFocus != NULL)
     { if (CurrentFocus->theModule == theModule) return; }
  
   /*=====================================*/
   /* If the focus is being watched, then */
   /* print an information message.       */
   /*=====================================*/
   
#if DEBUGGING_FUNCTIONS   
   if (WatchFocus)
     {
      PrintCLIPS(WTRACE,"==> Focus ");
      PrintCLIPS(WTRACE,ValueToString(theModule->name));
      if (CurrentFocus != NULL)
        {
         PrintCLIPS(WTRACE," from ");
         PrintCLIPS(WTRACE,ValueToString(CurrentFocus->theModule->name));
        }
      PrintCLIPS(WTRACE,"\n");
     }
#endif
   
   /*=======================================*/
   /* Add the new focus to the focus stack. */
   /*=======================================*/
   
   tempFocus = get_struct(focus);
   tempFocus->theModule = theModule;
   tempFocus->theDefruleModule = GetDefruleModuleItem(theModule);
   tempFocus->next = CurrentFocus;
   CurrentFocus = tempFocus;
   FocusChanged = CLIPS_TRUE;
  }

/************************************************/
/* ClearFocusStackCommand: CLIPS access routine */
/*   for the clear-focus-stack command.         */
/************************************************/
globle VOID ClearFocusStackCommand()
  {
   if (ArgCountCheck("list-focus-stack",EXACTLY,0) == -1) return;

   ClearFocusStack();
  }
  
/****************************************/
/* ClearFocusStack: C access routine    */
/*   for the clear-focus-stack command. */
/****************************************/
globle VOID ClearFocusStack()
  {
   while (CurrentFocus != NULL) PopFocus();
     
   FocusChanged = CLIPS_TRUE;
  }
  
/**************************************************************/
/* AddRunFunction: Adds a function to the ListOfRunFunctions. */
/**************************************************************/
globle BOOLEAN AddRunFunction(name,functionPtr,priority)
  char *name;
#if ANSI_COMPILER
   VOID (*functionPtr)(void);
#else
   VOID (*functionPtr)();
#endif
  int priority;
  {
   ListOfRunFunctions = AddFunctionToCallList(name,priority,
                                              functionPtr,
                                              ListOfRunFunctions);
   return(1);
  }

/**********************************************************************/
/* RemoveRunFunction: Removes a function from the ListOfRunFunctions. */
/**********************************************************************/
globle BOOLEAN RemoveRunFunction(name)
  char *name;
  {
   int found;

   ListOfRunFunctions = RemoveFunctionFromCallList(name,ListOfRunFunctions,&found);

   if (found) return(CLIPS_TRUE);
   
   return(CLIPS_FALSE);
  }


#if EXTENDED_RUN_OPTIONS /* added 03-06-96 */

/************************************************************************************

       NOTE: Following routines added at NRC for support to
                 allow functions to be executed on entry
                 and exit from the 'run' command

************************************************************************************/

  
/************************************************************************/
/* AddRunStartFunction: Adds a function to the ListOfRunStartFunctions. */
/************************************************************************/
globle BOOLEAN AddRunStartFunction(name,functionPtr,priority)
  char *name;
#if ANSI_COMPILER
   VOID (*functionPtr)(void);
#else
   VOID (*functionPtr)();
#endif
  int priority;
  {
   ListOfRunStartFunctions = AddFunctionToCallList(name,priority,
                                              functionPtr,
                                              ListOfRunStartFunctions);
   return(1);
  }

/********************************************************************************/
/* RemoveRunStartFunction: Removes a function from the ListOfRunStartFunctions. */
/********************************************************************************/
globle BOOLEAN RemoveRunStartFunction(name)
  char *name;
  {
   int found;

   ListOfRunStartFunctions = RemoveFunctionFromCallList(name,ListOfRunStartFunctions,&found);

   if (found) return(CLIPS_TRUE);
   
   return(CLIPS_FALSE);
  }

  
/************************************************************************/
/* AddRunStopFunction: Adds a function to the ListOfRunStopFunctions.   */
/************************************************************************/
globle BOOLEAN AddRunStopFunction(name,functionPtr,priority)
  char *name;
#if ANSI_COMPILER
   VOID (*functionPtr)(void);
#else
   VOID (*functionPtr)();
#endif
  int priority;
  {
   ListOfRunStopFunctions = AddFunctionToCallList(name,priority,
                                              functionPtr,
                                              ListOfRunStopFunctions);
   return(1);
  }

/********************************************************************************/
/* RemoveRunStopFunction: Removes a function from the ListOfRunStopFunctions.   */
/********************************************************************************/
globle BOOLEAN RemoveRunStopFunction(name)
  char *name;
  {
   int found;

   ListOfRunStopFunctions = RemoveFunctionFromCallList(name,ListOfRunStopFunctions,&found);

   if (found) return(CLIPS_TRUE);
   
   return(CLIPS_FALSE);
  }



#endif   /* EXTENDED_RUN_OPTIONS */

/*****************************************************************************/
/* InitializeEngine: Initializes the activations and statistics watch items. */
/*****************************************************************************/
globle VOID InitializeEngine()
  {
#if DEBUGGING_FUNCTIONS
   AddWatchItem("statistics",0,&WatchStatistics,20,NULL,NULL);
   AddWatchItem("focus",0,&WatchFocus,0,NULL,NULL);
#endif
  }
  
/*********************************************************/
/* RunCommand: CLIPS access routine for the run command. */
/*********************************************************/
globle VOID RunCommand()
  {
   int numArgs;
   long int runLimit = -1;
   DATA_OBJECT argPtr;

   if ((numArgs = ArgCountCheck("run",NO_MORE_THAN,1)) == -1) return;

   if (numArgs == 0)
     { runLimit = -1; }
   else if (numArgs == 1)
     {
      if (ArgTypeCheck("run",1,INTEGER,&argPtr) == CLIPS_FALSE) return;
      runLimit = DOToLong(argPtr);
     }

   Run(runLimit);

   return;
  }
  
/***********************************************/
/* HaltCommand: Causes rule execution to halt. */
/***********************************************/
globle VOID HaltCommand()
  {
   ArgCountCheck("halt",EXACTLY,0);
   HaltRules = CLIPS_TRUE;
  }
  
#if DEBUGGING_FUNCTIONS

/*********************************************************/
/* SetBreak: C access routine for the set-break command. */
/*********************************************************/
globle VOID SetBreak(theRule)
  VOID *theRule;
  {
   struct defrule *thePtr;

   for (thePtr = (struct defrule *) theRule;
        thePtr != NULL;
        thePtr = thePtr->disjunct)
     { thePtr->afterBreakpoint = 1; }
  }

/***************************************************************/
/* RemoveBreak: C access routine for the remove-break command. */
/***************************************************************/
globle BOOLEAN RemoveBreak(theRule)
  VOID *theRule;
  {
   struct defrule *thePtr;
   int rv = CLIPS_FALSE;

   for (thePtr = (struct defrule *) theRule;
        thePtr != NULL;
        thePtr = thePtr->disjunct)
     {
      if (thePtr->afterBreakpoint == 1)
        {
         thePtr->afterBreakpoint = 0;
         rv = CLIPS_TRUE;
        }
     }

   return(rv);
  }

/**************************************************/
/* RemoveAllBreakpoints: Removes all breakpoints. */
/**************************************************/
globle VOID RemoveAllBreakpoints()
  {
   VOID *theRule;
   VOID *theDefmodule = NULL;
   
   while ((theDefmodule = GetNextDefmodule(theDefmodule)) != NULL)
     {
      theRule = NULL;
      while ((theRule = GetNextDefrule(theRule)) != NULL)
        { RemoveBreak(theRule); }
     }
  }

/*************************************************************/
/* ShowBreaks: C access routine for the show-breaks command. */
/*************************************************************/
globle VOID ShowBreaks(logicalName,vTheModule)
  char *logicalName;
  VOID *vTheModule;
  {  
   ListItemsDriver(logicalName,(struct defmodule *) vTheModule,
                   NULL,NULL,
#if ANSI_COMPILER
                   GetNextDefrule,(char *(*)(VOID *)) GetConstructNameString,
#else
                   GetNextDefrule,(char *(*)()) GetConstructNameString,
#endif
                   NULL,DefruleHasBreakpoint);
   }

/************************************************************************************/
/* DefruleHasBreakpoint: Indicates whether the specified rule has a breakpoint set. */
/************************************************************************************/
globle BOOLEAN DefruleHasBreakpoint(theRule)
  VOID *theRule;
  {
   return(((struct defrule *) theRule)->afterBreakpoint);
  }

/*****************************************/
/* SetBreakCommand: CLIPS access routine */
/*   for the set-break command.          */
/*****************************************/
globle VOID SetBreakCommand()
  {
   DATA_OBJECT argPtr;
   char *argument;
   VOID *defrulePtr;

   if (ArgCountCheck("set-break",EXACTLY,1) == -1) return;

   if (ArgTypeCheck("set-break",1,SYMBOL,&argPtr) == CLIPS_FALSE) return;

   argument = DOToString(argPtr);

   if ((defrulePtr = FindDefrule(argument)) == NULL)
     {
      CantFindItemErrorMessage("defrule",argument);
      return;
     }

   SetBreak(defrulePtr);
  }

/********************************************/
/* RemoveBreakCommand: CLIPS access routine */
/*   for the remove-break command.          */
/********************************************/
globle VOID RemoveBreakCommand()
  {
   DATA_OBJECT argPtr;
   char *argument;
   int nargs;
   VOID *defrulePtr;

   if ((nargs = ArgCountCheck("remove-break",NO_MORE_THAN,1)) == -1)
     { return; }

   if (nargs == 0)
     {
      RemoveAllBreakpoints();
      return;
     }

   if (ArgTypeCheck("remove-break",1,SYMBOL,&argPtr) == CLIPS_FALSE) return;

   argument = DOToString(argPtr);

   if ((defrulePtr = FindDefrule(argument)) == NULL)
     {
      CantFindItemErrorMessage("defrule",argument);
      return;
     }

   if (RemoveBreak(defrulePtr) == CLIPS_FALSE)
     {
      PrintCLIPS(WERROR,"Rule ");
      PrintCLIPS(WERROR,argument);
      PrintCLIPS(WERROR," does not have a breakpoint set.\n");
     }
  }

/*******************************************/
/* ShowBreaksCommand: CLIPS access routine */
/*   for the show-breaks command.          */
/*******************************************/
globle VOID ShowBreaksCommand()
  {
   int numArgs, error;
   struct defmodule *theModule;
   
   if ((numArgs = ArgCountCheck("show-breaks",NO_MORE_THAN,1)) == -1) return;

   if (numArgs == 1)
     {
      theModule = GetModuleName("show-breaks",1,&error);
      if (error) return;
     }
   else
     { theModule = ((struct defmodule *) GetCurrentModule()); }

   ShowBreaks(WDISPLAY,theModule);
  }
  
/***********************************************/
/* ListFocusStackCommand: CLIPS access routine */
/*   for the list-focus-stack command.         */
/***********************************************/
globle VOID ListFocusStackCommand()
  {
   if (ArgCountCheck("list-focus-stack",EXACTLY,0) == -1) return;

   ListFocusStack(WDISPLAY);
  }
  
/****************************************/
/* ListFocusStack: C access routine for */
/*   the list-focus-stack command.      */
/****************************************/
globle VOID ListFocusStack(logicalName)
  char *logicalName;
  {
   struct focus *theFocus;
        
   for (theFocus = CurrentFocus;
        theFocus != NULL;
        theFocus = theFocus->next)
     {
      PrintCLIPS(logicalName,GetDefmoduleName(theFocus->theModule));
      PrintCLIPS(logicalName,"\n");
     }
  }
  
#endif

/***********************************************/
/* GetFocusStackFunction: CLIPS access routine */
/*   for the get-focus-stack function.         */
/***********************************************/
globle VOID GetFocusStackFunction(returnValue)
  DATA_OBJECT_PTR returnValue;
  {
   if (ArgCountCheck("get-focus-stack",EXACTLY,0) == -1) return;

   GetFocusStack(returnValue);
  }
  
/***************************************/
/* GetFocusStack: C access routine for */
/*   the get-focus-stack function.     */
/***************************************/
globle VOID GetFocusStack(returnValue)
  DATA_OBJECT_PTR returnValue;
  {
   struct focus *theFocus;
   struct multifield *theList;
   long count = 0;
      
   /*===========================================*/
   /* If there is no current focus, then return */
   /* a multifield value of length zero.        */
   /*===========================================*/
   
   if (CurrentFocus == NULL)
     {
      SetpType(returnValue,MULTIFIELD);
      SetpDOBegin(returnValue,1);
      SetpDOEnd(returnValue,0);
      SetpValue(returnValue,(VOID *) CreateMultifield(0L));
      return;
     }
  
   /*=====================================================*/
   /* Determine the number of modules on the focus stack. */
   /*=====================================================*/
   
   for (theFocus = CurrentFocus; theFocus != NULL; theFocus = theFocus->next)
     { count++; }
   
   /*=============================================*/
   /* Create a multifield of the appropriate size */
   /* in which to store the module names.         */
   /*=============================================*/
   
   SetpType(returnValue,MULTIFIELD);
   SetpDOBegin(returnValue,1);
   SetpDOEnd(returnValue,count);
   theList = (struct multifield *) CreateMultifield(count);
   SetpValue(returnValue,(VOID *) theList);
   
   /*=================================================*/
   /* Store the module names in the multifield value. */
   /*=================================================*/
   
   for (theFocus = CurrentFocus, count = 1; 
        theFocus != NULL; 
        theFocus = theFocus->next, count++)
     { 
      SetMFType(theList,count,SYMBOL);
      SetMFValue(theList,count,theFocus->theModule->name);
     }
  }
  
/******************************************/
/* PopFocusFunction: CLIPS access routine */
/*   for the pop-focus function.          */
/******************************************/
globle SYMBOL_HN *PopFocusFunction()
  {
   struct defmodule *theModule;
   
   ArgCountCheck("pop-focus",EXACTLY,0);
   
   theModule = (struct defmodule *) PopFocus();
   if (theModule == NULL) return((SYMBOL_HN *) CLIPSFalseSymbol);
   return(theModule->name);
  }

/******************************************/
/* GetFocusFunction: CLIPS access routine */
/*   for the get-focus function.          */
/******************************************/
globle SYMBOL_HN *GetFocusFunction()
  {
   struct defmodule *rv;
   
   ArgCountCheck("get-focus",EXACTLY,0);
   rv = (struct defmodule *) WRGetFocus(); 
   if (rv == NULL) return((SYMBOL_HN *) CLIPSFalseSymbol);  
   return(rv->name);
  }
  
/**********************************/
/* GetFocus: C access routine for */
/*   the get-focus function.      */
/**********************************/
globle VOID *WRGetFocus()
  {
   if (CurrentFocus == NULL) return(NULL);
   
   return((VOID *) CurrentFocus->theModule);
  }
  
/**************************************/
/* FocusCommand: CLIPS access routine */
/*   for the focus function.          */
/**************************************/
globle int FocusCommand()
  {
   DATA_OBJECT argPtr;
   char *argument;
   struct defmodule *theModule;
   int argCount, i;

   /*=====================================================*/
   /* Check for the correct number and type of arguments. */
   /*=====================================================*/
      
   if ((argCount = ArgCountCheck("focus",AT_LEAST,1)) == -1)
     { return(CLIPS_FALSE); }

   /*===========================================*/
   /* Focus on the specified defrule module(s). */
   /*===========================================*/
   
   for (i = argCount; i > 0; i--)
     {
      if (ArgTypeCheck("focus",i,SYMBOL,&argPtr) == CLIPS_FALSE)
        { return(CLIPS_FALSE); }

      argument = DOToString(argPtr);
      theModule = (struct defmodule *) FindDefmodule(argument);
   
      if (theModule == NULL)
        {
         CantFindItemErrorMessage("defmodule",argument);
         return(CLIPS_FALSE);
        }
     
      Focus((VOID *) theModule);
     }
   
   /*===================================================*/
   /* Return TRUE to indicate success of focus command. */
   /*===================================================*/
   
   return(CLIPS_TRUE);
  }
  
/********************************************************************/
/* GetFocusChanged: Returns the value of the variable FocusChanged. */
/********************************************************************/
globle int GetFocusChanged()
  { 
   return(FocusChanged);
  }

/*****************************************************************/
/* SetFocusChanged: Sets the value of the variable FocusChanged. */
/*****************************************************************/
globle VOID SetFocusChanged(value)
  int value;
  {
   FocusChanged = value;
  }

#endif /* DEFRULE_CONSTRUCT */

