   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  05/12/93            */
   /*                                                     */
   /*                     MAIN MODULE                     */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Bob Orchard (NRCC - Nat'l Research Council of Canada)*/
/*                  (Fuzzy reasoning extensions)             */
/*                  (certainty factors for facts and rules)  */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#include <stdio.h>
#include "setup.h"
#include "sysdep.h"
#include "extnfunc.h"
#include "commline.h"

#if FUZZY_DEFTEMPLATES
#include "fuzzyutl.h"
#endif

#if ANSI_COMPILER
int main(int,char *[]);
VOID UserFunctions(void);
#else
int main();
VOID UserFunctions();
#endif

/***************************************************************/
/* MAIN: Start execution of CLIPS.  This function must be      */
/*   redefined in order to embed CLIPS within another program. */
/*   Example of redefined main:                                */
/*     main()                                                  */
/*       {                                                     */
/*        InitializeCLIPS();                                   */
/*            .                                                */
/*            .                                                */
/*            .                                                */
/*        ProcessData();                                       */
/*        RunCLIPS(-1);                                        */
/*        EvaluateData();                                      */
/*            .                                                */
/*            .                                                */
/*            .                                                */
/*        FinalResults();                                      */
/*       }                                                     */
/***************************************************************/
int main(argc,argv)
  int argc;
  char *argv[] ;
  {
   InitializeCLIPS();   
   RerouteStdin(argc,argv);
   CommandLoop();
   return(-1);
  }
  
/*************************************************************/
/* UserFunctions:  The function which informs CLIPS of any   */
/*   user defined functions.  In the default case, there are */
/*   no user defined functions.  To define functions, either */
/*   this function must be replaced by a function with the   */
/*   same name within this file, or this function can be     */
/*   deleted from this file and included in another file.    */
/*   User defined functions may be included in this file or  */
/*   other files.                                            */
/*   Example of redefined UserFunctions:                     */
/*     UserFunctions()                                       */
/*       {                                                   */
/*        DefineFunction("fun1",'i',fun1,"fun1");            */
/*        DefineFunction("other",'f',other,"other");         */
/*       }                                                   */
/*************************************************************/
VOID UserFunctions()
  {
/*
#if FUZZY_DEFTEMPLATES

#define ShowerProblemUI 1

#if ! RUN_TIME
#if ShowerProblemUI
   VOID initSimulation();
   VOID SetValvePositions();

   DefineFunction("initSimulation",'v',PTIF initSimulation, "initSimulation");
   DefineFunction("SetValvePositions",'v',PTIF SetValvePositions, "SetValvePositions");
#endif

#endif


#endif  end of #if FUZZY_DEFTEMPLATES
*/
  }

