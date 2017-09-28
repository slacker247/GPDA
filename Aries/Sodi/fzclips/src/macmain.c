   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*        CLIPS Macintosh Version 3.2  05/12/93        */
   /*                                                     */
   /*               MACINTOSH MAIN MODULE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Bob Orchard (NRCC)                                   */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#include "setup.h"

#include <stdio.h>
#define _CLIPS_STDIO_

#include "sysdep.h"
#include "commline.h"
#include "extnfunc.h"

#include "macinit.h"

#if ANSI_COMPILER
VOID UserFunctions(void);
#else
VOID UserFunctions();
#endif

#include "moduldef.h"

#if FUZZY_DEFTEMPLATES
#include "fuzzyutl.h"
#endif


/***************************************************************/
/* MAIN: Start execution of CLIPS.  This function must be      */
/*   redefined in order to embed CLIPS within another program. */
/*   Example of redefined main:                                */
/*     main()                                                  */
/*       {                                                     */
/*        init_clips();                                        */
/*            .                                                */
/*            .                                                */
/*            .                                                */
/*        process_data();                                      */
/*        RunCLIPS(-1);                                        */
/*        evaluate_data();                                     */
/*            .                                                */
/*            .                                                */
/*            .                                                */
/*        final_results();                                     */
/*       }                                                     */
/***************************************************************/
main()
  {
   InitializeCLIPS();
   InitializeInterface();
   CommandLoop();
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
#if FUZZY_DEFTEMPLATES

#if ! RUN_TIME

#endif


#endif  /* end of #if FUZZY_DEFTEMPLATES */
  }
  

       
