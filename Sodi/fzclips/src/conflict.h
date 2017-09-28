   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*  			    and				  */
   /*           Canada National Reserch Council           */
   /*                                                     */
   /*             FZ_CLIPS Version 6.04  06/19/96         */
   /*                                                     */
   /*                CONFICT HEADER FILE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: To solve the problem of name confliction with    */                                               /*           system functions/files                          */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Robert Orchard                                       */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

/*   06-19-96   FindSymbol is a function name in MPW 3.4.1   */

#ifndef H_conflict
#define H_conflict

#if MAC_MPW
#define FindSymbol MPW_FindSymbol
#endif


#endif



