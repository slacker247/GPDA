   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  05/12/93            */
   /*                                                     */
   /*           CONSTRUCT CONSTRUCTS-TO-C HEADER          */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*      Support functions for the constructs-to-c of         */
/*      construct headers and related items.                 */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_cstrccmp
#define _H_cstrccmp

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _CSTRCCMP_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#ifndef _CLIPS_STDIO_
#define _CLIPS_STDIO_
#include <stdio.h>
#endif

#if ANSI_COMPILER  
   LOCALE VOID                           MarkConstructHeaders(int);
#else
   LOCALE VOID                           MarkConstructHeaders();
#endif 

#endif




