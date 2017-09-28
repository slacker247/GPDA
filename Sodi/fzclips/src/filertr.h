   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  02/25/94            */
   /*                                                     */
   /*             FILE I/O ROUTER HEADER FILE             */
   /*******************************************************/

/*************************************************************/
/* Purpose: I/O Router routines which allow files to be used */
/*   as input and output sources.                            */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_filertr
#define _H_filertr

#ifndef _CLIPS_STDIO_
#define _CLIPS_STDIO_
#include <stdio.h>
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _FILERTR_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ANSI_COMPILER
   LOCALE VOID                           InitializeFileRouter(void);
   LOCALE FILE                          *FindFptr(char *);
   LOCALE int                            OpenFile(char *,char *,char *);
   LOCALE int                            CloseAllFiles(void);
   LOCALE int                            CloseFile(char *);
   LOCALE int                            FindFile(char *);
#else
   LOCALE VOID                           InitializeFileRouter();
  LOCALE FILE                          *FindFptr();
   LOCALE int                            OpenFile();
   LOCALE int                            CloseAllFiles();
   LOCALE int                            CloseFile();
   LOCALE int                            FindFile();
#endif 
   
#endif






