   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  10/14/93            */
   /*                                                     */
   /*               PRETTY PRINT HEADER FILE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Routines for processing the pretty print         */
/*   representation of constructs.                           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_pprint
#define _H_pprint

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _PPRINT_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ANSI_COMPILER
   LOCALE VOID                           FlushPPBuffer(void);
   LOCALE VOID                           DestroyPPBuffer(void);
   LOCALE VOID                           SavePPBuffer(char *);
   LOCALE VOID                           PPBackup(void);
   LOCALE char                          *CopyPPBuffer(void);
   LOCALE char                          *GetPPBuffer(void);
   LOCALE VOID                           PPCRAndIndent(void);
   LOCALE VOID                           IncrementIndentDepth(int);  
   LOCALE VOID                           DecrementIndentDepth(int);  
   LOCALE VOID                           SetIndentDepth(int);  
   LOCALE VOID                           SetPPBufferStatus(int);
   LOCALE int                            GetPPBufferStatus(void);
#else 
   LOCALE VOID                           FlushPPBuffer();
   LOCALE VOID                           DestroyPPBuffer();
   LOCALE VOID                           SavePPBuffer();
   LOCALE VOID                           PPBackup();
   LOCALE char                          *CopyPPBuffer();
   LOCALE char                          *GetPPBuffer();
   LOCALE VOID                           PPCRAndIndent();
   LOCALE VOID                           IncrementIndentDepth();  
   LOCALE VOID                           DecrementIndentDepth();  
   LOCALE VOID                           SetIndentDepth();  
   LOCALE VOID                           SetPPBufferStatus();
   LOCALE int                            GetPPBufferStatus();
#endif

#endif




