   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00 10/18/93             */
   /*                                                     */
   /*               I/O FUNCTIONS HEADER FILE             */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_iofun

#define _H_iofun

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _IOFUN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ANSI_COMPILER
   LOCALE VOID                           IOFunctionDefinitions(void);
#if BASIC_IO
   LOCALE VOID                           PrintoutFunction(void);
   LOCALE VOID                           ReadFunction(DATA_OBJECT_PTR);
   LOCALE int                            OpenFunction(void);
   LOCALE int                            CloseFunction(void);
#endif
#if EXT_IO
   LOCALE VOID                           ReadlineFunction(DATA_OBJECT_PTR);
   LOCALE VOID                          *FormatFunction(void);
   LOCALE int                            RemoveFunction(void);
   LOCALE int                            RenameFunction(void);
#endif
#else
   LOCALE VOID                           IOFunctionDefinitions();
#if BASIC_IO
   LOCALE VOID                           PrintoutFunction();
   LOCALE VOID                           ReadFunction();
   LOCALE int                            OpenFunction();
   LOCALE int                            CloseFunction();
#endif
#if EXT_IO
   LOCALE VOID                           ReadlineFunction();
   LOCALE VOID                          *FormatFunction();
   LOCALE int                            RemoveFunction();
   LOCALE int                            RenameFunction();
#endif
#endif

#endif






