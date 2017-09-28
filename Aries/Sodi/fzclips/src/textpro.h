   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00 10/18/93             */
   /*                                                     */
   /*             TEXT PROCESSING HEADER FILE             */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_textpro

#define _H_textpro

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _TEXTPRO_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ANSI_COMPILER
#if CLP_TEXTPRO
   LOCALE VOID                           FetchCommand(DATA_OBJECT *);
   LOCALE int                            PrintRegionCommand(void);
   int                                   TossCommand(void);
#endif

#if CLP_HELP
   LOCALE VOID                           HelpFunction(void);
   LOCALE VOID                           HelpPathFunction(void);
#endif

   LOCALE VOID                           HelpFunctionDefinitions(void);
#else
#if CLP_TEXTPRO
   LOCALE VOID                           FetchCommand();
   LOCALE int                            PrintRegionCommand();
   int                                   TossCommand();
#endif

#if CLP_HELP
   LOCALE VOID                           HelpFunction();
   LOCALE VOID                           HelpPathFunction();
#endif

   LOCALE VOID                           HelpFunctionDefinitions();
#endif
#endif






