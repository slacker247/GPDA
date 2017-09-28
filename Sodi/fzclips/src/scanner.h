   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  10/14/93            */
   /*                                                     */
   /*                 SCANNER HEADER FILE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Routines for scanning lexical tokens from an     */
/*   input source.                                           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_scanner
#define _H_scanner

struct token;

#ifndef _H_pprint
#include "pprint.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _SCANNER_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

struct token
  {
   int type;
   VOID *value;
   char *printForm;
  };

#define print_rep printForm

#if ANSI_COMPILER
   LOCALE VOID                           GetToken(char *,struct token *);
   LOCALE VOID                           CopyToken(struct token *,struct token *);
#if CERTAINTY_FACTORS
   LOCALE VOID                           UnGetToken(struct token *);
   LOCALE VOID                           ClearTheUnToken();
#endif
 
#else
   LOCALE VOID                           GetToken();
   LOCALE VOID                           CopyToken();
#if CERTAINTY_FACTORS
   LOCALE VOID                           UnGetToken();
   LOCALE VOID                           ClearTheUnToken();
#endif
 
#endif

#ifndef _SCANNER_SOURCE_
   extern int                            IgnoreCompletionErrors;
#endif
#endif




