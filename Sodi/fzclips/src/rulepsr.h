   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  03/08/94            */
   /*                                                     */
   /*               RULE PARSING HEADER FILE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Coordinates parsing of a rule.                   */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_rulepsr
#define _H_rulepsr

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _RULEPSR_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif
  
#if ANSI_COMPILER
   LOCALE int                            ParseDefrule(char *);
   LOCALE struct lhsParseNode           *FindVariable(struct symbolHashNode *,
                                                      struct lhsParseNode *);
#else
   LOCALE int                            ParseDefrule();
   LOCALE struct lhsParseNode           *FindVariable();
#endif

#endif




