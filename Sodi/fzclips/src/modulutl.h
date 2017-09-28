   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  06/22/94            */
   /*                                                     */
   /*            DEFMODULE UTILITY HEADER FILE            */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides routines for parsing module/construct   */
/*   names and searching through modules for specific        */
/*   constructs.                                             */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_modulutl
#define _H_modulutl

#ifndef _H_symbol
#include "symbol.h"
#endif
#ifndef _H_moduldef
#include "moduldef.h"
#endif
  
#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _MODULUTL_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ANSI_COMPILER
   LOCALE int                            FindModuleSeparator(char *);
   LOCALE SYMBOL_HN                     *ExtractModuleName(int,char *);
   LOCALE SYMBOL_HN                     *ExtractConstructName(int,char *);
   LOCALE char                          *ExtractModuleAndConstructName(char *);
   LOCALE VOID                          *FindImportedConstruct(char *,struct defmodule *,
                                                               char *,int *,int,struct defmodule *);
   LOCALE VOID                           AmbiguousReferenceErrorMessage(char *,char *);
   LOCALE VOID                           MarkModulesAsUnvisited(void);
   LOCALE VOID                           ListItemsDriver(char *,struct defmodule *,
                                                         char *,char *,
                                                          VOID *(*)(VOID *), 
                                                          char *(*)(VOID *), 
                                                          VOID (*)(char *,VOID *),
                                                          int (*)(VOID *));
#else
   LOCALE int                            FindModuleSeparator();
   LOCALE SYMBOL_HN                     *ExtractModuleName();
   LOCALE SYMBOL_HN                     *ExtractConstructName();
   LOCALE char                          *ExtractModuleAndConstructName();
   LOCALE VOID                          *FindImportedConstruct();
   LOCALE VOID                           AmbiguousReferenceErrorMessage();
   LOCALE VOID                           MarkModulesAsUnvisited();
   LOCALE VOID                           ListItemsDriver(); /* added 03-08-96 */
#endif
   
#endif



