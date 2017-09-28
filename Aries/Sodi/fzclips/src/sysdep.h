   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  10/18/93            */
   /*                                                     */
   /*            SYSTEM DEPENDENT HEADER FILE             */
   /*******************************************************/

/*************************************************************/
/* Purpose: Isolation of system dependent routines.          */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_sysdep
#define _H_sysdep

#if IBM_TBC || IBM_MSC || IBM_ICB
#include <dos.h>
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _SYSDEP_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ANSI_COMPILER
   LOCALE VOID                        InitializeCLIPS(void);
   LOCALE VOID                        SetRedrawFunction(void (*)(void));
   LOCALE VOID                        SetPauseEnvFunction(void (*)(void));
   LOCALE VOID                        SetContinueEnvFunction(void (*)(int));
   LOCALE VOID                        RerouteStdin(int,char *[]);
   LOCALE double                      gentime(void);
   LOCALE VOID                        gensystem(void);
   LOCALE VOID                        VMSSystem(char *);
   LOCALE int                         GenOpen(char *,char *);
   LOCALE VOID                        GenSeek(long);
   LOCALE VOID                        GenClose(void);
   LOCALE VOID                        GenRead(VOID *,unsigned long);
   LOCALE VOID                        genexit(int);
   LOCALE int                         genrand(void);
   LOCALE VOID                        genseed(int);
   LOCALE int                         genremove(char *);
   LOCALE int                         genrename(char *,char *);
#else
   LOCALE VOID                        InitializeCLIPS();
   LOCALE VOID                        SetRedrawFunction();
   LOCALE VOID                        SetPauseEnvFunction();
   LOCALE VOID                        SetContinueEnvFunction();
   LOCALE VOID                        RerouteStdin();
   LOCALE double                      gentime();
   LOCALE VOID                        gensystem();
   LOCALE VOID                        VMSSystem();
   LOCALE int                         GenOpen();
   LOCALE VOID                        GenSeek();
   LOCALE VOID                        GenClose();
   LOCALE VOID                        GenRead();
   LOCALE VOID                        genexit();
   LOCALE int                         genrand();
   LOCALE VOID                        genseed();
   LOCALE int                         genremove();
   LOCALE int                         genrename();
#endif

#ifndef _SYSDEP_SOURCE_
#if ANSI_COMPILER
   extern VOID                    (*RedrawScreenFunction)(void);
   extern VOID                    (*PauseEnvFunction)(void);
   extern VOID                    (*ContinueEnvFunction)(int);
#else
   extern VOID                    (*RedrawScreenFunction)();
   extern VOID                    (*PauseEnvFunction)();
   extern VOID                    (*ContinueEnvFunction)();
#endif
#endif

#endif
 




