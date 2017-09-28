   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00 12/30/93             */
   /*                                                     */
   /*              FILE COMMANDS HEADER FILE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for file commands including    */
/*   batch, dribble-on, dribble-off, save, load, bsave, and  */
/*   bload.                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_filecom

#define _H_filecom

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _FILECOM_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ANSI_COMPILER
   LOCALE VOID                           FileCommandDefinitions(void);
   LOCALE BOOLEAN                        DribbleOn(char *);
   LOCALE BOOLEAN                        DribbleActive(void);
   LOCALE BOOLEAN                        DribbleOff(void);
   LOCALE VOID                           SetDribbleStatusFunction(int (*)(int));
   LOCALE int                            LLGetcBatch(char *,int);
   LOCALE int                            Batch(char *);
   LOCALE int                            OpenBatch(char *,int);
   LOCALE int                            OpenStringBatch(char *,char *,int);
   LOCALE int                            RemoveBatch(void);
   LOCALE BOOLEAN                        BatchActive(void);
   LOCALE VOID                           CloseAllBatchSources(void);
   LOCALE int                            BatchCommand(void);
   LOCALE int                            BatchStarCommand(void); /*03-07-96*/
   LOCALE int                            BatchStar(char *);      /*03-07-96*/
   LOCALE int                            LoadCommand(void);
   LOCALE int                            LoadStarCommand(void);  /*03-07-96*/
   LOCALE int                            SaveCommand(void);
   LOCALE int                            DribbleOnCommand(void);
   LOCALE int                            DribbleOffCommand(void);
#else
   LOCALE VOID                           FileCommandDefinitions();
   LOCALE BOOLEAN                        DribbleOn();
   LOCALE BOOLEAN                        DribbleActive();
   LOCALE BOOLEAN                        DribbleOff();
   LOCALE VOID                           SetDribbleStatusFunction();
   LOCALE int                            LLGetcBatch();
   LOCALE int                            Batch();
   LOCALE int                            OpenBatch();
   LOCALE int                            OpenStringBatch();
   LOCALE int                            RemoveBatch();
   LOCALE BOOLEAN                        BatchActive();
   LOCALE VOID                           CloseAllBatchSources();
   LOCALE int                            BatchCommand();
   LOCALE int                            BatchStarCommand();
   LOCALE int                            BatchStar();
   LOCALE int                            LoadCommand();
   LOCALE int                            LoadStarCommand();
   LOCALE int                            SaveCommand();
   LOCALE int                            DribbleOnCommand();
   LOCALE int                            DribbleOffCommand();
#endif

#endif






