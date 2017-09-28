
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  12/30/93            */
   /*                                                     */
   /*                 FILE COMMANDS MODULE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for file commands including    */
/*   batch, dribble-on, dribble-off, save, load, bsave, and  */
/*   bload.                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Bebe Ly                                              */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#define _FILECOM_SOURCE_

#include <stdio.h>

#define _CLIPS_STDIO_
#include <string.h>

#include "setup.h"

#include "clipsmem.h"
#include "argacces.h"
#include "router.h"
#include "strngrtr.h"
#include "constrct.h"
#include "extnfunc.h"
#include "cstrcpsr.h"
#include "utility.h"
#include "commline.h"   /* added 03-07-96 */
#include "prcdrfun.h"   /* added 03-07-96 */
#include "filecom.h"

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
#include "bsave.h"
#include "bload.h"
#endif

/***************/
/* STRUCTURES  */
/***************/  

struct batchEntry
  {
   int batchType;
   VOID *inputSource;
   char *theString;
   struct batchEntry *next;
  };

/***************/
/* DEFINITIONS */
/***************/  

#define FILE_BATCH      0
#define STRING_BATCH    1

#define BUFFER_SIZE   120   
       
/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if ANSI_COMPILER
#if DEBUGGING_FUNCTIONS
   static int                     FindDribble(char *);
   static int                     GetcDribble(char *);
   static int                     UngetcDribble(int,char *);
   static int                     ExitDribble(int);
   static int                     PrintDribble(char *,char *);
   static VOID                    PutcDribbleBuffer(int);
#endif
   static int                     FindBatch(char *);
   static int                     GetcBatch(char *);
   static int                     UngetcBatch(int,char *);
   static int                     ExitBatch(int);
   static VOID                    AddBatch(int,VOID *,int,char *);
#else
#if DEBUGGING_FUNCTIONS
   static int                     PrintDribble();
   static VOID                    PutcDribbleBuffer();
   static int                     FindDribble();
   static int                     GetcDribble();
   static int                     UngetcDribble();
   static int                     ExitDribble();
#endif
   static int                     FindBatch();
   static int                     GetcBatch();
   static int                     UngetcBatch();
   static int                     ExitBatch();
   static VOID                    AddBatch();
#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

#if DEBUGGING_FUNCTIONS
   static FILE               *DribbleFP = NULL;
   static char               *DribbleBuffer;
   static int                 DribbleCurrentPosition = 0;
   static int                 DribbleMaximumPosition = 0;
#if ANSI_COMPILER
   static int               (*DribbleStatusFunction)(int) = NULL;
#else
   static int               (*DribbleStatusFunction)() = NULL;
#endif
#endif
   static int                 BatchType;
   static VOID               *BatchSource = NULL;
   static char               *BatchBuffer;
   static int                 BatchCurrentPosition = 0;  /* changed 03-07-96 */
   static int                 BatchMaximumPosition = 0;  /* changed 03-07-96 */
   static struct batchEntry  *TopOfBatchList = NULL;     /* changed 03-07-96 */
   static struct batchEntry  *BottomOfBatchList = NULL;  /* changed 03-07-96 */

/***************************************/
/* FileCommandDefinitions: Initializes */
/*   file commands.                    */
/***************************************/
#if ! RUN_TIME
globle VOID FileCommandDefinitions()
  {
#if DEBUGGING_FUNCTIONS
   DefineFunction2("batch",'b',PTIF BatchCommand,"BatchCommand","11k");
   DefineFunction2("batch*",'b',PTIF BatchStarCommand,"BatchStarCommand","11k");
   DefineFunction2("dribble-on",'b',PTIF DribbleOnCommand,"DribbleOnCommand","11k");
   DefineFunction2("dribble-off",'b',PTIF DribbleOffCommand,"DribbleOffCommand","00");
   DefineFunction2("save",'b',PTIF SaveCommand,"SaveCommand","11k");
#endif
   DefineFunction2("load",'b',PTIF LoadCommand,"LoadCommand","11k");
   DefineFunction2("load*",'b',PTIF LoadStarCommand,"LoadStarCommand","11k");
#if BLOAD_AND_BSAVE
   DefineFunction2("bsave",'b', PTIF BsaveCommand,"BsaveCommand","11k");
#endif
#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
   DefineFunction2("bload",'b',PTIF BloadCommand,"BloadCommand","11k");
#endif
  }
#endif

#if DEBUGGING_FUNCTIONS
/*****************************************************/
/* FindDribble: Find routine for the dribble router. */
/*****************************************************/
static int FindDribble(logicalName)
  char *logicalName;
  {
   if ( (strcmp(logicalName,"stdout") == 0) ||
        (strcmp(logicalName,"stdin") == 0) ||
        (strcmp(logicalName,WCLIPS) == 0) ||
        (strcmp(logicalName,WTRACE) == 0) ||
        (strcmp(logicalName,WERROR) == 0) ||
        (strcmp(logicalName,WWARNING) == 0) ||
        (strcmp(logicalName,WDISPLAY) == 0) ||
        (strcmp(logicalName,WDIALOG) == 0) )
     { return(CLIPS_TRUE); }

    return(CLIPS_FALSE);
  }

/*******************************************************/
/* PrintDribble: Print routine for the dribble router. */
/*******************************************************/
static int PrintDribble(logicalName,str)
  char *logicalName, *str;
  {
   int i;

   /*======================================*/
   /* Send the output to the dribble file. */
   /*======================================*/

   for (i = 0 ; str[i] != EOS ; i++)
     { PutcDribbleBuffer(str[i]); }

   /*===========================================================*/
   /* Send the output to any routers interested in printing it. */
   /*===========================================================*/

   DeactivateRouter("dribble");
   PrintCLIPS(logicalName,str);
   ActivateRouter("dribble");
   
   return(1);
  }

/*****************************************************/
/* GetcDribble: Getc routine for the dribble router. */
/*****************************************************/
static int GetcDribble(logicalName)
  char *logicalName;
  {
   int rv;

   /*===========================================*/
   /* Deactivate the dribble router and get the */
   /* character from another active router.     */
   /*===========================================*/
   
   DeactivateRouter("dribble");
   rv = GetcCLIPS(logicalName);
   ActivateRouter("dribble");

   /*==========================================*/
   /* Put the character retrieved from another */
   /* router into the dribble buffer.          */
   /*==========================================*/
   
   PutcDribbleBuffer(rv);

   /*=======================*/
   /* Return the character. */
   /*=======================*/
   
   return(rv);
  }

/***********************************************************/
/* PutcDribbleBuffer: Putc routine for the dribble router. */
/***********************************************************/
static VOID PutcDribbleBuffer(rv)
  int rv;
  {
   /*===================================================*/
   /* Receiving an end-of-file character will cause the */
   /* contents of the dribble buffer to be flushed.     */
   /*===================================================*/
   
   if (rv == EOF)
     {
      if (DribbleCurrentPosition > 0)
        {
         fprintf(DribbleFP,"%s",DribbleBuffer);
         DribbleCurrentPosition = 0;
         DribbleBuffer[0] = EOS;
        }
     }
   
   /*===========================================================*/
   /* If we aren't receiving command input, then the character  */
   /* just received doesn't need to be placed in the dribble    */
   /* buffer--It can be written directly to the file. This will */
   /* occur for example when the "CLIPS>" prompt is being       */
   /* printed (the CLIPSInputCount variable will be -1 because  */
   /* command input has not been receivied yet). Before writing */
   /* the character to the file, the dribble buffer is flushed. */
   /*===========================================================*/
     
   else if (CLIPSInputCount < 0)
     {
      if (DribbleCurrentPosition > 0)
        {
         fprintf(DribbleFP,"%s",DribbleBuffer);
         DribbleCurrentPosition = 0;
         DribbleBuffer[0] = EOS;
        }

      fputc(rv,DribbleFP);
     }
   
   /*=====================================================*/
   /* Otherwise, add the character to the dribble buffer. */
   /*=====================================================*/
   
   else
     {
      DribbleBuffer = ExpandStringWithChar(rv,DribbleBuffer,
                                           &DribbleCurrentPosition,
                                           &DribbleMaximumPosition,
                                           DribbleMaximumPosition+BUFFER_SIZE);
     }
  }

/*********************************************************/
/* UngetcDribble: Ungetc routine for the dribble router. */
/*********************************************************/
static int UngetcDribble(ch,logicalName)
  int ch;
  char *logicalName;
  {
   int rv;

   /*===============================================*/
   /* Remove the character from the dribble buffer. */
   /*===============================================*/
   
   if (DribbleCurrentPosition > 0) DribbleCurrentPosition--;
   DribbleBuffer[DribbleCurrentPosition] = EOS;
   
   /*=============================================*/
   /* Deactivate the dribble router and pass the  */
   /* ungetc request to the other active routers. */
   /*=============================================*/
   
   DeactivateRouter("dribble");
   rv = UngetcCLIPS(ch,logicalName);
   ActivateRouter("dribble");

   /*==========================================*/
   /* Return the result of the ungetc request. */
   /*==========================================*/
   
   return(rv);
  }

/*****************************************************/
/* ExitDribble: Exit routine for the dribble router. */
/*****************************************************/
#if IBM_TBC
#pragma argsused
#endif
static int ExitDribble(num)
  int num;
  {
#if MAC_MPW || MAC_MCW   /* added 03-07-96 */
#pragma unused(num)
#endif
   if (DribbleCurrentPosition > 0)
     { fprintf(DribbleFP,"%s",DribbleBuffer); }

   if (DribbleFP != NULL) fclose(DribbleFP);
   return(1);
  }

/******************************************/
/* DribbleOnCommand: CLIPS access routine */
/*   for the dribble-on command.          */
/******************************************/
globle int DribbleOnCommand()
  {
   char *fileName;

   if (ArgCountCheck("dribble-on",EXACTLY,1) == -1) return(CLIPS_FALSE);
   if ((fileName = GetFileName("dribble-on",1)) == NULL) return(CLIPS_FALSE);

   return (DribbleOn(fileName));
  }

/***********************************/
/* DribbleOn: C access routine for */
/*   the dribble-on command.       */
/***********************************/
globle BOOLEAN DribbleOn(fileName)
  char *fileName;
  {
   /*==============================*/
   /* If a dribble file is already */
   /* open, then close it.         */
   /*==============================*/
                       
   if (DribbleFP != NULL)
     { DribbleOff(); }

   /*========================*/
   /* Open the dribble file. */
   /*========================*/
   
   DribbleFP = fopen(fileName,"w");
   if (DribbleFP == NULL)
     {
      OpenErrorMessage("dribble-on",fileName);
      return(0);
     }

   /*============================*/
   /* Create the dribble router. */
   /*============================*/
   
   AddRouter("dribble", 40,
             FindDribble, PrintDribble,
             GetcDribble, UngetcDribble,  
             ExitDribble);  

   DribbleCurrentPosition = 0;

   /*================================================*/
   /* Call the dribble status function. This is used */
   /* by some of the machine specific interfaces to  */
   /* do things such as changing the wording of menu */
   /* items from "Turn Dribble On..." to             */
   /* "Turn Dribble Off..."                          */
   /*================================================*/
   
   if (DribbleStatusFunction != NULL)
     { (*DribbleStatusFunction)(CLIPS_TRUE); }

   /*=====================================*/
   /* Return TRUE to indicate the dribble */
   /* file was successfully opened.       */
   /*=====================================*/
   
   return(CLIPS_TRUE);
  }

/**********************************************/
/* DribbleActive: Returns TRUE if the dribble */
/*   router is active, otherwise FALSE>       */
/**********************************************/
globle BOOLEAN DribbleActive()
  {
   if (DribbleFP != NULL) return(CLIPS_TRUE);

   return(CLIPS_FALSE);
  }

/*******************************************/
/* DribbleOffCommand: CLIPS access routine */
/*   for the dribble-off command.          */
/*******************************************/
globle int DribbleOffCommand()
  {
   if (ArgCountCheck("dribble-off",EXACTLY,0) == -1) return(CLIPS_FALSE);
   return(DribbleOff());
  }

/************************************/
/* DribbleOff: C access routine for */
/*   the dribble-off command.       */
/************************************/
globle BOOLEAN DribbleOff()
  {
   int rv = 0;
   
   /*================================================*/
   /* Call the dribble status function. This is used */
   /* by some of the machine specific interfaces to  */
   /* do things such as changing the wording of menu */
   /* items from "Turn Dribble On..." to             */
   /* "Turn Dribble Off..."                          */
   /*================================================*/

   if (DribbleStatusFunction != NULL)
     { (*DribbleStatusFunction)(CLIPS_FALSE); }

   /*=======================================*/
   /* Close the dribble file and deactivate */
   /* the dribble router.                   */
   /*=======================================*/
   
   if (DribbleFP != NULL)
     {
      if (DribbleCurrentPosition > 0)
        { fprintf(DribbleFP,"%s",DribbleBuffer); }
      DeleteRouter("dribble");
      if (fclose(DribbleFP) == 0) rv = 1;
     }
   else
     { rv = 1; }

   DribbleFP = NULL;

   /*============================================*/
   /* Free the space used by the dribble buffer. */
   /*============================================*/
   
   if (DribbleBuffer != NULL)
     {
      rm(DribbleBuffer,DribbleMaximumPosition);
      DribbleBuffer = NULL;
     }

   DribbleCurrentPosition = 0;
   DribbleMaximumPosition = 0;

   /*============================================*/
   /* Return TRUE if the dribble file was closed */
   /* without error, otherwise return FALSE.     */
   /*============================================*/
   
   return(rv);
  }

/*****************************************************/
/* SetDribbleStatusFunction: Sets the function which */
/*   is called whenever the dribble router is turned */
/*   on or off.                                      */
/*****************************************************/
globle VOID SetDribbleStatusFunction(fnptr)
#if ANSI_COMPILER
  int (*fnptr)(int);
#else
  int (*fnptr)();
#endif
  {
   DribbleStatusFunction = fnptr;
  }
#endif

/*************************************************/
/* FindBatch: Find routine for the batch router. */
/*************************************************/
static int FindBatch(logicalName)
  char *logicalName;
  {
   if (strcmp(logicalName,"stdin") == 0)
     { return(CLIPS_TRUE); }

   return(CLIPS_FALSE);
  }

/*************************************************/
/* GetcBatch: Getc routine for the batch router. */
/*************************************************/
static int GetcBatch(logicalName)
  char *logicalName;
  {
   return(LLGetcBatch(logicalName,CLIPS_FALSE));
  }

/***************************************************/
/* LLGetcBatch: Lower level routine for retrieving */
/*   a character when a batch file is active.      */
/***************************************************/
globle int LLGetcBatch(logicalName,returnOnEOF)
  char *logicalName;
  int returnOnEOF;
  {
   int rv = EOF, flag = 1;

   /*=================================================*/
   /* Get a character until a valid character appears */
   /* or no more batch files are left.                */
   /*=================================================*/

   while ((rv == EOF) && (flag == 1))
     {
      if (BatchType == FILE_BATCH)
        { rv = getc((FILE *) BatchSource); }
      else
        { rv = GetcCLIPS((char *) BatchSource); }

      if (rv == EOF)
        {
         if (BatchCurrentPosition > 0) PrintCLIPS("stdout",(char *) BatchBuffer);
         flag = RemoveBatch();
        }
     }

   /*=========================================================*/
   /* If the character retrieved is an end-of-file character, */
   /* then there are no batch files with character input      */
   /* remaining. Remove the batch router.                     */
   /*=========================================================*/
   
   if (rv == EOF)
     {
      if (BatchCurrentPosition > 0) PrintCLIPS("stdout",(char *) BatchBuffer);
      DeleteRouter("batch");
      RemoveBatch();
      if (returnOnEOF == CLIPS_TRUE)
        { return (EOF); }
      else
        { return(GetcCLIPS(logicalName)); }
     }

   /*========================================*/
   /* Add the character to the batch buffer. */
   /*========================================*/
   
   BatchBuffer = ExpandStringWithChar((char) rv,BatchBuffer,&BatchCurrentPosition,
                                      &BatchMaximumPosition,BatchMaximumPosition+BUFFER_SIZE);

   /*======================================*/
   /* If a carriage return is encountered, */
   /* then flush the batch buffer.         */
   /*======================================*/
   
   if ((char) rv == '\n')
     {
      PrintCLIPS("stdout",(char *) BatchBuffer);
      BatchCurrentPosition = 0;
      if ((BatchBuffer != NULL) && (BatchMaximumPosition > BUFFER_SIZE))
        {
         rm(BatchBuffer,BatchMaximumPosition);
         BatchMaximumPosition = 0;
         BatchBuffer = NULL;
        }
     }

   /*=====================================================*/
   /* Return the character retrieved from the batch file. */
   /*=====================================================*/
   
   return(rv);
  }

/*****************************************************/
/* UngetcBatch: Ungetc routine for the batch router. */
/*****************************************************/
#if IBM_TBC
#pragma argsused
#endif
static int UngetcBatch(ch,logicalName)
  int ch;
  char *logicalName;
  {
#if MAC_MPW || MAC_MCW      /* added 03-07-96 */
#pragma unused(logicalName)
#endif
   if (BatchCurrentPosition > 0) BatchCurrentPosition--;
   if (BatchBuffer != NULL) BatchBuffer[BatchCurrentPosition] = EOS;
   if (BatchType == FILE_BATCH)
     { return(ungetc(ch,(FILE *) BatchSource)); }

   return(UngetcCLIPS(ch,(char *) BatchSource));
  }

/*************************************************/
/* ExitBatch: Exit routine for the batch router. */
/*************************************************/
#if IBM_TBC
#pragma argsused
#endif
static int ExitBatch(num)
  int num;
  {
#if MAC_MPW || MAC_MCW    /* added 03-07-96 */
#pragma unused(num)
#endif
   CloseAllBatchSources();
   return(1);
  }

/**************************************/
/* BatchCommand: CLIPS access routine */
/*   for the batch command.           */
/**************************************/
globle int BatchCommand()
  {
   char *fileName;

   if (ArgCountCheck("batch",EXACTLY,1) == -1) return(CLIPS_FALSE);
   if ((fileName = GetFileName("batch",1)) == NULL) return(CLIPS_FALSE);

   return(OpenBatch(fileName,CLIPS_FALSE));
  }
  
/**************************************************/
/* Batch: C access routine for the batch command. */
/**************************************************/
globle int Batch(fileName)
  char *fileName;
  { return(OpenBatch(fileName,CLIPS_FALSE)); }

/***********************************************/
/* OpenBatch: Adds a file to the list of files */
/*   opened with the batch command.            */
/***********************************************/
globle int OpenBatch(fileName,placeAtEnd)
  char *fileName;
  int placeAtEnd;
  {
   FILE *theFile;

   /*======================*/
   /* Open the batch file. */
   /*======================*/
   
   theFile = fopen(fileName,"r");

   if (theFile == NULL)
     {
      OpenErrorMessage("batch",fileName);
      return(CLIPS_FALSE);
     }

   /*============================*/
   /* Create the batch router if */
   /* it doesn't already exist.  */
   /*============================*/
   
   if (TopOfBatchList == NULL)
     {
      AddRouter("batch", 20,                
                 FindBatch, NULL,              
                 GetcBatch, UngetcBatch,       
                 ExitBatch);       
     }

   /*====================================*/
   /* Add the newly opened batch file to */
   /* the list of batch files opened.    */
   /*====================================*/
   
   AddBatch(placeAtEnd,(VOID *) theFile,FILE_BATCH,NULL);

   /*===================================*/
   /* Return TRUE to indicate the batch */
   /* file was successfully opened.     */
   /*===================================*/
   
   return(CLIPS_TRUE);
  }

/*****************************************************************/
/* OpenStringBatch: Opens a string source for batch processing.  */
/*   The memory allocated for the argument stringName must be    */
/*   deallocated by the user. The memory allocated for theString */
/*   will be deallocated by the batch routines when batch        */
/*   processing for the  string is completed.                    */
/*****************************************************************/
globle int OpenStringBatch(stringName,theString,placeAtEnd)
  char *stringName;
  char *theString;
  int placeAtEnd;
  {
   if (OpenStringSource(stringName,theString,0) == 0)
     { return(0); }

   if (TopOfBatchList == NULL)
     {
      AddRouter("batch", 20,              
                 FindBatch, NULL,            
                 GetcBatch, UngetcBatch,       
                 ExitBatch);      
     }

   AddBatch(placeAtEnd,(VOID *) stringName,STRING_BATCH,theString);

   return(1);
  }

/*******************************************************/
/* AddBatch: Creates the batch file data structure and */
/*   adds it to the list of opened batch files.        */
/*******************************************************/
static VOID AddBatch(placeAtEnd,theSource,type,theString)
  int placeAtEnd;
  VOID *theSource;
  int type;
  char *theString;
  {
   struct batchEntry *bptr;

   /*=========================*/
   /* Create the batch entry. */
   /*=========================*/

   bptr = get_struct(batchEntry);
   bptr->batchType = type;
   bptr->inputSource = theSource;
   bptr->theString = theString;
   bptr->next = NULL;

   /*============================*/
   /* Add the entry to the list. */
   /*============================*/
   
   if (TopOfBatchList == NULL)
     {
      TopOfBatchList = bptr;
      BottomOfBatchList = bptr;
      BatchType = type;
      BatchSource = theSource;
      BatchCurrentPosition = 0;
     }
   else if (placeAtEnd == CLIPS_FALSE)
     {
      bptr->next = TopOfBatchList;
      TopOfBatchList = bptr;
      BatchType = type;
      BatchSource = theSource;
      BatchCurrentPosition = 0;
     }
   else
     {
      BottomOfBatchList->next = bptr;
      BottomOfBatchList = bptr;
     }
  }

/******************************************************************/
/* RemoveBatch: Removes the top entry on the list of batch files. */
/******************************************************************/
globle int RemoveBatch()
  {
   struct batchEntry *bptr;
   int rv;

   if (TopOfBatchList == NULL) return(CLIPS_FALSE);

   /*==================================================*/
   /* Close the source from which batch input is read. */
   /*==================================================*/
   
   if (TopOfBatchList->batchType == FILE_BATCH)
     { fclose((FILE *) TopOfBatchList->inputSource); }
   else
     {
      CloseStringSource((char *) TopOfBatchList->inputSource);
      rm(TopOfBatchList->theString,(int) strlen(TopOfBatchList->theString) + 1);
     }

   /*=================================*/
   /* Remove the entry from the list. */
   /*=================================*/
   
   bptr = TopOfBatchList;
   TopOfBatchList = TopOfBatchList->next;

   rtn_struct(batchEntry,bptr);

   /*========================================================*/
   /* If there are no batch files remaining to be processed, */
   /* then free the space used by the batch buffer.          */
   /*========================================================*/
   
   if (TopOfBatchList == NULL)
     {
      BottomOfBatchList = NULL;
      BatchSource = NULL;
      if (BatchBuffer != NULL)
        {
         rm(BatchBuffer,BatchMaximumPosition);
         BatchBuffer = NULL;
        }
      BatchCurrentPosition = 0;
      BatchMaximumPosition = 0;
      rv = 0;
     }
   
   /*===========================================*/
   /* Otherwise move on to the next batch file. */
   /*===========================================*/
   
   else
     {
      BatchType = TopOfBatchList->batchType;
      BatchSource = TopOfBatchList->inputSource;
      BatchCurrentPosition = 0;
      rv = 1;
     }

   /*====================================================*/
   /* Return TRUE if a batch file if there are remaining */
   /* batch files to be processed, otherwise FALSE.      */
   /*====================================================*/
   
   return(rv);
  }

/****************************************/
/* BatchActive: Returns TRUE if a batch */
/*   file is open, otherwise FALSE.     */
/****************************************/
globle BOOLEAN BatchActive()
  {
   if (TopOfBatchList != NULL) return(CLIPS_TRUE);

   return(CLIPS_FALSE);
  }

/******************************************************/
/* CloseAllBatchSources: Closes all open batch files. */
/******************************************************/
globle VOID CloseAllBatchSources()
  {
   /*================================================*/
   /* Free the batch buffer if it contains anything. */
   /*================================================*/
   
   if (BatchBuffer != NULL)
     {
      if (BatchCurrentPosition > 0) PrintCLIPS("stdout",(char *) BatchBuffer);
      rm(BatchBuffer,BatchMaximumPosition);
      BatchBuffer = NULL;
      BatchCurrentPosition = 0;
      BatchMaximumPosition = 0;
     }

   /*==========================*/
   /* Delete the batch router. */
   /*==========================*/
   
   DeleteRouter("batch");
   
   /*=====================================*/
   /* Close each of the open batch files. */
   /*=====================================*/
   
   while (RemoveBatch())
     { /* Do Nothing */ }
  }
  
/******************************************/
/* BatchStarCommand: CLIPS access routine */
/*   for the batch* command.              */
/******************************************/
globle int BatchStarCommand()   /* added 03-07-96 */
  {
   char *fileName;

   if (ArgCountCheck("batch*",EXACTLY,1) == -1) return(CLIPS_FALSE);
   if ((fileName = GetFileName("batch*",1)) == NULL) return(CLIPS_FALSE);

   return(BatchStar(fileName));
  }

#if ! RUN_TIME
  
/*******************************************************/
/* BatchStar: C access routine for the batch* command. */
/*******************************************************/
globle int BatchStar(fileName)
  char *fileName;
  {
   int inchar;
   FILE *theFile;
   char *theString = NULL;
   int position = 0;
   int maxChars = 0;

   /*======================*/
   /* Open the batch file. */
   /*======================*/
   
   theFile = fopen(fileName,"r");
   
   if (theFile == NULL)
     {
      OpenErrorMessage("batch",fileName);
      return(CLIPS_FALSE);
     }
     
   /*========================*/
   /* Reset the error state. */
   /*========================*/
   
   SetHaltExecution(CLIPS_FALSE);
   SetEvaluationError(CLIPS_FALSE);
   
   /*=============================================*/
   /* Evaluate commands from the file one by one. */
   /*=============================================*/
   
   while ((inchar = getc(theFile)) != EOF)
     {
      theString = ExpandStringWithChar(inchar,theString,&position,
                                       &maxChars,maxChars+80);
     
      if (CompleteCommand(theString) != 0)
        {
         FlushPPBuffer();
         SetPPBufferStatus(OFF);
         RouteCommand(theString,CLIPS_FALSE);
         FlushPPBuffer();
         SetHaltExecution(CLIPS_FALSE);
         SetEvaluationError(CLIPS_FALSE);
         FlushBindList();
         genfree(theString,(unsigned) maxChars);
         theString = NULL;
         maxChars = 0;
         position = 0;
        }
     }
     
   /*=======================*/
   /* Close the batch file. */
   /*=======================*/
   
   fclose(theFile);
   return(CLIPS_TRUE);
  }
  
#else

/*******************************************************/
/* BatchStar: This is the non-functional stub provided */
/*   for use with a run-time version of CLIPS.         */
/*******************************************************/
globle int BatchStar(fileName)
  char *fileName;
  {
#if (MAC_MPW || MAC_MCW) && RUN_TIME
#pragma unused(fileName)
#endif

   PrintErrorID("FILECOM",1,CLIPS_FALSE);
   PrintCLIPS(WERROR,"Function batch* does not work in run time modules.\n");
   return(CLIPS_FALSE);
  }
  
#endif
       
/***********************************************************/
/* LoadCommand: CLIPS access routine for the load command. */
/***********************************************************/
globle int LoadCommand()
  {
#if (! BLOAD_ONLY) && (! RUN_TIME)
   char *theFileName;
   int rv;

   if (ArgCountCheck("load",EXACTLY,1) == -1) return(CLIPS_FALSE);
   if ((theFileName = GetFileName("load",1)) == NULL) return(CLIPS_FALSE);
   
   SetPrintWhileLoading(CLIPS_TRUE);
   
   if ((rv = Load(theFileName)) == CLIPS_FALSE)
     {
      SetPrintWhileLoading(CLIPS_FALSE);
      OpenErrorMessage("load",theFileName);
      return(CLIPS_FALSE);
     }

   SetPrintWhileLoading(CLIPS_FALSE);
   if (rv == -1) return(CLIPS_FALSE);
   return(CLIPS_TRUE);
#else
   PrintCLIPS(WDIALOG,"Load is not available in this environment\n");
   return(CLIPS_FALSE);
#endif
  }

/****************************************************************/
/* LoadStarCommand: CLIPS access routine for the load* command. */
/****************************************************************/
globle int LoadStarCommand() /* added 03-07-96 */
  {
#if (! BLOAD_ONLY) && (! RUN_TIME)
   char *theFileName;
   int rv;

   if (ArgCountCheck("load*",EXACTLY,1) == -1) return(CLIPS_FALSE);
   if ((theFileName = GetFileName("load*",1)) == NULL) return(CLIPS_FALSE);
   
   if ((rv = Load(theFileName)) == CLIPS_FALSE)
     {
      OpenErrorMessage("load*",theFileName);
      return(CLIPS_FALSE);
     }

   if (rv == -1) return(CLIPS_FALSE);
   return(CLIPS_TRUE);
#else
   PrintCLIPS(WDIALOG,"Load* is not available in this environment\n");
   return(CLIPS_FALSE);
#endif
  }
  
#if DEBUGGING_FUNCTIONS
/***********************************************************/
/* SaveCommand: CLIPS access routine for the save command. */
/***********************************************************/
globle int SaveCommand()
  {
#if (! BLOAD_ONLY) && (! RUN_TIME)
   char *theFileName;

   if (ArgCountCheck("save",EXACTLY,1) == -1) return(CLIPS_FALSE);
   if ((theFileName = GetFileName("save",1)) == NULL) return(CLIPS_FALSE);

   if (Save(theFileName) == CLIPS_FALSE)
     {
      OpenErrorMessage("save",theFileName);
      return(CLIPS_FALSE);
     }

   return(CLIPS_TRUE);
#else
   PrintCLIPS(WDIALOG,"Save is not available in this environment\n");
   return(CLIPS_FALSE);
#endif
  }
#endif



