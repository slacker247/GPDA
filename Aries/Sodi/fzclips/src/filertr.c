   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  02/25/94            */
   /*                                                     */
   /*               FILE I/O ROUTER MODULE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: I/O Router routines which allow files to be used */
/*   as input and output sources.                            */
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

#define _FILERTR_SOURCE_

#include <stdio.h>
#define _CLIPS_STDIO_
#include <string.h>

#include "setup.h"

#include "constant.h"
#include "clipsmem.h"
#include "router.h"

#include "filertr.h"

struct fileRouter
  {
   char *logicalName;
   FILE *stream;
   struct fileRouter *next;
  };

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if ANSI_COMPILER
   static int                     ExitFile(int);
   static int                     PrintFile(char *,char *);
   static int                     GetcFile(char *);
   static int                     UngetcFile(int,char *);
#else
   static int                     ExitFile();
   static int                     PrintFile();
   static int                     GetcFile();
   static int                     UngetcFile();
#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static struct fileRouter   *ListOfFileRouters = NULL;

/***************************************************************/
/* InitializeFileRouter: Initializes file input/output router. */
/***************************************************************/
globle VOID InitializeFileRouter()
  {
   AddRouter("fileio",0,FindFile,
             PrintFile,GetcFile,
             UngetcFile,ExitFile);
  }

/*****************************************/
/* FindFptr: Returns a pointer to a file */
/*   stream for a given logical name.    */
/*****************************************/
globle FILE *FindFptr(logicalName)
  char *logicalName;
  {
   struct fileRouter *fptr;

   /*========================================================*/
   /* Check to see if standard input or output is requested. */
   /*========================================================*/

   if (strcmp(logicalName,"stdout") == 0)
     { return(stdout); }
   else if (strcmp(logicalName,"stdin") == 0)
     { return(stdin); }
   else if (strcmp(logicalName,WTRACE) == 0)
     { return(stdout); }
   else if (strcmp(logicalName,WDIALOG) == 0)
     { return(stdout); }
   else if (strcmp(logicalName,WCLIPS) == 0)
     { return(stdout); }
   else if (strcmp(logicalName,WDISPLAY) == 0)
     { return(stdout); }
   else if (strcmp(logicalName,WERROR) == 0)
     { return(stdout); }
   else if (strcmp(logicalName,WWARNING) == 0)
     { return(stdout); }

   /*==============================================================*/
   /* Otherwise, look up the logical name on the global file list. */
   /*==============================================================*/

   fptr = ListOfFileRouters;
   while ((fptr != NULL) ? (strcmp(logicalName,fptr->logicalName) != 0) : CLIPS_FALSE)
     { fptr = fptr->next; }

   if (fptr != NULL) return(fptr->stream);

   return(NULL);
  }

/*****************************************************/
/* FindFile: Find routine for file router logical    */
/*   names. Returns TRUE if the specified logical    */
/*   name has an associated file stream (which means */
/*   that the logical name can be handled by the     */
/*   file router). Otherwise, FALSE is returned.     */
/*****************************************************/
globle int FindFile(logicalName)
  char *logicalName;
  {
   if (FindFptr(logicalName) != NULL) return(CLIPS_TRUE);

   return(CLIPS_FALSE);
  }

/********************************************/
/* ExitFile:  Exit routine for file router. */
/********************************************/
#if IBM_TBC
#pragma argsused
#endif
static int ExitFile(num)
  int num;
  {
#if MAC_MPW || MAC_MCW  /* added 03-07-96 */
#pragma unused(num)
#endif
#if BASIC_IO
   CloseAllFiles();
#endif
   return(1);
  }

/*********************************************/
/* PrintFile: Print routine for file router. */
/*********************************************/
static int PrintFile(logicalName,str)
  char *logicalName, *str;
  {
   FILE *fptr;

   fptr = FindFptr(logicalName);
   fprintf(fptr,"%s",str);
   fflush(fptr);
   return(1);
  }

/*******************************************/
/* GetcFile: Getc routine for file router. */
/*******************************************/
static int GetcFile(logicalName)
  char *logicalName;
  {
   FILE *fptr;
   int theChar;

   fptr = FindFptr(logicalName);

   theChar = getc(fptr);

   /*=================================================*/
   /* The following code prevents Control-D on UNIX   */
   /* machines from terminating all input from stdin. */
   /*=================================================*/
   
   if ((fptr == stdin) && (theChar == EOF)) clearerr(stdin);

   return(theChar);
  }

/***********************************************/
/* UngetcFile: Ungetc routine for file router. */
/***********************************************/
static int UngetcFile(ch,logicalName)
  int ch;
  char *logicalName;
  {
   FILE *fptr;

   fptr = FindFptr(logicalName);
   return(ungetc(ch,fptr));
  }

/*********************************************************/
/* OpenFile: Opens a file with the specified access mode */
/*   and stores the opened stream on the list of files   */
/*   associated with logical names Returns TRUE if the   */
/*   file was succesfully opened, otherwise FALSE.       */
/*********************************************************/
globle int OpenFile(fileName,accessMode,logicalName)
  char *fileName, *accessMode, *logicalName;
  {
   FILE *newstream;
   struct fileRouter *newRouter;

   /*==================================*/
   /* Make sure the file can be opened */
   /* with the specified access mode.  */
   /*==================================*/

   if ((newstream = fopen(fileName,accessMode)) == NULL) 
     { return(CLIPS_FALSE); }

   /*===========================*/
   /* Create a new file router. */
   /*===========================*/
   
   newRouter = get_struct(fileRouter); 
   newRouter->logicalName = (char *) gm2 ((int) strlen(logicalName) + 1);
   strcpy(newRouter->logicalName,logicalName);
   newRouter->stream = newstream;
   
   /*==========================================*/
   /* Add the newly opened file to the list of */
   /* files associated with logical names.     */
   /*==========================================*/

   newRouter->next = ListOfFileRouters;
   ListOfFileRouters = newRouter;

   /*==================================*/
   /* Return TRUE to indicate the file */
   /* was opened successfully.         */
   /*==================================*/
   
   return(CLIPS_TRUE);
  }

/*************************************************************/
/* CloseFile: Closes the file associated with the specified  */
/*   logical name. Returns TRUE if the file was successfully */ 
/*   closed, otherwise FALSE.                                */
/*************************************************************/
globle int CloseFile(fid)
  char *fid;
  {
   struct fileRouter *fptr, *prev;

   for (fptr = ListOfFileRouters, prev = NULL;
        fptr != NULL;
        fptr = fptr->next)
     {
      if (strcmp(fptr->logicalName,fid) == 0)
        {
         fclose(fptr->stream);
         rm(fptr->logicalName,(int) strlen(fptr->logicalName) + 1);
         if (prev == NULL)
           { ListOfFileRouters = fptr->next; }
         else
           { prev->next = fptr->next; }
         rm(fptr,(int) sizeof(struct fileRouter));

         return(CLIPS_TRUE);
        }

      prev = fptr;
     }

   return(CLIPS_FALSE);
  }

/**********************************************/
/* CloseAllFiles: Closes all files associated */
/*   with a file I/O router. Returns TRUE if  */
/*   any file was closed, otherwise FALSE.    */
/**********************************************/
globle int CloseAllFiles()
  {
   struct fileRouter *fptr, *prev;

   if (ListOfFileRouters == NULL) return(CLIPS_FALSE);

   fptr = ListOfFileRouters;

   while (fptr != NULL)
     {
      fclose(fptr->stream);
      prev = fptr;
      rm(fptr->logicalName,(int) strlen(fptr->logicalName) + 1);
      fptr = fptr->next;
      rm(prev,(int) sizeof(struct fileRouter));
     }

   ListOfFileRouters = NULL;

   return(CLIPS_TRUE);
  }



