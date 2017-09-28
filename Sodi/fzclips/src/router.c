   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  10/14/93            */
   /*                                                     */
   /*                  I/O ROUTER MODULE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides a centralized mechanism for handling    */
/*   input and output requests.                              */
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

#define _ROUTER_SOURCE_

#include <stdio.h>
#define _CLIPS_STDIO_
#include <string.h>

#include "setup.h"

#include "constant.h"
#include "clipsmem.h"
#include "filertr.h"
#include "strngrtr.h"
#include "extnfunc.h"
#include "argacces.h"
#include "sysdep.h"

#include "router.h"

struct router
  {
   char *name;
   int active;
   int priority;
#if ANSI_COMPILER
   int (*query)(char *);
   int (*printer)(char *,char *);
   int (*exiter)(int);
   int (*charget)(char *);
   int (*charunget)(int,char *);
#else
   int (*query)();
   int (*printer)();
   int (*exiter)();
   int (*charget)();
   int (*charunget)();
#endif
   struct router *next;
  };

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if ANSI_COMPILER
   static int                     QueryRouter(char *,struct router *);
#else
   static int                     QueryRouter();
#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static struct router       *ListOfRouters = NULL;
   static FILE                *FastLoadFilePtr = NULL;
   static FILE                *FastSaveFilePtr = NULL;
   static int                  Abort;

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   globle char                *WWARNING = "wwarning";
   globle char                *WERROR = "werror";
   globle char                *WTRACE = "wtrace";
   globle char                *WDIALOG = "wdialog";
   globle char                *WCLIPS  = "wclips";
   globle char                *WDISPLAY = "wdisplay";
   globle int                  CLIPSInputCount = -1;

/*********************************************************/
/* InitializeDefaultRouters: Initializes output streams. */
/*********************************************************/
globle VOID InitializeDefaultRouters()
  {
#if (! RUN_TIME)
   DefineFunction2("exit",    'v', PTIF ExitCommand,    "ExitCommand", "00");
#endif
   InitializeFileRouter();
   InitializeStringRouter();
  }

/***************************************/
/* PrintCLIPS: Generic print function. */
/***************************************/
globle int PrintCLIPS(logicalName, str)
  char *logicalName;
  char *str;
  {
   struct router *currentPtr;

   /*===================================================*/
   /* If the "fast save" option is being used, then the */
   /* logical name is actually a pointer to a file and  */
   /* fprintf can be called directly to bypass querying */
   /* all of the routers.                               */
   /*===================================================*/
   
   if (((char *) FastSaveFilePtr) == logicalName)
     {
      fprintf(FastSaveFilePtr,"%s",str);
      return(2);
     }

   /*==============================================*/
   /* Search through the list of routers until one */
   /* is found that will handle the print request. */
   /*==============================================*/
   
   currentPtr = ListOfRouters;
   while (currentPtr != NULL)
     {
      if ((currentPtr->printer != NULL) ? QueryRouter(logicalName,currentPtr) : CLIPS_FALSE)
        {
         (*currentPtr->printer) (logicalName,str);
         return(1);
        }
      currentPtr = currentPtr->next;
     }

   /*=====================================================*/
   /* The logical name was not recognized by any routers. */
   /*=====================================================*/
   
   if (strcmp(WERROR,logicalName) != 0) UnrecognizedRouterMessage(logicalName);
   return(0);
  }
  
/**********************************************/
/* GetcCLIPS: Generic get character function. */
/**********************************************/
globle int GetcCLIPS(logicalName)
  char *logicalName;
  {
   struct router *currentPtr;
   int inchar;

   /*===================================================*/
   /* If the "fast load" option is being used, then the */
   /* logical name is actually a pointer to a file and  */
   /* getc can be called directly to bypass querying    */
   /* all of the routers.                               */
   /*===================================================*/
   
   if (((char *) FastLoadFilePtr) == logicalName)
     {
      inchar = getc(FastLoadFilePtr);

      if (inchar == '\r') return('\n');

      if (inchar != '\b')
        { return(inchar); }

      return(inchar);
     }

   /*==============================================*/
   /* Search through the list of routers until one */
   /* is found that will handle the getc request.  */
   /*==============================================*/
   
   currentPtr = ListOfRouters;
   while (currentPtr != NULL)
     {
      if ((currentPtr->charget != NULL) ? QueryRouter(logicalName,currentPtr) : CLIPS_FALSE)
        {
         inchar = (*currentPtr->charget) (logicalName);

         if (inchar == '\r') return('\n');

         if (inchar != '\b')
           { return(inchar); }

         return(inchar);
        }
      currentPtr = currentPtr->next;
     }

   /*=====================================================*/
   /* The logical name was not recognized by any routers. */
   /*=====================================================*/
   
   UnrecognizedRouterMessage(logicalName);
   return(-1);
  }

/**************************************************/
/* UngetcCLIPS: Generic unget character function. */
/**************************************************/
globle int UngetcCLIPS(ch,logicalName)
  int ch;
  char *logicalName;
  {
   struct router *currentPtr;

   /*===================================================*/
   /* If the "fast load" option is being used, then the */
   /* logical name is actually a pointer to a file and  */
   /* ungetc can be called directly to bypass querying  */
   /* all of the routers.                               */
   /*===================================================*/
   
   if (((char *) FastLoadFilePtr) == logicalName)
     { return(ungetc(ch,FastLoadFilePtr)); }

   /*===============================================*/
   /* Search through the list of routers until one  */
   /* is found that will handle the ungetc request. */
   /*===============================================*/
   
   currentPtr = ListOfRouters;
   while (currentPtr != NULL)
     {
      if ((currentPtr->charunget != NULL) ? QueryRouter(logicalName,currentPtr) : CLIPS_FALSE)
        { return((*currentPtr->charunget) (ch,logicalName)); }
      currentPtr = currentPtr->next;
     }

   /*=====================================================*/
   /* The logical name was not recognized by any routers. */
   /*=====================================================*/
   
   UnrecognizedRouterMessage(logicalName);
   return(-1);
  }
  
/*********************************************/
/* ExitCommand: Exits the CLIPS environment. */
/*********************************************/
globle VOID ExitCommand()
  {
   if (ArgCountCheck("exit",EXACTLY,0) == -1) return;
#if VAX_VMS
   ExitCLIPS(-2);  /* Fix for weird VMS com file problem. */
#else
   ExitCLIPS(-1);
#endif
   return;
  }
  
/*******************************************/
/* ExitCLIPS: Generic exit function. Calls */
/*   all of the router exit functions.     */
/*******************************************/
globle VOID ExitCLIPS(num)
  int num;
  {
   struct router *currentPtr, *nextPtr;

   Abort = CLIPS_FALSE;
   currentPtr = ListOfRouters;
   while (currentPtr != NULL)
     {
      nextPtr = currentPtr->next;
      if (currentPtr->active == CLIPS_TRUE)
        { if (currentPtr->exiter != NULL) (*currentPtr->exiter) (num); }
      currentPtr = nextPtr;
     }

   if (Abort) return;
   genexit(num);
  }

/********************************************/
/* AbortExit: Forces ExitCLIPS to terminate */
/*   after calling all closing routers.     */
/********************************************/
globle VOID AbortExit()
  {
   Abort = CLIPS_TRUE;
  }

/*********************************************************/
/* AddRouter: Adds an I/O router to the list of routers. */
/*********************************************************/
globle BOOLEAN AddRouter(routerName,priority,queryFunction,printFunction,
                         getcFunction,ungetcFunction,exitFunction)
  char *routerName;
  int priority;
#if ANSI_COMPILER
  int (*queryFunction)(char *);
  int (*printFunction)(char *,char *);
  int (*getcFunction)(char *);
  int (*ungetcFunction)(int,char *);
  int (*exitFunction)(int);
#else
  int (*queryFunction)();
  int (*printFunction)();
  int (*getcFunction)();
  int (*ungetcFunction)();
  int (*exitFunction)();
#endif
  {
   struct router *newPtr, *lastPtr, *currentPtr;

   newPtr = get_struct(router);

   newPtr->name = routerName;
   newPtr->active = CLIPS_TRUE;
   newPtr->priority = priority;
   newPtr->query = queryFunction;
   newPtr->printer = printFunction;
   newPtr->exiter = exitFunction;
   newPtr->charget = getcFunction;
   newPtr->charunget = ungetcFunction;
   newPtr->next = NULL;

   if (ListOfRouters == NULL)
     {
      ListOfRouters = newPtr;
      return(1);
     }

   lastPtr = NULL;
   currentPtr = ListOfRouters;
   while ((currentPtr != NULL) ? (priority < currentPtr->priority) : CLIPS_FALSE)
     {
      lastPtr = currentPtr;
      currentPtr = currentPtr->next;
     }

   if (lastPtr == NULL)
     {
      newPtr->next = ListOfRouters;
      ListOfRouters = newPtr;
     }
   else
     {
      newPtr->next = currentPtr;
      lastPtr->next = newPtr;
     }

   return(1);
  }

/*****************************************************************/
/* DeleteRouter: Removes an I/O router from the list of routers. */
/*****************************************************************/
globle int DeleteRouter(routerName)
  char *routerName;
  {
   struct router *currentPtr, *lastPtr;

   currentPtr = ListOfRouters;
   lastPtr = NULL;

   while (currentPtr != NULL)
     {
      if (strcmp(currentPtr->name,routerName) == 0)
        {
         if (lastPtr == NULL)
           {
            ListOfRouters = currentPtr->next;
            rm(currentPtr,(int) sizeof(struct router));
            return(1);
           }
         lastPtr->next = currentPtr->next;
         rm(currentPtr,(int) sizeof(struct router));
         return(1);
        }
      lastPtr = currentPtr;
      currentPtr = currentPtr->next;
     }

   return(0);
  }

/*********************************************************************/
/* QueryRouters: Determines if any router recognizes a logical name. */
/*********************************************************************/
globle int QueryRouters(logicalName)
  char *logicalName;
  {
   struct router *currentPtr;

   currentPtr = ListOfRouters;
   while (currentPtr != NULL)
     {
      if (QueryRouter(logicalName,currentPtr) == CLIPS_TRUE) return(CLIPS_TRUE);
      currentPtr = currentPtr->next;
     }

   return(CLIPS_FALSE);
  }

/************************************************/
/* QueryRouter: Determines if a specific router */
/*    recognizes a logical name.                */
/************************************************/
static int QueryRouter(logicalName,currentPtr)
  char *logicalName;
  struct router *currentPtr;
  {
   /*===================================================*/
   /* If the router is inactive, then it can't respond. */
   /*===================================================*/
   
   if (currentPtr->active == CLIPS_FALSE)
     { return(CLIPS_FALSE); }

   /*=============================================================*/
   /* If the router has no query function, then it can't respond. */
   /*=============================================================*/
   
   if (currentPtr->query == NULL) return(CLIPS_FALSE);

   /*=========================================*/
   /* Call the router's query function to see */
   /* if it recognizes the logical name.      */
   /*=========================================*/
   
   if ( (*currentPtr->query) (logicalName) == CLIPS_TRUE )
     { return(CLIPS_TRUE); }

   return(CLIPS_FALSE);
  }

/****************************************************/
/* DeactivateRouter: Deactivates a specific router. */
/****************************************************/
globle int DeactivateRouter(routerName)
  char *routerName;
  {
   struct router *currentPtr;

   currentPtr = ListOfRouters;

   while (currentPtr != NULL)
     {
      if (strcmp(currentPtr->name,routerName) == 0)
        {
         currentPtr->active = CLIPS_FALSE;
         return(CLIPS_TRUE);
        }
      currentPtr = currentPtr->next;
     }

   return(CLIPS_FALSE);
  }

/************************************************/
/* ActivateRouter: Activates a specific router. */
/************************************************/
globle int ActivateRouter(routerName)
  char *routerName;
  {
   struct router *currentPtr;

   currentPtr = ListOfRouters;

   while (currentPtr != NULL)
     {
      if (strcmp(currentPtr->name,routerName) == 0)
        {
         currentPtr->active = CLIPS_TRUE;
         return(CLIPS_TRUE);
        }
      currentPtr = currentPtr->next;
     }

   return(CLIPS_FALSE);
  }

/********************************************************/
/* SetFastLoad: Used to bypass router system for loads. */
/********************************************************/
globle VOID SetFastLoad(filePtr)
  FILE *filePtr;
  { FastLoadFilePtr = filePtr; }

/********************************************************/
/* SetFastSave: Used to bypass router system for saves. */
/********************************************************/
globle VOID SetFastSave(filePtr)
  FILE *filePtr;
  { FastSaveFilePtr = filePtr; }

/******************************************************/
/* GetFastLoad: Returns the "fast load" file pointer. */
/******************************************************/
globle FILE *GetFastLoad()
  { return(FastLoadFilePtr); }

/******************************************************/
/* GetFastSave: Returns the "fast save" file pointer. */
/******************************************************/
globle FILE *GetFastSave()
  { return(FastSaveFilePtr); }

/*****************************************************/
/* UnrecognizedRouterMessage: Standard error message */
/*   for an unrecognized router name.                */
/*****************************************************/
globle VOID UnrecognizedRouterMessage(logicalName)
  char *logicalName;
  {
   PrintErrorID("ROUTER",1,CLIPS_FALSE);
   PrintCLIPS(WERROR,"Logical name ");
   PrintCLIPS(WERROR,logicalName);
   PrintCLIPS(WERROR," was not recognized by any routers\n");
  }
  
