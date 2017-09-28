   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*               CLIPS Version 6.00  08/02/94          */
   /*                                                     */
   /*                    BLOAD MODULE                     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides core routines for loading constructs    */
/*   from a binary file.                                     */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#define _BLOAD_SOURCE_

#include "setup.h"

#include "clipsmem.h"
#include "exprnpsr.h"
#include "argacces.h"
#include "router.h"
#include "constrct.h"
#include "bsave.h"
#include "cstrnbin.h"
#include "utility.h"

#include "bload.h"

#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if ANSI_COMPILER
   static struct FunctionDefinition * HUGE_ADDR *ReadNeededFunctions(long *,int *);
   static struct FunctionDefinition  *FastFindFunction(char *,struct FunctionDefinition *);
   static int                         ClearBload(void);
   static VOID                        AbortBload(void);
   static int                         BloadOutOfMemoryFunction(unsigned long);
#else
   static struct FunctionDefinition * HUGE_ADDR *ReadNeededFunctions();
   static struct FunctionDefinition  *FastFindFunction();
   static int                         ClearBload();
   static VOID                        AbortBload();
   static int                         BloadOutOfMemoryFunction();
#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static int                                    BloadActive = CLIPS_FALSE;
   static struct callFunctionItem               *BeforeBloadFunctions = NULL;
   static struct callFunctionItem               *AfterBloadFunctions = NULL;
   static struct callFunctionItem               *ClearBloadReadyFunctions = NULL;
   static struct callFunctionItem               *AbortBloadFunctions = NULL;

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   globle char                                  *BinaryPrefixID = "\1\2\3\4CLIPS";
   globle char                                  *BinaryVersionID = "V6.00";
   globle struct FunctionDefinition * HUGE_ADDR *FunctionArray;

/****************************/
/* Bload: C access routine  */
/*   for the bload command. */
/****************************/
globle int Bload(fileName)
  char *fileName;
  {
   long numberOfFunctions;
   unsigned long space;
   int error;
   char IDbuffer[20];
   char constructBuffer[CONSTRUCT_HEADER_SIZE];
   struct BinaryItem *biPtr;
   struct callFunctionItem *bfPtr;

   /*================*/
   /* Open the file. */
   /*================*/

   if (GenOpen("bload",fileName) == 0) return(CLIPS_FALSE);

   /*=====================================*/
   /* Determine if this is a binary file. */
   /*=====================================*/

   GenRead(IDbuffer,(unsigned long) strlen(BinaryPrefixID) + 1);
   if (strcmp(IDbuffer,BinaryPrefixID) != 0)
     {
      PrintErrorID("BLOAD",2,CLIPS_FALSE);
      PrintCLIPS(WERROR,"File ");
      PrintCLIPS(WERROR,fileName);
      PrintCLIPS(WERROR," is not a binary construct file.\n");
      GenClose();
      return(CLIPS_FALSE);
     }

   /*===========================================*/
   /* Determine if it's a binary file using a   */
   /* format from a different version of CLIPS. */
   /*===========================================*/

   GenRead(IDbuffer,(unsigned long) strlen(BinaryVersionID) + 1);
   if (strcmp(IDbuffer,BinaryVersionID) != 0)
     {
      PrintErrorID("BLOAD",3,CLIPS_FALSE);
      PrintCLIPS(WERROR,"File ");
      PrintCLIPS(WERROR,fileName);
      PrintCLIPS(WERROR," is an incompatible binary construct file.\n");
      GenClose();
      return(CLIPS_FALSE);
     }

   /*====================*/
   /* Clear environment. */
   /*====================*/

   if (BloadActive)
     {
      if (ClearBload() == CLIPS_FALSE)
        {
         GenClose();
         return(CLIPS_FALSE);
        }
     }

   /*====================================*/
   /* Determine if the CLIPS environment */
   /* was successfully cleared.          */
   /*====================================*/

   if (ClearReady() == CLIPS_FALSE)
     {
      GenClose();
      PrintCLIPS(WERROR,"The CLIPS environment could not be cleared.\n");
      PrintCLIPS(WERROR,"Binary load cannot continue.\n");
      return(CLIPS_FALSE);
     }
     
   /*==================================*/
   /* Call the list of functions to be */
   /* executed before a bload occurs.  */
   /*==================================*/
   
   for (bfPtr = BeforeBloadFunctions;
        bfPtr != NULL;
        bfPtr = bfPtr->next)
     { (*bfPtr->func)(); }

#if FUZZY_DEFTEMPLATES 
   /* NOTE: order changed for fuzzy CLIPS!!! */

   /*==========================================================*/
   /* Read in the memory requirements of the constructs stored */
   /* in this binary image and allocate the necessary space    */
   /*==========================================================*/
   for (GenRead(constructBuffer,(unsigned long) CONSTRUCT_HEADER_SIZE);
        strncmp(constructBuffer,BinaryPrefixID,CONSTRUCT_HEADER_SIZE) != 0;
        GenRead(constructBuffer,(unsigned long) CONSTRUCT_HEADER_SIZE))
     {
      BOOLEAN found;
      
      /*================================================*/
      /* Search for the construct type in the list of   */
      /* binary items. If found, allocate the storage   */
      /* needed by the construct for this binary image. */
      /*================================================*/
      
      found = CLIPS_FALSE;
      for (biPtr = ListOfBinaryItems;
           biPtr != NULL;
           biPtr = biPtr->next)
        {
         if (strncmp(biPtr->name,constructBuffer,CONSTRUCT_HEADER_SIZE) == 0)
           {
            if (biPtr->bloadStorageFunction != NULL)
              {
               (*biPtr->bloadStorageFunction)();
               found = CLIPS_TRUE;
              }
            break;
           }
        }
        
      /*==========================================*/
      /* If the construct type wasn't found, skip */
      /* the storage binary load information for  */
      /* this construct.                          */
      /*==========================================*/
      
      if (! found)
        {
         GenRead(&space,(unsigned long) sizeof(unsigned long));
         GenSeek((long) space);
         if (space != 0)
           {
            PrintCLIPS(WDIALOG,"\nSkipping ");
            PrintCLIPS(WDIALOG,constructBuffer);
            PrintCLIPS(WDIALOG," constructs because of unavailibility\n");
           }
        }
     }
  
#endif

   /*====================================================*/
   /* Read in the functions needed by this binary image. */
   /*====================================================*/

   FunctionArray = ReadNeededFunctions(&numberOfFunctions,&error);
   if (error)
     {
      GenClose();
      AbortBload();
      return(CLIPS_FALSE);
     }

   /*================================================*/
   /* Read in the atoms needed by this binary image. */
   /*================================================*/

   ReadNeededAtomicValues();
   
   /*=================================================*/
   /* Determine the number of expressions to be read  */
   /* and allocate the appropriate space              */
   /*=================================================*/
   
   AllocateExpressions();
   
#if !FUZZY_DEFTEMPLATES 
   /*==========================================================*/
   /* Read in the memory requirements of the constructs stored */
   /* in this binary image and allocate the necessary space    */
   /*==========================================================*/

   for (GenRead(constructBuffer,(unsigned long) CONSTRUCT_HEADER_SIZE);
        strncmp(constructBuffer,BinaryPrefixID,CONSTRUCT_HEADER_SIZE) != 0;
        GenRead(constructBuffer,(unsigned long) CONSTRUCT_HEADER_SIZE))
     {
      BOOLEAN found;
      
      /*================================================*/
      /* Search for the construct type in the list of   */
      /* binary items. If found, allocate the storage   */
      /* needed by the construct for this binary image. */
      /*================================================*/
      
      found = CLIPS_FALSE;
      for (biPtr = ListOfBinaryItems;
           biPtr != NULL;
           biPtr = biPtr->next)
        {
         if (strncmp(biPtr->name,constructBuffer,CONSTRUCT_HEADER_SIZE) == 0)
           {
            if (biPtr->bloadStorageFunction != NULL)
              {
               (*biPtr->bloadStorageFunction)();
               found = CLIPS_TRUE;
              }
            break;
           }
        }
        
      /*==========================================*/
      /* If the construct type wasn't found, skip */
      /* the storage binary load information for  */
      /* this construct.                          */
      /*==========================================*/
      
      if (! found)
        {
         GenRead(&space,(unsigned long) sizeof(unsigned long));
         GenSeek((long) space);
         if (space != 0)
           {
            PrintCLIPS(WDIALOG,"\nSkipping ");
            PrintCLIPS(WDIALOG,constructBuffer);
            PrintCLIPS(WDIALOG," constructs because of unavailibility\n");
           }
        }
     }
   
#endif

   /*======================================*/
   /* Refresh the pointers in expressions. */
   /*======================================*/
   
   RefreshExpressions();
   
   /*==========================*/
   /* Read in the constraints. */
   /*==========================*/
   
   ReadNeededConstraints();
   
   /*======================================================*/
   /* Read in the constructs stored in this binary image.  */
   /*======================================================*/


   for (GenRead(constructBuffer,(unsigned long) CONSTRUCT_HEADER_SIZE);
        strncmp(constructBuffer,BinaryPrefixID,CONSTRUCT_HEADER_SIZE) != 0;
        GenRead(constructBuffer,(unsigned long) CONSTRUCT_HEADER_SIZE))
     {
      BOOLEAN found;
      
      /*==================================================*/
      /* Search for the function to load the construct    */
      /* into the previously allocated storage. If found, */
      /* call the function to load the construct.         */
      /*==================================================*/
      
      found = CLIPS_FALSE;
      for (biPtr = ListOfBinaryItems;
           biPtr != NULL;
           biPtr = biPtr->next)
        {
         if (strncmp(biPtr->name,constructBuffer,CONSTRUCT_HEADER_SIZE) == 0)
           {
            if (biPtr->bloadFunction != NULL)
              {
               (*biPtr->bloadFunction)();
               found = CLIPS_TRUE;
              }
            break;
           }
        }
 
      /*==========================================*/
      /* If the construct type wasn't found, skip */
      /* the binary data for this construct.      */
      /*==========================================*/
        
      if (! found)
        {
         GenRead(&space,(unsigned long) sizeof(unsigned long));
         GenSeek((long) space);
        }
     }

   /*=================*/
   /* Close the file. */
   /*=================*/
   
   GenClose();

   /*========================================*/
   /* Free up temporary storage used for the */
   /* function and atomic value information. */
   /*========================================*/

   if (FunctionArray != NULL)
     { 
      genlongfree((VOID *) FunctionArray,
                  (unsigned long) sizeof(struct FunctionDefinition *) * numberOfFunctions);
     }
   FreeAtomicValueStorage();
    
   /*==================================*/
   /* Call the list of functions to be */
   /* executed after a bload occurs.   */
   /*==================================*/

   for (bfPtr = AfterBloadFunctions;
        bfPtr != NULL;
        bfPtr = bfPtr->next)
     { (*bfPtr->func)(); }

   /*=======================================*/
   /* Add a clear function to remove binary */
   /* load when a clear command is issued.  */
   /*=======================================*/
 
   BloadActive = CLIPS_TRUE;
   AddClearFunction("bload",(VOID (*)(VOID_ARG)) ClearBload,10000);

   /*=============================*/
   /* Return TRUE to indicate the */
   /* binary load was successful. */
   /*=============================*/

   return(CLIPS_TRUE);
  }

/************************************************************
  NAME         : BloadandRefresh
  DESCRIPTION  : Loads and refreshes objects - will bload
                 all objects at once, if possible, but
                 will aslo work in increments if memory is
                 restricted
  INPUTS       : 1) the number of objects to bload and update
                 2) the size of one object
                 3) An update function which takes a bloaded
                    object buffer and the index of the object
                    to refresh as arguments
  RETURNS      : Nothing useful
  SIDE EFFECTS : Objects bloaded and updated
  NOTES        : Assumes binary file pointer is positioned
                 for bloads of the objects
 ************************************************************/
globle VOID BloadandRefresh(objcnt,objsz,objupdate)
  long objcnt;
  unsigned objsz;
#if ANSI_COMPILER
  VOID (*objupdate)(VOID *,long);
#else
  VOID (*objupdate)();
#endif
  {
   register long i,bi;
   char HUGE_ADDR *buf;
   long objsmaxread,objsread;
   unsigned long space;
#if ANSI_COMPILER
   int (*oldOutOfMemoryFunction)(unsigned long);
#else
   int (*oldOutOfMemoryFunction)();
#endif
   
   if (objcnt == 0L) return;

   oldOutOfMemoryFunction = SetOutOfMemoryFunction(BloadOutOfMemoryFunction);
   objsmaxread = objcnt;
   do
     {
      space = objsmaxread * objsz;
      buf = (char HUGE_ADDR *) genlongalloc(space);
      if (buf == NULL)
        {
         if ((objsmaxread / 2) == 0)
           {
            if ((*oldOutOfMemoryFunction)(space) == CLIPS_TRUE)
              {
               SetOutOfMemoryFunction(oldOutOfMemoryFunction);
               return;
              }
           }
         else
           objsmaxread /= 2;
        }
     }
   while (buf == NULL);

   SetOutOfMemoryFunction(oldOutOfMemoryFunction);
   
   i = 0L;
   do
     {
      objsread = (objsmaxread > (objcnt - i)) ? (objcnt - i) : objsmaxread;
      GenRead((VOID *) buf,objsread * objsz);
      for (bi = 0L ; bi < objsread ; bi++ , i++)
        (*objupdate)(buf + objsz * bi,i);
     }
   while (i < objcnt);
   genlongfree((VOID *) buf,space);
  }

/**********************************************/
/* ReadNeededFunctions: Reads in the names of */
/*   functions needed by the binary image.    */
/**********************************************/
static struct FunctionDefinition * HUGE_ADDR *ReadNeededFunctions(numberOfFunctions,
                                                                  error)
  long int *numberOfFunctions;
  int *error;
  {
   char *functionNames, *namePtr;
   unsigned long int space,temp;
   long i;
   struct FunctionDefinition * HUGE_ADDR *newFunctionArray, 
*functionPtr;
   int functionsNotFound = 0;

   /*===================================================*/
   /* Determine the number of function names to be read */
   /* and the space required for them.                  */
   /*===================================================*/

   GenRead(numberOfFunctions,(unsigned long) sizeof(long int));
   GenRead(&space,(unsigned long) sizeof(unsigned long int));
   if (*numberOfFunctions == 0)
     {
      *error = CLIPS_FALSE;
      return(NULL);
     }

   /*=======================================*/
   /* Allocate area for strings to be read. */
   /*=======================================*/

   functionNames = (char *) genlongalloc(space);
   GenRead((VOID *) functionNames,space);

   /*====================================================*/
   /* Store the function pointers in the function array. */
   /*====================================================*/

   temp = (unsigned long) sizeof(struct FunctionDefinition *) * *numberOfFunctions;
   newFunctionArray = (struct FunctionDefinition **) genlongalloc(temp);
   namePtr = functionNames;
   functionPtr = NULL;
   for (i = 0; i < *numberOfFunctions; i++)
     {
      if ((functionPtr = FastFindFunction(namePtr,functionPtr)) == NULL)
        {
         if (! functionsNotFound)
           {
            PrintErrorID("BLOAD",6,CLIPS_FALSE);
            PrintCLIPS(WERROR,"The following undefined functions are ");
            PrintCLIPS(WERROR,"referenced by this binary image:\n");
           }

         PrintCLIPS(WERROR,"   ");
         PrintCLIPS(WERROR,namePtr);
         PrintCLIPS(WERROR,"\n");
         functionsNotFound = 1;
        }

      newFunctionArray[i] = functionPtr;
      namePtr += strlen(namePtr) + 1;
     }

   /*==========================================*/
   /* Free the memory used by the name buffer. */
   /*==========================================*/

   genlongfree((VOID *) functionNames,space);

   /*==================================================*/
   /* If any of the required functions were not found, */
   /* then free the memory used by the function array. */
   /*==================================================*/

   if (functionsNotFound)
     {
      genlongfree((VOID *) newFunctionArray,temp);
      newFunctionArray = NULL;
     }

   /*===================================*/
   /* Set globals to appropriate values */
   /* and return the function array.    */
   /*===================================*/

   *error = functionsNotFound;
   return(newFunctionArray);
  }

/***********************************************/
/* FastFindFunction: Search the CLIPS function */
/*   list for a specific function.             */
/***********************************************/
static struct FunctionDefinition *FastFindFunction(functionName,lastFunction)
  char *functionName;
  struct FunctionDefinition *lastFunction;
  {
   struct FunctionDefinition *theList, *theFunction;

   /*==============================*/
   /* Get the CLIPS function list. */
   /*==============================*/
   
   theList = GetFunctionList();
   if (theList == NULL) { return(NULL); }

   /*=======================================*/
   /* If we completed a previous function   */
   /* search, start where we last left off. */
   /*=======================================*/
   
   if (lastFunction != NULL)
     { theFunction = lastFunction->next; }
   else
     { theFunction = theList; }

   /*======================================================*/
   /* Traverse the rest of the function list searching for */
   /* the named function wrapping around if necessary.     */
   /*======================================================*/
   
   while (strcmp(functionName,ValueToString(theFunction->callFunctionName)) != 0)
     {
      theFunction = theFunction->next;
      if (theFunction == lastFunction) return(NULL);
      if (theFunction == NULL) theFunction = theList;
     }

   /*=======================*/
   /* Return the pointer to */
   /* the found function.   */
   /*=======================*/
   
   return(theFunction);
  }

/******************************************/
/* Bloaded: Returns TRUE if the current   */
/*   environment is the result of a bload */
/*   command, otherwise returns FALSE.    */
/******************************************/
globle BOOLEAN Bloaded()
  {
   return(BloadActive);
  }
/*************************************/
/* ClearBload: Clears a binary image */
/*   from the CLIPS environment.     */
/*************************************/
static int ClearBload()
  {
   struct BinaryItem *biPtr;
   struct callFunctionItem *bfPtr;
   int ready,error;

   /*=================================================*/
   /* Make sure it's safe to clear the bloaded image. */
   /*=================================================*/
   
   error = CLIPS_FALSE;
   for (bfPtr = ClearBloadReadyFunctions;
        bfPtr != NULL;
        bfPtr = bfPtr->next)
     {
      ready = (* ((int (*)(VOID_ARG)) bfPtr->func))();
      if (ready == CLIPS_FALSE)
        {
         if (! error)
           {
            PrintErrorID("BLOAD",5,CLIPS_FALSE); 
            PrintCLIPS(WERROR,
                       "Some constructs are still in use by the current binary image:\n");
           }
         PrintCLIPS(WERROR,"   ");
         PrintCLIPS(WERROR,bfPtr->name);
         PrintCLIPS(WERROR,"\n");
         error = CLIPS_TRUE;
        }
     }


   /*==================================================*/
   /* If some constructs are still in use and can't be */
   /* cleared, indicate the binary load can't continue */
   /* and return FALSE to indicate this condition.     */
   /*==================================================*/

   if (error == CLIPS_TRUE)
     {
      PrintCLIPS(WERROR,"Binary clear cannot continue.\n");
      return(CLIPS_FALSE);
     }

   /*=============================*/
   /* Call bload clear functions. */
   /*=============================*/

   for (biPtr = ListOfBinaryItems;
        biPtr != NULL;
        biPtr = biPtr->next)
     { if (biPtr->clearFunction != NULL) (*biPtr->clearFunction)(); }
      
   /*===========================*/
   /* Free bloaded expressions. */
   /*===========================*/
   
   ClearBloadedExpressions();
   
   /*===========================*/
   /* Free bloaded constraints. */
   /*===========================*/
   
   ClearBloadedConstraints();
   
   /*==================================*/
   /* Remove the bload clear function. */
   /*==================================*/

   BloadActive = CLIPS_FALSE;
   RemoveClearFunction("bload");

   /*====================================*/
   /* Return TRUE to indicate the binary */
   /* image was successfully cleared.    */
   /*====================================*/

   return(CLIPS_TRUE);
  }


/*************************************************/
/* AbortBload: Cleans up effects of before-bload */
/*   functions in event of failure.              */
/*************************************************/
static VOID AbortBload()
  {
   struct callFunctionItem *bfPtr;

   for (bfPtr = AbortBloadFunctions;
        bfPtr != NULL;
        bfPtr = bfPtr->next)
     { (*bfPtr->func)(); }
  }

/********************************************/
/* AddBeforeBloadFunction: Adds a function  */
/*   to the list of functions called before */
/*   a binary load occurs.                  */
/********************************************/
globle VOID AddBeforeBloadFunction(name,func,priority)
  char *name;
  VOID (*func)(VOID_ARG);
  int priority;
  {
   BeforeBloadFunctions = 
     AddFunctionToCallList(name,priority,func,BeforeBloadFunctions);
  }

/*******************************************/
/* AddAfterBloadFunction: Adds a function  */
/*   to the list of functions called after */
/*   a binary load occurs.                 */
/*******************************************/
globle VOID AddAfterBloadFunction(name,func,priority)
  char *name;
  VOID (*func)(VOID_ARG);
  int priority;
  {
   AfterBloadFunctions =
      AddFunctionToCallList(name,priority,func,AfterBloadFunctions);
  }

/**************************************************/
/* AddClearBloadReadyFunction: Adds a function to */
/*   the list of functions called to determine if */
/*   a binary image can be cleared.               */
/**************************************************/
globle VOID AddClearBloadReadyFunction(name,func,priority)
  char *name;
  int (*func)(VOID_ARG);
  int priority;
  {
   ClearBloadReadyFunctions = 
      AddFunctionToCallList(name,priority,
                            (VOID (*)(VOID_ARG)) func,
                            ClearBloadReadyFunctions);
  }

/*********************************************/
/* AddAbortBloadFunction: Adds a function to */
/*   the list of functions called if a bload */
/*   has to be aborted.                      */
/*********************************************/
globle VOID AddAbortBloadFunction(name,func,priority)
  char *name;
  VOID (*func)(VOID_ARG);
  int priority;
  {
   AbortBloadFunctions = AddFunctionToCallList(name,priority,func,AbortBloadFunctions);
  }

/*******************************************************
  NAME         : BloadOutOfMemoryFunction
  DESCRIPTION  : Memory function used by bload to
                   force CLIPS not to exit when out
                   of memory - used by BloadandRefresh
  INPUTS       : The memory request size (unused)
  RETURNS      : CLIPS_TRUE (indicates a failure and for
                 the CLIPS memory functions to simply
                 return a NULL pointer)
  SIDE EFFECTS : None
  NOTES        : None
 *******************************************************/
#if IBM_TBC
#pragma argsused
#endif
static int BloadOutOfMemoryFunction(size)
  unsigned long size;
  {
#if MAC_MPW || MAC_MCW
#pragma unused(size)
#endif
   return(CLIPS_TRUE);
  }

/*****************************************************/
/* CannotLoadWithBloadMessage: Generic error message */
/*   for indicating that a construct can't be loaded */
/*   when a binary image is active.                  */
/*****************************************************/  
globle VOID CannotLoadWithBloadMessage(constructName)
  char *constructName;
  {      
   PrintErrorID("BLOAD",1,CLIPS_TRUE);
   PrintCLIPS(WERROR,"Cannot load ");
   PrintCLIPS(WERROR,constructName);
   PrintCLIPS(WERROR," construct with binary load in effect.\n");
  }

#endif /* (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) */

/**************************************/
/* BloadCommand: CLIPS access routine */
/*   for the bload command.           */
/**************************************/
globle int BloadCommand()
  {
#if (! RUN_TIME) && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)
   char *fileName;

   if (ArgCountCheck("bload",EXACTLY,1) == -1) return(CLIPS_FALSE);
   fileName = GetFileName("bload",1);
   if (fileName != NULL) return(Bload(fileName));
#endif
   return(CLIPS_FALSE);
  }
