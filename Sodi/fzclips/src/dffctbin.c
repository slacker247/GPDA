   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.01  02/08/94            */
   /*                                                     */
   /*             DEFFACTS BSAVE/BLOAD MODULE             */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the binary save/load feature for the  */
/*    deffacts construct.                                    */
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

#define _DFFCTBIN_SOURCE_

#include "setup.h"

#if DEFFACTS_CONSTRUCT && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME)

#include <stdio.h>
#define _CLIPS_STDIO_

#include "clipsmem.h"
#include "dffctdef.h"
#include "moduldef.h"
#include "bload.h"
#include "bsave.h"

#include "dffctbin.h"

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static struct deffacts HUGE_ADDR        *DeffactsArray = NULL;
   static long                              NumberOfDeffacts = 0;
   static struct deffactsModule HUGE_ADDR  *ModuleArray;
   static long                              NumberOfDeffactsModules;

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if ANSI_COMPILER
#if BLOAD_AND_BSAVE
   static VOID                    BsaveFind(void);
   static VOID                    BsaveExpressions(FILE *);
   static VOID                    BsaveStorage(FILE *);
   static VOID                    BsaveBinaryItem(FILE *);
#endif
   static VOID                    BloadStorage(void);
   static VOID                    BloadBinaryItem(void);
   static VOID                    UpdateDeffactsModule(VOID *,long);
   static VOID                    UpdateDeffacts(VOID *,long);
   static VOID                    ClearBload(void);
#else
#if BLOAD_AND_BSAVE
   static VOID                    BsaveFind();
   static VOID                    BsaveExpressions();
   static VOID                    BsaveStorage();
   static VOID                    BsaveBinaryItem();
#endif
   static VOID                    BloadStorage();
   static VOID                    BloadBinaryItem();
   static VOID                    UpdateDeffactsModule();
   static VOID                    UpdateDeffacts();
   static VOID                    ClearBload();
#endif

/********************************************/
/* DeffactsBinarySetup: Installs the binary */
/*   save/load feature for deffacts.        */
/********************************************/
globle VOID DeffactsBinarySetup()
  {
#if BLOAD_AND_BSAVE
   AddBinaryItem("deffacts",0,BsaveFind,BsaveExpressions,
                             BsaveStorage,BsaveBinaryItem,
                             BloadStorage,BloadBinaryItem,
                             ClearBload);
#endif

#if (BLOAD || BLOAD_ONLY)
   AddBinaryItem("deffacts",0,NULL,NULL,NULL,NULL,
                             BloadStorage,BloadBinaryItem,
                             ClearBload);
#endif
  }

#if BLOAD_AND_BSAVE

/*********************************************************/
/* BsaveFind: Counts the number of data structures which */
/*   must be saved in the binary image for the deffacts  */
/*   in the current environment.                         */
/*********************************************************/
static VOID BsaveFind()
  {
   struct deffacts *theDeffacts; /* changed 03-05-96 */
   struct defmodule *theModule;

   /*=======================================================*/
   /* If a binary image is already loaded, then temporarily */
   /* save the count values since these will be overwritten */
   /* in the process of saving the binary image.            */
   /*=======================================================*/
   
   if (Bloaded())
     {
      SaveBloadCount(NumberOfDeffactsModules);
      SaveBloadCount(NumberOfDeffacts);
     }
   
   /*========================================*/
   /* Set the count of deffacts and deffacts */
   /* module data structures to zero.        */
   /*========================================*/
   
   NumberOfDeffacts = 0;
   NumberOfDeffactsModules = 0;
   
   /*===========================*/
   /* Loop through each module. */
   /*===========================*/
   
   for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      /*===============================================*/
      /* Set the current module to the module being    */
      /* examined and increment the number of deffacts */
      /* modules encountered.                          */
      /*===============================================*/
      
      SetCurrentModule((VOID *) theModule);
      NumberOfDeffactsModules++;
      
      /*===================================================*/
      /* Loop through each deffacts in the current module. */
      /*===================================================*/
      
      for (theDeffacts = (struct deffacts *) GetNextDeffacts(NULL);
           theDeffacts != NULL;
           theDeffacts = (struct deffacts *) GetNextDeffacts(theDeffacts))
        {
         /*======================================================*/
         /* Initialize the construct header for the binary save. */
         /*======================================================*/
         
         MarkConstructHeaderNeededItems(&theDeffacts->header,NumberOfDeffacts++);
         
         /*============================================================*/
         /* Count the number of expressions contained in the deffacts' */
         /* assertion list and mark any atomic values contained there  */
         /* as in use.                                                 */
         /*============================================================*/
         
         ExpressionCount += ExpressionSize(theDeffacts->assertList);
         MarkNeededItems(theDeffacts->assertList);
        }
     }
  }

/************************************************/
/* BsaveExpressions: Saves the expressions used */
/*   by deffacts to the binary save file.       */
/************************************************/
static VOID BsaveExpressions(fp)
  FILE *fp;
  {
   struct deffacts *theDeffacts;
   struct defmodule *theModule;

   /*===========================*/
   /* Loop through each module. */
   /*===========================*/
   
   for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      /*======================================================*/
      /* Set the current module to the module being examined. */
      /*======================================================*/

      SetCurrentModule((VOID *) theModule);
      
      /*==================================================*/
      /* Loop through each deffacts in the current module */
      /* and save the assertion list expression.          */
      /*==================================================*/
      
      for (theDeffacts = (struct deffacts *) GetNextDeffacts(NULL);
           theDeffacts != NULL;
           theDeffacts = (struct deffacts *) GetNextDeffacts(theDeffacts))
        { BsaveExpression(theDeffacts->assertList,fp); }
     }
  }

/******************************************************/
/* BsaveStorage: Writes out the storage requirements  */
/*    for all deffacts structures to the binary file. */
/******************************************************/
static VOID BsaveStorage(fp)
  FILE *fp;
  {
   unsigned long space;

   /*=================================================================*/
   /* Only two data structures are saved as part of a deffacts binary */
   /* image: the deffacts data structure and the deffactsModule data  */
   /* structure. The assertion list expressions are not save with the */
   /* deffacts portion of the binary image.                           */
   /*=================================================================*/
   
   space = sizeof(long) * 2;
   GenWrite(&space,(unsigned long) sizeof(unsigned long int),fp);
   GenWrite(&NumberOfDeffacts,(unsigned long) sizeof(long int),fp);
   GenWrite(&NumberOfDeffactsModules,(unsigned long) sizeof(long int),fp);
  }
  
/********************************************/
/* BsaveBinaryItem: Writes out all deffacts */
/*   structures to the binary file.         */
/********************************************/
static VOID BsaveBinaryItem(fp)
  FILE *fp;
  {
   unsigned long int space;
   struct deffacts *theDeffacts;
   struct bsaveDeffacts newDeffacts;
   struct defmodule *theModule;
   struct bsaveDeffactsModule tempDeffactsModule;
   struct deffactsModule *theModuleItem;
   
   /*=========================================================*/
   /* Write out the amount of space taken up by the deffacts  */
   /* and deffactsModule data structures in the binary image. */
   /*=========================================================*/
   
   space = NumberOfDeffacts * sizeof(struct bsaveDeffacts) +
           (NumberOfDeffactsModules * sizeof(struct bsaveDeffactsModule));
   GenWrite(&space,(unsigned long) sizeof(unsigned long int),fp);
   
   /*================================================*/
   /* Write out each deffacts module data structure. */
   /*================================================*/

   NumberOfDeffacts = 0;
   for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      SetCurrentModule((VOID *) theModule);
      
      theModuleItem = (struct deffactsModule *) GetModuleItem(NULL,DeffactsModuleIndex);
      AssignBsaveDefmdlItemHdrVals(&tempDeffactsModule.header,&theModuleItem->header);
      GenWrite(&tempDeffactsModule,(unsigned long) sizeof(struct bsaveDeffactsModule),fp);
     }
     
   /*==========================*/
   /* Write out each deffacts. */
   /*==========================*/

   for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      SetCurrentModule((VOID *) theModule);
  
      for (theDeffacts = (struct deffacts *) GetNextDeffacts(NULL);
           theDeffacts != NULL;
           theDeffacts = (struct deffacts *) GetNextDeffacts(theDeffacts))
        {
         AssignBsaveConstructHeaderVals(&newDeffacts.header,&theDeffacts->header);
         if (theDeffacts->assertList != NULL)
           {
            newDeffacts.assertList = ExpressionCount;
            ExpressionCount += ExpressionSize(theDeffacts->assertList);
           }
         else
           { newDeffacts.assertList = -1L; }

         GenWrite(&newDeffacts,(unsigned long) sizeof(struct bsaveDeffacts),fp);
        }
     }
   
   /*=============================================================*/
   /* If a binary image was already loaded when the bsave command */
   /* was issued, then restore the counts indicating the number   */
   /* of deffacts and deffacts modules in the binary image (these */
   /* were overwritten by the binary save).                       */
   /*=============================================================*/

   if (Bloaded())
     {
      RestoreBloadCount(&NumberOfDeffactsModules);
      RestoreBloadCount(&NumberOfDeffacts);
     }
  }
  
#endif /* BLOAD_AND_BSAVE */

/****************************************************/
/* BloadStorage: Allocates storage requirements for */
/*   the deffacts used by this binary image.        */
/****************************************************/
static VOID BloadStorage()
  {
   unsigned long int space;

   /*=====================================================*/
   /* Determine the number of deffacts and deffactsModule */
   /* data structures to be read.                         */
   /*=====================================================*/

   GenRead(&space,(unsigned long) sizeof(unsigned long int));
   GenRead(&NumberOfDeffacts,(unsigned long) sizeof(long int));
   GenRead(&NumberOfDeffactsModules,(unsigned long) sizeof(long int));
   
   /*===================================*/
   /* Allocate the space needed for the */
   /* deffactsModule data structures.   */
   /*===================================*/
   
   if (NumberOfDeffactsModules == 0)
     {
      DeffactsArray = NULL;
      ModuleArray = NULL;
      return;
     }
     
   space = NumberOfDeffactsModules * sizeof(struct deffactsModule);
   ModuleArray = (struct deffactsModule HUGE_ADDR *) genlongalloc(space);

   /*===================================*/
   /* Allocate the space needed for the */
   /* deffacts data structures.         */
   /*===================================*/
   
   if (NumberOfDeffacts == 0)
     {
      DeffactsArray = NULL;
      return;
     }
     
   space = (unsigned long) (NumberOfDeffacts * sizeof(struct deffacts));
   DeffactsArray = (struct deffacts HUGE_ADDR *) genlongalloc(space);
  }

/*****************************************************/
/* BloadBinaryItem: Loads and refreshes the deffacts */
/*   constructs used by this binary image.           */
/*****************************************************/
static VOID BloadBinaryItem()
  {
   unsigned long int space;

   /*======================================================*/
   /* Read in the amount of space used by the binary image */
   /* (this is used to skip the construct in the event it  */
   /* is not available in the version of CLIPS being run). */
   /*======================================================*/
   
   GenRead(&space,(unsigned long) sizeof(unsigned long int));
   
   /*============================================*/
   /* Read in the deffactsModule data structures */
   /* and refresh the pointers.                  */
   /*============================================*/
   
   BloadandRefresh(NumberOfDeffactsModules,
                   (unsigned) sizeof(struct bsaveDeffactsModule),
                   UpdateDeffactsModule);
                   
   /*======================================*/
   /* Read in the deffacts data structures */
   /* and refresh the pointers.            */
   /*======================================*/
                   
   BloadandRefresh(NumberOfDeffacts,
                   (unsigned) sizeof(struct bsaveDeffacts),
                   UpdateDeffacts);
  }
  
/***************************************************/
/* UpdateDeffactsModule: Bload refresh routine for */
/*   deffacts module data structures.              */
/***************************************************/
static VOID UpdateDeffactsModule(buf,obji)
  VOID *buf;
  long obji;
  {
   struct bsaveDeffactsModule *bdmPtr;
   
   bdmPtr = (struct bsaveDeffactsModule *) buf;
   UpdateDefmoduleItemHeader(&bdmPtr->header,&ModuleArray[obji].header,
                             (int) sizeof(struct deffacts),(VOID *) DeffactsArray);
  }

/*********************************************/
/* UpdateDeffacts: Bload refresh routine for */
/*   deffacts data structures.               */
/*********************************************/
static VOID UpdateDeffacts(buf,obji)
  VOID *buf;
  long obji;
  {
   struct bsaveDeffacts *bdp;
   
   bdp = (struct bsaveDeffacts *) buf;
   UpdateConstructHeader(&bdp->header,&DeffactsArray[obji].header,
                         (int) sizeof(struct deffactsModule),(VOID *) ModuleArray,
                         (int) sizeof(struct deffacts),(VOID *) DeffactsArray);
   DeffactsArray[obji].assertList = ExpressionPointer(bdp->assertList);
  }
  
/**************************************/
/* ClearBload: Deffacts clear routine */
/*   when a binary load is in effect. */
/**************************************/
static VOID ClearBload()
  {
   long i;
   unsigned long space;

   /*=============================================*/
   /* Decrement in use counters for atomic values */
   /* contained in the construct headers.         */
   /*=============================================*/
   
   for (i = 0; i < NumberOfDeffacts; i++)
     { UnmarkConstructHeader(&DeffactsArray[i].header); }

   /*=============================================================*/
   /* Deallocate the space used for the deffacts data structures. */
   /*=============================================================*/
   
   space = NumberOfDeffacts * sizeof(struct deffacts);
   if (space != 0) genlongfree((VOID *) DeffactsArray,space);

   /*====================================================================*/
   /* Deallocate the space used for the deffacts module data structures. */
   /*====================================================================*/
   
   space =  NumberOfDeffactsModules * sizeof(struct deffactsModule);
   if (space != 0) genlongfree((VOID *) ModuleArray,space);
  }
  
/******************************************************/
/* BloadDeffactsModuleReference: Returns the deffacts */
/*   module pointer for use with the bload function.  */
/******************************************************/
globle VOID *BloadDeffactsModuleReference(index)
  int index;
  {
   return ((VOID *) &ModuleArray[index]);
  }
  
#endif /* DEFFACTS_CONSTRUCT && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME) */


