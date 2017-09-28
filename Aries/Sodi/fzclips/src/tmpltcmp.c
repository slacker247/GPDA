   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  02/08/94            */
   /*                                                     */
   /*          DEFTEMPLATE CONSTRUCTS-TO-C MODULE         */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the constructs-to-c feature for the   */
/*    deftemplate construct.                                 */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Donnell                                     */
/*      Bob Orchard (NRCC - Nat'l Research Council of Canada)*/
/*                  (Fuzzy reasoning extensions)             */
/*                  (certainty factors for facts and rules)  */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#define _TMPLTCMP_SOURCE_

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT && CONSTRUCT_COMPILER && (! RUN_TIME)

#define SlotPrefix() ArbitraryPrefix(DeftemplateCodeItem,2)

#if FUZZY_DEFTEMPLATES    /* added 03-11-96 */
/* add 2 more files to store the Fuzzy template definitions */
#define LvUniversePrefix()  ArbitraryPrefix(DeftemplateCodeItem,3)
#define PrimaryTermPrefix() ArbitraryPrefix(DeftemplateCodeItem,4)
#endif

#include <stdio.h>
#define _CLIPS_STDIO_

#include "conscomp.h"
#include "factcmp.h"
#include "cstrncmp.h"
#include "tmpltdef.h"

#include "tmpltcmp.h"

#if FUZZY_DEFTEMPLATES     /* added 03-11-96 */
#include "fuzzyval.h"
#include "fuzzylv.h"
#include "dffnxcmp.h"
#include "prntutil.h"
#endif

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if ANSI_COMPILER
   static int                     ConstructToCode(char *,int,FILE *,int,int);
   static VOID                    SlotToCode(FILE *,struct templateSlot *,int,int,int);
   static VOID                    DeftemplateModuleToCode(FILE *,struct defmodule *,int,int,int);
#if FUZZY_DEFTEMPLATES    /* added 03-11-96 */
   static VOID                    DeftemplateToCode(FILE *,struct deftemplate *,
                                                 int,int,int,int,int,int);
   static VOID                    CloseDeftemplateFiles(FILE *,FILE *,FILE *,FILE *,FILE *,int);
#else
   static VOID                    DeftemplateToCode(FILE *,struct deftemplate *,
                                                 int,int,int,int);
   static VOID                    CloseDeftemplateFiles(FILE *,FILE *,FILE *,int);
#endif
#if FUZZY_DEFTEMPLATES     /* added 03-11-96 */
   static VOID                    LvUniverseToCode(FILE *,struct fuzzyLv *,
                                                 int,int,int,int);
   static VOID                    primaryTermToCode(FILE *,struct primary_term *,
                                                 int,int,int *, int);
#endif
#else
   static int                     ConstructToCode();
   static VOID                    SlotToCode();
   static VOID                    DeftemplateModuleToCode();
   static VOID                    DeftemplateToCode();
   static VOID                    CloseDeftemplateFiles();
#if FUZZY_DEFTEMPLATES     /* added 03-11-96 */
   static VOID                    LvUniverseToCode();
   static VOID                    primaryTermToCode();
#endif
#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static struct CodeGeneratorItem *DeftemplateCodeItem;

/*********************************************************/
/* DeftemplateCompilerSetup: Initializes the deftemplate */
/*   construct for use with the constructs-to-c command. */
/*********************************************************/
globle VOID DeftemplateCompilerSetup()
  {
#if FUZZY_DEFTEMPLATES     /* added 03-11-96 */
DeftemplateCodeItem = AddCodeGeneratorItem("deftemplate",0,NULL,NULL,ConstructToCode,5);
#else
   DeftemplateCodeItem = AddCodeGeneratorItem("deftemplate",0,NULL,NULL,ConstructToCode,3);
#endif
  }
  
/*************************************************************/
/* ConstructToCode: Produces deftemplate code for a run-time */
/*   module created using the constructs-to-c function.      */
/*************************************************************/
static int ConstructToCode(fileName,fileID,headerFP,imageID,maxIndices)
  char *fileName;
  int fileID;
  FILE *headerFP;
  int imageID;
  int maxIndices;
  {
   int fileCount = 1;
   struct defmodule *theModule;
   struct deftemplate *theTemplate;
   struct templateSlot *slotPtr;
   int slotCount = 0, slotArrayCount = 0, slotArrayVersion = 1;
   int moduleCount = 0, moduleArrayCount = 0, moduleArrayVersion = 1;  
   int templateArrayCount = 0, templateArrayVersion = 1;
   FILE *slotFile = NULL, *moduleFile = NULL, *templateFile = NULL;
#if FUZZY_DEFTEMPLATES    /* added 03-11-96 */
   int lvUniverseArrayCount = 0, lvUniverseArrayVersion = 1;
   int primaryTermArrayCount = 0, primaryTermArrayVersion = 1;
   FILE *lvUniverseFile = NULL, *primaryTermFile = NULL;
   struct fuzzyLv *lvPtr;
   struct primary_term *primaryTermPtr;
#endif

   /*==================================================*/
   /* Include the appropriate deftemplate header file. */
   /*==================================================*/
   
   fprintf(headerFP,"#include \"tmpltdef.h\"\n");
   
   /*=============================================================*/
   /* Loop through all the modules, all the deftemplates, and all */
   /* the deftemplate slots writing their C code representation   */
   /* to the file as they are traversed.                          */
   /*=============================================================*/
   
   theModule = (struct defmodule *) GetNextDefmodule(NULL);
   
   while (theModule != NULL)
     {           
      SetCurrentModule((VOID *) theModule);
            
      moduleFile = OpenFileIfNeeded(moduleFile,fileName,fileID,imageID,&fileCount,
                                    moduleArrayVersion,headerFP,
                                    "struct deftemplateModule",ModulePrefix(DeftemplateCodeItem),
                                    CLIPS_FALSE,NULL);
                                    
      if (moduleFile == NULL)
        {
#if FUZZY_DEFTEMPLATES    /* added 03-11-96 */
         CloseDeftemplateFiles(moduleFile,templateFile,slotFile,
                               lvUniverseFile,primaryTermFile,maxIndices);
#else         CloseDeftemplateFiles(moduleFile,templateFile,slotFile,maxIndices);
#endif
         return(0);
        }
        
      DeftemplateModuleToCode(moduleFile,theModule,imageID,maxIndices,moduleCount);
      moduleFile = CloseFileIfNeeded(moduleFile,&moduleArrayCount,&moduleArrayVersion,
                                     maxIndices,NULL,NULL);

      /*=======================================================*/
      /* Loop through each of the deftemplates in this module. */
      /*=======================================================*/
      
      theTemplate = (struct deftemplate *) GetNextDeftemplate(NULL);

      while (theTemplate != NULL)
        {
         templateFile = OpenFileIfNeeded(templateFile,fileName,fileID,imageID,&fileCount,
                                         templateArrayVersion,headerFP,
                                         "struct deftemplate",ConstructPrefix(DeftemplateCodeItem),
                                         CLIPS_FALSE,NULL);
         if (templateFile == NULL)
           {
#if FUZZY_DEFTEMPLATES   /* added 03-11-96 */
            CloseDeftemplateFiles(moduleFile,templateFile,slotFile,
                                  lvUniverseFile,primaryTermFile,maxIndices);
#else
                  CloseDeftemplateFiles(moduleFile,templateFile,slotFile,maxIndices);
#endif
            return(0);
           }
           
#if FUZZY_DEFTEMPLATES    /* added 03-11-96 */
         DeftemplateToCode(templateFile,theTemplate,imageID,maxIndices,
                           moduleCount,slotCount,
                           lvUniverseArrayCount, lvUniverseArrayVersion);
#else
         DeftemplateToCode(templateFile,theTemplate,imageID,maxIndices,
                        moduleCount,slotCount);
#endif
         templateArrayCount++;
         templateFile = CloseFileIfNeeded(templateFile,&templateArrayCount,&templateArrayVersion,
                                          maxIndices,NULL,NULL);

#if FUZZY_DEFTEMPLATES     /* added 03-11-96 */
         /* write out the fuzzyLv with universe */
         lvPtr = theTemplate->fuzzyTemplate;
         if (lvPtr != NULL)
           {
             lvUniverseFile = OpenFileIfNeeded(lvUniverseFile,fileName,fileID,
                                               imageID,&fileCount,
                                               lvUniverseArrayVersion,headerFP,
                                               "struct fuzzyLv", 
                                               LvUniversePrefix(),CLIPS_FALSE,NULL);
             if (lvUniverseFile == NULL) 
                {
                 CloseDeftemplateFiles(moduleFile,templateFile,slotFile,
                                       lvUniverseFile,primaryTermFile,maxIndices);
                 return(0);	
                }
              LvUniverseToCode(lvUniverseFile,lvPtr,imageID,maxIndices,
                               primaryTermArrayCount,primaryTermArrayVersion);
              lvUniverseArrayCount++;
              lvUniverseFile = CloseFileIfNeeded(lvUniverseFile,&lvUniverseArrayCount,
                                                &lvUniverseArrayVersion,
                                                maxIndices,NULL,NULL);
			  
              /* now write out the primaryTerm list*/
              primaryTermPtr = lvPtr->primary_term_list;
              primaryTermFile = OpenFileIfNeeded(primaryTermFile,fileName,fileID,
                                                 imageID,&fileCount,
                                                 primaryTermArrayVersion,headerFP,
                                                 "struct primary_term",
                                                 PrimaryTermPrefix(),CLIPS_FALSE,NULL);
              if (primaryTermFile == NULL) 
                {
                 CloseDeftemplateFiles(moduleFile,templateFile,slotFile,
			               lvUniverseFile,primaryTermFile,maxIndices);
                 return(0);	
                }

              primaryTermToCode(primaryTermFile,primaryTermPtr,imageID,maxIndices, 
&primaryTermArrayCount,primaryTermArrayVersion);
              primaryTermFile = CloseFileIfNeeded(primaryTermFile,&primaryTermArrayCount,
                                               &primaryTermArrayVersion,maxIndices,NULL,NULL);
			  
			  
			  
			  
            } /* end of if (lvPtr != NULL) */
#endif
                                                             
         /*======================================================*/
         /* Loop through each of the slots for this deftemplate. */
         /*======================================================*/
          
         slotPtr = theTemplate->slotList;
         while (slotPtr != NULL)
           {
            slotFile = OpenFileIfNeeded(slotFile,fileName,fileID,imageID,&fileCount,
                                        slotArrayVersion,headerFP,
                                       "struct templateSlot",SlotPrefix(),CLIPS_FALSE,NULL);
            if (slotFile == NULL) 
              {
#if FUZZY_DEFTEMPLATES    /* added 03-11-96 */
               CloseDeftemplateFiles(moduleFile,templateFile,slotFile,
                                     lvUniverseFile,primaryTermFile,maxIndices);
#else
                    CloseDeftemplateFiles(moduleFile,templateFile,slotFile,maxIndices);
#endif
               return(0);
              }
              
            SlotToCode(slotFile,slotPtr,imageID,maxIndices,slotCount);
            slotCount++;
            slotArrayCount++;
            slotFile = CloseFileIfNeeded(slotFile,&slotArrayCount,&slotArrayVersion,
                                         maxIndices,NULL,NULL);
            slotPtr = slotPtr->next;
           }
           
         theTemplate = (struct deftemplate *) GetNextDeftemplate(theTemplate);
        }
        
      theModule = (struct defmodule *) GetNextDefmodule(theModule);
      moduleCount++;
      moduleArrayCount++;

     }

#if FUZZY_DEFTEMPLATES    /* added 03-11-96 */
   CloseDeftemplateFiles(moduleFile,templateFile,slotFile,
                         lvUniverseFile,primaryTermFile,maxIndices);
#else           CloseDeftemplateFiles(moduleFile,templateFile,slotFile,maxIndices);
#endif
     
   return(1);
  }

/************************************************************/
/* CloseDeftemplateFiles: Closes all of the C files created */
/*   for deftemplates. Called when an error occurs or when  */
/*   the deftemplates have all been written to the files.   */
/************************************************************/
#if FUZZY_DEFTEMPLATES    /* added 03-11-96 */
static VOID CloseDeftemplateFiles(moduleFile,templateFile,slotFile,
                                  lvUniverseFile,primaryTermFile,maxIndices)
#else
static VOID CloseDeftemplateFiles(moduleFile,templateFile,slotFile,maxIndices)
#endif
  FILE *moduleFile, *templateFile, *slotFile;
#if FUZZY_DEFTEMPLATES   /* added 03-11-96 */
  FILE *lvUniverseFile, *primaryTermFile;
#endif 
  int maxIndices;  
  {
   int count = maxIndices;
   int arrayVersion = 0;
   
   if (slotFile != NULL) 
     {
      count = maxIndices;
      CloseFileIfNeeded(slotFile,&count,&arrayVersion,maxIndices,NULL,NULL);
     }
     
   if (templateFile != NULL) 
     {
      count = maxIndices;
      CloseFileIfNeeded(templateFile,&count,&arrayVersion,maxIndices,NULL,NULL);
     }
     
   if (moduleFile != NULL) 
     {
      count = maxIndices;
      CloseFileIfNeeded(moduleFile,&count,&arrayVersion,maxIndices,NULL,NULL);
     }

#if FUZZY_DEFTEMPLATES    /* added 03-11-96 */
   if (lvUniverseFile != NULL) 
     {
      count = maxIndices;
      lvUniverseFile = CloseFileIfNeeded(lvUniverseFile,&count,&arrayVersion,maxIndices,NULL,NULL);
     }

   if (primaryTermFile != NULL) 
     {
      count = maxIndices;
      primaryTermFile = CloseFileIfNeeded(primaryTermFile,&count,&arrayVersion,maxIndices,NULL,NULL);
     }

#endif
  }

#if FUZZY_DEFTEMPLATES   /* added 03-11-96 */

/* generate code for the fuzzy deftemplate definitions */

/************************************************************/
/* LvUniverseToCode:                                        */
/************************************************************/
#if IBM_TBC
#pragma argsused
#endif
static VOID LvUniverseToCode(lvUniverseFile,lvPtr,imageID,maxIndices,
                             primaryTermArrayCount,primaryTermArrayVersion)
  FILE *lvUniverseFile;
  struct fuzzyLv *lvPtr;
  int imageID;
  int maxIndices;
  int primaryTermArrayCount,primaryTermArrayVersion;
  {
#if MAC_MPW
#pragma unused(maxIndices)
#endif
    fprintf(lvUniverseFile, "{%s, %s, ", 
            FloatToString(lvPtr->from), FloatToString(lvPtr->to));
    PrintSymbolReference(lvUniverseFile,lvPtr->units);
    fprintf(lvUniverseFile, ", &%s%d_%d[%d] }",
            PrimaryTermPrefix(), imageID, primaryTermArrayVersion, primaryTermArrayCount);
  }


/************************************************************/
/* primaryTermToCode:                                       */
/************************************************************/
static VOID primaryTermToCode(primaryTermFile,primaryTermPtr,imageID,maxIndices,
							  primaryTermArrayCount,primaryTermArrayVersion)
  FILE *primaryTermFile;
  struct primary_term *primaryTermPtr;
  int imageID;
  int maxIndices;
  int *primaryTermArrayCount,primaryTermArrayVersion;
  {
    int count, arrayVersion;
    struct primary_term *nextPtr;
	
    while (primaryTermPtr != NULL)
       {
         nextPtr = primaryTermPtr->next;
         if ((nextPtr == NULL) && (*primaryTermArrayCount >= maxIndices))
           {
             count = 0;
             arrayVersion = primaryTermArrayVersion+1;
           }
         else
           {
             count = *primaryTermArrayCount+1;
             arrayVersion = primaryTermArrayVersion;
           }
	  
         fprintf(primaryTermFile,"{");  
         PrintFuzzyValueReference( primaryTermFile, primaryTermPtr->fuzzy_value_description);
         if (nextPtr != NULL)
            fprintf(primaryTermFile,",&%s%d_%d[%d]}",
                    PrimaryTermPrefix(), imageID, arrayVersion, count);
         else
            fprintf(primaryTermFile, ",NULL}");
	     
         *primaryTermArrayCount += 1;
         primaryTermPtr = nextPtr;
		 
         if (primaryTermPtr != NULL)
         fprintf(primaryTermFile,",");
       }
  }


#endif	
	    
/*************************************************************/
/* DeftemplateModuleToCode: Writes the C code representation */
/*   of a single deftemplate module to the specified file.   */
/*************************************************************/
#if IBM_TBC
#pragma argsused
#endif
static VOID DeftemplateModuleToCode(theFile,theModule,imageID,maxIndices,moduleCount)
  FILE *theFile;
  struct defmodule *theModule;
  int imageID;
  int maxIndices;
  int moduleCount;
  {   
#if MAC_MPW || MAC_MCW   /* added 03-11-96 */
#pragma unused(moduleCount)
#endif
   fprintf(theFile,"{"); 
   
   ConstructModuleToCode(theFile,theModule,imageID,maxIndices,
                                  DeftemplateModuleIndex,ConstructPrefix(DeftemplateCodeItem));
      
   fprintf(theFile,"}"); 
  }
  
/************************************************************/
/* DeftemplateToCode: Writes the C code representation of a */
/*   single deftemplate construct to the specified file.    */
/************************************************************/
#if FUZZY_DEFTEMPLATES   /* added 03-11-96 */
static VOID DeftemplateToCode(theFile,theTemplate,imageID,maxIndices,
                           moduleCount,slotCount,
						   lvUniverseArrayCount, lvUniverseArrayVersion)
#else
static VOID DeftemplateToCode(theFile,theTemplate,imageID,maxIndices,
                           moduleCount,slotCount)
#endif
  FILE *theFile;
  struct deftemplate *theTemplate;
  int imageID;
  int maxIndices;
  int moduleCount;
  int slotCount;
#if FUZZY_DEFTEMPLATES    /* added 03-11-96 */
  int lvUniverseArrayCount;
  int lvUniverseArrayVersion;
#endif
  {
   /*====================*/
   /* Deftemplate Header */
   /*====================*/
   
   fprintf(theFile,"{");
            
   ConstructHeaderToCode(theFile,&theTemplate->header,imageID,maxIndices,
                                  moduleCount,ModulePrefix(DeftemplateCodeItem),
                                  ConstructPrefix(DeftemplateCodeItem));
   fprintf(theFile,","); 
    
   /*===========*/
   /* Slot List */
   /*===========*/

   if (theTemplate->slotList == NULL)
     { fprintf(theFile,"NULL,"); }
   else
     {
      fprintf(theFile,"&%s%d_%d[%d],",SlotPrefix(),
                 imageID,
                 (slotCount / maxIndices) + 1,
                 slotCount % maxIndices);
     }

   /*==========================================*/
   /* Implied Flag, Watch Flag, In Scope Flag, */
   /* Number of Slots, and Busy Count.         */
   /*==========================================*/
   
   fprintf(theFile,"%d,0,0,%d,%ld,",theTemplate->implied,theTemplate->numberOfSlots,theTemplate->busyCount);

   /*=================*/
   /* Pattern Network */
   /*=================*/
   
   if (theTemplate->patternNetwork == NULL)
     { fprintf(theFile,"NULL"); }
   else
     { FactPatternNodeReference(theTemplate->patternNetwork,theFile,imageID,maxIndices); }

#if FUZZY_DEFTEMPLATES  /* added 03-11-96 */

   /*==========================================*/
   /* hasFuzzySlots field and fuzzyTemplate    */
   /*==========================================*/

   if (theTemplate->fuzzyTemplate == NULL)
     { fprintf(theFile,",%d,NULL", theTemplate->hasFuzzySlots); }
   else
     { fprintf(theFile, ",%d,(struct fuzzyLv *)&%s%d_%d[%d]", 
                        theTemplate->hasFuzzySlots,
                        LvUniversePrefix(),imageID,
	                lvUniverseArrayVersion,lvUniverseArrayCount); 
     }
	 
#endif /* FUZZY_DEFTEMPLATES */   
   fprintf(theFile,"}");
  }
  
/*****************************************************/
/* SlotToCode: Writes the C code representation of a */
/*   single deftemplate slot to the specified file.  */
/*****************************************************/
static VOID SlotToCode(theFile,theSlot,imageID,maxIndices,slotCount)
  FILE *theFile;
  struct templateSlot *theSlot;
  int imageID;
  int maxIndices;
  int slotCount;
  {
   /*===========*/
   /* Slot Name */
   /*===========*/
   
   fprintf(theFile,"{");
   PrintSymbolReference(theFile,theSlot->slotName);
   
   /*=============================*/
   /* Multislot and Default Flags */
   /*=============================*/
   
   fprintf(theFile,",%d,%d,%d,%d,",theSlot->multislot,theSlot->noDefault,
                                   theSlot->defaultPresent,theSlot->defaultDynamic);
   
   /*=============*/
   /* Constraints */
   /*=============*/
               
   PrintConstraintReference(theFile,theSlot->constraints,imageID,maxIndices);
      
   /*===============*/
   /* Default Value */
   /*===============*/
   
   fprintf(theFile,",");
   PrintHashedExpressionReference(theFile,theSlot->defaultList,imageID,maxIndices);
   fprintf(theFile,",");

   /*===========*/
   /* Next Slot */
   /*===========*/
   
   if (theSlot->next == NULL)
     { fprintf(theFile,"NULL}"); }
   else
     {
      fprintf(theFile,"&%s%d_%d[%d]}",SlotPrefix(),imageID,
                               ((slotCount+1) / maxIndices) + 1,
                                (slotCount+1) % maxIndices);
     }
  }
        
/*****************************************************************/
/* DeftemplateCModuleReference: Writes the C code representation */
/*   of a reference to a deftemplate module data structure.      */
/*****************************************************************/
globle VOID DeftemplateCModuleReference(theFile,count,imageID,maxIndices)
  FILE *theFile;
  int count;
  int imageID;
  int maxIndices;
  {
   fprintf(theFile,"MIHS &%s%d_%d[%d]",ModulePrefix(DeftemplateCodeItem),
                      imageID,
                      (count / maxIndices) + 1,
                      (count % maxIndices));
  }

/********************************************************************/
/* DeftemplateCConstructReference: Writes the C code representation */
/*   of a reference to a deftemplate data structure.                */
/********************************************************************/
globle VOID DeftemplateCConstructReference(theFile,vTheTemplate,imageID,maxIndices)
  FILE *theFile;
  VOID *vTheTemplate;
  int imageID;
  int maxIndices;
  {  
   struct deftemplate *theTemplate = (struct deftemplate *) vTheTemplate;

   if (theTemplate == NULL)
     { fprintf(theFile,"NULL"); }
   else
     {
      fprintf(theFile,"&%s%d_%ld[%ld]",ConstructPrefix(DeftemplateCodeItem),
                      imageID,
                      (theTemplate->header.bsaveID / maxIndices) + 1,
                      theTemplate->header.bsaveID % maxIndices);
     }

  } 
  
#endif /* DEFTEMPLATE_CONSTRUCT && CONSTRUCT_COMPILER && (! RUN_TIME) */
