   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  03/10/94            */
   /*                                                     */
   /*                 SYMBOL BSAVE MODULE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the binary save/load feature for      */
/*    atomic data values.                                    */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Bob Orchard (NRCC - Nat'l Research Council of Canada)*/
/*                  (Fuzzy reasoning extensions)             */
/*                  (certainty factors for facts and rules)  */
/*                  (extensions to run command)              */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#define _BSAVE_SOURCE_      /* changed 03-12-96 */

#include "setup.h"

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE || BLOAD_INSTANCES || BSAVE_INSTANCES

#if FUZZY_DEFTEMPLATES    /* added 03-12-96 */
#include "symbol.h"
#include "tmpltbin.h"
#include "fuzzyval.h"
#include "fuzzyrhs.h"
#include "fuzzypsr.h"
#endif

#include "clipsmem.h"
#include "exprnpsr.h"
#include "argacces.h"
#include "router.h"
#include "cstrnbin.h"
#include "moduldef.h"
#include "bload.h"

#include "bsave.h"

#include "symblbin.h"

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static long                                   NumberOfSymbols = 0;
   static long                                   NumberOfFloats = 0;
   static long                                   NumberOfIntegers = 0;
   static long                                   NumberOfBitMaps = 0;
#if FUZZY_DEFTEMPLATES     /* added 03-12-96 */
   static long                                   NumberOfFuzzyValues = 0;
#endif

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   globle SYMBOL_HN * HUGE_ADDR                 *SymbolArray;
   globle struct floatHashNode * HUGE_ADDR      *FloatArray;
   globle INTEGER_HN * HUGE_ADDR                *IntegerArray;
   globle BITMAP_HN * HUGE_ADDR                 *BitMapArray;
#if FUZZY_DEFTEMPLATES     /* added 03-12-96 */
   globle FUZZY_VALUE_HN * HUGE_ADDR            *FuzzyValueArray;
#endif
   
/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if ANSI_COMPILER
   static VOID                        ReadNeededSymbols(void);
   static VOID                        ReadNeededFloats(void);
   static VOID                        ReadNeededIntegers(void);
   static VOID                        ReadNeededBitMaps(void);
#if FUZZY_DEFTEMPLATES    /* added 03-12-96 */
   static VOID                        ReadNeededFuzzyValues(void);
#endif
#if BLOAD_AND_BSAVE || BSAVE_INSTANCES
   static VOID                        WriteNeededSymbols(FILE *);
   static VOID                        WriteNeededFloats(FILE *);
   static VOID                        WriteNeededIntegers(FILE *);
   static VOID                        WriteNeededBitMaps(FILE *);
#if FUZZY_DEFTEMPLATES    /* added 03-12-96 */
   static VOID                        WriteNeededFuzzyValues(FILE *);
#endif
#endif
#else
   static VOID                        ReadNeededSymbols();
   static VOID                        ReadNeededFloats();
   static VOID                        ReadNeededIntegers();
   static VOID                        ReadNeededBitMaps();
#if FUZZY_DEFTEMPLATES    /* added 03-12-96 */
   static VOID                        ReadNeededFuzzyValues();
#endif
#if BLOAD_AND_BSAVE || BSAVE_INSTANCES
   static VOID                        WriteNeededSymbols();
   static VOID                        WriteNeededFloats();
   static VOID                        WriteNeededIntegers();
   static VOID                        WriteNeededBitMaps();
#if FUZZY_DEFTEMPLATES    /* added 03-12-96 */
   static VOID                        WriteNeededFuzzyValues();
#endif
#endif
#endif

#if BLOAD_AND_BSAVE || BSAVE_INSTANCES

/**********************************************/
/* WriteNeededAtomicValues: Save all symbols, */
/*   floats, integers, and bitmaps needed by  */
/*   this binary image to the binary file.    */
/**********************************************/
globle VOID WriteNeededAtomicValues(fp)
  FILE *fp;
  {
   WriteNeededSymbols(fp);
   WriteNeededFloats(fp);
   WriteNeededIntegers(fp);
   WriteNeededBitMaps(fp);
#if FUZZY_DEFTEMPLATES    /* added 03-12-96 */
   WriteNeededFuzzyValues(fp);
#endif
  }
  
/********************************************************/
/* InitAtomicValueNeededFlags: Initializes all symbols, */
/*   floats, integers, and bitmaps as being unneeded by */
/*   the binary image being saved.                      */
/********************************************************/
globle VOID InitAtomicValueNeededFlags()
  {
   int i;
   SYMBOL_HN *symbolPtr, **symbolArray;
   FLOAT_HN *floatPtr, **floatArray;
   INTEGER_HN *integerPtr, **integerArray;
   BITMAP_HN *bitMapPtr, **bitMapArray;
#if FUZZY_DEFTEMPLATES     /* added 03-12-96 */
   /* this MUST be done after ReadNeededSymbols 
      because of the symbol HN in a fuzzy value ...
	  not because of the WRITE but the READ! 
   */
   FUZZY_VALUE_HN *fuzzyValuePtr, **fuzzyValueArray;
#endif

   /*===============*/
   /* Mark symbols. */
   /*===============*/
   
   symbolArray = GetSymbolTable();

   for (i = 0; i < SYMBOL_HASH_SIZE; i++)
     {
      symbolPtr = symbolArray[i];
      while (symbolPtr != NULL)
        {
         symbolPtr->neededSymbol = CLIPS_FALSE;
         symbolPtr = symbolPtr->next;
        }
     }

   /*==============*/
   /* Mark floats. */
   /*==============*/
   
   floatArray = GetFloatTable();

   for (i = 0; i < FLOAT_HASH_SIZE; i++)
     {
      floatPtr = floatArray[i];
      while (floatPtr != NULL)
        {
         floatPtr->neededFloat = CLIPS_FALSE;
         floatPtr = floatPtr->next;
        }
     }

   /*================*/
   /* Mark integers. */
   /*================*/
   
   integerArray = GetIntegerTable();

   for (i = 0; i < INTEGER_HASH_SIZE; i++)
     {
      integerPtr = integerArray[i];
      while (integerPtr != NULL)
        {
         integerPtr->neededInteger = CLIPS_FALSE;
         integerPtr = integerPtr->next;
        }
     }

   /*===============*/
   /* Mark bitmaps. */
   /*===============*/
   
   bitMapArray = GetBitMapTable();

   for (i = 0; i < BITMAP_HASH_SIZE; i++)
     {
      bitMapPtr = bitMapArray[i];
      while (bitMapPtr != NULL)
        {
         bitMapPtr->neededBitMap = CLIPS_FALSE;
         bitMapPtr = bitMapPtr->next;
        }
     }

#if FUZZY_DEFTEMPLATES    /* added 03-12-96 */
   fuzzyValueArray = GetFuzzyValueTable();

   for (i = 0; i < FUZZY_VALUE_HASH_SIZE; i++)
     {
      fuzzyValuePtr = fuzzyValueArray[i];
      while (fuzzyValuePtr != NULL)
        {
         fuzzyValuePtr->neededFuzzyValue = CLIPS_FALSE;
         fuzzyValuePtr = fuzzyValuePtr->next;
        }
     }
#endif


  }

/*****************************************************************/
/* WriteNeededSymbols: Stores all of the symbols in the symbol   */
/*   table needed for this binary image in the binary save file. */
/*****************************************************************/
static VOID WriteNeededSymbols(fp)
  FILE *fp;
  {
   int i, length;
   SYMBOL_HN **symbolArray;
   SYMBOL_HN *symbolPtr;
   unsigned long int numberOfUsedSymbols = 0, size = 0;

   /*=================================*/
   /* Get a copy of the symbol table. */
   /*=================================*/

   symbolArray = GetSymbolTable();

   /*======================================================*/
   /* Get the number of symbols and the total string size. */
   /*======================================================*/

   for (i = 0; i < SYMBOL_HASH_SIZE; i++)
     {
      for (symbolPtr = symbolArray[i];
           symbolPtr != NULL;
           symbolPtr = symbolPtr->next)
        {
         if (symbolPtr->neededSymbol)
           {
            numberOfUsedSymbols++;
            size += strlen(symbolPtr->contents) + 1;
           }
        }
     }

   /*=============================================*/
   /* Write out the symbols and the string sizes. */
   /*=============================================*/

   GenWrite((VOID *) &numberOfUsedSymbols,(unsigned long) sizeof(unsigned long int),fp);
   GenWrite((VOID *) &size,(unsigned long) sizeof(unsigned long int),fp);

   for (i = 0; i < SYMBOL_HASH_SIZE; i++)
     {
      for (symbolPtr = symbolArray[i];
           symbolPtr != NULL;
           symbolPtr = symbolPtr->next)
        {
         if (symbolPtr->neededSymbol)
           {
            length = strlen(symbolPtr->contents) + 1;
            GenWrite((VOID *) symbolPtr->contents,(unsigned long) length,fp);
           }
        }
     }
  }

/*****************************************************************/
/* WriteNeededFloats: Stores all of the floats in the float      */
/*   table needed for this binary image in the binary save file. */
/*****************************************************************/
static VOID WriteNeededFloats(fp)
  FILE *fp;
  {
   int i;
   FLOAT_HN **floatArray;
   FLOAT_HN *floatPtr;
   unsigned long int numberOfUsedFloats = 0;

   /*================================*/
   /* Get a copy of the float table. */
   /*================================*/

   floatArray = GetFloatTable();

   /*===========================*/
   /* Get the number of floats. */
   /*===========================*/

   for (i = 0; i < FLOAT_HASH_SIZE; i++)
     {
      for (floatPtr = floatArray[i];
           floatPtr != NULL;
           floatPtr = floatPtr->next)
        { if (floatPtr->neededFloat) numberOfUsedFloats++; }
     }

   /*======================================================*/
   /* Write out the number of floats and the float values. */
   /*======================================================*/

   GenWrite(&numberOfUsedFloats,(unsigned long) sizeof(unsigned long int),fp);

   for (i = 0 ; i < FLOAT_HASH_SIZE; i++)
     {
      for (floatPtr = floatArray[i];
           floatPtr != NULL;
           floatPtr = floatPtr->next)
        {
         if (floatPtr->neededFloat)
           { GenWrite(&floatPtr->contents,
                      (unsigned long) sizeof(floatPtr->contents),fp); }
        }
     }
  }

/******************************************************************/
/* WriteNeededIntegers: Stores all of the integers in the integer */
/*   table needed for this binary image in the binary save file.  */
/******************************************************************/
static VOID WriteNeededIntegers(fp)
  FILE *fp;
  {
   int i;
   INTEGER_HN **integerArray;
   INTEGER_HN *integerPtr;
   unsigned long int numberOfUsedIntegers = 0;

   /*==================================*/
   /* Get a copy of the integer table. */
   /*==================================*/

   integerArray = GetIntegerTable();

   /*=============================*/
   /* Get the number of integers. */
   /*=============================*/

   for (i = 0 ; i < INTEGER_HASH_SIZE; i++)
     {
      for (integerPtr = integerArray[i];
           integerPtr != NULL;
           integerPtr = integerPtr->next)
        {
         if (integerPtr->neededInteger) numberOfUsedIntegers++;
        }
     }

   /*==========================================================*/
   /* Write out the number of integers and the integer values. */
   /*==========================================================*/

   GenWrite(&numberOfUsedIntegers,(unsigned long) sizeof(unsigned long int),fp);

   for (i = 0 ; i < INTEGER_HASH_SIZE; i++)
     {
      for (integerPtr = integerArray[i];
           integerPtr != NULL;
           integerPtr = integerPtr->next)
        {
         if (integerPtr->neededInteger)
           {
            GenWrite(&integerPtr->contents,
                     (unsigned long) sizeof(integerPtr->contents),fp);
           }
        }
     }
  }
  
/*****************************************************************/
/* WriteNeededBitMaps: Stores all of the bitmaps in the bitmap   */
/*   table needed for this binary image in the binary save file. */
/*****************************************************************/
static VOID WriteNeededBitMaps(fp)
  FILE *fp;
  {
   int i;
   BITMAP_HN **bitMapArray;
   BITMAP_HN *bitMapPtr;
   unsigned long int numberOfUsedBitMaps = 0, size = 0;
   char tempSize;

   /*=================================*/
   /* Get a copy of the bitmap table. */
   /*=================================*/

   bitMapArray = GetBitMapTable();

   /*======================================================*/
   /* Get the number of bitmaps and the total bitmap size. */
   /*======================================================*/

   for (i = 0; i < BITMAP_HASH_SIZE; i++)
     {
      for (bitMapPtr = bitMapArray[i];
           bitMapPtr != NULL;
           bitMapPtr = bitMapPtr->next)
        {
         if (bitMapPtr->neededBitMap)
           {
            numberOfUsedBitMaps++;
            size += bitMapPtr->size + 1;
           }
        }
     }
     
   /*========================================*/
   /* Write out the bitmaps and their sizes. */
   /*========================================*/

   GenWrite((VOID *) &numberOfUsedBitMaps,(unsigned long) sizeof(unsigned long int),fp);
   GenWrite((VOID *) &size,(unsigned long) sizeof(unsigned long int),fp);

   for (i = 0; i < BITMAP_HASH_SIZE; i++)
     {
      for (bitMapPtr = bitMapArray[i];
           bitMapPtr != NULL;
           bitMapPtr = bitMapPtr->next)
        {
         if (bitMapPtr->neededBitMap)
           {
            tempSize = bitMapPtr->size;
            GenWrite((VOID *) &tempSize,(unsigned long) sizeof(char),fp);
            GenWrite((VOID *) bitMapPtr->contents,(unsigned long) bitMapPtr->size,fp);
           }
        }
     }
  }


#if FUZZY_DEFTEMPLATES    /* added 03-12-96 */

/************************************************************************/
/* WriteNeededFuzzyValues: Stores all of the fuzzy values in the        */
/*   symbol table needed for this binary image in the binary save file  */
/************************************************************************/
static VOID WriteNeededFuzzyValues(fp)
  FILE *fp;
  {
   int i;
   FUZZY_VALUE_HN **fuzzyValueArray;
   FUZZY_VALUE_HN *fuzzyValuePtr;
   unsigned long int numberOfUsedFuzzyValues = 0;

   /*=================================*/
   /* Get a copy of the symbol table. */
   /*=================================*/

   fuzzyValueArray = GetFuzzyValueTable();

   /*======================================================*/
   /* Get the number of fuzzy values and the total size.   */
   /*======================================================*/

   for (i = 0; i < FUZZY_VALUE_HASH_SIZE; i++)
     {
      fuzzyValuePtr = fuzzyValueArray[i];
      while (fuzzyValuePtr != NULL)
        {
         if (fuzzyValuePtr->neededFuzzyValue)
            numberOfUsedFuzzyValues++;
         fuzzyValuePtr = fuzzyValuePtr->next;
        }
     }

   /*==================================================*/
   /* Write out the fuzzy values and the sizes.        */
   /*==================================================*/

   GenWrite((VOID *) &numberOfUsedFuzzyValues,(unsigned long) sizeof(unsigned long int),fp);

   for (i = 0; i < FUZZY_VALUE_HASH_SIZE; i++)
     {
      fuzzyValuePtr = fuzzyValueArray[i];
      while (fuzzyValuePtr != NULL)
        {
         struct fuzzy_value *fvptr;
         long deftemplateBsaveID;
         int fvn, nameLen;
		 
         if (fuzzyValuePtr->neededFuzzyValue)
           {
            fvptr = (struct fuzzy_value *)fuzzyValuePtr->contents;
            /* write out the deftemplate ptr as the bsaveID of the
               deftemplate - if null write a -1L
            */
            if (fvptr->whichDeftemplate == NULL)
               deftemplateBsaveID = -1L;
            else
               deftemplateBsaveID = ((struct constructHeader *)fvptr->whichDeftemplate)->bsaveID;
            GenWrite((VOID *) &deftemplateBsaveID,(unsigned long) sizeof(long),fp);
            /* write the length of the name string and the string */
            nameLen = strlen(fvptr->name)+1;
            GenWrite((VOID *) &nameLen,(unsigned long) sizeof(int),fp);
            GenWrite((VOID *) fvptr->name,(unsigned long) sizeof(char)*nameLen,fp);
            /* write the size of the x and y arrays */
            fvn = fvptr->n;
            GenWrite((VOID *) &fvn,(unsigned long) sizeof(int),fp);
            /* write the x and y arrays */
            GenWrite((VOID *) fvptr->x,(unsigned long) sizeof(double)*fvn,fp);
            GenWrite((VOID *) fvptr->y,(unsigned long) sizeof(double)*fvn,fp);
           }
         fuzzyValuePtr = fuzzyValuePtr->next;
        }
     }
  }

#endif
  
#endif /* BLOAD_AND_BSAVE || BSAVE_INSTANCES */

/*********************************************/
/* ReadNeededAtomicValues: Read all symbols, */
/*   floats, integers, and bitmaps needed by */
/*   this binary image from the binary file. */
/*********************************************/
globle VOID ReadNeededAtomicValues()
  {
   ReadNeededSymbols();
   ReadNeededFloats();
   ReadNeededIntegers();
   ReadNeededBitMaps();
#if FUZZY_DEFTEMPLATES    /* added 03-12-96 */

   /* this MUST be done after ReadNeededSymbols 
      because of the symbol HN in a fuzzy value
   */
   ReadNeededFuzzyValues();  
#endif
  }

/*******************************************/
/* ReadNeededSymbols: Reads in the symbols */
/*   used by the binary image.             */
/*******************************************/
static VOID ReadNeededSymbols()
  {
   char *symbolNames, *namePtr;
   unsigned long space;
   long i;

   /*=================================================*/
   /* Determine the number of symbol names to be read */
   /* and space required for them.                    */
   /*=================================================*/

   GenRead((VOID *) &NumberOfSymbols,(unsigned long) sizeof(long int));
   GenRead(&space,(unsigned long) sizeof(unsigned long int));
   if (NumberOfSymbols == 0)
     {
      SymbolArray = NULL;
      return;
     }

   /*=======================================*/
   /* Allocate area for strings to be read. */
   /*=======================================*/

   symbolNames = (char *) gm3((long) space);
   GenRead((VOID *) symbolNames,space);

   /*================================================*/
   /* Store the symbol pointers in the symbol array. */
   /*================================================*/

   SymbolArray = (SYMBOL_HN **)
                 gm3((long) sizeof(SYMBOL_HN *) *  NumberOfSymbols);
   namePtr = symbolNames;
   for (i = 0; i < NumberOfSymbols; i++)
     {
      SymbolArray[i] = (SYMBOL_HN *) AddSymbol(namePtr);
      namePtr += strlen(namePtr) + 1;
     }

   /*=======================*/
   /* Free the name buffer. */
   /*=======================*/

   rm3((VOID *) symbolNames,(long) space);
  }

/*****************************************/
/* ReadNeededFloats: Reads in the floats */
/*   used by the binary image.           */
/*****************************************/
static VOID ReadNeededFloats()
  {
   double HUGE_ADDR *floatValues;
   long i;

   /*============================================*/
   /* Determine the number of floats to be read. */
   /*============================================*/

   GenRead(&NumberOfFloats,(unsigned long) sizeof(long int));
   if (NumberOfFloats == 0)
     {
      FloatArray = NULL;
      return;
     }

   /*===============================*/
   /* Allocate area for the floats. */
   /*===============================*/

   floatValues = (double *) gm3((long) sizeof(double) * NumberOfFloats);
   GenRead((VOID *) floatValues,(unsigned long) (sizeof(double) * NumberOfFloats));

   /*======================================*/
   /* Store the floats in the float array. */
   /*======================================*/

   FloatArray = (FLOAT_HN **)
               gm3((long) sizeof(FLOAT_HN *) * NumberOfFloats);
   for (i = 0; i < NumberOfFloats; i++)
     { FloatArray[i] = (FLOAT_HN *) AddDouble(floatValues[i]); }

   /*========================*/
   /* Free the float buffer. */
   /*========================*/

   rm3((VOID *) floatValues,(long) (sizeof(double) * NumberOfFloats));
  }

/*********************************************/
/* ReadNeededIntegers: Reads in the integers */
/*   used by the binary image.               */
/*********************************************/
static VOID ReadNeededIntegers()
  {
   long int HUGE_ADDR *integerValues;
   long i;

   /*==============================================*/
   /* Determine the number of integers to be read. */
   /*==============================================*/

   GenRead(&NumberOfIntegers,(unsigned long) sizeof(unsigned long int));
   if (NumberOfIntegers == 0)
     {
      IntegerArray = NULL;
      return;
     }
     
   /*=================================*/
   /* Allocate area for the integers. */
   /*=================================*/

   integerValues = (long *) gm3((long) (sizeof(long) * NumberOfIntegers));
   GenRead((VOID *) integerValues,(unsigned long) (sizeof(long) * NumberOfIntegers));

   /*==========================================*/
   /* Store the integers in the integer array. */
   /*==========================================*/

   IntegerArray = (INTEGER_HN **)
           gm3((long) (sizeof(INTEGER_HN *) * NumberOfIntegers));
   for (i = 0; i < NumberOfIntegers; i++)
     { IntegerArray[i] = (INTEGER_HN *) AddLong(integerValues[i]); }

   /*==========================*/
   /* Free the integer buffer. */
   /*==========================*/

   rm3((VOID *) integerValues,(long) (sizeof(long int) * NumberOfIntegers));
  }
  
/*******************************************/
/* ReadNeededBitMaps: Reads in the bitmaps */
/*   used by the binary image.             */
/*******************************************/
static VOID ReadNeededBitMaps()
  {
   char *bitMapStorage, *bitMapPtr;
   unsigned long space;
   long i;

   /*=======================================*/
   /* Determine the number of bitmaps to be */
   /* read and space required for them.     */
   /*=======================================*/

   GenRead((VOID *) &NumberOfBitMaps,(unsigned long) sizeof(long int));
   GenRead(&space,(unsigned long) sizeof(unsigned long int));
   if (NumberOfBitMaps == 0)
     {
      BitMapArray = NULL;
      return;
     }
     
   /*=======================================*/
   /* Allocate area for bitmaps to be read. */
   /*=======================================*/

   bitMapStorage = (char *) gm3((long) space);
   GenRead((VOID *) bitMapStorage,space);

   /*================================================*/
   /* Store the bitMap pointers in the bitmap array. */
   /*================================================*/

   BitMapArray = (BITMAP_HN **)
                 gm3((long) sizeof(BITMAP_HN *) *  NumberOfBitMaps);
   bitMapPtr = bitMapStorage;
   for (i = 0; i < NumberOfBitMaps; i++)
     {
      BitMapArray[i] = (BITMAP_HN *) AddBitMap(bitMapPtr+1,(int) *bitMapPtr);
      bitMapPtr += *bitMapPtr + 1;
     }

   /*=========================*/
   /* Free the bitmap buffer. */
   /*=========================*/

   rm3((VOID *) bitMapStorage,(long) space);
  }

#if FUZZY_DEFTEMPLATES    /* added 03-12-96 */


/****************************************************/
/* ReadNeededFuzzyValues: Reads in the fuzzy values */
/*   saved by the binary image.                     */
/****************************************************/
static VOID ReadNeededFuzzyValues()
  {
   long i;
   struct fuzzy_value *fvptr;
   long namebucket;
   long deftemplateBsaveID;
   int nameLen;

   /*==================================================*/
   /* Determine the number of fuzzy values to be read. */
   /*==================================================*/

   GenRead(&NumberOfFuzzyValues,(unsigned long) sizeof(long int));
   if (NumberOfFuzzyValues == 0)
     {
      FuzzyValueArray = NULL;
      return;
     }

   /*==================================================*/
   /* Store the fuzzy values in the fuzzy value array. */
   /*==================================================*/

   FuzzyValueArray = (FUZZY_VALUE_HN **)
               gm3((long) sizeof(FUZZY_VALUE_HN *) * NumberOfFuzzyValues);
			   
   for (i = 0; i < NumberOfFuzzyValues; i++)
     { 
	   /* get a fuzzy value structure and put the contents into it */
	   fvptr = get_struct(fuzzy_value);
	   
       GenRead(&deftemplateBsaveID,(unsigned long) sizeof(long));
/* At this time Instances do not use fuzzy values -- later if this capability
   is added then we will need to save detemplates when instances can be
   saved since fuzzy values need them!!
*/
#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
       if (deftemplateBsaveID == -1)
          fvptr->whichDeftemplate = NULL;
       else
          fvptr->whichDeftemplate = DeftemplatePointer(deftemplateBsaveID);
#else
       fvptr->whichDeftemplate = NULL;
#endif
      GenRead(&nameLen, (unsigned long) sizeof(int));
      fvptr->name = gm2(nameLen);
      GenRead(fvptr->name, (unsigned long) sizeof(char)*nameLen);
	   GenRead(&fvptr->n,(unsigned long) sizeof(int));
	   fvptr->maxn = fvptr->n;
	   fvptr->x = FgetArray(fvptr->n);
	   fvptr->y = FgetArray(fvptr->n);
	   GenRead(fvptr->x,(unsigned long) sizeof(double) * fvptr->n);
	   GenRead(fvptr->y,(unsigned long) sizeof(double) * fvptr->n);
	   
	   FuzzyValueArray[i] = (FUZZY_VALUE_HN *) AddFuzzyValue(fvptr);
	   
	   /* must return the fuzzy value structure since AddFuzzyValue makes a copy */
	   rtnFuzzyValue(fvptr);
	 }
    
  }


#endif /* FUZZY_DEFTEMPLATES */

  
/**********************************************************/
/* FreeAtomicValueStorage: Returns the memory allocated   */
/*   for storing the pointers to atomic data values used  */
/*   in refreshing expressions and other data structures. */
/**********************************************************/
globle VOID FreeAtomicValueStorage()
  {
   if (SymbolArray != NULL)
     rm3((VOID *) SymbolArray,(long) sizeof(SYMBOL_HN *) * NumberOfSymbols);
   if (FloatArray != NULL)
     rm3((VOID *) FloatArray,(long) sizeof(FLOAT_HN *) * NumberOfFloats);
   if (IntegerArray != NULL)
     rm3((VOID *) IntegerArray,(long) sizeof(INTEGER_HN *) * NumberOfIntegers);
   if (BitMapArray != NULL)
     rm3((VOID *) BitMapArray,(long) sizeof(BITMAP_HN *) * NumberOfBitMaps);
#if FUZZY_DEFTEMPLATES   /* added 03-12-96 */
   if (FuzzyValueArray != NULL)
     rm3((VOID *) FuzzyValueArray,(long) sizeof(FUZZY_VALUE_HN *) * NumberOfFuzzyValues);
#endif
  }
  
#endif /* BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE || BLOAD_INSTANCES || BSAVE_INSTANCES */
