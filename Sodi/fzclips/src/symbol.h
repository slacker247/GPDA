   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.10  04/19/94            */
   /*                                                     */
   /*                 SYMBOL HEADER FILE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: Manages the atomic data value hash tables for    */
/*   storing symbols, integers, floats, and bit maps.        */
/*   Contains routines for adding entries, examining the     */
/*   hash tables, and performing garbage collection to       */
/*   remove entries no longer in use.                        */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Bob Orchard (NRCC - Nat'l Research Council of Canada)*/
/*                  (Fuzzy reasoning extensions)             */
/*                  (certainty factors for facts and rules)  */
/*                  (extensions to run command)              */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_symbol
#define _H_symbol

#ifndef _H_setup    /* added 03-13-96 */
#include "setup.h"
#endif

struct symbolHashNode;
struct floatHashNode;
struct integerHashNode;
struct bitMapHashNode;
#if FUZZY_DEFTEMPLATES    /* added 03-13-96 */
struct fuzzyValueHashNode;
#endif
struct genericHashNode;
struct symbolMatch;


/* position relocated 03-13-96 */
/* These typedefs must be with the corresponding declarations */
typedef struct symbolHashNode SYMBOL_HN;
typedef struct floatHashNode FLOAT_HN;
typedef struct integerHashNode INTEGER_HN;
typedef struct bitMapHashNode BITMAP_HN;
#if FUZZY_DEFTEMPLATES    /* added 03-13-96 */
typedef struct fuzzyValueHashNode FUZZY_VALUE_HN;
#endif
typedef struct genericHashNode GENERIC_HN;


#if FUZZY_DEFTEMPLATES    /* added 03-13-96 */
#ifndef _H_fuzzyval
#include "fuzzyval.h"
#endif
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _SYMBOL_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define SYMBOL_HASH_SIZE        1013
#define FLOAT_HASH_SIZE          503 
#define INTEGER_HASH_SIZE        167 
#define BITMAP_HASH_SIZE         167
#if FUZZY_DEFTEMPLATES    /* added 03-13-96 */
#define FUZZY_VALUE_HASH_SIZE    167
#endif

/************************************************************/
/* symbolHashNode STRUCTURE:                                */
/************************************************************/
struct symbolHashNode
  {
   struct symbolHashNode *next;
   long count;
   int depth;
   unsigned int markedEphemeral : 1;
   unsigned int neededSymbol : 1;
   unsigned int bucket : 14;
   char *contents;
  };
  
/************************************************************/
/* floatHashNode STRUCTURE:                                  */
/************************************************************/
struct floatHashNode
  {
   struct floatHashNode *next;
   long count;
   int depth;
   unsigned int markedEphemeral : 1;
   unsigned int neededFloat : 1;
   unsigned int bucket : 14;
   double contents;
  };

/************************************************************/
/* integerHashNode STRUCTURE:                               */
/************************************************************/
struct integerHashNode
  {
   struct integerHashNode *next;
   long count;
   int depth;
   unsigned int markedEphemeral : 1;
   unsigned int neededInteger : 1;
   unsigned int bucket : 14;
   long int contents;
  };
  
/************************************************************/
/* bitMapHashNode STRUCTURE:                                */
/************************************************************/
struct bitMapHashNode
  {
   struct bitMapHashNode *next;
   long count;
   int depth;
   unsigned int markedEphemeral : 1;
   unsigned int neededBitMap : 1;
   unsigned int bucket : 14;
   char *contents;
   unsigned short size;
  };

#if FUZZY_DEFTEMPLATES    /* added 03-13-96 */
  
/************************************************************/
/* fuzzyValueHashNode STRUCTURE:                            */
/************************************************************/
struct fuzzyValueHashNode
  {
   struct fuzzyValueHashNode *next;
   long count;
   int depth;
   unsigned int markedEphemeral : 1;
   unsigned int neededFuzzyValue : 1;
   unsigned int bucket : 14;
   struct fuzzy_value *contents;
  };

#endif
  
/************************************************************/
/* genericHashNode STRUCTURE:                               */
/************************************************************/
struct genericHashNode
  {
   struct genericHashNode *next;
   long count;
   int depth;
   unsigned int markedEphemeral : 1;
   unsigned int needed : 1;
   unsigned int bucket : 14;
  };
  
/************************************************************/
/* symbolMatch STRUCTURE:                               */
/************************************************************/
struct symbolMatch
  {
   struct symbolHashNode *match;
   struct symbolMatch *next;
  };
 
#define ValueToString(target) (((struct symbolHashNode *) (target))->contents)
#define ValueToDouble(target) (((struct floatHashNode *) (target))->contents)
#define ValueToLong(target) (((struct integerHashNode *) (target))->contents)
#define ValueToInteger(target) ((int) (((struct integerHashNode *) (target))->contents))
#define ValueToBitMap(target) ((VOID *) ((struct bitMapHashNode *) (target))->contents)
#if FUZZY_DEFTEMPLATES    /* added 03-13-96 */
#define ValueToFuzzyValue(target) (((struct fuzzyValueHashNode *) (target))->contents)
#endif

#define IncrementSymbolCount(theValue) (((SYMBOL_HN *) theValue)->count++)
#define IncrementFloatCount(theValue) (((FLOAT_HN *) theValue)->count++)
#define IncrementIntegerCount(theValue) (((INTEGER_HN *) theValue)->count++)
#define IncrementBitMapCount(theValue) (((BITMAP_HN *) theValue)->count++)
#if FUZZY_DEFTEMPLATES    /* added 03-13-96 */
#define IncrementFuzzyValueCount(theValue) (((FUZZY_VALUE_HN *) theValue)->count++)
#endif

/*****************************************************/
/* The FindSymbol function is remapped under certain */
/* conditions because it conflicts with a Metroworks */
/* Code Warrior library function.                    */
/*****************************************************/
#if MAC_MCW    /* added 03-13-96 */
#define FindSymbol MCWFindSymbol  
#endif

#if ANSI_COMPILER
   LOCALE VOID                          *AddSymbol(char *);
   LOCALE SYMBOL_HN                     *FindSymbol(char *);
   LOCALE VOID                          *AddDouble(double);
   LOCALE VOID                          *AddLong(long int);
   LOCALE VOID                          *AddBitMap(VOID *,int);
   LOCALE INTEGER_HN                    *FindLong(long int);
   LOCALE VOID                           InitializeAtomTables(void);
   LOCALE int                            HashSymbol(char *,int);
   LOCALE int                            HashFloat(double,int);
   LOCALE int                            HashInteger(long int,int);
   LOCALE int                            HashBitMap(char *,int,int);
   LOCALE VOID                           DecrementSymbolCount(struct symbolHashNode *);
   LOCALE VOID                           DecrementFloatCount(struct floatHashNode *);
   LOCALE VOID                           DecrementIntegerCount(struct integerHashNode *);
   LOCALE VOID                           DecrementBitMapCount(struct bitMapHashNode *);
   LOCALE VOID                           RemoveEphemeralAtoms(void); 
   LOCALE struct symbolHashNode        **GetSymbolTable(void);
   LOCALE VOID                           SetSymbolTable(struct symbolHashNode **);
   LOCALE struct floatHashNode          **GetFloatTable(void);
   LOCALE VOID                           SetFloatTable(struct floatHashNode **);
   LOCALE struct integerHashNode       **GetIntegerTable(void);
   LOCALE VOID                           SetIntegerTable(struct integerHashNode **);
   LOCALE struct bitMapHashNode        **GetBitMapTable(void);
   LOCALE VOID                           SetBitMapTable(struct bitMapHashNode **);
   LOCALE VOID                           RefreshSpecialSymbols(void);
   LOCALE struct symbolMatch            *FindSymbolMatches(char *,int *,int *);
   LOCALE VOID                           ReturnSymbolMatches(struct symbolMatch *);
   LOCALE SYMBOL_HN                     *GetNextSymbolMatch(char *,int,SYMBOL_HN *,int,int *);
   LOCALE VOID                           ClearBitString(VOID *,int);
   LOCALE VOID                           SetAtomicValueIndices(int);
   LOCALE VOID                           RestoreAtomicValueBuckets(void);
#if FUZZY_DEFTEMPLATES     /* added 03-13-96 */
   LOCALE VOID                          *AddFuzzyValue(struct fuzzy_value *);
   LOCALE int                            HashFuzzyValue(struct fuzzy_value *,int);
   LOCALE VOID                           DecrementFuzzyValueCount(struct fuzzyValueHashNode *);
   LOCALE struct fuzzyValueHashNode     **GetFuzzyValueTable(void);
   LOCALE VOID                           SetFuzzyValueTable(struct fuzzyValueHashNode **);
#endif
#else
   LOCALE VOID                          *AddSymbol();
   LOCALE SYMBOL_HN                     *FindSymbol();
   LOCALE VOID                          *AddDouble();
   LOCALE VOID                          *AddLong();
   LOCALE VOID                          *AddBitMap();
   LOCALE INTEGER_HN                    *FindLong();
   LOCALE VOID                           InitializeAtomTables();
   LOCALE int                            HashSymbol();
   LOCALE int                            HashFloat();
   LOCALE int                            HashInteger();
   LOCALE int                            HashBitMap();
   LOCALE VOID                           DecrementSymbolCount();
   LOCALE VOID                           DecrementFloatCount();
   LOCALE VOID                           DecrementIntegerCount();
   LOCALE VOID                           DecrementBitMapCount();
   LOCALE VOID                           RemoveEphemeralAtoms(); 
   LOCALE struct symbolHashNode        **GetSymbolTable();
   LOCALE VOID                           SetSymbolTable();
   LOCALE struct floatHashNode          **GetFloatTable();
   LOCALE VOID                           SetFloatTable();
   LOCALE struct integerHashNode       **GetIntegerTable();
   LOCALE VOID                           SetIntegerTable();
   LOCALE struct bitMapHashNode        **GetBitMapTable();
   LOCALE VOID                           SetBitMapTable();
   LOCALE VOID                           RefreshSpecialSymbols();
   LOCALE struct symbolMatch            *FindSymbolMatches();
   LOCALE VOID                           ReturnSymbolMatches();
   LOCALE SYMBOL_HN                     *GetNextSymbolMatch();
   LOCALE VOID                           ClearBitString();
   LOCALE VOID                           SetAtomicValueIndices();
   LOCALE VOID                           RestoreAtomicValueBuckets();
#if FUZZY_DEFTEMPLATES    /* added 03-13-96 */
   LOCALE VOID                          *AddFuzzyValue();
   LOCALE int                            HashFuzzyValue();
   LOCALE VOID                           DecrementFuzzyValueCount();
   LOCALE struct fuzzyValueHashNode     **GetFuzzyValueTable();
   LOCALE VOID                           SetFuzzyValueTable();
#endif
#endif


#ifndef _SYMBOL_SOURCE
   extern VOID                   *CLIPSTrueSymbol;
   extern VOID                   *CLIPSFalseSymbol;
   extern VOID                   *NegativeInfinity;
   extern VOID                   *PositiveInfinity;
   extern VOID                   *Zero;
#endif

#endif



