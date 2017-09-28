   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  06/17/94            */
   /*                                                     */
   /*           DEFRULE BSAVE/BLOAD HEADER FILE           */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the binary save/load feature for the  */
/*    defrule construct.                                     */
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

#if (! RUN_TIME)
#ifndef _H_rulebin

#define _H_rulebin

#include "modulbin.h"
#include "cstrcbin.h"
#ifndef _H_network
#include "network.h"
#endif

#if FUZZY_DEFTEMPLATES
/* These structs are added at the end of rule structs
   that have patterns that have FUZZY slot references.
   They record for each fuzzy slot the pattern number
   in the rule and the slot number within the pattern
   as well as the ptr to that fuzzy values hash node.
*/
struct bsaveFzSlotLocator
  {
    unsigned int patternNum;
    unsigned int slotNum : 13;
    long     fvhnPtr;
  };
#endif
    
struct bsaveDefrule
  {
   struct bsaveConstructHeader header;
   int salience;
   int localVarCnt;
   unsigned int complexity      : 12;
   unsigned int autoFocus       :  1;
   long dynamicSalience;
   long actions;
   long logicalJoin;
   long lastJoin;
   long disjunct;
#if CERTAINTY_FACTORS
   double CF;
   long  dynamicCF;
#endif
#if FUZZY_DEFTEMPLATES
   double       min_of_maxmins;
   unsigned int lhsRuleType;
   unsigned int numberOfFuzzySlots;
   long         pattern_fv_arrayPtr;
#endif
  };
  
struct bsavePatternNodeHeader
  {
   long entryJoin;
   unsigned int singlefieldNode : 1;
   unsigned int multifieldNode : 1;
   unsigned int stopNode : 1;
   unsigned int blocked : 1; 
   unsigned int initialize : 1;
   unsigned int marked : 1;
   unsigned int beginSlot : 1;
   unsigned int endSlot : 1;
  };
  
struct bsaveDefruleModule
  {
   struct bsaveDefmoduleItemHeader header;
  };
  
struct bsaveJoinNode
  { 
   unsigned int firstJoin : 1;
   unsigned int logicalJoin : 1;
   unsigned int joinFromTheRight : 1;
   unsigned int patternIsNegated : 1;
   unsigned int rhsType : 3;
   unsigned int depth : 7;
   long networkTest;
   long rightSideEntryStructure;
   long nextLevel;
   long lastLevel;
   long rightDriveNode;
   long rightMatchNode;
   long ruleToActivate;
  };

#define BloadDefrulePointer(x,i) ((struct defrule *) (( i == -1L) ? NULL : &x[i]))
#define BsaveJoinIndex(joinPtr) ((joinPtr == NULL) ? -1L :  ((struct joinNode *) joinPtr)->bsaveID)
#define BloadJoinPointer(i) ((struct joinNode *) ((i == -1L) ? NULL : &JoinArray[i]))

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _RULEBIN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ANSI_COMPILER   
   LOCALE VOID                           DefruleBinarySetup(void);
   LOCALE VOID                           UpdatePatternNodeHeader(struct patternNodeHeader *,
                                                                 struct bsavePatternNodeHeader *);
   LOCALE VOID                           AssignBsavePatternHeaderValues(struct bsavePatternNodeHeader *,
                                                                 struct patternNodeHeader *);
   LOCALE VOID                          *BloadDefruleModuleReference(int);
#else
   LOCALE VOID                           DefruleBinarySetup();
   LOCALE VOID                           UpdatePatternNodeHeader();
   LOCALE VOID                           AssignBsavePatternHeaderValues();
   LOCALE VOID                          *BloadDefruleModuleReference();
#endif

#endif
#endif






