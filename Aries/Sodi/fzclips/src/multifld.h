   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  10/14/93            */
   /*                                                     */
   /*                MULTIFIELD HEADER FILE               */
   /*******************************************************/

/*************************************************************/
/* Purpose: Routines for creating and manipulating           */
/*   multifield values.                                      */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_setup
#include "setup.h"
#endif

#if FUZZY_DEFTEMPLATES
/* Due to a circular set of definitions we need to do this
   evaluatn.h includes symbol.h which includes fuzzyval.h 
   which includes tmpltdef.h which includes ... .h 
   which includes multifld.h => problem!!
   This effectively makes the include of evaluatn.h an
   include of symbol.h to make sure tmpltdef is included 
   before evaluatn!
*/

#ifndef _H_symbol
#include "symbol.h"
#endif
#endif


#ifndef _H_multifld

#define _H_multifld

struct field;
struct multifield;

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

struct field
  {
   short int type;
   VOID *value;
  };
    
struct multifield
  {
   unsigned busyCount;
   short depth;
   long multifieldLength;   /* changed 03-11-96 */
   struct multifield *next;
   struct field theFields[1];
  };

typedef struct multifield SEGMENT;
typedef struct multifield * SEGMENT_PTR;
typedef struct multifield * MULTIFIELD_PTR;
typedef struct field FIELD;
typedef struct field * FIELD_PTR;

#define GetMFLength(target)     (((struct multifield *) (target))->multifieldLength) 
#define GetMFPtr(target,index)  (&(((struct field HUGE_ADDR *) ((struct multifield *) (target))->theFields)[index-1])) 
#define SetMFType(target,index,value)  (((struct field HUGE_ADDR *) ((struct multifield *) (target))->theFields)[index-1].type = (value)) 
#define SetMFValue(target,index,val)  (((struct field HUGE_ADDR *) ((struct multifield *) (target))->theFields)[index-1].value = (VOID *) (val))  
#define GetMFType(target,index)  (((struct field HUGE_ADDR *) ((struct multifield *) (target))->theFields)[index-1].type) 
#define GetMFValue(target,index)  (((struct field HUGE_ADDR *) ((struct multifield *) (target))->theFields)[index-1].value) 

#ifdef LOCALE
#undef LOCALE
#endif
#ifdef _MULTIFLD_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ANSI_COMPILER
   LOCALE VOID                          *CreateMultifield2(long);
   LOCALE VOID                           ReturnMultifield(struct multifield *);
   LOCALE VOID                           MultifieldInstall(struct multifield *);
   LOCALE VOID                           MultifieldDeinstall(struct multifield *);
   LOCALE struct multifield             *StringToMultifield(char *);
   LOCALE VOID                          *CreateMultifield(long);
   LOCALE VOID                           AddToMultifieldList(struct multifield *);
   LOCALE VOID                           FlushMultifields(void);
   LOCALE VOID                           DuplicateMultifield(struct dataObject *,struct dataObject *);
   LOCALE VOID                           PrintMultifield(char *,SEGMENT_PTR,long,long,int);  /* changed 03-11-96 */
   LOCALE BOOLEAN                        MultifieldDOsEqual(DATA_OBJECT_PTR,DATA_OBJECT_PTR);
   LOCALE VOID                           StoreInMultifield(DATA_OBJECT *,EXPRESSION *,int);
   LOCALE VOID                          *CopyMultifield(struct multifield *);
   LOCALE BOOLEAN                        MultifieldsEqual(struct multifield *,struct multifield *);
   LOCALE VOID                          *DOToMultifield(DATA_OBJECT *);
#else
   LOCALE VOID                          *CreateMultifield2();
   LOCALE VOID                           ReturnMultifield();
   LOCALE VOID                           MultifieldInstall();
   LOCALE VOID                           MultifieldDeinstall();
   LOCALE struct multifield             *StringToMultifield();
   LOCALE VOID                          *CreateMultifield();
   LOCALE VOID                           AddToMultifieldList();
   LOCALE VOID                           FlushMultifields();
   LOCALE VOID                           DuplicateMultifield();
   LOCALE VOID                           PrintMultifield();
   LOCALE BOOLEAN                        MultifieldDOsEqual();
   LOCALE VOID                           StoreInMultifield();
   LOCALE VOID                          *CopyMultifield();
   LOCALE BOOLEAN                        MultifieldsEqual();
   LOCALE VOID                          *DOToMultifield();
#endif

#endif





