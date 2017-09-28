   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  10/14/93            */
   /*                                                     */
   /*                PRINT UTILITY MODULE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Utility routines for printing various items      */
/*   and messages.                                           */
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

#define _PRNTUTIL_SOURCE_

#include <stdio.h>
#define _CLIPS_STDIO_
#include <string.h>

#include "setup.h"

#include "constant.h"
#include "symbol.h"
#include "utility.h"
#include "evaluatn.h"
#include "argacces.h"
#include "router.h"

#include "prntutil.h"

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   globle BOOLEAN              PreserveEscapedCharacters = CLIPS_FALSE;
   globle BOOLEAN              AddressesToStrings = CLIPS_FALSE;
   globle BOOLEAN              InstanceAddressesToNames = CLIPS_FALSE;

/***********************************************************/
/* PrintInChunks:  Prints a string in chunks to accomodate */
/*   systems which have a limit on the maximum size of a   */
/*   string which can be printed.                          */
/***********************************************************/
globle VOID PrintInChunks(logicalName,bigString) /*changed 03-11-96*/
  char *logicalName, *bigString;   /*changed 03-11-96*/
  {
   char tc, *subString;        /*changed 03-11-96*/

   subString = bigString;      /*changed 03-11-96*/

   if (subString == NULL) return;

   while (((int) strlen(subString)) > 500)
     {
      tc = subString[500];
      subString[500] = EOS;
      PrintCLIPS(logicalName,subString);
      subString[500] = tc;
      subString += 500;
     }

   PrintCLIPS(logicalName,subString);
  }

/************************************************************/
/* PrintFloat: Controls printout of floating point numbers. */
/************************************************************/
globle VOID PrintFloat(fileid,number)
  char *fileid;
  double number;
  {
   char *theString;

   theString = FloatToString(number);
   PrintCLIPS(fileid,theString);
  }

/****************************************************/
/* PrintLongInteger: Controls printout of integers. */
/****************************************************/
globle VOID PrintLongInteger(logicalName,number)
  char *logicalName;      /*changed 03-11-96*/
  long int number;
  {
   char printBuffer[32];

   sprintf(printBuffer,"%ld",number);
   PrintCLIPS(logicalName,printBuffer);
  }

#if FUZZY_DEFTEMPLATES
/*********************************************************/
/* PrintFuzzyValue:  Controls printout of fuzzy values. */
/*********************************************************/
globle VOID PrintFuzzyValue(fileid,fv)
  char *fileid;
  struct fuzzy_value *fv;
  {
   PrintCLIPS(fileid,fv->name);
  }

#endif
  
/**************************************/
/* PrintAtom: Prints an atomic value. */
/**************************************/
globle VOID PrintAtom(logicalName,type,value)
  char *logicalName;
  int type;
  VOID *value;
  {
   char buffer[20];

   switch (type)
     {
      case FLOAT:
        PrintFloat(logicalName,ValueToDouble(value));
        break;
      case INTEGER:
        PrintLongInteger(logicalName,ValueToLong(value));
        break;
      case SYMBOL:
        PrintCLIPS(logicalName,ValueToString(value));
        break;
      case STRING:
        if (PreserveEscapedCharacters)
          { PrintCLIPS(logicalName,StringPrintForm(ValueToString(value))); }
        else
          {
           PrintCLIPS(logicalName,"\"");
           PrintCLIPS(logicalName,ValueToString(value));
           PrintCLIPS(logicalName,"\"");
          }
        break;

      case EXTERNAL_ADDRESS:
        if (AddressesToStrings) PrintCLIPS(logicalName,"\"");
        PrintCLIPS(logicalName,"<Pointer-");
#if ANSI_COMPILER
        sprintf(buffer,"%p",value);
#else
        sprintf(buffer,"%lu",(unsigned long) value);
#endif
        PrintCLIPS(logicalName,buffer);
        PrintCLIPS(logicalName,">");
        if (AddressesToStrings) PrintCLIPS(logicalName,"\"");
        break;

#if OBJECT_SYSTEM
      case INSTANCE_NAME:
        PrintCLIPS(logicalName,"[");
        PrintCLIPS(logicalName,ValueToString(value));
        PrintCLIPS(logicalName,"]");
        break;
#endif

#if FUZZY_DEFTEMPLATES   /* added 03-11-96 */
      case FUZZY_VALUE:
        PrintFuzzyValue(logicalName,ValueToFuzzyValue(value));
        break;
#endif

      case RVOID:       /* added 03-11-96 */
        break;
        
      default:
        if (PrimitivesArray[type] == NULL) break;
        if (PrimitivesArray[type]->longPrintFunction == NULL)
          {
           PrintCLIPS(logicalName,"<unknown atom type>");
           break;
          } 
        (*PrimitivesArray[type]->longPrintFunction)(logicalName,value);
        break;
     }
  }
  
/**********************************************************/
/* PrintTally: Prints a tally count indicating the number */
/*   of items that have been displayed. Used by functions */
/*   such as list-defrules.                               */
/**********************************************************/
globle VOID PrintTally(logicalName,count,singular,plural)
  char *logicalName;
  long count;
  char *singular, *plural;
  {
   if (count == 0) return;

   PrintCLIPS(logicalName,"For a total of ");
   PrintLongInteger(logicalName,count);
   PrintCLIPS(logicalName," ");

   if (count == 1) PrintCLIPS(logicalName,singular);
   else PrintCLIPS(logicalName,plural);

   PrintCLIPS(logicalName,".\n");
  }

/********************************************/
/* PrintErrorID: Prints the module name and */
/*   error ID for an error message.         */
/********************************************/
globle VOID PrintErrorID(module,errorID,printCR)
  char *module;
  int errorID;
  int printCR;
  {
   if (printCR) PrintCLIPS(WERROR,"\n");
   PrintCLIPS(WERROR,"[");
   PrintCLIPS(WERROR,module);
   PrintLongInteger(WERROR,(long int) errorID);
   PrintCLIPS(WERROR,"] ");
  }
  
/**********************************************/
/* PrintWarningID: Prints the module name and */
/*   warning ID for a warning message.        */
/**********************************************/
globle VOID PrintWarningID(module,warningID,printCR)
  char *module;
  int warningID;
  int printCR;
  {
   if (printCR) PrintCLIPS(WWARNING,"\n");
   PrintCLIPS(WWARNING,"[");
   PrintCLIPS(WWARNING,module);
   PrintLongInteger(WWARNING,(long int) warningID);
   PrintCLIPS(WWARNING,"] WARNING: ");
  }
  
/***************************************************/
/* CantFindItemErrorMessage: Generic error message */
/*  when an "item" can not be found.               */                 
/***************************************************/
globle VOID CantFindItemErrorMessage(itemType,itemName)
  char *itemType;
  char *itemName;
  {
   PrintErrorID("PRNTUTIL",1,CLIPS_FALSE);
   PrintCLIPS(WERROR,"Unable to find ");
   PrintCLIPS(WERROR,itemType);
   PrintCLIPS(WERROR," ");
   PrintCLIPS(WERROR,itemName);
   PrintCLIPS(WERROR,".\n");
  }
  
/*****************************************************/
/* CantDeleteItemErrorMessage: Generic error message */
/*  when an "item" can not be deleted.               */ 
/*****************************************************/
globle VOID CantDeleteItemErrorMessage(itemType,itemName)
  char *itemType;
  char *itemName;
  {
   PrintErrorID("PRNTUTIL",4,CLIPS_FALSE);
   PrintCLIPS(WERROR,"Unable to delete ");
   PrintCLIPS(WERROR,itemType);
   PrintCLIPS(WERROR," ");
   PrintCLIPS(WERROR,itemName);
   PrintCLIPS(WERROR,".\n");
  }

/****************************************************/
/* AlreadyParsedErrorMessage: Generic error message */
/*  when an "item" has already been parsed.         */ 
/****************************************************/
globle VOID AlreadyParsedErrorMessage(itemType,itemName)
  char *itemType;
  char *itemName;
  {
   PrintErrorID("PRNTUTIL",5,CLIPS_TRUE);
   PrintCLIPS(WERROR,"The ");
   if (itemType != NULL) PrintCLIPS(WERROR,itemType);
   if (itemName != NULL) PrintCLIPS(WERROR,itemName);
   PrintCLIPS(WERROR," has already been parsed.\n");
  }
    
/*********************************************************/
/* SyntaxErrorMessage: Generalized syntax error message. */
/*********************************************************/
globle VOID SyntaxErrorMessage(location)
  char *location;
  {
   PrintErrorID("PRNTUTIL",2,CLIPS_TRUE);
   PrintCLIPS(WERROR,"Syntax Error");
   if (location != NULL)
     {
      PrintCLIPS(WERROR,":  Check appropriate syntax for ");
      PrintCLIPS(WERROR,location);
     }

   PrintCLIPS(WERROR,".\n");
   SetEvaluationError(CLIPS_TRUE);
  }
  
/****************************************************/
/* LocalVariableErrorMessage: Generic error message */
/*  when a local variable is accessed by an "item"  */
/*  which can not access local variables.           */ 
/****************************************************/
globle VOID LocalVariableErrorMessage(byWhat)
  char *byWhat;
  {
   PrintErrorID("PRNTUTIL",6,CLIPS_TRUE);
   PrintCLIPS(WERROR,"Local variables can not be accessed by ");
   PrintCLIPS(WERROR,byWhat);
   PrintCLIPS(WERROR,".\n");
  }
   
/**************************************************************************/
/* CLIPSSystemError: Generalized error message for major internal errors. */
/**************************************************************************/
globle VOID CLIPSSystemError(module,errorID)
  char *module;
  int errorID;
  {
   PrintErrorID("PRNTUTIL",3,CLIPS_TRUE);
   PrintCLIPS(WERROR,"\n*** CLIPS SYSTEM ERROR ***\n");
   PrintCLIPS(WERROR,"ID = ");
   PrintCLIPS(WERROR,module);
   PrintLongInteger(WERROR,(long int) errorID);
   PrintCLIPS(WERROR,"\n");
   PrintCLIPS(WERROR,"CLIPS data structures are in an inconsistent or corrupted state.\n");
   PrintCLIPS(WERROR,"This error may have occurred from errors in user defined code.\n");
   PrintCLIPS(WERROR,"**************************\n");
  }  
  
/*******************************************************/
/* DivideByZeroErrorMessage: Generalized error message */
/*   for when a function attempts to divide by zero.   */
/*******************************************************/
globle VOID DivideByZeroErrorMessage(functionName)
  char *functionName;
  {
   PrintErrorID("PRNTUTIL",7,CLIPS_FALSE);
   PrintCLIPS(WERROR,"Attempt to divide by zero in ");
   PrintCLIPS(WERROR,functionName);
   PrintCLIPS(WERROR," function.\n");
  }

/**********************************************************/
/* FloatToString: Converts number to CLIPS string format. */
/**********************************************************/
globle char *FloatToString(number)
  double number;
  {
   char floatString[40];
   int i;
   char x;
   VOID *thePtr;

   sprintf(floatString,"%.16g",number);

   for (i = 0; (x = floatString[i]) != '\0'; i++)
     {
      if ((x == '.') || (x == 'e'))
        {
         thePtr = AddSymbol(floatString);
         return(ValueToString(thePtr));
        }
     }

   strcat(floatString,".0");

   thePtr = AddSymbol(floatString);
   return(ValueToString(thePtr));
  }

/**********************************************************************/
/* LongIntegerToString: Converts long integer to CLIPS string format. */
/**********************************************************************/
globle char *LongIntegerToString(number)
  long number;
  {
   char buffer[30];
   VOID *thePtr;

   sprintf(buffer,"%ld",number);

   thePtr = AddSymbol(buffer);
   return(ValueToString(thePtr));
  }



