   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  10/14/93            */
   /*                                                     */
   /*                    SCANNER MODULE                   */
   /*******************************************************/

/*************************************************************/
/* Purpose: Routines for scanning lexical tokens from an     */
/*   input source.                                           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Chris Culbert                                        */
/*      Brian Donnell                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#define _SCANNER_SOURCE_

#include <ctype.h>
#include <stdio.h>
#define _CLIPS_STDIO_
#include <string.h>
#include <limits.h>    /* added 03-12-96 */

#include "setup.h"
#include "constant.h"
#include "router.h"
#include "symbol.h"
#include "utility.h"
#include "clipsmem.h"

#include "scanner.h"

#if ANSI_COMPILER
#include <stdlib.h>
#else
extern double atof();
extern long atol();
#endif

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if ANSI_COMPILER
   static VOID                   *ScanSymbol(char *,int,int *);
   static VOID                   *ScanString(char *);
   static VOID                    ScanNumber(char *,struct token *);
#else
   static VOID                   *ScanSymbol();
   static VOID                   *ScanString();
   static VOID                    ScanNumber();
#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static char            *GlobalString = NULL;
   static int              GlobalMax = 0;
   static int              GlobalPos = 0;

#if CERTAINTY_FACTORS    /* added 03-12-96 */
   static struct token     theUnToken;
   static BOOLEAN          UnTokenIsAvailable = CLIPS_FALSE;
#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   globle int              IgnoreCompletionErrors = CLIPS_FALSE;


#if CERTAINTY_FACTORS     /* added 03-12-96 */

/*** NOTE: although this UnGetToken handling is added to support some
           special needs of Certainty Factors it might just be useful in
		   general?! Would have to be very careful to clear the Un-gotten
		   token in all the right places -- currently this is ONLY
		   used by GetRHSPattern function and when done with GetRHSPattern
		   the looh ahead token must be cleared!!!!
***/

/***********************************************************************/
/* UnGetToken: Stores a token in a temporary location (theUnToken) and */
/* sets a flag (IsUnTokenAvailable) to TRUE to indicate that next      */
/* GetToken should get the Token from this place                       */
/* This routine also does a PPBackup - therefore expects that this is  */
/* done after a token is read and before any other things saved to the */
/* PPBuffer                                                            */
/*                                                                     */
/* NOTE: we do NOT UnGet STOP tokens since they may never be removed   */
/*       from theUnToken store -- if they next token is requested a    */
/*       STOP will be obtained again anyways -- I hope!!               */
/***********************************************************************/

globle VOID UnGetToken(theToken)
 struct token *theToken;
 { 
   if (theToken->type != STOP)
     {
      CopyToken(&theUnToken, theToken);
      UnTokenIsAvailable = CLIPS_TRUE;
#if (! RUN_TIME) && (! BLOAD_ONLY)   
      PPBackup();
#endif
     }
 }


/***********************************************************************/
/* ClearTheUnToken:                                                    */
/*                                                                     */
/* just reset the UnTokenIsAvailable flag so the UnToken will not be   */
/* used on the next call to GetToken                                   */
/***********************************************************************/
globle VOID ClearTheUnToken()
{
   UnTokenIsAvailable = CLIPS_FALSE;
}


#endif

/***********************************************************************/
/* GetToken: Reads next token from the input stream. The pointer to    */
/*   the token data structure passed as an argument is set to contain  */
/*   the type of token (e.g., symbol, string, integer, etc.), the data */
/*   value for the token (i.e., a symbol table location if it is a     */
/*   symbol or string, an integer table location if it is an integer), */
/*   and the pretty print representation.                              */
/***********************************************************************/
globle VOID GetToken(logicalName,theToken)
 char *logicalName;
 struct token *theToken;
 {
   int inchar;
   int type;

#if CERTAINTY_FACTORS
   if (UnTokenIsAvailable)
     {
	   /* get the token from the returned (unGotten!) token holder (theUnToken) */
	   CopyToken( theToken, &theUnToken );
       UnTokenIsAvailable = CLIPS_FALSE;
	   
   
#if (! RUN_TIME) && (! BLOAD_ONLY) 
       if (theToken->type == INSTANCE_NAME)
         {
          SavePPBuffer("[");
          SavePPBuffer(theToken->printForm);
          SavePPBuffer("]");
         }
       else
         { SavePPBuffer(theToken->printForm); 
		 }
#endif

	   return;
	 }
#endif 

   /*=======================================*/
   /* Set Unknown default values for token. */
   /*=======================================*/

   theToken->type = UNKNOWN;
   theToken->value = NULL;
   theToken->printForm = "unknown";
   GlobalPos = 0;
   GlobalMax = 0;

   /*==============================================*/
   /* Remove all white space before processing the */
   /* GetToken() request.                          */
   /*==============================================*/

   inchar = GetcCLIPS(logicalName);
   while ((inchar == ' ') || (inchar == '\n') || (inchar == '\f') ||
          (inchar == '\r') || (inchar == ';') || (inchar == '\t'))
     {
      /*=======================*/
      /* Remove comment lines. */
      /*=======================*/

      if (inchar == ';')
        {
         inchar = GetcCLIPS(logicalName);
         while ((inchar != '\n') && (inchar != '\r') && (inchar != EOF) )
           { inchar = GetcCLIPS(logicalName); }
        }
      inchar = GetcCLIPS(logicalName);
     }

   /*==========================*/
   /* Process Symbolic Tokens. */
   /*==========================*/

   if (isalpha(inchar))
     {
      theToken->type = SYMBOL;
      UngetcCLIPS(inchar,logicalName);
      theToken->value = (VOID *) ScanSymbol(logicalName,0,&type);
      theToken->printForm = ValueToString(theToken->value);
     }

   /*===============================================*/
   /* Process Number Tokens beginning with a digit. */
   /*===============================================*/

   else if (isdigit(inchar))
     {
      UngetcCLIPS(inchar,logicalName);
      ScanNumber(logicalName,theToken);
     }

   else switch (inchar)
     {
      /*========================*/
      /* Process String Tokens. */
      /*========================*/

      case '"':
         theToken->value = (VOID *) ScanString(logicalName);
         theToken->type = STRING;
         theToken->printForm = StringPrintForm(ValueToString(theToken->value));
         break;

      /*=======================================*/
      /* Process Tokens that might be numbers. */
      /*=======================================*/

      case '-':
      case '.':
      case '+':
         UngetcCLIPS(inchar,logicalName);
         ScanNumber(logicalName,theToken);
         break;

      /*===================================*/
      /* Process ? and ?<variable> Tokens. */
      /*===================================*/

       case '?':
          inchar = GetcCLIPS(logicalName);
          if (isalpha(inchar)
#if DEFGLOBAL_CONSTRUCT
              || (inchar == '*'))
#else
              )
#endif
            {
             UngetcCLIPS(inchar,logicalName);
             theToken->value = (VOID *) ScanSymbol(logicalName,0,&type);
             theToken->type = SF_VARIABLE;
#if DEFGLOBAL_CONSTRUCT
             if ((ValueToString(theToken->value)[0] == '*') &&
                 (((int) strlen(ValueToString(theToken->value))) > 1) &&
                 (ValueToString(theToken->value)[strlen(ValueToString(theToken->value)) - 1] == '*'))
               { 
                int count;
                
                theToken->type = GBL_VARIABLE;
                theToken->printForm = AppendStrings("?",ValueToString(theToken->value));
                count = strlen(GlobalString);
                GlobalString[count-1] = EOS;
                theToken->value = AddSymbol(GlobalString+1);
                GlobalString[count-1] = (char) inchar;
                
               }
             else
#endif
             theToken->printForm = AppendStrings("?",ValueToString(theToken->value));
            }
          else
            {
             theToken->type = SF_WILDCARD;
             theToken->value = (VOID *) AddSymbol("?");
             UngetcCLIPS(inchar,logicalName);
             theToken->printForm = "?";
            }
          break;

      /*=====================================*/
      /* Process $? and $?<variable> Tokens. */
      /*=====================================*/

      case '$':
         if ((inchar = GetcCLIPS(logicalName)) == '?')
           {
            inchar = GetcCLIPS(logicalName);
            if (isalpha(inchar)
#if DEFGLOBAL_CONSTRUCT
                 || (inchar == '*'))
#else
                 )
#endif
              {
               UngetcCLIPS(inchar,logicalName);
               theToken->value = (VOID *) ScanSymbol(logicalName,0,&type);
               theToken->type = MF_VARIABLE;
#if DEFGLOBAL_CONSTRUCT
             if ((ValueToString(theToken->value)[0] == '*') &&
                 ((int) (strlen(ValueToString(theToken->value))) > 1) &&
                 (ValueToString(theToken->value)[strlen(ValueToString(theToken->value)) - 1] == '*'))
               { 
                int count;
                
                theToken->type = MF_GBL_VARIABLE;
                theToken->printForm = AppendStrings("$?",ValueToString(theToken->value));
                count = strlen(GlobalString);
                GlobalString[count-1] = EOS;
                theToken->value = AddSymbol(GlobalString+1);
                GlobalString[count-1] = (char) inchar;
               }
             else
#endif
               theToken->printForm = AppendStrings("$?",ValueToString(theToken->value));
              }
            else
              {
               theToken->type = MF_WILDCARD;
               theToken->value = (VOID *) AddSymbol("$?");
               theToken->printForm = "$?";
               UngetcCLIPS(inchar,logicalName);
              }
           }
         else
           {
            theToken->type = SYMBOL;
            GlobalString = ExpandStringWithChar('$',GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            UngetcCLIPS(inchar,logicalName);
            theToken->value = (VOID *) ScanSymbol(logicalName,1,&type);
            theToken->printForm = ValueToString(theToken->value);
           }
         break;
         
      /*============================*/
      /* Symbols beginning with '<' */
      /*============================*/
      
      case '<':
         theToken->type = SYMBOL;
         GlobalString = ExpandStringWithChar('<',GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
         theToken->value = (VOID *) ScanSymbol(logicalName,1,&type);
         theToken->printForm = ValueToString(theToken->value);
         break;

      /*=============================================*/
      /* Process "(", ")", "~", "|", and "&" Tokens. */
      /*=============================================*/

      case '(':
         theToken->type = LPAREN;
         theToken->value = (VOID *) AddSymbol("(");
         theToken->printForm = "(";
         break;

      case ')':
         theToken->type= RPAREN;
         theToken->value = (VOID *) AddSymbol(")");
         theToken->printForm = ")";
         break;

      case '~':
         theToken->type = NOT_CONSTRAINT;
         theToken->value = (VOID *) AddSymbol("~");
         theToken->printForm = "~";
         break;

      case '|':
         theToken->type = OR_CONSTRAINT;
         theToken->value = (VOID *) AddSymbol("|");
         theToken->printForm = "|";
         break;

      case '&':
         theToken->type =  AND_CONSTRAINT;
         theToken->value = (VOID *) AddSymbol("&");
         theToken->printForm = "&";
         break;

      /*============================*/
      /* Process End-of-File Token. */
      /*============================*/

      case EOF:
      case 0:
      case 3:
         theToken->type = STOP;
         theToken->value = (VOID *) AddSymbol("stop");
         theToken->printForm = "";
         break;

      /*=======================*/
      /* Process Other Tokens. */
      /*=======================*/

      default:
         if (isprint(inchar))
           {
            UngetcCLIPS(inchar,logicalName);
            theToken->value = (VOID *) ScanSymbol(logicalName,0,&type);
            theToken->type = type;
            theToken->printForm = ValueToString(theToken->value);
           }
         else
           { theToken->printForm = "<<<unprintable character>>>"; }
         break;
     }
     
   /*===============================================*/
   /* Put the new token in the pretty print buffer. */
   /*===============================================*/

#if (! RUN_TIME) && (! BLOAD_ONLY) 
   if (theToken->type == INSTANCE_NAME)
     {
      SavePPBuffer("[");
      SavePPBuffer(theToken->printForm);
      SavePPBuffer("]");
     }
   else
     { SavePPBuffer(theToken->printForm); }
#endif

   /*=========================================================*/
   /* Return the temporary memory used in scanning the token. */
   /*=========================================================*/
   
   if (GlobalString != NULL)
     {
      rm(GlobalString,GlobalMax);
      GlobalString = NULL;
     }

   return;
  }

/*************************************/
/* ScanSymbol: Scans a symbol token. */
/*************************************/
static VOID *ScanSymbol(logicalName,count,type)
  char *logicalName;
  int count;
  int *type;
  {
   int inchar;
#if OBJECT_SYSTEM
   VOID *symbol;
#endif

   /*=====================================*/
   /* Scan characters and add them to the */
   /* symbol until a delimiter is found.  */
   /*=====================================*/
   
   inchar = GetcCLIPS(logicalName);
   while ( (inchar != '<') && (inchar != '"') &&
           (inchar != '(') && (inchar != ')') &&
           (inchar != '&') && (inchar != '|') && (inchar != '~') &&
           (inchar != ' ') && (inchar != ';') &&
           isprint(inchar) )
     {
      GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);


      count++;
      inchar = GetcCLIPS(logicalName);
     }

   /*===================================================*/
   /* Return the last character scanned (the delimiter) */
   /* to the input stream so it will be scanned as part */
   /* of the next token.                                */
   /*===================================================*/

   UngetcCLIPS(inchar,logicalName);

   /*====================================================*/
   /* Add the symbol to the symbol table and return the  */
   /* symbol table address of the symbol. Symbols of the */
   /* form [<symbol>] are instance names, so the type    */
   /* returned is INSTANCE_NAME rather than SYMBOL.      */
   /*====================================================*/

#if OBJECT_SYSTEM
   if (count > 2)
     {
      if ((GlobalString[0] == '[') ? (GlobalString[count-1] == ']') : CLIPS_FALSE)
        {
         *type = INSTANCE_NAME;
         inchar = ']';
        }
      else
        {
         *type = SYMBOL;
         return(AddSymbol(GlobalString));
        }
      GlobalString[count-1] = EOS;
      symbol = AddSymbol(GlobalString+1);
      GlobalString[count-1] = (char) inchar;
      return(symbol);
     }
   else
     {
      *type = SYMBOL;
      return(AddSymbol(GlobalString));
     }
#else
   *type = SYMBOL;
   return(AddSymbol(GlobalString));
#endif
  }

/*************************************/
/* ScanString: Scans a string token. */
/*************************************/
static VOID *ScanString(logicalName)
  char *logicalName;
  {
   int inchar;
   int pos = 0, max = 0;
   char *theString = NULL;
   VOID *thePtr;
   
   /*============================================*/
   /* Scan characters and add them to the string */
   /* until the " delimiter is found.            */
   /*============================================*/

   inchar = GetcCLIPS(logicalName);
   while ((inchar != '"') && (inchar != EOF))
     {
      if (inchar == '\\')
        { inchar = GetcCLIPS(logicalName); }

      theString = ExpandStringWithChar(inchar,theString,&pos,&max,max+80);
      inchar = GetcCLIPS(logicalName);
     }

   if ((inchar == EOF) && (IgnoreCompletionErrors == CLIPS_FALSE))
     { PrintCLIPS(WERROR,"\nEncountered End-Of-File while scanning a string\n"); }

   /*===============================================*/
   /* Add the string to the symbol table and return */
   /* the symbol table address of the string.       */
   /*===============================================*/
   
   if (theString == NULL)
     { thePtr = AddSymbol(""); }
   else
     {
      thePtr = AddSymbol(theString);
      rm(theString,max);
     }

   return(thePtr);
  }

/**************************************/
/* ScanNumber: Scans a numeric token. */
/**************************************/
static VOID ScanNumber(logicalName,theToken)
  char *logicalName;
  struct token *theToken;
  {
   int count = 0;
   int inchar, phase;
   int digitFound = CLIPS_FALSE;
   int processFloat = CLIPS_FALSE;
   double fvalue;
   long lvalue;
   int type;

   /* Phases:              */
   /*  -1 = sign           */
   /*   0 = integral       */
   /*   1 = decimal        */
   /*   2 = exponent-begin */
   /*   3 = exponent-value */
   /*   5 = done           */
   /*   9 = error          */

   inchar = GetcCLIPS(logicalName);
   phase = -1;

   while ((phase != 5) && (phase != 9))
     {
      if (phase == -1)
        {
         if (isdigit(inchar))
           {
            phase = 0;
            digitFound = CLIPS_TRUE;
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
           }
         else if ((inchar == '+') || (inchar == '-'))
           {
            phase = 0;
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
           }
         else if (inchar == '.')
           {
            processFloat = CLIPS_TRUE;
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
            phase = 1;
           }
         else if ((inchar == 'E') || (inchar == 'e'))
           {
            processFloat = CLIPS_TRUE;
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
            phase = 2;
           }
         else if ( (inchar == '<') || (inchar == '"') ||
                   (inchar == '(') || (inchar == ')') ||
                   (inchar == '&') || (inchar == '|') || (inchar == '~') ||
                   (inchar == ' ') || (inchar == ';') ||
                   (isprint(inchar) == 0) )
           { phase = 5; }
         else
           {
            phase = 9;
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
           }
        }
      else if (phase == 0)
        {
         if (isdigit(inchar))
           {
            digitFound = CLIPS_TRUE;
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
           }
         else if (inchar == '.')
           {
            processFloat = CLIPS_TRUE;
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
            phase = 1;
           }
         else if ((inchar == 'E') || (inchar == 'e'))
           {
            processFloat = CLIPS_TRUE;
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
            phase = 2;
           }
         else if ( (inchar == '<') || (inchar == '"') ||
                   (inchar == '(') || (inchar == ')') ||
                   (inchar == '&') || (inchar == '|') || (inchar == '~') ||
                   (inchar == ' ') || (inchar == ';') ||
                   (isprint(inchar) == 0) )
           { phase = 5; }
         else
           {
            phase = 9;
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
           }
        }
      else if (phase == 1)
        {
         if (isdigit(inchar))
           {
            digitFound = CLIPS_TRUE;
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
           }
         else if ((inchar == 'E') || (inchar == 'e'))
           {
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
            phase = 2;
           }
         else if ( (inchar == '<') || (inchar == '"') ||
                   (inchar == '(') || (inchar == ')') ||
                   (inchar == '&') || (inchar == '|') || (inchar == '~') ||
                   (inchar == ' ') || (inchar == ';') ||
                   (isprint(inchar) == 0) )
           { phase = 5; }
         else
           {
            phase = 9;
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
           }
        }
      else if (phase == 2)
        {
         if (isdigit(inchar))
           {
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
            phase = 3;
           }
         else if ((inchar == '+') || (inchar == '-'))
           {
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
            phase = 3;
           }
         else if ( (inchar == '<') || (inchar == '"') ||
                   (inchar == '(') || (inchar == ')') ||
                   (inchar == '&') || (inchar == '|') || (inchar == '~') ||
                   (inchar == ' ') || (inchar == ';') ||
                   (isprint(inchar) == 0) )
           {
            digitFound = CLIPS_FALSE;
            phase = 5;
           }
         else
           {
            phase = 9;
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
           }
        }
      else if (phase == 3)
        {
         if (isdigit(inchar))
           {
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
           }
         else if ( (inchar == '<') || (inchar == '"') ||
                   (inchar == '(') || (inchar == ')') ||
                   (inchar == '&') || (inchar == '|') || (inchar == '~') ||
                   (inchar == ' ') || (inchar == ';') ||
                   (isprint(inchar) == 0) )
           {
            if ((GlobalString[count-1] == '+') || (GlobalString[count-1] == '-'))
              { digitFound = CLIPS_FALSE; }
            phase = 5;
           }
         else
           {
            phase = 9;
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
           }
        }

      if ((phase != 5) && (phase != 9))
        { inchar = GetcCLIPS(logicalName); }
     }

   if (phase == 9)
     {
      theToken->value = (VOID *) ScanSymbol(logicalName,count,&type);
      theToken->type = type;
      theToken->printForm = ValueToString(theToken->value);
      return;
     }

   /*=======================================*/
   /* Stuff last character back into buffer */
   /* and return the number.                */
   /*=======================================*/

   UngetcCLIPS(inchar,logicalName);

   if (! digitFound)
     {
      theToken->type = SYMBOL;
      theToken->value = (VOID *) AddSymbol(GlobalString);
      theToken->printForm = ValueToString(theToken->value);
      return;
     }

   if (processFloat)
     {
      fvalue = atof(GlobalString);
      theToken->type = FLOAT;
      theToken->value = (VOID *) AddDouble(fvalue);
      theToken->printForm = FloatToString(ValueToDouble(theToken->value));
     }
   else
     {
      lvalue = atol(GlobalString);
      if ((lvalue == LONG_MAX) || (lvalue == LONG_MIN))
        {
         PrintWarningID("SCANNER",1,CLIPS_FALSE);
         PrintCLIPS(WWARNING,"Over or underflow of long integer.\n");
        }
      theToken->type = INTEGER;
      theToken->value = (VOID *) AddLong(lvalue);
      theToken->printForm = LongIntegerToString(ValueToLong(theToken->value));
     }

   return;
  }

/***********************************************************/
/* CopyToken: Copies values of one token to another token. */
/***********************************************************/
globle VOID CopyToken(destination,source)
  struct token *destination, *source;
  {
   destination->type = source->type;
   destination->value = source->value;
   destination->printForm = source->printForm;
  }



