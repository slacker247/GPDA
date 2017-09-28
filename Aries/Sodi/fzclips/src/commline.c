   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.04  03/04/96            */
   /*                                                     */
   /*                COMMAND LINE MODULE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides a set of routines for processing        */
/*   commands entered at the CLIPS top level prompt.         */
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

#define _COMMLINE_SOURCE_

#include <stdio.h>
#define _CLIPS_STDIO_
#include <string.h>
#include <ctype.h>

#include "setup.h"
#include "constant.h"
#include "commline.h"

#include "symbol.h"
#include "clipsmem.h"
#include "scanner.h"
#include "exprnpsr.h"
#include "argacces.h"
#include "router.h"
#include "strngrtr.h"
#include "constrct.h"
#include "prcdrfun.h"
#include "prcdrpsr.h"
#include "utility.h"
#include "filecom.h"
#include "cstrcpsr.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if ! RUN_TIME  /* 3-4-96 */
#if ANSI_COMPILER
   static int                     DoString(char *,int,int *);
   static int                     DoComment(char *,int);
   static int                     DoWhiteSpace(char *,int);
   static VOID                    DefaultGetNextEvent(void);
#else
   static int                     DoString();
   static int                     DoComment();
   static int                     DoWhiteSpace();
   static VOID                    DefaultGetNextEvent();
#endif
#endif  /* 3-4-96 */

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   globle int               EvaluatingTopLevelCommand = CLIPS_FALSE;

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

#if ! RUN_TIME

   static char             *CommandString = NULL;
   static int               MaximumCharacters = 0;
   static int               ParsingTopLevelCommand = CLIPS_FALSE;
#if FUZZY_DEFTEMPLATES
   static char             *VersionString = "         Fuzzy CLIPS (V6.04A 10/23/98)\n";
#else
   static char             *VersionString = "         CLIPS (V6.04 01/17/96)\n";
#endif

#if ANSI_COMPILER
   static int              (*EventFunction)(void) =
                                           (int (*)(void)) DefaultGetNextEvent;
   static int              (*MemoryStatusFunction)(void) = NULL;
#else
   static int              (*EventFunction)() =
                                               (int (*)()) DefaultGetNextEvent;
   static int              (*MemoryStatusFunction)() = NULL;
#endif

/***************************************************/
/* ExpandCommandString: Appends a character to the */
/*   command string. Returns TRUE if the command   */
/*   string was successfully expanded, otherwise   */
/*   FALSE. Expanding the string also includes     */
/*   adding a backspace character which reduces    */
/*   string's length.                              */
/***************************************************/
globle int ExpandCommandString(inchar)
  int inchar;
  {
   register int k;

   k = CLIPSInputCount;
   CommandString = ExpandStringWithChar(inchar,CommandString,&CLIPSInputCount,
                                        &MaximumCharacters,MaximumCharacters+80);
   return((CLIPSInputCount != k) ? CLIPS_TRUE : CLIPS_FALSE);
  }

/******************************************************************/
/* FlushCommandString: Empties the contents of the CommandString. */
/******************************************************************/
globle VOID FlushCommandString()
  {
   if (CommandString != NULL) rm(CommandString,MaximumCharacters);
   CommandString = NULL;
   MaximumCharacters = 0;
   CLIPSInputCount = 0;
  }

/*********************************************************************************/
/* SetCommandString: Sets the contents of the CommandString to a specific value. */
/*********************************************************************************/
globle VOID SetCommandString(str)
  char *str;
  {
   int length;

   FlushCommandString();
   length = strlen(str);
   CommandString = genrealloc(CommandString,(unsigned) MaximumCharacters,
                              (unsigned) MaximumCharacters + length + 1);

   strcpy(CommandString,str);
   MaximumCharacters += (length + 1);
   CLIPSInputCount += length;
  }

/******************************************************************************/
/* AppendCommandString: Appends a value to the contents of the CommandString. */
/******************************************************************************/
globle VOID AppendCommandString(str)
  char *str;
  {
   CommandString = AppendToString(str,CommandString,&CLIPSInputCount,&MaximumCharacters);
  }

/*****************************************************************************/
/* GetCommandString: Returns a pointer to the contents of the CommandString. */
/*****************************************************************************/
globle char *GetCommandString()
  {
   return(CommandString);
  }

/**************************************************************************/
/* CompleteCommand: Determines whether a string forms a complete command. */
/*   A complete command is either a constant, a variable, or a function   */
/*   call which is followed (at some point) by a carriage return. Once a  */
/*   complete command is found (not including the parenthesis),           */
/*   extraneous parenthesis and other tokens are ignored. If a complete   */
/*   command exists, then 1 is returned. 0 is returned if the command was */
/*   not complete and without errors. -1 is returned if the command       */
/*   contains an error.                                                   */
/**************************************************************************/
globle int CompleteCommand(mstring)
  char *mstring;
  {
   int i;
   char inchar;
   int depth = 0;
   int moreThanZero = 0;
   int complete;
   int error = 0;

   if (mstring == NULL) return(0);

   /*===================================================*/
   /* Loop through each character of the command string */
   /* to determine if there is a complete command.      */
   /*===================================================*/
   
   i = 0;
   while ((inchar = mstring[i++]) != EOS)
     {
      switch(inchar)
        {
         /*======================================================*/
         /* If a carriage return or line feed is found, there is */
         /* at least one completed token in the command buffer,  */
         /* and parentheses are balanced, then a complete        */
         /* command has been found. Otherwise, remove all white  */
         /* space beginning with the current character.          */
         /*======================================================*/
         
         case '\n' :
         case '\r' :
           if (error) return(-1);
           if (moreThanZero && (depth == 0)) return(1);
           i = DoWhiteSpace(mstring,i);
           break;

         /*=====================*/
         /* Remove white space. */
         /*=====================*/
         
         case ' ' :
         case '\f' :
         case '\t' :
           i = DoWhiteSpace(mstring,i);
           break;

         /*======================================================*/
         /* If the opening quotation of a string is encountered, */
         /* determine if the closing quotation of the string is  */
         /* in the command buffer. Until the closing quotation   */
         /* is found, a complete command can not be made.        */
         /*======================================================*/
         
         case '"' :
           i = DoString(mstring,i,&complete);
           if ((depth == 0) && complete) moreThanZero = CLIPS_TRUE;
           break;

         /*====================*/
         /* Process a comment. */
         /*====================*/
         
         case ';' :
           i = DoComment(mstring,i);
           if (moreThanZero && (depth == 0) && (mstring[i] != EOS))
             {
              if (error) return(-1);
              else return(1);
             }
           else if (mstring[i] != EOS) i++;
           break;

         /*====================================================*/
         /* A left parenthesis increases the nesting depth of  */
         /* the current command by 1. Don't bother to increase */
         /* the depth if the first token encountered was not   */
         /* a parenthesis (e.g. for the command string         */
         /* "red (+ 3 4", the symbol red already forms a       */
         /* complete command, so the next carriage return will */
         /* cause evaluation of red--the closing parenthesis   */
         /* for "(+ 3 4" does not have to be found).           */
         /*====================================================*/
         
         case '(' :
           if ((depth > 0) || (moreThanZero == CLIPS_FALSE))
             {
              depth++;
              moreThanZero = CLIPS_TRUE;
             }
           break;

         /*====================================================*/
         /* A right parenthesis decreases the nesting depth of */
         /* the current command by 1. If the parenthesis is    */
         /* the first token of the command, then an error is   */
         /* generated.                                         */
         /*====================================================*/
         
         case ')' :
           if (depth > 0) depth--;
           else if (moreThanZero == CLIPS_FALSE) error = CLIPS_TRUE;
           break;

         /*=====================================================*/
         /* If the command begins with any other character and  */
         /* an opening parenthesis hasn't yet been found, then  */
         /* skip all characters on the same line. If a carriage */
         /* return or line feed is found, then a complete       */
         /* command exists.                                     */
         /*=====================================================*/
         
         default:
           if (depth == 0)
             {
              if (isprint(inchar))
                {
                 while ((inchar = mstring[i++]) != EOS)
                   {
                    if ((inchar == '\n') || (inchar == '\r'))
                      {
                       if (error) return(-1);
                       else return(1);
                      }
                   }
                 return(0);
                }
             }
           break;
        }
     }

   /*====================================================*/
   /* Return 0 because a complete command was not found. */
   /*====================================================*/
   
   return(0);
  }

/***********************************************************/
/* DoString: Skips over a string contained within a string */
/*   until the closing quotation mark is encountered.      */
/***********************************************************/
static int DoString(str,pos,complete)
  char *str;
  int pos;
  int *complete;
  {
   int inchar;

   /*=================================================*/
   /* Process the string character by character until */
   /* the closing quotation mark is found.            */
   /*=================================================*/
   
   inchar = str[pos];
   while (inchar  != '"')
     {
      /*=====================================================*/
      /* If a \ is found, then the next character is ignored */
      /* even if it is a closing quotation mark.             */
      /*=====================================================*/
      
      if (inchar == '\\')
        {
         pos++;
         inchar = str[pos];
        }

      /*===================================================*/
      /* If the end of input is reached before the closing */
      /* quotation mark is found, the return the last      */
      /* position that was reached and indicate that a     */
      /* complete string was not found.                    */
      /*===================================================*/
      
      if (inchar == EOS)
        {
         *complete = CLIPS_FALSE;
         return(pos);
        }

      /*================================*/
      /* Move on to the next character. */
      /*================================*/
      
      pos++;
      inchar = str[pos];
     }

   /*======================================================*/
   /* Indicate that a complete string was found and return */
   /* the position of the closing quotation mark.          */
   /*======================================================*/
   
   pos++;
   *complete = CLIPS_TRUE;
   return(pos);
  }

/*************************************************************/
/* DoComment: Skips over a comment contained within a string */
/*   until a line feed or carriage return is encountered.    */
/*************************************************************/
static int DoComment(str,pos)
  char *str;
  int pos;
  {
   int inchar;

   inchar = str[pos];
   while ((inchar != '\n') && (inchar != '\r'))
     {
      if (inchar == EOS)
        { return(pos); }

      pos++;
      inchar = str[pos];
     }

   return(pos);
  }

/**************************************************************/
/* DoWhiteSpace: Skips over white space consisting of spaces, */
/*   tabs, and form feeds that is contained within a string.  */
/**************************************************************/
static int DoWhiteSpace(str,pos)
  char *str;
  int pos;
  {
   int inchar;

   inchar = str[pos];
   while ((inchar == ' ') || (inchar == '\f') || (inchar == '\t'))
     {
      pos++;
      inchar = str[pos];
     }

   return(pos);
  }

/********************************************************************/
/* CommandLoop: Endless loop which waits for user commands and then */
/*   executes them. The command loop will bypass the EventFunction  */
/*   if there is an active batch file.                              */
/********************************************************************/
globle VOID CommandLoop()
  {
   int inchar;

   PrintCLIPS(WCLIPS,VersionString);
   SetHaltExecution(CLIPS_FALSE);
   SetEvaluationError(CLIPS_FALSE);
   PeriodicCleanup(CLIPS_TRUE,CLIPS_FALSE);
   PrintPrompt();
   CLIPSInputCount = 0;
   
   while (CLIPS_TRUE)
     {
      /*===================================================*/
      /* If a batch file is active, grab the command input */
      /* directly from the batch file, otherwise call the  */
      /* event function.                                   */
      /*===================================================*/

      if (BatchActive() == CLIPS_TRUE)
        {
         inchar = LLGetcBatch("stdin",CLIPS_TRUE);
         if (inchar == EOF)
           { (*EventFunction)(); }
         else
           { ExpandCommandString((char) inchar); }
        }
      else
        { (*EventFunction)(); }

      /*=================================================*/
      /* If execution was halted, then remove everything */
      /* from the command buffer.                        */
      /*=================================================*/
      
      if (GetHaltExecution() == CLIPS_TRUE)
        {
         SetHaltExecution(CLIPS_FALSE);
         SetEvaluationError(CLIPS_FALSE);
         FlushCommandString();
#if ! WINDOW_INTERFACE
         fflush(stdin);
#endif
         PrintCLIPS(WCLIPS,"\n");
         PrintPrompt();
        }

      /*=========================================*/
      /* If a complete command is in the command */
      /* buffer, then execute it.                */
      /*=========================================*/
      
      if ((CompleteCommand(CommandString) != 0) && (CLIPSInputCount > 0))
        {
         FlushPPBuffer();
         SetPPBufferStatus(OFF);
         CLIPSInputCount = -1;
         RouteCommand(CommandString,CLIPS_TRUE); /* 3-4-96 */
         FlushPPBuffer();
         SetHaltExecution(CLIPS_FALSE);
         SetEvaluationError(CLIPS_FALSE);
         FlushCommandString();
         FlushBindList();
         PeriodicCleanup(CLIPS_TRUE,CLIPS_FALSE);
         PrintPrompt();
        }
     }
  }

/*************************************************/
/* PrintPrompt: Prints the CLIPS command prompt. */
/*************************************************/
globle VOID PrintPrompt()
   {
    PrintCLIPS(WCLIPS,"CLIPS> ");

    if (MemoryStatusFunction != NULL)
      { (*MemoryStatusFunction)(); }
   }

/********************************************************************************/
/* SetMemoryStatusFunction: Replaces the current value of MemoryStatusFunction. */
/********************************************************************************/
globle VOID SetMemoryStatusFunction(funptr)
  int (*funptr)(VOID_ARG);
  {
   MemoryStatusFunction = funptr;
  }

/************************************************/
/* RouteCommand: Processes a completed command. */
/************************************************/
globle BOOLEAN RouteCommand(command,printResult) /* added 3-4-96 */
  char *command;
  int printResult;
  {
   DATA_OBJECT result;
   struct expr *top;
   char *commandName;
   struct token theToken;

   if (command == NULL)
     { return(0); }

   /*========================================*/
   /* Open a string input source and get the */
   /* first token from that source.          */
   /*========================================*/
   
   OpenStringSource("command",command,0);

   GetToken("command",&theToken);

   /*=====================*/
   /* Evaluate constants. */
   /*=====================*/
   
   if ((theToken.type == SYMBOL) || (theToken.type == STRING) ||
       (theToken.type == FLOAT) || (theToken.type == INTEGER) ||
       (theToken.type == INSTANCE_NAME))
     {
      CloseStringSource("command");
      if (printResult)
        {
         PrintAtom("stdout",theToken.type,theToken.value);
         PrintCLIPS("stdout","\n");
        }
      return(1);
     }

   /*============================*/
   /* Evaluate global variables. */
   /*============================*/
   
   if (theToken.type == GBL_VARIABLE)
     {
      CloseStringSource("command");
      top = GenConstant(theToken.type,theToken.value);
      EvaluateExpression(top,&result);
      rtn_struct(expr,top);
      if (printResult)
        {
         PrintDataObject("stdout",&result);
         PrintCLIPS("stdout","\n");
        }
      return(1);
     }

   /*========================================================*/
   /* If the next token isn't the beginning left parenthesis */
   /* of a command or construct, then whatever was entered   */
   /* cannot be evaluated at the command prompt.             */
   /*========================================================*/
   
   if (theToken.type != LPAREN)
     {   
      PrintErrorID("COMMLINE",1,CLIPS_FALSE);
      PrintCLIPS(WERROR,"Expected a '(', constant, or global variable\n");
      CloseStringSource("command");
      return(0);
     }

   /*===========================================================*/
   /* The next token must be a function name or construct type. */
   /*===========================================================*/
   
   GetToken("command",&theToken);
   if (theToken.type != SYMBOL)
     {
      PrintErrorID("COMMLINE",2,CLIPS_FALSE);
      PrintCLIPS(WERROR,"Expected a command.\n");
      CloseStringSource("command");
      return(0);
     }

   commandName = ValueToString(theToken.value);

   /*======================*/
   /* Evaluate constructs. */
   /*======================*/

#if (! RUN_TIME) && (! BLOAD_ONLY)
   {
    int errorFlag;

    errorFlag = ParseConstruct(commandName,"command");
    if (errorFlag != -1)
      {
       CloseStringSource("command");
       if (errorFlag == 1)
         {
          PrintCLIPS(WERROR,"\nERROR:\n");
          PrintInChunks(WERROR,GetPPBuffer());
          PrintCLIPS(WERROR,"\n");
         }
       DestroyPPBuffer();
       return(errorFlag);
      }
   }
#endif

   /*========================*/
   /* Parse a function call. */
   /*========================*/

   ParsingTopLevelCommand = CLIPS_TRUE;
   top = Function2Parse("command",commandName);
   ParsingTopLevelCommand = CLIPS_FALSE;
   ClearParsedBindNames();
   
   /*================================*/
   /* Close the string input source. */
   /*================================*/
   
   CloseStringSource("command"); 
   
   /*=========================*/
   /* Evaluate function call. */
   /*=========================*/
   
   if (top == NULL) return(0);
   EvaluatingTopLevelCommand = CLIPS_TRUE;
   ExpressionInstall(top);
   EvaluateExpression(top,&result);
   ExpressionDeinstall(top);
   EvaluatingTopLevelCommand = CLIPS_FALSE;
   ReturnExpression(top);

   if ((result.type != RVOID) && printResult)  /* 3-4-96 */
     {
      PrintDataObject("stdout",&result);
      PrintCLIPS("stdout","\n");
     }

   return(1);
  }

/*****************************************************************/
/* DefaultGetNextEvent: Default event-handling function. Handles */
/*   only keyboard events by first calling GetcCLIPS to get a    */
/*   character and then calling ExpandCommandString to add the   */
/*   character to the CommandString.                             */
/*****************************************************************/
static VOID DefaultGetNextEvent()
  {
   int inchar;

   inchar = GetcCLIPS("stdin");

   if (inchar == EOF) inchar = '\n';

   ExpandCommandString((char) inchar);
  }

/*************************************/
/* SetEventFunction: Replaces the    */
/*   current value of EventFunction. */
/*************************************/
#if ! MAC_MCW    /* added 3-4-96 */
globle int (*SetEventFunction(theFunction))(VOID_ARG)
  int (*theFunction)(VOID_ARG);
#else
globle int (*SetEventFunction(theFunction))()
  int (*theFunction)(VOID_ARG);
#endif
  {
   int (*tmp_ptr)(VOID_ARG);

   tmp_ptr = EventFunction;
   EventFunction = theFunction;
   return(tmp_ptr);
  }

/****************************************/
/* TopLevelCommand: Indicates whether a */
/*   top-level command is being parsed. */
/****************************************/
globle BOOLEAN TopLevelCommand()
  {
   return(ParsingTopLevelCommand);
  }

#endif



