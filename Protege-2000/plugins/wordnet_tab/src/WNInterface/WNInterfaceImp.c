#include <jni.h>
#include "WNInterface.h"
#include <stdio.h>
#include <string.h>
/*
#include "wn.h"
*/


#define MAX_ARRAY_SIZE 1024


/*----------------------------------------------------------------------------*/

JNIEXPORT jint JNICALL Java_WNInterface_ttt_1wninit
  (JNIEnv   *env,
   jobject   obj)
{
jint val = 1;
/*
printf("TTT -- IN Java_WNInterface_ttt_wninit ----------------------------\n");
fflush(stdout);
*/
return val;
}

/*----------------------------------------------------------------------------*/

JNIEXPORT jint JNICALL Java_WNInterface_wninitFoo
  (JNIEnv   *env,
   jobject   obj)
{
/*ttt
  return wninit();
ttt*/
                   /*--ttt-- Don't know what to do here, isn't called anyway */
return 1;
}

/*----------------------------------------------------------------------------*/

JNIEXPORT jstring JNICALL Java_WNInterface_findtheinfo
  (JNIEnv   *env,
   jobject   obj,
   jstring   searchString,
   jint      pos,
   jint      ptrTyp,
   jint      whichSense)
{
  char  *info;
  char  *C_searchString;


  C_searchString = (char *) (*env)->GetStringUTFChars(env, searchString, 0);

/*ttt
  info = findtheinfo(C_searchString, pos, ptrTyp, whichSense);
ttt*/

  (*env)->ReleaseStringUTFChars(env, searchString, C_searchString);

  return (*env)->NewStringUTF(env, info);
}

/*----------------------------------------------------------------------------*/

/* Better version of above, checks all of the morphed forms of the word */

JNIEXPORT jstring JNICALL Java_WNInterface_findAllTheInfo
  (JNIEnv   *env,
   jobject   obj,
   jstring   searchString,
   jint      pos,
   jint      ptrTyp,
   jint      whichSense)
{
  char  *info;
  char  *C_searchString;


  C_searchString = (char *) (*env)->GetStringUTFChars(env, searchString, 0);

/*ttt
  info = findtheinfo(C_searchString, pos, ptrTyp, whichSense);
ttt*/
                                                             /*-- do ? -- ttt */

  (*env)->ReleaseStringUTFChars(env, searchString, C_searchString);

  return (*env)->NewStringUTF(env, info);
}

/*----------------------------------------------------------------------------*/

/* Returns the definitions for the searchStr and its morphed versions. */

JNIEXPORT jobjectArray JNICALL Java_WNInterface_getDefinitions
  (JNIEnv   *env,
   jobject   obj,
   jstring   searchString,
   jint      pos,
   jint      ptrTyp,
   jint      whichSense)
{
  int           i;
  int           numDefs;
  char          *def;
  char          *C_defs[MAX_ARRAY_SIZE];
  char          *C_searchString;
  jobjectArray  defs;


  C_searchString = (char *) (*env)->GetStringUTFChars(env, searchString, 0);

  numDefs = 0;
/*ttt
  def = morphstr(C_searchString, pos);
ttt*/
                                      /*---------------------------------- ttt*/

  while (def != 0)
  {
    if (numDefs == MAX_ARRAY_SIZE)
    {
      fprintf(stderr, "getJustDefs:  Array too small\n");
      break;
    }

    if (strcmp(def, C_searchString) != 0)
      C_defs[numDefs++] = def;

/*ttt
    def = morphstr(0, pos);
ttt*/
                                      /*---------------------------------- ttt*/
  }

  (*env)->ReleaseStringUTFChars(env, searchString, C_searchString);


  if (numDefs != 0)
  {
    defs = (jobjectArray) (*env)->NewObjectArray
      (env, numDefs, (*env)->FindClass(env, "java/lang/String"),
       (*env)->NewStringUTF(env, ""));

    for (i = 0; i < numDefs; i++)
    {
      (*env)->SetObjectArrayElement
        (env, defs, i, (*env)->NewStringUTF(env, C_defs[i]));
    }
  }
  else
    defs = 0;


  (*env)->ReleaseStringUTFChars(env, searchString, C_searchString);


  return (defs);
}

/*----------------------------------------------------------------------------*/

/* Helper function that only returns the direct synonyms *
 * of the searchStr and versions of the searchStr        */

JNIEXPORT jobjectArray JNICALL Java_WNInterface_getJustWords
  (JNIEnv   *env,
   jobject   obj,
   jstring   searchString,
   jint      pos,
   jint      ptrTyp,
   jint      whichSense)
{
  int           i;
  int           numWords;
  char          *wrd;
  char          *C_words[MAX_ARRAY_SIZE];
  char          *C_searchString;
  jobjectArray  words;


  C_searchString = (char *) (*env)->GetStringUTFChars(env, searchString, 0);

  numWords = 0;
/*ttt
  wrd = morphstr(C_searchString, pos);
ttt*/
                                      /*---------------------------------- ttt*/

  while (wrd != 0)
  {
    if (numWords == MAX_ARRAY_SIZE)
    {
      fprintf(stderr, "getJustWords:  Array too small\n");
      break;
    }

    if (strcmp(wrd, C_searchString) != 0)
      C_words[numWords++] = wrd;

/*ttt
    wrd = morphstr(0, pos);
ttt*/
                                      /*---------------------------------- ttt*/
  }

  (*env)->ReleaseStringUTFChars(env, searchString, C_searchString);


  if (numWords != 0)
  {
    words = (jobjectArray) (*env)->NewObjectArray
      (env, numWords, (*env)->FindClass(env, "java/lang/String"),
       (*env)->NewStringUTF(env, ""));

    for (i = 0; i < numWords; i++)
    {
      (*env)->SetObjectArrayElement
        (env, words, i, (*env)->NewStringUTF(env, C_words[i]));
    }
  }
  else
    words = 0;


  return (words);
}

/*----------------------------------------------------------------------------*/

/* Returns morphologically base form of the word for the   *
 * pos specified, not including the word originally passed *
 * in to the function, so it may return null.              */

JNIEXPORT jobjectArray JNICALL Java_WNInterface_getMorphForms
  (JNIEnv   *env,
   jobject   obj,
   jstring   origString,
   jint      pos)
{
  int           i;
  int           numForms;
  char          *form;
  char          *C_forms[MAX_ARRAY_SIZE];
  char          *C_origString;
  jobjectArray  forms;


  C_origString = (char *) (*env)->GetStringUTFChars(env, origString, 0);

  numForms = 0;
/*ttt
  form = morphstr(C_origString, pos);
ttt*/

  while (form != 0)
  {
    if (numForms == MAX_ARRAY_SIZE)
    {
      fprintf(stderr, "getMorphForms:  Array too small\n");
      break;
    }

    if (strcmp(form, C_origString) != 0)
      C_forms[numForms++] = form;

/*ttt
    form = morphstr(0, pos);
ttt*/
  }

  (*env)->ReleaseStringUTFChars(env, origString, C_origString);


  if (numForms != 0)
  {
    forms = (jobjectArray) (*env)->NewObjectArray
      (env, numForms, (*env)->FindClass(env, "java/lang/String"),
       (*env)->NewStringUTF(env, ""));

    for (i = 0; i < numForms; i++)
    {
      (*env)->SetObjectArrayElement
        (env, forms, i, (*env)->NewStringUTF(env, C_forms[i]));
    }
  }
  else
    forms = 0;

  return (forms);
}
