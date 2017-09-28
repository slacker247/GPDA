#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <sys/timeb.h>

#include "DSBconfig.h"
#include "Globals.h"


char    DS_defTimeUnit;
char    DS_evidLabel1[8];
char    DS_evidLabel2[8];
char    DS_evidLabel3[8];
char    DS_startTime[32];
char    DS_endTime[32];

int     DS_isDiscrete;
int     DS_numPossibilities;

DS_rangeRecType DS_possibilities[DS_Max_Possibilities];


int  getLine (char*, FILE*);


/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

void DS_AlgoPrintConfig(void)
{
  printf("\nConfig data:\n");

  printf("  - Discrete        = %s\n"
         "  - Start time      = (%s)\n"
         "  - End time        = (%s)\n"
         "  - Def time units  = (%c)\n",
         DS_isDiscrete ? "true" : "false",
         DS_startTime, DS_endTime, DS_defTimeUnit);

  printf("  - Evidence labels = (%s)  (%s)  (%s)\n",
         DS_evidLabel1, DS_evidLabel2, DS_evidLabel3);

  printf("  - %d  Possibilities:\n", DS_numPossibilities);

  for (int i = 0; i < DS_numPossibilities; i++)
  {
     printf ("    - %04.2f  %04.2f  %s\n", DS_possibilities[i].rangeStart,
       DS_possibilities[i].rangeEnd, DS_possibilities[i].string);
  }

  fflush(stdout);
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int getLine(char *lineOfText, FILE *fp)
{
  do
  {
    if (fgets (lineOfText, 256, fp) == NULL)
    {
      fprintf (stderr, "getLine : Error reading file.\n");
      fflush(stderr);
      return (-1);
    }
  }
  while ((lineOfText[0] == '#') || (lineOfText[0] == '\n'));

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

void DS_AlgoInitConfig(void)
{
  struct timeb  secs;
  struct tm     *timeRec;


  ftime(&secs);
  timeRec = localtime(&secs.time);
  sprintf(DS_startTime, "%d:%d:%d:%d:%d:%d",
          timeRec->tm_year + 1900, timeRec->tm_mon + 1, timeRec->tm_mday,
          timeRec->tm_hour, timeRec->tm_min, timeRec->tm_sec);

  strcpy(DS_endTime, DS_startTime);
  DS_isDiscrete = true;
  DS_defTimeUnit = 'M';

  sprintf(DS_evidLabel1, "?");
  sprintf(DS_evidLabel2, "?");
  sprintf(DS_evidLabel3, "?");

  DS_numPossibilities = 9;

  strcpy(DS_possibilities[0].string, "impossible");
  DS_possibilities[0].rangeStart = 0.0;
  DS_possibilities[0].rangeEnd = 0.0;
  strcpy(DS_possibilities[1].string, "extremely unlikely");
  DS_possibilities[1].rangeStart = 0.0;
  DS_possibilities[1].rangeEnd = 0.05;
  strcpy(DS_possibilities[2].string, "very low chance");
  DS_possibilities[2].rangeStart = 0.05;
  DS_possibilities[2].rangeEnd = 0.20;
  strcpy(DS_possibilities[3].string, "small chance");
  DS_possibilities[3].rangeStart = 0.20;
  DS_possibilities[3].rangeEnd = 0.38;
  strcpy(DS_possibilities[4].string, "may");
  DS_possibilities[4].rangeStart = 0.38;
  DS_possibilities[4].rangeEnd = 0.60;
  strcpy(DS_possibilities[5].string, "meaningful chance");
  DS_possibilities[5].rangeStart = 0.60;
  DS_possibilities[5].rangeEnd = 0.79;
  strcpy(DS_possibilities[6].string, "most likely");
  DS_possibilities[6].rangeStart = 0.79;
  DS_possibilities[6].rangeEnd = 0.95;
  strcpy(DS_possibilities[7].string, "extremely likely");
  DS_possibilities[7].rangeStart = 0.95;
  DS_possibilities[7].rangeEnd = 0.99;
  strcpy(DS_possibilities[8].string, "certain");
  DS_possibilities[8].rangeStart = 0.99;
  DS_possibilities[8].rangeEnd = 1.0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoReadConfig(char *fileName, int mayUseDefault)
{
  FILE   *fp;
  int    successVal = 0;
  int    i;
  char   *token;
  char   flag[8];
  char   Hold[256];


  // Parameter error checking.

  if (fileName == NULL)
  {
    if (!mayUseDefault)
    {
      printf ("DS_AlgoReadConfig : File name is NULL.\n");
      fflush(stdout);
      return (-1);
    }
    else
      successVal = -1;
  }

  if (successVal == 0)
  {
    if ((fp = fopen (fileName, "r")) == NULL)
    {
      if (!mayUseDefault)
      {
        fprintf (stderr, "DS_AlgoReadConfig: Cannot open (%s).\n", fileName);
        return (-1);
      }
      else
        successVal = -1;
    }
  }

  // If couldn't open fileName file, open the default file.

  if (successVal == -1)
  {
    if ((fp = fopen ("DSBFiles/default.cfg", "r")) == NULL)
    {
      fprintf (stderr, "DS_AlgoReadConfig: Cannot open (%s).\n",
               "DSBFiles/default.cfg");
      return (-1);
    }
    else
      successVal = 0;
  }


  // Read data from file.

  if (getLine(Hold, fp))
    return (-1);

  sscanf(Hold, "%s", flag);
  DS_isDiscrete = (strcasecmp(flag, "true") == 0);

  if (getLine(Hold, fp))
    return (-1);

  sscanf(Hold, "%s", DS_startTime);

  if (getLine(Hold, fp))
    return (-1);

  sscanf(Hold, "%s", DS_endTime);

  if (getLine(Hold, fp))
    return (-1);

  token = strtok(Hold, " ");
  DS_defTimeUnit = toupper(token[0]);


  /* Read evidence labels. */

  if (getLine(Hold, fp))
    return (-1);

  sscanf(Hold, "%s %s %s", DS_evidLabel1, DS_evidLabel2, DS_evidLabel3);


  /* Read possibility strings and their value ranges */

  if (getLine(Hold, fp))
    return (-1);

  sscanf(Hold, "%d", &DS_numPossibilities);


  for (i = 0; i < DS_numPossibilities; i++)
  {
    if (getLine(Hold, fp))
      return (-1);

    sscanf (Hold, "%lf %lf %s", &DS_possibilities[i].rangeStart,
            &DS_possibilities[i].rangeEnd, DS_possibilities[i].string);
    strsub(DS_possibilities[i].string, '_', ' ');
  }


  fclose (fp);

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoSaveConfig (char *fileName)
{
  FILE   *fp;         // File pointer
  int    i;           // Loop counter
  char   str[256];    // Character string


  if (fileName == NULL)
  {
    printf ("DS_AlgoSaveConfig : File name is NULL.\n");
    fflush(stdout);
    return (-1);
  }


  // Open the file for writing.

  if ((fp = fopen (fileName, "w")) == NULL)
  {
    printf ("DS_AlgoSaveConfig : Cannot open (%s).\n", fileName);
    fflush(stdout);
    return (-1);
  }


  // Write discrete flag and run times.

  fprintf (fp, "%s                # Discrete\n",
           DS_isDiscrete ? "true " : "false");

  fprintf (fp, "%s  # Start time\n%s  # End time\n",
           DS_startTime, DS_endTime);


  // Write default time unit.

  DS_defTimeUnit = toupper(DS_defTimeUnit);
  switch (DS_defTimeUnit)
  {
    case 'S':
      fprintf(fp, "Sec ");
      break;

    case 'M':
      fprintf(fp, "Min ");
      break;

    case 'H':
      fprintf(fp, "Hour");
      break;

    case 'D':
      fprintf(fp, "Day ");
      break;

    default:
      fprintf(fp, "?   ");
      break;
  }
  fprintf(fp, "                 #  Default time units.  "
          "Must be: Sec, Min, Hours, or Days\n");


  // Write evidence labels.

  fprintf(fp, "%-4s  %-4s  %-4s     "
          "# Evidence labels.  Must be 3, each <= 4 chars\n",
          DS_evidLabel1, DS_evidLabel2, DS_evidLabel3);


  // Write the possibility strings.

  fprintf(fp, "\n%d  Possibilities\n", DS_numPossibilities);

  for (i = 0; i < DS_numPossibilities; i++)
  {
    strcpy(str, DS_possibilities[i].string);
    strsub(str, ' ', '_');
    fprintf (fp, "%04.2f  %04.2f  %s\n", DS_possibilities[i].rangeStart,
             DS_possibilities[i].rangeEnd, str);
  }


  fclose (fp);

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoIsDiscrete (void)
{
  return DS_isDiscrete;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

void DS_AlgoSetDiscrete (int discrete)
{
  DS_isDiscrete = discrete;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoGetTimeRange (char* timeStart, char* timeEnd)
{
  if (timeStart == NULL)
  {
    printf ("DS_AlgoGetTimeRange : timeStart parameter is NULL.\n");
    fflush(stdout);
    return (-1);
  }

  if (timeEnd == NULL)
  {
    printf ("DS_AlgoGetTimeRange : timeEnd parameter is NULL.\n");
    fflush(stdout);
    return (-1);
  }

  strcpy(timeStart, DS_startTime);
  strcpy(timeEnd, DS_endTime);
  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoSetTimeRange (char* timeStart, char* timeEnd)
{
  if (timeStart == NULL)
  {
    printf ("DS_AlgoSetTimeRange : timeStart parameter is NULL.\n");
    fflush(stdout);
    return (-1);
  }

  if (timeEnd == NULL)
  {
    printf ("DS_AlgoSetTimeRange : timeEnd parameter is NULL.\n");
    fflush(stdout);
    return (-1);
  }

  strcpy(DS_startTime, timeStart);
  strcpy(DS_endTime, timeEnd);
  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

char DS_AlgoGetDefaultTimeUnit (void)
{
  return DS_defTimeUnit;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoSetDefaultTimeUnit (char timeUnit)
{
  char  unit = toupper(timeUnit);

  if ((unit != 'D') &&
      (unit != 'H') &&
      (unit != 'M') &&
      (unit != 'S'))
  {
    printf ("DS_AlgoSetDefaultTimeUnit : time unit not H, D, M, or S.\n");
    fflush(stdout);
    return (-1);
  }

  DS_defTimeUnit = unit;
  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoGetEvidenceLabels (char* label1, char* label2, char* label3)
{
  if ((label1 == NULL) || (label2 == NULL) || (label3 == NULL))
  {
    printf ("DS_AlgoGetEvidenceLabels : label is NULL.\n");
    fflush(stdout);
    return (-1);
  }

  strcpy(label1, DS_evidLabel1);
  strcpy(label2, DS_evidLabel2);
  strcpy(label3, DS_evidLabel3);

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoSetEvidenceLabels (char* label1, char* label2, char* label3)
{
  if ((label1 == NULL) || (label2 == NULL) || (label3 == NULL))
  {
    printf ("DS_AlgoSetEvidenceLabels : label is NULL.\n");
    fflush(stdout);
    return (-1);
  }

  strcpy(DS_evidLabel1, label1);
  strcpy(DS_evidLabel2, label2);
  strcpy(DS_evidLabel3, label3);

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoGetNumPossibilities (void)
{
  return DS_numPossibilities;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoSetNumPossibilities (int num)
{
  if ((num < 1) || (num > DS_Max_Possibilities))
  {
    printf ("DS_AlgoSetNumPossibilities : Number of possibilities "
            "%d is out of bounds.\n", num);
    fflush(stdout);
    return (-1);
  }

  DS_numPossibilities = num;

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoGetPossibility (double value, char* string)
{
  int  val = -1;

  if (string == NULL)
  {
    printf ("DS_AlgoGetPossibility : String is NULL.\n");
    fflush(stdout);
    return (-1);
  }

  sprintf(string, "");

  for (int i = 0; i < DS_numPossibilities; i++)
  {
    if ((DS_possibilities[i].rangeStart < value) &&
        (DS_possibilities[i].rangeEnd >= value))
    {
      strcpy(string, DS_possibilities[i].string);
      val = 0;
      break;
    }
  }

  return val;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoGetPossibility (char* string, double &midpoint)
{
  if (string == NULL)
  {
    printf ("DS_AlgoGetPossibility : String is NULL.\n");
    fflush(stdout);
    return (-1);
  }

  for (int i = 0; i < DS_numPossibilities; i++)
  {
    if (strcmp(string, DS_possibilities[i].string) == 0)
    {
      midpoint = (DS_possibilities[i].rangeStart +
                  DS_possibilities[i].rangeEnd) / 2.0;
      break;
    }
  }

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoGetPossibility (int index, char* string, double &rangeStart,
                           double &rangeEnd)
{
  if (string == NULL)
  {
    printf ("DS_AlgoGetPossibility : String is NULL.\n");
    fflush(stdout);
    return (-1);
  }

  sprintf(string, "");

  if ((index < 0) || (index >= DS_numPossibilities))
  {
    printf ("DS_AlgoGetPossibility : Index %d is out of bounds.\n", index);
    fflush(stdout);
    return (-1);
  }

  strcpy(string, DS_possibilities[index].string);
  rangeStart = DS_possibilities[index].rangeStart;
  rangeEnd = DS_possibilities[index].rangeEnd;

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoSetPossibility (int index, char* string, double rangeStart,
                           double rangeEnd)
{
  if (string == NULL)
  {
    printf ("DS_AlgoSetPossibility : Possibility string is NULL.\n");
    fflush(stdout);
    return (-1);
  }

  sprintf(string, "");

  if ((index < 0) || (index >= DS_numPossibilities))
  {
    printf ("DS_AlgoSetPossibility : Index %d is out of bounds.\n", index);
    fflush(stdout);
    return (-1);
  }

  strcpy(DS_possibilities[index].string, string);
  DS_possibilities[index].rangeStart = rangeStart;
  DS_possibilities[index].rangeEnd = rangeEnd;

  return 0;
}

