/*
  speed things up:
    - add addNode and deletenode passing nodeIndex instead of coords
    - change arrays of strings to arrays of pointers to strings
      DON'T FORGET TO DELETE!!!
*/


// NOTE: This code ASSUMES merging tree has extra levels 
//                ONLY AT END OF TREE!!!

// NOTE: On merging trees, if a node was in both trees then it's final
//       tThreshold value is the AVERAGE of the 2 nodes' values.


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <time.h>

#include "DSBattributes.h"
#include "Globals.h"
#include "savedNet.h"


const int  B = 0;
const int  D = 1;


char    DS_timeUnit;
char    DS_chTunit;
char    DS_treeName[DS_Max_String_Length];
char    DS_evidLabel1[DS_Max_String_Length];
char    DS_evidLabel2[DS_Max_String_Length];
char    DS_evidLabel3[DS_Max_String_Length];
char    DS_levelNames[DS_Max_Depth][DS_Max_String_Length];
char    DS_nodeNames[DS_Max_Nodes][DS_Max_String_Length];
char    DS_nodeStrings[DS_Max_Nodes][DS_Max_String_Length];

int     DS_treeDepth;
int     DS_isContinuous;
int     DS_numPossibilities;
int     DS_numNodes;
int     DS_levelWidths[DS_Max_Depth];
int     DS_mergeTreeNodeIsFrom[DS_Max_Nodes];
int     DS_numNodesLost[DS_Max_Depth];

double  DS_startTime;
double  DS_endTime;
double  DS_bThresh[DS_Max_Nodes];
double  DS_tThresh[DS_Max_Nodes];
double  DS_nodeValues[2][DS_Max_Nodes];
double  DS_links[2][DS_Max_Nodes][DS_Max_Nodes];

struct {
  double  rangeStart;
  double  rangeEnd;
  char    string[DS_Max_String_Length];
} DS_possibilities[DS_Max_Possibilities];


int   getNextLine(char*, FILE*);


/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

void printInfo(void)
{
  int  level, node;


  printf("\n(%s) Info:\n", DS_treeName);
  printf("  Depth = %d\n", DS_treeDepth);

  printf("  Level names:\n");
  for (level = 0; level < DS_treeDepth; level++)
    printf("    (%s)\n", DS_levelNames[level]);

  printf("  Level widths:\n");
  for (level = 0; level < DS_treeDepth; level++)
    printf("    %d\n", DS_levelWidths[level]);

  printf("  Num nodes = %d\n", DS_numNodes);


  printf("  Node info:\n");
  level = 0;
  node = 0;

  for (int i = 0; i < DS_numNodes; i++)
  {
    if (node == 0)
      printf("    Level %d:\n", level);

    printf("      (%s)  (%s)  %f  %f\n",
           DS_nodeNames[i], DS_nodeStrings[i], DS_bThresh[i], DS_tThresh[i]);

    if (++node == DS_levelWidths[level])
    {
      node = 0;
      level++;
    }
  }


  printf("  Impacts:\n");
  for (int chNode = 0; chNode < DS_numNodes; chNode++)
  {
    for (int bd = B; bd <= D; bd++)
    {
      printf ("    %2d-%c  ", chNode, bd ? 'D' : 'B');

      for (node = 0; node < DS_numNodes; node++)
      {
        printf ("  % 5.3f", DS_links[bd][chNode][node]);
      }

      printf ("\n");
    }

    printf ("\n");
  }

  fflush(stdout);
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int getNextLine(char *lineOfText, FILE *fp)
{
  do
  {
    if (fgets (lineOfText, DS_Max_String_Length, fp) == NULL)
    {
      fprintf (stderr, "getNextLine : Error reading file.\n");
      return (-1);
    }
  }
  while ((lineOfText[0] == '#') || (lineOfText[0] == '\n'));

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_CoordToIndex(int node, int level)
{
  int  index, n;


  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_CoordToIndex : Node level %d is out of bounds.\n", level);
    fflush(stdout);
    return (-1);
  }

  if ((node < 0) || (node >= DS_levelWidths[level]))
  {
    printf ("DS_CoordToIndex : Node index %d is out of bounds on level %d.\n",
            node, level);
    fflush(stdout);
    return (-1);
  }

  index = node;
  for (n = 0; n < level; n++)
    index += DS_levelWidths[n];

  return index;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_IndexToCoord(int index, int &node, int &level)
{
  int  i;


  if ((index < 0) || (index >= DS_numNodes))
  {
    printf ("DS_IndexToCoord : Node index %d is out of bounds.\n", index);
    return (-1);
  }

  level = 0;
  node = -1;

  for (i = 0; i <= index; i++)
  {
    node++;

    if (node >= DS_levelWidths[level])
    {
      level++;
      node = 0;
    }
  }

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_InitAttributes(char *fname, int verbose)
{
  FILE   *att;
  char   *token;
  char   Hold[DS_Max_String_Length];
  short  BandDImpactsDiffer = TRUE;
  int    bd, level, i, parNode, chNode, node;
  int    mode;


  if (verbose > 1)
  {
    printf("IN DS_InitAttributes\n");fflush(stdout);
    fflush(stdout);
  }

  /* Initialize variables. */

  DS_treeDepth = 0;
  DS_numNodes = 0;

  for (chNode = 0; chNode < DS_Max_Nodes; chNode++)
  {
    for (parNode = 0; parNode < DS_Max_Nodes; parNode++)
    {
      DS_links[B][chNode][parNode] = DS_links[D][chNode][parNode] = 0;
    }
  }

  sprintf(DS_treeName, "???");

  for (level = 0; level < DS_Max_Depth; level++)
  {
    DS_levelWidths[level] = 0;
    DS_levelNames[level][0] = '\0';
    DS_numNodesLost[level] = 0;
  }

  for (i = 0; i < DS_Max_Nodes; i++)
  {
    DS_nodeNames[i][0] = '\0';
    DS_nodeStrings[i][0] = '\0';
    DS_bThresh[i] = 0.75;
    DS_tThresh[i] = 60;
    DS_mergeTreeNodeIsFrom[i] = DS_Current_Tree;
  }

  strcpy(DS_possibilities[0].string, "impossible");
  DS_possibilities[0].rangeStart = 0.0;
  DS_possibilities[0].rangeEnd = 0.0;
  strcpy(DS_possibilities[1].string, "extremely unlikely");
  DS_possibilities[1].rangeStart = 0.01;
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
  DS_possibilities[8].rangeStart = 1.0;
  DS_possibilities[8].rangeEnd = 1.0;

  DS_numPossibilities = 9;

  sprintf(DS_evidLabel1, "???");
  sprintf(DS_evidLabel2, "???");
  sprintf(DS_evidLabel3, "???");

  DS_isContinuous = 0;

  DS_timeUnit = 'H';
  DS_chTunit = 'H';

  DS_startTime = DS_endTime = 0.0;


  /* If no attribute file was specified, init done, return. */

  if (fname == NULL)
    return 0;


  if (verbose > 1)
  {
    printf("OPEN (%s)...\n", fname);fflush(stdout);
    fflush(stdout);
  }

  if ((att = fopen (fname, "r")) == NULL)
  {
    fprintf (stderr, "DS_InitAttributes: Cannot open (%s).\n", fname);
    return (-1);
  }


  /* Read in general tree info. */

  if (verbose > 1)
  {
    printf("READ first line...\n");fflush(stdout);
    fflush(stdout);
  }

  if (getNextLine(Hold, att))
    return (-1);
  
  if (verbose > 1)
  {
    printf("PARSE first line...\n");fflush(stdout);
    fflush(stdout);
  }

  sscanf(Hold, "%d %s %d %lf %lf %c",
         &DS_treeDepth, DS_treeName, &mode,
         &DS_startTime, &DS_endTime, &DS_timeUnit);

  if ((DS_treeDepth < 1) || (DS_treeDepth > DS_Max_Depth))
  {
    printf ("Number of levels in tree must range 1...%1d\n", DS_Max_Depth);
    return (-1);
  }

  strsub(DS_treeName, '_', ' ');

  if (mode == 0)
    DS_isContinuous = TRUE;
  else
    DS_isContinuous = FALSE;

  DS_timeUnit = toupper(DS_timeUnit);


  /* Read in the number of nodes that are at each level of tree */

  DS_numNodes = 0;
  i = 0;

  for (level = 0; level < DS_treeDepth; level++)
  {
    /* Read # nodes and level name. */

    if (verbose > 1)
    {
      printf("READ level (%d) line...\n", level);fflush(stdout);
      fflush(stdout);
    }

    if (getNextLine(Hold, att))
      return (-1);
  
    if (verbose > 1)
    {
      printf("PARSE line...\n");fflush(stdout);
      fflush(stdout);
    }

    sscanf (Hold, "%d %s", &DS_levelWidths[level], DS_levelNames[level]);

    if ((DS_levelWidths[level] < 1) || (DS_levelWidths[level] > DS_Max_Width))
    {
      printf ("Number of nodes in a level of the tree must range 1...%1d\n",
              DS_Max_Width);
      return (-1);
    }

    strsub(DS_levelNames[level], '_', ' ');

    DS_numNodes += DS_levelWidths[level];


    for (node = 0; node < DS_levelWidths[level]; node++)
    {
      /* Read node name, node description and B and T thresholds. */

      if (verbose > 1)
      {
        printf("READ node (%d) line...\n", i);fflush(stdout);
        fflush(stdout);
      }

      if (getNextLine(Hold, att))
        return (-1);

      if (verbose > 1)
      {
        printf("PARSE line...\n");fflush(stdout);
        fflush(stdout);
      }

      sscanf(Hold, "%s %s %lf %lf",
             DS_nodeNames[i], DS_nodeStrings[i],
             &DS_bThresh[i], &DS_tThresh[i]);

      strsub(DS_nodeNames[i], '_', ' ');
      strsub(DS_nodeStrings[i], '_', ' ');
      i++;
    }
  }


  /* Read in the parent-to-child link matrices. */

  if (verbose > 1)
  {
    printf("READ T/F line...\n");fflush(stdout);
    fflush(stdout);
  }

  if (getNextLine(Hold, att))
    return (-1);

  if (verbose > 1)
  {
    printf("PARSE line...\n");fflush(stdout);
    fflush(stdout);
  }

  if ((Hold[0] == 'T') || (Hold[0] == 't'))
    BandDImpactsDiffer = TRUE;
  else
    BandDImpactsDiffer = FALSE;

  for (bd = B; bd <= D; bd++)
  {
    for (chNode = 0; chNode < DS_numNodes; chNode++)
    {
      if ((bd == B) || BandDImpactsDiffer)
      {
        if (verbose > 1)
        {
          printf("READ linkWts (%d) line...\n", chNode);fflush(stdout);
          fflush(stdout);
        }

        if (getNextLine(Hold, att))
          return (-1);

        if (strncmp(Hold, " ----", 5) == 0)
        {
          if (getNextLine(Hold, att))
            return (-1);
        }

        if (verbose > 1)
        {
          printf("  (%s)\n", Hold);fflush(stdout);
          printf("PARSE line...\n");fflush(stdout);
          fflush(stdout);
        }

        token = strtok(Hold, " |");

        for (parNode = 0; parNode < DS_numNodes; parNode++)
        {
          sscanf (token, "%lf", &DS_links[bd][chNode][parNode]);
          token = strtok(NULL, " |");
        }
      }
      else
      {
        for (parNode = 0; parNode < DS_numNodes; parNode++)
        {
          DS_links[D][chNode][parNode] = DS_links[B][chNode][parNode];
        }
      }
    }
  }


  /* Read possibility strings and their value ranges */

  if (verbose > 1)
  {
    printf("READ numPoss line...\n");fflush(stdout);
    fflush(stdout);
  }

  if (getNextLine(Hold, att))
    return (-1);

  if (verbose > 1)
  {
    printf("PARSE line...\n");fflush(stdout);
    fflush(stdout);
  }

  sscanf(Hold, "%d", &DS_numPossibilities);

  for (i = 0; i < DS_numPossibilities; i++)
  {
    if (verbose > 1)
    {
      printf("READ poss (%d) line...\n", i);fflush(stdout);
      fflush(stdout);
    }

    if (getNextLine(Hold, att))
      return (-1);

    if (verbose > 1)
    {
      printf("PARSE line...\n");fflush(stdout);
      fflush(stdout);
    }

    sscanf (Hold, "%lf %lf %s", &DS_possibilities[i].rangeStart,
            &DS_possibilities[i].rangeEnd, DS_possibilities[i].string);
    strsub(DS_possibilities[i].string, '_', ' ');
  }


  /* Read evidence labels. */

  if (verbose > 1)
  {
    printf("READ evid line...\n");fflush(stdout);
    fflush(stdout);
  }

  if (getNextLine(Hold, att))
    return (-1);

  if (verbose > 1)
  {
    printf("PARSE line...\n");fflush(stdout);
    fflush(stdout);
  }

  sscanf(Hold, "%s %s %s", DS_evidLabel1, DS_evidLabel2, DS_evidLabel3);


  /* Read T unit. */

  if (verbose > 1)
  {
    printf("READ Tunit line...\n");fflush(stdout);
    fflush(stdout);
  }

  if (getNextLine(Hold, att))
    return (-1);

  if (verbose > 1)
  {
    printf("PARSE line...\n");fflush(stdout);
    fflush(stdout);
  }

  token = strtok(Hold, " ");
  DS_chTunit = toupper(token[0]);

  fclose (att);



  if (verbose > 1)
  {
    printf("print input...\n");fflush(stdout);
    fflush(stdout);
  }

  if (verbose)
  {
    printf("tree (%s)  mode = %d  startTime = %f  endTime = %f %c\n",
           DS_treeName, mode, DS_startTime, DS_endTime, DS_timeUnit);

    level = 0;
    node = 0;
    for (i = 0; i < DS_numNodes; i++)
    {
      if (node == 0)
        printf("\nlevel (%s)  has %d nodes\n",
               DS_levelNames[level], DS_levelWidths[level]);

      printf("  - (%s)\n        (%s)\n        %f  %f\n",
             DS_nodeNames[i], DS_nodeStrings[i], DS_bThresh[i], DS_tThresh[i]);

      if (++node == DS_levelWidths[level])
      {
        node = 0;
        level++;
      }
    }

    for (bd = B; bd <= D; bd++)
    {
      if (bd == B)
      {
        if (BandDImpactsDiffer)
          printf ("Belief Array.\n\n");
        else
          printf ("Belief/Disbelief Array.\n\n");
      }
      else
        printf ("Disbelief Array.\n\n");

      for (chNode = 0; chNode < DS_numNodes; chNode++)
      {
        for (parNode = 0; parNode < DS_numNodes; parNode++)
        {
          printf ("%8.3f", DS_links[bd][chNode][parNode]);
        }
        printf ("\n\n");
      }

      if (!BandDImpactsDiffer)
        break;
    }

    printf("%d  Possibilities:\n", DS_numPossibilities);

    for (i = 0; i < DS_numPossibilities; i++)
    {
       printf ("  %04.2f  %04.2f  %s\n", DS_possibilities[i].rangeStart,
         DS_possibilities[i].rangeEnd, DS_possibilities[i].string);
    }

    printf("\nEvidence labels:  (%s)  (%s)  (%s)\n\n",
           DS_evidLabel1, DS_evidLabel2, DS_evidLabel3);

    printf("%c  = Default time units\n\n", DS_chTunit);
    fflush(stdout);
  }

  fflush(stdin);

  return(0);
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoSaveAttributes (char  *fname)
{
  FILE   *att;                             // Attribute file pointer
  char   str[DS_Max_String_Length];        // Character string
  char   sep;                              // Column separater
  char   sepLine[DS_Max_String_Length];    // Line separater
  char   blankLine[DS_Max_String_Length];  // Blank line
  short  BandDImpactsDiffer = TRUE;        // TRUE if B and D impact mats differ
  int    i, l, n, level, node, chNode, parNode;  // Loop counters
  int    numMatrices;                  // 1 if B and D impact mats same, else 2
  int    numNameChars;
  int    numStringChars;


  if (fname == NULL)
  {
    printf ("DS_AlgoSaveAttributes : File name is NULL.\n");
    fflush(stdout);
    return (-1);
  }

  if ((att = fopen (fname, "w")) == NULL)
  {
    printf ("DS_AlgoSaveAttributes : Cannot open (%s).\n", fname);
    return (-1);
  }


  /* Write the general tree information. */

  strcpy(str, DS_treeName);
  strsub(str, ' ', '_');

  if (DS_isContinuous)
    i = 0;
  else
    i = 1;

  fprintf (att,
           "# Tree height, Name, DS_isContinuous,"
           " Start time, End time, Time unit\n");
  fprintf (att, "%d  %s  %d  %3.1f  %3.1f  %c\n\n",
           DS_treeDepth, str, i, DS_startTime, DS_endTime, DS_timeUnit);


  /* Write the width of the tree at each level, and the level name. */
  n = 0;
  for (level = 0; level < DS_treeDepth; level++)
  {
    strcpy(str, DS_levelNames[level]);
    strsub(str, ' ', '_');
    fprintf (att, "%d     %s\n", DS_levelWidths[level], str);

    numNameChars = 0;
    for (node = n; node < n + DS_levelWidths[level]; node++)
    {
      if (numNameChars < strlen(DS_nodeNames[node]))
        numNameChars = strlen(DS_nodeNames[node]);
    }

    numStringChars = 0;
    for (node = n; node < n + DS_levelWidths[level]; node++)
    {
      if (numStringChars < strlen(DS_nodeStrings[node]))
        numStringChars = strlen(DS_nodeStrings[node]);
    }

    for (node = n; node < n + DS_levelWidths[level]; node++)
    {
      /* Write node name, node description, and B and T thresholds. */

      strcpy(str, DS_nodeNames[node]);
      strsub(str, ' ', '_');
      fprintf(att, "  %1$s%2$*3$s", str, " ",
              numNameChars + 3 - strlen(DS_nodeNames[node]));

      strcpy(str, DS_nodeStrings[node]);
      strsub(str, ' ', '_');
      fprintf(att, "%1$s%2$*3$s", str, " ",
              numStringChars + 3 - strlen(DS_nodeStrings[node]));

      fprintf(att, "%4.2f  %4.2f\n",
              DS_bThresh[node], DS_tThresh[node]);
    }

    fprintf(att, "\n");
    n += DS_levelWidths[level];
  }


  /* Determine whether or not belief and disbelief impact *
   * matrices are the same, write the answer to file.     */

  BandDImpactsDiffer = FALSE;

  for (chNode = 0; (chNode < DS_numNodes) && !BandDImpactsDiffer; chNode++)
  {
    for (parNode = 0; parNode < DS_numNodes; parNode++)
    {
      if (DS_links[B][chNode][parNode] != DS_links[D][chNode][parNode])
      {
        BandDImpactsDiffer = TRUE;
        break;
      }
    }
  }

  if (BandDImpactsDiffer)
  {
    numMatrices = 2;
    fprintf (att, "true    B and D matrices are listed separately\n");
  }
  else
  {
    numMatrices = 1;
    fprintf (att, "false   B and D matrices are listed separately\n");
  }


  /* Create a blank line and separator line to improve readability. */

  memset(blankLine, ' ', DS_numNodes * 7);
  memset(sepLine, '-', DS_numNodes * 7);
  sepLine[0] = ' ';
  i = 0;
  for (level = 0; level < DS_treeDepth; level++)
  {
    i += DS_levelWidths[level] * 7;
    blankLine[i - 1] = '|';
    sepLine[i - 1] = '|';
  }
  blankLine[0] = '#';
  blankLine[DS_numNodes * 7] = '\0';
  sepLine[0] = '#';
  sepLine[DS_numNodes * 7] = '\0';


  /* Write the impact matrices. */

  for (i = 0; i < numMatrices; i++)
  {
    /* Label the columns. */

    fprintf (att, "# E0   ");

    for (level = 0; level < DS_treeDepth; level++)
    {
      if (level == 0)
      {
        for (node = 1; node < DS_levelWidths[level]; node++)
        {
          fprintf (att, "  E%d   ", node);
        }
      }
      else if (level == DS_treeDepth - 1)
      {
        for (node = 0; node < DS_levelWidths[level]; node++)
        {
          fprintf (att, "  A%d   ", node);
        }
      }
      else
      {
        if (DS_treeDepth > 3)
        {
          for (node = 0; node < DS_levelWidths[level]; node++)
            fprintf (att, " H%d_%d  ", level - 1, node);
        }
        else
        {
          for (node = 0; node < DS_levelWidths[level]; node++)
            fprintf (att, "  H%d   ", node);
        }
      }
    }

    fprintf (att, "\n#");
    for (node = 0; node < DS_numNodes; node++)
      fprintf (att, "----   ");
    fprintf (att, "\n");


    /* Write out the matrix values. */

    level = node = 0;
    for (chNode = 0; chNode < DS_numNodes; chNode++)
    {
      l = n = 0;
      for (parNode = 0; parNode < DS_numNodes; parNode++)
      {
        if (n == DS_levelWidths[l] - 1)
        {
          sep = '|';
          n = 0;
          l++;
        }
        else
        {
          sep = ' ';
          n++;
        }

        fprintf (att, " %4.2f %c", DS_links[i][chNode][parNode], sep);
      }

      if (level == 0)
      {
        fprintf (att, "#  E%d\n", node);
      }
      else if (level == DS_treeDepth - 1)
      {
        fprintf (att, "#  A%d\n", node);
      }
      else
      {
        if (DS_treeDepth > 3)
        {
          fprintf (att, "#  H%d_%d\n", level - 1, node);
        }
        else
        {
          fprintf (att, "#  H%d\n", node);
        }
      }

      node++;

      if (node >= DS_levelWidths[level])
      {
        level++;
        node = 0;

        fprintf (att, "%s\n", sepLine);
      }
      else
        fprintf (att, "%s\n", blankLine);
    }

    fprintf (att, "\n");
  }


  /* Write the possibility strings. */

  fprintf(att, "%d  Possibilities\n", DS_numPossibilities);

  for (i = 0; i < DS_numPossibilities; i++)
  {
    strcpy(str, DS_possibilities[i].string);
    strsub(str, ' ', '_');
    fprintf (att, "%04.2f  %04.2f  %s\n", DS_possibilities[i].rangeStart,
             DS_possibilities[i].rangeEnd, str);
  }


  /* Write evidence labels. */
  fprintf(att, "\n#  Evidence labels follow. Must be 3, each < 4 chars\n");
  fprintf(att, "%s      %s      %s\n",
          DS_evidLabel1, DS_evidLabel2, DS_evidLabel3);


  /* Write T unit. */
  fprintf(att,
          "#  Default time units follow. Must be: Sec, Min, Hour, or Days\n");
  DS_chTunit = toupper(DS_chTunit);
  switch (DS_chTunit)
  {
    case 'S':
      fprintf(att, "Sec\n");
      break;

    case 'M':
      fprintf(att, "Min\n");
      break;

    case 'H':
      fprintf(att, "Hour\n");
      break;

    case 'D':
      fprintf(att, "Day\n");
      break;

    default:
      fprintf(att, "?\n");
      break;
  }

  fclose (att);

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoGetTreeDepth (void)
{
  return DS_treeDepth;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoSetTreeDepth (int depth)
{
  if ((depth < 3) || (depth > DS_Max_Depth))
  {
    printf ("DS_AlgoSetTreeDepth : Depth %d is out of bounds.\n", depth);
    return -1;
  }

  DS_treeDepth = depth;

  return DS_treeDepth;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

void DS_AlgoGetGeneralInfo (int &depthOfTree, char* nameOfTree,
                            int &isContinuous, double &timeStart,
                            double &timeEnd, char &unitOfTime)
{
  depthOfTree = DS_treeDepth;
  strcpy(nameOfTree, DS_treeName);
  isContinuous = DS_isContinuous;
  timeStart = DS_startTime;
  timeEnd = DS_endTime;
  unitOfTime = DS_timeUnit;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoSetGeneralInfo (int depthOfTree, char* nameOfTree, int isContinuous,
                            double timeStart, double timeEnd, char unitOfTime)
{
  if (nameOfTree == NULL)
  {
    printf ("DS_AlgoSetGeneralInfo : Tree name is NULL.\n");
    return (-1);
  }

  if ((depthOfTree < 3) || (depthOfTree > DS_Max_Depth))
  {
    printf ("DS_AlgoSetGeneralInfo : Tree depth is out of bounds.\n");
    return (-1);
  }

  unitOfTime = toupper(unitOfTime);
  if ((unitOfTime != 'D') && (unitOfTime != 'H') &&
      (unitOfTime != 'M') && (unitOfTime != 'S'))
  {
    printf ("DS_AlgoSetGeneralInfo : Unrecognized time unit %c.\n", unitOfTime);
    return (-1);
  }

  DS_treeDepth = depthOfTree;
  strcpy(DS_treeName, nameOfTree);
  DS_isContinuous = isContinuous;
  DS_startTime = timeStart;
  DS_endTime = timeEnd;
  DS_timeUnit = unitOfTime;

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoGetLevelWidth (int level)
{
  int  width = 0;


  if ((level >= 0) && (level < DS_treeDepth))
    width = DS_levelWidths[level];

  return width;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoSetLevelWidth (int level, int width)
{
  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_AlgoSetLevelWidth : Level %d is out of bounds.\n", level);
    return (-1);
  }

  if ((width < 0) || (width > DS_Max_Width))
  {
    printf ("DS_AlgoSetLevelWidth : Width %d is out of bounds.\n", width);
    return (-1);
  }

  DS_numNodes -= DS_levelWidths[level];
  DS_levelWidths[level] = width;
  DS_numNodes += width;

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoGetLevelName (int level, char* name)
{
  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_AlgoGetLevelName : Level %d is out of bounds.\n", level);
    strcpy(name, "");
    return (-1);
  }

  strcpy(name, DS_levelNames[level]);

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoSetLevelName (int level, char* name)
{
  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_AlgoSetLevelName : Level %d is out of bounds.\n", level);
    return (-1);
  }

  strcpy(DS_levelNames[level], name);

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AddNode (int node, int level, char *name, char *description,
                double Bthreshold, double Tthreshold)
{
  int  bd, i, n, l, parNode, chNode;


  i = 0;
  for (l = 0; l < DS_treeDepth; l++)
  {
    for (n = 0; n < DS_levelWidths[l]; n++)
    {
      if (strcmp(name, DS_nodeNames[i]) == 0)
      {
        printf ("DS_AlgoAddNode : Node (%s) is already in the tree at %d,%d.\n",
                name, l, n);
        return (-2);
      }

      i++;
    }
  }

  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_AlgoAddNode : Node level %d is out of bounds.\n", level);
    return (-1);
  }

  if (DS_levelWidths[level] >= DS_Max_Width)
  {
    printf ("DS_AlgoAddNode : No room for node on level %d.\n", level);
    return (-1);
  }


  if (node < 0)
    node = 0;

  if (node > DS_levelWidths[level])
  {
    node = DS_levelWidths[level];
  }


  DS_levelWidths[level]++;
  DS_numNodes++;

  i = DS_CoordToIndex(node, level);


  // Shift down the name and description strings, thresholds,
  // and merge flag to make room for the node.

  for (n = DS_numNodes; n > i; n--)
  {
    strcpy (DS_nodeNames[n], DS_nodeNames[n - 1]);
    strcpy (DS_nodeStrings[n], DS_nodeStrings[n - 1]);
    DS_bThresh[n] = DS_bThresh[n - 1];
    DS_tThresh[n] = DS_tThresh[n - 1];
    DS_mergeTreeNodeIsFrom[n] = DS_mergeTreeNodeIsFrom[n - 1];
  }


  // Add the new node's name and description
  // strings, threshold values, and merge flag.

  if (name != NULL)
    strcpy (DS_nodeNames[i], name);
  else
    sprintf (DS_nodeNames[i], "");

  if (description != NULL)
    strcpy (DS_nodeStrings[i], description);
  else
    sprintf (DS_nodeStrings[i], "");

  DS_bThresh[i] = Bthreshold;
  DS_tThresh[i] = Tthreshold;

  DS_mergeTreeNodeIsFrom[i] = DS_Current_Tree;


  // Shift down the node columns in the links matrix to make room for new node.

  for (bd = B; bd <= D; bd++)
  {
    for (chNode = 0; chNode < DS_numNodes; chNode++)
    {
      for (parNode = DS_numNodes - 1; parNode > i; parNode--)
      {
        DS_links[bd][chNode][parNode] = DS_links[bd][chNode][parNode - 1];
      }

      DS_links[bd][chNode][parNode] = 0;
    }
  }


  // Shift down the node rows in the links matrix to make room for new node.

  for (bd = B; bd <= D; bd++)
  {
    for (chNode = DS_numNodes - 1; chNode > i; chNode--)
    {
      for (parNode = 0; parNode < DS_numNodes; parNode++)
      {
        DS_links[bd][chNode][parNode] = DS_links[bd][chNode - 1][parNode];
      }
    }

    for (parNode = 0; parNode < DS_numNodes; parNode++)
    {
      DS_links[bd][chNode][parNode] = 0;
    }
  }

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_DeleteNode (int node, int level)
{
  int  bd, i, n, parNode, chNode;


  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_AlgoDeleteNode : Node level %d is out of bounds.\n", level);
    return (-1);
  }

  if ((node < 0) || (node >= DS_levelWidths[level]))
  {
    printf ("DS_AlgoDeleteNode : Node index %d is out of bounds.\n", node);
    return (-1);
  }


  i = DS_CoordToIndex(node, level);

  // Shift the name and description strings, the threshold
  // values, and the merge flag up to delete the node.

  for (n = i; n < DS_numNodes - 1; n++)
  {
    strcpy (DS_nodeNames[n], DS_nodeNames[n + 1]);
    strcpy (DS_nodeStrings[n], DS_nodeStrings[n + 1]);
    DS_bThresh[n] = DS_bThresh[n + 1];
    DS_tThresh[n] = DS_tThresh[n + 1];
    DS_mergeTreeNodeIsFrom[n] = DS_mergeTreeNodeIsFrom[n + 1];
  }

  // Shift up the node columns of the links matrix to delete the node.

  for (bd = B; bd <= D; bd++)
  {
    for (chNode = 0; chNode < DS_numNodes; chNode++)
    {
      for (parNode = i; parNode < DS_numNodes - 1; parNode++)
      {
        DS_links[bd][chNode][parNode] = DS_links[bd][chNode][parNode + 1];
      }

      DS_links[bd][chNode][parNode] = 0;
    }
  }


  // Shift up the node rows of the links matrix to delete the node.

  for (bd = B; bd <= D; bd++)
  {
    for (chNode = i; chNode < DS_numNodes - 1; chNode++)
    {
      for (parNode = 0; parNode < DS_numNodes; parNode++)
      {
        DS_links[bd][chNode][parNode] = DS_links[bd][chNode + 1][parNode];
      }
    }

    for (parNode = 0; parNode < DS_numNodes; parNode++)
    {
      DS_links[bd][chNode][parNode] = 0;
    }
  }

  DS_levelWidths[level]--;
  DS_numNodes--;

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */    
int DS_AlgoGetNodeCoordinates (char *name, int &node, int &level)
{
  if (name == NULL)
  {
    printf ("DS_AlgoGetNodeCoordinates : Node name is NULL.\n");
    return -1;
  }

  int i = 0;
  for (level = 0; level < DS_treeDepth; level++)
  {
    for (node = 0; node < DS_levelWidths[level]; node++)
    {
      if (strcasecmp(DS_nodeNames[i], name) == 0)
        return 0;

      i++;
    }
  }

  node = -1;
  level = -1;
  return -1;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */    
int DS_AlgoGetNodeIndex (char *name)
{
  if (name == NULL)
  {
    printf ("DS_AlgoGetNodeIndex : Node name is NULL.\n");
    return -1;
  }

  for (int i = 0; i < DS_numNodes; i++)
  {
    if (strcasecmp(DS_nodeNames[i], name) == 0)
      return i;
  }

  return -1;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoGetNodeName (int node, int level, char *name)
{
  strcpy(name, "");

  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_AlgoGetNodeName : Node level %d is out of bounds.\n",
            level);
    return (-1);
  }

  if ((node < 0) || (node >= DS_levelWidths[level]))
  {
    printf ("DS_AlgoGetNodeName : Node index %d is out of bounds.\n",
            node);
    return (-1);
  }

  strcpy(name, DS_nodeNames[DS_CoordToIndex (node, level)]);

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoSetNodeName (int node, int level, char *name)
{
  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_AlgoSetNodeName : Node level %d is out of bounds.\n", level);
    return (-1);
  }

  if ((node < 0) || (node >= DS_levelWidths[level]))
  {
    printf ("DS_AlgoSetNodeName : Node index %d is out of bounds.\n", node);
    return (-1);
  }

  if (name == NULL)
  {
    printf ("DS_AlgoSetNodeName : Name string is NULL.\n");
    return (-1);
  }

  strcpy(DS_nodeNames[DS_CoordToIndex (node, level)], name);

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoGetNodeDescription (int node, int level, char *description)
{
  strcpy(description, "");

  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_AlgoGetNodeDescription : Node level %d is out of bounds.\n",
            level);
    return (-1);
  }

  if ((node < 0) || (node >= DS_levelWidths[level]))
  {
    printf ("DS_AlgoGetNodeDescription : Node index %d is out of bounds.\n",
            node);
    return (-1);
  }

  strcpy(description, DS_nodeStrings[DS_CoordToIndex (node, level)]);

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoSetNodeDescription (int node, int level, char *description)
{
  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_AlgoSetNodeDescription : Node level %d is out of bounds.\n",
            level);
    return (-1);
  }

  if ((node < 0) || (node >= DS_levelWidths[level]))
  {
    printf ("DS_AlgoSetNodeDescription : Node index %d is out of bounds.\n",
            node);
    return (-1);
  }

  if (description == NULL)
  {
    printf ("DS_AlgoSetNodeDescription : Description string is NULL.\n");
    return (-1);
  }

  strcpy(DS_nodeStrings[DS_CoordToIndex (node, level)], description);

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoGetThresholds (int node, int level, double &Bthreshold,
                          double &Tthreshold)
{
  Bthreshold = -1;
  Tthreshold = -1;

  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_AlgoGetThresholds : Node level %d is out of bounds.\n", level);
    return (-1);
  }

  if ((node < 0) || (node >= DS_levelWidths[level]))
  {
    printf ("DS_AlgoGetThresholds : Node index %d is out of bounds.\n", node);
    return (-1);
  }

  Bthreshold = DS_bThresh[DS_CoordToIndex (node, level)];
  Tthreshold = DS_tThresh[DS_CoordToIndex (node, level)];

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoSetThresholds (int node, int level, double Bthreshold,
                          double Tthreshold)
{
  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_AlgoSetThresholds : Node level %d is out of bounds.\n", level);
    return (-1);
  }

  if ((node < 0) || (node >= DS_levelWidths[level]))
  {
    printf ("DS_AlgoSetThresholds : Node index %d is out of bounds.\n", node);
    return (-1);
  }

  DS_bThresh[DS_CoordToIndex (node, level)] = Bthreshold;
  DS_tThresh[DS_CoordToIndex (node, level)] = Tthreshold;

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoSetNodeImpact (int srcNode, int srcLevel, int destNode,
                          int destLevel, float B_Weight, float D_Weight)
{
  int  srcIndex;
  int  destIndex;
  int  success = 0;


  if ((srcLevel < 0) || (srcLevel >= DS_treeDepth))
  {
    printf ("DS_AlgoSetNodeImpact : Parent node level %d is out of bounds.\n",
            srcLevel);
    return (-1);
  }

  if ((srcNode < 0) || (srcNode >= DS_levelWidths[srcLevel]))
  {
    printf ("DS_AlgoSetNodeImpact : Parent node index %d is out of bounds.\n",
            srcNode);
    return (-1);
  }

  if ((destLevel < 0) || (destLevel >= DS_treeDepth))
  {
    printf ("DS_AlgoSetNodeImpact : Child node level %d is out of bounds.\n",
            destLevel);
    return (-1);
  }

  if ((destNode < 0) || (destNode >= DS_levelWidths[destLevel]))
  {
    printf ("DS_AlgoSetNodeImpact : Child node index %d is out of bounds.\n",
            destNode);
    return (-1);
  }


  srcIndex = DS_CoordToIndex(srcNode, srcLevel);
  destIndex = DS_CoordToIndex(destNode, destLevel);

/*****ttt THIS LOWER-BOUNDS THE WEIGHTS
  if (B_Weight >= 0.0)
    DS_links[B][destIndex][srcIndex] = B_Weight;
  else
  {
    DS_links[B][destIndex][srcIndex] = 0.0;
    success = -1;
  }

  if (D_Weight >= 0.0)
    DS_links[D][destIndex][srcIndex] = D_Weight;
  else
  {
    DS_links[D][destIndex][srcIndex] = 0.0;
    success = -1;
  }
ttt*****/
  DS_links[B][destIndex][srcIndex] = B_Weight;
  DS_links[D][destIndex][srcIndex] = D_Weight;

  return success;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoGetEvidenceLabels (char* label1, char* label2, char* label3)
{
  if ((label1 == NULL) || (label2 == NULL) || (label3 == NULL)) 
  {
    printf ("DS_AlgoGetEvidenceLabels : label is NULL.\n");
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
    return (-1);
  }

  strcpy(DS_evidLabel1, label1);
  strcpy(DS_evidLabel2, label2);
  strcpy(DS_evidLabel3, label3);

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

void DS_AlgoGetTUnit (char &tUnit)
{
  tUnit = DS_chTunit;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

void DS_AlgoSetTUnit (char tUnit)
{
  DS_chTunit = toupper(tUnit);
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoGetPossibility (double value, char* string)
{
  int  val = -1;

  if (string == NULL) 
  {
    printf ("DS_AlgoGetPossibility : String is NULL.\n");
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

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoGetPossibility (int index, char* string, double &rangeStart,
                           double &rangeEnd)
{
  if (string == NULL) 
  {
    printf ("DS_AlgoGetPossibility : String is NULL.\n");
    return (-1);
  }

  sprintf(string, "");

  if ((index < 0) || (index >= DS_numPossibilities))
  {
    printf ("DS_AlgoGetPossibility : Index %d is out of bounds.\n", index);
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
    return (-1);
  }

  sprintf(string, "");

  if ((index < 0) || (index >= DS_numPossibilities))
  {
    printf ("DS_AlgoSetPossibility : Index %d is out of bounds.\n", index);
    return (-1);
  }

  strcpy(DS_possibilities[index].string, string);
  DS_possibilities[index].rangeStart = rangeStart;
  DS_possibilities[index].rangeEnd = rangeEnd;

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
    return (-1);
  }

  DS_numPossibilities = num;

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_MergeTrees (int mergeFunction, char* structFile, char* dataFile)
{
  FILE    *att;
  char    *token;
  char    Hold[1500];
  char    other_nodeNames[DS_Max_Nodes][DS_Max_String_Length];
  char    other_nodeStrings[DS_Max_Nodes][DS_Max_String_Length];
  char    other_levelNames[DS_Max_Depth][DS_Max_String_Length];
  short   BandDImpactsDiffer = TRUE;
  short   nodeFound = FALSE;
  int     n, bd;
  int     level, chLevel, node, parNode, chNode;
  int     DS_level, DS_chLevel, DS_node, DS_chNode;
  int     other_index, chIndex;
  int     DS_index, DS_chIndex;
  int     other_numNodes;
  int     other_treeDepth;
  int     other_levelWidths[DS_Max_Depth];
  double  other_bThresh[DS_Max_Nodes];
  double  other_tThresh[DS_Max_Nodes];
  double  other_links[2][DS_Max_Nodes][DS_Max_Nodes];


  if ((mergeFunction != DS_Intersection) && (mergeFunction != DS_Union))
  {
    fprintf (stderr, "DS_AlgoMergeTrees : Unrecognized merge function (%d).\n",
             mergeFunction);
    return -1;
  }

  if (structFile == NULL)
  {
    fprintf (stderr, "DS_AlgoMergeTrees : Tree filename is NULL.\n");
    return -1;
  }

  if (dataFile == NULL)
  {
    fprintf (stderr, "DS_AlgoMergeTrees : Data filename is NULL.\n");
    return -1;
  }


  // Init merging-tree data.

  for (chNode = 0; chNode < DS_Max_Nodes; chNode++)
  {
    DS_mergeTreeNodeIsFrom[chNode] = DS_Current_Tree;
    other_nodeNames[chNode][0] = '\0';
    other_nodeStrings[chNode][0] = '\0';
    other_bThresh[chNode] = 0.75;
    other_tThresh[chNode] = 60;

    for (parNode = 0; parNode < DS_Max_Nodes; parNode++)
    {
      other_links[B][chNode][parNode] = other_links[D][chNode][parNode] = 0;
    }
  }

  for (level = 0; level < DS_Max_Depth; level++)
  {
    other_levelWidths[level] = 0;
    DS_numNodesLost[level] = 0;
  }


  // Read in the description of the merging-tree.

  if ((att = fopen (structFile, "r")) == NULL)
  {
    fprintf (stderr, "DS_AlgoMergeTrees : Cannot open (%s).\n", structFile);
    return (-1);
  }


  /* Read in number of tree levels. */

  if (getNextLine(Hold, att))
    return (-1);
  
  sscanf(Hold, "%d", &other_treeDepth);

  if ((other_treeDepth < 1) || (other_treeDepth > DS_Max_Depth))
  {
    printf ("DS_AlgoMergeTrees : Number of levels in tree (%d)"
            " must range 1...%1d\n", other_treeDepth, DS_Max_Depth);
    return (-1);
  }


  /* Read level names and node info. */

  other_numNodes = 0;
  n = 0;

  for (level = 0; level < other_treeDepth; level++)
  {
    if (getNextLine(Hold, att))
      return (-1);
  
    sscanf (Hold, "%d %s", &other_levelWidths[level], other_levelNames[level]);

    if ((other_levelWidths[level] < 1) ||
        (other_levelWidths[level] > DS_Max_Width))
    {
      printf ("DS_AlgoMergeTrees : Number of nodes (%d) in a level of the tree "
              "must range 1...%1d\n", other_levelWidths[level], DS_Max_Width);
      return (-1);
    }

    strsub(other_levelNames[level], '_', ' ');

    other_numNodes += other_levelWidths[level];


    for (node = 0; node < other_levelWidths[level]; node++)
    {
      /* Read node name, and B and T thresholds. */

      if (getNextLine(Hold, att))
        return (-1);

      sscanf(Hold, "%s %s %lf %lf",
             other_nodeNames[n], other_nodeStrings[n],
             &other_bThresh[n], &other_tThresh[n]);

      strsub(other_nodeNames[n], '_', ' ');
      strsub(other_nodeStrings[n], '_', ' ');
      n++;
    }
  }


  /* Read in the parent-to-child link matrices. */

  if (getNextLine(Hold, att))
    return (-1);

  if ((Hold[0] == 'T') || (Hold[0] == 't'))
    BandDImpactsDiffer = TRUE;
  else
    BandDImpactsDiffer = FALSE;

  for (bd = B; bd <= D; bd++)
  {
    for (chNode = 0; chNode < other_numNodes; chNode++)
    {
      if ((bd == B) || BandDImpactsDiffer)
      {
        if (getNextLine(Hold, att))
          return (-1);

        if (strncmp(Hold, " ----", 5) == 0)
        {
          if (getNextLine(Hold, att))
            return (-1);
        }

        token = strtok(Hold, " |");

        for (parNode = 0; parNode < other_numNodes; parNode++)
        {
          sscanf (token, "%lf", &other_links[bd][chNode][parNode]);
          token = strtok(NULL, " |");
        }
      }
      else
      {
        for (parNode = 0; parNode < other_numNodes; parNode++)
        {
          other_links[D][chNode][parNode] = other_links[B][chNode][parNode];
        }
      }
    }
  }

  fclose (att);


  // Read in the data of the merging-tree.

  if (savedNet_readFile(dataFile) != 0)
  {
    return (-1);
  }

 
  if ((mergeFunction == DS_Union) && (other_treeDepth > DS_treeDepth))
  {




    //   NOTE: This code ASSUMES merging tree has extra levels 
    //                  ONLY AT END OF TREE!!!




    for (level = DS_treeDepth; level < other_treeDepth; level++)
      strcpy(DS_levelNames[level], other_levelNames[level]);

    DS_treeDepth = other_treeDepth;
  }


  // Merge nodes.

  if (mergeFunction == DS_Union)
  {
    other_index = 0;

    for (level = 0; level < other_treeDepth; level++)
    {
      for (node = 0; node < other_levelWidths[level]; node++, other_index++)
      {

        n = DS_AddNode (DS_levelWidths[level] + 1, level,
                        other_nodeNames[other_index],
                        other_nodeStrings[other_index],
                        other_bThresh[other_index],
                        other_tThresh[other_index]);

        // If DS_AddNode returns -2, the node is already in the tree.
        // Set the node's value to the greater of the DS_ and other_
        // values.  Set the node's threshold values to the average of
        // the DS_ and other_ values.

        if (n == -2)
        {
          DS_index = DS_AlgoGetNodeIndex (other_nodeNames[other_index]);

          if (DS_nodeValues[B][DS_index] < savedNet_nodes[level][node].belief)
            DS_nodeValues[B][DS_index] = savedNet_nodes[level][node].belief;

          DS_bThresh[DS_index] =
            (DS_bThresh[DS_index] + other_bThresh[other_index]) / 2.0;
          DS_tThresh[DS_index] =
            (DS_tThresh[DS_index] + other_tThresh[other_index]) / 2.0;

          DS_mergeTreeNodeIsFrom[DS_index] = DS_Both_Trees;
        }

        // Else if DS_AddNode returns -1 here, the level is
        // already full and the node couldn't be added.  If
        // the node with the least B value on the level of the
        // combined tree is less than the other_ node's B value,
        // remove that node and reattempt to add this other_ node.

        else if (n == -1)
        {
          DS_index = DS_CoordToIndex(1, level);
          int  min_index = DS_index - 1;
          int  min_node = 0;
          for (n = 1; n < DS_levelWidths[level]; n++, DS_index++)
          {
            if (DS_nodeValues[B][DS_index] < DS_nodeValues[B][min_index])
            {
              min_index = DS_index;
              min_node = n;
            }
          }

          if (DS_nodeValues[B][min_index] < savedNet_nodes[level][node].belief)
          {
            DS_DeleteNode (min_node, level);
            if (DS_AddNode (DS_levelWidths[level] + 1, level,
                            other_nodeNames[other_index],
                            other_nodeStrings[other_index],
                            other_bThresh[other_index],
                            other_tThresh[other_index]) == 0)
            {
              DS_index = DS_AlgoGetNodeIndex (other_nodeNames[other_index]);
              DS_mergeTreeNodeIsFrom[DS_index] = DS_Saved_Tree;
            }
          }

          DS_numNodesLost[level]++;
        }

        //   Else DS_AddNode returned 0, meaning the other_ node was added
        //   successfully.  Indicate that the node came from the saved tree.

        else
        {
          DS_index = DS_AlgoGetNodeIndex (other_nodeNames[other_index]);
          DS_mergeTreeNodeIsFrom[DS_index] = DS_Saved_Tree;
        }
      } // for node
    } // for level
  } // if Union


  if (mergeFunction == DS_Intersection)
  {
    // Remove nodes not in merging tree.

    for (DS_index = 0; DS_index < DS_numNodes;)
    {
      nodeFound = FALSE;

      // If this DS_ node is not in the other_ tree, delete it from DS_ tree.

      for (other_index = 0; other_index < other_numNodes; other_index++)
      {
        if (strcasecmp(DS_nodeNames[DS_index],
                       other_nodeNames[other_index]) == 0)
        {
          nodeFound = TRUE;
          break;
        }
      }

      if (nodeFound)
      {
        DS_index++;
      }
      else
      {
        if (DS_IndexToCoord(DS_index, DS_node, DS_level) == 0)
          DS_DeleteNode (DS_node, DS_level);
      }
    }
  }


  // Merge links.

  for (other_index = 0; other_index < other_numNodes; other_index++)
  {
    // If node is in the DS_ tree, add new other_ links
    // and average the weights of existing ones.

    DS_index = DS_AlgoGetNodeIndex (other_nodeNames[other_index]);

    if (DS_index != -1)
    {
      for (chIndex = 0; chIndex < other_numNodes; chIndex++)
      {
        DS_chIndex = -1;

        for (bd = B; bd <= D; bd++)
        {
          if (other_links[bd][chIndex][other_index] != 0.0)
          {
            if ((bd == B) ||
                ((bd == D) && (other_links[B][chIndex][other_index] == 0.0)))
              DS_chIndex = DS_AlgoGetNodeIndex (other_nodeNames[chIndex]);

            if (DS_chIndex != -1)
            {
              // Average the weights of the DS_ and other_ links.

              if (DS_links[bd][DS_chIndex][DS_index])
              {
                DS_links[bd][DS_chIndex][DS_index] =
                  (DS_links[bd][DS_chIndex][DS_index] +
                   other_links[bd][chIndex][other_index]) / 2.0;
              }
              else
              {
                DS_links[bd][DS_chIndex][DS_index] =
                   other_links[bd][chIndex][other_index] / 2.0;
              }
            }
          }
        }
      }
    }
  }


  // Halve DS_ links not in other_ tree.

  for (DS_index = 0; DS_index < DS_numNodes; DS_index++)
  {
    // Find the parent node in other_ tree.

    for (other_index = 0; other_index < other_numNodes; other_index++)
    {
      if (strcasecmp(DS_nodeNames[DS_index], other_nodeNames[other_index]) == 0)
      {
        break;
      }
    }

    for (DS_chIndex = 0; DS_chIndex < DS_numNodes; DS_chIndex++)
    {
      for (bd = B; bd <= D; bd++)
      {
        if (DS_links[bd][DS_chIndex][DS_index])
        {
          if ((bd == B) ||
              ((bd == D) && (DS_links[B][DS_chIndex][DS_index] == 0.0)))
          {
            for (chIndex = 0; chIndex < other_numNodes; chIndex++)
            {
              if (strcasecmp(DS_nodeNames[DS_chIndex],
                             other_nodeNames[chIndex]) == 0)
              {
                break;
              }
            }
          }

          if (!other_links[bd][chIndex][other_index])
          {
            DS_links[bd][DS_chIndex][DS_index] /= 2.0;
          }
        }
      }
    }
  }


  for (level = 0; level < DS_treeDepth; level++)
  {
    if (DS_levelWidths[level] < 1)
    {
      fprintf (stderr, "Warning -- Combined tree contains one or more levels"
               " with 0 nodes!\n");
      return -2;
    }
  }

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoGetMergeNodesOriginalTree (int node, int level)
{
  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_AlgoGetMergeNodesOriginalTree : Node level %d "
            "is out of bounds.\n", level);
    return (-1);
  }

  if ((node < 0) || (node >= DS_levelWidths[level]))
  {
    printf ("DS_AlgoGetMergeNodesOriginalTree : Node index %d"
            " is out of bounds.\n", node);
    return (-1);
  }

  return DS_mergeTreeNodeIsFrom[DS_CoordToIndex (node, level)];
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoGetNumMergeNodesLost (int level)
{
  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_AlgoGetNumMergeNodesLost : Node level %d is out of bounds.\n",
            level);
    return (-1);
  }

  return DS_numNodesLost[level];
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

void DS_AlgoReverseLinks (void)
{
  double  temp;

  
  for (int bd = B; bd <= D; bd++)
  {
    for (int chNode = 0; chNode < DS_numNodes; chNode++)
    {
      for (int parNode = chNode + 1; parNode < DS_numNodes; parNode++)
      {
        temp = DS_links[bd][chNode][parNode];
        DS_links[bd][chNode][parNode] = DS_links[bd][parNode][chNode];
        DS_links[bd][parNode][chNode] = temp;
      }
    }
  }
}

