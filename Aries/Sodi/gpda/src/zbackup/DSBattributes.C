
//handle NULL data in cols
//handle missing data (eg. 3 pars but one name)

//////////////////call SQLFreeStmt(SQL_UNBIND)
//////////////////instead of freeing handles and reallocating them

// USE SQLGetData TO GET VARIABLE LENGTH DATA IN PARTS

///////////////// NEED TO APPEND DATABASE EXT ------------
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

// #include <sqltypes.h>
// #include <sqlext.h>
// #include <sql.h>
#include "isql.h"

#include "DSBattributes.h"
#include "Globals.h"
#include "savedNet.h"


const int  B = 0;
const int  D = 1;
const int  U = 2;
const int  DB_NAME_LEN = 255;
const int  MAX_LINE_LEN = 1500;

const char db_DSN[] = "MySQL";
const char db_UID[] = "root";
const char db_PWD[] = "gpda";

const int db_numColumns = 12;
const char db_colNames[db_numColumns][DB_NAME_LEN] =
{
  "LEVEL_LABELS",
  "LINK_LABELS",
  "PARENTS",
  "B_WEIGHTS",
  "D_WEIGHTS",
  "BELIEF",
  "DISBELIEF",
  "HYPOTHESIS",
  "LEVEL",
  "PHRASE",
  "THRESHOLD",
  "CUTOFF"
};

const int   LEVEL_LABELS = 0;
const int   LINK_LABELS  = 1;
const int   PARENTS      = 2;
const int   B_WEIGHTS    = 3;
const int   D_WEIGHTS    = 4;
const int   BELIEF       = 5;
const int   DISBELIEF    = 6;
const int   HYPOTHESIS   = 7;
const int   LEVEL        = 8;
const int   PHRASE       = 9;
const int   THRESHOLD    = 10;
const int   CUTOFF       = 11;


extern int       bHTMLTable;
extern int       cDelimiter;
extern int       bColumnNames;
extern SQLHSTMT  hStmt;


char    DS_missionName[DS_Max_String_Length];
char    DS_storyName[DS_Max_String_Length];
char    DS_levelNames[DS_Max_Depth][DS_Max_String_Length];
char    DS_nodeNames[DS_Max_Nodes][DS_Max_String_Length];
char    DS_nodeStrings[DS_Max_Nodes][DS_Max_String_Length];
char    DS_linkNames[DS_Max_Nodes][DS_Max_Nodes][DS_Max_String_Length];

int     DS_verbose;
int     DS_treeDepth;
int     DS_numNodes;
int     DS_levelWidths[DS_Max_Depth];
int     DS_mergeTreeNodeIsFrom[DS_Max_Nodes];
int     DS_numNodesLost[DS_Max_Depth];

double  DS_bThresh[DS_Max_Nodes];
double  DS_tThresh[DS_Max_Nodes];
double  DS_nodeValues[3][DS_Max_Nodes];
double  DS_links[2][DS_Max_Nodes][DS_Max_Nodes];


int  DS_GetNextLine (char*, FILE*);
int  DS_ReadFromFlatFile (char*, char*);
int  DS_ReadFromDB (char*, char*);
int  DS_SaveToFlatFile (char*, char*);
int  DS_SaveToDB (char*, char*);
/***
int  DS_OpenDatabase(SQLHENV*, SQLHDBC*);
void DS_CloseDatabase(SQLHENV henv, SQLHDBC hdbc);
int  DS_ExecuteSQL(SQLHDBC, char*, SQLHSTMT*);
***/
int  DS_SearchSQL(SQLHDBC, char*, char*);

//------------------------------------------------------------------------------

/*** FOR WHEN THIS CODE IS CONVERTED TO A CLASS
DSattributes::DSattributes()
{
printf("IN DSattributes()\n");
fflush(stdout);

  for (int i = 0; i < DS_Max_Nodes; i++)
    extNetInfo[i] = NULL;

  DS_InitAttributes(NULL, NULL, 0, 0)
}

//------------------------------------------------------------------------------

DSattributes::DSattributes(const DSattributes& att)
{
  int  i, j;

printf("IN DSattributes(DSattributes)\n");
fflush(stdout);


  DS_verbose = att.DS_verbose;
  DS_treeDepth = att.DS_treeDepth;
  DS_numNodes = att.DS_numNodes;
  strcpy(DS_missionName, att.DS_missionName);
  strcpy(DS_storyName, att.DS_storyName);

  for (i = 0; i < DS_treeDepth; i++)
  {
    strcpy(DS_levelNames[i], att.DS_levelNames[i]);
    DS_levelWidths[i] = att.DS_levelWidths[i];
    DS_numNodesLost[i] = att.DS_numNodesLost[i];
  }

  for (i = 0; i < DS_numNodes; i++)
  {
    strcpy(DS_nodeNames[i], att.DS_nodeNames[i]);
    strcpy(DS_nodeStrings[i], att.DS_nodeStrings[i]);
    strcpy(explainFile[i], att.explainFile[i]);
    DS_mergeTreeNodeIsFrom[i] = att.DS_mergeTreeNodeIsFrom[i];
    DS_bThresh[i] = att.DS_bThresh[i];
    DS_tThresh[i] = att.DS_tThresh[i];

    if (att.extNetInfo[i] != NULL)
    {
printf("\ncpyCons -- Newing extNetInfo[%d] --\n\n", i);
fflush(stdout);
      extNetInfo[i] = new ExtNetInfoType;
      strcpy(extNetInfo[i].hypothesis, att.extNetInfo[i].hypothesis);
      strcpy(extNetInfo[i].mission, att.extNetInfo[i].mission);
    }
    else
    {
      extNetInfo[i] = NULL;
    }

    for (bd = B; bd <= U; bd++)
      DS_nodeValues[bd][i] = att.DS_nodeValues[bd][i];

    for (j = 0; j < DS_numNodes; j++)
    {
      strcpy(DS_linkNames[i][j], att.DS_linkNames[i][j]);

      for (bd = B; bd <= D; bd++)
        DS_links[bd][i][j] = att.DS_links[bd][i][j];
    }
  }
}

//------------------------------------------------------------------------------

DSattributes::~DSattributes()
{
  for (int i = 0; i < DS_numNodes; i++)
    if (extNetInfo[i] != NULL)
      delete extNetInfo[i];
}

//------------------------------------------------------------------------------

DSattributes::operator = (const DSattributes &att)
{
  int  i, j;

printf("IN =(DSattributes)\n");
fflush(stdout);

  DS_verbose = att.DS_verbose;
  DS_treeDepth = att.DS_treeDepth;
  DS_numNodes = att.DS_numNodes;
  strcpy(DS_missionName, att.DS_missionName);
  strcpy(DS_storyName, att.DS_storyName);

  for (i = 0; i < DS_treeDepth; i++)
  {
    strcpy(DS_levelNames[i], att.DS_levelNames[i]);
    DS_levelWidths[i] = att.DS_levelWidths[i];
    DS_numNodesLost[i] = att.DS_numNodesLost[i];
  }

  for (i = 0; i < DS_numNodes; i++)
  {
    strcpy(DS_nodeNames[i], att.DS_nodeNames[i]);
    strcpy(DS_nodeStrings[i], att.DS_nodeStrings[i]);
    strcpy(explainFile[i], att.explainFile[i]);
    DS_mergeTreeNodeIsFrom[i] = att.DS_mergeTreeNodeIsFrom[i];
    DS_bThresh[i] = att.DS_bThresh[i];
    DS_tThresh[i] = att.DS_tThresh[i];

    if (extNetInfo[i] != NULL)
{
printf("\n= -- Deleting extNetInfo[%d] --\n\n", i);
fflush(stdout);
      delete extNetInfo[i];
}

    if (att.extNetInfo[i] != NULL)
    {
printf("\n= -- Newing extNetInfo[%d] --\n\n", i);
fflush(stdout);
      extNetInfo[i] = new ExtNetInfoType;
      strcpy(extNetInfo[i].hypothesis, att.extNetInfo[i].hypothesis);
      strcpy(extNetInfo[i].mission, att.extNetInfo[i].mission);
    }
    else
    {
      extNetInfo[i] = NULL;
    }

    for (bd = B; bd <= U; bd++)
      DS_nodeValues[bd][i] = att.DS_nodeValues[bd][i];

    for (j = 0; j < DS_numNodes; j++)
    {
      strcpy(DS_linkNames[i][j], att.DS_linkNames[i][j]);

      for (bd = B; bd <= D; bd++)
        DS_links[bd][i][j] = att.DS_links[bd][i][j];
    }
  }

  return *this;
}
***/

//------------------------------------------------------------------------------

void DS_PrintAttributes(void)
{
  int  level, node;


  printf("\n(%s) Info:\n", DS_missionName);
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
} // DS_PrintAttributes

//------------------------------------------------------------------------------

int DS_GetNextLine(char *lineOfText, FILE *fp)
{
  do
  {
    if (fgets (lineOfText, MAX_LINE_LEN, fp) == NULL)
    {
      fprintf (stderr, "DS_GetNextLine : Error reading file.\n");
      fflush(stderr);
      return (-1);
    }
  }
  while ((lineOfText[0] == '#') || (lineOfText[0] == '\n'));

  return 0;
}

//------------------------------------------------------------------------------

int DS_InitAttributes(char *missionName, char *storyName,
                      int verbose, int readFromDB)
{
  int    successVal = 0;
  int    level, i, parNode, chNode;


  DS_verbose = verbose;

  if (DS_verbose > 1)
  {
    printf("IN DS_InitAttributes\n");
    fflush(stdout);
  }


  // Initialize variables.

  DS_treeDepth = 0;
  DS_numNodes = 0;

  for (chNode = 0; chNode < DS_Max_Nodes; chNode++)
  {
    for (parNode = 0; parNode < DS_Max_Nodes; parNode++)
    {
      DS_links[B][chNode][parNode] = DS_links[D][chNode][parNode] = 0;
      DS_linkNames[chNode][parNode][0] = '?';
      DS_linkNames[chNode][parNode][1] = '\0';
    }
  }

  sprintf(DS_missionName, "?");

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



  if (missionName != NULL)
  {
    if (readFromDB)
      successVal = DS_ReadFromDB (missionName, storyName);
    else
      successVal = DS_ReadFromFlatFile (missionName, storyName);
  }

  return successVal;
} // DS_InitAttributes

//------------------------------------------------------------------------------

int DS_ReadFromFlatFile (char *missionName, char *storyName)
{
  FILE   *att;
  char   *token;
  char   fname[DS_Max_String_Length];
  char   hold[MAX_LINE_LEN];
  short  BandDImpactsDiffer = TRUE;
  int    bd, level, i, parNode, chNode, node;


/*** Don't build filename for now, as filename is passed via missionName
  if (storyName == NULL)
  {
    printf ("DS_AlgoInitAttributes : Story name is NULL.\n");
    fflush(stdout);
    return (-1);
  }

  if (strlen(storyName) > 0)
    sprintf(fname, "%s_%s.dsbn", missionName, storyName);
  else
    sprintf(fname, "%s.dsbn", missionName);
***/
  strcpy(fname, missionName);

  if (DS_verbose > 1)
  {
    printf("OPEN (%s)...\n", fname);
    fflush(stdout);
  }

  if ((att = fopen (fname, "r")) == NULL)
  {
    fprintf (stderr, "DS_InitAttributes: Cannot open (%s).\n", fname);
    return (-1);
  }


  // Read in general tree info.

  if (DS_verbose > 1)
  {
    printf("READ first line...\n");
    fflush(stdout);
  }

  if (DS_GetNextLine(hold, att))
    return (-1);

  if (DS_verbose > 1)
  {
    printf("PARSE first line...\n");
    fflush(stdout);
  }

  sscanf(hold, "%d %s", &DS_treeDepth, DS_missionName);

  if ((DS_treeDepth < 1) || (DS_treeDepth > DS_Max_Depth))
  {
    printf ("Number of levels in tree must range 1...%1d\n", DS_Max_Depth);
    fflush(stdout);
    return (-1);
  }

  strsub(DS_missionName, '_', ' ');


  // Read in the number of nodes that are at each level of tree.

  DS_numNodes = 0;
  i = 0;

  for (level = 0; level < DS_treeDepth; level++)
  {
    // Read # nodes and level name.

    if (DS_verbose > 1)
    {
      printf("READ level (%d) line...\n", level);
      fflush(stdout);
    }

    if (DS_GetNextLine(hold, att))
      return (-1);

    if (DS_verbose > 1)
    {
      printf("PARSE line...\n");
      fflush(stdout);
    }

    sscanf (hold, "%d %s", &DS_levelWidths[level], DS_levelNames[level]);

    if ((DS_levelWidths[level] < 1) || (DS_levelWidths[level] > DS_Max_Width))
    {
      printf ("Number of nodes in a level of the tree must range 1...%1d\n",
              DS_Max_Width);
      fflush(stdout);
      return (-1);
    }

    strsub(DS_levelNames[level], '_', ' ');

    DS_numNodes += DS_levelWidths[level];


    for (node = 0; node < DS_levelWidths[level]; node++)
    {
      // Read node name, node description and B and T thresholds.

      if (DS_verbose > 1)
      {
        printf("READ node (%d) line...\n", i);
        fflush(stdout);
      }

      if (DS_GetNextLine(hold, att))
        return (-1);

      if (DS_verbose > 1)
      {
        printf("PARSE line...\n");
        fflush(stdout);
      }

      sscanf(hold, "%s %s %lf %lf",
             DS_nodeNames[i], DS_nodeStrings[i],
             &DS_bThresh[i], &DS_tThresh[i]);

      strsub(DS_nodeNames[i], '_', ' ');
      strsub(DS_nodeStrings[i], '_', ' ');
      i++;
    }
  }


  // Read in the parent-to-child link matrices.

  if (DS_verbose > 1)
  {
    printf("READ T/F line...\n");
    fflush(stdout);
  }

  if (DS_GetNextLine(hold, att))
    return (-1);

  if (DS_verbose > 1)
  {
    printf("PARSE line...\n");
    fflush(stdout);
  }

  if ((hold[0] == 'T') || (hold[0] == 't'))
    BandDImpactsDiffer = TRUE;
  else
    BandDImpactsDiffer = FALSE;

  for (bd = B; bd <= D; bd++)
  {
    for (chNode = 0; chNode < DS_numNodes; chNode++)
    {
      if ((bd == B) || BandDImpactsDiffer)
      {
        if (DS_verbose > 1)
        {
          printf("READ linkWts (%d) line...\n", chNode);
          fflush(stdout);
        }

        if (DS_GetNextLine(hold, att))
          return (-1);

        if (strncmp(hold, " ----", 5) == 0)
        {
          if (DS_GetNextLine(hold, att))
            return (-1);
        }

        if (DS_verbose > 1)
        {
          printf("  (%s)\n", hold);
          printf("PARSE line...\n");
          fflush(stdout);
        }

        token = strtok(hold, " |");

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

  fclose (att);



  if (DS_verbose > 1)
  {
    printf("print input...\n");
    fflush(stdout);
  }

  if (DS_verbose)
  {
    printf("tree (%s)\n", DS_missionName);

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
  }

  fflush(stdin);

  return(0);
}  // DS_ReadFromFlatFile

//------------------------------------------------------------------------------

int DS_ReadFromDB (char *missionName, char *storyName)
{
  int        i;
  int        node;
  int        parNode;
  int        level;
  int        parLevel;
  int        nCols;
  int        successVal = 0;
  int        found = FALSE;

  char       str[1024];
  char       str1[1024];
  char       str2[1024];
  char       str3[1024];
  char       nodeName[DB_NAME_LEN];
  char       colValues[db_numColumns][DB_NAME_LEN];
  char       *name;
  char       *nameEnd;
  char       *bWeightStr;
  char       *dWeightStr;
  char       *bWeightEnd;
  char       *dWeightEnd;
  char       *label;
  char       *labelEnd;

  double     bWeight;
  double     dWeight;
  double     belief;
  double     disbelief;
  double     Bthresh;
  double     Tthresh;

  SQLHENV    henv;
  SQLHDBC    hdbc;
  SQLRETURN  ret;

  SDWORD     colLengths[db_numColumns];


  if (storyName == NULL)
  {
    printf ("DS_AlgoInitAttributes : Story name is NULL.\n");
    fflush(stdout);
    return (-1);
  }


  //if (DS_OpenDatabase(&henv, &hdbc) != 0)
  if (OpenDatabase(&henv, &hdbc, (char*)db_DSN, (char*)db_UID, (char*)db_PWD)
      == 0)
    return -1;


  // See if the database we want to use exists.

  found = DS_SearchSQL(hdbc, "DATABASES", missionName);

  if (!found)
  {
    printf("readFromDB : No (%s) database found.\n", missionName);
    fflush(stdout);
    return -1;
  }


  // Database found, select it for use.

  sprintf(str, "USE `%s`;", missionName);
  //ret = DS_ExecuteSQL(hdbc, str, &hStmt);
  ret = ExecuteSQL(hdbc, str, cDelimiter, bColumnNames, bHTMLTable, nCols);

  if (ret != 0)
    return -1;

  SQLFreeStmt(hStmt, SQL_DROP);


  // See if the table we want to use exists.

  found = DS_SearchSQL(hdbc, "TABLES", storyName);

  if (!found)
  {
    printf("readFromDB : No (%s) table found in database (%s).\n",
           storyName, missionName);
    fflush(stdout);
    return -1;
  }


  DS_AlgoSetTreeNames (missionName, storyName);


  // Select for retrieval all the data from the structure table.
  sprintf(str, "SELECT * FROM `%s`;", storyName);
  //ret = DS_ExecuteSQL(hdbc, str, &hStmt);
  ret = ExecuteSQL(hdbc, str, cDelimiter, bColumnNames, bHTMLTable, nCols);

  if (ret != 0)
    return -1;

  // Bind each column to a variable.
  for (i = 0; i < db_numColumns; i++)
  {
    // NOTE -- Start at column 0 if use bookmarks.
    ret = SQLBindCol(hStmt,  i + 1, SQL_C_CHAR, colValues[i],
                     DB_NAME_LEN, &colLengths[i]);
  }

  if (DS_verbose)
  {
    printf("DB (%s)  Table (%s)  column data:\n", missionName, storyName);
    fflush(stdout);
  }

  while (TRUE)
  {
    ret = SQLFetch(hStmt);

//jjj
    if (ret == SQL_SUCCESS || ret == SQL_SUCCESS_WITH_INFO)
    {
      if (DS_verbose)
      {
        for (i = 0; i < db_numColumns; i++)
        {
          printf(" - (%s)\n", colValues[i]);
          fflush(stdout);
        }

        printf("\n");
        fflush(stdout);
      }
    }
    else
    {
      break;
    }


    // Get level labels from first record.

    if (strlen(colValues[LEVEL_LABELS]) == 0)
    {
      break;
    }


    if (DS_treeDepth == 0)
    {
      strcpy(str, colValues[LEVEL_LABELS]);
      label = str;

      do
      {
        labelEnd = strchr(label, ',');

        if (labelEnd)
          *labelEnd = '\0';

        DS_treeDepth++;
        DS_AlgoSetLevelName(DS_treeDepth - 1, label);

        label = labelEnd + 1;
      } while (labelEnd != NULL);
    }

    if (DS_treeDepth == 0)
      break;


    // Set node data.

    strcpy(str, colValues[THRESHOLD]);
    sscanf(str, "%lf", &Bthresh);

    strcpy(str, colValues[CUTOFF]);
    sscanf(str, "%lf", &Tthresh);

    strcpy(str, colValues[LEVEL]);
    level = DS_AlgoGetLevelIndex(str);

    strcpy(nodeName, colValues[HYPOTHESIS]);
    strcpy(str, colValues[PHRASE]);

    if (DS_AddNode(DS_levelWidths[level], level, nodeName, str,
                    Bthresh, Tthresh) != 0)
    {
      printf ("readFromDB : Cannot add node (%s).\n", nodeName);
      fflush(stdout);
      successVal = -1;
      continue;
    }

    strcpy(str, colValues[BELIEF]);
    sscanf(str, "%lf", &belief);

    strcpy(str, colValues[DISBELIEF]);
    sscanf(str, "%lf", &disbelief);

    DS_AlgoSetNodeValues(DS_levelWidths[level] - 1, level, belief, disbelief);
  }

  SQLFreeHandle(SQL_HANDLE_STMT, hStmt);


  // Get link data.

  sprintf(str, "SELECT * FROM `%s`;", storyName);
  //ret = DS_ExecuteSQL(hdbc, str, &hStmt);
  ret = ExecuteSQL(hdbc, str, cDelimiter, bColumnNames, bHTMLTable, nCols);

  if (ret != 0)
    return -1;

  for (i = 0; i < db_numColumns; i++)
  {
    ret = SQLBindCol(hStmt,  i + 1, SQL_C_CHAR, colValues[i],
                     DB_NAME_LEN, &colLengths[i]);
  }

  while (TRUE)
  {
    ret = SQLFetch(hStmt);

    if (ret == SQL_SUCCESS || ret == SQL_SUCCESS_WITH_INFO)
    {
      if (DS_verbose)
      {
        for (i = 0; i < db_numColumns; i++)
        {
          printf(" - (%s)\n", colValues[i]);
          fflush(stdout);
        }

        printf("\n");
        fflush(stdout);
      }
    }
    else
    {
      if (DS_verbose)
      {
        printf("Fetch failed, quitting getting data from db.\n");
        fflush(stdout);
      }
      break;
    }


    strcpy(str, colValues[PARENTS]);

    if (strlen(str) > 0)
    {
      strcpy(str1, colValues[B_WEIGHTS]);
      strcpy(str2, colValues[D_WEIGHTS]);
      strcpy(str3, colValues[LINK_LABELS]);


      // Get the coordinates of the current node.

      strcpy(nodeName, colValues[HYPOTHESIS]);

      if (DS_AlgoGetNodeCoordinates(nodeName, node, level) != 0)
      {
        printf ("readFromDB : Cannot find node (%s) in tree.\n",str);
        fflush(stdout);
        successVal = -1;
        continue;
      }


      // Loop thru the node's list of parents.

      name = str;
      bWeightStr = str1;
      dWeightStr = str2;
      label = str3;

      do
      {
        nameEnd = strchr(name, ',');

        if (nameEnd)
          *nameEnd = '\0';

        if (DS_AlgoGetNodeCoordinates(name, parNode, parLevel))
        {
          printf ("readFromDB : Cannot find node (%s) in tree.\n",
                  name);
          fflush(stdout);
          successVal = -1;
          name = nameEnd + 1;
          continue;
        }


        if (strlen(bWeightStr) > 0)
        {
          bWeightEnd = strchr(bWeightStr, ',');

          if (bWeightEnd)
            *bWeightEnd = '\0';

          sscanf(bWeightStr, "%lf", &bWeight);
        }
        else
        {
          bWeight = 0.0;
        }


        if (strlen(dWeightStr) > 0)
        {
          dWeightEnd = strchr(dWeightStr, ',');

          if (dWeightEnd)
            *dWeightEnd = '\0';

          sscanf(dWeightStr, "%lf", &dWeight);
        }
        else
        {
          dWeight = 0.0;
        }


        if (strlen(label) > 0)
        {
          labelEnd = strchr(label, ',');

          if (labelEnd)
            *labelEnd = '\0';
        }
        else
        {
          strcpy(label, "?");
        }


        if (DS_AlgoSetNodeImpact(parNode, parLevel, node, level,
                                 bWeight, dWeight))
        {
          printf ("readFromDB : Cannot set link weight from "
                  "(%s) to (%s).\n", name, nodeName);
          fflush(stdout);
          successVal = -1;
        }


        name = nameEnd + 1;

        if (bWeightEnd)
          bWeightStr = bWeightEnd + 1;

        if (dWeightEnd)
          dWeightStr = dWeightEnd + 1;

        if (labelEnd)
          label = labelEnd + 1;
      } while (nameEnd != NULL);
    }
  }


  // Free statement handle.
  SQLFreeHandle(SQL_HANDLE_STMT, hStmt);

  //DS_CloseDatabase(henv, hdbc);
  CloseDatabase(henv, hdbc);

  return successVal;
}  // DS_ReadFromDB

//------------------------------------------------------------------------------

int DS_AlgoSaveAttributes (char *missionName, char *storyName, int writeToDB)
{
  int  successVal = 0;


  if (missionName == NULL)
  {
    printf ("DS_AlgoSaveAttributes : Mission name is NULL.\n");
    fflush(stdout);
    return (-1);
  }

  if (writeToDB)
    successVal = DS_SaveToDB (missionName, storyName);
  else
    successVal = DS_SaveToFlatFile (missionName, storyName);

  return successVal;
}

//------------------------------------------------------------------------------

int DS_SaveToFlatFile (char *missionName, char *storyName)
{
  FILE   *att;                             // Attribute file pointer
  char   fname[DS_Max_String_Length];
  char   str[DS_Max_String_Length];        // Character string
  char   sep;                              // Column separater
  char   sepLine[DS_Max_String_Length];    // Line separater
  char   blankLine[DS_Max_String_Length];  // Blank line
  short  BandDImpactsDiffer = TRUE;        // TRUE if B and D impact mats differ
  int    i, l, n, level, node, chNode, parNode;  // Loop counters
  int    numMatrices;                  // 1 if B and D impact mats same, else 2
  int    numNameChars;
  int    numStringChars;


/*** Don't build filename for now, as filename is passed via missionName
  if (storyName == NULL)
  {
    printf ("DS_AlgoSaveAttributes : Story name is NULL.\n");
    fflush(stdout);
    return (-1);
  }

  if (strlen(storyName) > 0)
    sprintf(fname, "%s_%s.dsbn", missionName, storyName);
  else
    sprintf(fname, "%s.dsbn", missionName);
***/
  strcpy(fname, missionName);

  if ((att = fopen (fname, "w")) == NULL)
  {
    printf ("DS_AlgoSaveAttributes : Cannot open (%s).\n", fname);
    fflush(stdout);
    return (-1);
  }


  // Write the general tree information.

  strcpy(str, DS_missionName);
  strsub(str, ' ', '_');

  fprintf (att, "# Tree height, Name\n");
  fprintf (att, "%d  %s\n\n", DS_treeDepth, str);


  // Write the width of the tree at each level, and the level name.
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
      // Write node name, node description, and B and T thresholds.

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


  // Determine whether or not belief and disbelief impact
  // matrices are the same, write the answer to file.

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


  // Create a blank line and separator line to improve readability.

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


  // Write the impact matrices.

  for (i = 0; i < numMatrices; i++)
  {
    // Label the columns.

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


    // Write out the matrix values.

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

  fclose (att);

  return 0;
} // DS_SaveToFlatFile

//------------------------------------------------------------------------------

int DS_SaveToDB (char *missionName, char *storyName)
{
  int        i;
  int        index;
  int        level;
  int        nCols;
  int        successVal = 0;
  char       str[1024];
  char       colValues[db_numColumns][255];

  SQLHENV    henv;
  SQLHDBC    hdbc;
  //SQLHSTMT   hstmt;
  SQLRETURN  ret;

  SDWORD     colLengths[db_numColumns];


  if (storyName == NULL)
  {
    printf ("DS_AlgoSaveAttributes : Story name is NULL.\n");
    fflush(stdout);
    return (-1);
  }

  if (strlen(storyName) == 0)
  {
    printf ("DS_AlgoSaveAttributes : Story name is not provided.\n");
    fflush(stdout);
    return (-1);
  }


  //if (DS_OpenDatabase(&henv, &hdbc) != 0)
  if (OpenDatabase(&henv, &hdbc, (char*)db_DSN, (char*)db_UID, (char*)db_PWD)
      == 0)
    return -1;


  // If the desired database does not exist, create it.

  sprintf(str, "CREATE DATABASE IF NOT EXISTS `%s`;", missionName);
  //ret = DS_ExecuteSQL(hdbc, str, &hstmt);
  ret = ExecuteSQL(hdbc, str, cDelimiter, bColumnNames, bHTMLTable, nCols);

  if (ret != 0)
    return -1;

  SQLFreeStmt(hStmt, SQL_DROP);


  // Select the database for use.

  sprintf(str, "USE `%s`;", missionName);
  //ret = DS_ExecuteSQL(hdbc, str, &hstmt);
  ret = ExecuteSQL(hdbc, str, cDelimiter, bColumnNames, bHTMLTable, nCols);

  if (ret != 0)
    return -1;

  SQLFreeStmt(hStmt, SQL_DROP);


  // If structure table already exists then delete it.

  sprintf(str,"DROP TABLE IF EXISTS `%s`;", storyName);
  //ret = DS_ExecuteSQL(hdbc, str, &hstmt);
  ret = ExecuteSQL(hdbc, str, cDelimiter, bColumnNames, bHTMLTable, nCols);

  if (ret != 0)
    return -1;

  SQLFreeStmt(hStmt, SQL_DROP);


  // Create the new story table.

  sprintf(str,"CREATE TABLE `%s` (", storyName);

  for (i = 0; i < db_numColumns; i++)
  {
    strcat(str, db_colNames[i]);
// max len is 255; otherwise use blob
    strcat(str, " varchar (255)");

    if (i < db_numColumns - 1)
      strcat(str, ",");
    else
      strcat(str, ");");
  }

  //ret = DS_ExecuteSQL(hdbc, str, &hstmt);
  ret = ExecuteSQL(hdbc, str, cDelimiter, bColumnNames, bHTMLTable, nCols);

  if (ret != 0)
    return -1;

  SQLFreeStmt(hStmt, SQL_DROP);


  // Insert a row of data for each node in the tree.

  colValues[LEVEL_LABELS][0] = '\0';

  for (i = 0; i < DS_treeDepth; i++)
  {
    strcat(colValues[LEVEL_LABELS], DS_levelNames[i]);

    if (i < DS_treeDepth - 1)
      strcat(colValues[LEVEL_LABELS], ",");
  }

//jjj
  for (int node = 0; node < DS_numNodes; node++)
  {
    colValues[LINK_LABELS][0] = '\0';
    colValues[PARENTS][0] = '\0';
    colValues[B_WEIGHTS][0] = '\0';
    colValues[D_WEIGHTS][0] = '\0';
    i = 0;

    for (int n = 0; n < DS_numNodes; n++)
    {
      if (DS_links[B][node][n] != 0.0)
      {
        if (i)
        {
          strcat(colValues[LINK_LABELS], ",");
          strcat(colValues[PARENTS], ",");
          strcat(colValues[B_WEIGHTS], ",");
          strcat(colValues[D_WEIGHTS], ",");
        }

        strcat(colValues[LINK_LABELS], "?");
        strcat(colValues[PARENTS], DS_nodeNames[n]);
        sprintf(str, "%f", DS_links[B][node][n]);
        strcat(colValues[B_WEIGHTS], str);
        sprintf(str, "%f", DS_links[D][node][n]);
        strcat(colValues[D_WEIGHTS], str);
        i++;
      }
    }

    DS_IndexToCoord(node, index, level);
    strcpy(colValues[LEVEL], DS_levelNames[level]);
    strcpy(colValues[HYPOTHESIS], DS_nodeNames[node]);
    strcpy(colValues[PHRASE], DS_nodeStrings[node]);
    sprintf(colValues[BELIEF], "%f", 0.0);    //ttt -- node value --------------
    sprintf(colValues[DISBELIEF], "%f", 0.0);    //ttt -- node value -----------
    sprintf(colValues[THRESHOLD], "%f", DS_bThresh[node]);
    sprintf(colValues[CUTOFF], "%f", DS_tThresh[node]);


    sprintf(str, "INSERT INTO `%s` VALUES (", storyName);

    for (i = 0; i < db_numColumns; i++)
    {
      strcat(str, "'");
      strcat(str, colValues[i]);
      strcat(str, "'");

      if (i < db_numColumns - 1)
        strcat(str, ",");
      else
        strcat(str, ");");
    }

    //ret = DS_ExecuteSQL(hdbc, str, &hstmt);
    ret = ExecuteSQL(hdbc, str, cDelimiter, bColumnNames, bHTMLTable, nCols);

    if (ret != 0)
    {
      printf("DS_SaveToDB - Error inserting node %d (%s) into database.\n",
             node, colValues[HYPOTHESIS]);
      fflush(stdout);
      successVal = -1;
    }

    SQLFreeHandle(SQL_HANDLE_STMT, hStmt);
  }


  //DS_CloseDatabase(henv, hdbc);
  CloseDatabase(henv, hdbc);

  return successVal;
} // DS_SaveToDB

//------------------------------------------------------------------------------

int DS_AlgoSetMissionName(char *missionName)
{
  if (missionName == NULL)
  {
    printf ("DS_AlgoSetMissionName : Mission name is NULL.\n");
    fflush(stdout);
    return (-1);
  }

  strcpy(DS_missionName, missionName);
  return 0;
}

//------------------------------------------------------------------------------

int DS_AlgoGetMissionName(char *missionName)
{
  if (missionName == NULL)
  {
    printf ("DS_AlgoGetMissionName : Mission name is NULL.\n");
    fflush(stdout);
    return (-1);
  }

  strcpy(missionName, DS_missionName);
  return 0;
}

//------------------------------------------------------------------------------

int DS_AlgoSetStoryName(char *storyName)
{
  if (storyName == NULL)
  {
    printf ("DS_AlgoSetStoryName : Story name is NULL.\n");
    fflush(stdout);
    return (-1);
  }

  strcpy(storyName, DS_storyName);

  return 0;
}

//------------------------------------------------------------------------------

int DS_AlgoGetStoryName(char *storyName)
{
  if (storyName == NULL)
  {
    printf ("DS_AlgoGetStoryName : Story name is NULL.\n");
    fflush(stdout);
    return (-1);
  }

  strcpy(DS_storyName, storyName);
  return 0;
}

//------------------------------------------------------------------------------

int DS_AlgoSetTreeNames (char* missionName, char* storyName)
{
  if (missionName == NULL)
  {
    printf ("DS_AlgoSetTreeNames : Mission name is NULL.\n");
    fflush(stdout);
    return (-1);
  }

  if (storyName == NULL)
  {
    printf ("DS_AlgoSetTreeNames : Story name is NULL.\n");
    fflush(stdout);
    return (-1);
  }

  strcpy(DS_missionName, missionName);
  strcpy(DS_storyName, storyName);

  return 0;
}

//------------------------------------------------------------------------------

int DS_GetTreeNames (char* missionName, char* storyName)
{
  if (missionName == NULL)
  {
    printf ("DS_GetTreeNames : Mission name is NULL.\n");
    fflush(stdout);
    return (-1);
  }

  if (storyName == NULL)
  {
    printf ("DS_GetTreeNames : Story name is NULL.\n");
    fflush(stdout);
    return (-1);
  }

  strcpy(missionName, DS_missionName);
  strcpy(storyName, DS_storyName);

  return 0;
}

//------------------------------------------------------------------------------

int DS_AddNode (int node, int level, char *name, char *description,
                double Bthreshold, double Tthreshold,
                double  Bval, double  Dval)
{
  int  bd, i, n, l, parNode, chNode;


  // Check if the node is already in the tree.

  i = 0;
  for (l = 0; l < DS_treeDepth; l++)
  {
    for (n = 0; n < DS_levelWidths[l]; n++)
    {
      if (strcmp(name, DS_nodeNames[i]) == 0)
      {
        //printf ("DS_AlgoAddNode : Node (%s) is already in the tree at %d,%d.\n",
        //        name, l, n);
        //fflush(stdout);
        return (-2);
      }

      i++;
    }
  }

  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_AlgoAddNode : Node level %d is out of bounds.\n", level);
    fflush(stdout);
    return (-1);
  }

  if (DS_levelWidths[level] >= DS_Max_Width)
  {
    //printf ("DS_AlgoAddNode : No room for node on level %d.\n", level);
    //fflush(stdout);
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
    DS_nodeValues[B][n] = DS_nodeValues[B][n - 1];
    DS_nodeValues[D][n] = DS_nodeValues[D][n - 1];
    DS_mergeTreeNodeIsFrom[n] = DS_mergeTreeNodeIsFrom[n - 1];
  }


  // Add the new node's name and description
  // strings, threshold values, and merge flag.

  if (name != NULL)
    strcpy (DS_nodeNames[i], name);
  else
    sprintf (DS_nodeNames[i], "?");

  if (description != NULL)
    strcpy (DS_nodeStrings[i], description);
  else
    sprintf (DS_nodeStrings[i], "?");

  DS_bThresh[i] = Bthreshold;
  DS_tThresh[i] = Tthreshold;

  DS_nodeValues[B][i] = Bval;
  DS_nodeValues[D][i] = Dval;

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
} // DS_AddNode

//------------------------------------------------------------------------------

int DS_DeleteNode (int node, int level)
{
  int  bd, i, n, parNode, chNode;


  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_AlgoDeleteNode : Node level %d is out of bounds.\n", level);
    fflush(stdout);
    return (-1);
  }

  if ((node < 0) || (node >= DS_levelWidths[level]))
  {
    printf ("DS_AlgoDeleteNode : Node index %d is out of bounds.\n", node);
    fflush(stdout);
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
    DS_nodeValues[B][n] = DS_nodeValues[B][n + 1];
    DS_nodeValues[D][n] = DS_nodeValues[D][n + 1];
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
} // DS_DeleteNode

//------------------------------------------------------------------------------

int DS_AlgoSetTreeDepth (int depth)
{
  if ((depth < 3) || (depth > DS_Max_Depth))
  {
    printf ("DS_AlgoSetTreeDepth : Depth %d is out of bounds.\n", depth);
    fflush(stdout);
    return -1;
  }

  DS_treeDepth = depth;

  return DS_treeDepth;
}

//------------------------------------------------------------------------------

int DS_AlgoGetTreeDepth (void)
{
  return DS_treeDepth;
}

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

int DS_AlgoGetLevelWidth (int level)
{
  int  width = 0;


  if ((level >= 0) && (level < DS_treeDepth))
    width = DS_levelWidths[level];

  return width;
}

//------------------------------------------------------------------------------

int DS_AlgoSetLevelName (int level, char* name)
{
  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_AlgoSetLevelName : Level %d is out of bounds.\n", level);
    fflush(stdout);
    return (-1);
  }

  strcpy(DS_levelNames[level], name);

  return 0;
}

//------------------------------------------------------------------------------

int DS_AlgoGetLevelName (int level, char* name)
{
  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_AlgoGetLevelName : Level %d is out of bounds.\n", level);
    fflush(stdout);
    strcpy(name, "?");
    return (-1);
  }

  strcpy(name, DS_levelNames[level]);

  return 0;
}

//------------------------------------------------------------------------------

int DS_AlgoGetLevelIndex (char* name)
{
  int level = -1;


  if (name != NULL)
  {
    for (level = DS_treeDepth - 1; level >= 0; level--)
    {
      if (strcmp(name, DS_levelNames[level]) == 0)
        break;
    }
  }

  return level;
}

//------------------------------------------------------------------------------

int DS_AlgoGetNodeCoordinates (char *name, int &node, int &level)
{
  if (name == NULL)
  {
    printf ("DS_AlgoGetNodeCoordinates : Node name is NULL.\n");
    fflush(stdout);
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

//------------------------------------------------------------------------------

int DS_AlgoGetNodeIndex (char *name)
{
  if (name == NULL)
  {
    printf ("DS_AlgoGetNodeIndex : Node name is NULL.\n");
    fflush(stdout);
    return -1;
  }

  for (int i = 0; i < DS_numNodes; i++)
  {
    if (strcasecmp(DS_nodeNames[i], name) == 0)
      return i;
  }

  return -1;
}

//------------------------------------------------------------------------------

int DS_AlgoSetNodeName (int node, int level, char *name)
{
  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_AlgoSetNodeName : Node level %d is out of bounds.\n", level);
    fflush(stdout);
    return (-1);
  }

  if ((node < 0) || (node >= DS_levelWidths[level]))
  {
    printf ("DS_AlgoSetNodeName : Node index %d is out of bounds.\n", node);
    fflush(stdout);
    return (-1);
  }

  if (name == NULL)
  {
    printf ("DS_AlgoSetNodeName : Name string is NULL.\n");
    fflush(stdout);
    return (-1);
  }

  strcpy(DS_nodeNames[DS_CoordToIndex (node, level)], name);

  return 0;
}

//------------------------------------------------------------------------------

int DS_AlgoGetNodeName (int node, int level, char *name)
{
  strcpy(name, "?");

  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_AlgoGetNodeName : Node level %d is out of bounds.\n",
            level);
    fflush(stdout);
    return (-1);
  }

  if ((node < 0) || (node >= DS_levelWidths[level]))
  {
    printf ("DS_AlgoGetNodeName : Node index %d is out of bounds.\n",
            node);
    fflush(stdout);
    return (-1);
  }

  strcpy(name, DS_nodeNames[DS_CoordToIndex (node, level)]);

  return 0;
}

//------------------------------------------------------------------------------

int DS_AlgoSetNodeDescription (int node, int level, char *description)
{
  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_AlgoSetNodeDescription : Node level %d is out of bounds.\n",
            level);
    fflush(stdout);
    return (-1);
  }

  if ((node < 0) || (node >= DS_levelWidths[level]))
  {
    printf ("DS_AlgoSetNodeDescription : Node index %d is out of bounds.\n",
            node);
    fflush(stdout);
    return (-1);
  }

  if (description == NULL)
  {
    printf ("DS_AlgoSetNodeDescription : Description string is NULL.\n");
    fflush(stdout);
    return (-1);
  }

  strcpy(DS_nodeStrings[DS_CoordToIndex (node, level)], description);

  return 0;
}

//------------------------------------------------------------------------------

int DS_AlgoGetNodeDescription (int node, int level, char *description)
{
  strcpy(description, "?");

  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_AlgoGetNodeDescription : Node level %d is out of bounds.\n",
            level);
    fflush(stdout);
    return (-1);
  }

  if ((node < 0) || (node >= DS_levelWidths[level]))
  {
    printf ("DS_AlgoGetNodeDescription : Node index %d is out of bounds.\n",
            node);
    fflush(stdout);
    return (-1);
  }

  strcpy(description, DS_nodeStrings[DS_CoordToIndex (node, level)]);

  return 0;
}

//------------------------------------------------------------------------------

int DS_AlgoSetNodeThresholds (int node, int level, double Bthreshold,
                              double Tthreshold)
{
  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_AlgoSetNodeThresholds : Node level %d is out of bounds.\n",
            level);
    fflush(stderr);
    return (-1);
  }

  if ((node < 0) || (node >= DS_levelWidths[level]))
  {
    printf ("DS_AlgoSetNodeThresholds : Node index %d is out of bounds.\n",
            node);
    fflush(stderr);
    return (-1);
  }

  DS_bThresh[DS_CoordToIndex (node, level)] = Bthreshold;
  DS_tThresh[DS_CoordToIndex (node, level)] = Tthreshold;

  return 0;
}

//------------------------------------------------------------------------------

int DS_AlgoGetNodeThresholds (int node, int level, double &Bthreshold,
                              double &Tthreshold)
{
  Bthreshold = -1;
  Tthreshold = -1;

  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_AlgoGetNodeThresholds : Node level %d is out of bounds.\n",
            level);
    fflush(stdout);
    return (-1);
  }

  if ((node < 0) || (node >= DS_levelWidths[level]))
  {
    printf ("DS_AlgoGetNodeThresholds : Node index %d is out of bounds.\n",
            node);
    fflush(stdout);
    return (-1);
  }

  Bthreshold = DS_bThresh[DS_CoordToIndex (node, level)];
  Tthreshold = DS_tThresh[DS_CoordToIndex (node, level)];

  return 0;
}

//------------------------------------------------------------------------------

int DS_AlgoSetNodeValues (int node, int level, double belief, double disbelief)
{
  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_AlgoSetNodeValues : Node level %d is out of bounds.\n", level);
    fflush(stdout);
    return (-1);
  }

  if ((node < 0) || (node >= DS_levelWidths[level]))
  {
    printf ("DS_AlgoSetNodeValues : Node index %d is out of bounds.\n", node);
    fflush(stdout);
    return (-1);
  }

//  DS_nodeValues[B] = belief; -------------------------ttt
//  DS_nodeValues[D] = disbelief;
//  DS_nodeValues[U] = 1.0 - belief - disbelief;

  return 0;
}

//------------------------------------------------------------------------------

int DS_AlgoGetNodeValues (int node, int level,
                          double &belief, double &disbelief)
{
  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_AlgoGetNodeValues : Node level %d is out of bounds.\n", level);
    fflush(stdout);
    return (-1);
  }

  if ((node < 0) || (node >= DS_levelWidths[level]))
  {
    printf ("DS_AlgoGetNodeValues : Node index %d is out of bounds.\n", node);
    fflush(stdout);
    return (-1);
  }

//  belief = DS_nodeValues[B]; -------------------------ttt
//  disbelief = DS_nodeValues[D];

  return 0;
}

//------------------------------------------------------------------------------

int DS_AlgoSetNodeImpact (int srcNode, int srcLevel, int destNode,
                          int destLevel, double B_Weight, double D_Weight)
{
  int  srcIndex;
  int  destIndex;
  int  success = 0;


  if ((srcLevel < 0) || (srcLevel >= DS_treeDepth))
  {
    printf ("DS_AlgoSetNodeImpact : Parent node level %d is out of bounds.\n",
            srcLevel);
    fflush(stdout);
    return (-1);
  }

  if ((srcNode < 0) || (srcNode >= DS_levelWidths[srcLevel]))
  {
    printf ("DS_AlgoSetNodeImpact : Parent node index %d is out of bounds.\n",
            srcNode);
    fflush(stdout);
    return (-1);
  }

  if ((destLevel < 0) || (destLevel >= DS_treeDepth))
  {
    printf ("DS_AlgoSetNodeImpact : Child node level %d is out of bounds.\n",
            destLevel);
    fflush(stdout);
    return (-1);
  }

  if ((destNode < 0) || (destNode >= DS_levelWidths[destLevel]))
  {
    printf ("DS_AlgoSetNodeImpact : Child node index %d is out of bounds.\n",
            destNode);
    fflush(stdout);
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

//------------------------------------------------------------------------------

int DS_AlgoGetNodeImpact (int srcNode, int srcLevel, int destNode,
                          int destLevel, double &B_Weight, double &D_Weight)
{
  int  srcIndex;
  int  destIndex;


  if ((srcLevel < 0) || (srcLevel >= DS_treeDepth))
  {
    printf ("DS_AlgoSetNodeImpact : Parent node level %d is out of bounds.\n",
            srcLevel);
    fflush(stdout);
    return (-1);
  }

  if ((srcNode < 0) || (srcNode >= DS_levelWidths[srcLevel]))
  {
    printf ("DS_AlgoSetNodeImpact : Parent node index %d is out of bounds.\n",
            srcNode);
    fflush(stdout);
    return (-1);
  }

  if ((destLevel < 0) || (destLevel >= DS_treeDepth))
  {
    printf ("DS_AlgoSetNodeImpact : Child node level %d is out of bounds.\n",
            destLevel);
    fflush(stdout);
    return (-1);
  }

  if ((destNode < 0) || (destNode >= DS_levelWidths[destLevel]))
  {
    printf ("DS_AlgoSetNodeImpact : Child node index %d is out of bounds.\n",
            destNode);
    fflush(stdout);
    return (-1);
  }


  srcIndex = DS_CoordToIndex(srcNode, srcLevel);
  destIndex = DS_CoordToIndex(destNode, destLevel);

  B_Weight = DS_links[B][destIndex][srcIndex];
  D_Weight = DS_links[D][destIndex][srcIndex];

  return 0;
}

//------------------------------------------------------------------------------

int DS_AlgoSetLinkLabel (int srcNode, int srcLevel, int destNode,
                         int destLevel, char *label)
{
  int  srcIndex;
  int  destIndex;


  if ((srcLevel < 0) || (srcLevel >= DS_treeDepth))
  {
    printf ("DS_AlgoSetLinkLabel : Parent node level %d is out of bounds.\n",
            srcLevel);
    fflush(stdout);
    return (-1);
  }

  if ((srcNode < 0) || (srcNode >= DS_levelWidths[srcLevel]))
  {
    printf ("DS_AlgoSetLinkLabel : Parent node index %d is out of bounds.\n",
            srcNode);
    fflush(stdout);
    return (-1);
  }

  if ((destLevel < 0) || (destLevel >= DS_treeDepth))
  {
    printf ("DS_AlgoSetLinkLabel : Child node level %d is out of bounds.\n",
            destLevel);
    fflush(stdout);
    return (-1);
  }

  if ((destNode < 0) || (destNode >= DS_levelWidths[destLevel]))
  {
    printf ("DS_AlgoSetLinkLabel : Child node index %d is out of bounds.\n",
            destNode);
    fflush(stdout);
    return (-1);
  }

  if (label == NULL)
  {
    printf ("DS_AlgoSetLinkLabel : Label string is NULL.\n");
    fflush(stdout);
    return (-1);
  }

  srcIndex = DS_CoordToIndex(srcNode, srcLevel);
  destIndex = DS_CoordToIndex(destNode, destLevel);

  strcpy(DS_linkNames[destIndex][srcIndex], label);

  return 0;
}

//------------------------------------------------------------------------------

int DS_AlgoGetLinkLabel (int srcNode, int srcLevel, int destNode,
                         int destLevel, char *label)
{
  int  srcIndex;
  int  destIndex;


  strcpy(label, "?");

  if ((srcLevel < 0) || (srcLevel >= DS_treeDepth))
  {
    printf ("DS_AlgoGetLinkLabel : Parent node level %d is out of bounds.\n",
            srcLevel);
    fflush(stdout);
    return (-1);
  }

  if ((srcNode < 0) || (srcNode >= DS_levelWidths[srcLevel]))
  {
    printf ("DS_AlgoGetLinkLabel : Parent node index %d is out of bounds.\n",
            srcNode);
    fflush(stdout);
    return (-1);
  }

  if ((destLevel < 0) || (destLevel >= DS_treeDepth))
  {
    printf ("DS_AlgoGetLinkLabel : Child node level %d is out of bounds.\n",
            destLevel);
    fflush(stdout);
    return (-1);
  }

  if ((destNode < 0) || (destNode >= DS_levelWidths[destLevel]))
  {
    printf ("DS_AlgoGetLinkLabel : Child node index %d is out of bounds.\n",
            destNode);
    fflush(stdout);
    return (-1);
  }


  srcIndex = DS_CoordToIndex(srcNode, srcLevel);
  destIndex = DS_CoordToIndex(destNode, destLevel);

  strcpy(label, DS_linkNames[destIndex][srcIndex]);

  return 0;
}

//------------------------------------------------------------------------------

int DS_MergeTrees (int mergeFunction, char* structFile, char* dataFile)
{
  FILE    *att;
  char    *token;
  char    hold[MAX_LINE_LEN];
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
    fflush(stderr);
    return -1;
  }

  if (structFile == NULL)
  {
    fprintf (stderr, "DS_AlgoMergeTrees : Tree filename is NULL.\n");
    fflush(stderr);
    return -1;
  }

  if (dataFile == NULL)
  {
    fprintf (stderr, "DS_AlgoMergeTrees : Data filename is NULL.\n");
    fflush(stderr);
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
    fflush(stdout);
    return (-1);
  }


  // Read in number of tree levels.

  if (DS_GetNextLine(hold, att))
    return (-1);

  sscanf(hold, "%d", &other_treeDepth);

  if ((other_treeDepth < 1) || (other_treeDepth > DS_Max_Depth))
  {
    printf ("DS_AlgoMergeTrees : Number of levels in tree (%d)"
            " must range 1...%1d\n", other_treeDepth, DS_Max_Depth);
    fflush(stdout);
    return (-1);
  }


  // Read level names and node info.

  other_numNodes = 0;
  n = 0;

  for (level = 0; level < other_treeDepth; level++)
  {
    if (DS_GetNextLine(hold, att))
      return (-1);

    sscanf (hold, "%d %s", &other_levelWidths[level], other_levelNames[level]);

    if ((other_levelWidths[level] < 1) ||
        (other_levelWidths[level] > DS_Max_Width))
    {
      printf ("DS_AlgoMergeTrees : Number of nodes (%d) in a level of the tree "
              "must range 1...%1d\n", other_levelWidths[level], DS_Max_Width);
      fflush(stdout);
      return (-1);
    }

    strsub(other_levelNames[level], '_', ' ');

    other_numNodes += other_levelWidths[level];


    for (node = 0; node < other_levelWidths[level]; node++)
    {
      // Read node name, and B and T thresholds.

      if (DS_GetNextLine(hold, att))
        return (-1);

      sscanf(hold, "%s %s %lf %lf",
             other_nodeNames[n], other_nodeStrings[n],
             &other_bThresh[n], &other_tThresh[n]);

      strsub(other_nodeNames[n], '_', ' ');
      strsub(other_nodeStrings[n], '_', ' ');
      n++;
    }
  }


  // Read in the parent-to-child link matrices.

  if (DS_GetNextLine(hold, att))
    return (-1);

  if ((hold[0] == 'T') || (hold[0] == 't'))
    BandDImpactsDiffer = TRUE;
  else
    BandDImpactsDiffer = FALSE;

  for (bd = B; bd <= D; bd++)
  {
    for (chNode = 0; chNode < other_numNodes; chNode++)
    {
      if ((bd == B) || BandDImpactsDiffer)
      {
        if (DS_GetNextLine(hold, att))
          return (-1);

        if (strncmp(hold, " ----", 5) == 0)
        {
          if (DS_GetNextLine(hold, att))
            return (-1);
        }

        token = strtok(hold, " |");

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
                        other_tThresh[other_index],
                        savedNet_nodes[level][node].belief);

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
          // Find the node in the main tree with the least B value.

          DS_index = DS_CoordToIndex(1, level);  // Get index of second node.
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
                            other_tThresh[other_index],
                            savedNet_nodes[level][node].belief) == 0)
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
      fflush(stderr);
      return -2;
    }
  }

  return 0;
} // DS_MergeTrees 

//------------------------------------------------------------------------------

int DS_AlgoGetMergeNodesOriginalTree (int node, int level)
{
  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_AlgoGetMergeNodesOriginalTree : Node level %d "
            "is out of bounds.\n", level);
    fflush(stdout);
    return (-1);
  }

  if ((node < 0) || (node >= DS_levelWidths[level]))
  {
    printf ("DS_AlgoGetMergeNodesOriginalTree : Node index %d"
            " is out of bounds.\n", node);
    fflush(stdout);
    return (-1);
  }

  return DS_mergeTreeNodeIsFrom[DS_CoordToIndex (node, level)];
}

//------------------------------------------------------------------------------

int DS_AlgoGetNumMergeNodesLost (int level)
{
  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("DS_AlgoGetNumMergeNodesLost : Node level %d is out of bounds.\n",
            level);
    fflush(stdout);
    return (-1);
  }

  return DS_numNodesLost[level];
}

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

int DS_IndexToCoord(int index, int &node, int &level)
{
  int  i;


  if ((index < 0) || (index >= DS_numNodes))
  {
    printf ("DS_IndexToCoord : Node index %d is out of bounds.\n", index);
    fflush(stdout);
    return (-1);
  }

  node = 0;

  level = 0;
  while (DS_levelWidths[level] == 0)
    level++;

  for (i = 0; i < index; i++)
  {
    node++;

    if (node >= DS_levelWidths[level])
    {
      level++;
      while (DS_levelWidths[level] == 0)
        level++;

      node = 0;
    }
  }

  return 0;
}

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

/****** Using isql.C instead.  Note -- These calls are more modern.
* // Could use OpenDatabase from isql.C instead...
* 
* int DS_OpenDatabase(SQLHENV *phEnv, SQLHDBC *phDbc)
* {
*   const char db_DSN[] = "MySQL";
*   const char db_UID[] = "root";
*   const char db_PWD[] = "gpda";
* 
*   SQLRETURN  ret;
* 
* 
*   // Allocate environment handle.
*   ret = SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, phEnv);
* 
*   if (ret != SQL_SUCCESS && ret != SQL_SUCCESS_WITH_INFO)
*   {
*     printf ("DS_OpenDatabase : Cannot allocate environment handle.\n");
*     fflush(stdout);
*     return -1;
*   }
* 
* 
*   ret = SQLSetEnvAttr(*phEnv, SQL_ATTR_ODBC_VERSION,
*                       (SQLPOINTER)SQL_OV_ODBC3, 0);
*   if (ret != SQL_SUCCESS && ret != SQL_SUCCESS_WITH_INFO)
*   {
*     printf ("DS_OpenDatabase : Cannot set the ODBC version.\n");
*     fflush(stdout);
*     SQLFreeEnv(*phEnv);
*     return -1;
*   }
* 
* 
*   // Allocate connection handle.
*   ret = SQLAllocHandle(SQL_HANDLE_DBC, *phEnv, phDbc);
* 
*   if (ret != SQL_SUCCESS && ret != SQL_SUCCESS_WITH_INFO)
*   {
*     printf ("DS_OpenDatabase : Cannot allocate connection handle.\n");
*     fflush(stdout);
*     SQLFreeEnv(*phEnv);
*     return -1;
*   }
* 
* 
*   // Connect to data source myodbc3.
*   if (!SQL_SUCCEEDED(SQLConnect(*phDbc,
*                                 (SQLCHAR*) db_DSN, SQL_NTS,
*                                 (SQLCHAR*) db_UID, SQL_NTS,   //optional userid
*                                 (SQLCHAR*) db_PWD, SQL_NTS))) //optional passwd
*   {
*     printf ("DS_ReadFromDB : Cannot connect to (%s).\n", db_DSN);
*     fflush(stdout);
*     SQLFreeConnect(*phDbc);
*     SQLFreeEnv(*phEnv);
*     return -1;
*   }
* 
* 
*   // Set auto commit to ON.
*   UWORD  commitVal = SQL_AUTOCOMMIT_ON;
*   ret = SQLSetConnectAttr(*phDbc, SQL_ATTR_AUTOCOMMIT,
*                           &commitVal, sizeof(commitVal));
*                           //SQL_AUTOCOMMIT_ON, 0);
* 
*   return 0;
* }
* 
* * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *
* 
* // Could use CloseDatabase from isql.C instead...
* 
* void DS_CloseDatabase(SQLHENV henv, SQLHDBC hdbc)
* {
*   // Disconnect from the server.
*   SQLDisconnect(hdbc);
* 
*   // Close the connection handle.
*   SQLFreeHandle(SQL_HANDLE_DBC, hdbc);
* 
*   // Close the environment handle.
*   SQLFreeHandle(SQL_HANDLE_ENV, henv);
* }
* 
* * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *
* 
* // From ExecuteSQL of isql.C, with changes.
* 
* int DS_ExecuteSQL(SQLHDBC hdbc, char *stmt, SQLHSTMT *phstmt)
* {
*   int        successVal = 0;
*   SQLRETURN  ret;
* 
* 
*   // Allocate statement handle.
*   ret = SQLAllocHandle(SQL_HANDLE_STMT, hdbc, phstmt);
* 
*   if (ret == SQL_SUCCESS || ret == SQL_SUCCESS_WITH_INFO)
*   {
*     ret = SQLExecDirect(*phstmt, (SQLCHAR *)stmt, SQL_NTS);
* 
*     if (ret != SQL_SUCCESS && ret != SQL_SUCCESS_WITH_INFO)
*     {
*       printf("DS_ExecuteSQL : Cannot (%s) - %d\n", stmt, ret);
*       fflush(stdout);
*       SQLFreeStmt(*phstmt, SQL_DROP);
*       successVal = -1;
*     }
*   }
*   else
*   {
*     printf ("DS_ExecuteSQL : Cannot allocate statement handle.\n");
*     fflush(stdout);
*     successVal = -1;
*   }
* 
*   return successVal;
* }
******/

//------------------------------------------------------------------------------

int DS_SearchSQL(SQLHDBC hdbc, char *searchObj, char *name)
{
  int        found = FALSE;
  int        nCols;
  char       str[1024];
  SQLCHAR    sqlStr[DB_NAME_LEN + 1] = "";
  SQLLEN     nIndicator = 0;
  SQLRETURN  ret;


  sprintf(str, "SHOW %s;", searchObj);

  //if (DS_ExecuteSQL(hdbc, str, &hstmt) != 0)
  ret = ExecuteSQL(hdbc, str, cDelimiter, bColumnNames, bHTMLTable, nCols);
  if (ret != 0)
    return -1;

DS_verbose = 1;
printf("Looking for (%s)\n", name); fflush(stdout);

  if (DS_verbose)
  {
    printf("%s:\n", searchObj);
    fflush(stdout);
  }

  ret = SQLFetch(hStmt);

  while (ret == SQL_SUCCESS || ret == SQL_SUCCESS_WITH_INFO)
  {
    ret = SQLGetData(hStmt, 1, SQL_C_CHAR, (SQLPOINTER)sqlStr,
                     sizeof(sqlStr), &nIndicator);

    if (ret == SQL_SUCCESS && nIndicator != SQL_NULL_DATA)
    {
      sqlStr[DB_NAME_LEN] = '\0';
      sprintf(str, "%s", sqlStr);

      if (DS_verbose)
      {
        printf(" - (%s)", str);
        fflush(stdout);
      }

      if (strcmp(str, name) == 0)
      {
        found = TRUE;

        if (DS_verbose)
        {
          printf(" <- USE\n");
          fflush(stdout);
        }
      }
      else if (DS_verbose)
      {
        printf("\n");
        fflush(stdout);
      }
    }
    else if (ret == SQL_ERROR)
    {
      printf("DS_SearchSQL - Cannot get data from table row\n");
      fflush(stdout);
      break;
    }

    ret = SQLFetch(hStmt);
  } // while rows
DS_verbose = 0;

  SQLFreeStmt(hStmt, SQL_DROP);

  return found;
}

