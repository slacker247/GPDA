#include <stdio.h>
#include "savedNet.h"
#include "Globals.h"


char  savedNet_mission[128];
int   savedNet_numLevels;

savedNet_levelType  savedNet_levels[DS_Max_Depth];
savedNet_nodeType   savedNet_nodes[DS_Max_Depth][DS_Max_Width];


//------------------------------------------------------------------------------


int savedNet_readFile(char* fileName)
{
  FILE   *fp;
  char   dsbtemp[128];
  int    levelNum, nodeNum;
  int    level, node;


  // Init.
  savedNet_mission[0] = '\0';
  savedNet_numLevels = 0;

  for (level = 0; level < DS_Max_Depth; level++)
  {
    savedNet_levels[level].name[0] = '\0';
    savedNet_levels[level].numNodes = 0;
  }


  if (fileName == NULL)
  {
    printf ("readFile -- fileName is NULL\n");
    fflush(stdout);
    return -1;
  }

  if ((fp = fopen(fileName, "r")) == NULL)
  {
    printf ("readFile -- Error opening file (%s)\n", fileName);
    fflush(stdout);
    return -1;
  }


  do fgets(dsbtemp, 128, fp); while (dsbtemp[0] == '#');

  sscanf(dsbtemp, "%d  %s\n", &savedNet_numLevels, savedNet_mission);
  strsub(savedNet_mission, '_', ' ');

  for (level = 0; level < savedNet_numLevels; level++)
  {
    do fgets(dsbtemp, 128, fp); while (dsbtemp[0] == '#');

    sscanf(dsbtemp, "%d", &levelNum);
    sscanf(dsbtemp, "%*d %d %s",
           &savedNet_levels[levelNum].numNodes,
           savedNet_levels[levelNum].name);
    strsub(savedNet_levels[levelNum].name, '_', ' ');

    for (node = 0; node < savedNet_levels[levelNum].numNodes; node++)
    {
      do fgets(dsbtemp, 128, fp); while (dsbtemp[0] == '#');

      sscanf(dsbtemp, "%d", &nodeNum);

      sscanf(dsbtemp, "%*d %f %f %f %s",
             &savedNet_nodes[levelNum][nodeNum].belief,
             &savedNet_nodes[levelNum][nodeNum].disbelief,
             &savedNet_nodes[levelNum][nodeNum].time,
             savedNet_nodes[levelNum][nodeNum].name);
      strsub(savedNet_nodes[levelNum][nodeNum].name, '_', ' ');
    }
  }

  fclose(fp);

  return 0;
}

//------------------------------------------------------------------------------

void savedNet_saveFile(char* fileName)
{
  FILE  *fp = fopen(fileName, "w");

  fprintf(fp, "%4d  %s      # Levels, CaseID\n",
          savedNet_numLevels, savedNet_mission);

  for (int level = 0; level < savedNet_numLevels; level++)
  {
    fprintf(fp, "%4d %4d %s      Level #, Nodes this level, Hypothesis\n",
            level, savedNet_levels[level].numNodes,
            savedNet_levels[level].name);

    for (int node = 0; node < savedNet_levels[level].numNodes; node++)
    {
      fprintf(fp, "     %d    %f %f %f   %s\n",
              node,
              savedNet_nodes[level][node].belief,
              savedNet_nodes[level][node].disbelief,
              savedNet_nodes[level][node].time,
              savedNet_nodes[level][node].name);
    }
  }

  fclose(fp);
}

