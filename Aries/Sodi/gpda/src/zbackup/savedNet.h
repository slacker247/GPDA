#ifndef _SAVEDNET_H
#define _SAVEDNET_H

#include "DSBattributes.h"

typedef struct {
  char  name[128];
  int   numNodes;
} savedNet_levelType;

typedef struct {
  char   name[128];
  float  belief;
  float  disbelief;
  float  time;
} savedNet_nodeType;


extern char  savedNet_mission[128];
extern int   savedNet_numLevels;

extern savedNet_levelType  savedNet_levels[DS_Max_Depth];
extern savedNet_nodeType   savedNet_nodes[DS_Max_Depth][DS_Max_Width];


extern int savedNet_readFile(char* fileName);

extern void savedNet_saveFile(char* fileName);


#endif  // _SAVEDNET_H
