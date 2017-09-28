
/***
  E means Evid   means Evidence,   the first level, index 0
  H means Hypot  means Hypotheses, the middle levels, indices 1...Tree_Depth-2
  A means Assoc  means Assessment, the last level, index Tree_Depth - 1

  B means Belief
  D means Disbelief
  U means Uncertainity or Don't Know (1 - B - D)
***/


#ifndef _DS_ATTRIBUTES_H
#define _DS_ATTRIBUTES_H

//------------------------------------------------------------------------------

const int  DS_Max_String_Length = 256; // Up to 255 characters and one \0

const int  DS_Max_Depth     = 6;   // Num hierarchy levels (E, Hs, A)
const int  DS_Max_Width     = 35; //6 ttt    // Num nodes at a level
const int  DS_Max_Nodes     = DS_Max_Depth * DS_Max_Width;

// Merge functions and merged node info:
const int  DS_Intersection  = 0;
const int  DS_Union         = 1;
const int  DS_Both_Trees    = 0;    // Node in new tree was in both trees
const int  DS_Current_Tree  = 1;    // Node in new tree is from current tree
const int  DS_Saved_Tree    = 2;    // Node in new tree is from saved tree

const int  DS_UseFlatFile   = 0;
const int  DS_UseDatabase   = 1;

//------------------------------------------------------------------------------

extern char    DS_levelNames[DS_Max_Depth][DS_Max_String_Length];
extern char    DS_nodeNames[DS_Max_Nodes][DS_Max_String_Length];
extern char    DS_nodeStrings[DS_Max_Nodes][DS_Max_String_Length];

extern int     DS_treeDepth;
extern int     DS_levelWidths[DS_Max_Depth];
extern int     DS_numNodes;

extern double  DS_nodeValues[3][DS_Max_Nodes];
extern double  DS_links[2][DS_Max_Nodes][DS_Max_Nodes];

//------------------------------------------------------------------------------

extern void DS_AlgoPrintAttributes(void);

  // DS_AlgoPrintAttributes prints tree structure info to stdout.


extern int DS_AlgoSaveAttributes
  (char  *missionName,     // In - Name of mission
   char  *storyName,       // In - Name of story
   int   readFromDB = 0);  // In - FALSE (0) = save to flatfile, TRUE (1) = DB

  // DS_AlgoSaveAttributes saves the DS network data.  If readFromDB is
  // TRUE, data is saved to the missionName database in the storyName
  // table, otherwise data is saved to the missionName flat
  // file. Returns 0 on success, -1 on failure.


extern int DS_AlgoGetTreeDepth (void);

  // Returns num node levels in tree


extern int DS_AlgoSetTreeDepth
  (int  depth);  // In - Num levels in tree (1..DS_Max_Depth)

  // DS_AlgoSetTreeDepth sets the tree depth to the number passed in
  // if it is 3 or greater, and returns that value; otherwise returns -1.


extern int DS_AlgoGetMissionName
  (char  *missionName);  // Out - mission/tree name copied to parameter array


extern int DS_AlgoSetMissionName
  (char  *missionName);  // In


extern int DS_AlgoGetStoryName
  (char  *storyName);  // Out - mission/tree name copied to parameter array


extern int DS_AlgoSetStoryName
  (char  *storyName);  // In


extern int DS_AlgoGetTreeNames
  (char  *missionName,  // Out - mission/tree name copied to parameter array
   char  *storyName);   // Out - story name copied to parameter array

  // DS_AlgoSetTreeNames sets the mission name and story name to those
  // provided.  Returns -1 if either is NULL otherwise returns 0;


extern int DS_AlgoSetTreeNames
  (char  *missionName,  // In
   char  *storyName);   // In

  // DS_AlgoSetTreeNames sets the mission name and story name to those
  // provided.  Returns -1 if either is NULL otherwise returns 0;


extern int DS_AlgoGetLevelWidth
  (int  level);  // In - Tree level index (0..Tree_Depth - 1)

  // DS_AlgoGetLevelWidth returns num nodes at given level of tree.


extern int DS_AlgoSetLevelWidth
  (int  level,   // In - Tree level index (0..Tree_Depth - 1)
   int  width);  // In - Level width (1..DS_Max_Width)

  // DS_AlgoSetLevelWidth sets the width at the specified
  // level to the specified width if the level and width are
  // in bounds, and returns that width; otherwise returns -1.


extern int DS_AlgoGetLevelName
  (int   level,   // In  - 0..Tree_Depth - 1
   char  *name);  // Out - Level name copied to parameter array


extern int DS_AlgoSetLevelName
  (int   level,   // In - 0..Tree_Depth - 1
   char  *name);  // In


extern int DS_AlgoGetLevelIndex
  (char  *name);  // In - Level name copied to parameter array


extern int DS_AlgoGetNodeCoordinates
  (char  *name,   // In  - Node name
   int   &node,   // Out - 0..levelWidth - 1
   int   &level); // Out - 0..Tree_Depth - 1

extern int DS_AlgoGetNodeName
  (int   node,    // In  - 0..levelWidth - 1
   int   level,   // In  - 0..Tree_Depth - 1
   char  *name);  // Out - String copied to parameter array


extern int DS_AlgoSetNodeName
  (int   node,    // In - 0..levelWidth - 1
   int   level,   // In - 0..Tree_Depth - 1
   char  *name);  // In


extern int DS_AlgoGetNodeDescription
  (int   node,           // In  - 0..levelWidth - 1
   int   level,          // In  - 0..Tree_Depth - 1
   char  *description);  // Out - String copied to parameter array


extern int DS_AlgoSetNodeDescription
  (int   node,           // In - Index of node on its level (0..levelWidth-1)
   int   level,          // In - Tree level node is at (0..Tree_Depth - 1)
   char  *description);  // In - Phrase describing node (elaborated title)

  // DS_AlgoSetNodeDescription sets the description of the specified node
  // at the given level to the strings passed in.  Returns -1 if level or
  // node is not in range, or description is NULL;  otherwise returns 0.


extern int DS_AlgoGetNodeThresholds
  (int     node,          // In  - 0..levelWidth - 1
   int     level,         // In  - 0..Tree_Depth - 1
   double  &Bthreshold,   // Out - Node's belief value
   double  &Tthreshold);  // Out - Node's time threshold


extern int DS_AlgoSetNodeThresholds
  (int     node,         // In - 0..levelWidth - 1
   int     level,        // In - 0..Tree_Depth - 1
   double  Bthreshold,   // In - Node's belief threshold
   double  Tthreshold);  // In - Node's time threshold


extern int DS_AlgoGetNodeValues
  (int     node,         // In  - 0..levelWidth - 1
   int     level,        // In  - 0..Tree_Depth - 1
   double  &belief,      // Out - Node's belief value
   double  &disbelief);  // Out - Node's disbelief value

  // DS_AlgoGetNodeValues returns the belief and disbelief values of
  // the given node.  Returns -1 if the node or level index is out of
  // bounds, otherwise returns 0.


extern int DS_AlgoSetNodeValues
  (int     node,        // In - 0..levelWidth - 1
   int     level,       // In - 0..Tree_Depth - 1
   double  belief,      // In - Node's belief value
   double  disbelief);  // In - Node's disbelief value

  // DS_AlgoSetNodeValues sets the belief and disbelief values of the
  // given node to the given values.  Any node values are accepted,
  // though in general they should range 0.0..1.0.  Returns -1 if the
  // node or level index is out of bounds, otherwise returns 0.


extern int DS_AlgoGetNodeImpact
  (int     srcNode,     // In  - Index of parent node in level srcLevel
   int     srcLevel,    // In  - Level in tree of parent node
   int     destNode,    // In  - Index of child node in level destLevel
   int     destLevel,   // In  - Level in tree of child node
   double  &B_Weight,   // Out - Impact weight of src belief on dest
   double  &D_Weight);  // Out - Impact weight of src disbelief on dest

  // DS_AlgoGetNodeImpact gets the impact values of the specified
  // source (parent) node on the specified destination (child) node.
  // Returns -1 if any parameter is out of bounds, otherwise returns 0.


extern int DS_AlgoSetNodeImpact
  (int     srcNode,    // In - Index of parent node in level srcLevel
   int     srcLevel,   // In - Level in tree of parent node
   int     destNode,   // In - Index of child node in level destLevel
   int     destLevel,  // In - Level in tree of child node
   double  B_Weight,   // In - Impact weight of parent's belief on child
   double  D_Weight);  // In - Impact weight of parent's disbelief on child

  // DS_AlgoSetNodeImpact sets the impact values of the specified
  // source (parent) node on the specified destination (child) node.
  // Returns -1 if any parameter is out of bounds, otherwise returns
  // 0.  Does not check that impact values exceed 1.0.


extern int DS_AlgoGetLinkLabel
  (int     srcNode,    // In  - Index of parent node in level srcLevel
   int     srcLevel,   // In  - Level in tree of parent node
   int     destNode,   // In  - Index of child node in level destLevel
   int     destLevel,  // In  - Level in tree of child node
   char*   label);     // Out - Impact weight of src disbelief on dest

  // DS_AlgoGetLinkLabel gets the label of the link from the specified
  // source (parent) node to the specified destination (child) node.
  // Returns -1 if any parameter is out of bounds, otherwise returns 0.


extern int DS_AlgoSetLinkLabel
  (int     srcNode,    // In - Index of parent node in level srcLevel
   int     srcLevel,   // In - Level in tree of parent node
   int     destNode,   // In - Index of child node in level destLevel
   int     destLevel,  // In - Level in tree of child node
   char*   label);     // In - Impact weight of parent's disbelief on child

  // DS_AlgoSetLinkLabel gets the label of the link from the specified
  // source (parent) node to the specified destination (child) node.
  // Returns -1 if any parameter is out of bounds or in label is null,
  // otherwise returns 0.


extern int DS_AlgoGetMergeNodesOriginalTree (int node, int level);

  // DS_AlgoGetNodesOriginalTree returns a value indicating which tree
  // (the current tree, saved tree, or both) the specified node of the
  // final tree came from.


extern int DS_AlgoGetNumMergeNodesLost (int level);

  // DS_AlgoGetNumNodesLost returns the number of nodes lost from the
  // specified tree level, due to the level being too full, during tree
  // merging.


extern void DS_AlgoReverseLinks(void);

  // DS_AlgoReverseLinks reverses the direction of all links in the tree.


//------------------------------------------------------------------------------

// These are functions that would be in Protected section if this were
// set up as a class, as inheritors (eg. DS_Algo) of this class would
// probably provide additional code to complete the functions.  Users
// of DS_Algo should not call these functions but instead call DS_Algo's
// version of these functions, which would then call these functions.


extern int DS_InitAttributes
  (char  *missionName,     // In - Name of mission
   char  *storyName,       // In - Name of story
   int   verbose,          // In - TRUE (1) = prints what reads from file
   int   readFromDB = 0);  // In - FALSE (0) = read from flatfile, TRUE (1) = DB

  // DS_InitAttributes initializes attribute variables.  If a mission
  // name is not given, everything is just initialized.  Otherwise if
  // readFromDB is TRUE, data is read from the storyName table of the
  // missionName DB, or is readFromDB is FALSE, the missionName flat file
  // is read.  Returns 0 on success, -1 on failure, -2 if the database
  // option is specified before database code is available.


extern int DS_AddNode
  (int     node,          // In - Index of new node (0..levelWidth)
   int     level,         // In - Level of new node (0..Tree_Depth - 1)
   char    *name,         // In - Node title, may be NULL
   char    *description,  // In - Node description, may be NULL
   double  Bthreshold,    // In - Belief threshold
   double  Tthreshold,    // In - Time cutoff
   double  Bval = 0.0,    // Opt IN - Belief value
   double  Dval = 0.0);   // Opt IN - Disbelief value

  // DS_AddNode inserts a new node at the position given.  New node
  // must go on an existing level.  If name or description string is NULL,
  // it is saved as "".  If node index is < 0, it is set to 0; if node
  // index > levelWidth, it is set to levelWidth.  Returns -1 if level is
  // out of bounds, there is no room for another node on the level, or if
  // there's already a name node in the tree; otherwise returns 0.


extern int DS_DeleteNode
  (int  node,    // In - Index of node (0..levelWidth - 1)
   int  level);  // In - Level of node (0..Tree_Depth - 1)

  // DS_DeleteNode removes the specified node from the tree.
  // Returns -1 if node or level is out of bounds, otherwise returns 0.



// These functions are in this section as only direct manipulators
// of the links matrices would have use for them:

extern int DS_IndexToCoord
  (int  index,
   int  &node,
   int  &level);

  // DS_IndexToCoord converts the given node-list index to tree
  // level and node index within that level.  Returns -1 if index
  // is out of bounds, otherwise returns 0.


extern int DS_CoordToIndex
  (int node,
   int level);

  // DS_CoordToIndex converts the given tree level and node index within
  // that level to the corresponding node-list index.   Returns -1 if
  // node or level is out of bounds, otherwise returns 0.


extern int DS_MergeTrees
  (int    mergeFunction,  // In
   char  *structFile,     // In
   char  *dataFile);      // In

  // DS_MergeTrees merges the specified saved tree with the current tree,
  // using the given merge function (union or intersection).  Returns -1
  // if a file name (structFile or dataFile) is NULL or if an error occurs
  // reading those files; returns -2 if processing is successful but one
  // or more layers of the final tree contains 0 nodes; returns 0 on success.


#endif  // _DS_ATTRIBUTES_H
