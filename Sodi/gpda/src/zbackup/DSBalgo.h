
/*
   E means Evid   means Evidence,   the first level, index 1
   H means Hypot  means Hypotheses, the middle levels, indices 2...Tree_Depth-1
   A means Assoc  means Assessment, the last level, index Tree_Depth
   B means Belief
   D means Disbelief
   U means Uncertainity or Don't Know (1 - B - D)

   BP means BackPropogation
*/


#ifndef _DSB_ALGO_H
#define _DSB_ALGO_H  1

#include "DSBattributes.h"


#define DS_Debug_None        0   // No debug displayed
#define DS_Debug_InOut       1   // Evidence and Assessment values displayed
#define DS_Debug_Flow        2   // Follow evidence data as flows thru tree
#define DS_Debug_Compute     3   // Follow evidence data, show computations

#define DS_Err_LevelTooBig        -1
#define DS_Err_NodeTooBig         -2
#define DS_Err_Value_OutOfBounds  -3


// Backpropogation constraints:
//   - Existing  = only existing links may change
//   - Any       = existing links may change, new links may be added
//   - Bounded   = link weights range 0..1
//   - Unbounded = link weights may take any value

#define DS_Any_Unbounded_Constraint      0
#define DS_Any_Bounded_Constraint        1
#define DS_Existing_Unbounded_Constraint 2
#define DS_Existing_Bounded_Constraint   3
#define DS_BP_No_Constraint        0  // Change any links, range 0 or greater
#define DS_BP_Partial_2_Constraint 1  // Change any links, range 0..1
#define DS_BP_Partial_1_Constraint 2  // Change existing links, range >= 0
#define DS_BP_Full_Constraint      3  // Change only existing links, range 0..1



typedef struct {
  int     Num_Iterations;         // Number of backpropogation iterations
  int     Num_B_Weights_Changed;  // Number of links whose B weight changed
  int     Num_D_Weights_Changed;  // Number of links whose D weight changed
  int     Constraint_Level;       // Constraint level for run
  double  Learning_Rate;          // Learning rate for run
  double  B_Error;                // Max diff in desired and actual B values
  double  D_Error;                // Max diff in desired and actual D values
  double  L;                      // Link weight perturbation
  double  meanL;                  // Mean link weight perpurbation
} DS_BP_Run_Statistics_Type;



extern int DS_Algo
  (int    node,         // In - Evidence node index (0..levelWidth - 1)
   int    level,        // In - Evidence level index (0..TreeDepth - 1)
   float  New_B,        // In - Belief value of new Evid data (0.0 .. 1.0)
   float  New_D,        // In - Disbelief value of new Evid data (0.0 .. 1.0)
   float  Time_Entry);  // In - Timestamp of new Evidence data

  // DS_Algo processes the given evidence data.
  // NOTE: DS_AlgoInit must be called before DS_Algo is called!


extern int DS_AlgoInit
  (char  *fname,    // In - Name of attribute file to be read
   int   verbose);  // In - see DS_Debug_... values above

  // DS_AlgoInit initializes the DS_Algo.  Call this function with
  // fname = NULL to create a new tree that will be defined via the
  // DS_Set... functions, or with fname = an attributeFile name to
  // read in the file.


extern void DS_AlgoReset (void);

  // DS_AlgoReset deletes all evidence passed thru the tree, resetting
  // nodes to [B U D] = [0 1 0].   Link weights are unchanged.


extern void DS_AlgoPrintTree (void);

  // DS_AlgoPrintTree prints to stdout the tree's node values and link weights.


extern int DS_AlgoAddNode
  (int     node,          // In - Index of new node (0..levelWidth)
   int     level,         // In - Level of new node (0..Tree_Depth - 1)
   char    *name,         // In - Node title, may be NULL
   char    *description,  // In - Node description, may be NULL
   double  bThreshold,    // In - Belief threshold
   double  tThreshold);   // In - Time cutoff

  // DS_AlgoAddNode inserts a new node at the position given.  New node
  // must go on an existing level.  If name or description string is NULL,
  // node's string is set to "".  Returns -1 if node or level is out of
  // bounds, otherwise returns 0.


extern int DS_AlgoDeleteNode
  (int  node,    // In - Index of node (0..levelWidth - 1)
   int  level);  // In - Level of node (0..Tree_Depth - 1)

  // DS_AlgoDeleteNode removes the specified node from the tree.
  // Returns -1 if node or level is out of bounds, otherwise returns 0.


extern int DS_AlgoGetResult
  (int  node,          // In -  Cell column index (0..num nodes in level - 1)
   int  level,         // In -  Cell row index
                       //         Evidence   : 0
                       //         Hypothesis : 1..(Tree_Depth - 2)
                       //         Assessment : Tree_Depth - 1
   float  results[]);  // Out - Calculated results
                       //         [0] = Cell name
                       //         [1] = Cell's belief value
                       //         [2] = Cell's disbelief value
                       //         [3] = Cell's uncertainty value
                       //         [4] = Send time in seconds


extern int DS_AlgoGetInverse
  (int  node,          // In -  Cell column index (0..num nodes in level)
   int  level,         // In -  Cell row index
                       //         Evidence   : 0
                       //         Hypothesis : 1..(Tree_Depth - 2)
                       //         Assessment : Tree_Depth - 1
   float  results[]);  // Out - Calculated results
                       //         [0] = Cell name
                       //         [1] = Belief value of inverse
                       //         [2] = Disbelief value of inverse
                       //         [3] = Uncertainty value of inverse
                       //         [4] = Send time in seconds


extern int DS_AlgoGetUnfuse
  (int  node,          // In -  Cell column index (0..num nodes in level - 1)
   int  level,         // In -  Cell row index
                       //         Evidence   : 0
                       //         Hypothesis : 1..(Tree_Depth - 2)
                       //         Assessment : Tree_Depth - 1
   float  thresh,      // In -  Desired [B, U, D]
   float  results[]);  // Out - Calculated results
                       //         [0] = Cell name
                       //         [1] = Belief value to get thresh
                       //         [2] = Disbelief value to get thresh
                       //         [3] = Uncertainty value to get thresh
                       //         [4] = Send time in seconds


extern void DS_AlgoGetMatrix
  (int    Src_Ix,      // In -  Line-source column index
                       //         (0..num nodes in src level - 1)
   int    Src_Iy,      // In -  Line-source row index
                       //         Evidence   : 0
                       //         Hypothesis : 1..(Tree_Depth - 2)
                       //         Assessment : Tree_Depth - 1
   int    Dest_Ix,     // In -  Line-destinastion column index (see Src_Ix)
   int    Dest_Iy,     // In -  Line-destinastion row index (see Src_Iy)
   char*  In_String,   // Out - Parent node name
   char*  Out_String,  // Out - Child node name
   char*  B_String,    // Out - String describing belief value
   char*  D_String,    // Out - String describing disbelief value
   float  &B_Weight,   // Out - Line-source belief value
   float  &D_Weight);  // Out - Line-source disbelief value


extern int DS_AlgoChangeNodeValues
  (int    node,    // Index of node : 0..num nodes at this level of the tree
   int    level,   // Tree level the node is on : 0..Tree_Depth - 1
   float  new_B,   // New belief value want node to have : 0.0 .. 1.0
   float  new_D);  // New disbelief value want node to have : 0.0 .. 1.0

  // DS_AlgoChangeNodeValues sets the specified node's Desired-Belief
  // and -Disbelief to the parameter values.  Subsequent running of
  // DS_AlgoChangeImpacts_BP will alter various tree link weights to
  // make the node's actual values match these desired values.  This
  // function should be called before the Back Propogation function is called.


extern int DS_AlgoChangeImpacts_BP
  (int    Constraint_Level,  // Node impact constraints -- see above
   float  Learning_Rate,     // 0 < rate <= 1; greater is faster, problematic
   int    Max_Backpropogation_Levels);  // Num levels back to process

  // DS_AlgoChangeImpacts_BP allows the operator to override node belief
  // and disbelief values after evidence has been processed.  This
  // function uses the Backpropogation algorithm to adjust ancestor link
  // weights so that certain nodes have values specified by the operator
  // (via the DS_AlgoChangeNodeValues function).  On error (such as
  // parameter values being out of range) this function returns -1,
  // otherwise it returns the number of processing iterations taken to
  // achieve the desired results, up to 8000.


extern int DS_AlgoGetBPRunStatistics
  (DS_BP_Run_Statistics_Type  *runStats);  // OUT - Run statistics

  // DS_AlgoGetBPRunStatistics fills the runStats var with data
  // pertaining to the last backpropogation run.  Returns -1 is
  // there has not been a run yet, otherwise 0.


extern int DS_AlgoMergeTrees
  (int   mergeFunction,  // IN - Union or Intersection
   char  *structFile,    // IN - Name of merging tree's description file
   char  *dataFile);     // IN - Name of merging tree's data file

#endif  /* _DSB_ALGO_H */
