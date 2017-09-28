
/*
    DSalgo loop code not yet in place!!!

    (is finished for backPropogation though)
*/


// LOOPS:
//   Data should pass thru a loop once, as if
//     back links delete after data passes thru.
//   Back propogation changes all necessary
//     links once each iteration thru the tree.

//---- NEED TO SET RETURN VAL FOR DENNIS ------

//--- tho all parents' Out fused together might be in bounds,
//    In from single parent sometimes out of bounds
//--- When node's fuse val out of bounds, need to adjust links


/*
  B   means Belief
  D   means Disbelief
  U   means Uncertainity or Don't Know (1 - B - D)
  E   means Evid   (Evidence),   the first level, index 0
  H   means Hypot  (Hypotheses), the middle levels, indices 1...DS_treeDepth-2
  A   means Assoc  (Assessment), the last level, index DS_treeDepth-1
  Rec    means Record

  BP  means Backpropogation
  ch  means child
  par means parent
*/

/* Variables typed as "short" are booleans, take on values TRUE and FALSE. */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

#include "DSBalgo.h"


const int  FALSE = 0;
const int  TRUE  = 1;

const int  B = 0;
const int  D = 1;
const int  U = 2;

const int  MAX_EVIDENCE_INPUTS = 500;

const double  MAX_ERROR      = 0.0005;
const double  EPSLON         = 0.000001;
const double  TEN_EPSLON     = 0.00001;
const double  HUNDRED_EPSLON = 0.0001;


const char bdChar[2] = {'B', 'D'};


struct Cell_Rec {
  int     Cell_Name;          // Cell level * 100 + cell index within level
  int     level;              // Tree level node is on (0..DS_treeDepth-1)
  int     node;               // Node index on its tree level (0..levelWidths-1)
  double  In[3];              // Belief/Disbelief input
  double  Ext_Fused[3];       // External B cell inputs so far fused together
  double  Fused[3];           // Inherited cell inputs so far fused together
  double  Desired[2];         // User-overridden cell belief value
  double  delta[2];           // Amount of change in cell's B or D during BP
  double  EpocSecSnd;         // Time evidence message was sent
  double  EpocSecRcv;         // Currently not used
  short   changeImpactsOn;    // TRUE = change impacts on this node in BP
  short   beingUpdated;       // TRUE when has new data to be processed
  short   needsUpdate;        // TRUE when has new In to be fused
  short   deltaIsComputed[2]; // TRUE when backProp delta has been computed
}; 

struct Cell_Rec  nodes[DS_Max_Nodes];


struct {
  int      Node_Number;   // Index of evidence node
  int      Level_Number;  // Index of network level
  double   B;             // Evidence belief value
  double   D;             // Evidence disbelief value
  double   Time;          // Time evidence message was sent
} evidence[MAX_EVIDENCE_INPUTS];



short  Debug_Level = DS_Debug_None;
short  Collect_Evidence = TRUE;
short  Ran_Backpropogation = FALSE;

int    Num_Evidence_Inputs;

DS_BP_Run_Statistics_Type  BP_Run_Stats;


int    Process_EHA       (int);
int    Transfer_Data     (void);
int    Fuse_Cells        (int, short, short&);
void   Prt_Cell          (int);
int    Fuse_BUD          (double, double, double, double, int,
                          double&, double&, double&);
void   Reset_Node_Values (void);
void   Mark_BP_Nodes     (int, int, short);
void   Compute_Delta     (int, int, short);


/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_Algo (int node, int level,
             float New_B, float New_D, float Time_Entry)
{
  int  i;
  int  returnValue = 0;


  /* Enter name number, B, D, time & other values to nodes Array. */

  if (level >= DS_treeDepth)
  {
    fprintf(stderr, "DS_Algo: Specified Level (%d) too big.\n", level);
    return (DS_Err_LevelTooBig);
  }

  if (node >= DS_levelWidths[level])
  {
    fprintf(stderr, "DS_Algo: Specified Node No. (%d) too big.\n", node);
    return (DS_Err_NodeTooBig);
  }

//qqq
//need warning number for when input B,D out of bounds
//need error code for when inputs/weights result in out of bounds
  if ((New_B > 1.0) || (New_B < 0.0) ||(New_D > 1.0) || (New_D < 0.0))
  {
    fprintf(stderr, "DS_Algo: input value(s) (B = %f, D = %f) out of bounds.\n",
            New_B, New_D);
    return(DS_Err_Value_OutOfBounds);
  }

  if (New_B + New_D > 1.0)
  {
    fprintf(stderr, "DS_Algo: input B + D must be <= 1.0\n");
    return(DS_Err_Value_OutOfBounds);
  }

  i = DS_CoordToIndex(node, level);
  nodes[i].In[B] = New_B;
  nodes[i].In[D] = New_D;
  nodes[i].In[U] = 1 - New_B - New_D;
  nodes[i].EpocSecSnd = Time_Entry;
  nodes[i].EpocSecRcv = 0.0;
  nodes[i].needsUpdate = TRUE;

  if (Debug_Level >= DS_Debug_InOut)
  {
    printf ("New evidence data:\n");
    Prt_Cell (i);
  }

  if (Collect_Evidence && (Num_Evidence_Inputs < MAX_EVIDENCE_INPUTS))
  {
    evidence[Num_Evidence_Inputs].Node_Number = node;
    evidence[Num_Evidence_Inputs].Level_Number = level;
    evidence[Num_Evidence_Inputs].B = New_B;
    evidence[Num_Evidence_Inputs].D = New_D;
    evidence[Num_Evidence_Inputs].Time = Time_Entry;
    Num_Evidence_Inputs++;
  }

  /* Run an update to the EHA Process. */

  returnValue = Process_EHA(i);
  return returnValue;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int Process_EHA (int nodeWithNewData)
{
  short  Node_Has_Evid;
  short  Have_Evid;
  int    node;
  int    numIterations = 0;  // Number of (loop) iterations
  int    returnValue = 0;


//printf("FUSE node %d's data...\n", nodeWithNewData);fflush(stdout);
  if (Fuse_Cells (nodeWithNewData, TRUE, Have_Evid))
    returnValue = -1;

  while (Have_Evid && (numIterations++ < 8000))
  {
    Have_Evid = FALSE;

    /* The nodes in the layer are done,     */
    /* transfer results to the child layer. */
//printf("XFER data...\n");fflush(stdout);
    Transfer_Data();

    if (Debug_Level >= DS_Debug_Flow)
    {
      printf ("- - - - - - - - - - -\n\n");
      printf ("Fuse new input with old external input "
                "for each node in the tree:\n");
    }

    for (node = 0; node < DS_numNodes; node++)
    {
      /* Child nodes have been passed new data, fuse     *
       * that data with each child's own external input. */

//printf("FUSE node %d's data...\n", node);fflush(stdout);
      if (Fuse_Cells (node, FALSE, Node_Has_Evid))
        returnValue = -1;

      if (Node_Has_Evid)
        Have_Evid = TRUE;
    }
  }

  return returnValue;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int Transfer_Data (void)
{
  int       bd;                // B,U,D loop index
  int       chLevel, parLevel; // Tree level number of children
  int       parNode, chNode;   // Tree level's node index
  int       chIndex, parIndex; // Node indices into link array
  short     doingFirstParent;  // TRUE if working on child's first parent
  short     updateTheChild;    // TRUE if child has a parent with new data
  Cell_Rec  Hold_Cell;


  /* For each node in the child level, this function fuses together       *
   * data of all nodes in the parent level that impact that child node.   *
   * A parent's data is first multiplied by its impact weights for that   *
   * child, the result stored in Hold_Cell's 'In' fields.  This non-fused *
   * data is next fused with the fused weighted data from any previously  *
   * processed parent nodes, which resides in Hold_Cell's 'Fused' fields, *
   * and the result is stored in Hold_Cell's 'Fused' fields.  After all   *
   * parent nodes are processed, the cummulative fused data is copied     *
   * to the child node's 'In' fields.                                     */

  if (Debug_Level >= DS_Debug_Compute)
  {
    printf ("---------------------\n\n");
  }

  /* Mark nodes as needing update if they have a parent with new data. */

  for (parIndex = 0; parIndex < DS_numNodes; parIndex++)
  {
    if (!nodes[parIndex].beingUpdated)
      continue;

    for (chIndex = 0; chIndex < DS_numNodes; chIndex++)
    {
      if ((DS_links[B][chIndex][parIndex] != 0.0) ||
          (DS_links[D][chIndex][parIndex] != 0.0))
      {
        nodes[chIndex].needsUpdate = TRUE;
      }
    }
  }


  /* Transfer results to child layer. */

  /* For each node in tree... */
  for (chIndex = 0; chIndex < DS_numNodes; chIndex++)
  {
    /* If the node needs an update... */

    if (nodes[chIndex].needsUpdate)
    {
      Hold_Cell.Fused[B] = 0.0;
      Hold_Cell.Fused[U] = 0.0;
      Hold_Cell.Fused[D] = 0.0;

      doingFirstParent = TRUE;


      /* Fuse together the data of all the node's parents. */

      for (parIndex = 0; parIndex < DS_numNodes; parIndex++)
      {
        if ((DS_links[B][chIndex][parIndex] != 0.0) ||
            (DS_links[D][chIndex][parIndex] != 0.0))
        {
          /* Multiply the parent's data values by its  *
           * impact weights and store in Hold_Cell.In. */
    
          /* Note:  This does not test to ensure Fused[B] and Fused[D]   *
           *        when multiplied by B and D link weights produce 0.0  *
           *        which when added together is zero, so Fused[U] is    *
           *        1.0 -- total Uncertainity.                           */

          for (bd = B; bd <= D; bd++)
          {
            Hold_Cell.In[bd] = DS_links[bd][chIndex][parIndex] *
              nodes[parIndex].Fused[bd];
          }

          Hold_Cell.In[U] = 1.0 - Hold_Cell.In[B] - Hold_Cell.In[D];


          if (Debug_Level >= DS_Debug_Compute)
          {
            printf ("Get data from parent\n");
            printf ("  Type  ImpactTable[%d][%d] Value  X  "
                    "Parent(%d) Value  =  New Child(%d) Value\n",
                    chIndex, parIndex, parIndex, chIndex);

            for (bd = B; bd <= D; bd++)
            {
              printf ("    %c           % 5.3f                 % 5.3f"
                      "              % 5.3f\n",
                      bdChar[bd],
                      DS_links[bd][chIndex][parIndex], 
                      nodes[parIndex].Fused[bd],
                      Hold_Cell.In[bd]);
            }

            fflush(stdout);
          }


          /* If are processing this child node's first      *
           * parent, there is no fused data yet.  Just copy *
           * the weighted input data to the fused fields.   */

          if (doingFirstParent)
          {
            /* Initialize Hold_Cell */
            for (bd = B; bd <= U; bd++)
              Hold_Cell.Fused[bd] = Hold_Cell.In[bd];

            Hold_Cell.EpocSecSnd = nodes[parIndex].EpocSecSnd;
            doingFirstParent = FALSE;
          }
          else  /* Fuse new weighted inputs with previously fused data. */
          {
            if (Debug_Level >= DS_Debug_Flow)
            {
              printf("Fuse parent outputs together...\n");
              fflush(stdout);
            }
            Fuse_BUD
              (Hold_Cell.Fused[B], Hold_Cell.Fused[D],
               Hold_Cell.In[B], Hold_Cell.In[D],
               nodes[chIndex].Cell_Name,
               Hold_Cell.Fused[B], Hold_Cell.Fused[U], Hold_Cell.Fused[D]);
            
            if (Hold_Cell.EpocSecSnd < nodes[parIndex].EpocSecSnd)
            {
              Hold_Cell.EpocSecSnd = nodes[parIndex].EpocSecSnd;
            }
          }
        } // if node is a parent
      } // for each node


      /* Copy Hold_Cell's fused data to the child node's In *
       * field.  Enter time & other values to node cell.    */

      for (bd = B; bd <= U; bd++)
        nodes[chIndex].In[bd] = Hold_Cell.Fused[bd];

      nodes[chIndex].EpocSecRcv = 0.0;

      if (nodes[chIndex].EpocSecSnd < Hold_Cell.EpocSecSnd)
      {
        nodes[chIndex].EpocSecSnd = Hold_Cell.EpocSecSnd;
      }

      if (Debug_Level >= DS_Debug_Flow)
      {
        printf ("  New child data:\n");
        Prt_Cell (chIndex);
      }
    }  // if needsUpdate
  } // for chIndex


  /* Are finished modifying parent nodes. */
  for (parIndex = 0; parIndex < DS_numNodes; parIndex++)
  {
    nodes[parIndex].beingUpdated = FALSE;
  }

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int Fuse_Cells(int nodeIndex, short Is_External_Input, short &Have_Evid)
{
  int  bd;             // BUD looping index
  int  returnVal = 0;  // Indicate function's success


  Have_Evid = FALSE;

  /* Only process this node's data if there is new input. */
  if (nodes[nodeIndex].needsUpdate)
  {
    if (Is_External_Input)
    {
      /* Fuse the new external input with old fused  *
       * data (from parents and old external input). */

      if ((fabs(nodes[nodeIndex].Fused[B]) < EPSLON) &&
          (fabs(nodes[nodeIndex].Fused[D]) < EPSLON))
      { 
        /* Node's first input, just copy new input to fused. */
        for (bd = B; bd <= U; bd++)
          nodes[nodeIndex].Fused[bd] = nodes[nodeIndex].In[bd];
      }
      else
      { 
        /* Fuse the old fused input with the new external input. */

        if (Debug_Level >= DS_Debug_Flow)
        {
          printf("  Fuse new ext with current val...\n");
          fflush(stdout);
        }

        Fuse_BUD (nodes[nodeIndex].Fused[B],
                  nodes[nodeIndex].Fused[D],
                  nodes[nodeIndex].In[B],
                  nodes[nodeIndex].In[D],
                  nodes[nodeIndex].Cell_Name,
                  nodes[nodeIndex].Fused[B],
                  nodes[nodeIndex].Fused[U],
                  nodes[nodeIndex].Fused[D]);
      }


      /* For later fusing with new parent inputs, save the new external *
       * input by fusing it with previous external inputs (Ext_Fused)   */

      if ((fabs(nodes[nodeIndex].Ext_Fused[B]) < EPSLON) &&
          (fabs(nodes[nodeIndex].Ext_Fused[D]) < EPSLON))
      { 
        /* Node's first external input, just copy the new input to ext_fused. */
        for (bd = B; bd <= U; bd++)
        { 
          nodes[nodeIndex].Ext_Fused[bd] = nodes[nodeIndex].In[bd];
        }
      }
      else
      { 
        /* Fuse the old fused external input with the new external input. */

        if (Debug_Level >= DS_Debug_Flow)
        {
          printf("  Fuse new ext with old ext...\n");
          fflush(stdout);
        }

        Fuse_BUD (nodes[nodeIndex].Ext_Fused[B],
                  nodes[nodeIndex].Ext_Fused[D],
                  nodes[nodeIndex].In[B],
                  nodes[nodeIndex].In[D],
                  nodes[nodeIndex].Cell_Name,
                  nodes[nodeIndex].Ext_Fused[B],
                  nodes[nodeIndex].Ext_Fused[U],
                  nodes[nodeIndex].Ext_Fused[D]);
      }
    }
    else
    {
      /* If the cell hadn't previously received external input, *
       * just copy the input from parent nodes to the Fuse var. */
      if ((fabs(nodes[nodeIndex].Ext_Fused[B]) < EPSLON) &&
          (fabs(nodes[nodeIndex].Ext_Fused[D]) < EPSLON))
      { 
        for (bd = B; bd <= U; bd++)
          nodes[nodeIndex].Fused[bd] = nodes[nodeIndex].In[bd];
      }
      else
      { 
        /* Fuse the fused external B and D with the new B and D. */

        if (Debug_Level >= DS_Debug_Flow)
        {
          printf("  Fuse new input with old ext...\n");
          fflush(stdout);
        }

        Fuse_BUD (nodes[nodeIndex].Ext_Fused[B],
                  nodes[nodeIndex].Ext_Fused[D],
                  nodes[nodeIndex].In[B],
                  nodes[nodeIndex].In[D],
                  nodes[nodeIndex].Cell_Name,
                  nodes[nodeIndex].Fused[B],
                  nodes[nodeIndex].Fused[U],
                  nodes[nodeIndex].Fused[D]);
      }
    }


    if ((Debug_Level >= DS_Debug_Flow) ||
        ((Debug_Level >= DS_Debug_InOut) && (nodeIndex < DS_levelWidths[0])))
    {
      printf ("  Fused data:\n");
      Prt_Cell (nodeIndex);
    }


    if ((nodes[nodeIndex].Fused[B] > 1.0) || 
        (nodes[nodeIndex].Fused[B] < 0.0) || 
        (nodes[nodeIndex].Fused[D] > 1.0) || 
        (nodes[nodeIndex].Fused[D] < 0.0))
    {
      fprintf(stderr,
              "Fuse_Cells: Node[%d][%d]'s fused value(s) (B = %5.3e, "
              "D = %5.3e) is/are out of bounds.\n",
              nodes[nodeIndex].level,
              nodes[nodeIndex].node,
              nodes[nodeIndex].Fused[B],
              nodes[nodeIndex].Fused[D]);
      returnVal = -1;
    }
    else if (nodes[nodeIndex].Fused[B] +
             nodes[nodeIndex].Fused[D] > 1.0)
    {
      fprintf(stderr,
              "Fuse_Cells: Node[%d][%d]'s B (%5.3e) + D (%5.3e) > 1.0\n",
              nodes[nodeIndex].level,
              nodes[nodeIndex].node,
              nodes[nodeIndex].Fused[B],
              nodes[nodeIndex].Fused[D]);
      returnVal = -1;
    }


    /* Change the node from needing update to being updated. */
    nodes[nodeIndex].needsUpdate = FALSE;
    Have_Evid = nodes[nodeIndex].beingUpdated = TRUE;
  }

//printf("FuseCells -- returning %d, Have_Evid = %d\n", returnVal, Have_Evid);fflush(stdout);
  return returnVal;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

void Prt_Cell (int nodeIndex)
{
  printf ("    Name -- %4d\n"
          "      BUD_In         %5.3f   %5.3f   %5.3f\n"
          "      BUD_Ext_Fused  %5.3f   %5.3f   %5.3f\n"
          "      BUD_Fused      %5.3f   %5.3f   %5.3f\n",
          nodes[nodeIndex].Cell_Name,
          nodes[nodeIndex].In[B],
          nodes[nodeIndex].In[U],
          nodes[nodeIndex].In[D],
          nodes[nodeIndex].Ext_Fused[B],
          nodes[nodeIndex].Ext_Fused[U],
          nodes[nodeIndex].Ext_Fused[D],
          nodes[nodeIndex].Fused[B],
          nodes[nodeIndex].Fused[U],
          nodes[nodeIndex].Fused[D]);

  printf ("    New data set for %d ", nodes[nodeIndex].Cell_Name);

  if (nodes[nodeIndex].level == 0)
    printf ("Evidence, ");
  else if (nodes[nodeIndex].level == DS_treeDepth - 1)
    printf ("Assessment, ");
  else if (DS_treeDepth > 3)
    printf ("Hypotheses_%1d, ", nodes[nodeIndex].level);
  else
    printf ("Hypotheses, ");

  printf ("time is %6.1f sec.\n\n", nodes[nodeIndex].EpocSecSnd);
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int Fuse_BUD (double O_B, double O_D, double N_B, double N_D, int Name,
              double &F_B, double &F_U, double &F_D)
{
  int     Error_Found_Flag = FALSE;
  double  O_U, N_U;
  double  Norm;
  double  BxB, BxU, BxD, UxB, UxU, UxD, DxB, DxU, DxD;  // These sum to 1.0


  O_U = 1.0 - O_B - O_D;
  N_U = 1.0 - N_B - N_D;

  BxB = O_B * N_B;
  BxU = O_B * N_U;
  BxD = O_B * N_D;
  UxB = O_U * N_B;
  UxU = O_U * N_U;
  UxD = O_U * N_D;
  DxB = O_D * N_B;
  DxU = O_D * N_U;
  DxD = O_D * N_D;

  Norm = 1.0 - BxD - DxB;
  if (Norm < TEN_EPSLON)
  {
    /* Can happen if one is pure B and other is pure D.  Must decide how */
    /* to resolve, below is one way.  Cannot allow a divide by zero.     */
    F_B = 0.5;
    F_U = 0;
    F_D = 0.5;
  }
  else
  {
    F_B  = (BxB + BxU + UxB) / Norm;
    F_U  =        UxU        / Norm;
    F_D  = (DxD + UxD + DxU) / Norm;
/***
if (F_B < 0.0) printf("!  %d --> Bfused = %12.10f < 0  <-- !\n", Name, F_B);
if (F_B > 1.0) printf("!  %d --> Bfused = %12.10f > 1  <-- !\n", Name, F_B);
if (F_D < 0.0) printf("!  %d --> Dfused = %12.10f < 0  <-- !\n", Name, F_D);
if (F_D > 1.0) printf("!  %d --> Dfused = %12.10f > 1  <-- !\n", Name, F_D);
***/
  }

  if (Debug_Level >= DS_Debug_Compute)
  {
    printf ("    %4d           NEW\n", Name);
    printf ("            | %5.3f %5.3f %5.3f ", N_B, N_U, N_D);
    printf ("  Norm = 1.000 - %5.3f - %5.3f = %5.3f\n", DxB,BxD,Norm);
    printf ("       -----|------------------\n");
    printf ("    O  %5.3f| %5.3f %5.3f %5.3f ",O_B, BxB, BxU, BxD);
    printf ("  F_B  = (%5.3f + %5.3f + %5.3f)/%5.3f = %5.3f\n",
                                                  BxB, BxU, UxB, Norm, F_B);
    printf ("            |\n");
    printf ("    L  %5.3f| %5.3f %5.3f %5.3f ",O_U,UxB,UxU,UxD);
    printf ("  F_U  = (        %5.3f        )/%5.3f = %5.3f\n", UxU, Norm, F_U);
    printf ("            |\n");
    printf ("    D  %5.3f| %5.3f %5.3f %5.3f ",O_D,DxB,DxU,DxD);
    printf ("  F_D  = (%5.3f + %5.3f + %5.3f)/%5.3f = %5.3f\n",
                                                  DxD, DxU, UxD, Norm, F_D);
    printf ("       -----|------------------\n\n");
  }

   /* This section checks that the computed values equal 1.0. */
  if (fabs(F_B + F_U + F_D - 1.0)                     > HUNDRED_EPSLON  ||
      fabs(BxB+BxU+BxD+UxB+UxU+UxD+DxB+DxU+DxD - 1.0) > HUNDRED_EPSLON     )
    Error_Found_Flag = TRUE;

  return Error_Found_Flag;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoInit(char *fname, int verbose)
{
  int  nodeIndex;


  Debug_Level = verbose;
  Collect_Evidence = TRUE;


  /* Initialize general tree data (dimensions, link weights, etc.). */

  if (DS_InitAttributes(fname, verbose))
    return (-1);


  /* Initialize tree node data. */

  DS_AlgoReset();

  for (nodeIndex = 0; nodeIndex < DS_numNodes; nodeIndex++)
  {
    DS_IndexToCoord(nodeIndex, nodes[nodeIndex].node, nodes[nodeIndex].level);
    nodes[nodeIndex].Cell_Name = (nodes[nodeIndex].level + 1) * 100 +
      nodes[nodeIndex].node + 1;
  }

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

void Reset_Node_Values(void)
{
  for (int node = 0; node < DS_numNodes; node++)
  {
    nodes[node].beingUpdated    = FALSE;
    nodes[node].needsUpdate     = FALSE;
    nodes[node].Ext_Fused[B]    = 0.0;
    nodes[node].Ext_Fused[U]    = 1.0;
    nodes[node].Ext_Fused[D]    = 0.0;
    nodes[node].Fused[B]        = 0.0;
    nodes[node].Fused[U]        = 1.0;
    nodes[node].Fused[D]        = 0.0;
    nodes[node].EpocSecSnd      = -999999;
  }
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

void DS_AlgoReset(void)
{
//printf("IN DS_AlgoReset\n");fflush(stdout);
  Reset_Node_Values();

  for (int node = 0; node < DS_numNodes; node++)
  {
    nodes[node].Desired[B] = -1.0;
    nodes[node].Desired[D] = -1.0;
    nodes[node].changeImpactsOn = FALSE;
  }

  BP_Run_Stats.Num_Iterations = 0;
  BP_Run_Stats.Num_B_Weights_Changed = 0;
  BP_Run_Stats.Num_D_Weights_Changed = 0;
  BP_Run_Stats.Constraint_Level = 0;
  BP_Run_Stats.Learning_Rate = 0;
  BP_Run_Stats.B_Error = 0;
  BP_Run_Stats.D_Error = 0;
  BP_Run_Stats.L = 0;
  BP_Run_Stats.meanL = 0;

  Num_Evidence_Inputs = 0;
  Ran_Backpropogation = FALSE;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

void DS_AlgoPrintTree(void)
{
  int  level, node, chNode, bd;


  printf("Node Values:\n");

  for (level = 0; level < DS_treeDepth; level++)
  {
    for (bd = B; bd <= D; bd++)
    {
      for (node = 0; node < DS_levelWidths[level]; node++)
      {
        printf ("  % 8.6f", nodes[DS_CoordToIndex(node,level)].Fused[bd]);
      }

      printf ("\n");
    }

    printf ("\n");
  }


  printf("\nImpacts:\n");

  for (chNode = 0; chNode < DS_numNodes; chNode++)
  {
    for (bd = B; bd <= D; bd++)
    {
      printf ("%d-%c  ", chNode, bdChar[bd]);

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

int DS_AlgoAddNode(int node, int level, char *name, char *description,
                   double bThreshold, double tThreshold)
{
  int  i, bd;
  int  nodeIndex;


  if (DS_AddNode(node, level, name, description, bThreshold, tThreshold))
    return -1;


  /* Shift node data down one to make room for new node. */

  nodeIndex = DS_CoordToIndex(node, level);

  for (i = DS_numNodes; i > nodeIndex; i--)
  {
    nodes[i] = nodes[i - 1];
    if (nodes[i].level == level)
      nodes[i].node++;
  }


  /* Add new node. */

  nodes[nodeIndex].Cell_Name       = (level + 1) * 100 + node + 1;
  nodes[nodeIndex].node            = node;
  nodes[nodeIndex].level           = level;
  nodes[nodeIndex].In[B]           = 0.0;
  nodes[nodeIndex].In[D]           = 0.0;
  nodes[nodeIndex].In[U]           = 0.0;
  nodes[nodeIndex].Ext_Fused[B]    = 0.0;
  nodes[nodeIndex].Ext_Fused[U]    = 1.0;
  nodes[nodeIndex].Ext_Fused[D]    = 0.0;
  nodes[nodeIndex].Fused[B]        = 0.0;
  nodes[nodeIndex].Fused[U]        = 1.0;
  nodes[nodeIndex].Fused[D]        = 0.0;
  nodes[nodeIndex].Desired[B]      = -1.0;
  nodes[nodeIndex].Desired[D]      = -1.0;
  nodes[nodeIndex].EpocSecSnd      = -999999;
  nodes[nodeIndex].EpocSecRcv      = 0.0;
  nodes[nodeIndex].changeImpactsOn = FALSE;
  nodes[nodeIndex].beingUpdated    = FALSE;
  nodes[nodeIndex].needsUpdate     = FALSE;


    nodes[node].beingUpdated    = FALSE;
    nodes[node].needsUpdate     = FALSE;
    nodes[node].Ext_Fused[B]    = 0.0;
    nodes[node].Ext_Fused[U]    = 1.0;
    nodes[node].Ext_Fused[D]    = 0.0;
    nodes[node].Fused[B]        = 0.0;
    nodes[node].Fused[U]        = 1.0;
    nodes[node].Fused[D]        = 0.0;
    nodes[node].EpocSecSnd      = -999999;
  // Adjust saved evidence so continues to go to same nodes.

  for (i = 0; i < Num_Evidence_Inputs; i++)
  {
    if ((evidence[i].Level_Number == level) &&
        (evidence[i].Node_Number >= node))
    {
       evidence[i].Node_Number++;
    }
  }

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoDeleteNode(int node, int level)
{
  int  i, j, bd;
  int  nodeIndex;


  if (DS_DeleteNode(node, level))
    return -1;


  // Shift node data up one to delete node.

  nodeIndex = DS_CoordToIndex(node, level);

  for (i = node; i < DS_numNodes; i++)
  {
    nodes[i] = nodes[i + 1];
    if (nodes[i].level == level)
      nodes[i].node--;
  }


  // Adjust saved evidence so continues to go to same nodes.

  for (i = 0; i < Num_Evidence_Inputs; i++)
  {
    if (evidence[i].Level_Number == level)
    {
      if (evidence[i].Node_Number == node)
      {
        for (j = i; j < Num_Evidence_Inputs - 1; j++)
        {
          evidence[j] = evidence[j + 1];
          Num_Evidence_Inputs--;
        }
      }
      else if (evidence[i].Node_Number > node)
      {
         evidence[i].Node_Number--;
      }
    }
  }

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoGetResult(int node, int level, float results[])
{
  /*   node     input      Cell index (1..num nodes in the level)
   *   level    input      Return results wanted
   *                         Evidence   : 0
   *                         Hypothesis : 1..(DS_treeDepth - 2)
   *                         Assessment : DS_treeDepth - 1
   *   results  output     Calculated results
   */

  int  nodeIndex = DS_CoordToIndex(node, level);

  if ((nodeIndex < 0) || (nodeIndex >= DS_numNodes))
    return -1;

  results[0] = (double) nodes[nodeIndex].Cell_Name;
  results[1] =          nodes[nodeIndex].Fused[B];
  results[2] =          nodes[nodeIndex].Fused[D];
  results[3] =          nodes[nodeIndex].Fused[U];
  results[4] =          nodes[nodeIndex].EpocSecSnd;

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

void adjust(double *B, double *U, double *D)
{
  const float  MAX_EPSLON = TEN_EPSLON + (TEN_EPSLON / 2.0);
  const float  MIN_EPSLON = TEN_EPSLON - (TEN_EPSLON / 2.0);

  float  halfDiff;


  /* Want U > 0, so adjust B and D by lowering each by half of epsilon - U. */

  if (fabs(*B - (int) *B) < EPSLON)
    *B = (int) *B;
  if (fabs(*D - (int) *D) < EPSLON)
    *D = (int) *D;
  *U = 1.0 - *B - *D;

  if (*U < MIN_EPSLON)
  {
//printf("\nU = %12.10f  B = %12.10f  D = %12.10f\n", *U, *B, *D);
    while ((*U < MIN_EPSLON) || (*U > MAX_EPSLON))
    {
      halfDiff = (TEN_EPSLON - *U) / 2.0;

      if (*B < halfDiff)
        *D = *D - (TEN_EPSLON - *U);
      else if (*D < halfDiff)
        *B = *B - (TEN_EPSLON - *U);
      else
      {
        *B = *B - halfDiff;
        *D = *D - halfDiff;
      }

      *U = 1.0 - *B - *D;
//printf("  U = %12.10f  B = %12.10f  D = %12.10f\n", *U, *B, *D);
    }
  }
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoGetInverse(int node, int level, float results[])
{
    /*   node     input      Cell index (0..num nodes in the level - 1)
     *   level    input      Return results wanted
     *                         Evidence   : 0
     *                         Hypothesis : 1..(DS_treeDepth - 2)
     *                         Assessment : DS_treeDepth - 1
     *   results  output     Calculated results: inverse of node[i,j]
     *   return   output     Error code:
     *                          0 -> No error
     *                         -1 -> Divide by zero in Ainv
     *                         -2 -> Results don't match
     */

  int     irc;
  int     index;
  double  A[3];
  double  Ainv[3];


  irc = 0;


  /* Set up the A = [b u d] vector so that b + u + d = 1.0 and u > 0.0. */

  index = DS_CoordToIndex(node, level);
  A[B] = nodes[index].Fused[B];
  A[D] = nodes[index].Fused[D];
  A[U] = nodes[index].Fused[U];
  adjust(&A[B], &A[U], &A[D]);
  //printf("\n\nA    = [%5.3f  %5.3f  %5.3f]\n", A[B], A[U], A[D]);


  /* Compute the inverse of the vector. */

  Ainv[U] = 1.0 / (A[U] - (A[B] * A[D]) / (A[B] + A[U]) -
                   (A[B] * A[D]) / (A[D] + A[U]));

  /* Prevent divide by zero. */
  if (fabs(A[B] + A[U]) == 0.0)
  {
    printf("DS_AlgoGetInverse : Cannot compute Ainv for %f,%f,%f -- "
           "Ainv[U] = %f\n", A[B], A[U], A[D], Ainv[U]);
    return -1;
  }

  Ainv[B] = -A[B] / (A[B] + A[U]) * Ainv[U];
  Ainv[D] = -A[D] / (A[D] + A[U]) * Ainv[U];
  //printf("Ainv = [%5.3f  %5.3f  %5.3f]\n\n\n", Ainv[B], Ainv[U], Ainv[D]);


  results[0] = (double) nodes[index].Cell_Name;
  results[1] =          Ainv[B];
  results[2] =          Ainv[D];
  results[3] =          Ainv[U];
  results[4] =          nodes[index].EpocSecSnd;

  return(irc);
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoGetUnfuse(int node, int level, float thresh, float results[])
{
    /*   node     input      Cell index (0..num nodes in the level - 12)
     *   level    input      Return results wanted
     *                         Evidence   : 0
     *                         Hypothesis : 1..(DS_treeDepth - 2)
     *                         Assessment : DS_treeDepth - 1
     *   thresh   input      Threshold wanted
     *   results  output     Calculated results:
     *                         A fused with B = C.  Given C (thresh) and
     *                         A (nodes[node,level]), this program figures out
     *		               what B should be.
     *   return   output     Error code:
     *                          0 -> No error
     *                         -1 -> Divide by zero in Ainv
     *                         -2 -> Results don't match
     */

  int     irc;
  int     index;
  double  Avec[3];
  double  Bvec[3];
  double  Cvec[3];
  double  Ainv[3];
  double  AB[3];
  double  N;


  irc   = 0;

  Cvec[B] = thresh;
  Cvec[U] = 1.0 - thresh;
  Cvec[D] = 0;
  //printf("\n\nC = [%5.3f  %5.3f  %5.3f]\n\n\n", Cvec[B], Cvec[U], Cvec[D]);

  irc = DS_AlgoGetInverse(node, level, results);

  if (irc)
  {
    return irc;
  }

  Ainv[B] = results[1];
  Ainv[U] = results[3];
  Ainv[D] = results[2];
  //printf("I = [%5.3f  %5.3f  %5.3f]\n\n\n", Ainv[B], Ainv[U], Ainv[D]);


  /* Fuse Ainv with C to get B. */

  N = 1.0  -  Ainv[B] * Cvec[D]  -  Ainv[D] * Cvec[B];

  if ((N == 0.0) || (N == -0.0))
  {
    printf("DS_AlgoGetUnfuse : Cannot compute Bvec -- 1 - bD - dB == 0\n");
    return -1;
  }

  Bvec[B] = (Ainv[B] * Cvec[B]  +  Ainv[B] * Cvec[U]  +  Ainv[U] * Cvec[B]) / N;
  Bvec[D] = (Ainv[D] * Cvec[D]  +  Ainv[D] * Cvec[U]  +  Ainv[U] * Cvec[D]) / N;
  Bvec[U] = 1.0 - Bvec[B] - Bvec[D];
  //printf("B = [%5.3f  %5.3f  %5.3f]\n\n\n", Bvec[B], Bvec[U], Bvec[D]);


  /* Check result -- fuse A with B, result should be equal to C. */

  N     = 1.0  -  Avec[B] * Bvec[D]  -  Avec[D] * Bvec[B];
  AB[B] = (Avec[B] * Bvec[B]  +  Avec[B] * Bvec[U]  +  Avec[U] * Bvec[B]) / N;
  AB[D] = (Avec[D] * Bvec[D]  +  Avec[D] * Bvec[U]  +  Avec[U] * Bvec[D]) / N;
  AB[U] = 1.0 - AB[B] - AB[D];

  if ((fabs(AB[B] - Cvec[B]) > EPSLON) || (fabs(AB[U] - Cvec[U]) > EPSLON) ||
      (fabs(AB[D] - Cvec[D]) > EPSLON))
  {
    irc = -2;
  }

  index = DS_CoordToIndex(node, level);
  results[0] = (double) nodes[index].Cell_Name;
  results[1] =          Bvec[B];
  results[2] =          Bvec[D];
  results[3] =          Bvec[U];
  results[4] =          nodes[index].EpocSecSnd;

  return irc;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

void DS_AlgoGetMatrix(int Src_Node, int Src_Level,
                      int Dest_Node, int Dest_Level,
                      char* In_String, char* Out_String,
                      char* B_String, char* D_String,
                      float &B_Weight, float &D_Weight)
{
   /*   Src_Node    input   Line-source col index (0..num nodes in src level-1)
    *   Src_Level   input   Line-source row index
    *                         Evidence   : 0
    *                         Hypothesis : 1..(DS_treeDepth - 2)
    *                         Assessment : DS_treeDepth - 1
    *   Dest_Node   input   Line-destinastion column index
    *   Dest_Level  input   Line-destinastion row index
    *   In_String   output  Parent node name
    *   Out_String  output  Child node name
    *   B_String    output  String describing belief value
    *   D_String    output  String describing belief value
    *   B_Weight    output  Line-source belief value
    *   D_Weight    output  Line-source disbelief value
    */


  int  srcIndex, destIndex;

  /* Convert node coordinates to node indices. */
  srcIndex = DS_CoordToIndex(Src_Node, Src_Level);
  destIndex = DS_CoordToIndex(Dest_Node, Dest_Level);

  /* Get the strings of the line's end nodes. */
  strcpy(In_String, DS_nodeStrings[srcIndex]);
  strcpy(Out_String, DS_nodeStrings[destIndex]);

  /* Get the link weights. */
  B_Weight = DS_links[B][destIndex][srcIndex];
  D_Weight = DS_links[D][destIndex][srcIndex];

  /* Set the belief and disbelief strings based on the values. */
  DS_AlgoGetPossibility(B_Weight, B_String);
  DS_AlgoGetPossibility(D_Weight, D_String);
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoChangeNodeValues (int node, int level, float New_B, float New_D)
{
  if ((level < 0) || (level >= DS_treeDepth))
  {
    printf ("\n\nDS_AlgoChangeNodeValues:  Level %d out of range, "
            "no changes made.\n\n", level);
    return -1;
  }

  if ((node < 0) || (node >= DS_levelWidths[level]))
  {
    printf ("\n\nDS_AlgoChangeNodeValues:  node %d out of range, "
            "no changes made.\n\n", node);
    return -1;
  }

  if ((New_B + New_D > 1.0) || (New_B < 0.0) || (New_D < 0.0))
  {
    printf ("\n\nDS_AlgoChangeNodeValues:  New belief %5.3f and/or disbelief"
            " %5.3f values out of range, no changes made.\n\n", New_B, New_D);
    return -1;
  }

  int  i = DS_CoordToIndex(node, level);
  nodes[i].Desired[B] = New_B;
  nodes[i].Desired[D] = New_D;
int l, n;
DS_IndexToCoord(i, n, l);
printf("Override node[%d][%d]\n", l, n);
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

void Mark_BP_Nodes(int chIndex, int numLevels, short changeAnyLinks)
{
  int  parIndex;


  if (numLevels > 0)
  {
    for (parIndex = 0; parIndex < DS_numNodes; parIndex++)
    {
      if (DS_links[B][chIndex][parIndex] || DS_links[D][chIndex][parIndex] ||
          (changeAnyLinks &&
           (nodes[parIndex].level == nodes[chIndex].level - 1)))
      {
        nodes[parIndex].changeImpactsOn = TRUE;
if (Debug_Level >= DS_Debug_Compute)
{
printf("  - node[%d][%d]\n", nodes[parIndex].level, nodes[parIndex].node);
}
        Mark_BP_Nodes(parIndex, numLevels - 1, changeAnyLinks);
      }
    }
  }
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

void Compute_Delta(int par, int bd, short changeAnyLinks)
{
  int     ch;             // Loop index
  double  actualOutput;   // Actual node value
  double  sumOfChildren;


  sumOfChildren = 0.0;

  for (ch = 0; ch < DS_numNodes; ch++)
  {
    /* If the child is a back propogation node and  *
     * this node may link to it, add in it's delta. */

    if (nodes[ch].changeImpactsOn)
    {
      if (DS_links[bd][ch][par])
      {
        sumOfChildren += (nodes[ch].delta[bd] * DS_links[bd][ch][par]);
      }
      else if (changeAnyLinks &&
               (nodes[par].level == nodes[ch].level - 1))
      {
        sumOfChildren += (nodes[ch].delta[bd] * 0.5);
      }
    }
  } // for each child


  /* Compute this node's delta. */

  /* When A (actualOutput) is 0.0 or 1.0, A * (1 - A) is zero,    *
   * making the node's Delta always zero and change of impacts on *
   * the node impossible.  In this case, use the derivative of a  *
   * line, 1.0, instead of A * (1 - A) to allow impact change.    */

  actualOutput = nodes[par].Fused[bd];

  if ((fabs(actualOutput) < EPSLON) || (fabs(actualOutput) > (1.0 - EPSLON)))
  {
    nodes[par].delta[bd] = sumOfChildren;
  }
  else
  {
    nodes[par].delta[bd] = actualOutput * (1.0 - actualOutput) * sumOfChildren;
  }

  nodes[par].deltaIsComputed[bd] = TRUE;
printf("%d -- Set delta%c (%f) of node[%d][%d]\n", par, bdChar[bd], nodes[par].delta[bd],
 nodes[par].level, nodes[par].node);
fflush(stdout);


  if ((Debug_Level >= DS_Debug_Compute) &&
      (nodes[par].Desired[bd] != -1.0))
  {
    printf("Delta%c [%d][%d] = %f\n",
           bdChar[bd], nodes[par].level, nodes[par].node,
           nodes[par].delta[bd]);
  }
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoChangeImpacts_BP (int Constraint_Level, float Learning_Rate,
                             int Max_Backpropogation_Levels)
{
  short   Change_Any_Links;     // FALSE = change only existing links
  short   Bound_Weights;        // TRUE = link weights must range 0.0 to 1.0
  short   theresANodeNotComputed;
  short   aDeltaWasComputed;
  short   allChildrenHaveDeltas;

  int     returnValue;          // -1 on error encountered, else num iterations
  int     i, bd, ch, par, gpar; // Loop indexes
  int     bpLevel;              // Num levels back propogating data
  int     counter = 0;          // For debug, counts propogation iterations
  int     numChildren;
  int     numComputed;
  int     nodeToCompute;

  char    *str;                 // Character string to get environment variable

  double  New_Impact;           // Modified impact weight
  double  New_Error[2];         // Max diff in desired and actual BD values now
  double  Old_Error[2];         // Old max diff in desired and actual BD values
  double  actualOutput;         // Actual node value
  double  sumOfChildren;        // Sum of node's childrens' B or D values
  double  ratio;
  double  greatestRatio;
  double  error;                // Used to determine maximum errors of nodes
  double  change;               // Original link weight - new link weight
  double  origLinks[2][DS_Max_Nodes][DS_Max_Nodes];

  time_t  startTime, endTime;   // Used to measure run time length


  returnValue = 0;

  if (Constraint_Level < 0)
  {
    printf("DS_AlgoChangeImpacts_BP : Constraint level %d is out of range, "
           "resetting it to NONE\n");
    Constraint_Level = DS_Any_Unbounded_Constraint;
    returnValue = -1;
  }
  else if (Constraint_Level > 3)
  {
    printf("DS_AlgoChangeImpacts_BP : Constraint level %d is out of range, "
           "resetting it to FULL\n");
    Constraint_Level = DS_Existing_Bounded_Constraint;
    returnValue = -1;
  }

  if (Learning_Rate < 0.0)
  {
    printf("DS_AlgoChangeImpacts_BP : Learning rate %5.3f is out of range, "
           "resetting it to 0.0\n", Learning_Rate);
    Learning_Rate = 0.0;
    returnValue = -1;
  }
  else if (Learning_Rate > 1.0)
  {
    printf("DS_AlgoChangeImpacts_BP : Learning rate %5.3f is out of range, "
           "resetting it to 1.0\n", Learning_Rate);
    Learning_Rate = 1.0;
    returnValue = -1;
  }

  if (Max_Backpropogation_Levels < 1)
  {
    printf("DS_AlgoChangeImpacts_BP : Max backpropogation levels %d is out "
           "of range, resetting it to 1\n");
    Max_Backpropogation_Levels = 1;
    returnValue = -1;
  }

if (Debug_Level >= DS_Debug_Compute)
{
for (bd = B; bd <= D; bd++)
 for (i = 0; i < DS_numNodes; i++)
  if (nodes[i].Desired[bd] != -1.0)
   printf("%d -- Node's desired%c = %f\n", i, bdChar[bd], nodes[i].Desired[bd]);
fflush(stdout);
}

  if (Debug_Level >= DS_Debug_InOut)
  {
    for (i = 0; i < DS_numNodes; i++)
    {
      if ((nodes[i].Desired[B] != -1.0) ||
          (nodes[i].Desired[D] != -1.0))
      {
        printf ("\nOld %1d node values:  B = %f  D = %f",
                nodes[i].Cell_Name,
                nodes[i].Fused[B],
                nodes[i].Fused[D]);
      }
    }

    for (bd = B; bd <= D; bd++)
    {
      printf("\n\nOld %c impacts:\n", bdChar[bd]);

      for (ch = 0; ch < DS_numNodes; ch++)
      {
        for (par = 0; par < DS_numNodes; par++)
          printf ("  %8.3f", DS_links[bd][ch][par]);
        printf ("\n\n");
      }

      printf ("\n");
    }

    fflush(stdout);
  }


  /* Start the timer. */
  time (&startTime);


  /* Turn off new evidence collection, want to rerun evidence already have. */
  Collect_Evidence = FALSE;

  Change_Any_Links = FALSE;
  Bound_Weights = FALSE;

  if ((Constraint_Level == DS_Any_Unbounded_Constraint) ||
      (Constraint_Level == DS_Any_Bounded_Constraint))
    Change_Any_Links = TRUE;

  if ((Constraint_Level == DS_Existing_Bounded_Constraint) ||
      (Constraint_Level == DS_Any_Bounded_Constraint))
    Bound_Weights = TRUE;


  /* Remember original link weights for later check on changes. */
  for (ch = 0; ch < DS_numNodes; ch++)
  {
    for (par = 0; par < DS_numNodes; par++)
    {
      origLinks[B][ch][par] = DS_links[B][ch][par];
      origLinks[D][ch][par] = DS_links[D][ch][par];
    }
  }


  /* To allow propogating changes only a few levels *
   * up, mark nodes whose impacts may change.       */

  for (i = 0; i < DS_numNodes; i++)
  {
    for (bd = B; bd <= D; bd++)
    {
      nodes[i].deltaIsComputed[bd] = TRUE;
      nodes[i].delta[bd] = 0;
    }

    nodes[i].changeImpactsOn = FALSE;
  }

if (Debug_Level >= DS_Debug_Compute)
{
printf("\nBP Nodes:\n");
}
  for (i = 0; i < DS_numNodes; i++)
  {
    if ((nodes[i].Desired[B] != -1.0) ||
        (nodes[i].Desired[D] != -1.0))
    {
if (Debug_Level >= DS_Debug_Compute)
{
printf("  - node[%d][%d]\n", nodes[i].level, nodes[i].node);
}
      nodes[i].changeImpactsOn = TRUE;
      Mark_BP_Nodes(i, Max_Backpropogation_Levels - 1, Change_Any_Links);
    }
  }
if (Debug_Level >= DS_Debug_Compute)
{
fflush(stdout);
}


  /* Will want to quit processing when error is not changing much. *
   * Set the New_Error to the current maximum errors of all nodes. */

  for (bd = B; bd <= D; bd++)
  {
    Old_Error[bd] = New_Error[bd] = 0.0;

    for (i = 0; i < DS_numNodes; i++)
    {
      if (nodes[i].Desired[bd] != -1.0)
      {
        error = fabs (nodes[i].Fused[bd] - nodes[i].Desired[bd]);

        if (error > New_Error[bd])
          New_Error[bd] = error;
      }
    }
  }


  /* Gradually alter impact values until have achieved       *
   * desired node values, as long as node values are         *
   * changing useful amounts.  For cases when processing     *
   * goes on forever, halt processing after 8000 iterations. */

  while (((New_Error[B] > MAX_ERROR) || (New_Error[D] > MAX_ERROR)) &&
//-- Uncomment these 2 lines if you want to quit when error barely changes. --
//         ((fabs(New_Error[B] - Old_Error[B]) > 0.000001) ||
//          (fabs(New_Error[D] - Old_Error[D]) > 0.000001)) &&
         (counter < 8000))
  {
    counter++;
if (Debug_Level >= DS_Debug_Compute)
{
printf("\n-- %d --\n", counter); fflush(stdout);
}

    for (par = 0; par < DS_numNodes; par++)
    {
      if (nodes[par].changeImpactsOn)
      {
        nodes[par].deltaIsComputed[B] = FALSE;
        nodes[par].deltaIsComputed[D] = FALSE;
        nodes[par].delta[B] = 0;
        nodes[par].delta[D] = 0;
      }
    }


    for (bd = B; bd <= D; bd++)
    {
if (Debug_Level >= DS_Debug_Compute)
{
printf(" %c:\n", bdChar[bd]); fflush(stdout);
}
      theresANodeNotComputed = TRUE;

      while (theresANodeNotComputed)
      {
if (Debug_Level >= DS_Debug_Compute)
{
printf(" theresANodeNotComputed\n"); fflush(stdout);
}

        theresANodeNotComputed = FALSE;
        aDeltaWasComputed = FALSE;

        for (par = 0; par < DS_numNodes; par++)
        {
if (Debug_Level >= DS_Debug_Compute)
{
printf("\n node %d...\n", par); fflush(stdout);
}
          if (!nodes[par].deltaIsComputed[bd])
          {
if (Debug_Level >= DS_Debug_Compute)
{
printf("   Node's delta not computed yet, so look at it...\n"); fflush(stdout);
}
            /* If this node's value is being overridden,    *
             * compute its delta as  the difference between *
             * its current value and the desired value.     */

            if (nodes[par].Desired[bd] != -1.0)
            {
if (Debug_Level >= DS_Debug_Compute)
{
printf("Desired != -1, delta ia diff\n");fflush(stdout);
}

              actualOutput = nodes[par].Fused[bd];

              if (fabs (nodes[par].Desired[bd] - actualOutput) <= MAX_ERROR)
              {
                nodes[par].delta[bd] = 0.0;
              }
              else
              {
                /* When A (actualOutput) is 0.0 or 1.0, A * (1 - A) is   *
                 * zero, making the node's Delta always zero and change  *
                 * of impacts on the node impossible.  In this case, use *
                 * the derivative of a line, 1.0, instead of A * (1 - A) *
                 * to allow impact change.                               */

                if ((fabs(actualOutput) < EPSLON) ||
                    (fabs(actualOutput) > (1.0 - EPSLON)))
                {
                  nodes[par].delta[bd] = nodes[par].Desired[bd] - actualOutput;
                }
                else
                {
                  nodes[par].delta[bd] =
                    (nodes[par].Desired[bd] - actualOutput) *
                    actualOutput * (1.0 - actualOutput);
                }
              }
if (Debug_Level >= DS_Debug_Compute)
{
printf("%d -- Set delta%c (%f) of node[%d][%d]\n", par, bdChar[bd], nodes[par].delta[bd],
 nodes[par].level, nodes[par].node);
fflush(stdout);
}

              nodes[par].deltaIsComputed[bd] = TRUE;
              aDeltaWasComputed = TRUE;
            }

            /* Else this node's value is not being overridden.  Compute *
             * its delta as the sum of its childrens' deltas times the  *
             * weights of the links to those children.                  */

            else
            {
              allChildrenHaveDeltas = TRUE;

if (Debug_Level >= DS_Debug_Compute)
{
printf("Children that Change:\n");
}
              for (ch = 0; ch < DS_numNodes; ch++)
              {
                if (!nodes[ch].changeImpactsOn)
                  continue;

                if (DS_links[bd][ch][par] ||
                    (Change_Any_Links &&
                     (nodes[par].level == nodes[ch].level - 1)))
                {
if (Debug_Level >= DS_Debug_Compute)
{
printf("  %d\n", ch);
}
                  if (!nodes[ch].deltaIsComputed[bd])
                  {
if (Debug_Level >= DS_Debug_Compute)
{
printf("  Child's delta not computed, SKIP this node\n");
}
                    allChildrenHaveDeltas = FALSE;
                    break;
                  }
                }
              }

              if (allChildrenHaveDeltas)
              {
if (Debug_Level >= DS_Debug_Compute)
{
printf("All children have deltas\n");fflush(stdout);
}
                Compute_Delta(par, bd, Change_Any_Links);
                aDeltaWasComputed = TRUE;
              }
              else
                theresANodeNotComputed = TRUE;
            }
          }
        }

        /* If no node's delta could be computed because all those left  *
         * have children without deltas, those nodes are in loops.      *
         * Find which uncomputed node has the greatest ratio of number  *
         * of computed children, then compute that node's delta so far. */

        if (theresANodeNotComputed && !aDeltaWasComputed)
        {
if (Debug_Level >= DS_Debug_Compute)
{
printf("\n-------LOOP-------\n");fflush(stdout);
}
          nodeToCompute = -1;
          greatestRatio = 0.0;

          for (par = 0; par < DS_numNodes; par++)
          {
            if (!nodes[par].deltaIsComputed[bd])
            {
              if (nodeToCompute == -1)
                nodeToCompute = par;

              numChildren = 0;
              numComputed = 0;

              for (ch = 0; ch < DS_numNodes; ch++)
              {
                if (!nodes[ch].changeImpactsOn)
                  continue;

                if (DS_links[bd][ch][par] ||
                    (Change_Any_Links &&
                     (nodes[par].level == nodes[ch].level - 1)))
                {
                  numChildren++;

                  if (nodes[ch].deltaIsComputed[bd])
                    numComputed++;
                }
              }

              ratio = numComputed / numChildren;
if (Debug_Level >= DS_Debug_Compute)
{
printf("\n\nttt-----> ratio = %d / %d = %f <-----ttt\n\n", numComputed, numChildren, ratio);
fflush(stdout);
}

              if (ratio > greatestRatio)
              {
                greatestRatio = ratio;
                nodeToCompute = par;
              }
            }
          }

          Compute_Delta(nodeToCompute, bd, Change_Any_Links);
        }
      }


      /* Change the weight of the links to each node. */

      for (ch = 0; ch < DS_numNodes; ch++)
      {
        if (!nodes[ch].changeImpactsOn)
          continue;

        for (par = 0; par < DS_numNodes; par++)
        {
          /* If the child is linked to this node, or *
           * if we're allowed to change any link...  */

          if (DS_links[bd][ch][par] ||
              (Change_Any_Links && (nodes[par].level == nodes[ch].level - 1)))
          {
            New_Impact = DS_links[bd][ch][par] +
              Learning_Rate * nodes[ch].delta[bd] * nodes[par].Fused[bd];

            /* Do any necessary weight bounding. */
/*****{
// This code enforces lower bound of 0 no matter what:
            if ((New_Impact > 1.0) && Bound_Weights)
              New_Impact = 1.0;
            else if (New_Impact < 0.0)
              New_Impact = 0.0;
}*****/
// This code allows negative weights:
            if (Bound_Weights)
            {
              if (New_Impact > 1.0)
                New_Impact = 1.0;
              else if (New_Impact < 0.0)
                New_Impact = 0.0;
            }

            if ((Debug_Level >= DS_Debug_Compute) &&
                (DS_links[bd][ch][par] != New_Impact))
            {
              printf("\n%c Impact [%d][%d] = %f + %f * %f * %f = %f\n",
                     bdChar[bd], ch, par,
                     DS_links[bd][ch][par], Learning_Rate,
                     nodes[ch].delta[bd], nodes[par].Fused[bd], New_Impact);
              fflush(stdout);
            }

            DS_links[bd][ch][par] = New_Impact;
          }
        } // for par
      } // for ch
    } // for BD


    /* Repropogate evidence inputs through tree. */

    if (Debug_Level >= DS_Debug_Flow)
    {
      printf("\nRerunning evidence inputs through modified tree...\n\n");
    }

    Reset_Node_Values();

    for (i = 0; i < Num_Evidence_Inputs; i++)
    {
      DS_Algo (evidence[i].Node_Number, evidence[i].Level_Number,
	       evidence[i].B, evidence[i].D, evidence[i].Time);
    }


    /* Determine the new maximum B and D errors of all tree nodes. */

    for (bd = B; bd <= D; bd++)
    {
      Old_Error[bd] = New_Error[bd];
      New_Error[bd] = 0;

      for (i = 0; i < DS_numNodes; i++)
      {
        if (nodes[i].Desired[bd] != -1.0)
        {
          error = fabs (nodes[i].Fused[bd] - nodes[i].Desired[bd]);

          if (error > New_Error[bd])
            New_Error[bd] = error;
        }
      }
    }
  } // while error is big


  time (&endTime);

  if (returnValue == 0)
    returnValue = counter;


  BP_Run_Stats.L = BP_Run_Stats.meanL = 0;
  BP_Run_Stats.Num_B_Weights_Changed = BP_Run_Stats.Num_D_Weights_Changed = 0;

  for (bd = B; bd <= D; bd++)
  {
    if (Debug_Level >= DS_Debug_InOut)
    {
      printf("\n\nNew %c impacts:\n", bdChar[bd]); 
    }

    for (ch = 0; ch < DS_numNodes; ch++)
    {
      for (par = 0; par < DS_numNodes; par++)
      {
        if (Debug_Level >= DS_Debug_InOut)
        {
          printf ("  %8.3f", DS_links[bd][ch][par]);
        }

        change = origLinks[bd][ch][par] - DS_links[bd][ch][par];

        if (change)
        {
          if (bd == B)
            BP_Run_Stats.Num_B_Weights_Changed++;
          else
            BP_Run_Stats.Num_D_Weights_Changed++;

          BP_Run_Stats.L += (change * change);
          BP_Run_Stats.meanL += fabs (change);
        }
      }

      if (Debug_Level >= DS_Debug_InOut)
      {
        printf ("\n\n");
      }
    }

    if (Debug_Level >= DS_Debug_InOut)
    {
      printf ("\n");
    }
  }

  if (BP_Run_Stats.Num_B_Weights_Changed ||
      BP_Run_Stats.Num_D_Weights_Changed)
  {
    BP_Run_Stats.L = sqrt(BP_Run_Stats.L);
    BP_Run_Stats.meanL = BP_Run_Stats.meanL /
      (BP_Run_Stats.Num_B_Weights_Changed +
       BP_Run_Stats.Num_D_Weights_Changed);
  }

//////////////////////  if (Debug_Level >= DS_Debug_InOut)
  {
    printf ("\nLink weight perturbation = %f\n", BP_Run_Stats.L);
    printf ("Mean link weight perturbation = %f\n", BP_Run_Stats.meanL);

    for (i = 0; i < DS_numNodes; i++)
    {
      printf ("\nNew %1d node values:  B = %f  D = %f",
              nodes[i].Cell_Name,
              nodes[i].Fused[B],
              nodes[i].Fused[D]);
    }

    printf("\n\nB_error = %f   D_error = %f\n\n", New_Error[B], New_Error[D]);
    printf ("To make back-propogation impact changes, it took:"
            "\n  %f seconds\n  %d iterations\n",
            difftime (endTime, startTime), counter);

    fflush(stdout);
  }

  BP_Run_Stats.Constraint_Level = Constraint_Level;
  BP_Run_Stats.Learning_Rate = Learning_Rate;
  BP_Run_Stats.Num_Iterations = counter;
  BP_Run_Stats.B_Error = New_Error[B];
  BP_Run_Stats.D_Error = New_Error[D];

  Collect_Evidence = TRUE;
  Ran_Backpropogation = TRUE;
  return returnValue;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoGetBPRunStatistics (DS_BP_Run_Statistics_Type  *runStats)
{
  runStats->Num_Iterations = BP_Run_Stats.Num_Iterations;
  runStats->Num_B_Weights_Changed = BP_Run_Stats.Num_B_Weights_Changed;
  runStats->Num_D_Weights_Changed = BP_Run_Stats.Num_D_Weights_Changed;
  runStats->Constraint_Level = BP_Run_Stats.Constraint_Level;
  runStats->Learning_Rate = BP_Run_Stats.Learning_Rate;
  runStats->B_Error = BP_Run_Stats.B_Error;
  runStats->D_Error = BP_Run_Stats.D_Error;
  runStats->L = BP_Run_Stats.L;
  runStats->meanL = BP_Run_Stats.meanL;

  return (Ran_Backpropogation ? 0 : -1);
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int DS_AlgoMergeTrees (int mergeFunction, char* structFile, char* dataFile)
{
  int  i, level, node;
  int  returnValue;   // 0 on success


  // DSattributes's merge function  needs to know the current
  // node values, so copy them to DSattribute's rec.

  for (i = 0; i < DS_numNodes; i++)
  {
    DS_nodeValues[B][i] = nodes[i].Fused[B];
    DS_nodeValues[D][i] = nodes[i].Fused[D];
  }

  returnValue = (DS_MergeTrees (mergeFunction, structFile, dataFile));


  // Make sure DSalgo's node data matches DSattrubutes' data.

  DS_AlgoReset();

  level = 0;
  node = 0;

  for (i = 0; i < DS_numNodes; i++)
  {
    nodes[i].Cell_Name = (level + 1) * 100 + node + 1;
    nodes[i].node      = node;
    nodes[i].level     = level;
    nodes[i].In[B]     = 0.0;
    nodes[i].In[D]     = 0.0;
    nodes[i].In[U]     = 1.0;

    if (node == DS_levelWidths[level] - 1)
    {
      node = 0;
      level++;
    }
  }

  return returnValue;
}

