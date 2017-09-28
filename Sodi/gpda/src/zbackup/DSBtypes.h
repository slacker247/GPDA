
/* Creates, types, and holds the variables used by both the D_S_Driver and the
        Dempster-Shafer algorithm, d_s_algo.c.  */

/* E means Evid   means Evidence,   the first level, index Tree_Depth
   H means Hypot  means Hypotheses, the middle levels, indices 2...Tree_Depth-1
   A means Assoc  means Assessment, the last level, index 1
   B means Belief
   D means Disbelief
   U means Uncertainity or Don't Know (1 - B - D)
   Rec            means Record
   Group          the cells for any given type and level
*/

/* Variables typed as "short" are booleans, take on values True and False. */

#define True              1
#define False             0
#define Cell_Max          101  /* only 100 cells in a group allowed  */
#define Max_Depth         13   /* 12 hierarchy levels (E, Hs, A), 0 not used  */
#define Max_Width         13   /* 12 nodes, 0 not used  */
#define Max_String_Length 128  /* Up to 127 characters */
#define Max_Possibilities 9    /* 9 possibility value ranges */

#define Epslon   0.000001


struct Cell_Rec { int     Cell_Name;
                  float   B_In;
                  float   D_In;
                  float   U_In;
                  float   B_Fused;
                  float   D_Fused;
                  float   U_Fused; 
                  float   EpocSecSnd;
                  float   EpocSecRec;       /* Currently not used  */
                  short   B_Retransmit;     /* Currently not used  */
                  short   D_Retransmit; };  /* Currently not used  */

struct Group_Cell_Rec { struct Cell_Rec   Cell_Info[Cell_Max];
                        int               Group_Count;         };

extern struct Group_Cell_Rec  Group_Rec_Array[Max_Width][Max_Depth];

extern int  Tree_Depth;
extern int  Level_Widths[Max_Depth];


void DS_Algo
  (int    Num_O_Name,  /* In - Evidence node index (1 ... num Evidence nodes) */
   float  New_B,       /* In - Belief value of new Evid data (0.0 ... 1.0)    */
   float  New_D,       /* In - Disbelief value of new Evid data (0.0 ... 1.0) */
   float  Time_Entry); /* In - Timestamp of new Evidence data                 */
/* NOTE: DS_AlgoInit must be called before DS_Algo is called! */


int DS_AlgoInit
  (char  *fname,    /* In - Name of Attribute file */
   int   verbose);  /* In - True (1) = debug output on, False (0) = off */


void DS_AlgoGetResult
  (int  ix,            /* In -  Cell column index (1..num nodes in level) */
   int  iy,            /* In -  Cell row index                            *
                        *         Evidence   : Tree_Depth                 *
                        *         Hypothesis : 2..(Tree_Depth - 1)        *
                        *         Assessment : 1                          */
   float  results[]);  /* Out - Calculated results                        *
                        *         [0] = Cell name                         *
                        *         [1] = Cell's belief value               *
                        *         [2] = Cell's disbelief value            *
                        *         [3] = Cell's uncertainty value          *
                        *         [4] = Send time in seconds              */


void DS_AlgoGetMatrix
  (int    Src_Ix,      /* In -  Line-source column index                    *
                        *         (1..num nodes in src level)               */
   int    Src_Iy,      /* In -  Line-source row index                       *
                        *         Evidence   : Tree_Depth                   *
                        *         Hypothesis : 2..(Tree_Depth - 1)          *
                        *         Assessment : 1                            */
   int    Dest_Ix,     /* In -  Line-destinastion column index (see Src_Ix) */
   int    Dest_Iy,     /* In -  Line-destinastion row index (see Src_Iy)    */
   char*  In_String,   /* Out - Parent node name                            */
   char*  Out_String,  /* Out - Child node name                             */
   char*  B_String,    /* Out - String describing belief value              */
   char*  D_String,    /* Out - String describing belief value              */
   float  &B_Weight,   /* Out - Line-source belief value                    */
   float  &D_Weight);  /* Out - Line-source disbelief value                 */


int DS_AlgoGetTreeDepth (void);  /* Returns number of node levels in tree */


int DS_AlgoGetLevelWidth  /* Returns number of nodes in given level of tree */
  (int  level);  /* In - Tree level index (1..Tree_Depth) */
