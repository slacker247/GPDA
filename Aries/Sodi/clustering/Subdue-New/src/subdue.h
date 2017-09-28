//---------------------------------------------------------------------------
// subdue.h
//
// Data type and prototype definitions for the Subdue system.
//
// Subdue, version 5.0
//---------------------------------------------------------------------------

#ifndef SUBDUE_H
#define SUBDUE_H

#include <limits.h>
#include <float.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SUBDUE_VERSION "5.0.6"

// Substructure evaluation methods
#define EVAL_MDL      1
#define EVAL_SIZE     2
#define EVAL_SETCOVER 3

// Graph match search space limited to V^MATCH_SEARCH_THRESHOLD_EXPONENT
#define MATCH_SEARCH_THRESHOLD_EXPONENT 2.0

// Starting strings for input files
#define SUB_TOKEN        "S"  // new substructure
#define PREDEF_SUB_TOKEN "PS" // new predefined substructure
#define POS_EG_TOKEN     "XP" // new positive example
#define NEG_EG_TOKEN     "XN" // new negative example

// Vertex and edge labels used for graph compression
#define SUB_LABEL_STRING     "SUB"
#define OVERLAP_LABEL_STRING "OVERLAP"
#define PREDEFINED_PREFIX    "PS"

// Costs of various graph match transformations
#define INSERT_VERTEX_COST             1.0 // insert vertex
#define DELETE_VERTEX_COST             1.0 // delete vertex
#define SUBSTITUTE_VERTEX_LABEL_COST   1.0 // substitute vertex label
#define INSERT_EDGE_COST               1.0 // insert edge
#define INSERT_EDGE_WITH_VERTEX_COST   1.0 // insert edge with vertex
#define DELETE_EDGE_COST               1.0 // delete edge
#define DELETE_EDGE_WITH_VERTEX_COST   1.0 // delete edge with vertex
#define SUBSTITUTE_EDGE_LABEL_COST     1.0 // substitute edge label
#define SUBSTITUTE_EDGE_DIRECTION_COST 1.0 // change directedness of edge
#define REVERSE_EDGE_DIRECTION_COST    1.0 // change direction of directed edge

// Constants for graph matcher.  Special vertex mappings use the upper few
// unsigned long integers.  This assumes graphs will never have this many
// vertices, which is a pretty safe assumption.  The maximum double is used
// for initial costs.
#define MAX_UNSIGNED_LONG ULONG_MAX  // ULONG_MAX defined in limits.h
#define VERTEX_UNMAPPED   MAX_UNSIGNED_LONG
#define VERTEX_DELETED    MAX_UNSIGNED_LONG - 1
#define MAX_DOUBLE        DBL_MAX    // DBL_MAX from float.h

// Label types
#define STRING_LABEL  0
#define NUMERIC_LABEL 1

// General defines
#define LIST_SIZE_INC  10  // initial size and increment for realloc-ed lists
#define TOKEN_LEN     256  // maximum length of token from input graph file
#define FILE_NAME_LEN 128  // maximum length of file names
#define COMMENT       '%'  // comment character for input graph file
#define NUMERIC_OUTPUT_PRECISION 6
#define LOG_2 0.6931471805599452862 // log_e(2) pre-computed

#define SPACE ' '
#define TAB   '\t'
#define NEWLINE '\n'
#define DOUBLEQUOTE '\"'

#define FALSE 0
#define TRUE  1

//---------------------------------------------------------------------------
// Type Definitions
//---------------------------------------------------------------------------

typedef unsigned char UCHAR;
typedef unsigned char BOOLEAN;
typedef unsigned long ULONG;

// Label
typedef struct {
  UCHAR labelType;       // one of STRING_LABEL or NUMERIC_LABEL
  union {
    char *stringLabel;
    double numericLabel;
  } labelValue;
} Label;

// Label list
typedef struct {
  ULONG size;      // Number of label slots currently allocated in array
  ULONG numLabels; // Number of actual labels stored in list
  Label *labels;   // Array of labels
} LabelList;

// Edge
typedef struct {
  ULONG   vertex1;  // source vertex index into vertices array
  ULONG   vertex2;  // target vertex index into vertices array
  ULONG   label;    // index into label list of edge's label
  BOOLEAN directed; // TRUE is edge is directed
  BOOLEAN used;     // flag for marking edge used at various times
                    //   used flag assumed FALSE, so always reset when done
} Edge;

// Vertex
typedef struct {
  ULONG label;    // index into label list of vertex's label
  ULONG numEdges; // number of edges defined using this vertex
  ULONG *edges;   // indices into edge array of edges using this vertex
  ULONG map;      // used to store mapping of this vertex to corresponding
                  //   vertex in another graph
  BOOLEAN used;   // flag for marking vertex used at various times
                  //   used flag assumed FALSE, so always reset when done
} Vertex;

// Graph
typedef struct {
  ULONG  numVertices; // number of vertices in graph
  ULONG  numEdges;    // number of edges in graph
  Vertex *vertices;   // array of graph vertices
  Edge   *edges;      // array of graph edges
} Graph;

// Instance
typedef struct {
  ULONG numVertices;   // number of vertices in instance
  ULONG numEdges;      // number of edges in instance
  ULONG *vertices;     // ordered indices of instance's vertices in graph
  ULONG *edges;        // ordered indices of instance's edges in graph
  double minMatchCost; // lowest cost so far of matching this instance to
                       // a substructure
  ULONG newVertex;     // index into vertices array of newly added vertex
                       //    (0 means no new vertex added)
  ULONG newEdge;       // index into edges array of newly added edge
  ULONG refCount;      // counter of references to this instance; if zero,
                       //    then can be deallocated
} Instance;

// InstanceListNode: node in singly-linked list of instances
typedef struct _instance_list_node {
  Instance *instance;
  struct _instance_list_node *next;
} InstanceListNode;

// InstanceList: singly-linked list of instances
typedef struct {
  InstanceListNode *head;
} InstanceList;

// Substructure
typedef struct {
  Graph  *definition;         // graph definition of substructure
  ULONG  numInstances;        // number of positive instances
  InstanceList *instances;    // instances in positive graph
  ULONG  numNegInstances;     // number of negative instances
  InstanceList *negInstances; // instances in negative graph
  double value;               // value of substructure
} Substructure;

// SubListNode: node in singly-linked list of substructures
typedef struct _sub_list_node {
  Substructure *sub;
  struct _sub_list_node *next;
} SubListNode;

// SubList: singly-linked list of substructures
typedef struct {
  SubListNode *head;
} SubList;

// VertexMap: vertex to vertex mapping for graph match search
typedef struct {
  ULONG v1;
  ULONG v2;
} VertexMap;

// MatchHeapNode: node in heap for graph match search queue
typedef struct {
  ULONG  depth; // depth of node in search space (number of vertices mapped)
  double cost;  // cost of mapping
  VertexMap *mapping;
} MatchHeapNode;

// MatchHeap: heap of match nodes
typedef struct {
  ULONG size;      // number of nodes allocated in memory
  ULONG numNodes;  // number of nodes in heap
  MatchHeapNode *nodes;
} MatchHeap;

// Parameters: parameters used throughout Subdue system
typedef struct {
  char inputFileName[FILE_NAME_LEN];   // main input file
  char psInputFileName[FILE_NAME_LEN]; // predefined substructures input file
  char outFileName[FILE_NAME_LEN];     // file for machine-readable output
  Graph *posGraph;      // Graph of positive examples
  Graph *negGraph;      // Graph of negative examples
  double posGraphDL;    // Description length of positive input graph
  double negGraphDL;    // Description length of negative input graph
  ULONG numPosEgs;      // Number of positive examples
  ULONG numNegEgs;      // Number of negative examples
  ULONG *posEgsVertexIndices; // vertex indices of where positive egs begin
  ULONG *negEgsVertexIndices; // vertex indices of where negative egs begin
  LabelList *labelList; // List of unique labels in input graph(s)
  Graph **preSubs;      // Array of predefined substructure graphs
  ULONG numPreSubs;     // Number of predefined substructures read in
  BOOLEAN predefinedSubs; // TRUE is predefined substructures given
  BOOLEAN outputToFile; // TRUE if file given for machine-readable output
  BOOLEAN directed;     // If TRUE, 'e' edges treated as directed
  ULONG beamWidth;      // Limit on size of substructure queue (> 0)
  ULONG limit;          // Limit on number of substructures expanded (> 0)
  ULONG maxVertices;    // Maximum vertices in discovered substructures
  ULONG minVertices;    // Minimum vertices in discovered substructures
  ULONG numBestSubs;    // Limit on number of best substructures
                        //   returned (> 0)
  BOOLEAN valueBased;   // If TRUE, then queues are trimmed to contain
                        //   all substructures with the top beamWidth
                        //   values; otherwise, queues are trimmed to
                        //   contain only the top beamWidth substructures.
  BOOLEAN prune;        // If TRUE, then expanded substructures with lower
                        //   value than their parents are discarded.
  ULONG outputLevel;    // More screen (stdout) output as value increases
  BOOLEAN allowInstanceOverlap; // Default is FALSE; if TRUE, then instances
                                // may overlap, but compression costlier
  ULONG evalMethod;     // One of EVAL_MDL (default), EVAL_SIZE or
                        //   EVAL_SETCOVER
  double threshold;     // Percentage of size by which an instance can differ
                        // from the substructure definition according to
                        // graph match transformation costs
  ULONG iterations;     // Number of Subdue iterations; if more than 1, then
                        // graph compressed with best sub between iterations
  double *log2Factorial;   // Cache array A[i] = lg(i!); grows as needed
  ULONG log2FactorialSize; // Size of log2Factorial array
} Parameters;


//---------------------------------------------------------------------------
// Function Prototypes
//---------------------------------------------------------------------------

// compress.c

Graph *CompressGraph (Graph *, InstanceList *, Parameters *);
void AddOverlapEdges (Graph *, Graph *, InstanceList *, ULONG);
Edge *AddOverlapEdge (Edge *, ULONG *, ULONG, ULONG, ULONG);
Edge *AddDuplicateEdges (Edge *, ULONG *, Edge *, Graph *, ULONG, ULONG);
void CompressFinalGraphs (Substructure *, Parameters *, ULONG, BOOLEAN);
void CompressLabelListWithGraph (LabelList *, Graph *, Parameters *);
ULONG SizeOfCompressedGraph (Graph *, InstanceList *, Parameters *);
ULONG NumOverlapEdges (Graph *, InstanceList *);
void RemovePosEgsCovered (Substructure *, Parameters *);
void MarkExample (ULONG, ULONG, Graph *, BOOLEAN);
void CopyUnmarkedGraph (Graph *, Graph *, ULONG);
void CompressWithPredefinedSubs (Parameters *);

// discover.c

SubList *DiscoverSubs (Parameters *);
SubList *GetInitialSubs (Parameters *);
BOOLEAN SinglePreviousSub (Substructure *, Parameters *);

// dot.c

void WriteGraphToDotFile (char *, Parameters *);

void WriteGraphWithInstancesToDotFile (char *, Graph *, InstanceList *,
				       Parameters *);
void WriteSubsToDotFile (char *, Graph **, ULONG, Parameters *);
void WriteVertexToDotFile (FILE *, ULONG, ULONG, Graph *, LabelList *, char *);
void WriteEdgeToDotFile (FILE *, ULONG, ULONG, Graph *, LabelList *, char *);

// evaluate.c

void EvaluateSub (Substructure *, Parameters *);
ULONG GraphSize (Graph *);
double MDL (Graph *, ULONG, Parameters *);
ULONG NumUniqueEdges (Graph *, ULONG);
ULONG MaxEdgesToSingleVertex (Graph *, ULONG);
double ExternalEdgeBits (Graph *, Graph *, ULONG);
double Log2Factorial (ULONG, Parameters *);
double Log2 (ULONG);
ULONG PosExamplesCovered (Substructure *, Parameters *);
ULONG NegExamplesCovered (Substructure *, Parameters *);
ULONG ExamplesCovered (InstanceList *, Graph *, ULONG, ULONG *);

// extend.c

SubList *ExtendSub (Substructure *, Parameters *);
InstanceList *ExtendInstances (InstanceList *, Graph *);
Instance *CreateExtendedInstance (Instance *, ULONG, ULONG, Graph *);
Substructure *CreateSubFromInstance (Instance *, Graph *);
void AddPosInstancesToSub (Substructure *, InstanceList *, Parameters *);
void AddNegInstancesToSub (Substructure *, InstanceList *, Parameters *);

// graphmatch.c

BOOLEAN GraphMatch (Graph *, Graph *, LabelList *, double, double *,
		    VertexMap *);
double InexactGraphMatch (Graph *, Graph *, LabelList *, double, VertexMap *);
void OrderVerticesByDegree (Graph *, ULONG *);
ULONG MaximumNodes (ULONG);
double DeletedEdgesCost (Graph *, Graph *, ULONG, ULONG, ULONG *, LabelList *);
double InsertedEdgesCost (Graph *, ULONG, ULONG *);
double InsertedVerticesCost (Graph *, ULONG *);
MatchHeap *AllocateMatchHeap (ULONG);
VertexMap *AllocateNewMapping (ULONG, VertexMap *, ULONG, ULONG);
void InsertMatchHeapNode (MatchHeapNode *, MatchHeap *);
void ExtractMatchHeapNode (MatchHeap *, MatchHeapNode *);
void HeapifyMatchHeap (MatchHeap *);
BOOLEAN MatchHeapEmpty (MatchHeap *);
void MergeMatchHeaps (MatchHeap *, MatchHeap *);
void CompressMatchHeap (MatchHeap *, ULONG);
void PrintMatchHeapNode (MatchHeapNode *);
void PrintMatchHeap (MatchHeap *);
void ClearMatchHeap (MatchHeap *);
void FreeMatchHeap (MatchHeap *);

// graphops.c

void ReadInputFile (Parameters *);
ULONG *AddVertexIndex (ULONG *, ULONG, ULONG);
void ReadPredefinedSubsFile (Parameters *);
Graph **ReadSubGraphsFromFile (char *, char *, ULONG *, Parameters *);
Graph *ReadGraph (char *, LabelList *, BOOLEAN);
void ReadVertex (Graph *, FILE *, LabelList *, ULONG *, ULONG *, ULONG);
void ReadEdge (Graph *, FILE *, LabelList *, ULONG *, ULONG *, BOOLEAN,
	       ULONG);
void StoreEdge (Edge *, ULONG, ULONG, ULONG, ULONG, BOOLEAN);
void AddEdgeToVertices (Graph *, ULONG);
int ReadToken (char *, FILE *, ULONG *);
ULONG ReadLabel (FILE *, LabelList *, ULONG *);
ULONG ReadInteger (FILE *, ULONG *);
Graph *AllocateGraph (ULONG, ULONG);
void FreeGraph (Graph *);
void PrintGraph (Graph *, LabelList *);
void PrintVertex (Graph *, ULONG, LabelList *);
void PrintEdge (Graph *, ULONG, LabelList *);
void WriteGraphToFile (FILE *, Graph *, LabelList *);

// labels.c

LabelList *AllocateLabelList (void);
ULONG StoreLabel (Label *, LabelList *);
ULONG GetLabelIndex (Label *, LabelList *);
ULONG SubLabelNumber (ULONG, LabelList *);
double LabelMatchFactor (ULONG, ULONG, LabelList *);
void PrintLabel (ULONG, LabelList *);
void PrintLabelList (LabelList *);
void FreeLabelList (LabelList *);
void WriteLabelToFile (FILE *, ULONG, LabelList *);

// sgiso.c

InstanceList *FindInstances (Graph *, Graph *, Parameters *);
InstanceList *FindSingleVertexInstances (Graph *, Vertex *, Parameters *);
InstanceList *ExtendInstancesByEdge (InstanceList *, Graph *, Edge *,
				     Graph *, Parameters *);
BOOLEAN EdgesMatch (Graph *, Edge *, Graph *, Edge *, Parameters *);
InstanceList *FilterInstances (Graph *, InstanceList *, Graph *,
			       Parameters *);

// subops.c

Substructure *AllocateSub (void);
void FreeSub (Substructure *);
void PrintSub (Substructure *, Parameters *);
SubListNode *AllocateSubListNode (Substructure *);
void FreeSubListNode (SubListNode *);
SubList *AllocateSubList (void);
void SubListInsert (Substructure *, SubList *, ULONG, BOOLEAN, LabelList *);
BOOLEAN MemberOfSubList (Substructure *, SubList *, LabelList *);
void FreeSubList (SubList *);
void PrintSubList (SubList *, Parameters *);
void PrintNewBestSub (Substructure *, SubList *, Parameters *);
Instance *AllocateInstance (ULONG, ULONG);
void FreeInstance (Instance *);
void PrintInstance (Instance *, Graph *, LabelList *);
void MarkInstanceVertices (Instance *, Graph *, BOOLEAN);
void MarkInstanceEdges (Instance *, Graph *, BOOLEAN);
InstanceListNode *AllocateInstanceListNode (Instance *);
void FreeInstanceListNode (InstanceListNode *);
InstanceList *AllocateInstanceList (void);
void FreeInstanceList (InstanceList *);
void PrintInstanceList (InstanceList *, Graph *, LabelList *);
void PrintPosInstanceList (Substructure *, Parameters *);
void PrintNegInstanceList (Substructure *, Parameters *);
ULONG InstanceExampleNumber (Instance *, ULONG *, ULONG);
ULONG CountInstances (InstanceList *);
void InstanceListInsert (Instance *, InstanceList *, BOOLEAN);
BOOLEAN MemberOfInstanceList (Instance *, InstanceList *);
BOOLEAN InstanceMatch (Instance *, Instance *);
BOOLEAN InstanceOverlap (Instance *, Instance *);
BOOLEAN InstanceListOverlap (Instance *, InstanceList *);
BOOLEAN InstancesOverlap (InstanceList *);
Graph *InstanceToGraph (Instance *, Graph *);

// utility.c

void OutOfMemoryError (char *);
void PrintBoolean (BOOLEAN);

#endif
