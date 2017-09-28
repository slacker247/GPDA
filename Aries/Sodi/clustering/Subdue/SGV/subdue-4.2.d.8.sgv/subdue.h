/********************************************************************
*
* SUBDUE
*
* FILE NAME: subdue.h
*
********************************************************************/

#ifndef __SUBDUE_H__
#define __SUBDUE_H__

// Assume that UNIX flavor is used if not WIN32
#ifndef WIN32
#undef UNIX
#define UNIX
#endif

#ifdef UNIX
#undef _UNIX_
#define _UNIX_
#endif

#ifdef TIMING
#define _TIMING_
#endif

////////////////////////////////
// Included Interfaces
////////////////////////////////
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <libgen.h>

// for graphics display
#include<signal.h>
#include<dirent.h>
#include<errno.h>
#include<sys/types.h>
#include<sys/socket.h>
#include<sys/wait.h>
#include<sys/stat.h>
#include<netinet/in.h>
#include<arpa/inet.h>


#ifdef _TIMING_
	#ifdef _UNIX_
		#include <sys/times.h>
		#include <unistd.h>
	#else //WIN32
		#include <time.h>
	#endif 
#endif

#ifdef WIN32
	#include <direct.h>
#endif //WIN32

#ifdef _PVM_SUBDUE_
	#include "pvm3.h"
#endif //_PVM_SUBDUE_

/* // Does not appear to be used
#ifdef _DMALLOC_SUBDUE_
	#include "dmalloc.h"
#endif //_DMALLOC_SUBDUE_
*/


/////////////////////
// Type definitions

typedef int             INT;		// Do not use! Use short and long instead.
typedef unsigned int    UINT;		// Do not use! Use short and long instead.
typedef	short			SHORT;
typedef	unsigned short	USHORT;
typedef	long			LONG;		// TInteger;
typedef	unsigned long	ULONG;		// TNumber;
#ifdef WIN32
typedef __int64				INT64;	// TLargeNumber;
typedef unsigned __int64	UINT64;
#else // _UNIX_
typedef long long			INT64;	// TLargeNumber;
typedef unsigned long long	UINT64;
#endif 
typedef float			FLOAT;
typedef double			DOUBLE;		// TReal;
typedef char			CHAR;
typedef unsigned char   UCHAR;		// CType
typedef UCHAR			BYTE;
typedef BYTE			BOOLEAN;	// TBoolean;



///////////////////////////////
// Defined values and Constants
///////////////////////////////
/*#define DEBUG*/
/*#define DEBUG_TRACE*/
/*#define DEBUG_MEMORY*/
#ifdef WIN32
	#ifdef _DEBUG
	#define DEBUG
	#endif
#endif

//** {}
#define BEAM_LENGTH_BY_VALUE		/* if set, the beam search queue is based on the values */

// Subdue version and naming
#define VERSION_NUMBER					"4.2.d.8"
#define SUBDUE_NAME						"Subdue"
#define PVM_SUBDUE_NAME					"PVMSubdue"
#define PVM_SUBDUE_GROUP_NAME			"PVMSubdue"

#define FILE_EXT_POSITIVE_GRAPH			"pos"
#define FILE_EXT_NEGATIVE_GRAPH			"neg"

// general
#define FALSE							0
#define TRUE							1
#define MAX_TOKEN_LENGTH				256			/* max label length = TOKENLEN-1 */
#define STR_BUFFER_LENGTH				512			/* max label length = TOKENLEN-1 */
#define BUF_LENGTH                                      2048
#define COMMENT							'%'			/* comment character in graph file */
#define NEGSTR						"***** "
#define TWO_PI			6.28318530717958647692		/* 2 * pi */
#define LOG_2			0.69314718055994530942		/* log base e of 2.0 */
#define e				2.7182818284590452353602	/* e, or exp( 1.0 ) */
#define POSITIVE_INFINITY_DOUBLE	 1000000000.0	/* not really, but good enough... */
#define NEGATIVE_INFINITY_DOUBLE	-1000000000.0	/* not really, but good enough... */
#define POSITIVE_INFINITY_ULONG		 1000000000		/* not really, but good enough... */
#define NEGATIVE_INFINITY_ULONG		-1000000000		/* not really, but good enough... */

#define GRAPH_VERTEX_EDGES_ALLOCATION_INCREMENT   4
#define DIRECTED                               TRUE 
#define UNDIRECTED                            FALSE 
#define INSERTION_HASH_LENGTH                    10
#define HASH_SCALE                                1
#define POWER_OF_VNUMBER_TO_MAX_SEARCH_NODES    4.0
#define NODE_DELETED                             -2
#define NEW_VERTEX								 -3
#define SUB_LABEL_PREFIX						"Sub_"
#define SUB_LABEL_PREFIX_LEN					4       /* strlen("Sub_") = 4 */

#define DEFAULT_BEAM_LENGTH						4

// Output Levels
#define MIN_OUTPUT_LEVEL						1
#define MAX_OUTPUT_LEVEL						5
#define OL_MINIMAL								1	/* minimal output (default for clustering) */
#define OL_BEST_SUBS							2	/* print best substructures only (default) */
#define OL_INTERMEDIATE_SUBS					3	/* print intermediate subs as they are discovered */
#define OL_INSTANCES							4	/* print table of instances, too */
#define OL_SUPERVISED							5	/* print subs from negative graph too (supervised learning) */
#define DEFAULT_OUTPUT_LEVEL					OL_BEST_SUBS

/*#define INSERT_NODE_COST             1.0 */	/* cost of an insert-node graph transformation */
#define DELETE_NODE_COST             1.0		/* cost of a delete-node graph transformation */
#define SUBSTITUTE_NODE_COST         1.0		/* cost of a substitute-node-label transformation */
#define INSERT_EDGE_COST             1.0		/* cost of an insert-edge graph transformation */
/*#define INSERT_EDGE_WITH_NODE_COST   1.0 */	/* cost of an insert-edge-with-node transformation */
#define DELETE_EDGE_COST             1.0		/* cost of a delete-edge graph transformation */
#define DELETE_EDGE_WITH_NODE_COST   1.0		/* cost of a delete-edge-with-node transformation */
#define SUBSTITUTE_EDGE_COST         1.0		/* cost of a substitute-edge-label transformation */
#define SUBSTITUTE_EDGE_DIRECTION_COST 1.0		/* cost of a directed-undirected edge transformation */
#define REVERSE_EDGE_DIRECTION_COST  1.0

#define MATCH_COST_SAFE_MARGINE		0.0001
#define MIN_MIS_MATCH_COST			1.0
#define MIN(x, y)					(x < y) ? x : y

/* Evaluation type */
#define E_OLD							0
#define E_NEW							1

// Command Line Arguments (CLA's)
#define CLA_LIMIT				"-limit"
#define CLA_ITERATIONS			"-iterations"
#define CLA_THRESHOLD			"-threshold"
#define CLA_NSUBS				"-nsubs"
#define CLA_PRUNE				"-prune"
#define CLA_NO_MIN_ENCODE		"-nominencode"
#define CLA_BEAM				"-beam"
#define CLA_OVERLAP				"-overlap"
#define CLA_UNDIRECT			"-undirect"
#define CLA_CONNECTIVITY		"-con"
#define CLA_COMPACTNESS			"-com"
#define CLA_COVERAGE			"-cov"
#define CLA_ALT_GROUPS			"-alt"
#define CLA_SIZE				"-size"
#define CLA_NPROC				"-nproc"
#define CLA_PREDEF_SUBS			"-ps"
#define CLA_OUTPUT				"-output"
#define CLA_POSITIVE_EXAMPLES	"-numpos"
#define CLA_POS_EX_RATIO		"-minpercent"
#define CLA_SUPERVISED			"-supervised"
#define CLA_SCRATCH				"-scratch"
#define CLA_NEG_WEIGHT			"-negweight"
#define CLA_PLOT				"-plot"
#define CLA_CLUSTER				"-cluster"
#define CLA_PRUNE2				"-prune2"
#define CLA_TRUE_LABEL			"-truelabel"
#define CLA_EXHAUST				"-exhaust"
#define CLA_EVAL_TYPE			"-oldeval"

// for graphics display  - Gayathri Sampath
#define CLA_DISPLAY                     "-display"

/*
* PVM constants for parallel and concept-learning versions.
*/
#define PVM_LOCAL_VALUE					1
#define PVM_ABSTRACT_SUB	            2
#define PVM_FINAL_VALUE                 3
#define PVM_LOCAL_GRAPH_SIZE			4
#define PVM_TIME_STAT					5
#define PVM_GROUP_ID                    6
#define PVM_NEXT_MSG                    7
#define PVM_HALT                        8
#define PVM_PARENT_SUB_ID               9
#define PVM_CURRENT_SUB_ID             10
#define PVM_END_FIRST_SET              11
#define PVM_VERTEX                     12
#define PVM_EDGE1                      13
#define PVM_EDGE2                      14
#define PVM_DEAD_SUB                   15
#define PVM_ITERATE                    16
#define PVM_ANY_SOURCE				   -1

/*
* Definitions used in cluster analysis
*/
#define CLASSIFICATION_LATTICE_FILE_EXT		"dot"

// AT&T Bell Lab's graphviz stuff
#define ATT_GRAPH_TYPE			"digraph"
#define DEFAULT_SHAPE			"ellipse"
#define EXHAUSTED_SHAPE			"box"

/////////////////////////////////////
// Macros
////////////////////////////////////

#ifdef DEBUG
#define ASSERT(f)          \
    if (f)                  \
       {}                    \
       else                   \
_Assert( __FILE__, __LINE__ )
#else
#define ASSERT(f)
#endif /* DEBUG */


/*******************************************************************************
Memory management macros.

Tha main point with this is that by default, the free() function does not make 
the pointer NULL, and if it does, it helps realloc.  We can get rid of 
the if/else type of allocation where initially malloc is used, but later 
realloc is used.
*******************************************************************************/
#define Malloc( size ) \
    malloc( size )

#define Calloc( t, size ) \
    calloc( t, size )

#define Realloc( mb, size ) \
    realloc( mb, size )

#define Free( mb )      \
    {                   \
        free( mb );     \
        mb = NULL;      \
    }


/////////////////////////////////////
// Structure definitions
/////////////////////////////////////

typedef enum 
{
	VERTICES, 
	EDGES, 
	FIRST_VERTEX 
} PRINT_MODE;	//TPrintMode;

typedef enum 
{ 
	M_EXACT, 
	M_TOLERANCE, 
	M_DIFFERENCE 
} MATCH;		//TMatch;

typedef struct _GROUP	// GroupStructure
{
	ULONG numberLabels;
	char**  labels;
} GROUP, *PGROUP;	// Group;

typedef union _LABEL_CONTENT	// LabelContentUnion
{
	char *stringValue;
	DOUBLE numericValue;
} LABEL_CONTENT, *PLABEL_CONTENT;	// ULabelContent;

typedef union _LABEL_MATCH	// LabelMatchUnion
{
	LONG group;     /* group to which the string label belongs; -1 for none */
	MATCH matchType;     /* for numeric label: M_EXACT, M_TOLERANCE, OR M_DIFFERENCE */
} LABEL_MATCH, *PLABEL_MATCH;	// ULabelMatch;

typedef struct _LABEL	// LabelStructure
{
	UCHAR labelType;             /* 's' for string label, 'n' for numeric label */
	LABEL_CONTENT content;   
	LABEL_MATCH match;           
	DOUBLE matchValue;	        /* tolerance value for tolerance-based match; */
	/* difference value for difference-based match; */
} LABEL, *PLABEL;	// TLabel, *PTLabel;                                    /* 0.0 for exact match */

typedef struct _GRAPH_EDGE	// GraphEdgeStructure
{                                    
	ULONG labelIndex;
	ULONG targetVertexIndex;
	BOOLEAN directed;
} GRAPH_EDGE, *PGRAPH_EDGE;	// TGraphEdge, *PTGraphEdge;

typedef struct _GRAPH_VERTEX	// GraphVertexStructure
{
	ULONG ID;            
	ULONG labelIndex;
	ULONG edgesArrayLength;
	ULONG numberOfEdges;
	PGRAPH_EDGE edges;
	ULONG fanIn;
	ULONG *fanInVertices;
} GRAPH_VERTEX, *PGRAPH_VERTEX;	// TGraphVertex, *PTGraphVertex;

typedef struct _GRAPH	// GraphStructure
{
	ULONG numberOfVertices;	       /* # vertices currently added to graph */
	ULONG numberOfEdges;	          /* # edges currently added to graph */
//	ULONG numberOfUndirectedEdges;
	ULONG verticesArrayLength;	               /* maximum # vertices in graph */
	ULONG highestVertexID;     
	PGRAPH_VERTEX vertices;	          /* array of graph vertex structures */
} GRAPH, *PGRAPH;	// TGraph, *PTGraph;

typedef struct _SUB_GRAPH_EDGE	// SubGraphEdgeStructure
{
	ULONG indexInGraphVertex;
} SUB_GRAPH_EDGE, *PSUB_GRAPH_EDGE;	// TSubGraphEdge, *PTSubGraphEdge;

typedef struct _SUB_GRAPH_VERTEX	// SubGraphVertexStructure
{
	ULONG indexInGraph;
	ULONG numberOfEdges;
	ULONG edgesIndex;
} SUB_GRAPH_VERTEX, *PSUB_GRAPH_VERTEX;	// TSubGraphVertex, *PTSubGraphVertex;

typedef struct _SUB_GRAPH	// SubGraphStructure
{
	PSUB_GRAPH_VERTEX vertices;
	PSUB_GRAPH_EDGE edges;
	ULONG numberOfVertices;
	ULONG numberOfEdges;
//	ULONG numberOfUndirectedEdges;
	LONG *matchingOrder;
	DOUBLE matchCost;
	ULONG uniqueCoverage;
	ULONG refCount;
	ULONG numberInSub;
} SUB_GRAPH, *PSUB_GRAPH;	// TSubGraph, *PTSubGraph;

typedef struct _LIST_OF_SUB_GRAPHS_NODE	// ListOfSubGraphsNodeStructure
{
	PSUB_GRAPH subGraph;
	struct  _LIST_OF_SUB_GRAPHS_NODE *next;
} LIST_OF_SUB_GRAPHS_NODE, *PLIST_OF_SUB_GRAPHS_NODE;	// TListOfSubGraphsNode, *PTListOfSubGraphsNode;

typedef struct _LIST_OF_SUB_GRAPHS	// ListOfSubGraphsStructure
{
	ULONG currentLength;
	PLIST_OF_SUB_GRAPHS_NODE currentNode;
	PLIST_OF_SUB_GRAPHS_NODE head;
} LIST_OF_SUB_GRAPHS, *PLIST_OF_SUB_GRAPHS;	// TListOfSubGraphs, *PTListOfSubGraphs;

typedef struct _SUB	// SubStructure
{
	PSUB_GRAPH definition;
	PLIST_OF_SUB_GRAPHS instances;
	DOUBLE value;							// 1 / value = compression
	DOUBLE inputGraphCompressedWithSubDL;
	ULONG totalInstancesSize;
	LONG newVertex;			/* index of the vertex that was added; -1 for no new vertex */
	LONG newEdge1;			/* index of the edge that was just added; -1 for no new edge */
	LONG newEdge2;			/* index of the undirected edge that matches newEdge1; 
							   -1 for no new edge */
} SUB, *PSUB;	// TSub, *PTSub;

typedef struct _LIST_OF_SUBS_NODE	// ListOfSubsNodeStructure
{
	PSUB sub;
	struct _LIST_OF_SUBS_NODE *next;
	struct _LIST_OF_SUBS_NODE *previous;
} LIST_OF_SUBS_NODE, *PLIST_OF_SUBS_NODE;	// TListOfSubsNode, *PTListOfSubsNode;

typedef struct _LIST_OF_SUBS	// ListOfSubsStructure
{
	ULONG maxLength;
	ULONG currentLength;
	ULONG numberOfValues;
	PLIST_OF_SUBS_NODE head;
	PLIST_OF_SUBS_NODE tail;
	PLIST_OF_SUBS_NODE currentNode;
} LIST_OF_SUBS, *PLIST_OF_SUBS;	// TListOfSubs, *PTListOfSubs;

typedef struct _CLASSIFICATION_LATTICE
{
//	PSUB_GRAPH definition;
//	PLIST_OF_SUB_GRAPHS instances;							// makes sense only if threshold > 0
	char *subLabel;											// label assigned to this sub
	char *trueLabel;										// true label assigned to this sub
	BOOLEAN shape;											// FALSE: default; TRUE: exhausted
	struct _CLASSIFICATION_LATTICE **children;
	ULONG numberOfChildren;
	ULONG numberOfParents;									// number of nodes having this as their child
	ULONG referenceCount;									// temporary counter, used for printing
} CLASSIFICATION_LATTICE, *PCLASSIFICATION_LATTICE;

typedef struct _GRAPH_TEMPLATE	// GraphTemplateStructure
{
	ULONG *verticesTemplate;
	ULONG **edgesTemplate;
} GRAPH_TEMPLATE, *PGRAPH_TEMPLATE;	// TGraphTemplate, *PTGraphTemplate;

typedef struct _MATCHED_PAIR	// MatchedPairStructure
{
	ULONG first;
	ULONG second;
} MATCHED_PAIR, *PMATCHED_PAIR;	// TMatchedPair, *PTMatchedPair;

typedef MATCHED_PAIR UNMATCHED_PAIR;	//TUnMatchedPair;

typedef PMATCHED_PAIR PUNMATCHED_PAIR;	//PTUnMatchedPair;

typedef struct _MATCH_QUEUE_NODE	// MatchQueueNodeStructure
{
	LONG depth;
	DOUBLE matchCost;
	PMATCHED_PAIR matchedPairs;
	struct _MATCH_QUEUE_NODE *previousNode;
	struct _MATCH_QUEUE_NODE *nextNode;
} MATCH_QUEUE_NODE, *PMATCH_QUEUE_NODE;	// TMatchQueueNode, *PTMatchQueueNode;

typedef struct _GLOBAL_MATCH_QUEUE	// GlobalMatchQueueStructure
{
	PMATCH_QUEUE_NODE QHead;
	PMATCH_QUEUE_NODE QTail;
} GLOBAL_MATCH_QUEUE, *PGLOBAL_MATCH_QUEUE;	// TGlobalMatchQueue, *PTGlobalMatchQueue;

typedef struct _LOCAL_MATCH_QUEUE	// LocalMatchQueueStructure
{
	PMATCH_QUEUE_NODE QHead;
	PMATCH_QUEUE_NODE QTail;
} LOCAL_MATCH_QUEUE, *PLOCAL_MATCH_QUEUE;	// TLocalMatchQueue, *PTLocalMatchQueue;

typedef struct _REVERSE_SUB_GRAPH_NODE	// ReverseSubGraphNodeStructure
{
	ULONG vertexIndex;
	BOOLEAN used;
	PGRAPH_EDGE graphEdge;
	struct _REVERSE_SUB_GRAPH_NODE *nextNode;
} REVERSE_SUB_GRAPH_NODE, *PREVERSE_SUB_GRAPH_NODE;	// TReverseSubGraphNode, *PTReverseSubGraphNode;

typedef struct _REVERSE_SUB_GRAPH	// ReverseSubGraphStructure
{
	ULONG *totalFan;
	PREVERSE_SUB_GRAPH_NODE *reverseVerticesEdges;
	PREVERSE_SUB_GRAPH_NODE *forwardVerticesEdges;
} REVERSE_SUB_GRAPH, *PREVERSE_SUB_GRAPH;	// TReverseSubGraph, *PTReverseSubGraph;

typedef struct _EXPANDED_INSTANCE_VE	// ExpandedInstanceVEStructure
{
	PSUB_GRAPH parentInstance;
	ULONG sourceVertexIndex;
	ULONG newVertexIndexInGraph;
	ULONG addedEdgeIndex;
	struct _EXPANDED_INSTANCE_VE  *next;
} EXPANDED_INSTANCE_VE, *PEXPANDED_INSTANCE_VE;	// TExpandedInstanceVE, *PTExpandedInstanceVE;

struct _VE_TEMPLATE;

typedef struct _CONSISTENT_VE_LIST_NODE	// ConsistentVEListNode
{
	struct _VE_TEMPLATE *Template;
	LONG *matchingPattern;
	struct _CONSISTENT_VE_LIST_NODE *next;
} CONSISTENT_VE_LIST_NODE, *PCONSISTENT_VE_LIST_NODE;	// TConsistentVETemplate, *PTConsistentVETemplate;

typedef struct _VE_TEMPLATE	// VETemplateStructure
{
	ULONG numberOfInstances;
	ULONG newVertexLabelIndex;
	ULONG newEdgeLabelIndex;
	ULONG sourceVertexMatchIndex;
	BOOLEAN edgeDirection;
	BOOLEAN sourceIsOldVertex;
	BOOLEAN allNoisy;
	struct _VE_TEMPLATE *nextTemplate;
#ifdef BEAM_LENGTH_BY_VALUE
	struct _VE_TEMPLATE *nextBestTemplate;
#endif
	PEXPANDED_INSTANCE_VE listOfInstances;
	PCONSISTENT_VE_LIST_NODE consistentList;
} VE_TEMPLATE, *PVE_TEMPLATE;	// TVETemplate, *PTVETemplate;

typedef struct _EXPANDED_INSTANCE_E	// ExpandedInstanceEStructure
{
	PSUB_GRAPH parentInstance;
	ULONG sourceVertexIndex;
	ULONG targetVertexIndex;
	ULONG addedEdgeIndex;
	struct _EXPANDED_INSTANCE_E *next;
} EXPANDED_INSTANCE_E, *PEXPANDED_INSTANCE_E;	// TExpandedInstanceE, *PTExpandedInstanceE;

struct _E_TEMPLATE;

typedef struct _CONSISTENT_E_LIST_NODE	// ConsistentEListNode
{
	struct _E_TEMPLATE *Template;
	LONG *matchingPattern;
	struct _CONSISTENT_E_LIST_NODE *next;
} CONSISTENT_E_LIST_NODE, *PCONSISTENT_E_LIST_NODE;	// TConsistentETemplate, *PTConsistentETemplate;

typedef struct _E_TEMPLATE	// ETemplateStructure
{
	ULONG numberOfInstances;
	ULONG newEdgeLabelIndex;
	ULONG sourceVertexMatchIndex;
	ULONG targetVertexMatchIndex;
	BOOLEAN edgeDirection;
	BOOLEAN allNoisy;
	struct _E_TEMPLATE *nextTemplate;
#ifdef BEAM_LENGTH_BY_VALUE
	struct _E_TEMPLATE *nextBestTemplate;
#endif
	PEXPANDED_INSTANCE_E listOfInstances;
	PCONSISTENT_E_LIST_NODE consistentList;
} E_TEMPLATE, *PE_TEMPLATE;	// TETemplate, *PTETemplate; 

typedef struct _EDGE_LABEL_TEMPLATES	// EdgeLabelTemplatesStructure
{
	PE_TEMPLATE ETemplates;
	PVE_TEMPLATE VETemplates;
} EDGE_LABEL_TEMPLATES, *PEDGE_LABEL_TEMPLATES;	// TEdgeLabelTemplates, *PTEdgeLabelTemplates;

typedef struct _ABSTRACT_SUB_VERTEX	// AbstractSubVertex
{
	ULONG labelIndex;
	ULONG edgesIndex;
	ULONG numberOfEdges;
	ULONG covered;
} ABSTRACT_SUB_VERTEX, *PABSTRACT_SUB_VERTEX;	// TAbstractSubVertex, *PTAbstractSubVertex;

typedef struct _ABSTRACT_SUB_EDGE	// AbstractSubEdge
{
	ULONG labelIndex;
	ULONG targetVertex;
	BOOLEAN directed;
	ULONG covered;
} ABSTRACT_SUB_EDGE, *PABSTRACT_SUB_EDGE;	// TAbstractSubEdge, *PTAbstractSubEdge;

typedef struct _ABSTRACT_SUB	//_SUB_DEFINITION	// SubDefinitionStructure
{
	ULONG numberOfVertices;
	ULONG numberOfEdges;
//	ULONG numberOfUndirectedEdges;
	PABSTRACT_SUB_VERTEX vertices;
	PABSTRACT_SUB_EDGE edges; 
} ABSTRACT_SUB, *PABSTRACT_SUB;	//SUB_DEFINITION, *PSUB_DEFINITION;	// TAbstractSub, *PTAbstractSub;		

typedef struct _ABSTRACT_SUB_NODE	// AbstractSubNodeStruct
{
	PABSTRACT_SUB abstractSub;
	struct _ABSTRACT_SUB_NODE *next;
} ABSTRACT_SUB_NODE, *PABSTRACT_SUB_NODE;	// TAbstractSubNode, *PTAbstractSubNode;

typedef struct _LIST_OF_ABSTRACT_SUBS	// ListOfAbstractSubsStruct
{
	PABSTRACT_SUB_NODE head;
	PABSTRACT_SUB_NODE tail;
} LIST_OF_ABSTRACT_SUBS, *PLIST_OF_ABSTRACT_SUBS;	// TListOfAbstractSubs, *PTListOfAbstractSubs;

typedef struct _NEGATIVE_SUB	// NegativeSubStructure
{
	ULONG negativeSubID;
	PABSTRACT_SUB abstractSub;
	PSUB sub;
	struct _NEGATIVE_SUB *next;
} NEGATIVE_SUB, *PNEGATIVE_SUB;	// TNegativeSub, *PTNegativeSub;

// for graphics display - Gayathri Sampath

typedef struct _SOCKET_VAR
{
	struct sockaddr_in serverAddr;
        struct sockaddr_in clientAddr;
        struct stat sbuf;
        CHAR host[STR_BUFFER_LENGTH];
        ULONG serv_fd;
        ULONG client_fd;
        INT client_len;
        USHORT portNo;
} SOCKET_VAR, *PSOCKET_VAR;


typedef struct _GRAPH_VARIABLES	// GraphVariablesStructure
{
	PGRAPH	graph;                                               /* input graph */
	ULONG	numberOfLabels;
	ULONG	numberOfVertexLabels;
	ULONG	numberOfEdgeLabels;
	ULONG	numberOfGroups;    /* number of alternative groups of string labels */
	GROUP	*groupList;
	LABEL	*labelList;
	PGRAPH_TEMPLATE graphTemplate;
	BOOLEAN	newLabel;
	BOOLEAN	finalGraph;     
	/* When GV.finalGraph is TRUE (i.e. when called during iterative compression
	by main() ), CompressUsing will add edges in both directions (from source
	to target and from target to source) for each undirected edge, and will
	record fanInVertices for directed edges.  Doing these things when
	calculating description length is time-consuming, uses memory, and is
	unnecessary for the calculation, so GV.finalGraph is FALSE when
	CompressUsing() is called by EvaluateSub(). */
	ULONG	subNumber;
	ULONG	maxNumberOfBestSubs;
	ULONG	limitOfProcessedSubs;
	BOOLEAN	allowInstanceOverlap;
	DOUBLE	matchCostMaxPercent;
	BOOLEAN	prune;								// TRUE: discard subs with > value than parent
	BOOLEAN	minEncode;
	DOUBLE	connectivityPower;
	DOUBLE	coveragePower;
	DOUBLE	compactnessPower;
	ULONG	beamLength;							// subs to keep in Q of subs to expand
//	ULONG	subBeamLength;						// extensions of a sub to keep
	//ULONG	sSBeamLength;
	BOOLEAN	edge_e_Directed;
	ULONG	numberOfCompressionLevels;			// number of iterations
	DOUBLE	inputGraphDL;						// Input graph Description Length
	PEDGE_LABEL_TEMPLATES expandTemplates;
	ULONG	count;
	BOOLEAN	alternativeGroups;
	BOOLEAN	limitedSize;
	ULONG	minSize;
	ULONG	maxSize;
	ULONG   evalType;
	ULONG	outputLevel;						// sets the detail of output [1..5]
	ULONG	*numberOfVerticesWithLabel;
	ULONG	numberOfPredefSubs;
	LONG	numPosExamples;		/* number of examples in the positive graph */
	DOUBLE	minPercent;			/* minimum percentage of positive instances that a
								   pattern must occur in to be kept */
	DOUBLE	negativeWeight;		/* Weight for the sub's negative value: see
								   eval.c: EvaluateSub() */
	DOUBLE  *lgOfFact;			// Lookup table for exact values of ( lg (n!) )
	ULONG   lgOfFactTableSize;	// size of the above table
	BOOLEAN prune2;
	ULONG	prune2Value;

	struct _GRAPH_VARIABLES *negativeGraph;	// pointer to the variables for the
  	                                        // negative graph (for supervised Subdue)
        PSOCKET_VAR sockVar;

	char graphFileName[MAX_TOKEN_LENGTH];	// the filename of the graph
	PNEGATIVE_SUB negativeSubsList;		// in negativeGraph, this will point to a list
										// of NegativeSubs
	BOOLEAN	firstSetOfSubs;
	BOOLEAN	fromScratch;				// 
	FILE	*plotFile;					// file descriptior of the plot file
										// if not NULL, it writes substructure stats into plotFile
										// the format: sub# #vertices DL compression

	// clusterization variables (by Istvan Jonyer)
	BOOLEAN	cluster;					// TRUE: Subdue is in clustering mode
	FILE	*clusterFile;				// file where the classification lattice is saved
	char	clusterFileName[512];		// filename of the classification lattice file
	BOOLEAN trueLabel;					// TRUE: CL has descriptive node labels (not "Sub_n")
	BOOLEAN exhaust;					// TRUE: cluster analysis proceeds until there are
										// no more original vertices. All are Sub_n.
	UINT	saveCL;						// save the CL after each saveCL number of iterations


       // variables for graphics display - Gayathri Sampath
       BOOLEAN display;       // TRUE if graphics needs to be displayed

       


						 
#ifdef _PVM_SUBDUE_
	ULONG numberOfLocalResultsEval;
	DOUBLE *localValueOfSubAtProc;
	INT procID;								// this process ID
	INT numberOfProcs;						// number of PVM processes
	INT masterProc;							// master process ID
	INT myGroupID;
	char groupName[30];
	PABSTRACT_SUB *bestAbstractSubAtProc;
	PSUB *bestSubAtProc;
	ULONG bestSubGroupID;
	DOUBLE *globalValueOfSubAtProc;
#endif

#ifdef _TIMING_
	DOUBLE extendInstancesTm;
	DOUBLE getBestSubsTm;
	DOUBLE getStrongClassesTm;
	DOUBLE getBestTemplatesTm;
	DOUBLE addENoiseTm;
	DOUBLE addVENoiseTm;
	DOUBLE fuzzyMatchTm;
	DOUBLE clockTick;
#endif

} GRAPH_VARIABLES, *PGRAPH_VARIABLES;	// TGraphVar, *PTGraphVar;


/*
* Function prototypes
*/

/*
* abstract.c
*/
PABSTRACT_SUB CreateAbstractSub( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph );
PABSTRACT_SUB NewAbstractSub( void );
void DestroyAbstractSub( PABSTRACT_SUB abstractSub );
PLIST_OF_ABSTRACT_SUBS CreateListOfAbstractSubs( void );
void InsertInAbstractSubsList( PABSTRACT_SUB abstractSub,
							  PLIST_OF_ABSTRACT_SUBS list  );
PABSTRACT_SUB RemoveNextAbstractSub( PLIST_OF_ABSTRACT_SUBS list );
void AddVertexToAbstractSub( PABSTRACT_SUB abstractSub, ULONG labelIndex );
void AddEdgeToAbstractSub( PABSTRACT_SUB abstractSub, ULONG sourceVertexIndex,
						  ULONG targetVertexIndex, ULONG labelIndex, 
						  BOOLEAN directed );

/*
* cluster.c
*/
PCLASSIFICATION_LATTICE CL_CreateLattice(char *latticeName);
PCLASSIFICATION_LATTICE CL_CreateNode(PGRAPH_VARIABLES GV, PSUB sub);
BOOLEAN CL_Insert(PGRAPH_VARIABLES GV, PCLASSIFICATION_LATTICE clRoot, PSUB sub, BOOLEAN shape);
BOOLEAN CL_HookUp(PCLASSIFICATION_LATTICE clParent, PCLASSIFICATION_LATTICE clChild);
PCLASSIFICATION_LATTICE CL_Find(PCLASSIFICATION_LATTICE clRoot, const char *label);
void CL_Print(PGRAPH_VARIABLES GV, PCLASSIFICATION_LATTICE clNode, 
			  ULONG height, BOOLEAN printOnScreen);
void CL_Destroy(PCLASSIFICATION_LATTICE clRoot);
void CL_Finish(PGRAPH_VARIABLES GV, PCLASSIFICATION_LATTICE clNode);
void CL_FormatEdge( PGRAPH_VARIABLES GV, PGRAPH_EDGE edge, 
				   PGRAPH_VERTEX vertices, ULONG sourceVertexIndex, char *trueLabel );
void CL_MakeTrueLabel( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph, char *trueLabel );
BOOLEAN IsGraphExhausted( PGRAPH_VARIABLES GV );

/*
* compress.c
*/
ULONG CompressLabel( PGRAPH_VARIABLES GV );
void AddUncoveredVertices( PGRAPH_VARIABLES GV, PGRAPH workGraph );
void AddUncoveredEdges( PGRAPH_VARIABLES GV, PGRAPH workGraph );
PGRAPH CompressUsing( PGRAPH_VARIABLES GV, PSUB sub );
DOUBLE SizeOfCompressedGraph( PGRAPH_VARIABLES GV, PSUB sub );


/*
* concept.c
*/
DOUBLE GetNegativeValue( PGRAPH_VARIABLES GV, PSUB sub, ULONG parentNegativeSubID );
PNEGATIVE_SUB CreateNegativeSub( ULONG negativeSubID,
								PABSTRACT_SUB abstractSub, PSUB sub );
void DestroyNegativeSub( PNEGATIVE_SUB negativeSub );
void PutNegativeSub( PNEGATIVE_SUB *list, PNEGATIVE_SUB newNegativeSub );
PNEGATIVE_SUB GetNegativeSub( PNEGATIVE_SUB *list, ULONG ID );
void DestroyNegativeSubsList( PNEGATIVE_SUB *list );
void FindAndDestroyNegativeSub( PGRAPH_VARIABLES GV, ULONG deadSubID );


/*
* dl.c
*/
DOUBLE GraphSubDL( PGRAPH_VARIABLES GV, PGRAPH workGraph, PSUB_GRAPH subGraph );
DOUBLE GraphDL( PGRAPH_VARIABLES GV, PGRAPH workGraph );
void ComputeGraphStat( PGRAPH workGraph, ULONG vertexIndex, 
					  ULONG *maxEdgesToNode, ULONG *rowOnes );
DOUBLE SubGraphDL( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph );
void ComputeSubGraphStat( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph, 
						 ULONG vertexIndex, 
						 ULONG *maxEdgesToNode, ULONG *rowOnes );
DOUBLE AbstractSubDL( PGRAPH_VARIABLES GV, PABSTRACT_SUB abstractSub );
void ComputeAbstractSubStat( PABSTRACT_SUB abstractSub, ULONG vertexIndex, 
							ULONG *maxEdgesToNode, ULONG *rowOnes );


/*
* eval.c
*/
DOUBLE InstanceWeight( PSUB_GRAPH subGraph );
DOUBLE Coverage( PGRAPH_VARIABLES GV, PSUB sub );
DOUBLE Compactness( PSUB sub );
DOUBLE Connectivity( PGRAPH_VARIABLES GV, PSUB sub );
DOUBLE EvalRules( PGRAPH_VARIABLES GV, PSUB sub );
void EvaluateSub( PGRAPH_VARIABLES GV, PSUB sub, PSUB parent );


/*
* extemp.c
*/
void DestroyVETemplate( PVE_TEMPLATE VETemplate );
void DestroyETemplate( PE_TEMPLATE ETemplate );
void DestroyExpTemp( PGRAPH_VARIABLES GV );
PSUB_GRAPH CreateVEInstanceSubGraph( PGRAPH_VARIABLES GV, PVE_TEMPLATE Template, 
									PEXPANDED_INSTANCE_VE instanceDisc,
									LONG *newVertexIndex,
									LONG *newEdgeIndex1, 
									LONG *newEdgeIndex2 );
PSUB_GRAPH CreateEInstanceSubGraph( PGRAPH_VARIABLES GV, /*PE_TEMPLATE Template, */
								   PEXPANDED_INSTANCE_E instanceDisc,
								   LONG *newEdgeIndex1,
								   LONG *newEdgeIndex2 );
PEXPANDED_INSTANCE_VE CreateNewVEInstance( PSUB_GRAPH parent, 
										 ULONG sourceVertexIndex, 
										 ULONG newVertexIndexInGraph, 
										 ULONG addedEdgeIndex );
void InsertInVETemplatesList( PEDGE_LABEL_TEMPLATES edgeTemplates, 
							 PEXPANDED_INSTANCE_VE newInstance, 
							 ULONG newVertexLabelIndex, 
							 ULONG newEdgeLabelIndex,
							 ULONG sourceVertexMatchIndex,
							 BOOLEAN edgeDirection, 
							 BOOLEAN sourceIsOldVertex );
PEXPANDED_INSTANCE_E CreateNewEInstance( PSUB_GRAPH parent, 
									   ULONG sourceVertexIndex,
									   ULONG targetVertexIndex,
									   ULONG addedEdgeIndex );
void InsertInETemplatesList( PEDGE_LABEL_TEMPLATES edgeTemplates, 
							PEXPANDED_INSTANCE_E newInstance, 
							ULONG newEdgeLabelIndex,
							ULONG sourceVertexMatchIndex,
							ULONG targetVertexMatchIndex,
							BOOLEAN edgeDirection );
void AddVENoise( PGRAPH_VARIABLES GV, PVE_TEMPLATE Template, PSUB newSub, 
				PSUB_GRAPH definition, BOOLEAN *instanceIncluded );
void AddENoise( PGRAPH_VARIABLES GV, PE_TEMPLATE Template, PSUB newSub, 
			   PSUB_GRAPH definition, BOOLEAN *instanceIncluded );
PSUB CreateSubFromVETemplate( PGRAPH_VARIABLES GV, PVE_TEMPLATE Template, 
							  BOOLEAN *instanceIncluded,
							  ULONG *oldToNewMatch,
							  ULONG *newVertexIndex );
PSUB CreateSubFromETemplate( PGRAPH_VARIABLES GV, PE_TEMPLATE Template, 
							 BOOLEAN *instanceIncluded,
							 ULONG *oldToNewMatch );
BOOLEAN FastCompare( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph, ULONG *first, 
				 ULONG *second, ULONG count );
void InsertInVEConsistentList( PVE_TEMPLATE currentTemplate, 
							  PVE_TEMPLATE consistentTemplate,
							  ULONG *matchingPattern, ULONG length );
void InsertInEConsistentList( PE_TEMPLATE currentTemplate,
							 PE_TEMPLATE consistentTemplate,
							 ULONG *matchingPattern,
							 ULONG length );
void GetStrongClasses( PGRAPH_VARIABLES GV, PSUB_GRAPH subDefinition );
/*#ifdef BEAM_LENGTH_BY_VALUE
void GetBestTemplates( PGRAPH_VARIABLES GV, PVE_TEMPLATE bestVETemplates, 
					  PE_TEMPLATE bestETemplates );
#else
*/
void GetBestTemplates( PGRAPH_VARIABLES GV, PVE_TEMPLATE *bestVETemplates, 
					  PE_TEMPLATE *bestETemplates );
//#endif
void GetBestSubs( PGRAPH_VARIABLES GV, PLIST_OF_SUBS bestSubsList, PSUB oldSub );
void GetBestSubs2( PGRAPH_VARIABLES GV, PLIST_OF_SUBS bestSubsList, PSUB oldSub,
			   DOUBLE worstValueOnChildList );
BOOLEAN GetNewVertex(PGRAPH_VARIABLES GV, PSUB_GRAPH subDef, PSUB_GRAPH oldSubDef,
					 PSUB_GRAPH_VERTEX *newVertex, PGRAPH_EDGE *newEdge, 
					 ULONG *centralVertexIndexInGraph, BOOLEAN *source);
ULONG CheckExpandable(PGRAPH_VARIABLES GV, PSUB sub2, PSUB_GRAPH_VERTEX newVertex, 
					  PGRAPH_EDGE newEdge, ULONG centralVertexIndexInGraph, 
					  BOOLEAN source );
void PurgeBestSubs(PGRAPH_VARIABLES GV, PLIST_OF_SUBS bestSubsList, PSUB oldSub);
PSUB GetBestTargetSub( PGRAPH_VARIABLES GV, PABSTRACT_SUB abstractSub, 
					   PSUB oldSub );
BOOLEAN AllCovered( PABSTRACT_SUB abstractSub );
DOUBLE FinalNoise( PABSTRACT_SUB abstractSub );


/*
* extend.c
*/
void ExtendInstances( PGRAPH_VARIABLES GV, PSUB sub );
BOOLEAN NoInstanceOverlap( PSUB_GRAPH candidateInstance, 
					   PLIST_OF_SUB_GRAPHS listOfInstances );
void ExtendSub( PGRAPH_VARIABLES GV, PSUB sub, PLIST_OF_SUBS extendedSubs,
			   DOUBLE worstValueOnChildList);


/*
*  fuzzymat.c
*/
void QuickSortSubGraph( ULONG *sortArray, ULONG *verticesTotalFan, 
					   ULONG from, ULONG to );
ULONG RandomizedPartition( ULONG *sortArray, ULONG *verticesTotalFan, 
						  ULONG from, ULONG to );
void SortVertices( ULONG *sortArray, PSUB_GRAPH subGraph, 
				  PREVERSE_SUB_GRAPH subGraphReverse );
ULONG IntialQuickSearchNodesNumber( PSUB_GRAPH subGraph );
DOUBLE DeletedEdges( PGRAPH_VARIABLES GV, PREVERSE_SUB_GRAPH subGraph1Reverse, 
					PREVERSE_SUB_GRAPH subGraph2Reverse,
					ULONG *vertices1Matches, ULONG sub1VertexIndex, 
					ULONG sub2VertexIndex );
DOUBLE insertedEdges( PREVERSE_SUB_GRAPH subGraph2Reverse, 
					 ULONG sub2VertexIndex, ULONG *sub2Matches );
DOUBLE FuzzyMatch( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph1, 
				  PREVERSE_SUB_GRAPH subGraph1Reverse,
				  PSUB_GRAPH subGraph2, PREVERSE_SUB_GRAPH subGraph2Reverse,
				  DOUBLE maxMatchCost, INT64 quickSearchThreshold,
				  ULONG *first, ULONG *second,
				  ULONG initialMatchesCount, LONG *matchingPattern,
				  BOOLEAN get2MatchingPattern );
INT64 MaximumSearchNodes( ULONG numberOfSubGraphVertices );
DOUBLE InExactGraphMatch( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph1,
						 PREVERSE_SUB_GRAPH subGraph1Reverse,
						 PSUB_GRAPH subGraph2,
						 PREVERSE_SUB_GRAPH subGraph2Reverse,
						 DOUBLE matchCostMaxPercentOf2Size,
						 ULONG *first, ULONG *second,
						 ULONG initialMatchesCount,
						 LONG *matchingPattern );


/*
* graphop.c
*/
PGRAPH CreateGraph( ULONG initialSize );
void ExtendGraph( PGRAPH extendedGraph, ULONG extensionSize );
ULONG AddVertex( PGRAPH workGraph, ULONG ID, ULONG labelIndex );
void AddEdge( PGRAPH workGraph, ULONG sourceVertexIndex, 
			 ULONG targetVertexIndex, ULONG labelIndex, 
			 BOOLEAN directed, BOOLEAN addAllDirections );
ULONG GetMaxVertexID( PGRAPH graph );
void DestroyGraph( PGRAPH workGraph );


/*
* labels.c
*/
ULONG AddLabelStr( PGRAPH_VARIABLES GV, char *labelStr, char *line, 
				  BOOLEAN vertexLabel, BOOLEAN predefSub );
ULONG StringFind(char *mainStr,char c);
ULONG AddLabel( PGRAPH_VARIABLES GV, LABEL label, BOOLEAN vertexLabel, 
			   BOOLEAN predefSub );
BOOLEAN LabelIsNumeric( char *labelStr );
void RestoreLabelList( PGRAPH_VARIABLES GV, ULONG oldNumberOfLabels );     
void DestroyLabelList( PGRAPH_VARIABLES GV );
void AssignGroups( PGRAPH_VARIABLES GV );
void PrintGroups( PGRAPH_VARIABLES GV );
void PrintLabels(  PGRAPH_VARIABLES GV );
void CheckGroups( PGRAPH_VARIABLES GV );
DOUBLE LabelMatchFactor( PGRAPH_VARIABLES GV, ULONG label1Index, 
						ULONG label2Index );
BOOLEAN SameLabel( PGRAPH_VARIABLES GV, ULONG label1Index, ULONG label2Index );
void ConvertLabels( PGRAPH_VARIABLES positiveGraph, PGRAPH_VARIABLES negativeGraph, 
				   PABSTRACT_SUB abstractSub );
void UpdateLabels(PGRAPH_VARIABLES GV);


/*
* main.c
*/
void ParseCommandLine( PGRAPH_VARIABLES GV, int argc, char *argv[], 
					  BOOLEAN *supervised, char **predefSubsFileName );
INT ParseInt(char *argument, char *option, INT min, INT max );
float ParseFloat(char *argument, char *option, float min, float max );
void ParseString(char *argument, char *option, char *output );
void Usage( void );
void LoadDefaultGraphParameters(PGRAPH_VARIABLES graph);
void ClosePlotFile(PGRAPH_VARIABLES graph);
PGRAPH_VARIABLES CreateNegativeGraph( PGRAPH_VARIABLES positiveGraph);
void PrintParameters( PGRAPH_VARIABLES GV );
void PrintTimings( PGRAPH_VARIABLES GV );
void _Assert( char *file, unsigned line );


/*
* matchq.c
*/
void InsertInGlobalMatchQueue( PGLOBAL_MATCH_QUEUE queue, 
							  PMATCH_QUEUE_NODE  newNode );
PMATCH_QUEUE_NODE CreateMatchNode( PMATCH_QUEUE_NODE parentNode, LONG first,
								 LONG second, DOUBLE matchCost );
void DestroyMatchNode( PMATCH_QUEUE_NODE node );
PGLOBAL_MATCH_QUEUE CreateGlobalMatchQueue( ULONG *first, ULONG *second, 
										  ULONG initialMatchesCount );
void DestroyGlobalMatchQueue( PGLOBAL_MATCH_QUEUE queue );
void DestroyMatchQueueNode( PMATCH_QUEUE_NODE node );
void CompressGlobalMatchQueue( PGLOBAL_MATCH_QUEUE queue, 
							  ULONG compressedLength );
PMATCH_QUEUE_NODE RemoveNextMatchNode( PGLOBAL_MATCH_QUEUE queue );
PMATCH_QUEUE_NODE RemoveNextLocalMatchNode( PLOCAL_MATCH_QUEUE queue );
void AddLocalMatches( PGLOBAL_MATCH_QUEUE globalQueue, 
					 PLOCAL_MATCH_QUEUE localQueue );
PLOCAL_MATCH_QUEUE CreateLocalMatchQueue( void );
void InsertInLocalQueueInOrder( PLOCAL_MATCH_QUEUE queue, 
							   PMATCH_QUEUE_NODE newNode );
void InsertInGlobalQueueInOrder( PGLOBAL_MATCH_QUEUE queue, 
								PMATCH_QUEUE_NODE newNode );
void DestroyLocalMatchQueue( PLOCAL_MATCH_QUEUE queue );
void DestroyLocalMatchQueueNodes( PLOCAL_MATCH_QUEUE queue );


/*
* maths.c
*/
DOUBLE Base2Log( ULONG number );
DOUBLE Base2LogOfFact( ULONG number );						// deprecated
DOUBLE Base2LogOfComb( ULONG number1, ULONG number2 );		// deprecated
DOUBLE Base2LogOfCombination( PGRAPH_VARIABLES GV, ULONG number1, ULONG number2 );
void CalculateLgFactN( PGRAPH_VARIABLES GV, ULONG numberOfVertices );


/*
* prntstct.c
*/
void PrintVertex( PGRAPH_VARIABLES GV, PGRAPH_VERTEX vertex, BOOLEAN negativeSub );
void PrintEdge( PGRAPH_VARIABLES GV, PGRAPH_EDGE edge, PGRAPH_VERTEX vertices, 
			   ULONG sourceVertexID, BOOLEAN negativeSub );
void PrintGraph( PGRAPH_VARIABLES GV, PGRAPH graph, BOOLEAN printVertices,
				BOOLEAN negativeSub );
void PrintPattern( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph );
void PrintSubGraph( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph, PRINT_MODE mode,
				   BOOLEAN negativeSub );
void PrintSubGraphsList( PGRAPH_VARIABLES GV, PLIST_OF_SUB_GRAPHS subGraphsList,
						BOOLEAN negativeSub );
void PrintSub( PGRAPH_VARIABLES GV, PSUB sub, BOOLEAN finalSub, 
			  BOOLEAN negativeSub );
void PrintSubsList( PGRAPH_VARIABLES GV, PLIST_OF_SUBS subsList );
void PrintAbstractSub( PGRAPH_VARIABLES GV, PABSTRACT_SUB abstractSub, 
					  BOOLEAN negativeSub );
void PrintResults( PGRAPH_VARIABLES graph );


/*
* pvm.c
*/
#ifdef _PVM_SUBDUE_
void BroadcastAbstractSub( PGRAPH_VARIABLES graph, PABSTRACT_SUB abstractSub, int messageType );
PABSTRACT_SUB ReceiveAbstractSub( PGRAPH_VARIABLES graph, int source, int messageType );
//void BroadcastNumber( PGRAPH_VARIABLES graph, ULONG number, int messageType );	// unused
void BroadcastReal( PGRAPH_VARIABLES graph, DOUBLE value, int messageType );
//void BroadcastReal( DOUBLE value, int messageType );
void Communicate( PGRAPH_VARIABLES graph );
//void Communicate( void );
void PrintPVMError(int errorCode);

#endif


/*
* readgrph.c
*/
void ErrorFatal( char *message );
void ReadToken( char *line, ULONG *lineIndex, char *str );
ULONG ReadNumber( char *line, ULONG *lineIndex );
void ReadVertexData( char *line, ULONG *lineIndex, ULONG *ID,
					ULONG correctID );
void ReadEdgeData( char *line, ULONG *lineIndex, ULONG *fromID,
				  ULONG *toID, ULONG maxID );
void ReadGroup( PGRAPH_VARIABLES GV, char *line, ULONG *lineIndex );
ULONG CountVertices( char *fileName );
void ReadGraph( PGRAPH_VARIABLES GV, char *fileName );
PLIST_OF_ABSTRACT_SUBS ReadPredefSubs( PGRAPH_VARIABLES GV, char *fileName );


/*
* rvrsesub.c
*/
ULONG VertexIndexInSubGraph( PSUB_GRAPH subGraph, 
							ULONG vertexIndexInGraph );
PREVERSE_SUB_GRAPH CreateReverseSubGraph( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph );
void DestroyReverseSubGraph( PREVERSE_SUB_GRAPH reverseSubGraph, 
							PSUB_GRAPH subGraph );

/*
 * server.c
 */
ULONG WriteSkt(ULONG fd, CHAR *ptr, ULONG n);
ULONG ReadSkt(ULONG fd, CHAR *ptr, ULONG n);
void GraphDisplay(PGRAPH_VARIABLES GV, PSUB newSub);
BOOLEAN StartServer(PGRAPH_VARIABLES GV);
void StopServer(PGRAPH_VARIABLES GV);
void GetVertex( PGRAPH_VARIABLES GV, PGRAPH_VERTEX vertex, CHAR *buffer);
void GetEdge( PGRAPH_VARIABLES GV, PGRAPH_EDGE edge, PGRAPH_VERTEX vertices, ULONG sourceVertexID, CHAR *buffer);
void GetSubGraph( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph, PRINT_MODE mode, CHAR *buffer );
void GetSub( PGRAPH_VARIABLES GV, PSUB sub, CHAR *buffer);

/*
* subdue.c
*/
PLIST_OF_SUBS Subdue( PGRAPH_VARIABLES GV );
PLIST_OF_SUBS Discover( PGRAPH_VARIABLES GV, PLIST_OF_SUBS parentList );
PLIST_OF_SUBS InitialSubs( PGRAPH_VARIABLES GV );
PSUB GetSubWithVertexLabel( PGRAPH_VARIABLES GV, ULONG vertexLabel );
PSUB GetBestInitialSub( PGRAPH_VARIABLES GV, PABSTRACT_SUB abstractSub );
PSUB DiscoverAbstractSub( PGRAPH_VARIABLES GV, PABSTRACT_SUB abstractSub );
PSUB DiscoverTargetSub( PGRAPH_VARIABLES GV, PSUB currentSub, 
						PABSTRACT_SUB abstractSub );
PLIST_OF_SUBS DiscoverPredefSubs( PGRAPH_VARIABLES GV, 
								PLIST_OF_ABSTRACT_SUBS predefSubs );


/*
* subgphop.c
*/
BOOLEAN SameSubGraph( PSUB_GRAPH first, PSUB_GRAPH second );
BOOLEAN Member( PSUB sub, PLIST_OF_SUBS subsList );
PSUB_GRAPH CreateSubGraph( ULONG firstVertexIndex );
PLIST_OF_SUB_GRAPHS CreateSubGraphsList( void );
void InsertInSubGraphsList( PLIST_OF_SUB_GRAPHS list, PSUB_GRAPH newSubGraph );
PSUB_GRAPH GetNextSubGraph( PLIST_OF_SUB_GRAPHS list );
void InitSubGraphsList( PLIST_OF_SUB_GRAPHS list );
PSUB_GRAPH CopySubGraph( PSUB_GRAPH subGraph, DOUBLE matchCost );
BOOLEAN MemberSubGraph( PSUB_GRAPH subGraph, PLIST_OF_SUB_GRAPHS list );
BOOLEAN SameReverseEdge( PGRAPH_VARIABLES GV, PGRAPH_EDGE edge1, PGRAPH_EDGE edge2, 
					 ULONG edge2Source );
ULONG AddVertexToSubGraph( PSUB_GRAPH subGraph, PSUB_GRAPH oldSubGraph, 
						  ULONG vertexIndexInGraph );
void AddEdgeToVertex( PSUB_GRAPH subGraph, ULONG vertexIndex, 
					 ULONG graphEdgeIndex, LONG *newEdgeIndex );
PSUB_GRAPH AddEdgeToSubGraph( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph, 
							 ULONG sourceVertexIndex, 
							 ULONG sourceIndexInGraph,
							 ULONG targetVertexIndex, 
							 ULONG addedEdgeIndex, PGRAPH_EDGE addedEdge,
							 LONG *newEdgeIndex1, LONG *newEdgeIndex2 );
PSUB_GRAPH AddEdgeAndVertexToSubGraph( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph, 
									  ULONG sourceVertexIndex,
									  ULONG sourceIndexInGraph, 
									  ULONG targetIndexInGraph,
									  ULONG addedEdgeIndex, 
									  PGRAPH_EDGE addedEdge, 
									  BOOLEAN sourceEdge,
									  LONG *newVertexIndex,
									  LONG *newEdgeIndex1,
									  LONG *newEdgeIndex2 );
void DestroySubGraph( PSUB_GRAPH subGraph );
void DestroySubGraphsList( PLIST_OF_SUB_GRAPHS list );
ULONG SizeOf( PSUB_GRAPH subGraph );
ULONG SubGraphsListLength( PLIST_OF_SUB_GRAPHS listOfSubGraphs );


/*
* subsop.c
*/
PLIST_OF_SUBS CreateSubsList( ULONG maxLength );
void DestroySub( PSUB sub );
BOOLEAN InsertInSubsListInOrder( PGRAPH_VARIABLES GV, PLIST_OF_SUBS list, PSUB newSub,
							 BOOLEAN destroyNegativeSub );
PSUB CreateSub( PSUB_GRAPH definitionSubGraph );
PSUB CopySub( PSUB oldSub );
void DestroySubsList( PLIST_OF_SUBS list );
PSUB RemoveNextSub( PLIST_OF_SUBS list );
PSUB GetNextSub( PLIST_OF_SUBS list );
void InitSubsList( PLIST_OF_SUBS list );
ULONG SubsListLength( PLIST_OF_SUBS ListOfSubs );
BOOLEAN AcceptableSize( PGRAPH_VARIABLES GV, PSUB sub );


/*
* tempop.c
*/
void CreateGraphTemplate( PGRAPH_VARIABLES GV );
void DestroyGraphTemplate( PGRAPH_VARIABLES GV );
void ResizeExpandTemplates( PGRAPH_VARIABLES GV );
void SetTemplate( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph );
void SetVerticesTemplate( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph );
void ResetVerticesTemplate( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph );
void InitializeVerticesTemplate( PGRAPH_VARIABLES GV );
ULONG RegisterInTemplate( PGRAPH_VARIABLES GV, PGRAPH workGraph, 
						 PSUB_GRAPH subGraph, ULONG subGraphVertexIndex );
void ReSetTemplate( PGRAPH_VARIABLES GV );
ULONG CountOverlaps( PGRAPH_VARIABLES GV, PLIST_OF_SUB_GRAPHS instances );


#endif // __SUBDUE_H__

