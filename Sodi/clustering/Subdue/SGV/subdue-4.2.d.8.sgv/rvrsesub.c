/********************************************************************
*
* SUBDUE
*
* FILE NAME: rvrsesub.c
*
********************************************************************/



#include "subdue.h"


/*******************************************************************************
FUNCTION NAME: VertexIndexInSubGraph
INPUTS:		PSUB_GRAPH subGraph, 
			ULONG vertexIndexInGraph
RETURNS:	ULONG
PURPOSE:	Use binary search to find vertexIndexInGraph's corresponding index in 
			the array subGraph->vertices.
CALLED BY:	rvrsesub.c: CreateReverseSubGraph()
*******************************************************************************/

ULONG VertexIndexInSubGraph( PSUB_GRAPH subGraph, ULONG vertexIndexInGraph )
{
	ULONG start;
	ULONG end;
	ULONG midPoint;
	
#ifdef DEBUG_TRACE
	printf( "%s: VertexIndexInSubGraph()\n", __FILE__ );
#endif
	
	start = 0;
	end = subGraph->numberOfVertices - 1;
	while ( TRUE )
	{
		if ( start > end )
			ErrorFatal("rvrsesub.c: VertexIndexInSubGraph(): vertex not in subgraph");
		midPoint = ( end + start ) / 2;
		if ( subGraph->vertices[midPoint].indexInGraph == vertexIndexInGraph )
			return midPoint;
		else
			if ( subGraph->vertices[midPoint].indexInGraph < vertexIndexInGraph )
				start = midPoint + 1;
			else 
				end = midPoint - 1;
	}
}


/*******************************************************************************
FUNCTION NAME: CreateReverseSubGraph
INPUTS:		PSUB_GRAPH subGraph
RETURNS:	PREVERSE_SUB_GRAPH
PURPOSE:	Create a reverse subgraph to be used by fuzzymat.c: 
			InExactGraphMatch(). 
				The reverse subgraph contains two arrays of linked lists 
			(reverseVerticesEdges and forwardVerticesEdges; each vertex has an 
			element in each array, and each edge emanating from the vertex has
			a node in at least the forwardVerticesEdges list), and an array of
			integers (totalFan) representing the number of edges emanating from
			each vertex; each totalFan element is really a count of the Reverse
			and Forward nodes that are in the linked lists for that vertex, so
			directed edges are counted twice.  Undirected edges have only a
			forwardVerticesEdges node; directed edges have a 
			forwardVerticesEdges node plus a reverseVerticesEdges node.
CALLED BY:	extemp.c: GetStrongClasses()
			extemp.c: AddVENoise()
			extemp.c: AddENoise()
*******************************************************************************/

PREVERSE_SUB_GRAPH CreateReverseSubGraph( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph )
{
	ULONG vertexIndex;
	ULONG edgeIndex;
	ULONG targetSubVertexIndex;
	PGRAPH_EDGE currentGraphEdges;
	PGRAPH_EDGE graphEdge;
	PSUB_GRAPH_EDGE currentSubGraphEdges;
	PSUB_GRAPH_VERTEX currentSubVertex;
	PREVERSE_SUB_GRAPH_NODE newNode;
	PREVERSE_SUB_GRAPH newReverseSubGraph;
	
#ifdef DEBUG_TRACE
	printf( "%s: CreateReverseSubGraph()\n", __FILE__ );
#endif
	
	newReverseSubGraph = 
		(PREVERSE_SUB_GRAPH) Malloc( sizeof( REVERSE_SUB_GRAPH ) );
	newReverseSubGraph->reverseVerticesEdges =
		(PREVERSE_SUB_GRAPH_NODE *) Malloc( sizeof( PREVERSE_SUB_GRAPH_NODE )
		* subGraph->numberOfVertices );
	newReverseSubGraph->forwardVerticesEdges =
		(PREVERSE_SUB_GRAPH_NODE *) Malloc( sizeof( PREVERSE_SUB_GRAPH_NODE )
		* subGraph->numberOfVertices );
	
	newReverseSubGraph->totalFan = (ULONG *) Calloc( subGraph->numberOfVertices,
		sizeof( ULONG ) );
	for ( vertexIndex = 0; vertexIndex < subGraph->numberOfVertices; vertexIndex++ )
	{
		newReverseSubGraph->reverseVerticesEdges[vertexIndex] = NULL;
		newReverseSubGraph->forwardVerticesEdges[vertexIndex] = NULL;
	}
	
	for ( vertexIndex = 0; vertexIndex < subGraph->numberOfVertices; vertexIndex++ )
	{
		currentSubVertex = &subGraph->vertices[vertexIndex];
		currentGraphEdges = GV->graph->vertices[currentSubVertex->
			indexInGraph].edges;
		currentSubGraphEdges = &subGraph->edges[currentSubVertex->edgesIndex];

		for ( edgeIndex = 0; edgeIndex < currentSubVertex->numberOfEdges; edgeIndex++ )
		{
			graphEdge = &currentGraphEdges[currentSubGraphEdges[edgeIndex].
				indexInGraphVertex];
			targetSubVertexIndex = 
				VertexIndexInSubGraph( subGraph, graphEdge->targetVertexIndex );
			newNode =                      /* allocate node for forward vertex edge */
				(PREVERSE_SUB_GRAPH_NODE) Malloc( sizeof( REVERSE_SUB_GRAPH_NODE ) );
			newNode->vertexIndex = targetSubVertexIndex;
			newNode->used = FALSE;
			newNode->graphEdge = graphEdge;
			/* insert node at head of list */
			newNode->nextNode = newReverseSubGraph->forwardVerticesEdges[vertexIndex];
			newReverseSubGraph->forwardVerticesEdges[vertexIndex] = newNode;
			newReverseSubGraph->totalFan[vertexIndex]++;

//{U}
//			if ( graphEdge->directed )
//			{
				newNode =                    /* allocate node for reverse vertex edge */
					(PREVERSE_SUB_GRAPH_NODE) Malloc( sizeof( REVERSE_SUB_GRAPH_NODE ) );
				newNode->vertexIndex = vertexIndex;
				newNode->used = FALSE;
				newNode->graphEdge = graphEdge;
				newNode->nextNode =                    /* insert node at head of list */
					newReverseSubGraph->reverseVerticesEdges[targetSubVertexIndex];
				newReverseSubGraph->reverseVerticesEdges[targetSubVertexIndex] = 
					newNode;
				newReverseSubGraph->totalFan[targetSubVertexIndex]++;
//			}
		}
	}

	return newReverseSubGraph;
}


/*******************************************************************************
FUNCTION NAME: DestroyReverseSubGraph
INPUTS:		PREVERSE_SUB_GRAPH reverseSubGraph, 
			PSUB_GRAPH subGraph
RETURNS:	none
PURPOSE:	Free the memory used by reverseSubGraph.
CALLED BY:	extemp.c: GetStrongClasses()
			extemp.c: AddVENoise()
			extemp.c: AddENoise()
*******************************************************************************/

void DestroyReverseSubGraph( PREVERSE_SUB_GRAPH reverseSubGraph, 
							PSUB_GRAPH subGraph )
{
	ULONG vertexIndex;
	PREVERSE_SUB_GRAPH_NODE currentNode;
	PREVERSE_SUB_GRAPH_NODE nextNode;
	
#ifdef DEBUG_TRACE
	printf( "%s: DestroyReverseSubGraph()\n", __FILE__ );
#endif
	
	for ( vertexIndex = 0; vertexIndex < subGraph->numberOfVertices; 
	vertexIndex++)
	{
		currentNode = reverseSubGraph->reverseVerticesEdges[vertexIndex];
		while ( currentNode != NULL )
		{
			nextNode = currentNode->nextNode;
			Free( currentNode );
			currentNode = nextNode;
		}
		currentNode = reverseSubGraph->forwardVerticesEdges[vertexIndex];
		while ( currentNode != NULL )
		{
			nextNode = currentNode->nextNode;
			Free( currentNode );
			currentNode = nextNode;
		}
	}
	Free( reverseSubGraph->reverseVerticesEdges );
	Free( reverseSubGraph->forwardVerticesEdges );
	Free( reverseSubGraph->totalFan );
	Free( reverseSubGraph );
}


