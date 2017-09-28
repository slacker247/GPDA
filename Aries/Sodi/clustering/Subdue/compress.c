/********************************************************************
*
* SUBDUE
*
* FILE NAME: compress.c
*
********************************************************************/

#include "subdue.h"

/*******************************************************************************
FUNCTION NAME: CompressLabel
INPUTS:		PGRAPH_VARIABLES GV
RETURNS:	ULONG
PURPOSE:	Specifies the label for compressed structure.
CALLED BY: compress.c: CompressUsing()
*******************************************************************************/

ULONG CompressLabel( PGRAPH_VARIABLES GV )
{
	ULONG labelIndex;
	char labelStr[MAX_TOKEN_LENGTH];
	
#ifdef DEBUG_TRACE
	printf( "%s: CompressLabel()\n", __FILE__ );
#endif
	
	sprintf( labelStr, "%s%lu", SUB_LABEL_PREFIX, GV->subNumber );
	labelIndex = AddLabelStr( GV, labelStr, labelStr, TRUE, FALSE );
	return labelIndex;
}


/*******************************************************************************
FUNCTION NAME: AddUncoveredVertices
INPUTS:		PGRAPH_VARIABLES GV, 
			PGRAPH workGraph
RETURNS:	none
PURPOSE:	For each vertex of the global graph that is not yet registered in the
			graph template of the compressed graph, register the vertex in that 
			template and add it to the compressed graph.
CALLED BY:	compress.c: CompressUsing()
*******************************************************************************/

void AddUncoveredVertices( PGRAPH_VARIABLES GV, PGRAPH workGraph )
{
	ULONG vertexIndex;
	
#ifdef DEBUG_TRACE
	printf( "%s: AddUncoveredVertices()\n", __FILE__ );
#endif
	
	for ( vertexIndex = 0; vertexIndex < GV->graph->numberOfVertices; vertexIndex++ )
	{
		if ( !GV->graphTemplate->verticesTemplate[vertexIndex] )
		{
			GV->graphTemplate->verticesTemplate[vertexIndex] =
				AddVertex( workGraph, GV->graph->vertices[vertexIndex].ID,
				GV->graph->vertices[vertexIndex].labelIndex ) + 1;
		}
	}
}

/*******************************************************************************
FUNCTION NAME: AddUncoveredEdges
INPUTS:		PGRAPH_VARIABLES GV, PGRAPH workGraph
RETURNS:	none
PURPOSE: 
CALLED BY:	compress.c: CompressUsing()
*******************************************************************************/
void AddUncoveredEdges( PGRAPH_VARIABLES GV, PGRAPH workGraph )
{
	ULONG vertexIndex;
	ULONG edgeIndex;
	ULONG targetVertexIndex;
	ULONG sourceVertexIndex;
	PGRAPH_EDGE currentEdges;
	PGRAPH_VERTEX currentVertex;
	
#ifdef DEBUG_TRACE
	printf( "%s: AddUncoveredEdges()\n", __FILE__ );
#endif
	
	// for all vertices in the graph
	for ( vertexIndex = 0; vertexIndex < GV->graph->numberOfVertices; vertexIndex++ )
	{
		currentVertex = &GV->graph->vertices[vertexIndex];
		currentEdges = currentVertex->edges;
		sourceVertexIndex = GV->graphTemplate->verticesTemplate[vertexIndex] - 1;

		// for all edges for this vertex
		for ( edgeIndex = 0; edgeIndex < currentVertex->numberOfEdges; edgeIndex++ )
		{
			if ( !GV->graphTemplate->edgesTemplate[vertexIndex][edgeIndex] )
			{
				targetVertexIndex = GV->graphTemplate->verticesTemplate
					[currentEdges[edgeIndex].targetVertexIndex] - 1;
//{U}
//				if ( ( targetVertexIndex > sourceVertexIndex ) ||
//					currentEdges[edgeIndex].directed || 
//					( (targetVertexIndex == sourceVertexIndex) 
//					/* RM added the parentheses around this && clause */
//					&& ( vertexIndex <= currentEdges[edgeIndex].targetVertexIndex ) ) ) 
//				{
					
					AddEdge( workGraph, sourceVertexIndex, targetVertexIndex,
						currentEdges[edgeIndex].labelIndex,
						currentEdges[edgeIndex].directed, GV->finalGraph );
//				}
			}
			else
				GV->graphTemplate->edgesTemplate[vertexIndex][edgeIndex] = 0;
		}
	}
}



/*******************************************************************************
FUNCTION NAME: CompressUsing
INPUTS:		PGRAPH_VARIABLES GV, 
			PSUB sub
RETURNS:	PGRAPH
PURPOSE:	Compress the global graph by replacing all instances of sub with a 
			single vertex representing sub.  Register all new vertices and edges
			in the graph template.
CALLED BY:	eval.c: EvaluateSub()
			main.c: main()
*******************************************************************************/

PGRAPH CompressUsing( PGRAPH_VARIABLES GV, PSUB sub )
{
	PGRAPH compressedGraph;
	PSUB_GRAPH instance;
	ULONG instanceNumber;
	ULONG maxVertexID;
	ULONG instanceNodeIndex;
	ULONG coveredVertices;
	ULONG labelIndex;
	
#ifdef DEBUG_TRACE
	printf( "%s: CompressUsing()\n", __FILE__ );
#endif
	
	instanceNumber = 1;
	coveredVertices = 0;
	maxVertexID = GetMaxVertexID( GV->graph );
	compressedGraph = CreateGraph( SubGraphsListLength( sub->instances ) );
	labelIndex = CompressLabel( GV );
	InitSubGraphsList( sub->instances );
	
	while ( (instance = GetNextSubGraph( sub->instances )) != NULL )	// for each instance
	{
		instanceNodeIndex = AddVertex( compressedGraph,					// add a new vertex
			maxVertexID + instanceNumber, labelIndex );
		coveredVertices += RegisterInTemplate( GV, compressedGraph, instance,
			instanceNodeIndex );
		instanceNumber++;
	}

	ExtendGraph( compressedGraph, GV->graph->numberOfVertices - coveredVertices );
	AddUncoveredVertices( GV, compressedGraph );
	AddUncoveredEdges( GV, compressedGraph );
	InitializeVerticesTemplate( GV );

	return compressedGraph;
}

/*******************************************************************************
FUNCTION NAME: SizeOfCompressedGraph
INPUTS:		PGRAPH_VARIABLES GV, 
			PSUB sub
RETURNS:	ULONG
PURPOSE:	Compute the size of the graph compressed using sub.
CALLED BY:	eval.c: EvaluateSub()
*******************************************************************************/

DOUBLE SizeOfCompressedGraph( PGRAPH_VARIABLES GV, PSUB sub )
{
	DOUBLE size;
	ULONG overlapEdges;
	
#ifdef DEBUG_TRACE
	printf( "%s: SizeOfCompressedGraph()\n", __FILE__ );
#endif
	
	if ( !GV->allowInstanceOverlap )		// if no overlap
	{
		/* start with the vertices representing the compressed instances. */
		size = SubGraphsListLength( sub->instances );
		
		/* now add the rest of the graph and subtract the vertices and edges 
		covered by the instances */
		size += GV->graph->numberOfVertices + GV->graph->numberOfEdges/* -  
			GV->graph->numberOfUndirectedEdges*/ - sub->totalInstancesSize;
	}
	else									// if overlap
	{
		/* we add an edge to describe each overlapped edge and vertex */
		overlapEdges = CountOverlaps( GV, sub->instances ); 
		
		/* start with the vertices representing the compressed instances. */
		size = SubGraphsListLength( sub->instances );
		
		/* now add the rest of the graph and the overlap edges, and subtract 
		the vertices and edges covered by the instances */
		size += GV->graph->numberOfVertices + GV->graph->numberOfEdges /*-
            GV->graph->numberOfUndirectedEdges*/ + overlapEdges - 
            sub->totalInstancesSize;
	}
	return size;
}


