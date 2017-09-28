/********************************************************************
*
* SUBDUE
*
* FILE NAME: graphop.c
*
********************************************************************/



#include "subdue.h"


/*******************************************************************************
FUNCTION NAME: CreateGraph
INPUTS:		ULONG initialSize
RETURNS:	PGRAPH
PURPOSE:	Create initial graph of the size specified.
CALLED BY:	readgrph.c: ReadGlobalGraph()
	compress.c:	CompressUsing()
*******************************************************************************/

PGRAPH CreateGraph( ULONG initialSize )
{
	PGRAPH newGraph;		                      /* pointer to the graph */
	
#ifdef DEBUG_TRACE
	printf( "%s: CreateGraph()\n", __FILE__ );
#endif
	
	newGraph = (PGRAPH) Malloc( sizeof( GRAPH ) );
	newGraph->numberOfVertices = 0;
	newGraph->numberOfEdges = 0;
//	newGraph->numberOfUndirectedEdges = 0;
	newGraph->verticesArrayLength = initialSize;
	newGraph->highestVertexID = 0;
	/* allocate space for array of vertices */
	newGraph->vertices = (PGRAPH_VERTEX) Malloc( initialSize * sizeof( GRAPH_VERTEX ) );
	return newGraph;
}


/*******************************************************************************
FUNCTION NAME: ExtendGraph 
INPUTS:		PGRAPH extendedGraph, 
			ULONG extensionSize
RETURNS:	none
PURPOSE:	Extend the graph by allocating more space for vertices.
CALLED BY: compress.c: CompressUsing()
*******************************************************************************/

void ExtendGraph( PGRAPH extendedGraph, ULONG extensionSize )
{
	
#ifdef DEBUG_TRACE
	printf( "%s: ExtendGraph()\n", __FILE__ );
#endif
	
	extendedGraph->verticesArrayLength += extensionSize;
	extendedGraph->vertices = (PGRAPH_VERTEX) Realloc( extendedGraph->vertices,
		extendedGraph->verticesArrayLength * sizeof( GRAPH_VERTEX ) );
	return;
}


/*******************************************************************************
FUNCTION NAME: AddVertex
INPUTS:		PGRAPH workGraph,                             (pointer to the graph)
			ULONG ID,                (vertex id corresponding to the input file)
			LabelInformation newLabel   (structure containing vertex label info)
RETURNS:	ULONG
PURPOSE:	Add a new vertex to the graph
CALLED BY:	readgrph.c: ReadGlobalGraph()
			compress.c: CompressUsing()
			compress.c: AddUncoveredVertices()
*******************************************************************************/

ULONG AddVertex( PGRAPH workGraph, ULONG ID, ULONG labelIndex )
{
	ULONG vertexIndex;
	
#ifdef DEBUG_TRACE
	printf( "%s: AddVertex()\n", __FILE__ );
#endif
	
	if ( workGraph->verticesArrayLength == workGraph->numberOfVertices )
		ErrorFatal( "Error in graphop.c: AddVertex(): attempt to add a vertex to a full graph." );

	vertexIndex = workGraph->numberOfVertices;
	// initialize members
	workGraph->vertices[vertexIndex].ID = ID;
	workGraph->vertices[vertexIndex].labelIndex = labelIndex;
	workGraph->vertices[vertexIndex].edgesArrayLength = 0;
	workGraph->vertices[vertexIndex].numberOfEdges = 0;
	workGraph->vertices[vertexIndex].edges = NULL;
	workGraph->vertices[vertexIndex].fanIn = 0;
	workGraph->vertices[vertexIndex].fanInVertices = NULL;

	if ( ID > workGraph->highestVertexID )
		workGraph->highestVertexID = ID;

	workGraph->numberOfVertices++;
	
	return vertexIndex;
}


/*******************************************************************************
FUNCTION NAME: AddEdge
INPUTS:		PGRAPH workGraph,                             (pointer to the graph)
			ULONG sourceVertexIndex              (index of the source vertex in)
												 (the array of vertices)
			ULONG targetVertexIndex                 (index of the target vertex)
													(in the array of vertices)
			LabelInformation newLabel     (structure containing edge label info)
			BOOLEAN directed                           (TRUE - edge is directed)
													(FALSE - edge is undirected)
			BOOLEAN addAllDirections            (TRUE - add, FALSE - do not add)
RETURNS:	none?                   (RM: old version returned ULONG vertexIndex)
PURPOSE:	Add a new edge to the graph.
CALLED BY:	readgrph.c: ReadGlobalGraph()
			tempop.c: RegisterInTemplate()
			compress.c: AddUncoveredEdges()
*******************************************************************************/

void AddEdge( PGRAPH workGraph, ULONG sourceVertexIndex, 
			 ULONG targetVertexIndex, ULONG labelIndex, 
			 BOOLEAN directed, BOOLEAN addAllDirections )
{
	PGRAPH_VERTEX workVertex;
	PGRAPH_EDGE edges;
	LONG edgeIndex;
	
#ifdef DEBUG_TRACE
	printf( "%s: AddEdge()\n", __FILE__);
#endif
	
	workVertex = &workGraph->vertices[sourceVertexIndex];
	if ( workVertex->numberOfEdges == workVertex->edgesArrayLength )
	{
		/* more space is needed for edges */
		workVertex->edgesArrayLength += GRAPH_VERTEX_EDGES_ALLOCATION_INCREMENT;
		workVertex->edges = (PGRAPH_EDGE) Realloc( workVertex->edges,
			workVertex->edgesArrayLength * sizeof( GRAPH_EDGE ) );
	}
	edges = workVertex->edges;
	/* shift edges to the right until the proper index is found */
	for ( edgeIndex = workVertex->numberOfEdges - 1; edgeIndex >= 0; edgeIndex-- ) {
		if ( edges[edgeIndex].targetVertexIndex > targetVertexIndex ) {
			edges[edgeIndex + 1] = edges[edgeIndex];				// scoot one up
		}
		else {
			break;													// found place to insert
		}
	}

	/* assign parameters */
	edges[edgeIndex + 1].targetVertexIndex = targetVertexIndex;
	edges[edgeIndex + 1].labelIndex = labelIndex;
	edges[edgeIndex + 1].directed = directed;
	workVertex->numberOfEdges++;                   /* increment number of edges */

	// Add edge to target vertex, if addAllDirections == TRUE
	if ( addAllDirections)//{U} && directed )       /* directed edge & all directions */
	{
		workVertex = &workGraph->vertices[targetVertexIndex];
		/* add fanIn vertices */
		workVertex->fanIn++;
		workVertex->fanInVertices = (ULONG *) 
			Realloc( workVertex->fanInVertices, sizeof( ULONG ) * workVertex->fanIn );
		workVertex->fanInVertices[workVertex->fanIn - 1] = sourceVertexIndex;
	}

	/* undirected edge & all directions */
//{U}
/*	if ( !directed && addAllDirections &&    
		( sourceVertexIndex != targetVertexIndex ) )
	{
		workVertex = &workGraph->vertices[targetVertexIndex];
		
		if ( workVertex->numberOfEdges == workVertex->edgesArrayLength )
		{
			workVertex->edgesArrayLength += GRAPH_VERTEX_EDGES_ALLOCATION_INCREMENT;
			workVertex->edges = (PGRAPH_EDGE) Realloc( workVertex->edges, 
				workVertex->edgesArrayLength * sizeof( GRAPH_EDGE ) );
		}
		edges = workVertex->edges;
		for ( edgeIndex = workVertex->numberOfEdges - 1; edgeIndex >= 0; edgeIndex-- )
			if ( edges[edgeIndex].targetVertexIndex > sourceVertexIndex )
				edges[edgeIndex + 1] = edges[edgeIndex];
			else 
				break;
		edges[edgeIndex + 1].targetVertexIndex = sourceVertexIndex;
		edges[edgeIndex + 1].labelIndex = labelIndex;
		edges[edgeIndex + 1].directed = directed;
		workVertex->numberOfEdges++;
	}
*/	
	workGraph->numberOfEdges++;

//{U}
/*	if ( !directed )
		workGraph->numberOfUndirectedEdges++;
*/
}

			 
/*******************************************************************************
FUNCTION NAME: GetMaxVertexID
INPUTS:		PGRAPH graph
RETURNS:	ULONG
PURPOSE:	Retrieve highest vertex ID in the graph.
CALLED BY:	compress.c: CompressUsing()
*******************************************************************************/

ULONG GetMaxVertexID( PGRAPH graph )
{
	
#ifdef DEBUG_TRACE
	printf( "%s: GetMaxVertexID()\n", __FILE__ );
#endif
	
	return graph->highestVertexID;
}
 
 
/*******************************************************************************
FUNCTION NAME: DestroyGraph
INPUTS:		PGRAPH workGraph
RETURNS:	none
PURPOSE:	Free all memory used by workGraph.
CALLED BY:	eval.c: EvaluateSub()
			main.c: main()
			readgrph.c: ReadNumber()
			readgrph.c: ReadGlobalGraph()
*******************************************************************************/

void DestroyGraph( PGRAPH workGraph )
{
	ULONG vertexIndex;
	
#ifdef DEBUG_TRACE
	printf( "%s: DestroyGraph()\n", __FILE__ );
#endif
	
	for ( vertexIndex = 0; vertexIndex < workGraph->numberOfVertices; 
	vertexIndex++ )
	{
		if ( workGraph->vertices[vertexIndex].numberOfEdges != 0 )
			Free( workGraph->vertices[vertexIndex].edges );
		if ( workGraph->vertices[vertexIndex].fanIn != 0 )
			Free( workGraph->vertices[vertexIndex].fanInVertices );
	}
	if ( workGraph->vertices != NULL )
		Free( workGraph->vertices );
	if ( workGraph != NULL )
		Free( workGraph );
	return;
}
 
 
