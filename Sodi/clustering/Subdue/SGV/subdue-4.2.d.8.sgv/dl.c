/********************************************************************
*
* SUBDUE
*
* FILE NAME: dl.c
*
********************************************************************/


#include "subdue.h"


/*******************************************************************************
FUNCTION NAME: GraphSubDL
INPUTS:		PGRAPH_VARIABLES GV, 
			PGRAPH workGraph,
			PSUB_GRAPH subGraph
RETURNS:	DOUBLE
PURPOSE:	Compute the minimum description length value for workGraph compressed
			by subGraph according to the MDL principle. See any paper written on
			Subdue for an explanation.

			IJ: This version calculates DL(G|S) based on the fact that the 
				compressed graph should be a single graph, not 2 separate ones.
				This is part of changing the value formula to DL(G)/DL(G|S).

CALLED BY:	main.c: main()
			eval.c: EvaluateSub()
*******************************************************************************/

DOUBLE GraphSubDL( PGRAPH_VARIABLES GV, PGRAPH workGraph, PSUB_GRAPH subGraph )
{
	DOUBLE verticesBits;
	DOUBLE rowsBits;
	DOUBLE edgesBits;
	ULONG maxEdgesBet2Nodes;
	ULONG totalAdjMatOnes;
	ULONG maxEdgesToNode;
	ULONG rowOnes;
	ULONG maxRowOnes;
	ULONG vertexIndex;
	ULONG totalNumberOfVertices;
	ULONG totalNumberOfEdges;
	
#ifdef DEBUG_TRACE
	printf( "%s: GraphSubDL()\n", __FILE__ );
#endif
	
	if ( !GV->minEncode )									// if -nominenecode
		return (DOUBLE) ( workGraph->numberOfVertices + workGraph->numberOfEdges +
			subGraph->numberOfVertices + subGraph->numberOfEdges);		// DL = V + E
	
	maxEdgesBet2Nodes = 0;
	totalAdjMatOnes = 0;
	maxEdgesToNode = 0;
	rowOnes = 0;
	maxRowOnes = 0;
	rowsBits = 0;

	totalNumberOfVertices = workGraph->numberOfVertices + subGraph->numberOfVertices;
	totalNumberOfEdges = workGraph->numberOfEdges + subGraph->numberOfEdges;

	verticesBits = Base2Log( totalNumberOfVertices ) +
		totalNumberOfVertices * Base2Log( GV->numberOfVertexLabels );

	// Calculate rowsBits for the compressed portion of the graph
	// for all vertices in the graph
	for ( vertexIndex = 0; vertexIndex < workGraph->numberOfVertices; vertexIndex++ )
	{
		ComputeGraphStat( workGraph, vertexIndex, &maxEdgesToNode, &rowOnes );	// get stats
		if ( rowOnes > maxRowOnes )												// update max
			maxRowOnes = rowOnes;

		if ( maxEdgesToNode > maxEdgesBet2Nodes )								// update max
			maxEdgesBet2Nodes = maxEdgesToNode;

		rowsBits += Base2LogOfCombination( GV, totalNumberOfVertices, rowOnes );
		totalAdjMatOnes += rowOnes;
	}
	
	// Calculate rowsBits for the subGraph
	// for all vertices in the subGraph
	for ( vertexIndex = 0; vertexIndex < subGraph->numberOfVertices; vertexIndex++ )
	{
		ComputeSubGraphStat( GV, subGraph, vertexIndex, &maxEdgesToNode, &rowOnes );
		if ( rowOnes > maxRowOnes )
			maxRowOnes = rowOnes;
		if ( maxEdgesToNode > maxEdgesBet2Nodes )
			maxEdgesBet2Nodes = maxEdgesToNode;
		rowsBits += Base2LogOfCombination( GV, totalNumberOfVertices, rowOnes );
		totalAdjMatOnes += rowOnes;
	}
	
	rowsBits = ( totalNumberOfVertices + 1 ) * Base2Log( maxRowOnes + 1 ) + rowsBits;
	
	edgesBits = ( totalNumberOfEdges * 
	       ( Base2Log( GV->numberOfEdgeLabels ) + 1.0 )
		   + ( ( totalAdjMatOnes + 1 ) * Base2Log( maxEdgesBet2Nodes ) ) );
	
	return ( verticesBits + rowsBits + edgesBits );
}

/*******************************************************************************
FUNCTION NAME: GraphDL
INPUTS:		PGRAPH_VARIABLES GV, 
			PGRAPH workGraph
RETURNS:	DOUBLE
PURPOSE:	Compute the minimum description length value for workGraph according
			to the MDL principle. See any paper written on Subdue for an 
			explanation.
CALLED BY:	main.c: main()
			eval.c: EvaluateSub()
*******************************************************************************/

DOUBLE GraphDL( PGRAPH_VARIABLES GV, PGRAPH workGraph )
{
	DOUBLE verticesBits;
	DOUBLE rowsBits;
	DOUBLE edgesBits;
	ULONG maxEdgesBet2Nodes;	// m: Maximum number of edges between 2 nodes
	ULONG totalAdjMatOnes;		// K: total number of 1's in adjacency matrix
	ULONG maxEdgesToNode;
	ULONG rowOnes;				// number of ones in a row
	ULONG maxRowOnes;			// b: maximum number of ones in any row
	ULONG vertexIndex;
	
#ifdef DEBUG_TRACE
	printf( "%s: GraphDL()\n", __FILE__ );
#endif
	
	if ( !GV->minEncode )									// if -nominenecode
		return (DOUBLE) ( workGraph->numberOfVertices + workGraph->numberOfEdges/* -
		workGraph->numberOfUndirectedEdges*/ );				// DL = V + E
	
	maxEdgesBet2Nodes = 0;
	totalAdjMatOnes = 0;
	maxEdgesToNode = 0;
	rowOnes = 0;
	maxRowOnes = 0;
	rowsBits = 0;
	
	verticesBits = Base2Log( workGraph->numberOfVertices ) +
		workGraph->numberOfVertices * Base2Log( GV->numberOfVertexLabels );
	
	// for all vertices in the graph
	for ( vertexIndex = 0; vertexIndex < workGraph->numberOfVertices; vertexIndex++ )
	{
		ComputeGraphStat( workGraph, vertexIndex, &maxEdgesToNode, &rowOnes );	// get stats
		if ( rowOnes > maxRowOnes )												// update max
			maxRowOnes = rowOnes;

		if ( maxEdgesToNode > maxEdgesBet2Nodes )								// update max
			maxEdgesBet2Nodes = maxEdgesToNode;

		rowsBits += Base2LogOfCombination( GV, workGraph->numberOfVertices, rowOnes );
		totalAdjMatOnes += rowOnes;
	}
	
	rowsBits = ( workGraph->numberOfVertices + 1 ) * Base2Log( maxRowOnes + 1 ) + rowsBits;
	
	edgesBits = ( (workGraph->numberOfEdges /* - workGraph->numberOfUndirectedEdges*/) * 
	       ( Base2Log( GV->numberOfEdgeLabels ) + 1.0 )
		   + ( ( totalAdjMatOnes + 1 ) * Base2Log( maxEdgesBet2Nodes ) ) );
	
	return ( verticesBits + rowsBits + edgesBits );
}

/*******************************************************************************
FUNCTION NAME: ComputeGraphStat
INPUTS:		PGRAPH_VARIABLES GV, 
			PGRAPH workGraph, 
			ULONG vertexIndex, 
			ULONG *maxEdgesToNode, 
			ULONG *rowOnes
RETURNS:	none
PURPOSE:	Compute maxEdgesToNode and rowOnes for vertex at vertexIndex; to be
			used for DL calculations.
CALLED BY:	dl.c: GraphDL()
*******************************************************************************/

void ComputeGraphStat( PGRAPH workGraph, ULONG vertexIndex, 
					  ULONG *maxEdgesToNode, ULONG *rowOnes )
{
	PGRAPH_EDGE graphVertexEdges;
	PGRAPH_EDGE currentEdge;
	ULONG numOfVertexEdges;
	ULONG edgeIndex;
	ULONG previousTargetIndex;
	ULONG numberOfEdgesToNode;
	
#ifdef DEBUG_TRACE
	printf( "%s: ComputeGraphStat()\n", __FILE__ );
#endif
	
	numberOfEdgesToNode = 0;
	*maxEdgesToNode = 0;
	*rowOnes = 0;
	
	graphVertexEdges = workGraph->vertices[vertexIndex].edges;			// get edges of vertex
	numOfVertexEdges = workGraph->vertices[vertexIndex].numberOfEdges;	// get # of edges
	previousTargetIndex = workGraph->numberOfVertices;					// get vertex count
	edgeIndex = 0;

	while ( edgeIndex < numOfVertexEdges )						// for each edge of the vertex
	{
		currentEdge = &graphVertexEdges[edgeIndex];				// grab edge
//{U}
//		if ( currentEdge->directed ||								// if directed or
//			(currentEdge->targetVertexIndex >= vertexIndex) )		// index(target) >= index(this) (consider undirected edges only once)
//		{
			if ( currentEdge->targetVertexIndex != previousTargetIndex )
			{
				( *rowOnes )++;
				if ( numberOfEdgesToNode > *maxEdgesToNode )
					*maxEdgesToNode = numberOfEdgesToNode;
				numberOfEdgesToNode = 1;
				previousTargetIndex = currentEdge->targetVertexIndex;
			}
			else {
				numberOfEdgesToNode++;
			}
//		}
		edgeIndex++;
	}

	if ( numberOfEdgesToNode > *maxEdgesToNode ) {
		*maxEdgesToNode = numberOfEdgesToNode;
	}
}


/*******************************************************************************
FUNCTION NAME: SubGraphDL
INPUTS:		PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph
RETURNS:	DOUBLE
PURPOSE:	Compute the Description length for subGraph.
CALLED BY:	eval.c: EvaluateSub()
*******************************************************************************/

DOUBLE SubGraphDL( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph )
{
	DOUBLE verticesBits;
	DOUBLE rowsBits;
	DOUBLE edgesBits;
	ULONG maxEdgesBet2Nodes;
	ULONG totalAdjMatOnes;
	ULONG maxEdgesToNode;
	ULONG rowOnes;
	ULONG maxRowOnes;
	ULONG vertexIndex;
	
#ifdef DEBUG_TRACE
	printf( "%s: SubGraphDL()\n", __FILE__ );
#endif
	
	maxEdgesBet2Nodes = 0;
	totalAdjMatOnes = 0;
	maxEdgesToNode = 0;
	rowOnes = 0;
	maxRowOnes = 0;
	rowsBits = 0;
	
	if ( subGraph->numberOfVertices == 1 )
		return 0.0;
	
	verticesBits = Base2Log( subGraph->numberOfVertices ) +
		subGraph->numberOfVertices * Base2Log( GV->numberOfVertexLabels );
	
	for ( vertexIndex = 0; vertexIndex < subGraph->numberOfVertices; vertexIndex++ )
	{
		ComputeSubGraphStat( GV, subGraph, vertexIndex, &maxEdgesToNode, &rowOnes );
		if ( rowOnes > maxRowOnes )
			maxRowOnes = rowOnes;
		if ( maxEdgesToNode > maxEdgesBet2Nodes )
			maxEdgesBet2Nodes = maxEdgesToNode;
		rowsBits += Base2LogOfCombination( GV, subGraph->numberOfVertices, rowOnes );
		totalAdjMatOnes += rowOnes;
	}

	rowsBits = ( subGraph->numberOfVertices + 1 ) * Base2Log( maxRowOnes + 1 ) + 
		rowsBits;
	
	edgesBits = ( (subGraph->numberOfEdges /*- subGraph->numberOfUndirectedEdges*/)
	       * (Base2Log(GV->numberOfEdgeLabels) + 1.0) 
		   + ( (totalAdjMatOnes + 1) * Base2Log(maxEdgesBet2Nodes) ) );

	
	return ( verticesBits + rowsBits + edgesBits );
}


/*******************************************************************************
FUNCTION NAME: ComputeSubGraphStat
INPUTS:		PGRAPH_VARIABLES GV, 
			PSUB_GRAPH subGraph, 
			ULONG vertexIndex, 
			ULONG *maxEdgesToNode, 
			ULONG *rowOnes
RETURNS:	none
PURPOSE:	Calculate the values of maxEdgesToNode and rowOnes for 
			subGraph->vertices[vertexIndex].
CALLED BY:	dl.c: SubGraphDL()
*******************************************************************************/

void ComputeSubGraphStat( PGRAPH_VARIABLES GV, 
						 PSUB_GRAPH subGraph, 
						 ULONG vertexIndex, 
						 ULONG *maxEdgesToNode, 
						 ULONG *rowOnes )
{
	PGRAPH_EDGE graphVertexEdges;
	PGRAPH_EDGE currentEdge;
	PSUB_GRAPH_EDGE subGraphVertexEdges;
	ULONG numOfSubGraphVertexEdges;
	ULONG subGraphEdgeIndex;
	ULONG previousTargetIndex;
	ULONG numberOfEdgesToNode;
	ULONG graphVertexIndex;
	
#ifdef DEBUG_TRACE
	printf( "%s: ComputeSubGraphStat()\n", __FILE__ );
#endif
	
	numberOfEdgesToNode = 0;
	*maxEdgesToNode = 0;
	*rowOnes = 0;
	subGraphEdgeIndex = 0;
	
	graphVertexIndex = subGraph->vertices[vertexIndex].indexInGraph;
	graphVertexEdges = GV->graph->vertices[graphVertexIndex].edges;
	subGraphVertexEdges = 
		&subGraph->edges[subGraph->vertices[vertexIndex].edgesIndex];
	numOfSubGraphVertexEdges = subGraph->vertices[vertexIndex].numberOfEdges;
	previousTargetIndex = GV->graph->numberOfVertices;
	
	while ( subGraphEdgeIndex < numOfSubGraphVertexEdges )
	{
		currentEdge = &graphVertexEdges[subGraphVertexEdges[subGraphEdgeIndex].
			indexInGraphVertex];
//{U}
//		if ( currentEdge->directed ||
//			( currentEdge->targetVertexIndex >= graphVertexIndex ) )
//		{
			if ( currentEdge->targetVertexIndex != previousTargetIndex )
			{
				( *rowOnes )++;
				if ( numberOfEdgesToNode > *maxEdgesToNode )
					*maxEdgesToNode = numberOfEdgesToNode;
				numberOfEdgesToNode = 1;
				previousTargetIndex = currentEdge->targetVertexIndex;
			}
			else 
				numberOfEdgesToNode++;
//		}
		subGraphEdgeIndex++;
	}

	if ( numberOfEdgesToNode > *maxEdgesToNode )
		*maxEdgesToNode = numberOfEdgesToNode;
}


/*******************************************************************************
FUNCTION NAME: AbstractSubDL
INPUTS:		PGRAPH_VARIABLES GV, PABSTRACT_SUB abstractSub
RETURNS:	DOUBLE
PURPOSE:	Compute the Description length value for abstractSub.
CALLED BY:	concept.c: GetNegativeValue()
*******************************************************************************/

DOUBLE AbstractSubDL( PGRAPH_VARIABLES GV, PABSTRACT_SUB abstractSub )
{
	DOUBLE verticesBits;
	DOUBLE rowsBits = 0;
	DOUBLE edgesBits;
	ULONG maxEdgesBet2Nodes;
	ULONG totalAdjMatOnes;
	ULONG maxEdgesToNode;
	ULONG rowOnes;
	ULONG maxRowOnes;
	ULONG vertexIndex;
	
#ifdef DEBUG_TRACE
	printf( "%s: AbstractSubDL()\n", __FILE__ );
#endif
	
	maxEdgesBet2Nodes = 0;
	totalAdjMatOnes = 0;
	maxEdgesToNode = 0;
	rowOnes = 0;
	maxRowOnes = 0;
	
	if ( abstractSub->numberOfVertices == 1 )
		return 0.0;
	
	verticesBits = Base2Log( abstractSub->numberOfVertices ) +
		abstractSub->numberOfVertices * Base2Log( GV->numberOfVertexLabels );
	
	for ( vertexIndex = 0; vertexIndex < abstractSub->numberOfVertices;
	vertexIndex++ )
	{
		ComputeAbstractSubStat( abstractSub, vertexIndex, &maxEdgesToNode, 
			&rowOnes );
		if ( rowOnes > maxRowOnes )
			maxRowOnes = rowOnes;
		if ( maxEdgesToNode > maxEdgesBet2Nodes )
			maxEdgesBet2Nodes = maxEdgesToNode;
		rowsBits += Base2LogOfCombination( GV, abstractSub->numberOfVertices, rowOnes );
		totalAdjMatOnes += rowOnes;
	}
	
	rowsBits = ( abstractSub->numberOfVertices + 1 ) * Base2Log( maxRowOnes + 1 ) +
		rowsBits;
	
	edgesBits = ( (abstractSub->numberOfEdges /*- abstractSub->numberOfUndirectedEdges*/)
		* ( Base2Log( GV->numberOfEdgeLabels ) + 1.0 ) + 
		( (totalAdjMatOnes + 1) * Base2Log(maxEdgesBet2Nodes) ) );
	
	return ( verticesBits + rowsBits + edgesBits );
}


/*******************************************************************************
FUNCTION NAME: ComputeAbstractSubStat
INPUTS:		PGRAPH_VARIABLES GV, 
			PABSTRACT_SUB abstractSub, 
			ULONG vertexIndex, 
			ULONG *maxEdgesToNode, 
			ULONG *rowOnes
RETURNS:	none
PURPOSE:	Calculate the values of maxEdgesToNode and rowOnes for 
			abstractSub->vertices[vertexIndex].
CALLED BY:	dl.c: AbstractSubDL()
*******************************************************************************/

void ComputeAbstractSubStat( PABSTRACT_SUB abstractSub, ULONG vertexIndex, 
							ULONG *maxEdgesToNode, ULONG *rowOnes )
{
	PABSTRACT_SUB_EDGE abstractSubVertexEdges;
	PABSTRACT_SUB_EDGE currentEdge;
	ULONG numOfAbstractSubVertexEdges;
	ULONG abstractSubEdgeIndex;
	ULONG previousTargetVertex;
	ULONG numberOfEdgesToNode;
	
#ifdef DEBUG_TRACE
	printf( "%s: ComputeAbstractSubStat()\n", __FILE__ );
#endif
	
	numberOfEdgesToNode = 0;
	*maxEdgesToNode = 0;
	*rowOnes = 0;
	abstractSubEdgeIndex = 0;
	
	abstractSubVertexEdges = 
		&abstractSub->edges[abstractSub->vertices[vertexIndex].edgesIndex];
	numOfAbstractSubVertexEdges = 
		abstractSub->vertices[vertexIndex].numberOfEdges;
	previousTargetVertex = abstractSub->numberOfVertices;
	
	while ( abstractSubEdgeIndex < numOfAbstractSubVertexEdges )
	{
		currentEdge = &abstractSubVertexEdges[abstractSubEdgeIndex];
//{U}
//		if ( currentEdge->directed ||
//			( currentEdge->targetVertex >= vertexIndex ) )
//		{
			if ( currentEdge->targetVertex != previousTargetVertex )
			{
				( *rowOnes )++;
				if ( numberOfEdgesToNode > *maxEdgesToNode )
					*maxEdgesToNode = numberOfEdgesToNode;
				numberOfEdgesToNode = 1;
				previousTargetVertex = currentEdge->targetVertex;
			}
			else 
				numberOfEdgesToNode++;
//		}
		abstractSubEdgeIndex++;
	}
	if ( numberOfEdgesToNode > *maxEdgesToNode )
		*maxEdgesToNode = numberOfEdgesToNode;
	
	return;
}

