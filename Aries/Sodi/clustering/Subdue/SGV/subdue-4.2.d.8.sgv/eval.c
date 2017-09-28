/********************************************************************
*
* SUBDUE
*
* FILE NAME: eval.c
*
********************************************************************/


#include "subdue.h"


/*******************************************************************************
FUNCTION NAME: InstanceWeight
INPUTS:		PSUB_GRAPH subGraph
RETURNS:	DOUBLE
PURPOSE:	Define weight of the graph substructure based on its size and match 
			cost
CALLED BY:	eval.c: Coverage()
			eval.c: Connectivity()
			eval.c: Compactness()
*******************************************************************************/

DOUBLE InstanceWeight( PSUB_GRAPH subGraph )
{
	ULONG size;
	
#ifdef DEBUG_TRACE
	printf( "%s: InstanceWeight()\n", __FILE__ );
#endif
	
	size = subGraph->numberOfVertices + subGraph->numberOfEdges /*- 
		subGraph->numberOfUndirectedEdges*/;

	if( subGraph->matchCost > size )
		return 0.0;

	return ( 1.0 - subGraph->matchCost / size );
}


/*******************************************************************************
FUNCTION NAME: Coverage
INPUTS:		PGRAPH_VARIABLES GV, 
			PSUB sub
RETURNS:	DOUBLE
PURPOSE:	Compute the Coverage value for sub.
CALLED BY:	eval.c: EvalRules()
*******************************************************************************/

DOUBLE Coverage( PGRAPH_VARIABLES GV, PSUB sub )
{
	DOUBLE total;
	ULONG graphSize;
	PSUB_GRAPH instance;
	
#ifdef DEBUG_TRACE
	printf( "%s: Coverage()\n", __FILE__ );
#endif
	
	total = 0;
	while( ( instance = GetNextSubGraph( sub->instances ) ) != NULL )
		total += InstanceWeight( instance ) * instance->uniqueCoverage;
	graphSize = GV->graph->numberOfVertices + GV->graph->numberOfEdges;
	return ( 1 + total / graphSize );
}


/*******************************************************************************
FUNCTION NAME: Compactness
INPUTS:		PSUB sub
RETURNS:	DOUBLE
PURPOSE:	Compute the Compactness value for sub
CALLED BY:	eval.c: EvalRules()
*******************************************************************************/

DOUBLE Compactness( PSUB sub )
{
	DOUBLE total;
	DOUBLE temp;
	PSUB_GRAPH instance;
	
#ifdef DEBUG_TRACE
	printf( "%s: Compactness()\n", __FILE__ );
#endif
	
	total = 0.0;
	while ( ( instance = GetNextSubGraph( sub->instances ) ) != NULL )
	{
		temp = instance->numberOfEdges /*- instance->numberOfUndirectedEdges*/;
		total += ( temp / instance->numberOfVertices * InstanceWeight( instance ) );
	}
	if( SubGraphsListLength( sub->instances ) == 0 )
		return 1.0;
	else 
		return ( 1.0 + total / SubGraphsListLength( sub->instances ) );
}


/*******************************************************************************
FUNCTION NAME: Connectivity
INPUTS:		PGRAPH_VARIABLES GV, 
			PSUB sub
RETURNS:	DOUBLE
PURPOSE:	Compute the Connectivity value for sub.
CALLED BY:	eval.c: EvalRules()
*******************************************************************************/

DOUBLE Connectivity( PGRAPH_VARIABLES GV, PSUB sub )
{
	DOUBLE total;
	PSUB_GRAPH instance;
	PGRAPH_EDGE  currentVertexEdges;
	PGRAPH_EDGE currentGraphEdge;
	PSUB_GRAPH_EDGE  currentSubVertexEdges;
	PSUB_GRAPH_VERTEX currentSubVertex;
	ULONG vertexIndex;
	ULONG edgeIndex;
	ULONG sourceVertexIndex;
	ULONG connections;
	
#ifdef DEBUG_TRACE
	printf( "%s: Connectivity()\n", __FILE__ );
#endif
	
	total = 0.0;
	while ( ( instance = GetNextSubGraph( sub->instances ) ) != NULL )
	{
		SetVerticesTemplate( GV, instance );
		connections = 0;
		for ( vertexIndex = 0; vertexIndex < instance->numberOfVertices; 
		vertexIndex++ )
		{
			currentSubVertex = &instance->vertices[vertexIndex];
			sourceVertexIndex = currentSubVertex->indexInGraph;
			currentSubVertexEdges = &instance->edges[currentSubVertex->edgesIndex];
			currentVertexEdges = GV->graph->vertices[sourceVertexIndex].edges;
			for ( edgeIndex = 0; edgeIndex < currentSubVertex->numberOfEdges; 
			edgeIndex++)
			{
				currentGraphEdge = &currentVertexEdges[currentSubVertexEdges[edgeIndex].
					indexInGraphVertex];
				//{U}
				//if ( currentGraphEdge->directed || 
				//	(currentGraphEdge->targetVertexIndex > sourceVertexIndex ) )
					if ( !GV->graphTemplate->verticesTemplate[currentGraphEdge->
						targetVertexIndex] )
						connections++;
			}
		}
		total += InstanceWeight( instance ) * connections;
		ResetVerticesTemplate( GV, instance );
	}
	if ( total == 0 )
		return 2.0;
	else return ( 1.0 + SubGraphsListLength( sub->instances ) / total );
}


/*******************************************************************************
FUNCTION NAME: EvalRules
INPUTS:		PGRAPH_VARIABLES GV, 
			PSUB sub
RETURNS:	DOUBLE
PURPOSE:	Calculate the background knowledge factor for sub, based on 
			Connectivity(), Coverage(), and Compactness().
CALLED BY:	eval.c: EvaluateSub()
*******************************************************************************/

DOUBLE EvalRules( PGRAPH_VARIABLES GV, PSUB sub )
{
	DOUBLE value;
	
#ifdef DEBUG_TRACE
	printf( "%s: EvalRules()\n", __FILE__ );
#endif
	
	value = 1.0;
	if ( GV->connectivityPower != 0.0 )
		value *= (DOUBLE) pow( (double) Connectivity( GV, sub ), 
		(double) GV->connectivityPower );
	if ( GV->coveragePower != 0.0 )
		value *= (DOUBLE) pow( (double) Coverage( GV, sub ), 
		(double) GV->coveragePower );
	if ( GV->compactnessPower != 0.0 )
		value *= (DOUBLE) pow( (double) Compactness( sub ),
		(double) GV->compactnessPower );
	return value;
}


/*******************************************************************************
FUNCTION NAME: EvaluateSub
INPUTS:		PSUB sub
RETURNS:	none
PURPOSE:	Compute the Subdue heuristic value for sub. Two heuristics are 
			available here: 
			- Minimum Description Length (used when GV->minEncode == TRUE)
			and 
			- Gehad's "size of Compressed graph" heuristic (-nominencode). 

For unsupervised Subdue, the MDL value is:

(a)  GV->inputGraphDL / ( DL(S) + DL(G|S) ),

	where S is sub->definition, and G|S is the input graph compressed using S.
	Note that this is really just the inverse of the graph compression;
	the inverse is used to give a value that increases with more compression.

For supervised Subdue, the MDL value is:
  
(b)  PositiveValue - (negativeWeight * NegativeValue),

	where PositiveValue is the sub's value in the positive graph (in the master 
	process) computed using equation (a), and NegativeValue is the sub's value
	in the negative graph (in the slave process), computed in the same way. 
	When no instances of S are found in the negative graph, concept.c: 
	GetNegativeValue() returns DL(S) + DL(Gneg), where Gneg is the negative
	graph.

	A similar calculation is done when using Gehad's heuristic.
			
CALLED BY:	extemp.c: GetBestSubs()
			subdue.c: DiscoverTargetSub()
*******************************************************************************/

void EvaluateSub( PGRAPH_VARIABLES GV, PSUB sub, PSUB parent )
{
	ULONG oldNumberOfLabels;
	ULONG oldNumberOfVertexLabels;
	ULONG oldNumberOfEdgeLabels;
	DOUBLE compressedDL;
	PGRAPH workGraph;
	LABEL label;
	ULONG vertexIndex;
	ULONG graphVertexIndex;
	
#ifdef DEBUG_TRACE
	printf( "%s: EvaluateSub()\n", __FILE__ );
#endif
	
	if ( GV->minEncode ) /* MDL heuristic */
	{
		// save some parameters
		oldNumberOfLabels = GV->numberOfLabels;
		oldNumberOfVertexLabels = GV->numberOfVertexLabels;
		oldNumberOfEdgeLabels = GV->numberOfEdgeLabels;
		
//-------------------------------------------
		// do evaluation
		workGraph = CompressUsing( GV, sub );					// compress graph using this sub

		if (GV->evalType == E_OLD)
		{
			// old DL calculation
			compressedDL = SubGraphDL( GV, sub->definition );		// get DL of this sub: DL(S)
			compressedDL += GraphDL( GV, workGraph );				// + DL(G|S)
		}
		else if (GV->evalType == E_NEW)
		{
			// new DL calculation
			compressedDL = GraphSubDL( GV, workGraph, sub->definition );	// DL(G|S)
		}

		sub->inputGraphCompressedWithSubDL = compressedDL;		// save DL(S) + DL(G|S)


//-------------------------------------------
		
		// undo changes from evaluation
		DestroyGraph( workGraph );
		RestoreLabelList( GV, oldNumberOfLabels );
		GV->numberOfVertexLabels = oldNumberOfVertexLabels;
		GV->numberOfEdgeLabels = oldNumberOfEdgeLabels;
	}	
	else /* Gehad's size heuristic */
	{
		compressedDL = sub->definition->numberOfVertices + sub->definition->numberOfEdges;
			//(DOUBLE) SizeOf( sub->definition );
		// IJ: We are not using SizeOf anymore; just add the vertices and
		//		edges, and do not subtract the number of undirected edges,
		//		since it is indicative of the difference undirected edges make.
		compressedDL += SizeOfCompressedGraph( GV, sub );
		sub->inputGraphCompressedWithSubDL = compressedDL;
	}
	
	sub->value = ( GV->inputGraphDL / compressedDL ) * EvalRules( GV, sub );
	
	/* heuristic value for supervised Subdue */
	if ( GV->negativeGraph )
	{
		/* if this sub contains a compressed vertex, give it a very low value so
		it will not be considered in the future; we don't want to spend time
		extending already-discovered concepts */
		if ( GV->subNumber > 1 ) {
			for ( vertexIndex = 0; vertexIndex < sub->definition->numberOfVertices;
			vertexIndex++ )
			{
				graphVertexIndex = sub->definition->vertices[vertexIndex].indexInGraph;
				label = GV->labelList[GV->graph->vertices[graphVertexIndex].labelIndex];
				if ( label.labelType == 's' ) {
					if ( strncmp( label.content.stringValue, 
								  SUB_LABEL_PREFIX, 
								  SUB_LABEL_PREFIX_LEN) == 0 )
					{
						sub->value = NEGATIVE_INFINITY_DOUBLE;
						break;
					}
				}
			}
			
			if ( sub->value > NEGATIVE_INFINITY_DOUBLE ) {
				sub->value -= GV->negativeWeight * GetNegativeValue( GV, sub, (ULONG) parent );
			}
		}
	}
}


