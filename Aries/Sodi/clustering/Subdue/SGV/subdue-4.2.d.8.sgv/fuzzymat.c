/********************************************************************
*
* SUBDUE
*
* FILE NAME: fuzzymat.c
*
********************************************************************/



#include "subdue.h"



/*******************************************************************************
FUNCTION NAME: QuickSortSubGraph
INPUTS:		ULONG *sortArray, 
			ULONG *verticesTotalFan, 
			ULONG from, 
			ULONG to
RETURNS:	none
PURPOSE:	Use quick sort to sort the vertices of the subgraph on their 
			verticesTotalFan values.
CALLED BY:	fuzzymat.c: SortVertices() 
*******************************************************************************/

void QuickSortSubGraph( ULONG *sortArray, ULONG *verticesTotalFan, 
					   ULONG from, ULONG to )
{
	ULONG midPoint;
	
#ifdef DEBUG_TRACE
	printf( "%s: QuickSortSubGraph()\n", __FILE__ );
#endif
	
	if ( from < to )
	{
		midPoint = RandomizedPartition( sortArray, verticesTotalFan, from, to );
		QuickSortSubGraph( sortArray, verticesTotalFan, from, midPoint );
		QuickSortSubGraph( sortArray, verticesTotalFan, midPoint + 1, to );
	}
	return;
}


/*******************************************************************************
FUNCTION NAME: RandomizedPartition
INPUTS:		ULONG *sortArray, 
			ULONG *verticesTotalFan, 
			ULONG from, 
			ULONG to
RETURNS:	ULONG
PURPOSE:	Generate a randomized partition for QuickSortSubGraph().
CALLED BY:	fuzzymat.c: QuickSortSubGraph()
*******************************************************************************/

ULONG RandomizedPartition( ULONG *sortArray, ULONG *verticesTotalFan, 
						  ULONG from, ULONG to )
{
	ULONG pivotIndex;
	ULONG pivotVertexFan;
	ULONG temp;
	LONG i;
	LONG j;
	
#ifdef DEBUG_TRACE
	printf( "%s: RandomizedPartition()\n", __FILE__ );
#endif
	
	pivotIndex = from + rand() % ( to - from + 1 );
	temp = sortArray[pivotIndex];
	sortArray[pivotIndex] = sortArray[from];
	sortArray[from] = temp;
	temp = verticesTotalFan[pivotIndex];
	verticesTotalFan[pivotIndex] = verticesTotalFan[from];
	verticesTotalFan[from] = temp;
	pivotVertexFan = verticesTotalFan[from];
	
	i = from - 1;
	j = to + 1;
	
	do
	{
		do j = j - 1;
		while ( pivotVertexFan > verticesTotalFan[j] );
		
		do i = i + 1;
		while ( pivotVertexFan < verticesTotalFan[i] );
		
		if ( i < j )
		{
			temp = sortArray[i];
			sortArray[i] = sortArray[j];
			sortArray[j] = temp;
			temp = verticesTotalFan[i];
			verticesTotalFan[i] = verticesTotalFan[j];
			verticesTotalFan[j] = temp;
		}
		else return j;
	}
	while( TRUE );
}


/*******************************************************************************
FUNCTION NAME: SortVertices
INPUTS:		ULONG *sortArray, 
			PSUB_GRAPH subGraph,
			PREVERSE_SUB_GRAPH subGraphReverse
RETURNS:	none
PURPOSE:	Sort the vertices of Subgraph.
CALLED BY:	fuzzymat.c: FuzzyMatch()
*******************************************************************************/

void SortVertices( ULONG *sortArray, PSUB_GRAPH subGraph, 
				  PREVERSE_SUB_GRAPH subGraphReverse )
{
	ULONG vertexIndex;
	ULONG *verticesTotalFan;
	
#ifdef DEBUG_TRACE
	printf( "%s: SortVertices()\n", __FILE__ );
#endif
	
	verticesTotalFan = (ULONG *) Malloc( sizeof( ULONG ) * 
		subGraph->numberOfVertices );
	for ( vertexIndex = 0; vertexIndex < subGraph->numberOfVertices; 
	vertexIndex++ )
	{
		sortArray[vertexIndex] = vertexIndex;
		verticesTotalFan[vertexIndex] = subGraphReverse->totalFan[vertexIndex];
	}
	QuickSortSubGraph( sortArray, verticesTotalFan, 0, 
		subGraph->numberOfVertices - 1 );
	Free( verticesTotalFan );
	return;
}


/*******************************************************************************
FUNCTION NAME: IntialQuickSearchNodesNumber
INPUTS:		PSUB_GRAPH subGraph
RETURNS:	ULONG
PURPOSE:	Return subGraph->numberOfVertices.
CALLED BY:	fuzzymat.c: FuzzyMatch()
*******************************************************************************/

ULONG IntialQuickSearchNodesNumber( PSUB_GRAPH subGraph )
{
	
#ifdef DEBUG_TRACE
	printf( "%s: IntialQuickSearchNodesNumber()\n", __FILE__ );
#endif
	
	return subGraph->numberOfVertices;
}


/*******************************************************************************
FUNCTION NAME: DeletedEdges
INPUTS:		PREVERSE_SUB_GRAPH subGraph1Reverse, 
			PREVERSE_SUB_GRAPH subGraph2Reverse,
			ULONG *vertices1Matches, 
			ULONG sub1VertexIndex, 
			ULONG sub2VertexIndex
RETURNS:	DOUBLE
PURPOSE:	Calculate the cost of deleting edges necessary to match the graphs.
CALLED BY:	fuzzymat.c: FuzzyMatch()
*******************************************************************************/

DOUBLE DeletedEdges( PGRAPH_VARIABLES GV, PREVERSE_SUB_GRAPH subGraph1Reverse, 
					PREVERSE_SUB_GRAPH subGraph2Reverse,
					ULONG *vertices1Matches, ULONG sub1VertexIndex, 
					ULONG sub2VertexIndex )
{
	PREVERSE_SUB_GRAPH_NODE currentSub1Edge;
	PREVERSE_SUB_GRAPH_NODE currentSub2Edge;
	PREVERSE_SUB_GRAPH_NODE bestMatchEdge;
	DOUBLE bestMatchCost;
	DOUBLE currentMatchCost;
	DOUBLE TotalCost;
	ULONG otherVertexIndex;
	ULONG otherVertexMatchedVertex;
	
#ifdef DEBUG_TRACE
	printf( "%s: DeletedEdges()\n", __FILE__ );
#endif
	
	currentSub1Edge = subGraph1Reverse->forwardVerticesEdges[sub1VertexIndex];
	TotalCost = 0;
	while ( currentSub1Edge != NULL )
	{
		bestMatchEdge = NULL;
		bestMatchCost = -1.0;
		otherVertexIndex = currentSub1Edge->vertexIndex;
		if ( vertices1Matches[otherVertexIndex] )
		{
			otherVertexMatchedVertex = vertices1Matches[otherVertexIndex] - 1;	
			currentSub2Edge = subGraph2Reverse->forwardVerticesEdges[sub2VertexIndex];
			while ( currentSub2Edge != NULL )
			{
				if ( !currentSub2Edge->used )
				{
					currentMatchCost = 0;
					if ( currentSub2Edge->vertexIndex == otherVertexMatchedVertex )
					{
						if ( currentSub1Edge->graphEdge->directed !=
							 currentSub2Edge->graphEdge->directed )
							currentMatchCost = SUBSTITUTE_EDGE_DIRECTION_COST;

						currentMatchCost += SUBSTITUTE_EDGE_COST *
							LabelMatchFactor( GV, currentSub1Edge->graphEdge->labelIndex,
							currentSub2Edge->graphEdge->labelIndex );
						if ( ( currentMatchCost < bestMatchCost ) || ( bestMatchCost < 0 ) )
						{
							bestMatchCost = currentMatchCost;
							bestMatchEdge = currentSub2Edge;
						}
					}
				}
				currentSub2Edge = currentSub2Edge->nextNode;
			}
			currentSub2Edge = subGraph2Reverse->reverseVerticesEdges[sub2VertexIndex];
			while ( currentSub2Edge != NULL )
			{
				if ( !currentSub2Edge->used )
				{
					currentMatchCost = 0;
					if ( currentSub2Edge->vertexIndex == otherVertexMatchedVertex )
					{
						if ( currentSub1Edge->graphEdge->directed ==
							currentSub2Edge->graphEdge->directed )
							currentMatchCost = REVERSE_EDGE_DIRECTION_COST;
						currentMatchCost += SUBSTITUTE_EDGE_COST *
							LabelMatchFactor( GV, currentSub1Edge->graphEdge->labelIndex,
							currentSub2Edge->graphEdge->labelIndex );
						if ( ( currentMatchCost < bestMatchCost ) || ( bestMatchCost < 0 ) )
						{
							bestMatchCost = currentMatchCost;
							bestMatchEdge = currentSub2Edge;
						}
					}
				}
				currentSub2Edge = currentSub2Edge->nextNode;
			}
			
			if ( bestMatchEdge != NULL )
			{
				bestMatchEdge->used = TRUE;
				TotalCost += bestMatchCost;
			}
			else TotalCost += DELETE_EDGE_COST;
		}
		currentSub1Edge = currentSub1Edge->nextNode;
	}
	currentSub1Edge = subGraph1Reverse->reverseVerticesEdges[sub1VertexIndex];
	while ( currentSub1Edge != NULL )
	{
		bestMatchEdge = NULL;
		bestMatchCost = -1.0;
		otherVertexIndex = currentSub1Edge->vertexIndex;
		if ( vertices1Matches[otherVertexIndex] )
		{
			otherVertexMatchedVertex = vertices1Matches[otherVertexIndex] - 1;
			currentSub2Edge = subGraph2Reverse->forwardVerticesEdges[sub2VertexIndex];
			while ( currentSub2Edge != NULL )
			{
				if ( !currentSub2Edge->used )
				{
					currentMatchCost = 0;
					if ( currentSub2Edge->vertexIndex == otherVertexMatchedVertex )
					{
						if ( currentSub1Edge->graphEdge->directed &&
							currentSub2Edge->graphEdge->directed )
							currentMatchCost = REVERSE_EDGE_DIRECTION_COST;
						currentMatchCost += SUBSTITUTE_EDGE_COST *
							LabelMatchFactor( GV, currentSub1Edge->graphEdge->labelIndex,
							currentSub2Edge->graphEdge->labelIndex );
						if ( ( currentMatchCost < bestMatchCost ) || ( bestMatchCost < 0 ) )
						{
							bestMatchCost = currentMatchCost;
							bestMatchEdge = currentSub2Edge;
						}
					}
				}
				currentSub2Edge = currentSub2Edge->nextNode;
			} 
			currentSub2Edge = subGraph2Reverse->reverseVerticesEdges[sub2VertexIndex];
			while ( currentSub2Edge != NULL )
			{
				if ( !currentSub2Edge->used )
				{
					currentMatchCost = 0;
					if ( currentSub2Edge->vertexIndex == otherVertexMatchedVertex )
					{
						if ( currentSub1Edge->graphEdge->directed !=
							currentSub2Edge->graphEdge->directed )
							currentMatchCost = SUBSTITUTE_EDGE_DIRECTION_COST;
						currentMatchCost += SUBSTITUTE_EDGE_COST *
							LabelMatchFactor( GV, currentSub1Edge->graphEdge->labelIndex,
							currentSub2Edge->graphEdge->labelIndex );
						if ( ( currentMatchCost < bestMatchCost ) || ( bestMatchCost < 0 ) )
						{
							bestMatchCost = currentMatchCost;
							bestMatchEdge = currentSub2Edge;
						}
					}
				}
				currentSub2Edge = currentSub2Edge->nextNode;
			}
			
			if ( bestMatchEdge != NULL )
			{
				bestMatchEdge->used = TRUE;
				TotalCost += bestMatchCost;
			}
			else TotalCost += DELETE_EDGE_COST;
		}
		currentSub1Edge = currentSub1Edge->nextNode;
	}
	return TotalCost;
}


/*******************************************************************************
FUNCTION NAME: InsertedEdges
INPUTS:		PREVERSE_SUB_GRAPH subGraph2Reverse, 
			ULONG sub2VertexIndex, 
			ULONG *sub2Matches
RETURNS:	DOUBLE
PURPOSE:	Calculate the cost of inserting edges necessary to match the graphs.
CALLED BY:	fuzzymat.c: FuzzyMatch()
*******************************************************************************/

DOUBLE InsertedEdges( PREVERSE_SUB_GRAPH subGraph2Reverse, 
					 ULONG sub2VertexIndex, ULONG *sub2Matches )
{
	DOUBLE insertedEdges;
	PREVERSE_SUB_GRAPH_NODE currentSub2Edge;
	
#ifdef DEBUG_TRACE
	printf( "%s: InsertedEdges()\n", __FILE__ );
#endif
	
	insertedEdges = 0;
	currentSub2Edge = subGraph2Reverse->forwardVerticesEdges[sub2VertexIndex];
	while ( currentSub2Edge != NULL)
	{
		if ( currentSub2Edge->used )
			currentSub2Edge->used = FALSE;
		else if ( sub2Matches[currentSub2Edge->vertexIndex] )
			insertedEdges++;
		currentSub2Edge = currentSub2Edge->nextNode;
	}
	
	currentSub2Edge = subGraph2Reverse->reverseVerticesEdges[sub2VertexIndex];
	while ( currentSub2Edge != NULL )
	{
		if ( currentSub2Edge->used )
			currentSub2Edge->used = FALSE;
		else if ( sub2Matches[currentSub2Edge->vertexIndex] )
			insertedEdges++;
		currentSub2Edge = currentSub2Edge->nextNode;
	}
	
	return ( insertedEdges * INSERT_EDGE_COST );
}


/*******************************************************************************
FUNCTION NAME: FuzzyMatch
INPUTS:		PSUB_GRAPH subGraph1, 
			PREVERSE_SUB_GRAPH subGraph1Reverse,
			PSUB_GRAPH subGraph2, 
			PREVERSE_SUB_GRAPH subGraph2Reverse,
			DOUBLE maxMatchCost, 
			INT64 quickSearchThreshold,
			LONG *first, 
			LONG *second, 
			ULONG initialMatchesCount,
			LONG *matchingPattern, 
			BOOLEAN get2MatchingPattern
RETURNS:	DOUBLE
PURPOSE:	Calculate the cost of the optimal inexact graph match between 
			subGraph1 and Subgraph2.
CALLED BY:	fuzzymat.c: InExactGraphMatch()
*******************************************************************************/

DOUBLE FuzzyMatch( 
				  PGRAPH_VARIABLES GV,
				  PSUB_GRAPH subGraph1, 
				  PREVERSE_SUB_GRAPH subGraph1Reverse,
				  PSUB_GRAPH subGraph2,
				  PREVERSE_SUB_GRAPH subGraph2Reverse,
				  DOUBLE maxMatchCost,			/* GV->matchCostMaxPercent*SizeOf(Subgraph2) for */
												/* AddVENoise() and AddENoise(); 0.0 for GetStrongClasses() */
				  INT64 quickSearchThreshold,   /* calculated by fuzzymat.c: */
												/* MaximumSearchNodes() */
				  ULONG *first,					/* these hold the sourceVertexMatchIndex (0) */
				  ULONG *second,				/* and targetVertexMatchIndex (1) for current */
												/* template (first) and next template (second) */
				  
				  ULONG initialMatchesCount,    /* 0 for AddVENoise() and AddENoise(); 1 or */
												/* 2 for extemp.c: GetStrongClasses() */
				  LONG *matchingPattern,        /* size is subGraph2->numberOfVertices */
				  BOOLEAN get2MatchingPattern ) /* TRUE if subGraph1->numberOfVertices >= */
												/* subGraph2->numberOfVertices; FALSE otherwise */
{
	ULONG *vertices1Matches;
	ULONG *vertices2Matches;
	INT64 numberOfExpandedNodes;
	PGLOBAL_MATCH_QUEUE globalMatchQueue;
	PLOCAL_MATCH_QUEUE localMatchQueue;
	PMATCH_QUEUE_NODE newNode;
	ULONG *sortedVertices;
	DOUBLE currentBestCost;
	DOUBLE newNodeMatchCost;
	BOOLEAN quickMatch;
	BOOLEAN terminate;
	PMATCH_QUEUE_NODE expandingNode;
	PGRAPH_VERTEX graphVertices;
	PSUB_GRAPH_VERTEX  sub1Vertices;
	PSUB_GRAPH_VERTEX  sub2Vertices;
	ULONG vertexToMatchIndex;
	ULONG pairIndex;
	ULONG vertex2Index;
	ULONG index;
	ULONG index1;
	ULONG vertexToMatchLabelIndex;
	ULONG *indices;
#ifdef _TIMING_
#ifdef _UNIX_
	struct tms tmsStart;
	struct tms tmsEnd;
#else // WIN32
	clock_t start, end;
#endif
#endif
	
#ifdef DEBUG_TRACE
	printf( "%s: FuzzyMatch()\n", __FILE__ );
#endif
	
#ifdef _TIMING_
#ifdef _UNIX_
	times( &tmsStart );
#else // WIN32
	start = clock();
#endif
#endif
	
	graphVertices = GV->graph->vertices;
	sub1Vertices = subGraph1->vertices;
	sub2Vertices = subGraph2->vertices;
	vertices1Matches = (ULONG *) Malloc( sizeof( ULONG ) * 
		subGraph1->numberOfVertices );
	vertices2Matches = (ULONG *) Malloc( sizeof( ULONG ) * 
		subGraph2->numberOfVertices );
	sortedVertices = (ULONG *) Malloc( sizeof( ULONG ) * 
		subGraph1->numberOfVertices );
	SortVertices( sortedVertices, subGraph1, subGraph1Reverse );
	
	if ( initialMatchesCount )
	{               /* move indices in first to the top of array sortedVertices */
		indices = (ULONG *) Malloc( sizeof( ULONG ) * initialMatchesCount );
		for ( index1 = 0; index1 < initialMatchesCount; index1++ )
			for ( index = 0; index < subGraph1->numberOfVertices; index++ )
				if ( sortedVertices[index] == first[index1] )
					indices[index1] = index;
				for( index1 = 0; index1 < initialMatchesCount; index1++ )
					if ( indices[index1] >= 0 && indices[index1] < subGraph1->numberOfVertices
						&& index1 >= 0 && index1 < subGraph1->numberOfVertices )  
						/* watch out for NODE_DELETED values */
					{
						sortedVertices[indices[index1]] = sortedVertices[index1];
						sortedVertices[index1] = first[index1];
					}
					else
					{
						printf( "\nIndices[index1] = %d\nIndex1 = %d\n\n", indices[index1],
							index1 );
						printf( "subGraph1->numberOfVertices = %d\n",
							subGraph1->numberOfVertices );
						PrintSubGraph( GV, subGraph1, VERTICES, FALSE );
						PrintSubGraph( GV, subGraph1, EDGES, FALSE );
						PrintSubGraph( GV, subGraph2, VERTICES,  FALSE );
						PrintSubGraph( GV, subGraph2, EDGES, FALSE );
						
						fflush( stdout );
					}
					Free( indices );
	}
	
	localMatchQueue = CreateLocalMatchQueue();
	globalMatchQueue = CreateGlobalMatchQueue( first, second, 
		initialMatchesCount );
	numberOfExpandedNodes = 0;
	currentBestCost = -1.0;
	quickMatch = FALSE;
	terminate = FALSE;
	
	expandingNode = RemoveNextMatchNode( globalMatchQueue );
	
	while ( ( expandingNode != NULL ) && !terminate )
	{
		if ( ( expandingNode->matchCost < currentBestCost ) || ( currentBestCost <
							     0 ) )
		{
			if ( (ULONG)expandingNode->depth == subGraph1->numberOfVertices )
			{
				currentBestCost = expandingNode->matchCost;
				/* A zero in the returned matching pattern */
				/* means that the node is not matched */
				if ( get2MatchingPattern )
				{
					memset( matchingPattern, 0, subGraph2->numberOfVertices * 
						sizeof( LONG ) );
					for ( index = 0; index < (ULONG)expandingNode->depth; index++ )
						if ( expandingNode->matchedPairs[index].second != NODE_DELETED )
							matchingPattern[expandingNode->matchedPairs[index].second] = 
							expandingNode->matchedPairs[index].first + 1;
				}
				else
				{
					memset( matchingPattern, 0, subGraph1->numberOfVertices * 
						sizeof( LONG ) );
					for ( index = 0; index < (ULONG)expandingNode->depth; index++ )
						if ( expandingNode->matchedPairs[index].second != NODE_DELETED )
							matchingPattern[expandingNode->matchedPairs[index].first] = 
							expandingNode->matchedPairs[index].second + 1;
				}
				DestroyMatchNode( expandingNode );
				if( !quickMatch )
					terminate = TRUE;
			}
			else
			{
				vertexToMatchIndex = sortedVertices[expandingNode->depth];
				vertexToMatchLabelIndex = 
					graphVertices[sub1Vertices[vertexToMatchIndex].indexInGraph].
					labelIndex;
				memset( vertices1Matches, 0, subGraph1->numberOfVertices * 
					sizeof( ULONG ) );
				memset( vertices2Matches, 0, subGraph2->numberOfVertices *
					sizeof( ULONG ) );
				for( pairIndex = 0; pairIndex < (ULONG)expandingNode->depth; pairIndex++ )
				{
					vertices1Matches[expandingNode->matchedPairs[pairIndex].first] = 
						expandingNode->matchedPairs[pairIndex].second + 1;
					if ( expandingNode->matchedPairs[pairIndex].second != NODE_DELETED )
						vertices2Matches[expandingNode->matchedPairs[pairIndex].second] =
						expandingNode->matchedPairs[pairIndex].first + 1;
				}
				newNodeMatchCost = expandingNode->matchCost;
				newNodeMatchCost += DELETE_NODE_COST;
				newNodeMatchCost += subGraph1Reverse->totalFan[vertexToMatchIndex] *
					DELETE_EDGE_WITH_NODE_COST;
				if( ( ( newNodeMatchCost - MATCH_COST_SAFE_MARGINE ) < maxMatchCost ) &&
					( ( newNodeMatchCost < currentBestCost ) || 
					( currentBestCost < 0 ) ) )
				{
					newNode = CreateMatchNode( expandingNode, vertexToMatchIndex, 
						NODE_DELETED, newNodeMatchCost );
					InsertInLocalQueueInOrder( localMatchQueue, newNode );
				}
				for( vertex2Index = 0; vertex2Index < subGraph2->numberOfVertices; 
				vertex2Index++ )
					if ( vertices2Matches[vertex2Index] == 0 )
					{
						newNodeMatchCost = expandingNode->matchCost;
						newNodeMatchCost += SUBSTITUTE_NODE_COST *
							LabelMatchFactor( GV, vertexToMatchLabelIndex, 
							graphVertices[sub2Vertices
							[vertex2Index].indexInGraph].
							labelIndex );
						newNodeMatchCost += DeletedEdges( GV, subGraph1Reverse, 
							subGraph2Reverse, vertices1Matches, vertexToMatchIndex, 
							vertex2Index );
						newNodeMatchCost += InsertedEdges( subGraph2Reverse, vertex2Index, 
							vertices2Matches );
						if ( ( ( newNodeMatchCost - MATCH_COST_SAFE_MARGINE ) < 
							maxMatchCost ) && ( ( newNodeMatchCost < currentBestCost ) ||
							( currentBestCost < 0 ) ) )
						{
							newNode = CreateMatchNode( expandingNode, vertexToMatchIndex,
								vertex2Index, newNodeMatchCost );
							InsertInLocalQueueInOrder( localMatchQueue, newNode );
						}
					}
					DestroyMatchNode( expandingNode );
					
					if( !quickMatch )    /* move all nodes in local queue to global queue */
						AddLocalMatches( globalMatchQueue, localMatchQueue );
					else
					{  /* move only the first node in the local queue to the global queue */
						newNode = RemoveNextLocalMatchNode( localMatchQueue );
						if ( newNode != NULL )
						{
							DestroyLocalMatchQueueNodes( localMatchQueue );
							InsertInGlobalMatchQueue( globalMatchQueue, newNode );
						}
					}
			}
			numberOfExpandedNodes++;
			if ( ( numberOfExpandedNodes > quickSearchThreshold ) && !quickMatch )
			{
				CompressGlobalMatchQueue( globalMatchQueue, 
					IntialQuickSearchNodesNumber( subGraph1 ) );
				quickMatch = TRUE;
			}
    }
    else
		DestroyMatchNode( expandingNode );
    expandingNode = RemoveNextMatchNode( globalMatchQueue );
  }
  if ( expandingNode != NULL )
	  DestroyMatchNode( expandingNode );
  Free( vertices1Matches );
  Free( vertices2Matches );
  Free( sortedVertices );
  DestroyGlobalMatchQueue( globalMatchQueue );
  DestroyLocalMatchQueue( localMatchQueue );
  
#ifdef _TIMING_
#ifdef _UNIX_
	times( &tmsEnd );
	GV->fuzzyMatchTm += ( tmsEnd.tms_utime - tmsStart.tms_utime ) / GV->clockTick;
#else // WIN32
	end = clock();
	GV->fuzzyMatchTm += ( end - start ) / GV->clockTick;
#endif
#endif
  
  return currentBestCost;
}


/*******************************************************************************
FUNCTION NAME: MaximumSearchNodes
INPUTS:		ULONG numberOfSubGraphVertices
RETURNS:	INT64
PURPOSE:	Calculate the value of quickSearchThreshold for fuzzymat.c:
			FuzzyMatch().
CALLED BY:	fuzzymat.c: InExactGraphMatch()
*******************************************************************************/

INT64 MaximumSearchNodes( ULONG numberOfSubGraphVertices )
{
	DOUBLE number;
	
#ifdef DEBUG_TRACE
	printf( "%s: MaximumSearchNodes()\n", __FILE__ );
#endif
	
	number = numberOfSubGraphVertices;
	return ( (INT64) pow( number, POWER_OF_VNUMBER_TO_MAX_SEARCH_NODES ) );
}


/*******************************************************************************
FUNCTION NAME: InExactGraphMatch
INPUTS:		PSUB_GRAPH subGraph1, 
			PREVERSE_SUB_GRAPH subGraph1Reverse,
			PSUB_GRAPH subGraph2, 
			PREVERSE_SUB_GRAPH subGraph2Reverse,
			DOUBLE matchCostMaxPercentOf2Size, 
			LONG *first, 
			LONG *second,
			ULONG initialMatchesCount, 
			LONG *matchingPattern
RETURNS:	DOUBLE
PURPOSE: 
CALLED BY:	extemp.c: GetStrongClasses()
			extemp.c: AddVENoise()
			extemp.c: AddENoise()
*******************************************************************************/

DOUBLE InExactGraphMatch( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph1,
						 PREVERSE_SUB_GRAPH subGraph1Reverse,
						 PSUB_GRAPH subGraph2,
						 PREVERSE_SUB_GRAPH subGraph2Reverse,
						 DOUBLE matchCostMaxPercentOf2Size,
						 ULONG *first, ULONG *second,
						 ULONG initialMatchesCount,
						 LONG *matchingPattern )
{
	DOUBLE matchCost;
	DOUBLE maxMatchCost;
	
#ifdef DEBUG_TRACE
	printf( "%s: InExactGraphMatch()\n", __FILE__ );
#endif
	
	GV->count++;
	
	maxMatchCost = matchCostMaxPercentOf2Size * SizeOf( subGraph2 );
	if ( subGraph1->numberOfVertices >= subGraph2->numberOfVertices )
		matchCost =
		FuzzyMatch( GV, subGraph1, subGraph1Reverse, subGraph2, subGraph2Reverse,
			maxMatchCost, 
			MaximumSearchNodes( subGraph1->numberOfVertices ),
			first, second, initialMatchesCount, matchingPattern, TRUE );
	else 
		matchCost =
		FuzzyMatch( GV, subGraph2, subGraph2Reverse, subGraph1, subGraph1Reverse,
			maxMatchCost,
			MaximumSearchNodes( subGraph2->numberOfVertices ),
			first, second, initialMatchesCount, matchingPattern, FALSE );
	
	return matchCost;
}


