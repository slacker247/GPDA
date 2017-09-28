/********************************************************************
*
* SUBDUE
*
* FILE NAME: subgphop.c
*
********************************************************************/



#include "subdue.h"


/*******************************************************************************
FUNCTION NAME: SameSubGraph
INPUTS:		PSUB_GRAPH first, 
			PSUB_GRAPH second
RETURNS:	BOOLEAN
PURPOSE:	Determine whether first and second represent the same subgraph.
CALLED BY:	subgphop.c: Member()
			subgphop.c: MemberSubGraph()
*******************************************************************************/

BOOLEAN SameSubGraph( PSUB_GRAPH first, PSUB_GRAPH second )
{
	ULONG vertexIndex;
	ULONG edgeIndex;
	PSUB_GRAPH_VERTEX firstVertex;
	PSUB_GRAPH_VERTEX secondVertex;
	
#ifdef DEBUG_TRACE
	printf( "%s: SameSubGraph()\n", __FILE__ );
#endif
	
	if ( (first->numberOfVertices != second->numberOfVertices) ||
		 (first->numberOfEdges != second->numberOfEdges) )
		return FALSE;
	
	for ( vertexIndex = 0; vertexIndex < first->numberOfVertices; vertexIndex++ )
	{
		firstVertex = &first->vertices[vertexIndex];
		secondVertex = &second->vertices[vertexIndex];
		if ( (firstVertex->indexInGraph != secondVertex->indexInGraph) ||
			 (firstVertex->numberOfEdges != secondVertex->numberOfEdges) )
			return FALSE;
		else
			for ( edgeIndex = 0; edgeIndex < firstVertex->numberOfEdges; edgeIndex++ )
				if ( first->edges[firstVertex->edgesIndex + edgeIndex].indexInGraphVertex != 
					second->edges[secondVertex->edgesIndex + edgeIndex].indexInGraphVertex )
					return FALSE;
	}

	return TRUE;
}


/*******************************************************************************
FUNCTION NAME: Member
INPUTS:		PSUB sub, 
			PLIST_OF_SUBS subsList
RETURNS:	BOOLEAN 
PURPOSE:	Determine whether sub is a member of subsList. See if the definition
			of sub matches all the instances of a sub in subsList.
CALLED BY:	psdiscover.c: FindInstancesOfPredefinedSub()
			subdue.c: Subdue()
			subdue.c: Discover()
*******************************************************************************/

BOOLEAN Member( PSUB sub, PLIST_OF_SUBS subsList )
{
	PSUB_GRAPH subGraph;
	PSUB currentSub;
	PSUB_GRAPH currentSubGraph;
	
#ifdef DEBUG_TRACE
	printf( "%s: Member()\n", __FILE__ );
#endif
	
	subGraph = sub->definition;
	InitSubsList( subsList );
	// for all subs in sublist
	while ( (currentSub = GetNextSub(subsList)) != NULL )
	{
		InitSubGraphsList( currentSub->instances );
		// for all instances of a sub
		while ( (currentSubGraph = GetNextSubGraph(currentSub->instances)) != NULL )
		{
			// if one instance matches the def of sub, it is a member of subsList
			if( SameSubGraph( currentSubGraph, subGraph ) ) {
				return TRUE;
			}
		}
	}

	return FALSE;
}


/*******************************************************************************
FUNCTION NAME: CreateSubGraph
INPUTS:		ULONG firstVertexIndex
RETURNS:	PSUB_GRAPH 
PURPOSE:	Allocate, initialize, and return a pointer to a SUB_GRAPH structure. 
CALLED BY:	subdue.c: InitialSubs()
*******************************************************************************/

PSUB_GRAPH CreateSubGraph( ULONG firstVertexIndex )
{
	PSUB_GRAPH newSubGraph;
	
#ifdef DEBUG_TRACE
	printf( "%s: CreateSubGraph()\n", __FILE__ );
#endif
	
	newSubGraph = (PSUB_GRAPH) Malloc( sizeof( SUB_GRAPH ) );
#ifdef DEBUG_MEMORY
	printf( "%s: CreateSubGraph(), line %d:\n", __FILE__, __LINE__ );
	printf( "Creating subGraph %x\n\n", (unsigned int) newSubGraph );
#endif
	newSubGraph->numberOfVertices = 1;
	newSubGraph->numberOfEdges = 0;
//	newSubGraph->numberOfUndirectedEdges = 0;
	newSubGraph->vertices = (PSUB_GRAPH_VERTEX) Malloc( sizeof( SUB_GRAPH_VERTEX ) );
#ifdef DEBUG_MEMORY
	printf( "%s: CreateSubGraph(), line %d:\n", __FILE__, __LINE__ );
	printf( "Adding vertices %x to subGraph %x\n\n", 
		(unsigned int) newSubGraph->vertices, (unsigned int) newSubGraph );
#endif
	newSubGraph->matchingOrder = (LONG *) Malloc( sizeof( LONG ) );
	newSubGraph->matchingOrder[0] = 0;
	newSubGraph->edges = NULL;
	newSubGraph->matchCost = 0;
	newSubGraph->uniqueCoverage = 0;
	newSubGraph->refCount = 0;
	
	newSubGraph->vertices[0].indexInGraph = firstVertexIndex;
	newSubGraph->vertices[0].numberOfEdges = 0;
	newSubGraph->vertices[0].edgesIndex = 0;
	
	return newSubGraph;
}


/*******************************************************************************
FUNCTION NAME: CreateSubGraphsList
INPUTS:		none 
RETURNS:	PLIST_OF_SUB_GRAPHS
PURPOSE:	Allocate and initialize a LIST_OF_SUB_GRAPHS structure.
CALLED BY:	subsop.c: CreateSub()
*******************************************************************************/

PLIST_OF_SUB_GRAPHS CreateSubGraphsList( void )
{
	PLIST_OF_SUB_GRAPHS newList;
	
#ifdef DEBUG_TRACE
	printf( "%s: CreateSubGraphsList()\n", __FILE__ );
#endif
	
	newList = (PLIST_OF_SUB_GRAPHS) Malloc( sizeof( LIST_OF_SUB_GRAPHS ) );
#ifdef DEBUG_MEMORY
	printf( "%s: CreateSubGraphsList(), line %d:\n", __FILE__, __LINE__ );
	printf( "Creating SubGraphs list %x\n\n", (unsigned int) newList );
#endif
	newList->currentLength = 0;
	newList->currentNode = NULL;
	newList->head = NULL;
	return newList;
}


/*******************************************************************************
FUNCTION NAME: InsertInSubGraphsList
INPUTS:		PLIST_OF_SUB_GRAPHS list, 
			PSUB_GRAPH newSubGraph
RETURNS:	none
PURPOSE:	Allocate a list node to hold newSubGraph; insert at the head of list.
CALLED BY:	predefsubop.c: InitialPredefinedSubs()
			extemp.c: CreateSubFromVETemplate()
			extemp.c: AddVENoise()
			extemp.c: CreateSubFromETemplate()
			subdue.c: InitialSubs()
*******************************************************************************/

void InsertInSubGraphsList( PLIST_OF_SUB_GRAPHS list, PSUB_GRAPH newSubGraph )
{
	PLIST_OF_SUB_GRAPHS_NODE newListNode;
	
#ifdef DEBUG_TRACE
	printf( "%s: InsertInSubGraphsList()\n", __FILE__ );
#endif
	
#ifdef DEBUG_MEMORY
	printf( "%s: InsertInSubGraphsList(), line %d:\n", __FILE__, __LINE__ );
	printf( "Inserting subGraph %x into list %x\n\n", (unsigned int) newSubGraph,
		(unsigned int) list );
#endif
	newListNode = (PLIST_OF_SUB_GRAPHS_NODE) 
		Malloc( sizeof( LIST_OF_SUB_GRAPHS_NODE ) );
	newListNode->subGraph = newSubGraph;
	newSubGraph->refCount++;
	newListNode->next = list->head;
	list->head = newListNode;
	list->currentLength++;
	return;
}


/*******************************************************************************
FUNCTION NAME: GetNextSubGraph
INPUTS:		PLIST_OF_SUB_GRAPHS list
RETURNS:	PSUB_GRAPH
PURPOSE:	Return the Subgraph in currentNode; set currentNode to 
			currentNode->next.
CALLED BY:	extend.c: ExtendInstances()
			extend.c: NoInstanceOverlap()
			subgphop.c: Member()
			subgphop.c: MemberSubGraph()
			tempop.c: CountOverlaps()
			eval.c: Connectivity()
			eval.c: Coverage()
			eval.c: Compactness()
*******************************************************************************/

PSUB_GRAPH GetNextSubGraph( PLIST_OF_SUB_GRAPHS list )
{
	PSUB_GRAPH nextSubGraph;
	
#ifdef DEBUG_TRACE
	printf( "%s: GetNextSubGraph()\n", __FILE__ );
#endif
	
	if ( list->currentNode == NULL )
		return NULL;
	nextSubGraph = list->currentNode->subGraph;
	list->currentNode = list->currentNode->next;
	return nextSubGraph;
}


/*******************************************************************************
FUNCTION NAME: InitSubGraphsList
INPUTS:		PLIST_OF_SUB_GRAPHS list
RETURNS:	none
PURPOSE:	Set list->currentNode to list->head.
CALLED BY:	extend.c: ExtendInstances()
			extend.c: NoInstanceOverlap()
			subgphop.c: Member()
			subgphop.c: MemberSubGraph() 
			tempop.c: CountOverlaps()
*******************************************************************************/

void InitSubGraphsList( PLIST_OF_SUB_GRAPHS list )
{
	
#ifdef DEBUG_TRACE
	printf( "%s: InitSubGraphsList()\n", __FILE__ );
#endif
	
	list->currentNode = list->head;
	return;
}


/*******************************************************************************
FUNCTION NAME: CopySubGraph
INPUTS:		PSUB_GRAPH subGraph, 
			DOUBLE matchCost
RETURNS:	PSUB_GRAPH
PURPOSE:	Make a copy of subGraph.
CALLED BY:	extemp.c: GetStrongClasses()
*******************************************************************************/

PSUB_GRAPH CopySubGraph( PSUB_GRAPH subGraph, DOUBLE matchCost )
{
	PSUB_GRAPH newSubGraph;
	
#ifdef DEBUG_TRACE
	printf( "%s: CopySubGraph()\n", __FILE__ );
#endif
	
	newSubGraph = (PSUB_GRAPH) Malloc( sizeof( SUB_GRAPH ) );
#ifdef DEBUG_MEMORY
	printf( "%s: CopySubGraph(), line %d:\n", __FILE__, __LINE__ );
	printf( "Creating subGraph %x\n\n", (unsigned int) newSubGraph );
#endif
	newSubGraph->numberOfVertices = subGraph->numberOfVertices;
	newSubGraph->numberOfEdges = subGraph->numberOfEdges;
//	newSubGraph->numberOfUndirectedEdges = subGraph->numberOfUndirectedEdges;
	newSubGraph->vertices = (PSUB_GRAPH_VERTEX)
		Malloc( newSubGraph->numberOfVertices * sizeof( SUB_GRAPH_VERTEX ) );
#ifdef DEBUG_MEMORY
	printf( "%s: CopySubGraph(), line %d:\n", __FILE__, __LINE__ );
	printf( "Adding vertices %x to subGraph %x\n\n", 
		(unsigned int) newSubGraph->vertices, (unsigned int) newSubGraph );
#endif
	if ( newSubGraph->numberOfEdges > 0 )
		newSubGraph->edges = (PSUB_GRAPH_EDGE)
		Malloc( newSubGraph->numberOfEdges * sizeof( SUB_GRAPH_EDGE ) );
	else
		newSubGraph->edges = NULL;
	newSubGraph->matchingOrder = (LONG *) 
		Malloc( sizeof( LONG ) * newSubGraph->numberOfVertices );
	newSubGraph->matchCost = matchCost;
	newSubGraph->uniqueCoverage = 0;
	newSubGraph->refCount = 0;
	memcpy( newSubGraph->vertices, subGraph->vertices,
		newSubGraph->numberOfVertices * sizeof( SUB_GRAPH_VERTEX ) );
	memcpy( newSubGraph->edges, subGraph->edges, newSubGraph->numberOfEdges *
		sizeof( SUB_GRAPH_EDGE ) );
	memcpy( newSubGraph->matchingOrder, subGraph->matchingOrder,
		newSubGraph->numberOfVertices * sizeof( LONG ) );
	
	return newSubGraph;
}


/*******************************************************************************
FUNCTION NAME: MemberSubGraph
INPUTS:		PSUB_GRAPH subGraph, 
			PLIST_OF_SUB_GRAPHS list
RETURNS:	BOOLEAN
PURPOSE:	Determine whether subGraph is already present in list.
CALLED BY:	extemp.c: CreateSubFromVETemplate()
			extemp.c: CreateSubFromETemplate()
*******************************************************************************/

BOOLEAN MemberSubGraph( PSUB_GRAPH subGraph, PLIST_OF_SUB_GRAPHS list )
{
	PLIST_OF_SUB_GRAPHS_NODE currentListState;
	PSUB_GRAPH currentSubGraph;
	
#ifdef DEBUG_TRACE
	printf( "%s: MemberSubGraph()\n", __FILE__ );
#endif
	
	currentListState = list->currentNode;
	InitSubGraphsList( list );
	while ( (currentSubGraph = GetNextSubGraph(list)) != NULL )
		if ( SameSubGraph(currentSubGraph, subGraph) )
		{
			list->currentNode = currentListState;
			return TRUE;
		}

	list->currentNode = currentListState;
	return FALSE;
}


/*******************************************************************************
FUNCTION NAME: SameReverseEdge
INPUTS:		PGRAPH_VARIABLES GV, 
			PGRAPH_EDGE edge1,
			PGRAPH_EDGE edge2, 
			ULONG edge2Source 
RETURNS:	BOOLEAN
PURPOSE:	Determine whether edge1 and edge2 are connected to the same pair of
			vertices, but in reverse directions.
CALLED BY:	subgphop.c: AddEdgeAndVertexToSubGraph()
			subgphop.c: AddEdgeToSubGraph()
*******************************************************************************/

BOOLEAN SameReverseEdge( PGRAPH_VARIABLES GV, PGRAPH_EDGE edge1, PGRAPH_EDGE edge2, 
					 ULONG edge2Source )
{
#ifdef DEBUG_TRACE
	printf( "%s: SameReverseEdge()\n", __FILE__ );
#endif
	
	if ( SameLabel( GV, edge1->labelIndex, edge2->labelIndex ) &&
		(edge1->targetVertexIndex == edge2Source) &&
		(edge1->directed == edge2->directed) )
		return TRUE;
	else
		return FALSE;
}


/*******************************************************************************
FUNCTION NAME: AddVertexToSubGraph
INPUTS:		PSUB_GRAPH subGraph, 
			PSUB_GRAPH oldSubGraph, 
			ULONG vertexIndexInGraph
RETURNS:	ULONG
PURPOSE:	Copy the vertices of oldSubGraph to subGraph, and add 
			vertexIndexInGraph to the vertices of subGraph, in its proper array
			position. The subGraph has a space for the new vertex and its number
			of vertices is already updated. Return the vertexIndex in Subgraph.
CALLED BY:	subgphop.c: AddEdgeAndVertexToSubGraph()
*******************************************************************************/

ULONG AddVertexToSubGraph( PSUB_GRAPH subGraph, PSUB_GRAPH oldSubGraph, 
						  ULONG vertexIndexInGraph )
{
	ULONG vertexIndex;
	
#ifdef DEBUG_TRACE
	printf( "%s: AddVertexToSubGraph()\n", __FILE__ );
#endif
	
	for ( vertexIndex = subGraph->numberOfVertices - 1; vertexIndex > 0; vertexIndex-- )
		if ( oldSubGraph->vertices[vertexIndex - 1].indexInGraph > vertexIndexInGraph )
			subGraph->vertices[vertexIndex] = oldSubGraph->vertices[vertexIndex - 1];
		else 
			break;

	subGraph->vertices[vertexIndex].indexInGraph = vertexIndexInGraph;
	subGraph->vertices[vertexIndex].numberOfEdges = 0;
	
	memcpy( &subGraph->vertices[0], &oldSubGraph->vertices[0],
		vertexIndex * sizeof( SUB_GRAPH_VERTEX ) );
	
	if ( vertexIndex < subGraph->numberOfVertices - 1 )
		subGraph->vertices[vertexIndex].edgesIndex = 
		subGraph->vertices[vertexIndex + 1].edgesIndex;
	else 
		subGraph->vertices[vertexIndex].edgesIndex = 
		subGraph->vertices[vertexIndex - 1].edgesIndex +
		subGraph->vertices[vertexIndex - 1].numberOfEdges;
	
	return vertexIndex;
}


/*******************************************************************************
FUNCTION NAME: AddEdgeToVertex
INPUTS:		PSUB_GRAPH subGraph, ULONG vertexIndex, ULONG graphEdgeIndex
RETURNS:	none
PURPOSE:	Insert graphEdgeIndex in the array of edges for vertexIndex in 
			subGraph.
CALLED BY:	subgphop.c: AddEdgeAndVertexToSubGraph()
			subgphop.c: AddEdgeToSubGraph()
*******************************************************************************/

void AddEdgeToVertex( PSUB_GRAPH subGraph, ULONG vertexIndex, 
					 ULONG graphEdgeIndex, LONG *newEdgeIndex )
{
	ULONG edgeIndex;
	PSUB_GRAPH_VERTEX sourceVertex;
	
#ifdef DEBUG_TRACE
	printf( "%s: AddEdgeToVertex()\n", __FILE__ );
#endif
	
	sourceVertex = &subGraph->vertices[vertexIndex];
	sourceVertex->numberOfEdges++;
	
	for ( edgeIndex = sourceVertex->edgesIndex + sourceVertex->numberOfEdges - 1;
	edgeIndex > sourceVertex->edgesIndex; edgeIndex-- )
		if ( subGraph->edges[edgeIndex - 1].indexInGraphVertex > graphEdgeIndex )
			subGraph->edges[edgeIndex] = subGraph->edges[edgeIndex - 1];
		else break;
		
		subGraph->edges[edgeIndex].indexInGraphVertex = graphEdgeIndex;
		if ( newEdgeIndex != NULL )
			*newEdgeIndex = (LONG) edgeIndex;
		return;
}


/*******************************************************************************
FUNCTION NAME: AddEdgeToSubGraph
INPUTS:		PGRAPH_VARIABLES GV, 
			PSUB_GRAPH subGraph, 
			ULONG sourceVertexIndex, 
			ULONG sourceIndexInGraph,
			ULONG targetVertexIndex, 
			ULONG addedEdgeIndex, 
			PGRAPH_EDGE addedEdge
RETURNS:	PSUB_GRAPH
PURPOSE: 
CALLED BY:	extemp.c: CreateEInstanceSubGraph()
*******************************************************************************/

PSUB_GRAPH AddEdgeToSubGraph( PGRAPH_VARIABLES GV, 
							 PSUB_GRAPH subGraph, 
							 ULONG sourceVertexIndex, 
							 ULONG sourceIndexInGraph,
							 ULONG targetVertexIndex, 
							 ULONG addedEdgeIndex, 
							 PGRAPH_EDGE addedEdge,
							 LONG *newEdgeIndex1, 
							 LONG *newEdgeIndex2 )
{
	PSUB_GRAPH newSubGraph;
	ULONG edgeIndex;
	PGRAPH_EDGE  targetVertexEdges;
	PGRAPH_VERTEX targetVertex;
	BOOLEAN add2Edges;
	ULONG vertexIndex;
	ULONG currentEdgeSource;
	ULONG currentEdgeDest;
	ULONG currentEdgesIndexInc;
	
#ifdef DEBUG_TRACE
	printf( "%s: AddEdgeToSubGraph()\n", __FILE__ );
#endif

//{U}
/*	if( !addedEdge->directed && 
		(sourceVertexIndex != targetVertexIndex) )
		add2Edges = TRUE;
	else 
*/		add2Edges = FALSE;
	
	/* Intialize newEdgeIndex2; if a second edge is added later on, 
	newEdgeIndex2 will be updated */
	if ( newEdgeIndex2 != NULL )
		*newEdgeIndex2 = -1;
	
	newSubGraph = (PSUB_GRAPH) Malloc( sizeof( SUB_GRAPH ) );
#ifdef DEBUG_MEMORY
	printf( "%s: AddEdgeToSubGraph(), line %d:\n", __FILE__, __LINE__ );
	printf( "Creating subGraph %x\n\n", (unsigned int) newSubGraph );
#endif
	newSubGraph->numberOfVertices = subGraph->numberOfVertices;

//{U}
/*	if( add2Edges )
	{
		newSubGraph->numberOfEdges = subGraph->numberOfEdges + 2;
		newSubGraph->numberOfUndirectedEdges = 
			subGraph->numberOfUndirectedEdges + 1;
	}
	else
	{
*/		newSubGraph->numberOfEdges = subGraph->numberOfEdges + 1;
//		newSubGraph->numberOfUndirectedEdges = subGraph->numberOfUndirectedEdges;
		targetVertexIndex = sourceVertexIndex;
//	}
	
	newSubGraph->vertices = (PSUB_GRAPH_VERTEX) 
		Malloc( newSubGraph->numberOfVertices * sizeof( SUB_GRAPH_VERTEX ) );
#ifdef DEBUG_MEMORY
	printf( "%s: AddEdgeToSubGraph(), line %d:\n", __FILE__, __LINE__ );
	printf( "Adding vertices %x to subGraph %x\n\n", 
		(unsigned int) newSubGraph->vertices, (unsigned int) newSubGraph );
#endif
	newSubGraph->matchingOrder = (LONG * ) 
		Malloc( sizeof( LONG ) * newSubGraph->numberOfVertices );
	newSubGraph->edges = (PSUB_GRAPH_EDGE) 
		Malloc( newSubGraph->numberOfEdges * sizeof( SUB_GRAPH_EDGE ) );
	newSubGraph->matchCost = 0.0;
	newSubGraph->uniqueCoverage = 0;
	newSubGraph->refCount = 0;
	memcpy( newSubGraph->vertices, subGraph->vertices, 
		newSubGraph->numberOfVertices * sizeof( SUB_GRAPH_VERTEX ) );
	memcpy( newSubGraph->matchingOrder, subGraph->matchingOrder,
		newSubGraph->numberOfVertices * sizeof( LONG ) );
	
	currentEdgeSource = 0;
	currentEdgeDest = 0;
	currentEdgesIndexInc = 0;
	for ( vertexIndex = 0; vertexIndex < newSubGraph->numberOfVertices;	vertexIndex++ )
	{
		newSubGraph->vertices[vertexIndex].edgesIndex += currentEdgesIndexInc;
		memcpy( &newSubGraph->edges[currentEdgeDest],
			&subGraph->edges[currentEdgeSource],
			subGraph->vertices[vertexIndex].numberOfEdges *
			sizeof( SUB_GRAPH_EDGE ) );
		currentEdgeSource += subGraph->vertices[vertexIndex].numberOfEdges;
		currentEdgeDest += newSubGraph->vertices[vertexIndex].numberOfEdges;
		if ( ( vertexIndex == sourceVertexIndex ) || 
			( vertexIndex == targetVertexIndex ) )
		{
			currentEdgesIndexInc++;
			currentEdgeDest++;
		}
	}
	
	AddEdgeToVertex( newSubGraph, sourceVertexIndex, addedEdgeIndex, newEdgeIndex1 );
	
	if( add2Edges )
	{
		targetVertex = &(GV->graph->vertices[subGraph->vertices[targetVertexIndex].
			indexInGraph]);
		targetVertexEdges = targetVertex->edges;
		for ( edgeIndex = 0; edgeIndex < targetVertex->numberOfEdges; edgeIndex++ )
			if ( SameReverseEdge( GV, &targetVertexEdges[edgeIndex], addedEdge,
				sourceIndexInGraph ) )
				break;
			AddEdgeToVertex( newSubGraph, targetVertexIndex, edgeIndex, newEdgeIndex2 );
	}
	return newSubGraph;
}


/*******************************************************************************
FUNCTION NAME: AddEdgeAndVertexToSubGraph
INPUTS:		PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph, 
			ULONG sourceVertexIndex,
			ULONG sourceIndexInGraph, 
			ULONG targetIndexInGraph,
			ULONG addedEdgeIndex, 
			PGRAPH_EDGE addedEdge, 
			BOOLEAN sourceEdge,
			ULONG *addedVertexIndex
RETURNS:	PSUB_GRAPH 
PURPOSE:	Add vertex targetIndexInGraph and edge addedEdge to a new copy of 
			subGraph; return the new copy.
CALLED BY:	extemp.c: CreateVEInstanceSubGraph()
*******************************************************************************/

PSUB_GRAPH AddEdgeAndVertexToSubGraph( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph, 
									  ULONG sourceVertexIndex,
									  ULONG sourceIndexInGraph, 
									  ULONG targetIndexInGraph,
									  ULONG addedEdgeIndex, 
									  PGRAPH_EDGE addedEdge, 
									  BOOLEAN sourceEdge,
									  LONG *newVertexIndex,
									  LONG *newEdgeIndex1,
									  LONG *newEdgeIndex2 ) 
{
	PSUB_GRAPH newSubGraph;
	ULONG targetVertexIndex;
	BOOLEAN add2Edges;
	ULONG vertexIndex;
	ULONG targetVertexPassed;
	ULONG currentEdgeSource;
	ULONG currentEdgeDest;
	ULONG currentEdgesIndexInc;
//{U}
//	PGRAPH_EDGE targetVertexEdges;
//	PGRAPH_VERTEX targetVertex;
//	ULONG edgeIndex;
	
#ifdef DEBUG_TRACE
	printf( "%s: AddEdgeAndVertexToSubGraph()\n", __FILE__ );
#endif
	
	newSubGraph = (PSUB_GRAPH) Malloc( sizeof( SUB_GRAPH ) );
#ifdef DEBUG_MEMORY
	printf( "%s: AddEdgeAndVertexToSubGraph(), line %d:\n", __FILE__, __LINE__ );
	printf( "Creating subGraph %x\n\n", (unsigned int) newSubGraph );
#endif
	newSubGraph->numberOfVertices = subGraph->numberOfVertices + 1;
	newSubGraph->vertices = (PSUB_GRAPH_VERTEX) 
		Malloc( newSubGraph->numberOfVertices * sizeof( SUB_GRAPH_VERTEX ) );
#ifdef DEBUG_MEMORY
	printf( "%s: AddEdgeAndVertexToSubGraph(), line %d:\n", __FILE__, __LINE__ );
	printf( "Adding vertices %x to subGraph %x\n\n", 
		(unsigned int) newSubGraph->vertices, (unsigned int) newSubGraph );
#endif
	newSubGraph->matchingOrder = (LONG *)
		Malloc( sizeof( LONG ) * newSubGraph->numberOfVertices );
	targetVertexIndex = AddVertexToSubGraph( newSubGraph, subGraph,	targetIndexInGraph );

	if ( targetVertexIndex <= sourceVertexIndex )
		sourceVertexIndex++;
	if ( newVertexIndex != NULL )
		*newVertexIndex = targetVertexIndex;
	
	/* Intialize newEdgeIndex2; if a second edge is added later on, 
	newEdgeIndex2 will be updated */
	if ( newEdgeIndex2 != NULL )
		*newEdgeIndex2 = -1;

	add2Edges = FALSE;
//{U}
/*	if ( sourceEdge && !addedEdge->directed)
		add2Edges = TRUE;
*/		


//{U}
/*	if ( add2Edges )
	{
		newSubGraph->numberOfEdges = subGraph->numberOfEdges + 2;
		newSubGraph->numberOfUndirectedEdges = 
			subGraph->numberOfUndirectedEdges + 1;
	}
	else
	{
*/		newSubGraph->numberOfEdges = subGraph->numberOfEdges + 1;
//		newSubGraph->numberOfUndirectedEdges = subGraph->numberOfUndirectedEdges;
//	}
	
	newSubGraph->edges = (PSUB_GRAPH_EDGE)
		Malloc( newSubGraph->numberOfEdges * sizeof( SUB_GRAPH_EDGE ) );
	newSubGraph->matchCost = 0.0;
	newSubGraph->uniqueCoverage = 0;
	newSubGraph->refCount = 0;
	currentEdgeSource = 0;
	currentEdgeDest = 0;
	currentEdgesIndexInc = 0;
	targetVertexPassed = 0;

	for ( vertexIndex = 0; vertexIndex < newSubGraph->numberOfVertices; vertexIndex++ )
	{
		if ( vertexIndex == targetVertexIndex )
		{
			targetVertexPassed = 1;
			newSubGraph->vertices[vertexIndex].edgesIndex += currentEdgesIndexInc;
			newSubGraph->matchingOrder[vertexIndex] = NEW_VERTEX;
			if ( add2Edges || !sourceEdge )
			{
				currentEdgeDest++;
				currentEdgesIndexInc++;
			}
		}
		else
		{
			newSubGraph->matchingOrder[vertexIndex] = 
				subGraph->matchingOrder[vertexIndex - targetVertexPassed];
			newSubGraph->vertices[vertexIndex].edgesIndex += currentEdgesIndexInc;
			memcpy( &newSubGraph->edges[currentEdgeDest],
				&subGraph->edges[currentEdgeSource],
				subGraph->vertices[vertexIndex-targetVertexPassed].numberOfEdges *
				sizeof( SUB_GRAPH_EDGE ) );
			currentEdgeSource += 
				subGraph->vertices[vertexIndex - targetVertexPassed].numberOfEdges;
			currentEdgeDest += 
				subGraph->vertices[vertexIndex - targetVertexPassed].numberOfEdges;
			if ( ( vertexIndex == sourceVertexIndex ) && sourceEdge )
			{
				currentEdgeDest++;
				currentEdgesIndexInc++;
			}
		}
	}
	
	/* if the edge is not from the source it is assumed to be directed */
	if ( !sourceEdge )
	{
		newSubGraph->edges[newSubGraph->vertices[targetVertexIndex].edgesIndex].
			indexInGraphVertex = addedEdgeIndex;
		if ( newEdgeIndex1 != NULL )
			*newEdgeIndex1 = newSubGraph->vertices[targetVertexIndex].edgesIndex;
		newSubGraph->vertices[targetVertexIndex].numberOfEdges++;
	}
	else
	{
//{U}
/*		if ( add2Edges )
		{
			targetVertex = &GV->graph->vertices[targetIndexInGraph];
			targetVertexEdges = targetVertex->edges;
			for ( edgeIndex = 0; edgeIndex < targetVertex->numberOfEdges;
			edgeIndex++ )
				if ( SameReverseEdge( GV, &targetVertexEdges[edgeIndex], addedEdge,
					sourceIndexInGraph ) )
					break;
				newSubGraph->edges[newSubGraph->vertices[targetVertexIndex].edgesIndex].
					indexInGraphVertex = edgeIndex;
				if ( newEdgeIndex2 != NULL )
					*newEdgeIndex2 = 
					(LONG) newSubGraph->vertices[targetVertexIndex].edgesIndex;
				newSubGraph->vertices[targetVertexIndex].numberOfEdges++;
		}
*/
		AddEdgeToVertex( newSubGraph, sourceVertexIndex, addedEdgeIndex,
			newEdgeIndex1 );
	}

	return newSubGraph;
}


/*******************************************************************************
FUNCTION NAME: DestroySubGraph
INPUTS:		PSUB_GRAPH subGraph
RETURNS:	none
PURPOSE:	Free the memory used by subGraph, as long as it is only being 
			referenced by one other data structure; else decrement refCount. 
CALLED BY:	extemp.c: GetStrongClasses()
			extemp.c: CreateSubFromVETemplate()
			extemp.c: CreateSubFromETemplate()
			extemp.c: AddVENoise()
			extemp.c: AddENoise()
			subgphop.c: DestroySubGraphsList()
*******************************************************************************/

void DestroySubGraph( PSUB_GRAPH subGraph )
{
#ifdef DEBUG_TRACE
	printf( "%s: DestroySubGraph()\n", __FILE__ );
#endif
	
	if ( subGraph->refCount > 1 )
	{
#ifdef DEBUG_MEMORY
		printf( "%s: DestroySubGraph(), line %d:\n", __FILE__, __LINE__ );
		printf( "Decrementing Refcount = %d ", subGraph->refCount );
#endif
		subGraph->refCount--;
#ifdef DEBUG_MEMORY
		printf( "to refCount = %d for subGraph %x\n\n", subGraph->refCount,
			(unsigned int) subGraph );
#endif
		return;
	}

#ifdef DEBUG_MEMORY
	printf( "%s: DestroySubGraph(), line %d:\n", __FILE__, __LINE__ );
	printf( "Destroying subGraph %x, refCount = %d\n\n", (unsigned int) subGraph, 
		subGraph->refCount );
#endif
	Free( subGraph->vertices );
	Free( subGraph->edges );
	Free( subGraph->matchingOrder );
	Free( subGraph ); 
	return;
}


/*******************************************************************************
FUNCTION NAME: DestroySubGraphsList
INPUTS:		PLIST_OF_SUB_GRAPHS list
RETURNS:	none
PURPOSE:	Destroy list.
CALLED BY:	subsop.c: DestroySub()
*******************************************************************************/

void DestroySubGraphsList( PLIST_OF_SUB_GRAPHS list )
{
	PLIST_OF_SUB_GRAPHS_NODE listNode;
	
#ifdef DEBUG_TRACE
	printf( "%s: DestroySubGraphsList()\n", __FILE__ );
#endif
	
#ifdef DEBUG_MEMORY
	printf( "%s: DestroySubGraphsList(), line %d:\n", __FILE__, __LINE__ );
	printf( "Destroying list %x\n\n", (unsigned int) list );
#endif
	while ( list->head != NULL )
	{
		listNode = list->head;
		list->head = listNode->next;
		DestroySubGraph( listNode->subGraph );
		Free( listNode );
	}
	Free( list );
	return;
}


/*******************************************************************************
FUNCTION NAME: SizeOf
INPUTS:		PSUB_GRAPH subGraph
RETURNS:	ULONG
PURPOSE:	Return the size of subGraph in terms of the number of its vertices and
			edges.
CALLED BY:	extemp.c: CreateSubFromVETemplate()
			extemp.c: AddVENoise()
			eval.c:	  EvaluateSub()
			extemp.c: CreateSubFromETemplate()
			extemp.c: AddENoise()
*******************************************************************************/

ULONG SizeOf( PSUB_GRAPH subGraph )
{
#ifdef DEBUG_TRACE
	printf( "%s: SizeOf()\n", __FILE__ );
#endif
	
	return ( subGraph->numberOfVertices + subGraph->numberOfEdges/* - 
		subGraph->numberOfUndirectedEdges*/ );
}


/*******************************************************************************
FUNCTION NAME: SubGraphsListLength
INPUTS:		PLIST_OF_SUB_GRAPHS listOfSubGraphs
RETURNS:	ULONG
PURPOSE:	Return listOfSubGraphs->currentLength.
CALLED BY:	eval.c: Connectivity()
			compress.c: CompressUsing()
			compress.c: SizeOfCompressedGraph()
*******************************************************************************/

ULONG SubGraphsListLength( PLIST_OF_SUB_GRAPHS listOfSubGraphs )
{
#ifdef DEBUG_TRACE
	printf( "%s: SubGraphsListLength()\n", __FILE__ );
#endif
	
	return listOfSubGraphs->currentLength;
}


