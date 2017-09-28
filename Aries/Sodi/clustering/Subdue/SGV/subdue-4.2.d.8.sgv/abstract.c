/********************************************************************
*
* SUBDUE
*
* FILE NAME: abstract.c
*
********************************************************************/


#include "subdue.h"



/*******************************************************************************
FUNCTION NAME: CreateAbstractSub
INPUTS:			PGRAPH_VARIABLES GV, 
				PSUB_GRAPH subGraph
RETURNS:		PABSTRACT_SUB
PURPOSE:
CALLED BY:		pvm.c: Communicate()
				concept.c: GetNegativeValue()
*******************************************************************************/

PABSTRACT_SUB CreateAbstractSub( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph )
{
	ULONG index;
	ULONG index2;
	ULONG globalEdgeIndex;
	PABSTRACT_SUB newAbstractSub;
	PGRAPH_VERTEX graphVertices;
	PGRAPH_VERTEX graphVertex;
	PSUB_GRAPH_VERTEX subGraphVertices;
	PSUB_GRAPH_EDGE subGraphEdges;
	
	graphVertices = GV->graph->vertices;
	subGraphVertices = subGraph->vertices;
	subGraphEdges = subGraph->edges;
	globalEdgeIndex = 0;
	
	newAbstractSub = (PABSTRACT_SUB) Malloc( sizeof( ABSTRACT_SUB ) );
	newAbstractSub->numberOfVertices = subGraph->numberOfVertices;
	newAbstractSub->numberOfEdges = subGraph->numberOfEdges;
//	newAbstractSub->numberOfUndirectedEdges = subGraph->numberOfUndirectedEdges;
	
	newAbstractSub->vertices = (PABSTRACT_SUB_VERTEX ) 
		Malloc( sizeof( ABSTRACT_SUB_VERTEX ) * subGraph->numberOfVertices );
	newAbstractSub->edges = (PABSTRACT_SUB_EDGE )
		Malloc( sizeof( ABSTRACT_SUB_EDGE ) * subGraph->numberOfEdges );
	
	for ( index = 0; index < subGraph->numberOfVertices; index++ )
	{
		graphVertex = &graphVertices[subGraphVertices[index].indexInGraph];
		
		newAbstractSub->vertices[index].labelIndex = graphVertex->labelIndex;
		newAbstractSub->vertices[index].edgesIndex =
			subGraphVertices[index].edgesIndex;
		newAbstractSub->vertices[index].numberOfEdges =
			subGraphVertices[index].numberOfEdges;
		newAbstractSub->vertices[index].covered = 0; 
		for ( index2 = 0; index2 < subGraphVertices[index].numberOfEdges; index2++ )
		{
			newAbstractSub->edges[globalEdgeIndex].labelIndex = 
				graphVertex->edges[subGraph->edges[globalEdgeIndex].
				indexInGraphVertex].labelIndex;
			newAbstractSub->edges[globalEdgeIndex].targetVertex =
				VertexIndexInSubGraph( subGraph, graphVertex->edges[subGraph->
				edges[globalEdgeIndex].indexInGraphVertex].targetVertexIndex );
			newAbstractSub->edges[globalEdgeIndex].directed = 
				graphVertex->edges[subGraph->edges[globalEdgeIndex].
				indexInGraphVertex].directed;
			newAbstractSub->edges[globalEdgeIndex].covered = 0;
			globalEdgeIndex++;
		}
	}	
	return newAbstractSub;
}


/*******************************************************************************
FUNCTION NAME: newAbstractSub
INPUTS:		none
RETURNS:	PABSTRACT_SUB
PURPOSE:	Allocate and initialize a new AbstracSub.
CALLED BY:	readgrph.c: ReadPredefSubs()
*******************************************************************************/

PABSTRACT_SUB NewAbstractSub( void )
{
	PABSTRACT_SUB abstractSub;
	
#ifdef DEBUG_TRACE
	printf( "%s: NewAbstractSub()\n", __FILE__ );
#endif
	
	abstractSub = (PABSTRACT_SUB) Malloc( sizeof( ABSTRACT_SUB ) );
	abstractSub->numberOfVertices = 0;
	abstractSub->numberOfEdges = 0;
//	abstractSub->numberOfUndirectedEdges = 0;
	abstractSub->vertices = NULL;
	abstractSub->edges = NULL;
	return abstractSub;
}


/*******************************************************************************
FUNCTION NAME: DestroyAbstractSub
INPUTS:		PABSTRACT_SUB abstractSub
RETURNS:	none
PURPOSE:	Free abstractSub's memory
CALLED BY:	subdue.c: DiscoverPredefSubs()
*******************************************************************************/

void DestroyAbstractSub( PABSTRACT_SUB abstractSub )
{
#ifdef DEBUG_TRACE
	printf( "%s: DestroyAbstractSub()\n", __FILE__ );
#endif
	
	if ( abstractSub->vertices != NULL )
		Free( abstractSub->vertices );
	if ( abstractSub->edges != NULL )
		Free( abstractSub->edges );

	Free( abstractSub );
}


/*******************************************************************************
FUNCTION NAME: CreateListOfAbstractSubs
INPUTS:		none
RETURNS:	PLIST_OF_ABSTRACT_SUBS
PURPOSE:	Allocate and initialize a ListOfAbstractSubs structure.
CALLED BY:	readgrph.c: ReadPredefSubs()
*******************************************************************************/

PLIST_OF_ABSTRACT_SUBS CreateListOfAbstractSubs( void )
{
	PLIST_OF_ABSTRACT_SUBS newList;
	
#ifdef DEBUG_TRACE
	printf( "%s: CreateListOfAbstractSubs()\n", __FILE__ );
#endif
	
	newList = (PLIST_OF_ABSTRACT_SUBS) Malloc( sizeof( LIST_OF_ABSTRACT_SUBS ) );
	newList->head = NULL;
	newList->tail = NULL;
	return newList;
}


/*******************************************************************************
FUNCTION NAME: InsertInAbstractSubsList
INPUTS:		PABSTRACT_SUB abstractSub, 
			PLIST_OF_ABSTRACT_SUBS list
RETURNS:	none
PURPOSE:	Insert abstractSub at the tail of list.
CALLED BY:	readgrph.c: ReadPredefSubs()
*******************************************************************************/

void InsertInAbstractSubsList( PABSTRACT_SUB abstractSub,
							  PLIST_OF_ABSTRACT_SUBS list  )
{
	PABSTRACT_SUB_NODE newNode;
	
#ifdef DEBUG_TRACE
	printf( "%s: InsertInAbstractSubsList()\n", __FILE__ );
#endif
	
	newNode = (PABSTRACT_SUB_NODE) Malloc( sizeof( ABSTRACT_SUB_NODE ) );
	newNode->abstractSub = abstractSub;
	newNode->next = NULL;
	if ( list->head == NULL )
	{
		list->head = newNode;
		list->tail = newNode;
	}
	else
	{
		list->tail->next = newNode;
		list->tail = newNode;
	}
	return;
}


/*******************************************************************************
FUNCTION NAME: RemoveNextAbstractSub
INPUTS:		PLIST_OF_ABSTRACT_SUBS list
RETURNS:	PABSTRACT_SUB
PURPOSE:	Remove the abstractSub at the head of list; return a pointer to it.
CALLED BY:	subdue.c: DiscoverPredefSub()
*******************************************************************************/

PABSTRACT_SUB RemoveNextAbstractSub( PLIST_OF_ABSTRACT_SUBS list )
{
	PABSTRACT_SUB abstractSub;
	PABSTRACT_SUB_NODE oldNode;
	
#ifdef DEBUG_TRACE
	printf( "%s: RemoveNextAbstractSub()\n", __FILE__ );
#endif
	
	if ( list == NULL || list->head == NULL )
		return NULL;
	abstractSub = list->head->abstractSub;
	oldNode = list->head;
	list->head = list->head->next;
	Free( oldNode );
	return abstractSub;
}


/*******************************************************************************
FUNCTION NAME: AddVertexToAbstractSub
INPUTS:		PABSTRACT_SUB abstractSub, 
			ULONG labelIndex
RETURNS:	none
PURPOSE:	Add a new vertex to abstractSub->vertices
CALLED BY:	readgrph.c: ReadPredefSubs()
*******************************************************************************/

void AddVertexToAbstractSub( PABSTRACT_SUB abstractSub, ULONG labelIndex )
{
	ULONG vertexIndex;
	
#ifdef DEBUG_TRACE
	printf( "%s: AddVertexToAbstractSub()\n", __FILE__ );
#endif
	
	abstractSub->numberOfVertices++;
/*	if ( abstractSub->numberOfVertices == 1 )
		abstractSub->vertices = (PABSTRACT_SUB_VERTEX) 
		Malloc( sizeof( ABSTRACT_SUB_VERTEX ) );
	else
*/		
	abstractSub->vertices = (PABSTRACT_SUB_VERTEX) Realloc( abstractSub->vertices, 
		sizeof( ABSTRACT_SUB_VERTEX ) * abstractSub->numberOfVertices );

	vertexIndex = abstractSub->numberOfVertices - 1;
	abstractSub->vertices[vertexIndex].labelIndex = labelIndex;
	abstractSub->vertices[vertexIndex].edgesIndex = 0;
	abstractSub->vertices[vertexIndex].numberOfEdges = 0;
	abstractSub->vertices[vertexIndex].covered = 0;
	return;
}


/*******************************************************************************
FUNCTION NAME: AddEdgeToAbstractSub
INPUTS:		PABSTRACT_SUB abstractSub, 
			ULONG sourceVertexIndex,
			ULONG targetVertexIndex, 
			ULONG labelIndex, 
			BOOLEAN directed
RETURNS:	none
PURPOSE:	Add an edge in abstractSub from sourceVertexIndex to targetVertexIndex
CALLED BY:	readgrph.c: ReadPredefSub()
*******************************************************************************/

void AddEdgeToAbstractSub( PABSTRACT_SUB abstractSub, ULONG sourceVertexIndex,
						  ULONG targetVertexIndex, ULONG labelIndex, 
						  BOOLEAN directed )
{
	ULONG index;
	ULONG vertexIndex;
	ULONG edgeIndex = 0;
	
#ifdef DEBUG_TRACE
	printf( "%s: AddEdgeToAbstractSub()\n", __FILE__ );
#endif
	
//{U}
	//if ( directed )
	//{
		/* allocate space for one more edge */
		abstractSub->edges = (PABSTRACT_SUB_EDGE) Realloc( abstractSub->edges,
			sizeof( ABSTRACT_SUB_EDGE ) * ( abstractSub->numberOfEdges + 1 ) );
	//}
	//else
	//{
		/* allocate space for two more edges */
	//	abstractSub->edges = (PABSTRACT_SUB_EDGE) Realloc( abstractSub->edges,
	//		sizeof( ABSTRACT_SUB_EDGE ) * ( abstractSub->numberOfEdges + 2 ) );
	//}
	
	/* add the forward edge */
	for ( vertexIndex = 0; vertexIndex <= sourceVertexIndex; vertexIndex++ )
		edgeIndex += abstractSub->vertices[vertexIndex].numberOfEdges;
	if ( edgeIndex != 0 )
	{
    /* find the new edge's place in the edges array - edges for each vertex
		are sorted by targetVertex */
		while ( edgeIndex > abstractSub->vertices[sourceVertexIndex].edgesIndex && 
			abstractSub->edges[edgeIndex - 1].targetVertex >
			targetVertexIndex )
			edgeIndex--;
		for ( index = abstractSub->numberOfEdges; index > edgeIndex; index-- )
			abstractSub->edges[index] = abstractSub->edges[index - 1];
	}
	abstractSub->edges[edgeIndex].labelIndex = labelIndex;
	abstractSub->edges[edgeIndex].targetVertex = targetVertexIndex;
	abstractSub->edges[edgeIndex].directed = directed;
	abstractSub->edges[edgeIndex].covered = 0;
	abstractSub->vertices[sourceVertexIndex].numberOfEdges++;
	abstractSub->numberOfEdges++;
	for ( vertexIndex = sourceVertexIndex + 1;
	vertexIndex < abstractSub->numberOfVertices; vertexIndex++ )
		abstractSub->vertices[vertexIndex].edgesIndex++;
	
	/* for undirected edge only: add the reverse edge */
	//{U}
/*	if ( !directed )
	{
		edgeIndex = 0;
		for ( vertexIndex = 0; vertexIndex <= targetVertexIndex; vertexIndex++ )
			edgeIndex += abstractSub->vertices[vertexIndex].numberOfEdges;
		if ( edgeIndex != 0 )
		{
		// find the new edge's place in the edges array - edges for each vertex
		// are sorted by targetVertex 
			while ( edgeIndex > abstractSub->vertices[sourceVertexIndex].edgesIndex &&
				abstractSub->edges[edgeIndex - 1].targetVertex >
				targetVertexIndex )
				edgeIndex--;
			
			//move the rest of the edges down in the array 
			for ( index = abstractSub->numberOfEdges; index > edgeIndex; index-- )
				abstractSub->edges[index] = abstractSub->edges[index - 1];
		}
		
		//write the new edge to the array 
		abstractSub->edges[edgeIndex].labelIndex = labelIndex;
		abstractSub->edges[edgeIndex].targetVertex = sourceVertexIndex;
		abstractSub->edges[edgeIndex].directed = FALSE;
		abstractSub->edges[edgeIndex].covered = 0;
		abstractSub->vertices[targetVertexIndex].numberOfEdges++;
		abstractSub->numberOfEdges++;
		abstractSub->numberOfUndirectedEdges++;
		
		//adjust edgesIndex for all vertices after targetVertex 
		for ( vertexIndex = targetVertexIndex + 1;
		vertexIndex < abstractSub->numberOfVertices; vertexIndex++ )
			abstractSub->vertices[vertexIndex].edgesIndex++;
	}
*/    
}


