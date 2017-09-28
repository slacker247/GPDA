/********************************************************************
*
* SUBDUE
*
* FILE NAME: matchq.c
*
********************************************************************/

#include "subdue.h"

/*******************************************************************************
FUNCTION NAME: InsertInGlobalMatchQueue
INPUTS:		PGLOBAL_MATCH_QUEUE queue, PMATCH_QUEUE_NODE newNode
RETURNS:	none
PURPOSE:	Insert newNode at the head of queue.
CALLED BY:	fuzzymat.c: FuzzyMatch()
			matchq.c: CreateGlobalMatchQueue()
*******************************************************************************/

void InsertInGlobalMatchQueue( PGLOBAL_MATCH_QUEUE queue, 
							  PMATCH_QUEUE_NODE  newNode )
{
#ifdef DEBUG_TRACE
	printf( "%s: InsertInGlobalMatchQueue()\n", __FILE__ );
#endif
	
	queue->QHead->nextNode->previousNode = newNode;
	newNode->nextNode = queue->QHead->nextNode;
	newNode->previousNode = queue->QHead;
	queue->QHead->nextNode = newNode;
	return;
}


/*******************************************************************************
FUNCTION NAME: CreateMatchNode
INPUTS:		PMATCH_QUEUE_NODE parentNode, 
			LONG first, 
			LONG second,
			DOUBLE matchCost
RETURNS:	PMATCH_QUEUE_NODE
PURPOSE:	Allocate and initialize a match queue node.
CALLED BY:	matchq.c: CreateGlobalMatchQueue()
*******************************************************************************/

PMATCH_QUEUE_NODE CreateMatchNode( PMATCH_QUEUE_NODE parentNode, LONG first,
								 LONG second, DOUBLE matchCost )
{
	PMATCH_QUEUE_NODE newNode;
	
#ifdef DEBUG_TRACE
	printf( "%s: CreateMatchNode()\n", __FILE__ );
#endif
	
	newNode = (PMATCH_QUEUE_NODE) Malloc( sizeof( MATCH_QUEUE_NODE ) );
	
	if ( parentNode == NULL )
	{
		newNode->depth = 0;
		newNode->matchedPairs = NULL;
		newNode->matchCost = 0.0;
	}
	else
	{
		newNode->depth = parentNode->depth + 1;
		newNode->matchCost = matchCost;
		newNode->matchedPairs = (PMATCHED_PAIR) Malloc( sizeof( MATCHED_PAIR ) *
			newNode->depth );
		memcpy( newNode->matchedPairs, parentNode->matchedPairs,
			sizeof( MATCHED_PAIR ) * parentNode->depth );
		newNode->matchedPairs[parentNode->depth].first = first;
		newNode->matchedPairs[parentNode->depth].second = second;
	}
	return newNode;
}


/*******************************************************************************
FUNCTION NAME: DestroyMatchNode
INPUTS:		PMATCH_QUEUE_NODE node
RETURNS:	none
PURPOSE:	Destroy a match node.
CALLED BY:	matchq.c: DestroyLocalMatchQueue()
			matchq.c: DestroyGlobalMatchQueue()
			fuzzymat.c: FuzzyMatch()
*******************************************************************************/

void DestroyMatchNode( PMATCH_QUEUE_NODE node )
{
	
#ifdef DEBUG_TRACE
	printf( "%s: DestroyMatchNode()\n", __FILE__ );
#endif
	
	if( node->depth > 0 )
		Free( node->matchedPairs );
	Free( node );
	return;
}


/*******************************************************************************
FUNCTION NAME: CreateGlobalMatchQueue
INPUTS:		ULONG *first, 
			ULONG *second, 
			ULONG initialMatchesCount
RETURNS:	PGLOBAL_MATCH_QUEUE
PURPOSE:	Allocate and initialize a global match queue data structure. The first
			node is initialized with the function parameter values.
CALLED BY:	fuzzymat.c: FuzzyMatch()
*******************************************************************************/

PGLOBAL_MATCH_QUEUE CreateGlobalMatchQueue( ULONG *first, ULONG *second, 
										  ULONG initialMatchesCount )
{
	PGLOBAL_MATCH_QUEUE newQueue;
	PMATCH_QUEUE_NODE newNode;
	ULONG index;
	
#ifdef DEBUG_TRACE
	printf( "%s: CreateGlobalMatchQueue()\n", __FILE__ );
#endif
	
	newQueue = (PGLOBAL_MATCH_QUEUE) Malloc( sizeof( GLOBAL_MATCH_QUEUE ) );
	newQueue->QHead = (PMATCH_QUEUE_NODE) Malloc( sizeof( MATCH_QUEUE_NODE ) );
	newQueue->QTail = (PMATCH_QUEUE_NODE) Malloc( sizeof( MATCH_QUEUE_NODE ) );
	newQueue->QHead->depth = -1;
	newQueue->QTail->depth = -1;
	newQueue->QHead->nextNode = newQueue->QTail;
	newQueue->QTail->previousNode = newQueue->QHead;
	newQueue->QTail->nextNode = newQueue->QTail;
	newNode = CreateMatchNode( NULL, 0, 0, 0.0 );   // dummy node to signal the
													// end of the queue (when 
													// initialMatchesCount == 0)
	if ( initialMatchesCount )                       
	{
		newNode->depth = initialMatchesCount;
		newNode->matchedPairs = (PMATCHED_PAIR) Malloc( sizeof( MATCHED_PAIR ) *
			newNode->depth );
		for ( index = 0; index < initialMatchesCount; index++ )
		{
			newNode->matchedPairs[index].first = first[index];
			newNode->matchedPairs[index].second = second[index];
		}
	}
	InsertInGlobalMatchQueue( newQueue, newNode );
	return newQueue;
}


/*******************************************************************************
FUNCTION NAME: DestroyGlobalMatchQueue
INPUTS:		PGLOBAL_MATCH_QUEUE queue
RETURNS:	none
PURPOSE:	Destroy the global match queue.
CALLED BY:	fuzzymat.c: FuzzyMatch()
*******************************************************************************/

void DestroyGlobalMatchQueue( PGLOBAL_MATCH_QUEUE queue )
{
	PMATCH_QUEUE_NODE currentNode;
	PMATCH_QUEUE_NODE nextNode;
	
#ifdef DEBUG_TRACE
	printf( "%s: DestroyGlobalMatchQueue()\n", __FILE__ );
#endif
	
	currentNode = queue->QHead->nextNode;
	while( currentNode->depth != -1)
	{
		nextNode = currentNode->nextNode;
		DestroyMatchNode( currentNode );
		currentNode = nextNode;
	}
	/*  DestroyMatchNode( currentNode ); */
	Free( queue->QHead );
	Free( queue->QTail );
	Free( queue );
	return;
}


/*******************************************************************************
FUNCTION NAME: DestroyMatchQueueNode
INPUTS:		PMATCH_QUEUE_NODE node 
RETURNS:	none
PURPOSE:	Remove node from the queue and destroy it.
CALLED BY:	matchq.c: CompressGlobalMatchQueue()
*******************************************************************************/

void DestroyMatchQueueNode( PMATCH_QUEUE_NODE node )
{
	
#ifdef DEBUG_TRACE
	printf( "%s: DestroyMatchQueueNode()\n", __FILE__ );
#endif
	
	node->nextNode->previousNode = node->previousNode;
	node->previousNode->nextNode = node->nextNode;
	DestroyMatchNode( node );
	return;
}


/*******************************************************************************
FUNCTION NAME: CompressGlobalMatchQueue
INPUTS:		PGLOBAL_MATCH_QUEUE queue, 
			ULONG compressedLength 
RETURNS:	none
PURPOSE:	Compress the global match queue by removing nodes that have a similar
			matchCost to a neighbor but have a shallower search depth, as well 
			as all nodes that are not within the desired compressedLength of 
			the queue.
CALLED BY:	fuzzymat.c: FuzzyMatch()
*******************************************************************************/

void CompressGlobalMatchQueue( PGLOBAL_MATCH_QUEUE queue, 
							  ULONG compressedLength )
{
	ULONG currentLength;
	PMATCH_QUEUE_NODE currentNode;
	PMATCH_QUEUE_NODE previousNode;
	PMATCH_QUEUE_NODE nextNode;
	
#ifdef DEBUG_TRACE
	printf( "%s: CompressGlobalMatchQueue()\n", __FILE__ );
#endif
	
	currentLength = 0;
	previousNode = queue->QHead->nextNode;
	if ( previousNode->depth == -1 )
		return;
	currentNode = previousNode->nextNode;
	
	while ( ( currentNode->depth != -1 ) && ( currentLength < compressedLength ) )
	{
		if ( floor( currentNode->matchCost ) != floor( previousNode->matchCost ) )
			if ( currentNode->depth < previousNode->depth )
			{
				DestroyMatchQueueNode( currentNode );
				currentNode = previousNode->nextNode;
			}
			else
			{
				currentLength++;
				previousNode = currentNode;
				currentNode = currentNode->nextNode;
			}
			else if ( currentNode->depth > previousNode->depth )
			{
				DestroyMatchQueueNode( previousNode );
				previousNode = currentNode;
				currentNode = currentNode->nextNode;
			}
			else
			{
				DestroyMatchQueueNode( currentNode );
				currentNode = previousNode->nextNode;
			}
	}
	while ( currentNode->depth != -1 )
	{
		nextNode = currentNode->nextNode;
		DestroyMatchQueueNode( currentNode );
		currentNode = nextNode;
	}
	return;
}


/*******************************************************************************
FUNCTION NAME: RemoveNextMatchNode
INPUTS:		PGLOBAL_MATCH_QUEUE queue
RETURNS:	PMATCH_QUEUE_NODE 
PURPOSE:	Remove the next match node from the head of queue.
CALLED BY:	fuzzymat.c: FuzzyMatch()
*******************************************************************************/

PMATCH_QUEUE_NODE RemoveNextMatchNode( PGLOBAL_MATCH_QUEUE queue )
{
	PMATCH_QUEUE_NODE node;
	
#ifdef DEBUG_TRACE
	printf( "%s: RemoveNextMatchNode()\n", __FILE__ );
#endif
	
	if ( queue->QHead->nextNode->depth != -1 )
	{
		node = queue->QHead->nextNode;
		queue->QHead->nextNode = node->nextNode;
		node->nextNode->previousNode = queue->QHead;
	}
	else
		node = NULL;
	return node;
}


/*******************************************************************************
FUNCTION NAME: RemoveNextLocalMatchNode
INPUTS:		PLOCAL_MATCH_QUEUE queue
RETURNS:	PMATCH_QUEUE_NODE
PURPOSE:	Remove the next node from the local match queue.
CALLED BY:	fuzzymat.c: FuzzyMatch()
			matchq.c: AddLocalMatches()
*******************************************************************************/

PMATCH_QUEUE_NODE RemoveNextLocalMatchNode( PLOCAL_MATCH_QUEUE queue )
{
	PMATCH_QUEUE_NODE node;
	
#ifdef DEBUG_TRACE
	printf( "%s: RemoveNextLocalMatchNode()\n", __FILE__ );
#endif
	
	if ( queue->QHead->nextNode->depth != -1 )
	{
		node = queue->QHead->nextNode;
		queue->QHead->nextNode = node->nextNode;
		node->nextNode->previousNode = queue->QHead;
	}
	else 
		node = NULL;
	return node;
}


/*******************************************************************************
FUNCTION NAME: AddLocalMatches
INPUTS:		PGLOBAL_MATCH_QUEUE globalQueue, 
			PLOCAL_MATCH_QUEUE localQueue
RETURNS:	none
PURPOSE:	Move the local match nodes to the global match queue.
CALLED BY:	fuzzymat.c: FuzzyMatch()
*******************************************************************************/

void AddLocalMatches( PGLOBAL_MATCH_QUEUE globalQueue, 
					 PLOCAL_MATCH_QUEUE localQueue )
{
	PMATCH_QUEUE_NODE node;
	
#ifdef DEBUG_TRACE
	printf( "%s: AddLocalMatches()\n", __FILE__ );
#endif
	
	while ( localQueue->QHead->nextNode->depth != -1 )
	{
		node = RemoveNextLocalMatchNode( localQueue );
		InsertInGlobalQueueInOrder( globalQueue, node );
	}
	localQueue->QHead->nextNode = localQueue->QTail;
	localQueue->QTail->previousNode = localQueue->QHead;
	localQueue->QTail->nextNode = localQueue->QTail;
	return;
}


/*******************************************************************************
FUNCTION NAME: CreateLocalMatchQueue
INPUTS:		none
RETURNS:	PLOCAL_MATCH_QUEUE
PURPOSE:	Create a local match queue data structure; allocate and initialize
			head and tail nodes.
CALLED BY:	fuzzymat.c: FuzzyMatch()
*******************************************************************************/

PLOCAL_MATCH_QUEUE CreateLocalMatchQueue( void )
{
	PLOCAL_MATCH_QUEUE newQueue;
	
#ifdef DEBUG_TRACE
	printf( "%s: CreateLocalMatchQueue()\n", __FILE__ );
#endif
	
	newQueue = (PLOCAL_MATCH_QUEUE) Malloc( sizeof( LOCAL_MATCH_QUEUE ) );
	newQueue->QHead = (PMATCH_QUEUE_NODE) Malloc( sizeof( MATCH_QUEUE_NODE ) );
	newQueue->QTail = (PMATCH_QUEUE_NODE) Malloc( sizeof( MATCH_QUEUE_NODE ) );
	newQueue->QHead->depth = -1;
	newQueue->QTail->depth = -1;
	newQueue->QHead->matchCost = 0.0;
	newQueue->QTail->matchCost = 0.0;
	newQueue->QHead->nextNode = newQueue->QTail;
	newQueue->QTail->previousNode = newQueue->QHead;
	newQueue->QTail->nextNode = newQueue->QTail;
	return newQueue;
}


/*******************************************************************************
FUNCTION NAME: InsertInLocalQueueInOrder
INPUTS:		PLOCAL_MATCH_QUEUE queue, PMATCH_QUEUE_NODE newNode
RETURNS:	none
PURPOSE:	Insert newNode in queue in order based on matchCost.
CALLED BY:	fuzzymat.c: FuzzyMatch()
*******************************************************************************/

void InsertInLocalQueueInOrder( PLOCAL_MATCH_QUEUE queue, 
							   PMATCH_QUEUE_NODE newNode )
{
	PMATCH_QUEUE_NODE currentNode;
	
#ifdef DEBUG_TRACE
	printf( "%s: InsertInLocalQueueInOrder()\n", __FILE__ );
#endif
	
	currentNode = queue->QHead->nextNode;
	while ( (currentNode->depth != -1) && 
			(currentNode->matchCost < newNode->matchCost) ) 
	{
		currentNode = currentNode->nextNode;
	}

	currentNode->previousNode->nextNode = newNode;
	newNode->previousNode = currentNode->previousNode;
	newNode->nextNode = currentNode;
	currentNode->previousNode = newNode;
	return;
}


/*******************************************************************************
FUNCTION NAME: InsertInGlobalQueueInOrder
INPUTS:		PGLOBAL_MATCH_QUEUE queue, 
			PMATCH_QUEUE_NODE newNode
RETURNS:	none
PURPOSE:	Insert newNode in queue, ordered on matchCost.
CALLED BY:	matchq.c: CreateGlobalMatchQueue()
*******************************************************************************/

void InsertInGlobalQueueInOrder( PGLOBAL_MATCH_QUEUE queue, 
								PMATCH_QUEUE_NODE newNode )
{
	PMATCH_QUEUE_NODE currentNode;
	
#ifdef DEBUG_TRACE
	printf( "%s: InsertInGlobalQueueInOrder()\n", __FILE__ );
#endif
	
	currentNode = queue->QHead->nextNode;
	while ( (currentNode->depth != -1) && 
			(currentNode->matchCost < newNode->matchCost) )
	{
		currentNode = currentNode->nextNode;
	}

	currentNode->previousNode->nextNode = newNode;
	newNode->previousNode = currentNode->previousNode;
	newNode->nextNode = currentNode;
	currentNode->previousNode = newNode;
	return;
}


/*******************************************************************************
FUNCTION NAME: DestroyLocalMatchQueue
INPUTS:		PLOCAL_MATCH_QUEUE queue
RETURNS:	none
PURPOSE:	Destroy the local match queue. 
CALLED BY:	fuzzymat.c: FuzzyMatch()
*******************************************************************************/

void DestroyLocalMatchQueue( PLOCAL_MATCH_QUEUE queue )
{
	PMATCH_QUEUE_NODE currentNode;
	PMATCH_QUEUE_NODE nextNode;
	
#ifdef DEBUG_TRACE
	printf( "%s: DestroyLocalMatchQueue()\n", __FILE__ );
#endif
	
	currentNode = queue->QHead->nextNode;
	while ( currentNode->depth != -1 )
	{
		nextNode = currentNode->nextNode;
		DestroyMatchNode( currentNode );
		currentNode = nextNode;
	}
	Free( queue->QHead );
	Free( queue->QTail );
	Free( queue );
	return;
}


/*******************************************************************************
FUNCTION NAME: DestroyLocalMatchQueueNodes
INPUTS:		PLOCAL_MATCH_QUEUE queue
RETURNS:	none
PURPOSE:	Empty the local match queue.
CALLED BY:	fuzzymat.c: FuzzyMatch()
*******************************************************************************/
void DestroyLocalMatchQueueNodes( PLOCAL_MATCH_QUEUE queue )
{
	PMATCH_QUEUE_NODE currentNode;
	PMATCH_QUEUE_NODE nextNode;
	
#ifdef DEBUG_TRACE
	printf( "%s: DestroyLocalMatchQueueNodes()\n", __FILE__ );
#endif
	
	currentNode = queue->QHead->nextNode;
	while ( currentNode->depth != -1 )
	{
		nextNode = currentNode->nextNode;
		DestroyMatchNode( currentNode );
		currentNode = nextNode;
	}
	queue->QHead->nextNode = queue->QTail;
	queue->QTail->previousNode = queue->QHead;
	queue->QTail->nextNode = queue->QTail;
	return;
}


