/********************************************************************
*
* SUBDUE
*
* FILE NAME: subsop.c
*
********************************************************************/



#include "subdue.h"


/*******************************************************************************
FUNCTION NAME: CreateSubsList
INPUTS:		ULONG maxLength
RETURNS:	PLIST_OF_SUBS
PURPOSE:	Allocate and initialize a LIST_OF_SUBS data structure.
CALLED BY:	predefsubop.c: ProcessPredefinedSubs()
			predefsubop.c: InitialPredefinedSubs()
			psdiscover.c: FindInstancesOfPredefinedSub()
			subdue.c: Discover()
			subdue.c: InitialSubs()
			subdue.c: Subdue()
*******************************************************************************/

PLIST_OF_SUBS CreateSubsList( ULONG maxLength )
{
	PLIST_OF_SUBS newList;
	
#ifdef DEBUG_TRACE
	printf( "%s: CreateSubsList()\n", __FILE__ );
#endif
	
	if ( maxLength < 1 )
		ErrorFatal( "subsop.c: CreateSubsList(): attempt to allocate a list with maximum elements < 1\n" );
	
	newList = (PLIST_OF_SUBS) Malloc( sizeof( LIST_OF_SUBS ) );
#ifdef DEBUG_MEMORY
	printf( "%s: CreateSubsList(), line %d:\n", __FILE__, __LINE__ );
	printf( "Creating Subs list %x\n\n", (UINT) newList );
#endif
	newList->maxLength = maxLength;
	newList->currentLength = 0;
	newList->numberOfValues = 0;		// counting the number of different values on the Q
										// for value based Q length
	newList->head = NULL;
	newList->tail = NULL;
	
	return newList;
}


/*******************************************************************************
FUNCTION NAME: DestroySub
INPUTS:		PSUB sub
RETURNS:	none
PURPOSE:	Free the memory used by sub.
CALLED BY:	subdue.c: Subdue()
*******************************************************************************/

void DestroySub( PSUB sub )
{
	
#ifdef DEBUG_TRACE
	printf( "%s: DestroySub()\n", __FILE__ );
#endif
	
	if ( sub == NULL )
		return ;
#ifdef DEBUG_MEMORY
	printf( "%s: DestroySub(), line %d:\n", __FILE__, __LINE__ );
	printf( "Destroying sub %x\n\n", (UINT) sub );
#endif
	DestroySubGraph( sub->definition );
	DestroySubGraphsList( sub->instances );
	Free( sub );
	return;
}

#ifdef BEAM_LENGTH_BY_VALUE
/*******************************************************************************
FUNCTION NAME: InsertInSubsListInOrder
INPUTS:		PGRAPH_VARIABLES GV, 
			PLIST_OF_SUBS list, 
			PSUB newSub
RETURNS:	BOOLEAN: if newSub is in list at return
PURPOSE:	Allocate a list node for newSub, insert in list in descending order
			by value.  If there are more distinct values than maxLength, remove
			all nodes from the end of list that represent the smallest value.
			If sub is not inserted, it is destroyed!
CALLED BY:	predefsubop.c: InitialPredefinedSubs()
			psdiscover.c: FindInstancesOfPredefinedSub()
			extemp.c: GetBestSubs()
			subdue.c: Subdue()
			subdue.c: Discover()

  This version of this function is vritten by Istvan Jonyer.  The point to this
  is that there is a certain level of arbitariness in Subdue if we consider the
  number of subs in the queue only, since we are arbitrarily bumping things off 
  at the end of the queue.  This function will adjust the queue length such 
  that there are only "maxLength" number of different values represented by all
  the subs in the queue.
*******************************************************************************/

BOOLEAN InsertInSubsListInOrder( PGRAPH_VARIABLES GV, 
								 PLIST_OF_SUBS list, 
								 PSUB newSub,
								 BOOLEAN destroyNegativeSub )
{
	PLIST_OF_SUBS_NODE newListNode;
	PLIST_OF_SUBS_NODE currentListNode;
	PLIST_OF_SUBS_NODE preListNode;
	PLIST_OF_SUBS_NODE tempNodePtr;
	PLIST_OF_SUBS_NODE tempNodePtr2;
	PLIST_OF_SUBS_NODE listOfLastValue;
	ULONG valueCount;
	DOUBLE previousValue;
	BOOLEAN wasInserted;						// newSub is in the list at return
	
#ifdef DEBUG_TRACE
	printf( "%s: InsertInSubsListInOrder()\n", __FILE__ );
#endif

	wasInserted = TRUE;
	
	// destroy newSub if it does not have the user-specified minimum number of instances 
	if ( (DOUBLE) newSub->instances->currentLength < GV->minPercent * GV->numPosExamples )
	{
		/*printf( "Call 5: sub %lu\n", (ULONG) newSub );*/
		if ( GV->negativeGraph && destroyNegativeSub )
			FindAndDestroyNegativeSub( GV->negativeGraph, (ULONG) newSub );

		DestroySub( newSub );
		return FALSE;								// new sub is not inserted into list
	}
	
	currentListNode = list->head;
	preListNode = NULL;
	valueCount = 0;
	previousValue = POSITIVE_INFINITY_DOUBLE;
	while ( currentListNode != NULL )						// search the whole list
	{															// for sub of less value
		if ( currentListNode->sub->value > newSub->value )
		{
			if (currentListNode->sub->value < previousValue) {
				valueCount++;
				previousValue = currentListNode->sub->value;		// set previous value
				if (valueCount >= list->maxLength) {
					DestroySub( newSub );					// destroy sub
					return FALSE;							// new sub is not inserted into list,
															// since it passed maxLength number
															// of larger values
				}
			}
		}
		else												// found -> this is where we'll insert
			break;											// the new sub

		preListNode = currentListNode;						// get next node
		currentListNode = currentListNode->next;
	}

	// create new list node for the sub
	newListNode = (PLIST_OF_SUBS_NODE) Malloc( sizeof( LIST_OF_SUBS_NODE ) );
	newListNode->sub = newSub;
	newListNode->next = NULL;
	newListNode->previous = NULL;
	
	// insert new sub into the list
	if ( list->head == NULL )							// if list is empty
	{
		list->head = newListNode;
		list->tail = newListNode;
		list->currentLength = 1;
	}
	else {
		if( preListNode == NULL ) {						// if this is the best sub, insert
			newListNode->next = list->head;				// to head of list
			list->head->previous = newListNode;
			list->head = newListNode;
		}
		else {
			newListNode->next = currentListNode;
			newListNode->previous = preListNode;
			preListNode->next = newListNode;
			if ( currentListNode != NULL )				// if not inserted at the end
				currentListNode->previous = newListNode;
			else {										// if new node is inserted at the end
				list->tail = newListNode;
			}
		}
		list->currentLength++;
	}

	// count the number of different values in queue
	tempNodePtr = list->head;							// get head of list
	previousValue = POSITIVE_INFINITY_DOUBLE;
	valueCount = 0;
	while (tempNodePtr != NULL) {						// while not end of list
		if (tempNodePtr->sub->value < previousValue) {		// if this is smaller than the previous
			valueCount++;										// increase counter
			previousValue = tempNodePtr->sub->value;			// set previous value
			listOfLastValue = tempNodePtr;						// remember where last value starts
		}
		tempNodePtr = tempNodePtr->next;					// get next node
	}
	list->numberOfValues = valueCount;

	// if maximum length of the list is exceeded, truncate it
	if ( valueCount > list->maxLength )
	{            
		/* maintain beam length by removing the last nodes if necessary */
		preListNode = listOfLastValue->previous;
		tempNodePtr = listOfLastValue;						// get first node of last value
		while (tempNodePtr != NULL) {						// while not end of list
			if ( GV->negativeGraph && destroyNegativeSub )		// take care of negative sub
				FindAndDestroyNegativeSub( GV->negativeGraph, (ULONG) tempNodePtr->sub );
			
			DestroySub( tempNodePtr->sub );						// distroy sub
			tempNodePtr2 = tempNodePtr;							// save this pointer
			tempNodePtr = tempNodePtr->next;					// get next node
			if (newListNode == tempNodePtr2)
				wasInserted = FALSE;		// newSub is not inserted (ie. inserted, but kicked out)
			Free( tempNodePtr2 );								// destroy this node
			list->currentLength--;								// decrement list length
		}

		preListNode->next = NULL;
		list->tail = preListNode;
		list->numberOfValues = list->maxLength;
	}

	return wasInserted;
}

#else	// not BEAM_LENGTH_BY_VALUE, but original (BEAM_LENGTH_BY_QUEUE_LENGTH)

/*******************************************************************************
FUNCTION NAME: InsertInSubsListInOrder
INPUTS:		PGRAPH_VARIABLES GV, 
			PLIST_OF_SUBS list, 
			PSUB newSub
RETURNS:	BOOLEAN: if newSub is in list at return
PURPOSE:	Allocate a list node for newSub, insert in list in descending order
			by value.  If the length of list exceeds its maxLength, remove a 
			node from the end of list.
CALLED BY:	predefsubop.c: InitialPredefinedSubs()
			psdiscover.c: FindInstancesOfPredefinedSub()
			extemp.c: GetBestSubs()
			subdue.c: Subdue()
			subdue.c: Discover()
*******************************************************************************/

BOOLEAN InsertInSubsListInOrder( PGRAPH_VARIABLES GV, PLIST_OF_SUBS list, PSUB newSub,
							 BOOLEAN destroyNegativeSub )
{
	PLIST_OF_SUBS_NODE newListNode;
	PLIST_OF_SUBS_NODE currentListNode;
	PLIST_OF_SUBS_NODE preListNode;
	BOOLEAN insertedAtTail;						// newSub is inserted at tail of list
	BOOLEAN wasInserted;						// newSub is in the list at return
	
#ifdef DEBUG_TRACE
	printf( "%s: InsertInSubsListInOrder()\n", __FILE__ );
#endif

	insertedAtTail = FALSE;
	wasInserted = TRUE;
	
	// destroy newSub if it does not have the user-specified minimum number of instances 
	if ( (DOUBLE) newSub->instances->currentLength < GV->minPercent * GV->numPosExamples )
	{
		/*printf( "Call 5: sub %lu\n", (ULONG) newSub );*/
		if ( GV->negativeGraph && destroyNegativeSub )
			FindAndDestroyNegativeSub( GV->negativeGraph, (ULONG) newSub );
		DestroySub( newSub );
		return FALSE;								// new sub is not inserted into list
	}
	
	currentListNode = list->head;
	preListNode = NULL;
	while ( currentListNode != NULL )						// search the whole list
	{															// for sub of less value
		if ( currentListNode->sub->value > newSub->value )
		{
			preListNode = currentListNode;						// get next node
			currentListNode = currentListNode->next;
		}
		else												// found -> this is where we'll insert
			break;											// the new sub
	}

	// create new sub
	newListNode = (PLIST_OF_SUBS_NODE) Malloc( sizeof( LIST_OF_SUBS_NODE ) );
	newListNode->sub = newSub;
	newListNode->next = NULL;
	newListNode->previous = NULL;
	
	// insert new sub into the list
	if ( list->head == NULL )							// if list is empty
	{
		list->head = newListNode;
		list->tail = newListNode;
		list->currentLength = 1;
	}
	else {
		if( preListNode == NULL ) {						// if this is the best sub, insert
			newListNode->next = list->head;				// to head of list
			list->head->previous = newListNode;
			list->head = newListNode;
		}
		else {
			newListNode->next = currentListNode;
			newListNode->previous = preListNode;
			preListNode->next = newListNode;
			if ( currentListNode != NULL )				// if not inserted at the end
				currentListNode->previous = newListNode;
			else {										// if new node is inserted at the end
				list->tail = newListNode;
				insertedAtTail = TRUE;
			}
		}
		list->currentLength++;
	}

	// if maximum length of the list is exceeded, truncate it
	if ( list->currentLength > list->maxLength )
	{            
		/* maintain beam length by removing the last node if necessary */
		preListNode = list->tail->previous;
		/*printf( "Call 6: sub %lu\n", (ULONG) list->tail->sub );*/
		if ( GV->negativeGraph && destroyNegativeSub )
			FindAndDestroyNegativeSub( GV->negativeGraph, (ULONG) list->tail->sub );
		DestroySub( list->tail->sub );
		Free( list->tail );
		preListNode->next = NULL;
		list->tail = preListNode;
		list->currentLength--;

		if (insertedAtTail)			// if newSub was inserted at tail, it was truncated here
			wasInserted = FALSE;		// newSub is not inserted (ie. inserted, but kicked out)
	}

	return wasInserted;
}
#endif	// BEAM_LENGTH_BY_VALUE

/******************************************************************************
FUNCTION NAME: InsertInSubsListInOrder2
INPUTS       : PGRAPH_VARIABLES GV, PLIST_OF_SUBS list, PSUB newSub,
               BOOLEAN destroyNegativeSub
RETURNS      : BOOLEAN if newSub is in list at return, but calls the display
               routine if the GV->display flag is set.
PURPOSE      : To display the new best substructure if the display flag is set 
               and the substructure  is inserted in the substructures list.
CALLED BY    : subdue.c : Discover() 
******************************************************************************/

BOOLEAN InsertInSubsListInOrder2( PGRAPH_VARIABLES GV, PLIST_OF_SUBS list, PSUB newSub,
							 BOOLEAN destroyNegativeSub )
{
   BOOLEAN newBest;

   newBest = InsertInSubsListInOrder(GV,list,newSub,destroyNegativeSub);
  
   if(GV->display && newBest)
     GraphDisplay(GV,newSub);
                             // function to send command over socket
                             // to 
                             // display the new substructure
   return newBest;  

}

/*******************************************************************************
FUNCTION NAME: CreateSub
INPUTS:		PSUB_GRAPH definitionSubGraph
RETURNS:	PSUB 
PURPOSE:	Allocate and initialize a SUB data structure.
CALLED BY:	predefsubop.c: InitialPredefinedSubs()
			extemp.c: CreateSubFromVETemplate()
			extemp.c: CreateSubFromETemplate()
			subdue.c: InitialSubs()
*******************************************************************************/

PSUB CreateSub( PSUB_GRAPH definitionSubGraph )
{
	PSUB newSub;
	
#ifdef DEBUG_TRACE
	printf( "%s: CreateSub()\n", __FILE__ );
#endif
	
	newSub = (PSUB) Malloc( sizeof( SUB ) );
#ifdef DEBUG_MEMORY
	printf( "%s: CreateSub(), line %d:\n", __FILE__, __LINE__ );
	printf( "Creating sub %x\n\n", (unsigned int) newSub );
#endif
	newSub->definition = definitionSubGraph;
	definitionSubGraph->refCount++;
	newSub->instances = CreateSubGraphsList();
	newSub->value = 0;
	newSub->totalInstancesSize = 0;
	return newSub;
}


/*******************************************************************************
FUNCTION NAME: CopySub
INPUTS:		PSUB oldSub
RETURNS:	PSUB
PURPOSE:	Make a copy of oldSub.  Note that the subgraphs are not copied; 
			newSub's subgraph pointers point to oldSub's subgraphs (RefCounts 
			are incremented).
CALLED BY:	concept.c: GetNegativeValue()
*******************************************************************************/

PSUB CopySub( PSUB oldSub )
{
	PSUB newSub;
	PSUB_GRAPH subGraph;
	
#ifdef DEBUG_TRACE
	printf( "%s: CopySub()\n", __FILE__ );
#endif
	
	newSub = (PSUB) Malloc( sizeof( SUB ) );
	newSub->definition = oldSub->definition;
	newSub->definition->refCount++;
	
	newSub->instances = CreateSubGraphsList();
	InitSubGraphsList( oldSub->instances );
	while ( ( subGraph = GetNextSubGraph( oldSub->instances ) ) != NULL )
		InsertInSubGraphsList( newSub->instances, subGraph );
	
	newSub->value = oldSub->value;
	newSub->totalInstancesSize = oldSub->totalInstancesSize;
	newSub->newVertex = oldSub->newVertex;
	newSub->newEdge1 = oldSub->newEdge1;
	newSub->newEdge2 = oldSub->newEdge2;
	
	return newSub;
}


/*******************************************************************************
FUNCTION NAME: DestroySubsList
INPUTS:		PLIST_OF_SUBS list
RETURNS:	none
PURPOSE: 
CALLED BY:	subdue.c: Subdue()
			subdue.c: Discover()
*******************************************************************************/

void DestroySubsList( PLIST_OF_SUBS list )
{
	PLIST_OF_SUBS_NODE listNode;
	
#ifdef DEBUG_TRACE
	printf( "%s: DestroySubsList()\n", __FILE__ );
#endif
	
#ifdef DEBUG_MEMORY
	printf( "%s: DestroySubsList(), line %d:\n", __FILE__, __LINE__ );
	printf( "Destroying Subs list %x\n\n", (unsigned int) list );
#endif
	while ( list->head != NULL )
	{
		listNode = list->head;
		list->head = listNode->next;
		DestroySub( listNode->sub );
		Free( listNode );
	}
	Free( list );
	return;
}


/*******************************************************************************
FUNCTION NAME: RemoveNextSub
INPUTS:		PLIST_OF_SUBS list
RETURNS:	PSUB
PURPOSE:	If the list is not empty, return the sub in the head node and Free the
			head node.
CALLED BY:	psdiscover.c: FindInstancesOfPredefinedSub()
			subdue.c: Discover()
			subdue.c: Subdue()
*******************************************************************************/

PSUB RemoveNextSub( PLIST_OF_SUBS list )
{
	PSUB nextSub;
	PLIST_OF_SUBS_NODE oldNode;
	
#ifdef DEBUG_TRACE
	printf( "%s: RemoveNextSub()\n", __FILE__ );
#endif
	
	if ( list->head == NULL )
		return NULL;

	oldNode = list->head;
	nextSub = list->head->sub;
	list->head = list->head->next;
	
	if ( list->head != NULL ) {
		list->head->previous = NULL;
		if (oldNode->sub->value > list->head->sub->value)
			list->numberOfValues--;
	}
	else {
		list->tail = NULL;
		list->numberOfValues = 0;
	}
	
	list->currentLength--;

	Free( oldNode );
	
	return nextSub;
}


/*******************************************************************************
FUNCTION NAME: GetNextSub
INPUTS:		PLIST_OF_SUBS list
RETURNS:	PSUB
PURPOSE:	Return the sub in currentNode; set currentNode to currentNode->next
CALLED BY:	main.c: main()
			subgphop.c: Member()
*******************************************************************************/

PSUB GetNextSub( PLIST_OF_SUBS list )
{
	PSUB nextSub;
	
#ifdef DEBUG_TRACE
	printf( "%s: GetNextSub()\n", __FILE__ );
#endif
	
	if ( list->currentNode == NULL )
		return NULL;
	nextSub = list->currentNode->sub;
	list->currentNode = list->currentNode->next;
	return nextSub;
}


/*******************************************************************************
FUNCTION NAME: InitSubsList
INPUTS:		PLIST_OF_SUBS list
RETURNS:	none
PURPOSE:	Set currentNode of list to head.
CALLED BY:	main.c: main()
			subgphop.c: Member()
*******************************************************************************/

void InitSubsList( PLIST_OF_SUBS list )
{
	
#ifdef DEBUG_TRACE
	printf( "%s: InitSubsList()\n", __FILE__ );
#endif
	
	list->currentNode = list->head;
	return;
}


/*******************************************************************************
FUNCTION NAME: SubsListLength
INPUTS:		PLIST_OF_SUBS ListOfSubs
RETURNS:	ULONG
PURPOSE:	Return ListOfSubs->currentLength.
CALLED BY:	subdue.c: Discover()
*******************************************************************************/

ULONG SubsListLength( PLIST_OF_SUBS ListOfSubs )
{
#ifdef DEBUG_TRACE
	printf( "%s: SubsListLength()\n", __FILE__ );
#endif
	
	return ListOfSubs->currentLength;
}


/*******************************************************************************
FUNCTION NAME: AcceptableSize
INPUTS:		PGRAPH_VARIABLES GV, PSUB sub
RETURNS:	BOOLEAN
PURPOSE:	Determine whether sub is within the user-specified size limits.
CALLED BY:	subdue.c: Discover()
*******************************************************************************/

BOOLEAN AcceptableSize( PGRAPH_VARIABLES GV, PSUB sub )
{
#ifdef DEBUG_TRACE
	printf( "%s: AcceptableSize()\n", __FILE__ );
#endif
	
	if ( !GV->limitedSize ||
		( sub->definition->numberOfVertices >= GV->minSize &&
		sub->definition->numberOfVertices <= GV->maxSize ) )
		return TRUE;
	else
		return FALSE;
}



