/********************************************************************
*
* SUBDUE
*
* FILE NAME: concept.c
*
********************************************************************/

#include "subdue.h"

/*******************************************************************************
FUNCTION NAME: GetNegativeValue
INPUTS:		PGRAPH_VARIABLES GV, 
			PSUB sub, 
			ULONG parentNegativeSubID
RETURNS:	none
PURPOSE:	Create an abstract sub from sub, find instances of the
			abstract sub in the negative graph, evaluate the resulting sub, and 
			return its value. Substructures are retained from one evaluation
			to the next so that the search for instances does not have to start 
			from scratch every time.
CALLED BY:	main.c: main()
*******************************************************************************/

DOUBLE GetNegativeValue( PGRAPH_VARIABLES GV, PSUB sub, ULONG parentNegativeSubID )
{
	PGRAPH_VARIABLES negativeGraph;
	PABSTRACT_SUB newAbstractSub;
	PABSTRACT_SUB oldAbstractSub;
	PSUB newSub;
	PSUB parentSub;
	PNEGATIVE_SUB parentNegativeSub;
	PNEGATIVE_SUB currentNegativeSub;
	ULONG index;
	ULONG vertexDec;
	ULONG edgeDec;
	ULONG newVertex;
	ULONG newEdge1;
	ULONG newEdge2;
	ULONG currentNegativeSubID;
	DOUBLE value;
	
#ifdef _DEBUG_TRACE_
	printf( "%s: FindNegativeSubInstances()\n", __FILE__ );
#endif
	
	negativeGraph = GV->negativeGraph;
	parentSub = NULL;
	parentNegativeSub = NULL;
	
	/*PrintSub( GV, sub, FALSE, TRUE );*/
	newAbstractSub = CreateAbstractSub( GV, sub->definition );
	ConvertLabels( GV, negativeGraph, newAbstractSub );
	newVertex = sub->newVertex;
	newEdge1 = sub->newEdge1;
	newEdge2 = sub->newEdge2;
	currentNegativeSubID = (ULONG) sub;
	/*PrintAbstractSub( negativeGraph, newAbstractSub, TRUE );*/
	
	if ( negativeGraph->newLabel )
		ResizeExpandTemplates( negativeGraph );
	
	if ( negativeGraph->firstSetOfSubs || negativeGraph->fromScratch )
	{
		newSub = DiscoverAbstractSub( negativeGraph, newAbstractSub );
	}
	else
	{
		parentNegativeSub = GetNegativeSub( &negativeGraph->negativeSubsList,
				        parentNegativeSubID );
		
		if ( parentNegativeSub->sub == NULL )
			newSub = NULL;
		else
		{
			parentSub = CopySub( parentNegativeSub->sub );
			oldAbstractSub = parentNegativeSub->abstractSub;
			
			/* set the covered values for newAbstractSub using oldAbstractSub's 
			values and the values for newVertex, newEdge1, and newEdge2 */ 
			
			vertexDec = 0;
			for ( index = 0; index < newAbstractSub->numberOfVertices; index++ )
				if ( index == newVertex )
					vertexDec = 1;
				else if ( oldAbstractSub->vertices[index - vertexDec].covered )
					newAbstractSub->vertices[index].covered =
					oldAbstractSub->vertices[index - vertexDec].covered;
				
				edgeDec = 0;
				for ( index = 0; index < newAbstractSub->numberOfEdges; index++ )
					if ( index == newEdge1 || index == newEdge2 )
						edgeDec++;
					else if ( oldAbstractSub->edges[index - edgeDec].covered )
						newAbstractSub->edges[index].covered =
						oldAbstractSub->edges[index - edgeDec].covered;
					
					newSub = DiscoverTargetSub( negativeGraph, parentSub, newAbstractSub );
		}
	}
	
	if ( newSub == NULL )
	{
		if ( GV->minEncode )
		{
			/* "add a label" for the compressed vertex label */
			negativeGraph->numberOfVertexLabels++;
			
			value = negativeGraph->inputGraphDL /
				( AbstractSubDL( negativeGraph, newAbstractSub ) +
				GraphDL( negativeGraph, negativeGraph->graph ) );
			
			/* "remove" the compressed vertex label */
			negativeGraph->numberOfVertexLabels--;
		}
		else
		{
			value = (DOUBLE) ( newAbstractSub->numberOfVertices + 
				newAbstractSub->numberOfEdges /*-
				newAbstractSub->numberOfUndirectedEdges*/ );

			value = negativeGraph->inputGraphDL / 
				( value + negativeGraph->inputGraphDL );
		}
	}
	else
		value = newSub->value;
	
	if ( GV->outputLevel >= OL_SUPERVISED )
	{
		if ( newSub == NULL )
		{
			printf( "%sNo instances found:\n", NEGSTR );
			PrintAbstractSub( negativeGraph, newAbstractSub, TRUE );
			printf( "%sValue = %f\n%s\n", NEGSTR, value, NEGSTR );
		}
		else {
			PrintSub( negativeGraph, newSub, FALSE, TRUE );
		}

		fflush( stdout );
	}
	
	if ( negativeGraph->fromScratch )
	{
		DestroyAbstractSub( newAbstractSub );
		DestroySub( newSub );
	}
	else
	{
		currentNegativeSub = CreateNegativeSub( currentNegativeSubID,
			newAbstractSub, newSub );
		PutNegativeSub( &negativeGraph->negativeSubsList, currentNegativeSub );
		if ( parentNegativeSub )
			PutNegativeSub( &negativeGraph->negativeSubsList, parentNegativeSub );
	}
	
	return value;
}


/*******************************************************************************
FUNCTION NAME: CreateNegativeSub 
INPUTS:		ULONG negativeSubID, 
			PABSTRACT_SUB abstractSub, 
			PSUB sub
RETURNS:	PNEGATIVE_SUB
PURPOSE:	Create a new negativeSub.
CALLED BY:	concept.c: GetNegativeValue()
*******************************************************************************/

PNEGATIVE_SUB CreateNegativeSub( ULONG negativeSubID,
								 PABSTRACT_SUB abstractSub, 
								 PSUB sub )
{
	PNEGATIVE_SUB newNegativeSub;
	
#ifdef _DEBUG_TRACE_
	printf( "%s: CreateNegativeSub()\n", __FILE__ );
#endif
	
	newNegativeSub = (PNEGATIVE_SUB) Malloc( sizeof( NEGATIVE_SUB ) );
	newNegativeSub->negativeSubID = negativeSubID;
	newNegativeSub->abstractSub = abstractSub;
	newNegativeSub->sub = sub;
	newNegativeSub->next = NULL;
	
	/*printf( "Created negativeSub %lu.\n", newNegativeSub->negativeSubID );
	fflush( stdout );*/
	
	return newNegativeSub;
}


/*******************************************************************************
FUNCTION NAME: DestroyNegativeSub
INPUTS:		PNEGATIVE_SUB negativeSub
RETURNS:	none
PURPOSE:	Free the memory used by negativeSub.
CALLED BY:	concept.c:	DestroyNegativeSubList()
						FindAndDestroyNegativeSub()
			main.c:		main()
*******************************************************************************/

void DestroyNegativeSub( PNEGATIVE_SUB negativeSub )
{
#ifdef _DEBUG_TRACE_
	printf( "%s: DestroyNegativeSub()\n", __FILE__ );
#endif
	
	if ( negativeSub == NULL )
	{
    /*printf( "Tried to destroy a NULL negativeSub.\n" );
		fflush( stdout );*/
		return;
	}
	/*printf( "Destroying negativeSub %lu.\n", negativeSub->negativeSubID );
	fflush( stdout );*/
	
	if ( negativeSub )
	{
		if ( negativeSub->abstractSub )
			DestroyAbstractSub( negativeSub->abstractSub );
		if ( negativeSub->sub )
			DestroySub( negativeSub->sub );
		Free( negativeSub );
	}
	
	return;
}


/*******************************************************************************
FUNCTION NAME: PutNegativeSub
INPUTS:		PNEGATIVE_SUB list, 
			PNEGATIVE_SUB newNegativeSub
RETURNS:	none
PURPOSE:	Insert newNegativeSub at the head of list.
CALLED BY:	concept.c: GetNegativeValue()
*******************************************************************************/

void PutNegativeSub( PNEGATIVE_SUB *list, PNEGATIVE_SUB newNegativeSub )
{
#ifdef _DEBUG_TRACE_
	printf( "%s: PutNegativeSub()\n", __FILE__ );
#endif
	
	/*printf ( "Putting negativeSub %lu on the list.\n", 
	   newNegativeSub->negativeSubID );
	fflush( stdout );*/
	
	newNegativeSub->next = *list;
	*list = newNegativeSub;
}


/*******************************************************************************
FUNCTION NAME: GetNegativeSub
INPUTS:		PNEGATIVE_SUB *list, 
			ULONG ID
RETURNS:	PNEGATIVE_SUB
PURPOSE:	Search list for the negativeSub with negativeSubID equal to ID; remove
			it from list and return a pointer to it.
CALLED BY:	concept.c:	GetNegativeValue()
						FindAndDestroyNegativeSub()
						FindAndDestroyNegativeSubList()
			main.c:		main()
*******************************************************************************/

PNEGATIVE_SUB GetNegativeSub( PNEGATIVE_SUB *list, ULONG ID )
{
	PNEGATIVE_SUB previous;
	PNEGATIVE_SUB current;
	BOOLEAN found;
	
#ifdef _DEBUG_TRACE_
	printf( "%s: GetNegativeSub()\n", __FILE__ );
#endif
	
	previous = NULL;
	found = FALSE;
	current = *list;
	
	while ( current != NULL )
	{
		if ( current->negativeSubID == ID )
		{
			found = TRUE;
			break;
		}
		previous = current;
		current = current->next;
	} 
	
	if ( found )
	{
    /*printf( "found negative sub %lu in list.\n", ID );
		fflush( stdout );*/
		if ( current == *list )
			*list = current->next;
		else
			previous->next = current->next;
		return current;
	}
	/*printf( "Didn't find negativeSub %lu in the list.\n", ID );
	fflush( stdout );*/
	return NULL;
}


/*******************************************************************************
FUNCTION NAME: DestroyNegativeSubsList
INPUTS:		PNEGATIVE_SUB *list
RETURNS:	none
PURPOSE:	Destroy all the NegativeSubs in list
CALLED BY:	main.c: main()
*******************************************************************************/

void DestroyNegativeSubsList( PNEGATIVE_SUB *list )
{
	PNEGATIVE_SUB previous;
	PNEGATIVE_SUB current;
	
#ifdef _DEBUG_TRACE_
	printf( "%s: DestroyNegativeSubsList()\n", __FILE__ );
#endif
	
	previous = NULL;
	current = *list;
	
	while ( current != NULL )
	{
		previous = current;
		current = current->next;
		DestroyNegativeSub( previous );
	}
	*list = NULL;
	
	return;
}


/*******************************************************************************
FUNCTION NAME: FindAndDestroyNegativeSub
INPUTS:		PGRAPH_VARIABLES GV, 
			ULONG deadSubID
RETURNS:	none
PURPOSE:	Find the negativeSub with deadSubID and destroy it.
CALLED BY:	subdue.c: Discover()
			subsop.c: InsertInSubsListInOrder()
*******************************************************************************/

void FindAndDestroyNegativeSub( PGRAPH_VARIABLES GV, ULONG deadSubID )
{
	PNEGATIVE_SUB negativeSub;
	
#ifdef DEBUG_TRACE
	printf( "%s: FindAndDestroyNegativeSub()\n", __FILE__ );
#endif
	
	negativeSub = GetNegativeSub( &GV->negativeSubsList, deadSubID );
	DestroyNegativeSub( negativeSub );
}

