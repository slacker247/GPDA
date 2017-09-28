/********************************************************************
*
* SUBDUE
*
* FILE NAME: pvm.c
*
********************************************************************/

#ifdef _PVM_SUBDUE_


#include "subdue.h"


/*******************************************************************************
FUNCTION NAME: BroadcastAbstractSub
INPUTS:		PABSTRACT_SUB abstractSub, 
			int messageType
RERETURNS:	none
TURNS:	none
PURPOSE:	Pack abstractSub into a PVM buffer and broadcast it to all the other
			processors.
CALLED BY:	pvm.c: Communicate()
*******************************************************************************/

void BroadcastAbstractSub( PGRAPH_VARIABLES graph, PABSTRACT_SUB abstractSub, int messageType )
{
	int bufferID;
	ULONG index;
	PABSTRACT_SUB_VERTEX vertices = abstractSub->vertices;
	PABSTRACT_SUB_EDGE edges = abstractSub->edges;
	PLABEL label;
	
	bufferID = pvm_initsend( PvmDataDefault );
	
	pvm_packf( "%lud %lud %lud", abstractSub->numberOfVertices,
		abstractSub->numberOfEdges,
		0 /*abstractSub->numberOfUndirectedEdges*/ );
	for ( index = 0; index < abstractSub->numberOfVertices; index++ )
	{                                                  /* pack the vertex label */
		label = &graph->labelList[vertices[index].labelIndex];
		pvm_packf( "%c", label->labelType );
		if ( label->labelType == 's' )
		{
			pvm_packf( "%ud", strlen( label->content.stringValue ) );
			pvm_pkstr( label->content.stringValue );
			pvm_packf( "%d", label->match.group );
		}
		else
			pvm_packf( "%lf %d %lf", label->content.numericValue, 
			label->match.matchType, label->matchValue );
		/* pack the other vertex data */
		pvm_packf( "%lud %lud", vertices[index].edgesIndex, 
			vertices[index].numberOfEdges );
	}
	for ( index = 0; index < abstractSub->numberOfEdges; index++ )
	{                                                    /* pack the edge label */
		label = &graph->labelList[edges[index].labelIndex];
		pvm_packf( "%c", label->labelType );
		if ( label->labelType == 's' )
		{
			pvm_packf( "%ud", strlen( label->content.stringValue ) );
			pvm_pkstr( label->content.stringValue );
			pvm_packf( "%d", label->match.group );
		}
		else
			pvm_packf( "%lf %d %lf", label->content.numericValue, 
			label->match.matchType, label->matchValue );
		/* pack the other edge data */
		pvm_packf( "%lud %uc", edges[index].targetVertex, edges[index].directed );
	}
	pvm_bcast( graph->groupName, messageType );
	return;
}


/*******************************************************************************
FUNCTION NAME: ReceiveAbstractSub
INPUTS:		int source, 
			int messageType
RETURNS:	PABSTRACT_SUB
PURPOSE:	Unpack abstractSub from the PVM buffer and return it.
CALLED BY:	pvm.c: Communicate()
*******************************************************************************/

PABSTRACT_SUB ReceiveAbstractSub( PGRAPH_VARIABLES graph, int source, int messageType )
{
	PABSTRACT_SUB abstractSub;
	int bufferID;
	ULONG index;
	PABSTRACT_SUB_VERTEX vertices;
	PABSTRACT_SUB_EDGE edges;
	LABEL label;
	ULONG length;
	
	bufferID = pvm_recv( source, messageType );	
	abstractSub = (PABSTRACT_SUB) Malloc( sizeof( ABSTRACT_SUB ) );
	
	pvm_unpackf( "%lud %lud %lud", &abstractSub->numberOfVertices,
	       &abstractSub->numberOfEdges/*,
		   &abstractSub->numberOfUndirectedEdges*/ );	// this may cause problems
	abstractSub->vertices = (PABSTRACT_SUB_VERTEX)
		Malloc( sizeof( ABSTRACT_SUB_VERTEX ) * abstractSub->numberOfVertices );
	abstractSub->edges = (PABSTRACT_SUB_EDGE) 
		Malloc( sizeof( ABSTRACT_SUB_EDGE ) * abstractSub->numberOfEdges );
	
	vertices = abstractSub->vertices;
	for( index = 0; index < abstractSub->numberOfVertices; index++ )
	{
		pvm_unpackf( "%c", &label.labelType );
		if ( label.labelType == 's' )
		{
			pvm_unpackf( "%lud", &length );
			label.content.stringValue = (char *)
				Malloc( sizeof( char ) * ( length + 1 ) );
			pvm_upkstr( label.content.stringValue );
			pvm_unpackf( "%d", &label.match.group );
		}
		else {
			pvm_unpackf( "%lf %d %lf", 
				&label.content.numericValue, 
				&label.match.matchType, 
				&label.matchValue );
		}

		vertices[index].labelIndex = AddLabel( graph, label, TRUE, TRUE );
		pvm_unpackf( "%lud %lud", &vertices[index].edgesIndex, &vertices[index].numberOfEdges );
		vertices[index].covered = 0;
	}
	
	edges = abstractSub->edges;
	for ( index = 0; index < abstractSub->numberOfEdges; index++ )
	{
		pvm_unpackf( "%c", &label.labelType );
		if ( label.labelType == 's' )
		{
			pvm_unpackf( "%lud", &length );
			label.content.stringValue = (char *)
				Malloc( sizeof( char ) * ( length + 1 ) );
			pvm_upkstr( label.content.stringValue );
			pvm_unpackf( "%d", &label.match.group );
		}
		else
			pvm_unpackf( "%lf %d %lf", &label.content.numericValue, 
			&label.match.matchType, &label.matchValue );
		edges[index].labelIndex = AddLabel( graph, label, FALSE, TRUE );
		pvm_unpackf( "%lud %uc", &edges[index].targetVertex,	&edges[index].directed );
		edges[index].covered = 0;
	}
	return abstractSub;
}


/*******************************************************************************
FUNCTION NAME: BroadcastNumber
INPUTS:		ULONG number, 
			int messageType
RETURNS:	none
PURPOSE:	Broadcast number to all processors in the group.
CALLED BY:	pvm.c: Communicate()
*******************************************************************************/
// UNUSED!
void BroadcastNumber( PGRAPH_VARIABLES graph, ULONG number, int messageType )
{
	int bufferID;
	
	bufferID = pvm_initsend( PvmDataDefault );		
	pvm_packf( "%lud", number );	
	pvm_bcast( graph->groupName, messageType );
	return;
}


/*******************************************************************************
FUNCTION NAME: BroadcastReal
INPUTS:		DOUBLE value, 
			int messageType
RETURNS:	none
PURPOSE:	Broadcast value to all the processors in the group.
CALLED BY:	pvm.c: Communicate()
*******************************************************************************/

void BroadcastReal( PGRAPH_VARIABLES graph, DOUBLE value, int messageType )
{
	int bufferID;
	
	bufferID = pvm_initsend( PvmDataDefault );		
	pvm_packf( "%lf", value );	
	pvm_bcast( graph->groupName, messageType );
	return;
}


/*******************************************************************************
FUNCTION NAME: Communicate
INPUTS:		none
RETURNS:	none
PURPOSE: 
CALLED BY:	main.c: main()
*******************************************************************************/

void Communicate( PGRAPH_VARIABLES graph )
{
	PABSTRACT_SUB abstractSub;
	int source;
	int messageType;
	int sourceGroupID;
	int numberOfReceivedResults;
	ULONG numberOfFinalEval;
	ULONG numberOfReceivedFinalEval;
	int index;
	int bufLen;
	int bufferID;
	ULONG maxValueGroupID;
	DOUBLE value;
	DOUBLE maxValue;
	
#ifdef DEBUG_TRACE
	printf( "%s: Communicate()\n", __FILE__ );
#endif

	/* broadcast best abstract sub at this processor and its local value */
	if ( graph->bestSubAtProc[graph->myGroupID] == NULL )
	{
		value = 0.0;
		BroadcastReal(graph, value, PVM_LOCAL_VALUE );
		graph->bestAbstractSubAtProc[graph->myGroupID] = NULL;
	}
	else
	{
		value = graph->bestSubAtProc[graph->myGroupID]->value;
		BroadcastReal(graph, value, PVM_LOCAL_VALUE );
		abstractSub = 
			CreateAbstractSub( graph, graph->bestSubAtProc[graph->myGroupID]->definition );
			/*   printf( "Sending this abstract sub:\n" );
		PrintAbstractSub( abstractSub ); */
		BroadcastAbstractSub( graph, abstractSub, PVM_ABSTRACT_SUB );
		graph->bestAbstractSubAtProc[graph->myGroupID] = abstractSub;
	}
	
	graph->localValueOfSubAtProc[graph->myGroupID] = value;
	
	/* receive the other processors' best subs and values */
	numberOfReceivedResults = 0;
	do
	{
		bufferID = pvm_recv( PVM_ANY_SOURCE, PVM_LOCAL_VALUE );
		pvm_bufinfo( bufferID, &bufLen, &messageType, &source );
		pvm_unpackf( "%lf", &value );
		sourceGroupID = pvm_getinst( graph->groupName, source );
		numberOfReceivedResults++;
		if ( value == 0.0 )
			graph->bestAbstractSubAtProc[sourceGroupID] = NULL;
		else
		{
			abstractSub = ReceiveAbstractSub( graph, source, PVM_ABSTRACT_SUB );
			/* printf( "Received this abstract sub:\n" );
			PrintAbstractSub( abstractSub ); */
			graph->bestAbstractSubAtProc[sourceGroupID] = abstractSub;
		}
	} while ( numberOfReceivedResults < graph->numberOfProcs - 1 );
	
	/* find instances of the best subs in the local graph */
	for ( index = 0; index < graph->numberOfProcs; index++ )
	{
		if ( index != graph->myGroupID )
		{
			if ( graph->bestAbstractSubAtProc[index] != NULL )
			{
				graph->bestSubAtProc[index] = 
					DiscoverAbstractSub( graph, graph->bestAbstractSubAtProc[index] );
				if ( graph->bestSubAtProc[index] != NULL )
					graph->localValueOfSubAtProc[index] = graph->bestSubAtProc[index]->value;
				else
					graph->localValueOfSubAtProc[index] = 0.0;
					/* printf( "Local value of sub %d is %e\n", index, 
				graph->bestSubAtProc[index]->value ); */
			}
			else
			{
				/*	printf( "graph->bestAbstractSubAtProc[%u] is NULL\n", index ); */
				graph->bestSubAtProc[index] = NULL;
				graph->localValueOfSubAtProc[index] = 0.0;
			}
		}
	}    
	/* send masterProc the value of each sub in the local graph */
	for ( index = 0; index < graph->numberOfProcs; index++ )
	{
		bufferID = pvm_initsend( PvmDataDefault );
		pvm_packf( "%ud %lf", index, graph->localValueOfSubAtProc[index] );	    
		pvm_send( graph->masterProc, PVM_FINAL_VALUE );
	}
	/* Master Processor: find the sub with the highest global value */
	/* and broadcast its processor's GroupID */
	if ( graph->procID == graph->masterProc )
	{                  /* receive final values from all processors for all subs */
		numberOfFinalEval = graph->numberOfProcs * graph->numberOfProcs;
		for ( numberOfReceivedFinalEval = 0; 
		numberOfReceivedFinalEval < numberOfFinalEval;
		numberOfReceivedFinalEval++ )
		{
			bufferID = pvm_recv( PVM_ANY_SOURCE, PVM_FINAL_VALUE );
			pvm_unpackf( "%ud %lf", &index, &value );
			graph->globalValueOfSubAtProc[index] += value;
		}
		/* find sub with highest global value */
		maxValue = 0.0;
		maxValueGroupID = 0;
		for ( index = 0; index < graph->numberOfProcs; index++ )
			if ( graph->globalValueOfSubAtProc[index] > maxValue )
			{
				maxValue = graph->globalValueOfSubAtProc[index];
				maxValueGroupID = index;
			}
			/* broadcast best sub's GroupID */
			bufferID = pvm_initsend( PvmDataDefault );
			pvm_packf( "%lud", maxValueGroupID );
			pvm_bcast( graph->groupName, PVM_GROUP_ID );
			graph->bestSubGroupID = maxValueGroupID;
	}
	else                                  /* Slaves: receive best sub's GroupID */
	{
		bufferID = pvm_recv( graph->masterProc, PVM_GROUP_ID );
		pvm_unpackf( "%lud", &graph->bestSubGroupID );
	}
	return;
}		

/*******************************************************************************
FUNCTION NAME: PrintPVMError
INPUTS:		FILE *outputFile, 
			int errorCode
RETURNS:	none
PURPOSE:	Prints an error message according to the error code. To print to the
			screen, pass stdio for outputFile.
CALLED BY:	pvm.c: Communicate()
*******************************************************************************/
void PrintPVMError(int errorCode)
{
	char *msg;

#ifdef DEBUG_TRACE
	printf( "%s: PrintPVMError()\n", __FILE__ );
#endif

	switch (errorCode) {
	case 0:				// OK
		msg = "OK";
		break;
	case -2:
		msg = "Invalid argument";
		break;
	case -3:
		msg = "Mismatch";
		break;
	case -4:
		msg = "Overflow";
		break;
	case -5:	
		msg = "End of buffer/No data";
		break;
	case -6:			
		msg = "No such host";
		break;
	case -7:	
		msg = "No such file";
		break;
	case -8:		
		msg = "Permission denied";
		break;
	case -10:		 
		msg = "Malloc failed";
		break;
	case -12:
		msg = "Can't decode message";
		break;
	case -14:
		msg = "Can't contact local daemon";
		break;
	case -15:
		msg = "No current buffer";
		break;
	case -16:
		msg = "No such buffer";
		break;
	case -17:
		msg = "Null group name";
		break;
	case -18:
		msg = "Already in group";
		break;
	case -19:
		msg = "No such group";
		break;
	case -20:
		msg = "Not in group";
		break;
	case -21:
		msg = "No such instance";
		break;
	case -22:
		msg = "Host failed";
		break;
	case -23:
		msg = "No parent task";
		break;
	case -24:
		msg = "Not implemented";
		break;
	case -25:
		msg = "Pvmd system error";
		break;
	case -26:
		msg = "Version mismatch";
		break;
	case -27:
		msg = "Out of resources";
		break;
	case -28:
		msg = "Duplicate host";
		break;
	case -29:
		msg = "Can't start pvmd";
		break;
	case -30:
		msg = "Already in progress";
		break;
	case -31:
		msg = "No such task";
		break;
	case -32:
		msg = "Not Found";
		break;
	case -33:
		msg = "Already exists";
		break;
	case -34:
		msg = "Hoster run on non-master host";
		break;
	case -35:
		msg = "Spawning parent set PvmNoSpawnParent";
		break;
	default:
		; // empty
	}

	printf("PVM Error %d: %s\n", errorCode, msg);
}

#endif /* _PVM_SUBDUE_ */


