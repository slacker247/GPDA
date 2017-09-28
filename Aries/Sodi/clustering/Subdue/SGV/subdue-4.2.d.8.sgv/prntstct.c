/********************************************************************
*
* SUBDUE
*
* FILE NAME: prntstct.c
*
********************************************************************/



#include "subdue.h"


/*******************************************************************************
FUNCTION NAME: PrintVertex 
INPUTS:		PGRAPH_VERTEX vertex, BOOLEAN negativeSub
RETURNS:	none
PURPOSE: 
CALLED BY:	prntstct.c: PrintSubGraph()
*******************************************************************************/

void PrintVertex( PGRAPH_VARIABLES GV, PGRAPH_VERTEX vertex, BOOLEAN negativeSub )
{ 
	LABEL label;
	char margin[8];
	
#ifdef DEBUG_TRACE
	printf( "%s: PrintVertex()\n", __FILE__ );
#endif
	
	if ( negativeSub )
		strncpy( margin, NEGSTR, 8 );
	else
		margin[0] = '\0';
	
	label = GV->labelList[vertex->labelIndex];
	if ( label.labelType == 's' )                               /* string label */
		printf( "%s%10u  %s \n", margin, vertex->ID, label.content.stringValue );
	else														/* numeric label */
		printf( "%s%10u  %f \n", margin, vertex->ID, label.content.numericValue );
	return;
}


/*******************************************************************************
FUNCTION NAME: PrintEdge
INPUTS:		PGRAPH_EDGE edge, 
			PGRAPH_VERTEX vertices, 
			ULONG sourceVertexID,
			BOOLEAN negativeSub
RETURNS:	void
PURPOSE: 
CALLED BY:	prntstct.c: PrintSubGraph()
*******************************************************************************/

void PrintEdge( PGRAPH_VARIABLES GV, PGRAPH_EDGE edge, PGRAPH_VERTEX vertices, 
			   ULONG sourceVertexID, BOOLEAN negativeSub )
{
	ULONG targetVertexID;
	LABEL label;
	char margin[8];
	
#ifdef DEBUG_TRACE
	printf( "%s: PrintEdge()\n", __FILE__ );
#endif
	
	if ( negativeSub )
		strncpy( margin, NEGSTR, 8 );
	else
		margin[0] = '\0';
	
	targetVertexID = vertices[edge->targetVertexIndex].ID;
	
	/* do not print undirected edges for a vertex with higher ID than target */
//{U}
//	if ( edge->directed == FALSE && targetVertexID < sourceVertexID )
//		return;
	
	printf( "%s\t[%lu", margin, sourceVertexID );
	if ( edge->directed )
		printf( " -> " );
	else
		printf( " -- " ); 
	printf( "%lu]  ", targetVertexID );
	
	label = GV->labelList[edge->labelIndex];
	if ( label.labelType == 's' )                               /* string label */
		printf( "%s\n", label.content.stringValue );
	else			 											/* numeric label */
		printf( "%f\n", label.content.numericValue );
	
	
	return;
}


/*******************************************************************************
FUNCTION NAME: PrintGraph
INPUTS:		PGRAPH graph, BOOLEAN printVertices
RETURNS:	none
PURPOSE:
CALLED BY:	NOT USED
*******************************************************************************/

void PrintGraph( PGRAPH_VARIABLES GV, PGRAPH graph, BOOLEAN printVertices,
				BOOLEAN negativeSub )
{
	ULONG vertexIndex;
	ULONG edgeIndex;
	PGRAPH_VERTEX currentVertex;
	PGRAPH_VERTEX vertices;
	
#ifdef DEBUG_TRACE
	printf( "%s: PrintGraph()\n", __FILE__ );
#endif
	
	if ( printVertices )
	{    
		printf( "Number of graph vertices = %lu\n", graph->numberOfVertices );
		printf( "Number of graph edges = %lu\n", graph->numberOfEdges );
		printf( "\nGraph vertices .....\n" );
	}
	else
		printf( "\nGraph edges .....\n" );
	vertices = graph->vertices;
	
	for( vertexIndex = 0; vertexIndex < graph->numberOfVertices; vertexIndex++ )
	{
		currentVertex = &vertices[vertexIndex];
		if ( printVertices )                                 /* printing vertices */
			PrintVertex( GV, currentVertex, negativeSub );
		else
			for( edgeIndex = 0; edgeIndex < currentVertex->numberOfEdges; 
			edgeIndex++ )
				PrintEdge( GV, &currentVertex->edges[edgeIndex], vertices,
				currentVertex->ID, negativeSub );
	}
	printf( "\n" );
}


/*******************************************************************************
FUNCTION NAME: PrintSubGraph
INPUTS:		PGRAPH_VARIABLES GV, 
			PSUB_GRAPH subGraph, 
			PRINT_MODE mode
RETURNS:	void
PURPOSE: 
CALLED BY:	psdiscover.c: FindInstancesOfPredefinedSub() 
			prntstct.c: PrintSubGraphsList()
			prntstct.c: PrintSub()
*******************************************************************************/

void PrintSubGraph( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph, PRINT_MODE mode,
				   BOOLEAN negativeSub )
{
	ULONG vertexIndex;
	ULONG edgeIndex;
	PSUB_GRAPH_VERTEX vertices;
	PSUB_GRAPH_VERTEX currentVertex;
	PSUB_GRAPH_EDGE currentSubEdges;
	PGRAPH_VERTEX graphVertices;
	PGRAPH_VERTEX currentGraphVertex;
	char margin[8];
	
#ifdef DEBUG_TRACE
	printf( "%s: PrintSubGraph()\n", __FILE__ );
#endif
	
	if ( negativeSub )
		strncpy( margin, NEGSTR, 8 );
	else
		margin[0] = '\0';
	
	if ( mode == FIRST_VERTEX )
	{
		PrintVertex( GV, &GV->graph->vertices[subGraph->vertices[0].indexInGraph],
			negativeSub );
		return;
	}
	else if ( mode == VERTICES )		                 /* printing vertices */
	{
		printf( "%s\n%sNumber of subgraph vertices = %lu\n", margin, margin,
			subGraph->numberOfVertices );
		printf( "%sNumber of subgraph edges = %lu\n", margin,
			subGraph->numberOfEdges /*- subGraph->numberOfUndirectedEdges*/ );
		printf( "%s\n%sSubgraph vertices\n", margin, margin );
	}
	else if ( mode == EDGES )		                    /* printing edges */
		printf( "%s\n%sSubgraph edges\n", margin, margin );

	vertices = subGraph->vertices;
	graphVertices = GV->graph->vertices;

	for ( vertexIndex = 0; vertexIndex < subGraph->numberOfVertices; vertexIndex++ )
	{
		currentVertex = &vertices[vertexIndex];
		currentGraphVertex = &graphVertices[currentVertex->indexInGraph];
		currentSubEdges = &subGraph->edges[currentVertex->edgesIndex];
		if ( mode == VERTICES )                              /* printing vertices */
			PrintVertex( GV, currentGraphVertex, negativeSub );
		else			                            /* printing edges */
			for ( edgeIndex = 0; edgeIndex < currentVertex->numberOfEdges; edgeIndex++ )
				PrintEdge( GV, &currentGraphVertex->
							edges[currentSubEdges[edgeIndex].indexInGraphVertex],
							graphVertices, currentGraphVertex->ID, negativeSub );
	}
}


/*******************************************************************************
FUNCTION NAME: PrintPattern
INPUTS:		PGRAPH graph, 
			PSUB_GRAPH subGraph
RETURNS:	none
PURPOSE:	Print the DNA sequence pattern this subGraph contains.
CALLED BY:	prntstct.c: PrintSubGraphsList()
			prntstct.c: PrintSub()
*******************************************************************************/

void PrintPattern( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph )
{
	ULONG vertexIndex;
	PSUB_GRAPH_VERTEX vertices;
	PSUB_GRAPH_VERTEX currentVertex;
	PGRAPH_VERTEX graphVertices;
	PGRAPH_VERTEX currentGraphVertex;
	PGRAPH graph;
	
	graph = GV->graph;
	vertices = subGraph->vertices;
	graphVertices = graph->vertices;
	for ( vertexIndex = 0; vertexIndex < subGraph->numberOfVertices;
	vertexIndex++ )
	{
		currentVertex = &vertices[vertexIndex];
		currentGraphVertex = &graphVertices[currentVertex->indexInGraph];
		printf( "%c", GV->labelList[currentGraphVertex->labelIndex].content.stringValue[0] );
	}
	printf( "\n" );
	return;
}


/*******************************************************************************
FUNCTION NAME: PrintSubGraphsList
INPUTS:		PLIST_OF_SUB_GRAPHS subGraphsList
RETURNS:	none
PURPOSE: 
CALLED BY:	prntstct.c: PrintSub()
*******************************************************************************/

void PrintSubGraphsList( PGRAPH_VARIABLES GV, PLIST_OF_SUB_GRAPHS subGraphsList,
						BOOLEAN negativeSub )
{
	PLIST_OF_SUB_GRAPHS_NODE currentState;
	PSUB_GRAPH currentSubGraph;
	ULONG count;
	
#ifdef DEBUG_TRACE
	printf( "%s: PrintSubGraphsList()\n", __FILE__ );
#endif
	
	count = 0;
	currentState = subGraphsList->currentNode;
	InitSubGraphsList( subGraphsList );
	while ( ( currentSubGraph = GetNextSubGraph( subGraphsList ) ) != NULL )
	{
		printf( "\t%10u        ", ++count );
		/* PrintPattern( GV->graph, currentSubGraph ); */
		PrintSubGraph( GV, currentSubGraph, FIRST_VERTEX, negativeSub );
	}
	subGraphsList->currentNode = currentState;
	return;
}


/*******************************************************************************
FUNCTION NAME: PrintSub
INPUTS:		PGRAPH_VARIABLES GV, 
			PSUB sub, 
			BOOLEAN finalSub,  
			BOOLEAN negativeSub
RETURNS:	void
PURPOSE: 
CALLED BY:	subdue.c: Discover()
*******************************************************************************/

void PrintSub( PGRAPH_VARIABLES GV, PSUB sub, BOOLEAN finalSub, BOOLEAN negativeSub )
{
	char margin[8];
	
#ifdef DEBUG_TRACE
	printf( "%s: PrintSub()\n", __FILE__ );
#endif
	
	if ( GV->limitedSize && sub->definition->numberOfVertices < GV->minSize )
		return;
	
	if ( negativeSub )
		strncpy( margin, NEGSTR, 8 );
	else
		margin[0] = '\0';
	
	printf( "%s---------------------------------------------------\n", margin );
	printf( "%s\n%sSubstructure definition: \n", margin, margin );
	/* PrintPattern( GV->graph, sub->definition ); */
	PrintSubGraph( GV, sub->definition, VERTICES, negativeSub );
	PrintSubGraph( GV, sub->definition, EDGES, negativeSub );
	printf( "%s\n%sNumber of instances = %lu \n", margin, margin, 
		sub->instances->currentLength );

	if (GV->outputLevel >= OL_BEST_SUBS) {
		printf( "%s\n%sValue = %f\n", margin, margin, sub->value );

		if ( GV->minEncode )
			printf( "%s\n%sDescription length ", margin, margin );   
		else
			printf( "%s\n%sSize ", margin, margin );

		printf( "of global graph compressed\n" );
		printf( "%s  using this substructure = %f\n%s\n", margin, 
			sub->inputGraphCompressedWithSubDL, margin );
		printf( "%sCompression = %f\n%s\n", margin, 
			sub->inputGraphCompressedWithSubDL / GV->inputGraphDL, margin );

		if ( GV->outputLevel >= OL_INSTANCES && finalSub )
		{
			printf( "substructure instances:\n\n" );
			printf( "\t  instance  first vertex: ID  label\n" );
			printf( "\t------------------------------------------\n" );
			PrintSubGraphsList( GV, sub->instances, FALSE );
			printf( "\n" );
		}
	}

	fflush( stdout );
}


/*******************************************************************************
FUNCTION NAME: PrintSubsList
INPUTS:		PLIST_OF_SUBS subsList
RETURNS:	void
PURPOSE: 
*******************************************************************************/

void PrintSubsList( PGRAPH_VARIABLES GV, PLIST_OF_SUBS subsList )
{
	PLIST_OF_SUBS_NODE currentState;
	PSUB currentSub;
	ULONG count;
	
#ifdef DEBUG_TRACE
	printf( "%s: PrintSubsList()\n", __FILE__ );
#endif
	
	count = 1;
	currentState = subsList->currentNode;
	InitSubsList( subsList );
	
	while ( ( currentSub = GetNextSub( subsList ) ) != NULL &&
		count <= GV->maxNumberOfBestSubs)
	{
		if (GV->cluster)											// for clustering
			printf("\n%s%lu\n", SUB_LABEL_PREFIX, GV->subNumber );	// print sub name only
		else
			printf( "\nSubstructure %d\n", count++ );			// for others, print long desc.
		PrintSub( GV, currentSub, TRUE, FALSE );
	}
	
	subsList->currentNode = currentState;
	return;
}


/*******************************************************************************
FUNCTION NAME: PrintAbstractSub
INPUTS:		PGRAPH_VARIABLES GV, 
			PABSTRACT_SUB abstractSub, 
			BOOLEAN negativeSub
RETURNS:	none
PURPOSE:	Print the vertices and edges of abstractSub.
CALLED BY:	prntstct.c: PrintResults()
*******************************************************************************/

void PrintAbstractSub( PGRAPH_VARIABLES GV, PABSTRACT_SUB abstractSub, 
					  BOOLEAN negativeSub )
{
	ULONG vertexIndex;
	ULONG edgeIndex;
	LABEL label;
	PABSTRACT_SUB_VERTEX vertex;
	PABSTRACT_SUB_EDGE edge;
	char margin[8];
	
#ifdef DEBUG_TRACE
	printf( "%s: PrintAbstractSub()\n", __FILE__ );
#endif
	
	if ( negativeSub )
		strncpy( margin, NEGSTR, 8 );
	else
		margin[0] = '\0';
	
	printf( "%s\n%sNumber of substructure vertices = %lu\n", margin, margin,
		abstractSub->numberOfVertices );
	printf( "%sNumber of substructure edges = %lu\n", margin,
		abstractSub->numberOfEdges /*- abstractSub->numberOfUndirectedEdges*/ );
	printf( "%s\n%sSubstructure vertices\n", margin, margin );
	for ( vertexIndex = 0; vertexIndex < abstractSub->numberOfVertices;
	vertexIndex++ )
	{
		printf( "%s%10u  ", margin, vertexIndex + 1 );
		label = GV->labelList[abstractSub->vertices[vertexIndex].labelIndex];
		if ( label.labelType == 's' )
			printf( "%s\n", label.content.stringValue );
		else
			printf( "%f\n", label.content.numericValue );
		/*printf( "%lu\n", abstractSub->vertices[vertexIndex].covered );*/
	}
	printf( "%s\n%sSubstructure edges\n", margin, margin );
	for ( vertexIndex = 0; vertexIndex < abstractSub->numberOfVertices;
	vertexIndex++ )
	{
		vertex = &abstractSub->vertices[vertexIndex];
		for ( edgeIndex = vertex->edgesIndex;
		edgeIndex < vertex->edgesIndex + vertex->numberOfEdges; edgeIndex++ )
		{
			edge = &abstractSub->edges[edgeIndex];
			if ( vertexIndex <= edge->targetVertex )
			{
				printf( "%s\t[%d ", margin, vertexIndex + 1 );
				if ( edge->directed )
					printf( "--> " );
				else
					printf( "-- " );
				printf( "%d]  ", edge->targetVertex + 1 );
				label = GV->labelList[edge->labelIndex];
				if ( label.labelType == 's' )
					printf( "%s\n", label.content.stringValue );
				else
					printf( "%f\n", label.content.numericValue );
			}
		}
	}
	printf( "%s\n", margin );
	return;
}


#ifdef _PVM_SUBDUE_
/*******************************************************************************
FUNCTION NAME: PrintResults
INPUTS:		none
RETURNS:	none
PURPOSE: 
CALLED BY:	main.c: main()
*******************************************************************************/

void PrintResults( PGRAPH_VARIABLES graph )
{
	int index;
	DOUBLE *discoveryTime;
	DOUBLE *remoteTime;
	DOUBLE *TotalTime;
	int bufferID;
	int messageType;
	int source;
	DOUBLE temp1;
	DOUBLE temp2;
	int sourceGroup;
	int bufLen;
	DOUBLE RMax1;
	DOUBLE RMin1;
	DOUBLE average1;
	DOUBLE SD1;
	DOUBLE RMax2;
	DOUBLE RMin2;
	DOUBLE average2;
	DOUBLE SD2;
	DOUBLE RMax3;
	DOUBLE RMin3;
	DOUBLE average3;
	DOUBLE SD3;
	
	discoveryTime = (DOUBLE *) Malloc( sizeof( DOUBLE ) * graph->numberOfProcs );
	remoteTime = (DOUBLE *) Malloc( sizeof( DOUBLE ) * graph->numberOfProcs );
	TotalTime = (DOUBLE *) Malloc( sizeof( DOUBLE ) * graph->numberOfProcs );
	
	for ( index = 0; index < graph->numberOfProcs; index++ )
	{
		messageType = PVM_TIME_STAT;
		source = PVM_ANY_SOURCE;
		bufferID = pvm_recv( source, messageType );
		pvm_bufinfo( bufferID, &bufLen, &messageType, &source ); 
		pvm_unpackf( "%f %f", &temp1, &temp2 );
		sourceGroup = pvm_getinst( graph->groupName, source );
		discoveryTime[sourceGroup] = temp1;
		remoteTime[sourceGroup] = temp2;
		TotalTime[sourceGroup] = temp1 + temp2;
	}	
	
	average1 = 0;
	RMax1 = discoveryTime[0];
	RMin1 = discoveryTime[0];
	for ( index = 0; index < graph->numberOfProcs; index++ )
	{
		average1 += discoveryTime[index];
		if ( discoveryTime[index] > RMax1 )
			RMax1 = discoveryTime[index] ;
		if ( discoveryTime[index] < RMin1 )
			RMin1 = discoveryTime[index] ;
	}
	
	average1 = average1 / graph->numberOfProcs;
	SD1 = 0;
	for ( index = 0; index < graph->numberOfProcs; index++ )
		SD1 += ( ( discoveryTime[index] - average1 ) * ( discoveryTime[index] -
		average1 ) );
	SD1 = SD1 / graph->numberOfProcs;
	SD1 = sqrt( SD1 );
	
	average2 = 0;
	RMax2 = remoteTime[0];
	RMin2 = remoteTime[0];
	for ( index = 0; index < graph->numberOfProcs; index++ )
	{
		average2 += remoteTime[index];
		if ( remoteTime[index] > RMax2 )
			RMax2 = remoteTime[index] ;
		
		if ( remoteTime[index] < RMin2 )
			RMin2 = remoteTime[index] ;
	}
	
	average2 = average2 / graph->numberOfProcs;
	SD2 = 0;
	
	for ( index = 0; index < graph->numberOfProcs; index++ )
		SD2 += ( ( remoteTime[index] - average2 ) * 
		( remoteTime[index] - average2 ) );
	SD2 = SD2 / graph->numberOfProcs;
	SD2 = sqrt( SD2 );
	
	average3 = 0;
	RMax3 = TotalTime[0];
	RMin3 = TotalTime[0];
	for ( index = 0; index < graph->numberOfProcs; index++ )
	{
		average3 += TotalTime[index];
		if ( TotalTime[index] > RMax3 )
			RMax3 = TotalTime[index] ;
		
		if ( TotalTime[index] < RMin3 )
			RMin3 = TotalTime[index] ;
	}
	
	average3 = average3 / graph->numberOfProcs;
	SD3 = 0;
	for ( index = 0; index < graph->numberOfProcs; index++ )
		SD3 += ( (TotalTime[index] - average3 ) * 
		(TotalTime[index] - average3 ) );
	SD3 = SD3 / graph->numberOfProcs;
	SD3 = sqrt( SD3 );
	
	printf( 
		"\n\nNode    Local Discovery Time   Remote Discovery Time   total Time\n" );
	for( index = 0; index < graph->numberOfProcs; index++ )
		printf( "%3u          %10.2f             %10.2f         %10.2f\n", index,
		discoveryTime[index], remoteTime[index], TotalTime[index] );
	
	printf( "Average      %10.2f             %10.2f         %10.2f\n", average1,
		average2, average3 );
	printf( "Maximum      %10.2f             %10.2f         %10.2f\n", RMax1,
		RMax2, RMax3 );
	printf( "Minimum      %10.2f             %10.2f         %10.2f\n", RMin1,
		RMin2, RMin3 );
	printf( "S.D.         %10.2f             %10.2f         %10.2f\n\n", SD1,
		SD2, SD3 );
	
	for( index = 0; index < graph->numberOfProcs; index++ ) {
		if ( graph->bestAbstractSubAtProc[index] != NULL )
		{
			printf( "\n-------------------------------------------------------\n" );
			printf( "Global value of substructure %u = %f\n", index,
				graph->globalValueOfSubAtProc[index] );
			PrintAbstractSub( graph, graph->bestAbstractSubAtProc[index], FALSE );
		}
	}
	
	Free( discoveryTime );
	Free( remoteTime );
	Free( TotalTime );
	return;
}
#endif /* _PVM_SUBDUE_ */

