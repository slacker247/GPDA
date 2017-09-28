/********************************************************************
*
* SUBDUE
*
* FILE NAME: tempop.c
*
********************************************************************/



#include "subdue.h"


/*******************************************************************************
FUNCTION NAME: CreateGraphTemplate
INPUTS:		PGRAPH_VARIABLES GV
RETURNS:	none
PURPOSE:	Allocate the graphTemplate and expandTemplates arrays.
CALLED BY:	main.c: main()
*******************************************************************************/

void CreateGraphTemplate( PGRAPH_VARIABLES GV )
{
	ULONG vertexIndex;
	ULONG index;
	
#ifdef DEBUG_TRACE
	printf( "%s: CreateGraphTemplate()\n", __FILE__ );
#endif
	
	GV->graphTemplate = (PGRAPH_TEMPLATE) Malloc( sizeof(GRAPH_TEMPLATE) );
	GV->graphTemplate->verticesTemplate =
		(ULONG *) Calloc( GV->graph->numberOfVertices, sizeof(ULONG) );
	/*  
	GV->graphTemplate->verticesTemplate =
		(ULONG *) Malloc( GV->graph->numberOfVertices * sizeof( ULONG ) );
		memset( GV->graphTemplate->verticesTemplate, 0, 
		GV->graph->numberOfVertices * sizeof( ULONG ) ); 
	*/

	GV->graphTemplate->edgesTemplate =
		(ULONG **) Malloc( GV->graph->numberOfVertices * sizeof( ULONG * ) );
	for ( vertexIndex = 0; vertexIndex < GV->graph->numberOfVertices; vertexIndex++ )
	{
		if ( GV->graph->vertices[vertexIndex].numberOfEdges > 0 )
			GV->graphTemplate->edgesTemplate[vertexIndex] = (ULONG *)
			Calloc( GV->graph->vertices[vertexIndex].numberOfEdges,
			sizeof( ULONG ) );
		else
			GV->graphTemplate->edgesTemplate[vertexIndex] = NULL;
	}
	GV->expandTemplates = (PEDGE_LABEL_TEMPLATES) 
		Malloc( sizeof( EDGE_LABEL_TEMPLATES ) * GV->numberOfLabels );
	for ( index = 0; index < GV->numberOfLabels; index++ )
	{                                             /* initialize expandTemplates */
		GV->expandTemplates[index].VETemplates = NULL;
		GV->expandTemplates[index].ETemplates = NULL;
	}
	return;
}


/*******************************************************************************
FUNCTION NAME: DestroyGraphTemplate
INPUTS: 
RETURNS:	none
PURPOSE:	Free the graphTemplate and expandTemplates arrays.
CALLED BY:	main.c: main()
*******************************************************************************/

void DestroyGraphTemplate( PGRAPH_VARIABLES GV )
{
	ULONG vertexIndex;
	
#ifdef DEBUG_TRACE
	printf( "%s: DestroyGraphTemplate()\n", __FILE__ );
#endif
	
	for ( vertexIndex = 0; vertexIndex < GV->graph->numberOfVertices; 
	vertexIndex++ )
		if ( GV->graphTemplate->edgesTemplate[vertexIndex] )
			Free( GV->graphTemplate->edgesTemplate[vertexIndex] );
		Free( GV->graphTemplate->edgesTemplate );
		Free( GV->graphTemplate->verticesTemplate );
		Free( GV->graphTemplate );
		Free( GV->expandTemplates );
		return;
}


/*******************************************************************************
FUNCTION NAME: ResizeExpandTemplates
INPUTS:		PGRAPH_VARIABLES GV
RETURNS:	none
PURPOSE:	Reallocate the GV->expandTemplates array to accomodate new labels.
CALLED BY: s	ubdue.c: DiscoverAbstractSub()
*******************************************************************************/

void ResizeExpandTemplates( PGRAPH_VARIABLES GV )
{
	ULONG index;
	
#ifdef DEBUG_TRACE
	printf( "%s: ResizeExpandTemplates()\n", __FILE__ );
#endif
	
	GV->expandTemplates = (PEDGE_LABEL_TEMPLATES) 
		Realloc( GV->expandTemplates, sizeof( EDGE_LABEL_TEMPLATES ) * GV->numberOfLabels );
	for ( index = 0; index < GV->numberOfLabels; index++ )
	{                                             /* initialize expandTemplates */
		GV->expandTemplates[index].VETemplates = NULL;
		GV->expandTemplates[index].ETemplates = NULL;
	}
	
	GV->newLabel = FALSE;
	
	return;
}


/*******************************************************************************
FUNCTION NAME: SetTemplate
INPUTS:		PGRAPH_VARIABLES GV, 
			PSUB_GRAPH subGraph
RETURNS:	none
PURPOSE:	Set the template values corresponding to the vertices and edges of
			subGraph to the matching subGraph index values.
CALLED BY:	extend.c: ExtendInstances()
*******************************************************************************/

void SetTemplate( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph )
{
	ULONG vertexIndex;
	ULONG edgeIndex;
	ULONG *verticesTemplate;
	ULONG *edgesTemplate;
	PSUB_GRAPH_VERTEX subVertices;
	PSUB_GRAPH_EDGE  vertexEdges;
	
#ifdef DEBUG_TRACE
	printf( "%s: SetTemplate()\n", __FILE__ );
#endif
	
	verticesTemplate = GV->graphTemplate->verticesTemplate;
	subVertices = subGraph->vertices;
	
	for ( vertexIndex = 0; vertexIndex < subGraph->numberOfVertices; 
	vertexIndex++ )
	{
		verticesTemplate[subVertices[vertexIndex].indexInGraph] = vertexIndex + 1;
		vertexEdges = &subGraph->edges[subVertices[vertexIndex].edgesIndex];
		edgesTemplate = 
			GV->graphTemplate->edgesTemplate[subVertices[vertexIndex].indexInGraph];
		for ( edgeIndex = 0; edgeIndex < subVertices[vertexIndex].numberOfEdges; 
		edgeIndex++ )
			edgesTemplate[vertexEdges[edgeIndex].indexInGraphVertex] = 
			vertexIndex + 1;
	}
}


/*******************************************************************************
FUNCTION NAME: SetVerticesTemplate
INPUTS:		PGRAPH_VARIABLES GV, 
			PSUB_GRAPH subGraph
RETURNS:	none
PURPOSE: 
CALLED BY:	eval.c: Connectivity()
*******************************************************************************/

void SetVerticesTemplate( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph )
{
	ULONG vertexIndex;
	ULONG *verticesTemplate;
	PSUB_GRAPH_VERTEX subVertices;
	
#ifdef DEBUG_TRACE
	printf( "%s: SetVerticesTemplate()\n", __FILE__ );
#endif
	
	verticesTemplate = GV->graphTemplate->verticesTemplate;
	subVertices = subGraph->vertices;
	
	for ( vertexIndex = 0; vertexIndex < subGraph->numberOfVertices; 
	vertexIndex++ )
		verticesTemplate[subVertices[vertexIndex].indexInGraph] = vertexIndex + 1;
	
	return;
}


/*******************************************************************************
FUNCTION NAME: ResetVerticesTemplate
INPUTS:		PGRAPH_VARIABLES GV, 
			PSUB_GRAPH subGraph
RETURNS:	none
PURPOSE: 
CALLED BY:	extend.c: ExtendInstances()
			eval.c: Connectivity()
*******************************************************************************/

void ResetVerticesTemplate( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph )
{
	ULONG vertexIndex;
	ULONG *verticesTemplate;
	PSUB_GRAPH_VERTEX subVertices;
	
#ifdef DEBUG_TRACE
	printf( "%s: ResetVerticesTemplate()\n", __FILE__ );
#endif
	
	verticesTemplate = GV->graphTemplate->verticesTemplate;
	subVertices = subGraph->vertices;
	for ( vertexIndex = 0; vertexIndex < subGraph->numberOfVertices; vertexIndex++ )
		verticesTemplate[subVertices[vertexIndex].indexInGraph] = 0;
	return;
}


/*******************************************************************************
FUNCTION NAME: InitializeVerticesTemplate
INPUTS:		PGRAPH_VARIABLES GV
RETURNS:	none
PURPOSE:	Initialize the array GV->graphTemplate->verticesTemplate.
CALLED BY:	compress.c: CompressUsing()
*******************************************************************************/

void InitializeVerticesTemplate( PGRAPH_VARIABLES GV )
{
#ifdef DEBUG_TRACE
	printf( "%s: InitializeVerticesTemplate()\n", __FILE__ );
#endif
	
	memset( GV->graphTemplate->verticesTemplate, 0, GV->graph->numberOfVertices *
		sizeof( ULONG ) );
	return;
}


/*******************************************************************************
FUNCTION NAME: RegisterInTemplate
INPUTS:		PGRAPH_VARIABLES GV, 
			PGRAPH workGraph, 
			PSUB_GRAPH subGraph, 
			ULONG subGraphVertexIndex
RETURNS:	ULONG
PURPOSE:	Register the vertices and edges of the subGraph add any external 
			edges (i.e. edges connecting vertices/edges of the subGraph with 
			non-subGraph vertices/edges - this will only happen when instance 
			overlaps are allowed) to workGraph.  Return the number of unique
			vertices in workGraph that are covered by the subGraph.
CALLED BY:	compress.c: CompressUsing()
*******************************************************************************/

ULONG RegisterInTemplate( PGRAPH_VARIABLES GV, PGRAPH workGraph, 
						 PSUB_GRAPH subGraph, ULONG subGraphVertexIndex )
{
	PSUB_GRAPH_VERTEX currentVertex;
	PGRAPH_EDGE graphVertexEdges;
	ULONG vertexIndex;
	ULONG edgeIndex;
	ULONG vertexUniqueCoverage;
	ULONG edgeUniqueCoverage;
	ULONG labelIndex;
	ULONG graphVertexIndex;
	ULONG *vertexEdgesTemplate;
	ULONG currentGraphEdgeIndex;
	PGRAPH_VERTEX graphVertex;
	char newLabel[MAX_TOKEN_LENGTH];
	LABEL label;
	
#ifdef DEBUG_TRACE
	printf( "%s: RegisterInTemplate()\n", __FILE__ );
#endif
	
	edgeUniqueCoverage = 0;
	vertexUniqueCoverage = 0;
	
	for ( vertexIndex = 0; vertexIndex < subGraph->numberOfVertices; vertexIndex++ )
	{
		currentVertex = &subGraph->vertices[vertexIndex];
		graphVertexIndex = currentVertex->indexInGraph;
		graphVertex = &GV->graph->vertices[graphVertexIndex];
		
		/* Here we check for overlapping instances; if this vertex is already 
		registered, it was part of another instance. We need to add edges between
		the two instances. */
		if ( GV->graphTemplate->verticesTemplate[graphVertexIndex] )
		{
			label = GV->labelList[graphVertex->labelIndex];
			if ( label.labelType == 's' )                           /* string label */
				sprintf( newLabel, "%s%lu - V %s", SUB_LABEL_PREFIX, GV->subNumber,
						label.content.stringValue );
			else								                    /* numeric label */
				sprintf( newLabel, "%s%lu - V %f", SUB_LABEL_PREFIX, GV->subNumber,
						label.content.numericValue );

			labelIndex = AddLabelStr( GV, newLabel, newLabel, FALSE, FALSE );
			
			/* This undirected edge represents the fact that this vertex is shared 
			between two instances. */
			AddEdge( workGraph,
				GV->graphTemplate->verticesTemplate[graphVertexIndex] - 1,
				subGraphVertexIndex, labelIndex, UNDIRECTED, GV->finalGraph );
			
			GV->graphTemplate->verticesTemplate[graphVertexIndex] =
				subGraphVertexIndex + 1;
			graphVertexEdges = graphVertex->edges;
			vertexEdgesTemplate = GV->graphTemplate->edgesTemplate[graphVertexIndex];
			for ( edgeIndex = 0; edgeIndex < currentVertex->numberOfEdges; edgeIndex++ )
			{
				currentGraphEdgeIndex = 
					subGraph->edges[currentVertex->edgesIndex + 
					edgeIndex].indexInGraphVertex;
			
				if ( vertexEdgesTemplate[currentGraphEdgeIndex] )
				{
//{U}
/*					if ( graphVertexEdges[currentGraphEdgeIndex].directed ||
						 (graphVertexIndex <
						  graphVertexEdges[currentGraphEdgeIndex].targetVertexIndex) )
					{
*/						label = GV->labelList[graphVertexEdges[currentGraphEdgeIndex].labelIndex];
						if ( label.labelType == 's' )	            /* string label */
							sprintf( newLabel,"%s%lu - E %s", SUB_LABEL_PREFIX, GV->subNumber,
							label.content.stringValue );
						else										/* numeric label */
							sprintf( newLabel,"%s%lu - E %f", SUB_LABEL_PREFIX, GV->subNumber,
							label.content.numericValue );

						labelIndex = AddLabelStr( GV, newLabel, newLabel, FALSE, FALSE );
						
						/* This undirected edge represents the fact that this edge is shared
						between two instances */
						AddEdge( workGraph,
							GV->graphTemplate->verticesTemplate[graphVertexIndex] - 1,
							subGraphVertexIndex, labelIndex, UNDIRECTED,
							GV->finalGraph );
//					}
					vertexEdgesTemplate[currentGraphEdgeIndex] = subGraphVertexIndex + 1;
				}
				else
				{
					edgeUniqueCoverage++;
					vertexEdgesTemplate[currentGraphEdgeIndex] = subGraphVertexIndex + 1;
				}
			}
		}
		
		/* no overlap; register the vertex and its edges */
		else
		{
			GV->graphTemplate->verticesTemplate[graphVertexIndex] =
				subGraphVertexIndex + 1;
			vertexUniqueCoverage++;
			vertexEdgesTemplate = GV->graphTemplate->edgesTemplate[graphVertexIndex];
			for ( edgeIndex = 0; edgeIndex < currentVertex->numberOfEdges; edgeIndex++ )
			{
				edgeUniqueCoverage++;
				vertexEdgesTemplate[subGraph->edges[currentVertex->edgesIndex +
					edgeIndex].indexInGraphVertex] = 
					subGraphVertexIndex + 1;
			}
		}
	}
	subGraph->uniqueCoverage = edgeUniqueCoverage + vertexUniqueCoverage;

	return vertexUniqueCoverage;
}


/*******************************************************************************
FUNCTION NAME: ReSetTemplate
INPUTS:		PGRAPH_VARIABLES GV
RETURNS:	none
PURPOSE:	Reset GV->graphTemplate->verticesTemplate to 0.
CALLED BY:	tempop.c: CountOverlaps()
*******************************************************************************/

void ReSetTemplate( PGRAPH_VARIABLES GV )
{
	ULONG vertexIndex;
	ULONG *verticesTemplate;
	
#ifdef DEBUG_TRACE
	printf( "%s: ReSetTemplate()\n", __FILE__ );
#endif
	
	verticesTemplate = GV->graphTemplate->verticesTemplate;
	
	for ( vertexIndex = 0; vertexIndex < GV->graph->numberOfVertices; 
	vertexIndex++ )
		if ( verticesTemplate[vertexIndex] )
		{
			verticesTemplate[vertexIndex] = 0;
			memset( GV->graphTemplate->edgesTemplate[vertexIndex], 0,
				GV->graph->vertices[vertexIndex].numberOfEdges * 
				sizeof( ULONG ) );
		}
		return;
}


/*******************************************************************************
FUNCTION NAME: CountOverlaps
INPUTS:		PGRAPH_VARIABLES GV, 
			PLIST_OF_SUB_GRAPHS instances
RETURNS:	ULONG  
PURPOSE:	count the number of vertices and edges in instances that are 
			registered in the graph template.
CALLED BY:	compress.c: SizeOfCompressedGraph()
*******************************************************************************/

ULONG CountOverlaps( PGRAPH_VARIABLES GV, PLIST_OF_SUB_GRAPHS instances ) 
{
	PSUB_GRAPH_VERTEX currentVertex;
	ULONG vertexIndex;
	ULONG edgeIndex;
	ULONG graphVertexIndex;
	ULONG *vertexEdgesTemplate;
	ULONG currentGraphEdgeIndex;
	ULONG overlapsCount = 0;
	PSUB_GRAPH subGraph;
	PGRAPH_EDGE graphVertexEdges;
	
#ifdef DEBUG_TRACE
	printf( "%s: CountOverlaps()\n", __FILE__ );
#endif
	
	InitSubGraphsList( instances );
	while ( ( subGraph = GetNextSubGraph( instances ) ) != NULL )
	{
		subGraph->uniqueCoverage = 0;
		for ( vertexIndex = 0; vertexIndex < subGraph->numberOfVertices;
		vertexIndex++ )
		{
			currentVertex = &subGraph->vertices[vertexIndex];
			graphVertexIndex = currentVertex->indexInGraph;
			graphVertexEdges = GV->graph->vertices[graphVertexIndex].edges;
			if ( GV->graphTemplate->verticesTemplate[graphVertexIndex] )
			{
				overlapsCount++; 
				vertexEdgesTemplate = 
					GV->graphTemplate->edgesTemplate[graphVertexIndex];
				for ( edgeIndex = 0; edgeIndex < currentVertex->numberOfEdges;
				edgeIndex++ )
				{
					currentGraphEdgeIndex = subGraph->edges[currentVertex->edgesIndex + 
						edgeIndex].indexInGraphVertex;
					if ( vertexEdgesTemplate[currentGraphEdgeIndex] )
					{
//{U}
/*						if ( graphVertexEdges[currentGraphEdgeIndex].directed ||
							(graphVertexIndex <
							graphVertexEdges[currentGraphEdgeIndex].targetVertexIndex) 
							)
*/		
							overlapsCount++;
/*						else
						{
							subGraph->uniqueCoverage++;
							vertexEdgesTemplate[currentGraphEdgeIndex] = 1;
						}
*/
					}
				}
			}
			else
			{
				GV->graphTemplate->verticesTemplate[graphVertexIndex] = 1;
				subGraph->uniqueCoverage++;
				vertexEdgesTemplate = 
					GV->graphTemplate->edgesTemplate[graphVertexIndex];
				for ( edgeIndex = 0; edgeIndex < currentVertex->numberOfEdges;
				edgeIndex++ )
				{
					subGraph->uniqueCoverage++;
					vertexEdgesTemplate[subGraph->edges[currentVertex->edgesIndex + 
						edgeIndex].indexInGraphVertex] = 1;
				}
			}
		}
	}

	ReSetTemplate( GV );
	return overlapsCount;
}
