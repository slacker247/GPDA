/********************************************************************
*
* SUBDUE
*
* FILE NAME: extend.c
*
********************************************************************/


#include "subdue.h"


/*******************************************************************************
FUNCTION NAME: ExtendInstances
INPUTS:		PGRAPH_VARIABLES GV, PSUB sub
RETURNS:	none
PURPOSE:	Extend the instances of sub in all possible ways by adding either an 
			edge (for outgoing edges, if the target vertex of an edge is already 
			registered in the template) or a vertex plus edge (for incoming 
			edges, if the source vertex is not registered, or for outgoing 
			edges, if the target vertex is not yet registered) to sub.
CALLED BY:	extend.c: ExtendSub()
			subdue.c: DiscoverTargetSub()
*******************************************************************************/

void ExtendInstances( PGRAPH_VARIABLES GV, PSUB sub )
{
	PSUB_GRAPH currentInstance;
	PEXPANDED_INSTANCE_VE newVEInstanceDisc;
	PEXPANDED_INSTANCE_E newEInstanceDisc;
	ULONG vertexIndex;
	ULONG edgeIndex;
	ULONG graphVertexIndex;
	ULONG targetVertexIndex;
	PGRAPH_EDGE vertexEdges;
	PGRAPH_VERTEX currentVertex;
	PGRAPH_VERTEX currentInVertex;
	ULONG *edgesTemplate;
	ULONG *verticesTemplate;
	PLIST_OF_SUB_GRAPHS instancesList;
	ULONG *fanInVertices;
	ULONG fanInIndex;
	ULONG instanceNumber;
	ULONG vertexLabelIndex;
	ULONG edgeLabelIndex;
#ifdef _TIMING_
#ifdef _UNIX_
	struct tms tmsStart;
	struct tms tmsEnd;
#else // WIN32
	clock_t start, end;
#endif
#endif
	
#ifdef DEBUG_TRACE
	printf( "%s: ExtendInstances()\n", __FILE__ );
#endif
	
#ifdef _TIMING_
#ifdef _UNIX_
	times( &tmsStart );
#else // WIN32
	start = clock();
#endif
#endif
	
	instancesList = sub->instances;
	instanceNumber = 0;
	
	InitSubGraphsList( instancesList );
	
	// for all instances of the substructure
	while ( (currentInstance = GetNextSubGraph(instancesList)) != NULL )
	{
		currentInstance->numberInSub = instanceNumber;
		SetTemplate( GV, currentInstance );
		verticesTemplate = GV->graphTemplate->verticesTemplate;

		// for each vertex in the instance
		for (vertexIndex = 0; vertexIndex < currentInstance->numberOfVertices;
		vertexIndex++ )
		{
			graphVertexIndex = currentInstance->vertices[vertexIndex].indexInGraph;
			currentVertex = &GV->graph->vertices[graphVertexIndex];
			vertexEdges = currentVertex->edges;
			edgesTemplate = GV->graphTemplate->edgesTemplate[graphVertexIndex];
			fanInVertices = currentVertex->fanInVertices;

			// for all incoming edges, add the edge and the source vertex
			for ( fanInIndex = 0; fanInIndex < currentVertex->fanIn; fanInIndex++ ) {
				/* if the source vertex isn't registered, add a vertex plus edge */
				if ( !verticesTemplate[fanInVertices[fanInIndex]] )
				{    
					currentInVertex =
						&GV->graph->vertices[fanInVertices[fanInIndex]];
					for ( edgeIndex = 0; edgeIndex < currentInVertex->numberOfEdges;
					edgeIndex++ )
					{
						if ( currentInVertex->edges[edgeIndex].targetVertexIndex ==
							graphVertexIndex )
						{
							newVEInstanceDisc =				// make extended instance
								CreateNewVEInstance( currentInstance, vertexIndex,
									fanInVertices[fanInIndex], edgeIndex );
							vertexLabelIndex = currentInVertex->labelIndex;
							edgeLabelIndex = currentInVertex->edges[edgeIndex].labelIndex;
							// insert into template list (make new template)
							InsertInVETemplatesList( &GV->expandTemplates[edgeLabelIndex],
								newVEInstanceDisc, 
								vertexLabelIndex,
								edgeLabelIndex,
								currentInstance->matchingOrder[vertexIndex],
								currentInVertex->edges[edgeIndex].directed,
								FALSE );
						}
					}
				}
			}

			// for all edges
			for ( edgeIndex = 0; edgeIndex < currentVertex->numberOfEdges; edgeIndex++ )
			{
				targetVertexIndex = vertexEdges[edgeIndex].targetVertexIndex;
				if( !edgesTemplate[edgeIndex] )
				{
					/* if the target vertex is registered, add an edge */
					if ( verticesTemplate[targetVertexIndex] )
					{
						// add an edge only
						newEInstanceDisc = 				// make extended instance
							CreateNewEInstance( currentInstance,
								vertexIndex, verticesTemplate[targetVertexIndex] - 1, edgeIndex );
						edgeLabelIndex = vertexEdges[edgeIndex].labelIndex;
						// insert into template list (make new template)
						InsertInETemplatesList( &GV->expandTemplates[edgeLabelIndex],
							newEInstanceDisc,
							edgeLabelIndex,
							currentInstance->matchingOrder[vertexIndex],
							currentInstance->matchingOrder[verticesTemplate
							[targetVertexIndex] - 1],
							vertexEdges[edgeIndex].directed );
					}
					else  /* the target vertex isn't registered; add a vertex plus edge */
					{
						// add an edge and the target vertex
						newVEInstanceDisc = CreateNewVEInstance( currentInstance,
							vertexIndex,targetVertexIndex,edgeIndex );
						vertexLabelIndex =
							GV->graph->vertices[targetVertexIndex].labelIndex;
						edgeLabelIndex = vertexEdges[edgeIndex].labelIndex;
						InsertInVETemplatesList( &GV->expandTemplates[edgeLabelIndex],
							newVEInstanceDisc, 
							vertexLabelIndex, 
							edgeLabelIndex,
							currentInstance->matchingOrder[vertexIndex],
							vertexEdges[edgeIndex].directed, 
							TRUE );
					}
				}
				else 
					edgesTemplate[edgeIndex] = 0;
			}
		}
		instanceNumber++;
		ResetVerticesTemplate( GV, currentInstance );
	}
	
#ifdef _TIMING_
#ifdef _UNIX_
	times( &tmsEnd );
	GV->extendInstancesTm += ( tmsEnd.tms_utime - tmsStart.tms_utime ) /
		GV->clockTick;
#else // WIN32
	end = clock();
	GV->extendInstancesTm += ( end - start ) / GV->clockTick;
#endif
#endif
}


/*******************************************************************************
FUNCTION NAME: NoInstanceOverlap
INPUTS:		PSUB_GRAPH candidateInstance, 
			PLIST_OF_SUB_GRAPHS listOfInstances
RETURNS:	BOOLEAN: FALSE if overlap is found, TRUE othervise
PURPOSE:	Determine whether candidateInstance has any vertices in common with
			any of the instances in listOfInstances.
CALLED BY:	extemp.c: CreateSubFromVETemplate()
			extemp.c: AddVENoise()
			extemp.c: AddENoise()
			extemp.c: CreateSubFromETemplate()
*******************************************************************************/

BOOLEAN NoInstanceOverlap( PSUB_GRAPH candidateInstance, 
					   PLIST_OF_SUB_GRAPHS listOfInstances )
{
	PSUB_GRAPH currentInstance;
	ULONG vertexIndex1;
	ULONG vertexIndex2;
	
#ifdef DEBUG_TRACE
	printf( "%s: NoInstanceOverlap()\n", __FILE__ );
#endif
	
	InitSubGraphsList( listOfInstances );
	while ( (currentInstance = GetNextSubGraph(listOfInstances)) != NULL )
	{
		vertexIndex1 = 0;
		vertexIndex2 = 0;
		while ( (vertexIndex1 < candidateInstance->numberOfVertices) &&
			    (vertexIndex2 < currentInstance->numberOfVertices) )
		{
			if ( candidateInstance->vertices[vertexIndex1].indexInGraph ==
				 currentInstance->vertices[vertexIndex2].indexInGraph )
			{
				return FALSE;								// overlap found
			}
			else {
				if ( candidateInstance->vertices[vertexIndex1].indexInGraph <
					 currentInstance->vertices[vertexIndex2].indexInGraph )
				{
					vertexIndex1++;
				}
				else {
					vertexIndex2++;
				}
			}
		}
	}

	return TRUE;											// no overlap found
}


/*******************************************************************************
FUNCTION NAME: ExtendSub
INPUTS:		PGRAPH_VARIABLES GV, 
			PSUB sub, 
			PLIST_OF_SUBS extendedSubs
RETURNS:	none
PURPOSE:	Extend the instances of sub and get the best substructures of the 
			extended instances.
CALLED BY:	subdue.c: Subdue()
			subdue.c: Discover()
*******************************************************************************/

void ExtendSub( PGRAPH_VARIABLES GV, PSUB sub, PLIST_OF_SUBS extendedSubs,
			   DOUBLE worstValueOnChildList )
{
	
#ifdef DEBUG_TRACE
	printf( "%s: ExtendSub()\n", __FILE__ );
#endif
	
	ExtendInstances( GV, sub );
//	GetBestSubs( GV, extendedSubs, sub );
	GetBestSubs2( GV, extendedSubs, sub, worstValueOnChildList );
	return;
}
