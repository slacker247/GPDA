/********************************************************************
*
* SUBDUE
*
* FILE NAME: extemp.c
*
********************************************************************/


#include "subdue.h"



/*
Gehad Galal:
The new approach for Subdue is contained in this file, note the following:

	1- The different ways of expanding a substructure are gathered in the global 
	array GV.expandTemplates which contain an entry for each label in the graph.
	The new expanded instances are indexed into the array by the label of the 
	edge. So for each edge label we have two lists:
  
		1- list of subgraph that result from the addition of an edge with the
		same label.
		
		2- list of subgraphs that result from the addition of an edge with the
		same label and a vertex of any label.

	Actually I was affected in this approach by the sample databases we have that 
	has a lot of edge labels but few vertices labels. Ideally the grouping of 
	expanded subgraphs should be done to minimize the resulting lists' lengths.
*/

/*******************************************************************************
FUNCTION NAME: DestroyVETemplate
INPUTS:		PVE_TEMPLATE VETemplate
RETURNS:	none
PURPOSE:	Free the memory used by VETemplate.
CALLED BY:	extemp.c: DestroyExpTemp()
*******************************************************************************/

void DestroyVETemplate( PVE_TEMPLATE VETemplate )
{
	PEXPANDED_INSTANCE_VE currentVEInstance;
	PEXPANDED_INSTANCE_VE oldVEInstance;
	
#ifdef DEBUG_TRACE
	printf( "%s: DestroyVETemplate()\n", __FILE__ );
#endif
	
	currentVEInstance = VETemplate->listOfInstances;
	while ( currentVEInstance != NULL )
	{
		oldVEInstance = currentVEInstance;
		currentVEInstance = currentVEInstance->next;
		DestroySubGraph( oldVEInstance->parentInstance );
		Free( oldVEInstance );
	}
	Free( VETemplate );
	return;
}

/*******************************************************************************
FUNCTION NAME: DestroyETemplate
INPUTS:		PE_TEMPLATE ETemplate
RETURNS:	none 
PURPOSE:	Free the memory used by ETemplate.
CALLED BY:	extemp.c: DestroyExpTemp()
*******************************************************************************/

void DestroyETemplate( PE_TEMPLATE ETemplate )
{
	PEXPANDED_INSTANCE_E currentEInstance;
	PEXPANDED_INSTANCE_E oldEInstance;
	
#ifdef DEBUG_TRACE
	printf( "%s: DestroyETemplate()\n", __FILE__ );
#endif
	
	currentEInstance = ETemplate->listOfInstances;
	while ( currentEInstance != NULL )
	{
		oldEInstance = currentEInstance;
		currentEInstance = currentEInstance->next;
		DestroySubGraph( oldEInstance->parentInstance );
		Free( oldEInstance );
	}
	Free( ETemplate );
	return;
}


/*******************************************************************************
FUNCTION NAME: DestroyExpTemp
INPUTS:		PGRAPH_VARIABLES GV
RETURNS:	none
PURPOSE:	Free the memory used by the linked lists attached to 
			GV->expandTemplates[]
CALLED BY:	extemp.c: GetBestSubs()
*******************************************************************************/

void DestroyExpTemp( PGRAPH_VARIABLES GV )
{
	ULONG index;
	PVE_TEMPLATE currentVETemplate;
	PVE_TEMPLATE oldVETemplate;
	PE_TEMPLATE currentETemplate;
	PE_TEMPLATE oldETemplate;
	PCONSISTENT_VE_LIST_NODE currentVEConsistent;
	PCONSISTENT_VE_LIST_NODE oldVEConsistent;
	PCONSISTENT_E_LIST_NODE currentEConsistent;
	PCONSISTENT_E_LIST_NODE oldEConsistent;
	
#ifdef DEBUG_TRACE
	printf( "%s: DestroyExpTemp()\n", __FILE__ );
#endif
	
	for( index = 0; index < GV->numberOfLabels; index++ )
	{
		if ( GV->expandTemplates[index].VETemplates != NULL )
		{
			currentVETemplate = GV->expandTemplates[index].VETemplates;
			while ( currentVETemplate != NULL )
			{
				currentVEConsistent = currentVETemplate->consistentList;
				while ( currentVEConsistent != NULL )
				{
					oldVEConsistent = currentVEConsistent;
					currentVEConsistent = currentVEConsistent->next;
					DestroyVETemplate( oldVEConsistent->Template );
					Free( oldVEConsistent->matchingPattern );
					Free( oldVEConsistent );
				}
				oldVETemplate = currentVETemplate;
				currentVETemplate = currentVETemplate->nextTemplate;
				
				DestroyVETemplate( oldVETemplate );
			}
		}
		if ( GV->expandTemplates[index].ETemplates != NULL )
		{
			currentETemplate = GV->expandTemplates[index].ETemplates;
			while ( currentETemplate != NULL )
			{
				currentEConsistent = currentETemplate->consistentList;
				while ( currentEConsistent != NULL )
				{
					oldEConsistent = currentEConsistent;
					currentEConsistent = currentEConsistent->next;
					DestroyETemplate( oldEConsistent->Template );
					Free( oldEConsistent->matchingPattern );
					Free( oldEConsistent );
				}
				oldETemplate = currentETemplate;
				currentETemplate = currentETemplate->nextTemplate;
				DestroyETemplate( oldETemplate );
			}
		}
		GV->expandTemplates[index].VETemplates = NULL;
		GV->expandTemplates[index].ETemplates = NULL;
	}
	return;
}


/*******************************************************************************
FUNCTION NAME: CreateVEInstanceSubGraph
INPUTS:		PGRAPH_VARIABLES GV, 
			PVE_TEMPLATE Template, 
			PEXPANDED_INSTANCE_VE instanceDisc, 
			LONG *newVertexIndex,
			LONG *newEdgeIndex1, 
			LONG *newEdgeIndex2
RETURNS:	PSUB_GRAPH
PURPOSE:	Call subgphop.c: AddEdgeAndVertexToSubGraph() to create a new 
			VE-instance subGraph.
CALLED BY:	extemp.c: CreateSubFromVETemplate()
			extemp.c: AddVENoise()
			extemp.c: AddENoise()
*******************************************************************************/

PSUB_GRAPH CreateVEInstanceSubGraph( PGRAPH_VARIABLES GV, PVE_TEMPLATE Template, 
									PEXPANDED_INSTANCE_VE instanceDisc,
									LONG *newVertexIndex,
									LONG *newEdgeIndex1, 
									LONG *newEdgeIndex2 )
{
	PSUB_GRAPH newSubGraph;
	PSUB_GRAPH parent;
	ULONG sourceIndexInSub;
	ULONG sourceIndexInGraph;
	ULONG targetIndexInGraph;
	ULONG edgeIndex;
	BOOLEAN sourceEdge;
	PGRAPH_EDGE addedEdge;
	
#ifdef DEBUG_TRACE
	printf( "%s: CreateVEInstanceSubGraph()\n", __FILE__ );
#endif
	
	parent = instanceDisc->parentInstance;
	sourceIndexInSub = instanceDisc->sourceVertexIndex;
	sourceIndexInGraph = 
		parent->vertices[instanceDisc->sourceVertexIndex].indexInGraph;
	targetIndexInGraph = instanceDisc->newVertexIndexInGraph;
	edgeIndex = instanceDisc->addedEdgeIndex;
	sourceEdge = Template->sourceIsOldVertex;
	if ( sourceEdge )
		addedEdge = &GV->graph->vertices[sourceIndexInGraph].edges[edgeIndex];
	else addedEdge = NULL;
	
	newSubGraph = AddEdgeAndVertexToSubGraph( GV, parent, sourceIndexInSub,
		sourceIndexInGraph, targetIndexInGraph, edgeIndex, addedEdge, sourceEdge,
		newVertexIndex, newEdgeIndex1, newEdgeIndex2 );
	
	return newSubGraph;
}						 	 


/*******************************************************************************
FUNCTION NAME: CreateEInstanceSubGraph
INPUTS:		PE_TEMPLATE Template, 
			PEXPANDED_INSTANCE_E instanceDisc,
			LONG *newEdgeIndex1, 
			LONG *newEdgeIndex2
RETURNS:	PSUB_GRAPH 
PURPOSE:	Call subgphop.c: AddEdgeToSubGraph() to create a new E-instance
			subGraph.
CALLED BY:	extemp.c: AddVENoise()
			extemp.c: AddENoise()
			extemp.c: CreateSubFromETemplate()
*******************************************************************************/

PSUB_GRAPH CreateEInstanceSubGraph( PGRAPH_VARIABLES GV, /*PE_TEMPLATE Template, */
								   PEXPANDED_INSTANCE_E instanceDisc,
								   LONG *newEdgeIndex1,
								   LONG *newEdgeIndex2 )
{
	PSUB_GRAPH newSubGraph;
	PSUB_GRAPH parent;
	ULONG sourceIndexInSub;
	ULONG sourceIndexInGraph;
	ULONG targetIndexInSub;
	ULONG edgeIndex;
	PGRAPH_EDGE addedEdge;
	
#ifdef DEBUG_TRACE
	printf( "%s: CreateEInstanceSubGraph()\n", __FILE__ );
#endif
	
	parent = instanceDisc->parentInstance;
	sourceIndexInSub = instanceDisc->sourceVertexIndex;
	sourceIndexInGraph = 
		parent->vertices[instanceDisc->sourceVertexIndex].indexInGraph;
	targetIndexInSub = instanceDisc->targetVertexIndex;
	edgeIndex = instanceDisc->addedEdgeIndex;
	addedEdge = &GV->graph->vertices[sourceIndexInGraph].edges[edgeIndex];
	
	newSubGraph = AddEdgeToSubGraph( GV, parent, sourceIndexInSub,
		sourceIndexInGraph, targetIndexInSub,
		edgeIndex, addedEdge, newEdgeIndex1,
		newEdgeIndex2 );
	return newSubGraph;
}						 	 


/*******************************************************************************
FUNCTION NAME: CreateNewVEInstance
INPUTS:		PSUB_GRAPH parent, 
			ULONG sourceVertexIndex, 
			ULONG newVertexIndexInGraph, 
			ULONG addedEdgeIndex
RETURNS:	PEXPANDED_INSTANCE_VE
PURPOSE:	Allocate and initialize a new EXPANDED_INSTANCE_VE data structure.
CALLED BY:	extend.c: ExtendInstances()
*******************************************************************************/

PEXPANDED_INSTANCE_VE CreateNewVEInstance( PSUB_GRAPH parent, 
										   ULONG sourceVertexIndex, 
										   ULONG newVertexIndexInGraph, 
										   ULONG addedEdgeIndex )
{
	PEXPANDED_INSTANCE_VE newInstance;
	
#ifdef DEBUG_TRACE
	printf( "%s: CreateNewVEInstance()\n", __FILE__ );
#endif
	
	newInstance = (PEXPANDED_INSTANCE_VE) Malloc( sizeof( EXPANDED_INSTANCE_VE ) );
	newInstance->parentInstance = parent;
	parent->refCount++;
	newInstance->sourceVertexIndex = sourceVertexIndex;
	newInstance->newVertexIndexInGraph = newVertexIndexInGraph;
	newInstance->addedEdgeIndex = addedEdgeIndex;
	newInstance->next = NULL;
	
	return newInstance;
}


/*******************************************************************************
FUNCTION NAME: InsertInVETemplatesList
INPUTS:		PEDGE_LABEL_TEMPLATES edgeTemplates, 
			PEXPANDED_INSTANCE_VE newInstance, 
			ULONG newVertexLabel, 
			LONG sourceVertexMatchIndex,
			BOOLEAN edgeDirection, 
			BOOLEAN sourceIsOldVertex
RETURNS:	none
PURPOSE:	Search the list edgeTemplates->VETemplates for a VETemplate that 
			matches newInstance.  If found, insert newInstance into the 
			VETemplate's list of instances; if not found, create a new 
			VETemplate for the newInstance and insert it into the list of VETemplates.
CALLED BY:	extend.c: ExtendInstances()
*******************************************************************************/

void InsertInVETemplatesList( PEDGE_LABEL_TEMPLATES edgeTemplates, 
							 PEXPANDED_INSTANCE_VE newInstance, 
							 ULONG newVertexLabelIndex,
							 ULONG newEdgeLabelIndex,
							 ULONG sourceVertexMatchIndex,
							 BOOLEAN edgeDirection, 
							 BOOLEAN sourceIsOldVertex )
{
	
	PVE_TEMPLATE currentTemplate;
	PVE_TEMPLATE preTemplate;
	
#ifdef DEBUG_TRACE
	printf( "%s: InsertInVETemplatesList()\n", __FILE__ );
#endif
	
	currentTemplate = edgeTemplates->VETemplates;
	preTemplate = NULL;
	while ( currentTemplate != NULL ) {
		/* if currentTemplate matches newInstance */
		if (currentTemplate->newVertexLabelIndex == newVertexLabelIndex &&
			currentTemplate->sourceVertexMatchIndex == sourceVertexMatchIndex &&
			currentTemplate->edgeDirection == edgeDirection &&
			currentTemplate->sourceIsOldVertex == sourceIsOldVertex )
		{
			if ( !currentTemplate->allNoisy && 
				newInstance->parentInstance->matchCost > MATCH_COST_SAFE_MARGINE )
			{
				newInstance->next = currentTemplate->listOfInstances->next;
				currentTemplate->listOfInstances->next = newInstance;
			}
			else 
			{
				if ( newInstance->parentInstance->matchCost < MATCH_COST_SAFE_MARGINE )
					currentTemplate->allNoisy = FALSE;
				
				if ( newInstance->parentInstance->matchCost <= 
					currentTemplate->listOfInstances->parentInstance->matchCost ) 
				{
				/* put newInstance at the head of the list; it's a candidate to 
					become the definition */
					newInstance->next = currentTemplate->listOfInstances;
					currentTemplate->listOfInstances = newInstance;
				}
				else
				{
					/* put newInstance behind the head; it will just be a sub instance */
					newInstance->next = currentTemplate->listOfInstances->next;
					currentTemplate->listOfInstances->next = newInstance;
				}
			}

			currentTemplate->numberOfInstances++;
			return;								// found instance that matches newInstance
		}
		else 
		{
			preTemplate = currentTemplate;
			currentTemplate = currentTemplate->nextTemplate;
		}
	}
		
	/* failed to find a template that matches this instance; create a new template */
	currentTemplate = (PVE_TEMPLATE) Malloc( sizeof( VE_TEMPLATE ) );
	currentTemplate->newVertexLabelIndex = newVertexLabelIndex;
	currentTemplate->newEdgeLabelIndex = newEdgeLabelIndex;
	currentTemplate->sourceVertexMatchIndex = sourceVertexMatchIndex;
	currentTemplate->edgeDirection = edgeDirection;
	currentTemplate->sourceIsOldVertex = sourceIsOldVertex;
	currentTemplate->numberOfInstances = 1;
	currentTemplate->listOfInstances = newInstance;
	currentTemplate->consistentList = NULL;

	if (newInstance->parentInstance->matchCost < MATCH_COST_SAFE_MARGINE)
		currentTemplate->allNoisy = FALSE;
	else 
		currentTemplate->allNoisy = TRUE;
	
	currentTemplate->nextTemplate = NULL;
	currentTemplate->nextBestTemplate = NULL;

	if ( preTemplate == NULL )
		edgeTemplates->VETemplates = currentTemplate;
	else 
		preTemplate->nextTemplate = currentTemplate;

	return;
}


/*******************************************************************************
FUNCTION NAME: CreateNewEInstance
INPUTS:		PSUB_GRAPH parent, 
			ULONG sourceVertexIndex,
			ULONG targetVertexIndex,
			ULONG addedEdgeIndex
RETURNS:	PEXPANDED_INSTANCE_E
PURPOSE:	Allocate and initialize a new EXPANDED_INSTANCE_E data structure.
CALLED BY:	extend.c: ExtendInstances()
*******************************************************************************/

PEXPANDED_INSTANCE_E CreateNewEInstance( PSUB_GRAPH parent, 
									   ULONG sourceVertexIndex,
									   ULONG targetVertexIndex,
									   ULONG addedEdgeIndex )
{
	PEXPANDED_INSTANCE_E newInstance;
	
#ifdef DEBUG_TRACE
	printf( "%s: CreateNewEInstance()\n", __FILE__ );
#endif
	
	newInstance = (PEXPANDED_INSTANCE_E) Malloc( sizeof( EXPANDED_INSTANCE_E ) );
	newInstance->parentInstance = parent;
	parent->refCount++;
	newInstance->sourceVertexIndex = sourceVertexIndex;
	newInstance->targetVertexIndex = targetVertexIndex;
	newInstance->addedEdgeIndex = addedEdgeIndex;
	newInstance->next = NULL;
	return newInstance;
}


/*******************************************************************************
FUNCTION NAME: InsertInETemplatesList
INPUTS:		PEDGE_LABEL_TEMPLATES edgeTemplates, 
			PEXPANDED_INSTANCE_E newInstance, 
			LONG sourceVertexMatchIndex,
			LONG targetVertexMatchIndex,
			BOOLEAN edgeDirection
RETURNS:	none
PURPOSE:	Search the list edgeTemplates->ETemplates for an ETemplate that 
			matches newInstance.  If found, insert newInstance into the ETemplate's
			list of instances; if not found, create a new ETemplate for the 
			newInstance and insert it into the list of ETemplates.
CALLED BY:	extend.c: ExtendInstances()
*******************************************************************************/

void InsertInETemplatesList( PEDGE_LABEL_TEMPLATES edgeTemplates, 
							PEXPANDED_INSTANCE_E newInstance, 
							ULONG newEdgeLabelIndex,
							ULONG sourceVertexMatchIndex,
							ULONG targetVertexMatchIndex,
							BOOLEAN edgeDirection )
{
	
	PE_TEMPLATE currentTemplate;
	PE_TEMPLATE preTemplate;
	
#ifdef DEBUG_TRACE
	printf( "%s: InsertInETemplatesList()\n", __FILE__ );
#endif
	
	currentTemplate = edgeTemplates->ETemplates;
	preTemplate = NULL;
	
	while ( currentTemplate != NULL ) {
		if ( ( currentTemplate->sourceVertexMatchIndex == sourceVertexMatchIndex &&
			currentTemplate->targetVertexMatchIndex == targetVertexMatchIndex &&
			currentTemplate->edgeDirection == edgeDirection ) ||
			( currentTemplate->sourceVertexMatchIndex == targetVertexMatchIndex &&
			currentTemplate->targetVertexMatchIndex == sourceVertexMatchIndex &&
			currentTemplate->edgeDirection == edgeDirection &&
			edgeDirection == UNDIRECTED ) )
		{
			if ( ( !currentTemplate->allNoisy ) && 
				( newInstance->parentInstance->matchCost > 
				MATCH_COST_SAFE_MARGINE ) )
			{
				newInstance->next = currentTemplate->listOfInstances->next;
				currentTemplate->listOfInstances->next = newInstance;
			}
			else 
			{
				if ( newInstance->parentInstance->matchCost < MATCH_COST_SAFE_MARGINE )
					currentTemplate->allNoisy = FALSE;
				if ( newInstance->parentInstance->matchCost <=
					currentTemplate->listOfInstances->parentInstance->matchCost ) 
				{
					newInstance->next = currentTemplate->listOfInstances;
					currentTemplate->listOfInstances = newInstance;
				}
				else
				{
					newInstance->next = currentTemplate->listOfInstances->next;
					currentTemplate->listOfInstances->next = newInstance;
				}
			}
			currentTemplate->numberOfInstances++;
			return;
		}
		else 
		{
			preTemplate = currentTemplate;
			currentTemplate = currentTemplate->nextTemplate;
		}
	}

	/* failed to find a template that matches newInstance; create a new template */
	currentTemplate = (PE_TEMPLATE) Malloc( sizeof( E_TEMPLATE ) );
	currentTemplate->newEdgeLabelIndex = newEdgeLabelIndex;
	currentTemplate->sourceVertexMatchIndex = sourceVertexMatchIndex;
	currentTemplate->targetVertexMatchIndex = targetVertexMatchIndex;
	currentTemplate->edgeDirection = edgeDirection;
	currentTemplate->numberOfInstances = 1;
	currentTemplate->listOfInstances = newInstance;
	currentTemplate->consistentList = NULL;
	
	if ( newInstance->parentInstance->matchCost < MATCH_COST_SAFE_MARGINE )
		currentTemplate->allNoisy = FALSE;
	else 
		currentTemplate->allNoisy = TRUE;
	
	currentTemplate->nextTemplate = NULL;
	currentTemplate->nextBestTemplate = NULL;
	if ( preTemplate == NULL )
		edgeTemplates->ETemplates = currentTemplate;
	else 
		preTemplate->nextTemplate = currentTemplate;
	return;
}


/*******************************************************************************
FUNCTION NAME: AddVENoise
INPUTS:		PGRAPH_VARIABLES GV, PVE_TEMPLATE Template, 
			PSUB newSub, 
			PSUB_GRAPH definition, 
			BOOLEAN *instanceIncluded
RETURNS:	none
PURPOSE:	For the old instances that did not result in an exact match to the
			definition of the new substructure "newSub", this function tries 
			to find an expansion of these instances that can match the 
			substructure within the noise threshold. 

Note the following:

	1- The expanded instances of the not already included old instances are 
	considered in the order they appear in the expansion table. This means
	that the chosen instance does not necessary minimize the noise. Another
	visible approach is to maintain a list for each old instance that contains
	all the resulting instances from its expansion, and then for each instance
	that is not included choose the expanded instance that minimizes the 
	matching cost.

	2- This method of detecting noise does not necessarily detect the noisy 
	data for the following reason:
	When we try to add noise we consider any instance that satisfies the 
	matching threshold as a noise and include it, while the information added
	to this instance in this stage is not guaranteed in any way to be noise.
	This is because we do not consider any statistical distribution of the new
	information (i.e. if an edge is added to most of the instances it can not
	be considered as noise).

	3- Another approach without graph matching is possible and logical, that will
	make use of the stored information about how the instances where expanded
	and try to find the noise by finding other uncommon expansions for the same
	instance.

CALLED BY:	extemp.c: CreateSubFromVETemplate()
*******************************************************************************/
		  
void AddVENoise( PGRAPH_VARIABLES GV, PVE_TEMPLATE Template, PSUB newSub, 
				PSUB_GRAPH definition, BOOLEAN *instanceIncluded )
{
	ULONG index;
	ULONG index2;
	PVE_TEMPLATE currentVETemplate;
	PVE_TEMPLATE parentVETemplate;
	PE_TEMPLATE currentETemplate;
	PE_TEMPLATE parentETemplate;
	DOUBLE matchCost;
	PSUB_GRAPH newInstance;
	PEXPANDED_INSTANCE_VE currentVEInstance;
	PEXPANDED_INSTANCE_E currentEInstance;	
	PREVERSE_SUB_GRAPH defReverse;
	PREVERSE_SUB_GRAPH newInstanceReverse;
	LONG *matchingPattern;
	PCONSISTENT_VE_LIST_NODE consistentVETemplate;
	PCONSISTENT_E_LIST_NODE consistentETemplate;
#ifdef _TIMING_
#ifdef _UNIX_
	struct tms tmsStart;
	struct tms tmsEnd;
#else // WIN32
	clock_t start;
	clock_t end;
#endif
#endif
	
#ifdef DEBUG_TRACE
	printf( "%s: AddVENoise()\n", __FILE__ );
#endif
	
	if ( ( GV->matchCostMaxPercent == 0.0 ) || 
		( GV->matchCostMaxPercent * SizeOf( definition ) < MIN_MIS_MATCH_COST ) )
		return;
	
#ifdef _TIMING_
#ifdef _UNIX_
	times( &tmsStart );
#else // WIN32
	start = clock();
#endif
#endif
	
	/* Ideally for an instance that is not included yet in the new substructure
	the different matching costs of all the expanded instance from these 
	instance must be compared and the best chosen */
	
	defReverse = CreateReverseSubGraph( GV, definition );
	
	for ( index = 0; index < GV->numberOfLabels; index++ )
	{
		if ( GV->expandTemplates[index].VETemplates != NULL ) 
		{
			currentVETemplate = GV->expandTemplates[index].VETemplates;
			parentVETemplate = currentVETemplate;
			consistentVETemplate = currentVETemplate->consistentList; 
			while ( currentVETemplate != NULL )
			{
				if ( currentVETemplate != Template )
				{
					currentVEInstance = currentVETemplate->listOfInstances;
					while ( currentVEInstance != NULL )
					{
						newInstance = CreateVEInstanceSubGraph( GV, currentVETemplate,
							currentVEInstance, NULL, 
							NULL, NULL );
						if ( !MemberSubGraph( newInstance, newSub->instances ) )
						{
							newInstanceReverse = CreateReverseSubGraph( GV, newInstance );
							matchingPattern = (LONG *) Malloc( sizeof( LONG ) * 
								newInstance->numberOfVertices );		
							matchCost = InExactGraphMatch( GV, definition, defReverse, 
								newInstance, newInstanceReverse, GV->matchCostMaxPercent, NULL,
								NULL, 0, matchingPattern );
							DestroyReverseSubGraph( newInstanceReverse, newInstance );
							if ( matchCost > 0.0 )
							{
								if( !GV->allowInstanceOverlap ) 
								{
									if ( NoInstanceOverlap( newInstance, newSub->instances ) ) 
									{
										newInstance->matchCost = matchCost;
										for ( index2 = 0; index2 < newInstance->numberOfVertices;
										index2++ )
											if ( matchingPattern[index2] == 0 )
												newInstance->matchingOrder[index2] = NODE_DELETED;
											else 
												newInstance->matchingOrder[index2] =
												matchingPattern[index2] - 1;
											InsertInSubGraphsList( newSub->instances, newInstance );
											instanceIncluded[currentVEInstance->parentInstance->
												numberInSub] = TRUE;
											newSub->totalInstancesSize += SizeOf( newInstance );
									}
									else 
										DestroySubGraph( newInstance );
								}
								else
								{
									newInstance->matchCost = matchCost;
									for( index2 = 0; index2 < newInstance->numberOfVertices;
									index2++ )
										if ( matchingPattern[index2] == 0 )
											newInstance->matchingOrder[index2] = NODE_DELETED;
										else 
											newInstance->matchingOrder[index2] = 
											matchingPattern[index2] - 1;
									InsertInSubGraphsList( newSub->instances, newInstance );
									instanceIncluded[currentVEInstance->parentInstance->
										numberInSub] = TRUE;
									newSub->totalInstancesSize += SizeOf( newInstance );
								}
							}
							else
								DestroySubGraph( newInstance );
							Free( matchingPattern );
						}
						currentVEInstance = currentVEInstance->next;
					}
				}
				if ( consistentVETemplate == NULL )
				{
					currentVETemplate = parentVETemplate->nextTemplate;
					parentVETemplate = currentVETemplate;
					if ( currentVETemplate != NULL ) 
						consistentVETemplate  = currentVETemplate->consistentList;
				}
				else 
				{
					currentVETemplate = consistentVETemplate->Template;
					consistentVETemplate = consistentVETemplate->next;
				}	
			}
		}
		
		if ( GV->expandTemplates[index].ETemplates != NULL ) 
		{
			currentETemplate = GV->expandTemplates[index].ETemplates;
			parentETemplate = currentETemplate;
			consistentETemplate = currentETemplate->consistentList;
			while ( currentETemplate != NULL )
			{
				{
					currentEInstance = currentETemplate->listOfInstances;
					while ( currentEInstance != NULL )
					{
						newInstance = CreateEInstanceSubGraph( GV, 
										/*	currentETemplate,*/
											currentEInstance, 
											NULL,
											NULL );
						if ( !MemberSubGraph( newInstance, newSub->instances ) )
						{
							newInstanceReverse = CreateReverseSubGraph( GV, newInstance );
							matchingPattern = (LONG *) Malloc( sizeof( LONG ) * 
								newInstance->numberOfVertices );		
							matchCost = InExactGraphMatch( GV, definition, defReverse, 
								newInstance, newInstanceReverse,
								GV->matchCostMaxPercent, NULL, NULL, 0, matchingPattern );
							DestroyReverseSubGraph( newInstanceReverse, newInstance );
							if ( matchCost > 0 )
							{
								if( !GV->allowInstanceOverlap ) {
									if ( NoInstanceOverlap( newInstance, newSub->instances ) ) 
									{
										newInstance->matchCost = matchCost; 
										for ( index2 = 0; index2 < newInstance->numberOfVertices;
										index2++ )
											if ( matchingPattern[index2] == 0 )
												newInstance->matchingOrder[index2] = NODE_DELETED;
											else 
												newInstance->matchingOrder[index2] =
												matchingPattern[index2] - 1;
											if ( !MemberSubGraph( newInstance, newSub->instances ) )
											{
												InsertInSubGraphsList( newSub->instances, newInstance );
												instanceIncluded[currentEInstance->parentInstance->
													numberInSub] = TRUE;
												newSub->totalInstancesSize += SizeOf( newInstance );
											}
									}
									else 
										DestroySubGraph( newInstance );
								}
								else
								{
									newInstance->matchCost = matchCost; 
									for ( index2 = 0; index2 < newInstance->numberOfVertices;
									index2++ )
										if ( matchingPattern[index2] == 0 )
											newInstance->matchingOrder[index2] = NODE_DELETED;
										else 
											newInstance->matchingOrder[index2] = 
											matchingPattern[index2] - 1;
										InsertInSubGraphsList( newSub->instances, newInstance );
										instanceIncluded[currentEInstance->parentInstance->
											numberInSub] = TRUE;
										newSub->totalInstancesSize += SizeOf( newInstance );
								}
							}
							else
								DestroySubGraph( newInstance );
							Free( matchingPattern );
						}
						currentEInstance = currentEInstance->next;
					}
				}	
				if( consistentETemplate == NULL )
				{
					currentETemplate = parentETemplate->nextTemplate;
					parentETemplate = currentETemplate;
					if ( currentETemplate != NULL ) 
						consistentETemplate  = currentETemplate->consistentList;
				}
				else 
				{
					currentETemplate = consistentETemplate->Template;
					consistentETemplate = consistentETemplate->next;
				}	
			}
		}
  }
  DestroyReverseSubGraph( defReverse, definition );
  
#ifdef _TIMING_
#ifdef _UNIX_
	times( &tmsEnd );
	GV->addVENoiseTm += ( tmsEnd.tms_utime - tmsStart.tms_utime ) / GV->clockTick;
#else // WIN32
	end = clock();
	GV->addVENoiseTm += ( end - start ) / GV->clockTick;
#endif
#endif
 
	return;
}


/*******************************************************************************
FUNCTION NAME: AddENoise
INPUTS:		PGRAPH_VARIABLES GV, PE_TEMPLATE Template, 
			PSUB newSub,
			PSUB_GRAPH definition, 
			BOOLEAN *instanceIncluded
RETURNS:	none
PURPOSE:	See the notes for AddVENoise()
CALLED BY:	extemp.c: CreateSubFromETemplate()
*******************************************************************************/

void AddENoise( PGRAPH_VARIABLES GV, PE_TEMPLATE Template, PSUB newSub, 
			   PSUB_GRAPH definition, 
			   BOOLEAN *instanceIncluded )
{
	ULONG index;
	ULONG index2;
	PVE_TEMPLATE currentVETemplate;
	PVE_TEMPLATE parentVETemplate;
	PE_TEMPLATE currentETemplate;
	PE_TEMPLATE parentETemplate;
	DOUBLE matchCost;
	PSUB_GRAPH newInstance;
	PEXPANDED_INSTANCE_VE currentVEInstance;
	PEXPANDED_INSTANCE_E currentEInstance;	
	PREVERSE_SUB_GRAPH defReverse;
	PREVERSE_SUB_GRAPH newInstanceReverse;
	LONG *matchingPattern;
	PCONSISTENT_VE_LIST_NODE consistentVETemplate;
	PCONSISTENT_E_LIST_NODE consistentETemplate;
#ifdef _TIMING_
#ifdef _UNIX_
	struct tms tmsStart;
	struct tms tmsEnd;
#else // WIN32
	clock_t start;
	clock_t end;
#endif
#endif
	
#ifdef DEBUG_TRACE
	printf( "%s: AddENoise()\n", __FILE__ );
#endif
	
	if ( ( GV->matchCostMaxPercent == 0.0 ) ||
		( GV->matchCostMaxPercent * SizeOf( definition ) < MIN_MIS_MATCH_COST ) )
		return;
	
#ifdef _TIMING_
#ifdef _UNIX_
	times( &tmsStart );
#else // WIN32
	start = clock();
#endif
#endif
	
	/* Ideally for an instance that is not included yet in the new substructure
	the different matching costs of all the expanded instance from these 
	instance must be compared and the best chosen */
	
	defReverse = CreateReverseSubGraph( GV, definition );
	
	for ( index = 0; index < GV->numberOfLabels; index++ )
	{
		if ( GV->expandTemplates[index].VETemplates != NULL ) 
		{
			currentVETemplate = GV->expandTemplates[index].VETemplates;
			parentVETemplate = currentVETemplate;
			consistentVETemplate = currentVETemplate->consistentList;
			while ( currentVETemplate != NULL )
			{
				{
					currentVEInstance = currentVETemplate->listOfInstances;
					while ( currentVEInstance != NULL )
					{
						newInstance = CreateVEInstanceSubGraph( GV, currentVETemplate,
							currentVEInstance, NULL,
							NULL, NULL );
						if ( !MemberSubGraph( newInstance, newSub->instances ) )
						{
							newInstanceReverse = CreateReverseSubGraph( GV, newInstance );
							matchingPattern = (LONG *) Malloc( sizeof( LONG ) *
								newInstance->numberOfVertices );		
							matchCost = InExactGraphMatch( GV, definition, defReverse, 
								newInstance, newInstanceReverse, GV->matchCostMaxPercent,
								NULL, NULL, 0, matchingPattern );
							DestroyReverseSubGraph( newInstanceReverse, newInstance );
							if ( matchCost > 0 )
							{
								if ( !GV->allowInstanceOverlap ) {
									if ( NoInstanceOverlap( newInstance, newSub->instances ) ) 
									{
										newInstance->matchCost = matchCost; 
										for ( index2 = 0; index2 < newInstance->numberOfVertices;
										index2++ )
											if ( matchingPattern[index2] == 0 )
												newInstance->matchingOrder[index2] = NODE_DELETED;
											else
												newInstance->matchingOrder[index2] =
												matchingPattern[index2] - 1;
											InsertInSubGraphsList( newSub->instances, newInstance );
											instanceIncluded[currentVEInstance->parentInstance->
												numberInSub] = TRUE;
											newSub->totalInstancesSize += SizeOf( newInstance );
									}
									else
										DestroySubGraph( newInstance );
								}
								else
								{
									newInstance->matchCost = matchCost; 
									for( index2 = 0; index2 < newInstance->numberOfVertices;
									index2++ )
										if ( matchingPattern[index2] == 0 )
											newInstance->matchingOrder[index2] = NODE_DELETED;
										else 
											newInstance->matchingOrder[index2] =
											matchingPattern[index2] - 1;

										InsertInSubGraphsList( newSub->instances, newInstance );
										instanceIncluded[currentVEInstance->parentInstance->
											numberInSub] = TRUE;
										newSub->totalInstancesSize += SizeOf( newInstance );
								}
							}
							else
								DestroySubGraph( newInstance );
							Free( matchingPattern );
						}
						currentVEInstance = currentVEInstance->next;
					}
				}	
				if ( consistentVETemplate == NULL )
				{
					currentVETemplate = parentVETemplate->nextTemplate;
					parentVETemplate = currentVETemplate;
					if ( currentVETemplate != NULL ) 
						consistentVETemplate  = currentVETemplate->consistentList;
				}
				else 
				{
					currentVETemplate = consistentVETemplate->Template;
					consistentVETemplate = consistentVETemplate->next;
				}	
			}
		}
		
		if( GV->expandTemplates[index].ETemplates != NULL ) 
		{
			currentETemplate = GV->expandTemplates[index].ETemplates;
			parentETemplate = currentETemplate;
			consistentETemplate = currentETemplate->consistentList;
			while ( currentETemplate != NULL )
			{
				if ( currentETemplate != Template )
				{
					currentEInstance = currentETemplate->listOfInstances;
					while ( currentEInstance != NULL )
					{
						newInstance = CreateEInstanceSubGraph( GV, 
										/*	currentETemplate,*/
											currentEInstance, 
											NULL,
											NULL );
						if ( !MemberSubGraph( newInstance, newSub->instances ) )
						{
							newInstanceReverse = CreateReverseSubGraph( GV, newInstance );
							matchingPattern = (LONG *) Malloc( sizeof( LONG ) *
								newInstance->numberOfVertices );		
							matchCost = InExactGraphMatch( GV, definition, defReverse,
								newInstance, newInstanceReverse, GV->matchCostMaxPercent, NULL,
								NULL, 0, matchingPattern );
							DestroyReverseSubGraph( newInstanceReverse, newInstance );
							if ( matchCost > 0 )
							{
								if ( !GV->allowInstanceOverlap )
								{
									if ( NoInstanceOverlap( newInstance, newSub->instances ) ) 
									{
										newInstance->matchCost = matchCost; 
										for ( index2 = 0; index2 < newInstance->numberOfVertices;
										index2++ )
											if ( matchingPattern[index2] == 0 )
												newInstance->matchingOrder[index2] = NODE_DELETED;
											else 
												newInstance->matchingOrder[index2] = 
												matchingPattern[index2] - 1;
											InsertInSubGraphsList( newSub->instances, newInstance );
											instanceIncluded[currentEInstance->parentInstance->
												numberInSub] = TRUE;
											newSub->totalInstancesSize += SizeOf( newInstance );
									}
									else
										DestroySubGraph( newInstance );
								}
								else
								{
									newInstance->matchCost = matchCost; 
									for( index2 = 0; index2 < newInstance->numberOfVertices;
									index2++ )
										if ( matchingPattern[index2] == 0 )
											newInstance->matchingOrder[index2] = NODE_DELETED;
										else 
											newInstance->matchingOrder[index2] = 
											matchingPattern[index2] - 1;

										InsertInSubGraphsList( newSub->instances, newInstance );
										instanceIncluded[currentEInstance->parentInstance->
											numberInSub] = TRUE;
										newSub->totalInstancesSize += SizeOf( newInstance );
								}
							}
							else
								DestroySubGraph( newInstance );
							Free( matchingPattern );
						}
						currentEInstance = currentEInstance->next;
					}
				}	
				if ( consistentETemplate == NULL )
				{
					currentETemplate = parentETemplate->nextTemplate;
					parentETemplate = currentETemplate;
					if ( currentETemplate != NULL ) 
						consistentETemplate  = currentETemplate->consistentList;
				}
				else 
				{
					currentETemplate = consistentETemplate->Template;
					consistentETemplate = consistentETemplate->next;
				}
			}
		}
  }
  DestroyReverseSubGraph( defReverse, definition );
  
#ifdef _TIMING_
#ifdef _UNIX_
  times( &tmsEnd );
  GV->addENoiseTm += ( tmsEnd.tms_utime - tmsStart.tms_utime ) / GV->clockTick;
#else // WIN32
	end = clock();
  GV->addENoiseTm += ( end - start ) / GV->clockTick;
#endif
#endif

  return;
}


/*******************************************************************************
FUNCTION NAME: CreateSubFromVETemplate
INPUTS:		PGRAPH_VARIABLES GV, 
			PVE_TEMPLATE Template, 
			BOOLEAN *instanceIncluded,
			ULONG *oldToNewMatch, 
			ULONG *newVertexIndex 
RETURNS:	PSUB
PURPOSE:	Build a sub from a VE template.
CALLED BY:	extemp.c: GetBestSubs()
			extemp.c: GetBestTargetSub()
*******************************************************************************/

PSUB CreateSubFromVETemplate( PGRAPH_VARIABLES GV, PVE_TEMPLATE Template, 
							  BOOLEAN *instanceIncluded,
							  ULONG *oldToNewMatch,
							  ULONG *newVertexIndex )
{
	PSUB_GRAPH newInstance;
	PSUB_GRAPH definition;
	PSUB newSub;
	PEXPANDED_INSTANCE_VE currentInstance;	
	LONG addedVertexIndex;
	LONG addedEdgeIndex1;
	LONG addedEdgeIndex2;
	ULONG numberOfVerticesInSub;
	ULONG *oldToNewMatchOrder;
	ULONG index;
	ULONG edgeLabelIndex;
	PCONSISTENT_VE_LIST_NODE currentConsistentTemplate;
	PVE_TEMPLATE currentTemplate;
	PVE_TEMPLATE consistentTemplate;
	LONG *consistentMatchingPattern;
	
#ifdef _DEBUG_TRACE_
	printf( "%s: CreateSubFromVETemplate()\n", __FILE__ );
#endif
	
	/* Create the sub's definition */
	currentInstance = Template->listOfInstances;
	definition = CreateVEInstanceSubGraph( GV, Template, currentInstance,
		&addedVertexIndex, &addedEdgeIndex1,
		&addedEdgeIndex2 );
	numberOfVerticesInSub = definition->numberOfVertices;
	oldToNewMatchOrder = (ULONG *) Malloc( sizeof( ULONG ) * 
					   ( numberOfVerticesInSub - 1 ) );
	
	for ( index = 0; index < numberOfVerticesInSub; index++ )	// for all the labels
		if ( definition->matchingOrder[index] != NEW_VERTEX )
			oldToNewMatchOrder[definition->matchingOrder[index]] = index;
		if ( oldToNewMatch != NULL )
		{
			memcpy( oldToNewMatch, oldToNewMatchOrder, sizeof( ULONG ) * 
				( numberOfVerticesInSub - 1 ) );
			*newVertexIndex = (ULONG) addedVertexIndex;
		}
		for ( index = 0; index < numberOfVerticesInSub; index++ )	
			definition->matchingOrder[index] = index;
		
		newSub = CreateSub( definition );
		InsertInSubGraphsList( newSub->instances, definition );
		newSub->totalInstancesSize += SizeOf( definition );
		newSub->newVertex = addedVertexIndex;
		newSub->newEdge1 = addedEdgeIndex1;
		newSub->newEdge2 = addedEdgeIndex2;
		
		/* check all the edge label expansions; this will pull in equivalent numeric
		labels and string labels in the same group */
		for ( edgeLabelIndex = 0; edgeLabelIndex < GV->numberOfLabels; 
        edgeLabelIndex++ )
		{
			if ( SameLabel( GV, edgeLabelIndex, Template->newEdgeLabelIndex ) )
			{
				currentTemplate = GV->expandTemplates[edgeLabelIndex].VETemplates;
				while ( currentTemplate != NULL )
				{
					/* if currentTemplate is equivalent to Template */
					if ( SameLabel( GV, currentTemplate->newVertexLabelIndex, 
									Template->newVertexLabelIndex ) &&
						currentTemplate->sourceVertexMatchIndex ==
							Template->sourceVertexMatchIndex &&
						currentTemplate->edgeDirection == Template->edgeDirection &&
						currentTemplate->sourceIsOldVertex == Template->sourceIsOldVertex )
					{
						currentInstance = currentTemplate->listOfInstances;
						while ( currentInstance != NULL )
						{
						/* make sure this isn't the instance that the definition was created from */
							if ( currentInstance != Template->listOfInstances )
							{
								newInstance = 
									CreateVEInstanceSubGraph( GV, Template, currentInstance,
									NULL, NULL, NULL );
								newInstance->matchCost = 
									currentInstance->parentInstance->matchCost;
								
								for ( index = 0; index < newInstance->numberOfVertices; index++ )
									if ( newInstance->matchingOrder[index] == NEW_VERTEX )
										newInstance->matchingOrder[index] = 
										(ULONG) addedVertexIndex;
									else if ( newInstance->matchingOrder[index] != NODE_DELETED )
										newInstance->matchingOrder[index] =
										oldToNewMatchOrder[newInstance->matchingOrder[index]];
									
									if ( !MemberSubGraph( newInstance, newSub->instances ) ) {
										if ( !GV->allowInstanceOverlap ) {
											if ( NoInstanceOverlap( newInstance, newSub->instances ) ) 
											{
												InsertInSubGraphsList( newSub->instances, newInstance );
												instanceIncluded[currentInstance->
													parentInstance->numberInSub] = TRUE;
												newSub->totalInstancesSize += SizeOf( newInstance );
											}
											else 
												DestroySubGraph( newInstance );
										}
										else 
										{
											InsertInSubGraphsList( newSub->instances, newInstance );
											instanceIncluded[currentInstance->
												parentInstance->numberInSub] = TRUE;
											newSub->totalInstancesSize += SizeOf( newInstance );
										}
									}
									else
										DestroySubGraph( newInstance );
							}
							currentInstance = currentInstance->next;
						}
						
						/* collect any consistent template instances */
						currentConsistentTemplate = currentTemplate->consistentList;
						while ( currentConsistentTemplate != NULL )
						{
							consistentTemplate = currentConsistentTemplate->Template;
							consistentMatchingPattern = 
								currentConsistentTemplate->matchingPattern;
							currentInstance = consistentTemplate->listOfInstances;
							while ( currentInstance != NULL )
							{
								newInstance = CreateVEInstanceSubGraph( GV, consistentTemplate,
									currentInstance, NULL, 
									NULL, NULL );
								newInstance->matchCost = 
									currentInstance->parentInstance->matchCost;
								for ( index = 0; index < newInstance->numberOfVertices; index++ )
									if ( newInstance->matchingOrder[index] == NEW_VERTEX )
										newInstance->matchingOrder[index] = 
										(ULONG) addedVertexIndex;
									else 
										if ( newInstance->matchingOrder[index] != NODE_DELETED )
											newInstance->matchingOrder[index] =
											oldToNewMatchOrder[consistentMatchingPattern[newInstance->
											matchingOrder[index]]];

									if ( !MemberSubGraph( newInstance, newSub->instances ) )
										if ( !GV->allowInstanceOverlap )
											if ( NoInstanceOverlap( newInstance, newSub->instances ) ) 
											{
												InsertInSubGraphsList( newSub->instances, newInstance );
												instanceIncluded[currentInstance->
													parentInstance->numberInSub] = TRUE;
												newSub->totalInstancesSize += SizeOf( newInstance );
											}
											else
												DestroySubGraph( newInstance );
										else 
										{
											InsertInSubGraphsList( newSub->instances, newInstance );
											instanceIncluded[currentInstance->
												parentInstance->numberInSub] = TRUE;
											newSub->totalInstancesSize += SizeOf( newInstance );
										}
									else
										DestroySubGraph( newInstance );
									currentInstance = currentInstance->next;
							}
							currentConsistentTemplate = currentConsistentTemplate->next;
						}
					}
					currentTemplate = currentTemplate->nextTemplate;
      }
    }
  }

  AddVENoise( GV, Template, newSub, definition, instanceIncluded );	
  Free( oldToNewMatchOrder );
  return newSub;
}


/*******************************************************************************
FUNCTION NAME: CreateSubFromETemplate
INPUTS:		PGRAPH_VARIABLES GV, 
			PE_TEMPLATE Template, 
			BOOLEAN *instanceIncluded, 
			ULONG *oldToNewMatch
RETURNS:	PSUB
PURPOSE: 
CALLED BY:	extemp.c: GetBestTargetSub()
*******************************************************************************/

PSUB CreateSubFromETemplate( PGRAPH_VARIABLES GV, PE_TEMPLATE Template, 
							 BOOLEAN *instanceIncluded,
							 ULONG *oldToNewMatch )
{
	PSUB_GRAPH newInstance;
	PSUB_GRAPH definition;
	PSUB newSub;
	PEXPANDED_INSTANCE_E currentInstance;	
	ULONG numberOfVerticesInSub;
	ULONG *oldToNewMatchOrder;
	ULONG index;
	ULONG edgeLabelIndex;
	PCONSISTENT_E_LIST_NODE currentConsistentTemplate;
	PE_TEMPLATE currentTemplate;
	PE_TEMPLATE consistentTemplate;
	LONG *consistentMatchingPattern;
	LONG addedEdgeIndex1;
	LONG addedEdgeIndex2;
	
#ifdef _DEBUG_TRACE_
	printf( "%s: CreateSubFromETemplate()\n", __FILE__ );
#endif
	
	currentInstance = Template->listOfInstances;
	definition = CreateEInstanceSubGraph( GV,/* Template,*/ currentInstance,
		&addedEdgeIndex1, &addedEdgeIndex2 );
	numberOfVerticesInSub = definition->numberOfVertices;
	oldToNewMatchOrder = (ULONG *) Malloc( sizeof( ULONG ) *
					   numberOfVerticesInSub );
	for ( index = 0; index < numberOfVerticesInSub; index++ )	
		oldToNewMatchOrder[currentInstance->parentInstance->matchingOrder[index]] =
		index;
	if ( oldToNewMatch != NULL )
		memcpy( oldToNewMatch, oldToNewMatchOrder, sizeof( ULONG ) *
		numberOfVerticesInSub );	
	for ( index = 0; index < numberOfVerticesInSub; index++ )	
		definition->matchingOrder[index] = index;
	
	newSub = CreateSub( definition );
	InsertInSubGraphsList( newSub->instances, definition );
	newSub->totalInstancesSize += SizeOf( definition );
	newSub->newVertex = -1;
	newSub->newEdge1 = addedEdgeIndex1;
	newSub->newEdge2 = addedEdgeIndex2;
	
	/* check all the edge label expansions; this will pull in equivalent numeric
	labels and string labels in the same group */
	for ( edgeLabelIndex = 0; edgeLabelIndex < GV->numberOfLabels; 
	edgeLabelIndex++ )
	{
		if ( SameLabel( GV, edgeLabelIndex, Template->newEdgeLabelIndex ) )
		{
			currentTemplate = GV->expandTemplates[edgeLabelIndex].ETemplates;
			while ( currentTemplate != NULL )
			{
				/* if currentTemplate is equivalent to Template */
				if ( ( currentTemplate->sourceVertexMatchIndex == 
					Template->sourceVertexMatchIndex &&
					currentTemplate->targetVertexMatchIndex == 
					Template->targetVertexMatchIndex &&
					currentTemplate->edgeDirection == Template->edgeDirection ) ||
					( currentTemplate->sourceVertexMatchIndex == 
					Template->targetVertexMatchIndex &&
					currentTemplate->targetVertexMatchIndex == 
					Template->sourceVertexMatchIndex &&
					currentTemplate->edgeDirection == Template->edgeDirection &&
					Template->edgeDirection == UNDIRECTED ) )
				{
					currentInstance = currentTemplate->listOfInstances;
					while ( currentInstance != NULL )
					{
					/* make sure this isn't the instance that the definition was created
						from */
						if ( currentInstance != Template->listOfInstances )
						{
							newInstance = CreateEInstanceSubGraph( GV, /*Template, */
								currentInstance, NULL, NULL );
							for ( index = 0; index < newInstance->numberOfVertices; index++ )
								if ( newInstance->matchingOrder[index] != NODE_DELETED )
									newInstance->matchingOrder[index] =
									oldToNewMatchOrder[newInstance->matchingOrder[index]];

								if ( !MemberSubGraph( newInstance, newSub->instances ) ) {
									if ( !GV->allowInstanceOverlap ) {
										if ( NoInstanceOverlap( newInstance, newSub->instances ) ) 
										{
											InsertInSubGraphsList( newSub->instances, newInstance );
											instanceIncluded[currentInstance->
												parentInstance->numberInSub] = TRUE;
											newSub->totalInstancesSize += SizeOf( newInstance );
										}
										else
											DestroySubGraph( newInstance );
									}
									else 
									{
										InsertInSubGraphsList( newSub->instances, newInstance );
										instanceIncluded[currentInstance->
											parentInstance->numberInSub] = TRUE;
										newSub->totalInstancesSize += SizeOf( newInstance );
									}
								}
								else 
									DestroySubGraph( newInstance );
						}
						currentInstance = currentInstance->next;
					}
					
					/* collect any consistent template instances */
					currentConsistentTemplate = currentTemplate->consistentList;
					while( currentConsistentTemplate != NULL )
					{
						consistentTemplate = currentConsistentTemplate->Template;
						consistentMatchingPattern = 
							currentConsistentTemplate->matchingPattern;
						currentInstance = consistentTemplate->listOfInstances;
						while ( currentInstance != NULL )
						{
							newInstance = 
								CreateEInstanceSubGraph( GV, /*Template,*/ currentInstance,
								NULL, NULL );
							for ( index = 0; index < newInstance->numberOfVertices; index++ )
								if ( newInstance->matchingOrder[index] != NODE_DELETED )
									newInstance->matchingOrder[index] =
									oldToNewMatchOrder[consistentMatchingPattern[newInstance->
									matchingOrder[index]]];

								if( !MemberSubGraph( newInstance, newSub->instances ) ) {
									if( !GV->allowInstanceOverlap ) {
										if ( NoInstanceOverlap( newInstance, newSub->instances ) ) 
										{ 
											InsertInSubGraphsList( newSub->instances, newInstance );
											instanceIncluded[currentInstance->
												parentInstance->numberInSub] = TRUE;
											newSub->totalInstancesSize += SizeOf( newInstance );
										}
										else 
											DestroySubGraph( newInstance );
									}
									else 
									{
										InsertInSubGraphsList( newSub->instances, newInstance );
										instanceIncluded[currentInstance->
											parentInstance->numberInSub] = TRUE;
										newSub->totalInstancesSize += SizeOf( newInstance );
									}
								}
								else
									DestroySubGraph( newInstance );
								currentInstance = currentInstance->next;
						}
						currentConsistentTemplate = currentConsistentTemplate->next;
					}
				}
				currentTemplate = currentTemplate->nextTemplate;
			}
		}
  }
  AddENoise( GV, Template, newSub, definition, instanceIncluded );	
  Free( oldToNewMatchOrder );	
  return newSub;
}


/*******************************************************************************
FUNCTION NAME: FastCompare
INPUTS:		PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph,
			ULONG *first,
			ULONG *second,
			ULONG count
RETURNS:	BOOLEAN 
PURPOSE:	Check whether one or two pairs (as indicated by the "count" parameter)
			of a subgraph can start a subgraph isomorphism pattern. The check is
			based on the vertices labels and the number of edges.
CALLED BY:	extemp.c: GetStrongClasses()
*******************************************************************************/

BOOLEAN FastCompare( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph, ULONG *first, 
				 ULONG *second, ULONG count )
{
	ULONG index;
	
#ifdef DEBUG_TRACE
	printf( "%s: FastCompare()\n", __FILE__ );
#endif
	
	for ( index = 0; index < count; index++ )
		if ( first[index] == NODE_DELETED || second[index] == NODE_DELETED )
		{
			if ( first[index] != second[index] )
				return FALSE;
		}
		else 
			if ( !SameLabel( GV, 
				GV->graph->vertices[subGraph->vertices[first[index]].indexInGraph].labelIndex,
				GV->graph->vertices[subGraph->vertices[second[index]].indexInGraph].labelIndex ) ||
				( subGraph->vertices[first[index]].numberOfEdges !=
				  subGraph->vertices[second[index]].numberOfEdges ) )
			return FALSE;   
		return TRUE;
}


/*******************************************************************************
FUNCTION NAME: InsertInVEConsistentList
INPUTS:		PVE_TEMPLATE currentTemplate, 
			PVE_TEMPLATE consistentTemplate,
			LONG *matchingPattern, 
			ULONG length
RETURNS:	none
PURPOSE:	Insert consistentTemplate in currentTemplate's consistent list.
CALLED BY:	extemp.c: GetStrongClasses()
*******************************************************************************/

void InsertInVEConsistentList( PVE_TEMPLATE currentTemplate, 
							  PVE_TEMPLATE consistentTemplate,
							  ULONG *matchingPattern, ULONG length )
{
	PCONSISTENT_VE_LIST_NODE newConsistentTemplate;
	
#ifdef DEBUG_TRACE
	printf( "%s: InsertInVEConsistentList()\n", __FILE__ );
#endif
	
	newConsistentTemplate = (PCONSISTENT_VE_LIST_NODE) 
		Malloc( sizeof( CONSISTENT_VE_LIST_NODE ) );
	newConsistentTemplate->matchingPattern = (LONG *)
		Malloc( sizeof( LONG ) * length );
	memcpy( newConsistentTemplate->matchingPattern, matchingPattern,
		sizeof( LONG ) * length );
	newConsistentTemplate->Template = consistentTemplate;
	newConsistentTemplate->next = currentTemplate->consistentList;
	currentTemplate->consistentList = newConsistentTemplate;
	currentTemplate->numberOfInstances += consistentTemplate->numberOfInstances;
	return;
}


/*******************************************************************************
FUNCTION NAME: InsertInEConsistentList
INPUTS:		PE_TEMPLATE currentTemplate,
			PE_TEMPLATE consistentTemplate,
			LONG *matchingPattern,
			ULONG length
RETURNS:	none
PURPOSE:	Insert consistentTemplate in currentTemplate's consistent list.
CALLED BY:	extemp.c: GetStrongClasses()
*******************************************************************************/

void InsertInEConsistentList( PE_TEMPLATE currentTemplate,
							 PE_TEMPLATE consistentTemplate,
							 ULONG *matchingPattern,
							 ULONG length )
{
	PCONSISTENT_E_LIST_NODE newConsistentTemplate;
	
#ifdef DEBUG_TRACE
	printf( "%s: InsertInEConsistentList()\n", __FILE__ );
#endif
	
	newConsistentTemplate = (PCONSISTENT_E_LIST_NODE)
		Malloc( sizeof( CONSISTENT_E_LIST_NODE ) );
	newConsistentTemplate->matchingPattern = (LONG *)
		Malloc( sizeof( LONG ) * length );
	memcpy( newConsistentTemplate->matchingPattern, matchingPattern, 
		sizeof( LONG ) * length );
	newConsistentTemplate->Template = consistentTemplate;
	newConsistentTemplate->next = currentTemplate->consistentList;
	currentTemplate->consistentList = newConsistentTemplate;
	currentTemplate->numberOfInstances += consistentTemplate->numberOfInstances;
	return;
}


/*******************************************************************************
FUNCTION NAME: GetStrongClasses
INPUTS:		PGRAPH_VARIABLES GV, PSUB_GRAPH subDefinition 
RETURNS:	none
PURPOSE:	Gehad Galal: As vertices in a subgraph can be isomorphic, the 
			templates resulting from expanding the instances can be isomorphic. 
			GetStrongClasses() detects these templates and adds them to the 
			consistent list of their matched template. The set of vertices pairs
			that are determined to be isomorphic or not isomorphic are stored 
			to minimize the number of graph	matches used. These tables can get 
			big (as big as quadratic in the number of vertices in the expanded
			substructure definition).  A limit can be used to limit the size of
			these tables and to use graph matches when the size of the table 
			reaches the limit.
CALLED BY:	extemp.c: GetBestSubs()
*******************************************************************************/

void GetStrongClasses( PGRAPH_VARIABLES GV, PSUB_GRAPH subDefinition )
{
	ULONG index;
	ULONG index2;
	PVE_TEMPLATE currentVETemplate;
	PVE_TEMPLATE nextVETemplate;
	PVE_TEMPLATE preVETemplate;
	PE_TEMPLATE currentETemplate;
	PE_TEMPLATE nextETemplate;
	PE_TEMPLATE preETemplate;
	ULONG **correctMatches = NULL;
	ULONG numberOfCorrectMatches = 0 ;
	PUNMATCHED_PAIR inCorrectMatches = NULL;
	ULONG numberOfInCorrectMatches = 0 ;
	PUNMATCHED_PAIR *doubleInCorrectMatches = NULL;
	ULONG numberOfDoubleInCorrectMatches = 0;
	LONG *matchingPattern;
	PSUB_GRAPH copyOfDef;
	PREVERSE_SUB_GRAPH reverseOfDef;
	PREVERSE_SUB_GRAPH reverseOfCopy;
	ULONG first[2], second[2];		/* these hold the sourceVertexMatchIndex (0) */
									/* and targetVertexMatchIndex (1) for current */
									/* template (first) and next template (second) */
	BOOLEAN done;
	BOOLEAN consistencyDetected;
#ifdef _TIMING_
#ifdef _UNIX_
	struct tms tmsStart, tmsEnd;
#else // WIN32
	clock_t start, end;
#endif
#endif
	
#ifdef DEBUG_TRACE
	printf( "%s: GetStrongClasses()\n", __FILE__ );
#endif
	
	/* 
	Galal:	Maybe it would be better to exclude the allNoisy templates from 
			consideration as consistent templates.
    Maglothin: It looks to me like they are excluded. Maybe the above comment 
			pertained to a previous version. 
	Jonyer: I agree. 
	*/
	
#ifdef _TIMING_
#ifdef _UNIX_
	times( &tmsStart );
#else // WIN32
	start = clock();
#endif
#endif
	
	matchingPattern = (LONG *)
		Malloc( sizeof( LONG ) * subDefinition->numberOfVertices );
	ASSERT( matchingPattern != NULL );
	copyOfDef = CopySubGraph( subDefinition, 0.0 );
	reverseOfDef = CreateReverseSubGraph( GV, subDefinition );
	reverseOfCopy = CreateReverseSubGraph( GV, copyOfDef );
	
	for( index = 0; index < GV->numberOfLabels; index++ )
	{
		if ( GV->expandTemplates[index].VETemplates != NULL ) 
		{
			currentVETemplate = GV->expandTemplates[index].VETemplates;
			while ( currentVETemplate != NULL )
			{                 /* check all the templates in the list for this label */
				if ( !currentVETemplate->allNoisy )
				{                                          /* skip allNoisy templates */
					nextVETemplate = currentVETemplate->nextTemplate;
					preVETemplate = currentVETemplate;
					while ( nextVETemplate != NULL )
					{    /* compare currentVETemplate to all templates after it in list */
						consistencyDetected = FALSE;
						if ( !nextVETemplate->allNoisy )
						{
							if ( ( currentVETemplate->newVertexLabelIndex == 
								nextVETemplate->newVertexLabelIndex ) &&
								( currentVETemplate->edgeDirection == 
								nextVETemplate->edgeDirection ) &&
								( currentVETemplate->sourceIsOldVertex == 
								nextVETemplate->sourceIsOldVertex ) )
							{          /* if the templates' new vertex-edges are consistent */
								done = FALSE;
								for ( index2 = 0; index2 < numberOfCorrectMatches; index2++ )
									/* if this expansion matches a previously-matched expansion */
									if ( ( correctMatches[index2][nextVETemplate->
										sourceVertexMatchIndex] ==
										currentVETemplate->sourceVertexMatchIndex ) ||
										( correctMatches[index2][currentVETemplate->
										sourceVertexMatchIndex] ==
										nextVETemplate->sourceVertexMatchIndex ) )
									{
										done = TRUE;
										consistencyDetected = TRUE;
										/*printf( "GetStrongClasses(): Consistency detected(1).\n" );
										fflush( stdout );*/
										InsertInVEConsistentList( currentVETemplate,
											nextVETemplate,
											correctMatches[index2],
											subDefinition->numberOfVertices );
										break;
									}
									
									if( !done )
									{
										for ( index2 = 0; index2 < numberOfInCorrectMatches; 
										index2++ )
										/* if this expansion matches an already-detected incorrect
										match */
										if ( ( ( inCorrectMatches[index2].first == 
											currentVETemplate->sourceVertexMatchIndex ) &&
											( inCorrectMatches[index2].second == 
											nextVETemplate->sourceVertexMatchIndex ) ) ||
											( ( inCorrectMatches[index2].first == 
											nextVETemplate->sourceVertexMatchIndex ) &&
											( inCorrectMatches[index2].second == 
											currentVETemplate->sourceVertexMatchIndex ) ) )
										{
											consistencyDetected = FALSE;
											done = TRUE;
											break;
										}
									}
									if ( !done )
									{
									/* we still don't know whether this expansion is consistent
										with the definition or not; try a fuzzy match */
										first[0] = currentVETemplate->sourceVertexMatchIndex;
										second[0] = nextVETemplate->sourceVertexMatchIndex;
										if ( FastCompare( GV, subDefinition, first, second, 1 ) )
										{
											if ( InExactGraphMatch( GV, subDefinition, reverseOfDef, 
												copyOfDef, reverseOfCopy, 0.0, first, second, 1,
												matchingPattern ) >= 0.0 )
											{
											/* we've found a new match; expand the correctMatches
												array and record this match */
												correctMatches = (ULONG **) 
													Realloc( correctMatches, sizeof( ULONG * ) *
													( numberOfCorrectMatches + 1 ) );
												ASSERT( correctMatches != NULL );
												correctMatches[numberOfCorrectMatches] = 
													(ULONG*) Malloc( subDefinition->numberOfVertices * 
													sizeof( ULONG ) );
												ASSERT( correctMatches[numberOfCorrectMatches] != NULL );
												for( index2 = 0; index2 < subDefinition->numberOfVertices;
												index2++ )
													correctMatches[numberOfCorrectMatches][index2] =
													matchingPattern[index2] - 1; 
												numberOfCorrectMatches++;
												consistencyDetected = TRUE;
												/*printf( "GetStrongClasses(): Consistency detected(2).\n");
												fflush( stdout );*/
												InsertInVEConsistentList( currentVETemplate,
													nextVETemplate,
													correctMatches[numberOfCorrectMatches - 1],
													subDefinition->numberOfVertices );		
											}
											else
											{
											/* we've found a new mismatch; expand the inCorrectMatches
												array and record the mismatch */
												consistencyDetected = FALSE;
												inCorrectMatches = (PUNMATCHED_PAIR) 
													Realloc( inCorrectMatches, sizeof( UNMATCHED_PAIR ) *
													( numberOfInCorrectMatches + 1 ) );

												ASSERT( inCorrectMatches != NULL );
												inCorrectMatches[numberOfInCorrectMatches].first = 
													currentVETemplate->sourceVertexMatchIndex;
												inCorrectMatches[numberOfInCorrectMatches].second = 
													nextVETemplate->sourceVertexMatchIndex;
												numberOfInCorrectMatches++;	
											}
										}
									}
		  }
		}                                          /* end of if not noisy */
		if ( consistencyDetected )
		/* remove consistent template from this list; it's now in the
		consistent list */
		preVETemplate->nextTemplate = nextVETemplate->nextTemplate;
		else
			preVETemplate = nextVETemplate;
		nextVETemplate = nextVETemplate->nextTemplate;
	  }
	}
	currentVETemplate = currentVETemplate->nextTemplate;
      }
    }
	
    if ( GV->expandTemplates[index].ETemplates != NULL ) 
    {
		currentETemplate = GV->expandTemplates[index].ETemplates;
		while ( currentETemplate != NULL )
		{
			if ( !currentETemplate->allNoisy )
			{
				nextETemplate = currentETemplate->nextTemplate;
				preETemplate = currentETemplate;
				while ( nextETemplate != NULL )
				{
					consistencyDetected = FALSE;
					if ( !nextETemplate->allNoisy )
					{
						done = FALSE;
						for ( index2 = 0; index2 < numberOfCorrectMatches; index2++ )
							if ( ( ( ( correctMatches[index2][nextETemplate->
								sourceVertexMatchIndex] ==
								currentETemplate->sourceVertexMatchIndex ) || 
								( correctMatches[index2][currentETemplate->
								sourceVertexMatchIndex] ==
								nextETemplate->sourceVertexMatchIndex ) ) &&
								( ( correctMatches[index2][nextETemplate->
								targetVertexMatchIndex] ==
								currentETemplate->targetVertexMatchIndex) ||
								( correctMatches[index2][currentETemplate->
								targetVertexMatchIndex] ==
								nextETemplate->targetVertexMatchIndex ) ) ) ||
								( ( currentETemplate->edgeDirection == UNDIRECTED ) &&
								( ( correctMatches[index2][nextETemplate->
								sourceVertexMatchIndex] ==
								currentETemplate->targetVertexMatchIndex ) ||
								( correctMatches[index2][currentETemplate->
								sourceVertexMatchIndex] ==
								nextETemplate->targetVertexMatchIndex ) ) &&
								( ( correctMatches[index2][nextETemplate->
								targetVertexMatchIndex] ==
								currentETemplate->sourceVertexMatchIndex ) || 
								( correctMatches[index2][currentETemplate->
								targetVertexMatchIndex] ==
								nextETemplate->sourceVertexMatchIndex ) ) ) ) 
							{
								done = TRUE;
								consistencyDetected = TRUE;
								/*printf( "GetStrongClasses(): Consistency detected(3).\n" );
								fflush( stdout );*/
								InsertInEConsistentList( currentETemplate, nextETemplate,
									correctMatches[index2],
									subDefinition->numberOfVertices );	
								break;
							}
							if( !done )
							{
								for ( index2 = 0; index2 < numberOfInCorrectMatches; index2++ )
									if ( ( ( ( inCorrectMatches[index2].first == 
										currentETemplate->sourceVertexMatchIndex )&&
										( inCorrectMatches[index2].second == 
										nextETemplate->sourceVertexMatchIndex ) ) ||
										( ( inCorrectMatches[index2].first ==
										nextETemplate->sourceVertexMatchIndex ) &&
										( inCorrectMatches[index2].second == 
										currentETemplate->sourceVertexMatchIndex ) ) ||
										( ( inCorrectMatches[index2].first == 
										currentETemplate->targetVertexMatchIndex ) &&
										( inCorrectMatches[index2].second ==
										nextETemplate->targetVertexMatchIndex ) ) ||
										( ( inCorrectMatches[index2].first ==
										nextETemplate->targetVertexMatchIndex ) &&
										( inCorrectMatches[index2].second == 
										currentETemplate->targetVertexMatchIndex ) ) ) &&
										( ( currentETemplate->edgeDirection == UNDIRECTED ) &&
										( ( ( inCorrectMatches[index2].first ==
										currentETemplate->sourceVertexMatchIndex )&&
										( inCorrectMatches[index2].second == 
										nextETemplate->targetVertexMatchIndex ) ) ||
										( ( inCorrectMatches[index2].first ==
										nextETemplate->targetVertexMatchIndex ) &&
										( inCorrectMatches[index2].second ==
										currentETemplate->sourceVertexMatchIndex ) ) ||
										( ( inCorrectMatches[index2].first == 
										currentETemplate->targetVertexMatchIndex ) &&
										( inCorrectMatches[index2].second == 
										nextETemplate->sourceVertexMatchIndex ) ) ||
										( ( inCorrectMatches[index2].first == 
										nextETemplate->sourceVertexMatchIndex) &&
										( inCorrectMatches[index2].second == 
										currentETemplate->targetVertexMatchIndex )
										) ) ) )
									{
										done = TRUE;
										consistencyDetected = FALSE;
										break;
									}
							}
							if( !done )
							{		
								first[0] = currentETemplate->sourceVertexMatchIndex;
								second[0] = nextETemplate->sourceVertexMatchIndex;
								first[1] = currentETemplate->targetVertexMatchIndex;
								second[1] = nextETemplate->targetVertexMatchIndex;
								for ( index2 = 0; index2 < numberOfDoubleInCorrectMatches;
								index2++ )
									if ( ( ( first[0] == doubleInCorrectMatches[index2][0].first )
										&& ( second[0] == doubleInCorrectMatches[index2][0].second )
										&& ( first[1] == doubleInCorrectMatches[index2][1].first )
										&& ( second[1] == doubleInCorrectMatches[index2][1].second )
										) ||
										( ( first[0] == doubleInCorrectMatches[index2][0].second )
										&& ( second[0] == doubleInCorrectMatches[index2][0].first )
										&& ( first[1] == doubleInCorrectMatches[index2][1].second )
										&& ( second[1] == doubleInCorrectMatches[index2][1].first )
										) ||
										( ( first[0] == doubleInCorrectMatches[index2][1].second )
										&& ( second[0] == doubleInCorrectMatches[index2][1].first )
										&& ( first[1] == doubleInCorrectMatches[index2][0].second )
										&& ( second[1] == doubleInCorrectMatches[index2][0].first ) 
										) ||
										( ( first[1] == doubleInCorrectMatches[index2][0].second )
										&& ( second[1] == doubleInCorrectMatches[index2][0].first )
										&& ( first[0] == doubleInCorrectMatches[index2][1].second )
										&& ( second[0] == doubleInCorrectMatches[index2][1].first )
										) )
									{
										done = TRUE;
										consistencyDetected = FALSE;
										break;
									}
							}
							
							if( !done )
							{
								if ( FastCompare( GV, subDefinition, first, second, 2 ) )
								{
									if ( InExactGraphMatch( GV, subDefinition, reverseOfDef, 
										copyOfDef, reverseOfCopy, 0.0, first, second, 2, 
										matchingPattern ) >= 0.0 )
									{
										done = TRUE;
										consistencyDetected = TRUE;
										/*printf( "GetStrongClasses(): Consistency detected(4).\n" );
										fflush( stdout );*/
										
										correctMatches = (ULONG **) 
											Realloc( correctMatches, sizeof( ULONG *) * 
											( numberOfCorrectMatches + 1 ) );
										ASSERT( correctMatches != NULL );
										correctMatches[numberOfCorrectMatches] = 
											(ULONG*) Malloc( subDefinition->numberOfVertices *
											sizeof( ULONG ) );
										ASSERT( correctMatches[numberOfCorrectMatches] != NULL );
										for( index2 = 0; index2 < subDefinition->numberOfVertices;
										index2++ )
											correctMatches[numberOfCorrectMatches][index2] =
											matchingPattern[index2] - 1; 
										numberOfCorrectMatches++; 
										InsertInEConsistentList( currentETemplate, nextETemplate,
											correctMatches[numberOfCorrectMatches - 1],
											subDefinition->numberOfVertices );	
									}
									else
									{  
										consistencyDetected = FALSE;
										doubleInCorrectMatches = (PUNMATCHED_PAIR *) 
											Realloc( doubleInCorrectMatches,
											sizeof( PUNMATCHED_PAIR ) *
											( numberOfDoubleInCorrectMatches + 1 ) );
										ASSERT( doubleInCorrectMatches != NULL );
										doubleInCorrectMatches[numberOfDoubleInCorrectMatches] =
											(PUNMATCHED_PAIR)Malloc( sizeof( UNMATCHED_PAIR ) * 2 );
										ASSERT( doubleInCorrectMatches
											[numberOfDoubleInCorrectMatches] != NULL );
										doubleInCorrectMatches[numberOfDoubleInCorrectMatches]
											[0].first = first[0];
										doubleInCorrectMatches[numberOfDoubleInCorrectMatches]
											[0].second = second[0];
										doubleInCorrectMatches[numberOfDoubleInCorrectMatches]
											[1].first = first[1];
										doubleInCorrectMatches[numberOfDoubleInCorrectMatches]
											[1].second = second[1];
										numberOfDoubleInCorrectMatches++;	
									}
								}
							}
							if ( !done && ( currentETemplate->edgeDirection == UNDIRECTED ) )
							{
								first[0] = currentETemplate->sourceVertexMatchIndex;
								second[0] = nextETemplate->targetVertexMatchIndex;
								first[1] = currentETemplate->targetVertexMatchIndex;
								second[1] = nextETemplate->sourceVertexMatchIndex;
								for ( index2 = 0; index2 < numberOfDoubleInCorrectMatches;
								index2++ )
									if ( ( ( first[0] == doubleInCorrectMatches[index2][0].first )
										&& ( second[0] == doubleInCorrectMatches[index2][0].second )
										&& ( first[1] == doubleInCorrectMatches[index2][1].first ) 
										&& ( second[1] == doubleInCorrectMatches[index2][1].second )
										) ||
										( ( first[0] == doubleInCorrectMatches[index2][0].second )
										&& ( second[0] == doubleInCorrectMatches[index2][0].first )
										&& ( first[1] == doubleInCorrectMatches[index2][1].second )
										&& ( second[1] == doubleInCorrectMatches[index2][1].first )
										) ||
										( ( first[0] == doubleInCorrectMatches[index2][1].second )
										&& ( second[0] == doubleInCorrectMatches[index2][1].first )
										&& ( first[1] == doubleInCorrectMatches[index2][0].second )
										&& ( second[1] == doubleInCorrectMatches[index2][0].first )
										) ||
										( ( first[1] == doubleInCorrectMatches[index2][0].second )
										&& ( second[1] == doubleInCorrectMatches[index2][0].first )
										&& ( first[0] == doubleInCorrectMatches[index2][1].second )
										&& ( second[0] == doubleInCorrectMatches[index2][1].first )
										) )
									{
										done = TRUE;
										consistencyDetected = FALSE;
										break;
									}
									if ( !done )
										if ( FastCompare( GV, subDefinition, first, second, 2 ) )
										{
											if ( InExactGraphMatch( GV, subDefinition, reverseOfDef, 
												copyOfDef, reverseOfCopy, 0.0, first, second, 2, 
												matchingPattern ) >= 0.0 ) 
											{
												done = TRUE;
												consistencyDetected = TRUE;
												/*printf( 
												"GetStrongClasses(): Consistency detected(5).\n" );
												fflush( stdout ); */
												
												correctMatches = (ULONG**)
													Realloc( correctMatches, sizeof(ULONG *) * 
													( numberOfCorrectMatches + 1 ) );
												ASSERT( correctMatches != NULL );
												correctMatches[numberOfCorrectMatches] = (ULONG*) 
													Malloc( subDefinition->numberOfVertices *
													sizeof( ULONG ) );
												ASSERT( correctMatches[numberOfCorrectMatches] != NULL );
												for ( index2 = 0; 
												index2 < subDefinition->numberOfVertices; index2++ )
													correctMatches[numberOfCorrectMatches][index2] =
													matchingPattern[index2] - 1; 
												numberOfCorrectMatches++; 
												InsertInEConsistentList( currentETemplate, nextETemplate,
													correctMatches[numberOfCorrectMatches - 1],             
													subDefinition->numberOfVertices );	
											}
											else
											{	
												consistencyDetected = FALSE;
												doubleInCorrectMatches = (PUNMATCHED_PAIR *) 
													Realloc( doubleInCorrectMatches,
													sizeof( PUNMATCHED_PAIR ) *
													( numberOfDoubleInCorrectMatches + 1 ) );
												ASSERT( doubleInCorrectMatches != NULL );
												doubleInCorrectMatches[numberOfDoubleInCorrectMatches]
													= (PUNMATCHED_PAIR) 
													Malloc( sizeof( UNMATCHED_PAIR ) * 2 );
												doubleInCorrectMatches[numberOfDoubleInCorrectMatches]    
													[0].first = first[0];
												doubleInCorrectMatches[numberOfDoubleInCorrectMatches]
													[0].second = second[0];
												doubleInCorrectMatches[numberOfDoubleInCorrectMatches]
													[1].first = first[1];
												doubleInCorrectMatches[numberOfDoubleInCorrectMatches]    
													[1].second = second[1];
												numberOfDoubleInCorrectMatches++;	
											}
										}
							}
		}
		if ( consistencyDetected )
			preETemplate->nextTemplate = nextETemplate->nextTemplate;
		else 
			preETemplate = nextETemplate;
		nextETemplate = nextETemplate->nextTemplate;
	  }
	}
	currentETemplate = currentETemplate->nextTemplate;
      }
    }
  }
  DestroyReverseSubGraph( reverseOfCopy, copyOfDef );
  DestroyReverseSubGraph( reverseOfDef, subDefinition );	
  DestroySubGraph( copyOfDef );
  for ( index = 0; index < numberOfCorrectMatches; index++ )
	  Free( correctMatches[index] );
  for ( index = 0; index < numberOfDoubleInCorrectMatches; index++ )
	  Free( doubleInCorrectMatches[index] );
  if ( numberOfCorrectMatches ) 
	  Free( correctMatches );
  if ( numberOfInCorrectMatches ) 
	  Free( inCorrectMatches );
  if ( numberOfDoubleInCorrectMatches ) 
	  Free( doubleInCorrectMatches );
  Free( matchingPattern );
  
#ifdef _TIMING_
#ifdef _UNIX_
	times( &tmsEnd );
	GV->getStrongClassesTm += ( tmsEnd.tms_utime - tmsStart.tms_utime ) /
	  GV->clockTick;
#else // WIN32
	end = clock();
	GV->getStrongClassesTm += ( end - start ) / GV->clockTick;
#endif
#endif
  
  return;
}

#ifdef BEAM_LENGTH_BY_VALUE

/*******************************************************************************
FUNCTION NAME: GetBestTemplates
INPUTS:		PGRAPH_VARIABLES GV, 
			PVE_TEMPLATE *bestVETemplates, 
			PE_TEMPLATE *bestETemplates
RETURNS:	none
PURPOSE:	Gehad Galal: Get the best GV->sSBeamLength templates both resulting 
			from adding only an edge "bestETemplates" or an edge and a vertex 
			"bestVETemplates". The comparison between templates is based solely
			on the number of instances in each template. A couple of 
			improvements can be done at this point.

	1- if a single vertex can be expanded in the same way twice which can result 
	in a deceiving number of instances in the resulting template. So the 
	chosen templates might not be the best. To overcome this, we can prevent
	any old instances to contribute more than 1 to the length of any template
	(we can not ignore similar expansions as they can affect the instances
	taken in case of not allowing overlaps).
  
	2- A very similar note holds when adding the consistent templates to any
	template as some instances in both can result from the same parent
	instance, thus resulting in a deceiving overall number of instances. This 
	can be handled when adding the length of a consistent template to another
	template by increasing the length by only the number of expansions from
	old instances that are not already included.
	
CALLED BY:	extemp.c: GetBestSubs()

  Notes: (IJ) This version orders the templates based on the number of instances,
		 by hooking them up in a linked list.
*****************************************************************************/
	  
void GetBestTemplates( PGRAPH_VARIABLES GV, 
					   PVE_TEMPLATE *_bestVETemplates, 
					   PE_TEMPLATE  *_bestETemplates )
{
	ULONG index;
	PVE_TEMPLATE bestVETemplates;
	PE_TEMPLATE  bestETemplates;
	PVE_TEMPLATE currentVETemplate;
	PE_TEMPLATE currentETemplate;
	PVE_TEMPLATE tempVETemplate;
	PE_TEMPLATE tempETemplate;
	PVE_TEMPLATE prevVETemplate;
	PE_TEMPLATE prevETemplate;
#ifdef _TIMING_
#ifdef _UNIX_
	struct tms tmsStart;
	struct tms tmsEnd;
#else // WIN32
	clock_t start, end;
#endif
#endif
		  
#ifdef DEBUG_TRACE
	printf( "%s: GetBestTemplates()\n", __FILE__ );
#endif
		  
#ifdef _TIMING_
#ifdef _UNIX_
	times( &tmsStart );
#else // WIN32
	start = clock();
#endif
#endif
	bestVETemplates = NULL;
	bestETemplates = NULL;

	for ( index = 0; index < GV->numberOfLabels; index++ )
	{
		// VETemplates
		if ( GV->expandTemplates[index].VETemplates != NULL ) {
			currentVETemplate = GV->expandTemplates[index].VETemplates;
			while ( currentVETemplate != NULL )							// for each VETemplate
			{
				if ( !currentVETemplate->allNoisy )						// if not allNoisy
				{
					if (bestVETemplates == NULL) {
						bestVETemplates = currentVETemplate;
					}
					else {
						tempVETemplate = bestVETemplates;
						prevVETemplate = NULL;
						while (tempVETemplate &&
							(currentVETemplate->numberOfInstances <			// current has more instances 
							 tempVETemplate->numberOfInstances) )
						{
							prevVETemplate = tempVETemplate;
							tempVETemplate = tempVETemplate->nextBestTemplate;
						}

						if (prevVETemplate)
							prevVETemplate->nextBestTemplate = currentVETemplate;
						else
							bestVETemplates = currentVETemplate;
						currentVETemplate->nextBestTemplate = tempVETemplate;
					}
				}
				currentVETemplate = currentVETemplate->nextTemplate;
			}
		}

		// ETemplates
		if ( GV->expandTemplates[index].ETemplates != NULL ) {
			currentETemplate = GV->expandTemplates[index].ETemplates;
			while ( currentETemplate != NULL )							// for each VETemplate
			{
				if ( !currentETemplate->allNoisy )						// if not allNoisy
				{
					if (bestETemplates == NULL) {
						bestETemplates = currentETemplate;
					}
					else {
						tempETemplate = bestETemplates;
						prevETemplate = NULL;
						while (tempETemplate &&
							(currentETemplate->numberOfInstances <			// current has more instances 
							 tempETemplate->numberOfInstances) )
						{
							prevETemplate = tempETemplate;
							tempETemplate = tempETemplate->nextBestTemplate;
						}

						if (prevETemplate)								// if there is a previous
							prevETemplate->nextBestTemplate = currentETemplate;	// hook it up
						else											// otherwise
							bestETemplates = currentETemplate;				// this is the first one
						currentETemplate->nextBestTemplate = tempETemplate;
					}
				}
				currentETemplate = currentETemplate->nextTemplate;
			}
		}
	}

	*_bestVETemplates = bestVETemplates;
	*_bestETemplates = bestETemplates;
	
#ifdef _TIMING_
#ifdef _UNIX_
	times( &tmsEnd );
	GV->getBestTemplatesTm += ( tmsEnd.tms_utime - tmsStart.tms_utime ) /
		GV->clockTick;
#else // WIN32
	end = clock();
	GV->getBestTemplatesTm += ( end - start ) / GV->clockTick;
#endif
#endif
	
}

#else // BEAM_LENGTH_BY_VALUE


/*******************************************************************************
FUNCTION NAME: GetBestTemplates
INPUTS:		PGRAPH_VARIABLES GV, 
			PVE_TEMPLATE *bestVETemplates, 
			PE_TEMPLATE *bestETemplates
RETURNS:	none
PURPOSE:	Gehad Galal: Get the best GV->sSBeamLength templates both resulting 
			from adding only an edge "bestETemplates" or an edge and a vertex 
			"bestVETemplates". The comparison between templates is based solely
			on the number of instances in each template. A couple of 
			improvements can be done at this point.

	1- if a single vertex can be expanded in the same way twice which can result 
	in a deceiving number of instances in the resulting template. So the 
	chosen templates might not be the best. To overcome this, we can prevent
	any old instances to contribute more than 1 to the length of any template
	(we can not ignore similar expansions as they can affect the instances
	taken in case of not allowing overlaps).
  
	2- A very similar note holds when adding the consistent templates to any
	template as some instances in both can result from the same parent
	instance, thus resulting in a deceiving overall number of instances. This 
	can be handled when adding the length of a consistent template to another
	template by increasing the length by only the number of expansions from
	old instances that are not already included.
	
CALLED BY:	extemp.c: GetBestSubs()
*******************************************************************************/
	  
void GetBestTemplates( PGRAPH_VARIABLES GV, PVE_TEMPLATE *bestVETemplates, 
					  PE_TEMPLATE *bestETemplates )
{
	ULONG index;
	LONG index2;
	LONG location;
	PVE_TEMPLATE currentVETemplate;
	PE_TEMPLATE currentETemplate;
#ifdef _TIMING_
#ifdef _UNIX_
	struct tms tmsStart;
	struct tms tmsEnd;
#else // WIN32
	clock_t start, end;
#endif
#endif
		  
#ifdef DEBUG_TRACE
	printf( "%s: GetBestTemplates()\n", __FILE__ );
#endif
		  
#ifdef _TIMING_
#ifdef _UNIX_
	times( &tmsStart );
#else // WIN32
	start = clock();
#endif
#endif
		  
	for ( index = 0; index < GV->numberOfLabels; index++ )
	{
		if ( GV->expandTemplates[index].VETemplates != NULL ) 
		{
			currentVETemplate = GV->expandTemplates[index].VETemplates;
			while ( currentVETemplate != NULL )								// for each VETemplate
			{
				if (  !currentVETemplate->allNoisy  &&					// if not allNoisy and
					  ( (bestVETemplates[0] == NULL) ||					// first VETemplate == NULL or
					   (currentVETemplate->numberOfInstances >			// current has more instances 
					   bestVETemplates[0]->numberOfInstances) ) )		// than first
				{
					// insert currentVETemplate in bestVETemplates in order
					for ( index2 = GV->sSBeamLength - 1; index2 >= 0; index2-- )
						if ( ( bestVETemplates[index2] == NULL ) ||
							( currentVETemplate->numberOfInstances >
							bestVETemplates[index2]->numberOfInstances ) )
							break;

					location = index2;
					for( index2 = 0; index2 < location; index2++ )		// scoot DOWN worse templates 
						bestVETemplates[index2] = bestVETemplates[index2 + 1];
					bestVETemplates[location] = currentVETemplate; 
				}
				currentVETemplate = currentVETemplate->nextTemplate;
			}
		}

		if ( GV->expandTemplates[index].ETemplates != NULL ) {
			currentETemplate = GV->expandTemplates[index].ETemplates;
			while ( currentETemplate != NULL )
			{
				if ( ( !currentETemplate->allNoisy ) &&
					( ( bestETemplates[0] == NULL ) ||
					( currentETemplate->numberOfInstances > 
					bestETemplates[0]->numberOfInstances ) ) )
				{
					// insert currentETemplate in bestVETemplates in order
					for ( index2 = GV->sSBeamLength - 1; index2 >= 0; index2-- )
						if ( ( bestETemplates[index2] == NULL ) ||
							( currentETemplate->numberOfInstances >
							bestETemplates[index2]->numberOfInstances ) )
							break;

					location = index2;
					for ( index2 = 0; index2 < location; index2++ )
						bestETemplates[index2] = bestETemplates[index2 + 1];
					bestETemplates[location] = currentETemplate; 
				}
				currentETemplate = currentETemplate->nextTemplate;
			}
		}
	}
	
#ifdef _TIMING_
#ifdef _UNIX_
	times( &tmsEnd );
	GV->getBestTemplatesTm += ( tmsEnd.tms_utime - tmsStart.tms_utime ) /
		GV->clockTick;
#else // WIN32
	end = clock();
	GV->getBestTemplatesTm += ( end - start ) / GV->clockTick;
#endif
#endif
	
	return;
}
#endif //not BEAM_LENGTH_BY_VALUE


/*******************************************************************************
FUNCTION NAME: GetBestSubs2
INPUTS:		PGRAPH_VARIABLES GV, 
			PVE_TEMPLATE *bestVETemplates, 
			PE_TEMPLATE *bestETemplates
			DOUBLE worstValueOnChildList
RETURNS:	none
PURPOSE:	Create new substructures from the templates and insert them in
			the bestSubsList.  Return only the GV->beamLength number of
			subs (value based).
CALLED BY:	extend.c: ExtendSub()
*****************************************************************************/
	  
void GetBestSubs2( PGRAPH_VARIABLES GV, PLIST_OF_SUBS bestSubsList, PSUB oldSub,
			   DOUBLE worstValueOnChildList )
{
	PLIST_OF_SUBS bestSubsListNM;
	PVE_TEMPLATE currentVETemplate;
	PE_TEMPLATE currentETemplate;
	ULONG index;
	PSUB newSub;
	BOOLEAN *instanceIncluded;
	BOOLEAN originalMinencodeFlag;
	DOUBLE previousValue;
	DOUBLE maxValue;			// max minencode value in latest nominencode evaluation
#ifdef _TIMING_
#ifdef _UNIX_
	struct tms tmsStart;
	struct tms tmsEnd;
#else // WIN32
	clock_t start, end;
#endif
#endif
		  
#ifdef DEBUG_TRACE
	printf( "%s: GetBestSubs2()\n", __FILE__ );
#endif
		  
#ifdef _TIMING_
#ifdef _UNIX_
	times( &tmsStart );
#else // WIN32
	start = clock();
#endif
#endif
		  
	bestSubsListNM = CreateSubsList( GV->beamLength );//subBeamLength );

	// IJ: What is the purpose of instanceIncluded?

	instanceIncluded = (BOOLEAN *) Calloc( sizeof( BOOLEAN ) ,	// this one needs to initialize to 0 -> Calloc
		oldSub->instances->currentLength );

	GetStrongClasses( GV, oldSub->definition );
	
	originalMinencodeFlag = GV->minEncode;
	GV->minEncode = FALSE;							// do template evaluation in noMinEncode

	// Evaluate all templates using nominencoding, and keep only subBeamLength
	// number of values (1 by default).
	for ( index = 0; index < GV->numberOfLabels; index++ )
	{
		// VETemplates
		if ( GV->expandTemplates[index].VETemplates != NULL ) {
			currentVETemplate = GV->expandTemplates[index].VETemplates;
			while ( currentVETemplate != NULL )						// for each VETemplate
			{
				if ( !currentVETemplate->allNoisy )					// if not allNoisy
				{
					newSub = CreateSubFromVETemplate( GV, currentVETemplate,
						instanceIncluded, NULL, NULL );				// make sub out of template
					EvaluateSub( GV, newSub, oldSub );				// evaluate it
					InsertInSubsListInOrder( GV, bestSubsListNM, newSub, FALSE );
				}
				currentVETemplate = currentVETemplate->nextTemplate;
			}
		}

		// ETemplates
		if ( GV->expandTemplates[index].ETemplates != NULL ) {
			currentETemplate = GV->expandTemplates[index].ETemplates;
			while ( currentETemplate != NULL )							// for each VETemplate
			{
				if ( !currentETemplate->allNoisy )						// if not allNoisy
				{
					newSub = CreateSubFromETemplate( GV, currentETemplate,
						instanceIncluded, NULL );				// make sub out of template
					EvaluateSub( GV, newSub, oldSub );				// evaluate it
					InsertInSubsListInOrder( GV, bestSubsListNM, newSub, FALSE );
				}
				currentETemplate = currentETemplate->nextTemplate;
			}
		}

		memset( instanceIncluded, 0, sizeof( BOOLEAN ) * oldSub->instances->currentLength );
	}

	GV->minEncode = originalMinencodeFlag;

	// now evaluate the best ones using minencode (if specified)
	previousValue = POSITIVE_INFINITY_DOUBLE;
	maxValue = 0;
	while ( (newSub = RemoveNextSub(bestSubsListNM)) != NULL )	// for all extended instances
	{
		if (newSub->value < previousValue) {	// count nominencode values processed
			previousValue = newSub->value;			// entering a new value
			if (maxValue != 0 && maxValue < worstValueOnChildList)	// if max is hopeless
				break;												// stop evaluating
			maxValue = 0;
		}

		if ( bestSubsList->numberOfValues <= GV->beamLength ) {
			if (GV->minEncode) {							// for minencoding
				EvaluateSub( GV, newSub, oldSub );			// reevaluate sub with minencoding
			}
			if (maxValue < newSub->value)					// update max value
				maxValue = newSub->value;
		}
		else {
//printf(" Beam Length of %d exceeded: %d\n", GV->beamLength, bestSubsList->numberOfValues);
			break;
		}
//	
//if (newSub->instances->currentLength == 1 && newSub->value > 1.0)
//{
//	PrintSub( GV, newSub, FALSE, FALSE );
//	printf("Value: %f\n", newSub->value);
//	printf("*");
//}


		if (newSub->value >= worstValueOnChildList) {
//{}
//printf(" -- %d newSub->value: %f\n", newSub->instances->currentLength, newSub->value);
			InsertInSubsListInOrder( GV, bestSubsList, newSub, FALSE );
		}
		else {
//
//printf(" -- %d newSub->value: %f   Insert Skipped\n", newSub->instances->currentLength, newSub->value);
			DestroySub( newSub );
		}
	}

	DestroySubsList( bestSubsListNM );			// destroy temporary list of subs
	DestroyExpTemp( GV );

	if (bestSubsList->currentLength > bestSubsList->numberOfValues)
											// if there are more subs on the Q than 
											// distinct values, then 1 value must
											// occur at least twice, and there is room
											// for purging
	{
		PurgeBestSubs(GV, bestSubsList, oldSub);
	
	}
//
//printf(" --------------------\n");
	

#ifdef _TIMING_
#ifdef _UNIX_
	times( &tmsEnd );
	GV->getBestSubsTm += ( tmsEnd.tms_utime - tmsStart.tms_utime ) / GV->clockTick;
#else // WIN32
	end = clock();
	GV->getBestSubsTm += ( end - start ) / GV->clockTick;
#endif
#endif
	
}

/*******************************************************************************
FUNCTION NAME: GetNewVertex
INPUTS:		PGRAPH_VARIABLES GV, 
			PSUB_GRAPH subDef, 
			PSUB_GRAPH oldSubDef,
			PSUB_GRAPH_VERTEX *newVertex,
OUTPUTS:	ULONG *centralVertexIndexInGraph,
			BOOLEAN *source
RETURNS:	BOOLEAN: TRUE if there is a new vertex
PURPOSE:	Find the new vertex and edge that has been added to the old sub to 
			create the new one.
CALLED BY:	extemp.c: PurgeBestSubs()
*******************************************************************************/
BOOLEAN GetNewVertex(PGRAPH_VARIABLES GV, 
					 PSUB_GRAPH subDef, 
					 PSUB_GRAPH oldSubDef,
					 PSUB_GRAPH_VERTEX *newVertex,
					 PGRAPH_EDGE *newEdge,
					 ULONG *centralVertexIndexInGraph,
					 BOOLEAN *source)
{

	ULONG index;
	ULONG vertexIndex;
	ULONG edgeIndex;
	PSUB_GRAPH_VERTEX subVertex;
	PSUB_GRAPH_VERTEX oldSubVertex;
	ULONG targetVertexIndexInGraph;
	PGRAPH_VERTEX graphVertex;

#ifdef DEBUG_TRACE
	printf( "%s: GetNewVertex()\n", __FILE__ );
#endif

	*centralVertexIndexInGraph = 0;
	if (oldSubDef->numberOfVertices == subDef->numberOfVertices - 1) // newSub must have 1 more vertex
	{
		graphVertex = GV->graph->vertices;
		*newVertex = &subDef->vertices[subDef->numberOfVertices - 1];
		for ( vertexIndex = 0; vertexIndex < oldSubDef->numberOfVertices; vertexIndex++ )
		{
			subVertex = &subDef->vertices[vertexIndex];
			oldSubVertex = &oldSubDef->vertices[vertexIndex];
			if ( graphVertex[subVertex->indexInGraph].labelIndex != 
				graphVertex[oldSubVertex->indexInGraph].labelIndex ) 
			{
				*newVertex = subVertex;
				break;
			}
		}

		// Get vertex (in the graph) on the other end of the edge that 
		// connects the new vertex to the subgraph.  The new vertex should 
		// only have 1 edge, so that is why " + 0".
		if ((*newVertex)->numberOfEdges > 0)				// if this is the source
		{
			*newEdge = &(graphVertex[(*newVertex)->indexInGraph].
					edges[subDef->edges[(*newVertex)->edgesIndex + 0].indexInGraphVertex]);
			*centralVertexIndexInGraph = (*newEdge)->targetVertexIndex;
			*source = TRUE;						// newVertex is the source of the edge
		}
		else {								// if this is the target
			// find source
			*source = FALSE;						// newVertex is the target of the edge
			targetVertexIndexInGraph = (*newVertex)->indexInGraph;
			for (index=0; index < subDef->numberOfVertices &&
				*centralVertexIndexInGraph == 0; index++)	// for all vertices
			{
				subVertex = &subDef->vertices[index];
				// for all the edges of the vertex
				for (edgeIndex=0; edgeIndex < subVertex->numberOfEdges; edgeIndex++)
				{
					*newEdge = &(graphVertex[subVertex->indexInGraph].
							edges[subDef->edges[subVertex->edgesIndex + edgeIndex].
							indexInGraphVertex]);
					if ( graphVertex[(*newEdge)->targetVertexIndex].labelIndex ==
						graphVertex[targetVertexIndexInGraph].labelIndex )
					{
						*centralVertexIndexInGraph = subVertex->indexInGraph;
						break;
					}
				}
			}
		}
		return TRUE;
	}

	return FALSE;
}

/*******************************************************************************
FUNCTION NAME: CheckExpandable
INPUTS:		PGRAPH_VARIABLES GV, 
			PSUB sub2, 
			PSUB_GRAPH_VERTEX newVertex, 
			ULONG centralVertexIndexInGraph,
			BOOLEAN source
RETURNS:	ULONG matchCount
PURPOSE:	Check to see if currentInstance can be extended with newVertex.
CALLED BY:	extemp.c: PurgeBestSubs()
*******************************************************************************/
ULONG CheckExpandable(PGRAPH_VARIABLES GV, 
					  PSUB sub2, 
					  PSUB_GRAPH_VERTEX newVertex,
					  PGRAPH_EDGE newEdge,
					  ULONG centralVertexIndexInGraph,
					  BOOLEAN source )
{
	BOOLEAN done;
	BOOLEAN cont;
	PGRAPH_VERTEX vertexInGraph;
	PSUB_GRAPH currentInstance;
	ULONG matchCount;
	ULONG edgeIndex;
	ULONG index;
	PGRAPH_EDGE edge;
	PGRAPH_VERTEX graphVertex;

#ifdef DEBUG_TRACE
	printf( "%s: CheckExpandable()\n", __FILE__ );
#endif


	graphVertex = GV->graph->vertices;
	matchCount = 0;
	cont = TRUE;						// this keeps it going while instances match
										// and forces giving up at the first mismatch
	
	InitSubGraphsList( sub2->instances );

	while ( (currentInstance = GetNextSubGraph(sub2->instances)) != NULL && cont)
	{
		done = FALSE;
		cont = FALSE;

		// for all vertices in current instance
		for (index=0; index < currentInstance->numberOfVertices && !done; index++) 
		{
			// get vertex in graph
			vertexInGraph = &graphVertex[currentInstance->vertices[index].indexInGraph];

			// SOURCE
			if (source)				// if vertex is source
			{
				// for all the edges of the new vertex
				for (edgeIndex=0; 
					 edgeIndex < graphVertex[newVertex->indexInGraph].numberOfEdges; 
					 edgeIndex++)
				{
					edge = &graphVertex[newVertex->indexInGraph].edges[edgeIndex];
					if (edge->labelIndex == newEdge->labelIndex &&
						edge->directed == newEdge->directed &&
						vertexInGraph->labelIndex == 
						graphVertex[edge->targetVertexIndex].labelIndex)
					{
						matchCount++;
						done = TRUE;
						cont = TRUE;
						break;
					}
				}
			}
			// TARGET
			else {					// if vertex is target
				if (vertexInGraph->labelIndex == 
					graphVertex[centralVertexIndexInGraph].labelIndex)
				{
					// for all the edges of the central vertex
					for (edgeIndex=0; 
						 edgeIndex < graphVertex[centralVertexIndexInGraph].numberOfEdges; 
						 edgeIndex++)
					{
						edge = &graphVertex[centralVertexIndexInGraph].edges[edgeIndex];
						if (edge->labelIndex == newEdge->labelIndex &&
							edge->directed == newEdge->directed &&
							graphVertex[newVertex->indexInGraph].labelIndex == 
							graphVertex[edge->targetVertexIndex].labelIndex)
						{
							matchCount++;
							done = TRUE;
							cont = TRUE;
							break;
						}
					}
				}
			}
		}
	}

	return matchCount;
}

/*******************************************************************************
FUNCTION NAME: PurgeBestSubs
INPUTS:		PGRAPH_VARIABLES GV, 
			PLIST_OF_SUBS bestSubsList, 
			PSUB oldSub
RETURNS:	none
PURPOSE:	Detect subs that can be expanded individually into the same sub.
			In other words, detect subs that are both substructures of the same
			larger sub. By eliminating one of them, we eliminate potentially
			exponentially growing search space.
		Note: It would be really cool if we could find a way not to expand
			the sub in a way that is eliminated in this function. That way we
			could get rid of this function, and it would make expansion faster,
			and it would eliminate needless evaluation of these subs.
CALLED BY:	extemp.c: GetBestSubs2()
*******************************************************************************/
void PurgeBestSubs(PGRAPH_VARIABLES GV, PLIST_OF_SUBS bestSubsList, PSUB oldSub)
{
	PLIST_OF_SUBS newBestSubsList;
	PSUB sub;
	PSUB sub2;
	PSUB_GRAPH_VERTEX newVertex;
	PGRAPH_EDGE newEdge;
	BOOLEAN source;
	ULONG centralVertexIndexInGraph;
	PSUB_GRAPH_VERTEX newVertex2;
	PGRAPH_EDGE newEdge2;
	BOOLEAN source2;
	ULONG centralVertexIndexInGraph2;
	ULONG matchCount;
	PGRAPH_VERTEX graphVertex;
	PLIST_OF_SUBS_NODE tempPtr;
	ULONG temp;

#ifdef DEBUG_TRACE
	printf( "%s: PurgeBestSubs()\n", __FILE__ );
#endif

	newBestSubsList = CreateSubsList( GV->beamLength );
	InitSubsList( bestSubsList );
	graphVertex = GV->graph->vertices;

	// Take subs off the Q one-by-one, and put the ones that survive
	// on newBestSubsList.
	while (bestSubsList->currentLength > 1)		// while there is at least one sub in the Q
	{
		sub = RemoveNextSub(bestSubsList);			// get next sub
		// insert sub into the new sub list
		InsertInSubsListInOrder( GV, newBestSubsList, sub, FALSE );

		// Find its extra vertex -- the one added in the expansion process
		// Note: if only an edge is added, it is not considered further.
		if ( GetNewVertex(GV, sub->definition, oldSub->definition, 
			&newVertex, &newEdge, &centralVertexIndexInGraph, &source) )
		{
			// for all the remaining subs in the Q
			InitSubsList( bestSubsList );
			while ( (sub2 = GetNextSub(bestSubsList)) != NULL)
			{
				if (sub->value == sub2->value) {			// if their value is the same
																// compare their definition
					matchCount = CheckExpandable(GV, sub2, newVertex, newEdge, 
						centralVertexIndexInGraph, source);

					// if all instances can be extended by this other vertex
					if (matchCount == sub2->instances->currentLength) 
					{
						// then check to see if 'sub' can be expanded with new vertex on sub2

						if ( GetNewVertex(GV, sub2->definition, oldSub->definition, 
							&newVertex2, &newEdge2, &centralVertexIndexInGraph2, 
							&source2) )
						{
							matchCount = CheckExpandable(GV, sub, newVertex2, 
								newEdge2, centralVertexIndexInGraph2, source2);
							if (matchCount == sub->instances->currentLength) 
							{

								// then this sub should be removed from the Q
//printf("Removed sub!\n");
								DestroySub( RemoveNextSub(bestSubsList) );
							}
						}
					}
					else					// otherwise it is unique, and make 
											// it the base of comparison
						break;
				}
				else 										// otherwise we can move on 
					break;									// to the next sub
			}
		}
	}

	sub = RemoveNextSub(bestSubsList);			// get last sub
	if (sub != NULL)
		InsertInSubsListInOrder( GV, newBestSubsList, sub, FALSE );

	// swap the 2 lists: newBestSubsList <--> bestSubsList
	temp = newBestSubsList->maxLength;
	newBestSubsList->maxLength = bestSubsList->maxLength;
	bestSubsList->maxLength = temp;

	temp = newBestSubsList->numberOfValues;
	newBestSubsList->numberOfValues = bestSubsList->numberOfValues;
	bestSubsList->numberOfValues = temp;

	temp = newBestSubsList->currentLength;
	newBestSubsList->currentLength = bestSubsList->currentLength;
	bestSubsList->currentLength = temp;

	tempPtr = newBestSubsList->currentNode;
	newBestSubsList->currentNode = bestSubsList->currentNode;
	bestSubsList->currentNode = tempPtr;

	tempPtr = newBestSubsList->head;
	newBestSubsList->head = bestSubsList->head;
	bestSubsList->head = tempPtr;

	tempPtr = newBestSubsList->tail;
	newBestSubsList->tail = bestSubsList->tail;
	bestSubsList->tail = tempPtr;

	DestroySubsList( newBestSubsList );					// destroy temporary list
}



#ifdef BEAM_LENGTH_BY_VALUE

/*******************************************************************************
FUNCTION NAME: GetBestSubs
INPUTS:		PGRAPH_VARIABLES GV, 
			PLIST_OF_SUBS bestSubsList, 
			PSUB oldSub
RETURNS:	none
PURPOSE:	Create new substructures from the best (based on number of instances)
			GV->sSBeamLength templates and insert them in the bestSubsList.
CALLED BY:	extend.c: ExtendSub()

  Notes: (IJ) This version gets the best subs based on the number of instances 
		of the templates, and if the number of instances agree, we take 
		VETemplate.
*******************************************************************************/

void GetBestSubs( PGRAPH_VARIABLES GV, PLIST_OF_SUBS bestSubsList, PSUB oldSub )
{
	PVE_TEMPLATE bestVETemplates;
	PE_TEMPLATE bestETemplates;
	ULONG index;
	PSUB newSub;
	BOOLEAN *instanceIncluded;
	ULONG prevInstances;
	ULONG mostInstances;
	BOOLEAN done;
	BOOLEAN E_VE_flag;
#ifdef _TIMING_
#ifdef _UNIX_
	struct tms tmsStart;
	struct tms tmsEnd;
#else // WIN32
	clock_t start, end;
#endif
#endif
		  
#ifdef DEBUG_TRACE
	printf( "%s: GetBestSubs()\n", __FILE__ );
#endif
		  
#ifdef _TIMING_
#ifdef _UNIX_
	times( &tmsStart );
#else // WIN32
	start = clock();
#endif
#endif
		  
	mostInstances = 0;
	index = 0;
	done = FALSE;
		
	instanceIncluded = (BOOLEAN *) Calloc( sizeof( BOOLEAN ) ,	// this one needs to initialize to 0 -> Calloc
		oldSub->instances->currentLength );

	GetStrongClasses( GV, oldSub->definition );
	GetBestTemplates( GV, &bestVETemplates, &bestETemplates );
	
/*	for ( index = 0; index < GV->sSBeamLength; index++ )
	{
*/
	while (!done) 
	{
		if ( ( bestVETemplates != NULL ) && ( bestETemplates != NULL ) )
		{
			if ( bestVETemplates->numberOfInstances >=		// added = too, since if they are = 
				 bestETemplates->numberOfInstances )		// VETemplate is better
			{
				prevInstances = bestVETemplates->numberOfInstances;
				E_VE_flag = TRUE;
				newSub = CreateSubFromVETemplate( GV, bestVETemplates,
					instanceIncluded, NULL, NULL );
				bestVETemplates = bestVETemplates->nextBestTemplate;	// get next best
			}
			else
			{
				prevInstances = bestETemplates->numberOfInstances;
				E_VE_flag = FALSE;
				newSub = CreateSubFromETemplate( GV, bestETemplates,
					instanceIncluded, NULL );
				bestETemplates = bestETemplates->nextBestTemplate;		// get next best
			}
		}
		else {
			if ( bestVETemplates != NULL )
			{
				prevInstances = bestVETemplates->numberOfInstances;
				E_VE_flag = TRUE;
				newSub = CreateSubFromVETemplate( GV, bestVETemplates, 
					instanceIncluded, NULL, NULL );
				bestVETemplates = bestVETemplates->nextBestTemplate;	// get next best
			}
			else { 
				if ( bestETemplates != NULL )
				{
					prevInstances = bestETemplates->numberOfInstances;
					E_VE_flag = FALSE;
					newSub = CreateSubFromETemplate( GV, bestETemplates,
						instanceIncluded, NULL );
					bestETemplates = bestETemplates->nextBestTemplate;	// get next best
				}
				else
					break;											// we're done
			}
		}

/*		if (prevInstances >= mostInstances) {
			mostInstances = prevInstances;
		}
		else {
			if (index < GV->sSBeamLength - 1 ) {
				mostInstances = prevInstances;
				index++;
			}
			else
				break;
			//done = TRUE;
		}
*/
		GV->minEncode = FALSE;
		EvaluateSub( GV, newSub, oldSub );		// nominencode
		GV->minEncode = TRUE;
		printf(" -- %d - %d: newSub->value: %f", E_VE_flag, prevInstances, newSub->value);

		EvaluateSub( GV, newSub, oldSub );		// minencode

//		printf("======= GetBestSubs =======\n");
		printf("   nm: %f\n", newSub->value);
//		PrintSub( GV, newSub, FALSE, FALSE );
	
		InsertInSubsListInOrder( GV, bestSubsList, newSub, FALSE );
		memset( instanceIncluded, 0, sizeof( BOOLEAN ) *
			oldSub->instances->currentLength );
	}
		
	printf(" --------------------\n");

	DestroyExpTemp( GV );
	
#ifdef _TIMING_
#ifdef _UNIX_
	times( &tmsEnd );
	GV->getBestSubsTm += ( tmsEnd.tms_utime - tmsStart.tms_utime ) /
		GV->clockTick;
#else // WIN32
	end = clock();
	GV->getBestSubsTm += ( end - start ) / GV->clockTick;
#endif
#endif
	
	return; 		
}


#else // not BEAM_LENGTH_BY_VALUE


/*******************************************************************************
FUNCTION NAME: GetBestSubs
INPUTS:		PGRAPH_VARIABLES GV, 
			PLIST_OF_SUBS bestSubsList, 
			PSUB oldSub
RETURNS:	none
PURPOSE:	Create new substructures from the best (based on number of instances)
			GV->sSBeamLength templates and insert them in the bestSubsList.
CALLED BY:	extend.c: ExtendSub()
*******************************************************************************/

void GetBestSubs( PGRAPH_VARIABLES GV, PLIST_OF_SUBS bestSubsList, PSUB oldSub )
{
	PVE_TEMPLATE *bestVETemplates;
	PE_TEMPLATE *bestETemplates;
	ULONG VEIndex;
	ULONG eIndex;
	ULONG index;
	PSUB newSub;
	BOOLEAN *instanceIncluded;
#ifdef _TIMING_
#ifdef _UNIX_
	struct tms tmsStart;
	struct tms tmsEnd;
#else // WIN32
	clock_t start, end;
#endif
#endif
		  
#ifdef DEBUG_TRACE
	printf( "%s: GetBestSubs()\n", __FILE__ );
#endif
		  
#ifdef _TIMING_
#ifdef _UNIX_
	times( &tmsStart );
#else // WIN32
	start = clock();
#endif
#endif
		  
	instanceIncluded = (BOOLEAN *) Calloc( sizeof( BOOLEAN ) ,	// this one needs to initialize to 0 -> Calloc
		oldSub->instances->currentLength );
	bestVETemplates = (PVE_TEMPLATE *) Malloc( sizeof( PVE_TEMPLATE ) * 
		GV->sSBeamLength );
	bestETemplates = (PE_TEMPLATE *) Malloc( sizeof( PE_TEMPLATE ) *
		GV->sSBeamLength );
	
	for ( index = 0; index < GV->sSBeamLength; index++ )
	{
		bestVETemplates[index] = NULL;
		bestETemplates[index] = NULL;
	}
	
	GetStrongClasses( GV, oldSub->definition );
	GetBestTemplates( GV, bestVETemplates, bestETemplates );
	
	VEIndex = GV->sSBeamLength - 1;
	eIndex = GV->sSBeamLength - 1;
	for ( index = 0; index < GV->sSBeamLength; index++ )
	{
		if ( ( bestVETemplates[VEIndex] != NULL ) && ( bestETemplates[eIndex] != NULL ) )
		{
			if ( bestVETemplates[VEIndex]->numberOfInstances >
				bestETemplates[eIndex]->numberOfInstances ) 
			{
				newSub = CreateSubFromVETemplate( GV, bestVETemplates[VEIndex],
					instanceIncluded, NULL, NULL );
				EvaluateSub( GV, newSub, oldSub );	
				InsertInSubsListInOrder( GV, bestSubsList, newSub, FALSE );
				VEIndex--;
			}
			else
			{
				newSub = CreateSubFromETemplate( GV, bestETemplates[eIndex],
					instanceIncluded, NULL );
				EvaluateSub( GV, newSub, oldSub );	
				InsertInSubsListInOrder( GV, bestSubsList, newSub, FALSE );
				eIndex--;
			}
		}
		else 
			if ( bestVETemplates[VEIndex] != NULL )
			{
				newSub = CreateSubFromVETemplate( GV, bestVETemplates[VEIndex], 
					instanceIncluded, NULL, NULL );
				EvaluateSub( GV, newSub, oldSub );	
				InsertInSubsListInOrder( GV, bestSubsList, newSub, FALSE );
				VEIndex--;
			}
			else 
				if ( bestETemplates[eIndex] != NULL )
				{
					newSub = CreateSubFromETemplate( GV, bestETemplates[eIndex],
						instanceIncluded, NULL );
					EvaluateSub( GV, newSub, oldSub );	
					InsertInSubsListInOrder( GV, bestSubsList, newSub, FALSE );
					eIndex--;
				}
				else
					break;
	
		memset( instanceIncluded, 0, sizeof( BOOLEAN ) *
			oldSub->instances->currentLength );
	}
	Free( bestVETemplates );
	Free( bestETemplates );
	Free( instanceIncluded );
	DestroyExpTemp( GV );
	
#ifdef _TIMING_
#ifdef _UNIX_
	times( &tmsEnd );
	GV->getBestSubsTm += ( tmsEnd.tms_utime - tmsStart.tms_utime ) /
		GV->clockTick;
#else // WIN32
	end = clock();
	GV->getBestSubsTm += ( end - start ) / GV->clockTick;
#endif
#endif
	
	return; 		
}
#endif // not BEAM_LENGTH_BY_VALUE


/*******************************************************************************
FUNCTION NAME: GetBestTargetSub
INPUTS:		PGRAPH_VARIABLES GV, 
			PABSTRACT_SUB abstractSub, 
			PSUB oldSub
RETURNS:	PSUB
PURPOSE:	This function is the heart of the predefined substructure search. It
			works by expanding the instances of oldSub in the ways that cover 
			uncovered vertices and edges of abstractSub and that maximize the 
			number of sub instances.
CALLED BY:	subdue.c: DiscoverTargetSub()
*******************************************************************************/

PSUB GetBestTargetSub( PGRAPH_VARIABLES GV, PABSTRACT_SUB abstractSub, PSUB oldSub )
{
	ULONG edgeStart;
	ULONG vertexLabelIndex;
	ULONG edgeLabelIndex;
	ULONG vertexIndex;
	ULONG edgeIndex;
	ULONG currentEMax;
	ULONG bestEVertex;
	ULONG bestEEdge;
	ULONG bestEEdgeLabel;
	ULONG bestETargetVertex;
	ULONG currentVEMax;
	ULONG bestVEVertex;
	ULONG bestVEEdge;
	ULONG bestVEAddedVertex;
	ULONG bestVEEdgeLabel;
	ULONG sourceMatchIndex;
	ULONG targetMatchIndex;
	ULONG newVertexLabelIndex;
	BOOLEAN edgeDirection;
	BOOLEAN sourceIsOldVertex;
	PE_TEMPLATE bestETemplate;
	PE_TEMPLATE currentETemplate;
	PVE_TEMPLATE bestVETemplate;
	PVE_TEMPLATE currentVETemplate;
	ULONG *oldToNewMatch;
	ULONG addedVertexIndex;
	BOOLEAN *instanceIncluded;
	PSUB newSub;
	
#ifdef DEBUG_TRACE
	printf( "%s: GetBestTargetSub()\n", __FILE__ );
#endif
	
	instanceIncluded = (BOOLEAN *) Malloc( sizeof( BOOLEAN ) * 
		oldSub->instances->currentLength );
	memset( instanceIncluded, 0, sizeof( BOOLEAN ) * 
		oldSub->instances->currentLength );
	oldToNewMatch = (ULONG *) Malloc( sizeof( ULONG ) * 
		oldSub->definition->numberOfVertices );

	/* If the templates are collapsed to for strong classes the possible
	expansions should check the consistent list in addition to the
	original template list. Also the same approach used in finding
	strong classes should be applied to the expansion as nodes may be
	mapped to different vertices in the abstract class initially
	although they are exact matches of one another. This can be
	handled by maintaining the tables of matches found during the
	findstrong classes function. */
	
	currentEMax = 0;
	currentVEMax = 0;	
	for ( vertexIndex = 0; vertexIndex < abstractSub->numberOfVertices;	vertexIndex++ )
	{
		/* if this vertex is covered */
		if ( abstractSub->vertices[vertexIndex].covered )
		{
			edgeStart = abstractSub->vertices[vertexIndex].edgesIndex;
			vertexLabelIndex = abstractSub->vertices[vertexIndex].labelIndex;
			/* check the edges */
			for ( edgeIndex = 0; 
			edgeIndex < abstractSub->vertices[vertexIndex].numberOfEdges; edgeIndex++ )
			{
				/* if this edge is not covered */
				if ( !abstractSub->edges[edgeStart + edgeIndex].covered )
				{
					edgeLabelIndex = abstractSub->edges[edgeStart + edgeIndex].labelIndex;
					/* if the target vertex is already covered */
					if ( abstractSub->vertices[abstractSub->edges[edgeStart + edgeIndex].
						targetVertex].covered )
					{
						/* Add an edge */
						currentETemplate = GV->expandTemplates[edgeLabelIndex].ETemplates; 
						sourceMatchIndex = abstractSub->vertices[vertexIndex].covered - 1;
						targetMatchIndex =  abstractSub->vertices[abstractSub->
							edges[edgeStart + edgeIndex].targetVertex].
							covered - 1;
						edgeDirection = abstractSub->edges[edgeStart + edgeIndex].directed;
						
						while( currentETemplate != NULL )
						{
							if ( ( !currentETemplate->allNoisy ) &&
								( sourceMatchIndex == 
								currentETemplate->sourceVertexMatchIndex ) &&
								( targetMatchIndex == 
								currentETemplate->targetVertexMatchIndex ) &&
								( edgeDirection == currentETemplate->edgeDirection ) &&
								( currentETemplate->numberOfInstances > currentEMax ) )
							{
								currentEMax = currentETemplate->numberOfInstances;
								bestEVertex = vertexIndex;
								bestEEdge = edgeIndex + edgeStart;
								bestEEdgeLabel = edgeLabelIndex;
								bestETargetVertex = abstractSub->edges[bestEEdge].targetVertex;
								bestETemplate = currentETemplate;
								break;
							}
							else currentETemplate = currentETemplate->nextTemplate;
						}
					}
					else
					{
						/* Add an edge and a vertex */
						currentVETemplate = GV->expandTemplates[edgeLabelIndex].VETemplates; 
						sourceMatchIndex = abstractSub->vertices[vertexIndex].covered - 1;
						newVertexLabelIndex =  abstractSub->
							vertices[abstractSub->edges[edgeStart + edgeIndex].targetVertex].
							labelIndex;
						edgeDirection = abstractSub->edges[edgeStart + edgeIndex].directed;
						sourceIsOldVertex = TRUE;
						
						while ( currentVETemplate != NULL )
						{
							if ( ( !currentVETemplate->allNoisy ) &&
								( sourceMatchIndex == 
								currentVETemplate->sourceVertexMatchIndex ) &&
								( newVertexLabelIndex == 
								currentVETemplate->newVertexLabelIndex ) &&
								( edgeDirection == currentVETemplate->edgeDirection ) &&
								( sourceIsOldVertex == 
								currentVETemplate->sourceIsOldVertex ) &&
								( currentVETemplate->numberOfInstances > currentVEMax ) )
							{
								currentVEMax = currentVETemplate->numberOfInstances;
								bestVEVertex = vertexIndex;
								bestVEEdge = edgeIndex + edgeStart;
								bestVEEdgeLabel = edgeLabelIndex;
								bestVEAddedVertex = abstractSub->edges[edgeStart + edgeIndex].
									targetVertex; 
								bestVETemplate = currentVETemplate;
								break;
							}
							else 
								currentVETemplate = currentVETemplate->nextTemplate;
						}
					}
				}
			}
		}
		else /* this vertex is not covered */
		{
			edgeStart = abstractSub->vertices[vertexIndex].edgesIndex;
			vertexLabelIndex = abstractSub->vertices[vertexIndex].labelIndex;
			/* check all its edges */
			for ( edgeIndex = 0; 
			edgeIndex < abstractSub->vertices[vertexIndex].numberOfEdges; edgeIndex++ )
			{
				/* if the target vertex of this edge is already covered */
				if ( ( abstractSub->edges[edgeStart + edgeIndex].directed ) &&
					( abstractSub->vertices[abstractSub->edges[edgeStart
					+ edgeIndex].targetVertex].covered ) )
				{
					/* add the uncovered vertex and its edge to the target vertex */ 
					edgeLabelIndex = abstractSub->edges[edgeStart + edgeIndex].labelIndex;
					currentVETemplate = GV->expandTemplates[edgeLabelIndex].VETemplates; 
					sourceMatchIndex = abstractSub->
						vertices[abstractSub->edges[edgeStart + edgeIndex].targetVertex].
						covered - 1;
					newVertexLabelIndex =  abstractSub->vertices[vertexIndex].labelIndex;
					edgeDirection = abstractSub->edges[edgeStart + edgeIndex].directed;
					sourceIsOldVertex = FALSE;
					
					while ( currentVETemplate != NULL ) {
						if ( ( !currentVETemplate->allNoisy ) &&
							( sourceMatchIndex ==
							currentVETemplate->sourceVertexMatchIndex ) &&
							( newVertexLabelIndex == 
							currentVETemplate->newVertexLabelIndex ) &&
							( edgeDirection == currentVETemplate->edgeDirection ) &&
							( sourceIsOldVertex == 
							currentVETemplate->sourceIsOldVertex ) &&
							( currentVETemplate->numberOfInstances > currentVEMax ) )
						{
							currentVEMax = currentVETemplate->numberOfInstances;
							bestVEVertex = vertexIndex;
							bestVEEdge = edgeIndex + edgeStart;
							bestVEEdgeLabel = edgeLabelIndex;
							bestVEAddedVertex = vertexIndex; 
							bestVETemplate = currentVETemplate;
							break;
						}
						else 
							currentVETemplate = currentVETemplate->nextTemplate;
					}
				}
			}
		}
	}
	
	
	/* determine whether the best ETemplate or the best VETemplate has more 
	instances */
	if ( ( currentVEMax == 0 ) && ( currentEMax == 0 ) )
		newSub = NULL;
	/* best VETemplate has more instances; create a sub from it */
	else if ( currentVEMax > currentEMax )
	{
		newSub = CreateSubFromVETemplate( GV, bestVETemplate, instanceIncluded,
			oldToNewMatch, &addedVertexIndex );
		for ( vertexIndex = 0; vertexIndex < abstractSub->numberOfVertices;
		vertexIndex++ )
			if ( abstractSub->vertices[vertexIndex].covered )
				abstractSub->vertices[vertexIndex].covered =
				oldToNewMatch[abstractSub->vertices[vertexIndex].covered - 1] + 1;
			abstractSub->vertices[bestVEAddedVertex].covered = addedVertexIndex + 1;
			abstractSub->edges[bestVEEdge].covered = 1;
//{U}
			/* if the added edge is undirected */
/*			if ( !abstractSub->edges[bestVEEdge].directed )
			{
				//mark the reverse edge as covered 
				edgeStart = abstractSub->vertices[bestVEAddedVertex].edgesIndex; 
				for ( edgeIndex = edgeStart; 
				edgeIndex < edgeStart + 
					abstractSub->vertices[bestVEAddedVertex].numberOfEdges;
				edgeIndex++ )
					if ( ( abstractSub->edges[edgeIndex].targetVertex == bestVEVertex ) && 
						( abstractSub->edges[edgeIndex].labelIndex == bestVEEdgeLabel ) &&
						( !abstractSub->edges[edgeIndex].directed ) &&
						( !abstractSub->edges[edgeIndex].covered ) )
					{
						abstractSub->edges[edgeIndex].covered = 1;
						break;
					}
			}
*/		
	}
	else /* best ETemplate has more instances; create a sub from it */
	{
		newSub = CreateSubFromETemplate( GV, bestETemplate, instanceIncluded,
			oldToNewMatch );
		for ( vertexIndex = 0; vertexIndex < abstractSub->numberOfVertices;
		vertexIndex++ )
			if ( abstractSub->vertices[vertexIndex].covered )
				abstractSub->vertices[vertexIndex].covered = 
				oldToNewMatch[abstractSub->vertices[vertexIndex].covered - 1] + 1;
			abstractSub->edges[bestEEdge].covered = 1;
//{U}
			/* if the added edge is undirected */
/*
			if ( !abstractSub->edges[bestEEdge].directed )
			{
				//mark the reverse edge as covered 
				edgeStart = abstractSub->vertices[bestETargetVertex].edgesIndex; 
				for ( edgeIndex = edgeStart; edgeIndex < edgeStart + 
					abstractSub->vertices[bestETargetVertex].numberOfEdges;
				edgeIndex++ )
					if ( ( abstractSub->edges[edgeIndex].targetVertex == bestEVertex ) && 
						( abstractSub->edges[edgeIndex].labelIndex == bestEEdgeLabel ) &&
						( !abstractSub->edges[edgeIndex].directed )&&
						( !abstractSub->edges[edgeIndex].covered ) )
						
					{
						abstractSub->edges[edgeIndex].covered = 1;
						break;
					}
			}
			*/		
	}
	
	Free( oldToNewMatch );
	Free( instanceIncluded );  
	return newSub;
}


/*******************************************************************************
FUNCTION NAME: AllCovered
INPUTS:		PABSTRACT_SUB abstractSub
RETURNS:	BOOLEAN
PURPOSE: 
CALLED BY:	subdue.c: DiscoverTargetSub()
*******************************************************************************/

BOOLEAN AllCovered( PABSTRACT_SUB abstractSub )
{
	ULONG index;
	
#ifdef DEBUG_TRACE
	printf( "%s: AllCovered()\n", __FILE__ );
#endif
	
	for ( index = 0; index < abstractSub->numberOfVertices; index++ )
		if ( abstractSub->vertices[index].covered == 0 )
			return FALSE;

	for ( index = 0; index < abstractSub->numberOfEdges; index++ )
		if ( abstractSub->edges[index].covered == 0 )
			return FALSE;

	return TRUE;
}


/*******************************************************************************
FUNCTION NAME: FinalNoise
INPUTS:		PABSTRACT_SUB abstractSub
RETURNS:	DOUBLE
PURPOSE: 
CALLED BY: subdue.c: DiscoverTargetSub()
*******************************************************************************/

DOUBLE FinalNoise( PABSTRACT_SUB abstractSub )
{
	ULONG index;
	DOUBLE cost = 0.0;
	
#ifdef DEBUG_TRACE
	printf( "%s: FinalNoise()\n", __FILE__ );
#endif
	
	for ( index = 0; index < abstractSub->numberOfVertices; index++ )
		if ( abstractSub->vertices[index].covered == 0 )
			cost += DELETE_NODE_COST;
		
		for ( index = 0; index < abstractSub->numberOfEdges; index++ )
			if ( abstractSub->edges[index].covered == 0 )
				cost += DELETE_EDGE_COST;
			return cost;
}



