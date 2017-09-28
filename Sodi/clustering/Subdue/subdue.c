/********************************************************************
*
* SUBDUE
*
* FILE NAME: subdue.c
*
********************************************************************/



#include "subdue.h"


/*******************************************************************************
FUNCTION NAME: Subdue
INPUTS:		PGRAPH_VARIABLES GV
RETURNS:	PLIST_OF_SUBS
PURPOSE:	Driver for the Subdue algorithm: Create the list of initial 
			substructures, extend each substructure and insert it in the 
			FinalSubs list, and call subdue.c: Discover() to expand the 
			substructures and find the best ones.
CALLED BY:	main.c: main()
*******************************************************************************/

PLIST_OF_SUBS Subdue( PGRAPH_VARIABLES GV )
{
	PLIST_OF_SUBS initialSubsList;
	PLIST_OF_SUBS finalSubsList;
	PLIST_OF_SUBS extendedSubs;
	PSUB currentSub;
	PSUB newSub;
	DOUBLE worstValueOnChildList;
	
#ifdef DEBUG_TRACE
	printf( "%s: Subdue()\n", __FILE__ );
#endif
	
	initialSubsList = InitialSubs( GV );
	finalSubsList = CreateSubsList( GV->beamLength );
	extendedSubs = CreateSubsList( GV->beamLength );//subBeamLength );
	
	if ( GV->negativeGraph )
		GV->negativeGraph->firstSetOfSubs = TRUE;
	
	while ( (currentSub = RemoveNextSub(initialSubsList)) != NULL )
	{
		if ( finalSubsList->tail && (finalSubsList->numberOfValues == GV->beamLength) )
			worstValueOnChildList = finalSubsList->tail->sub->value;
		else 
			worstValueOnChildList = 0;
		ExtendSub( GV, currentSub, extendedSubs, worstValueOnChildList );
		while ( ( newSub = RemoveNextSub( extendedSubs ) ) != NULL )
		{
			if ( !Member( newSub, finalSubsList ) )
				InsertInSubsListInOrder( GV, finalSubsList, newSub, TRUE );
			else 
			{
				/*printf( "Call 1: sub %lu\n", (ULONG) newSub );*/
				if ( GV->negativeGraph )
					FindAndDestroyNegativeSub( GV->negativeGraph, (ULONG) newSub );
				DestroySub( newSub );
			}
		}
		/*if ( GV->negativeGraph )
		FindAndDestroyNegativeSub( GV->negativeGraph, (ULONG) currentSub );*/
		DestroySub( currentSub );
	}
	DestroySubsList( initialSubsList );
	DestroySubsList( extendedSubs );
	
	if ( GV->negativeGraph )
		GV->negativeGraph->firstSetOfSubs = FALSE;
	
#ifdef _TIMING_
	if ( GV->outputLevel >= OL_INTERMEDIATE_SUBS ) {
		PrintTimings(GV);
	}
#endif
	
	return Discover( GV, finalSubsList );
}


/*******************************************************************************
FUNCTION NAME: Discover
INPUTS:		PGRAPH_VARIABLES GV, 
			PLIST_OF_SUBS parentList
RETURNS:	PLIST_OF_SUBS
PURPOSE:	Extend each sub in parentList, and if it is not a member of subsList,
			insert the extended subs into subsList. Insert the best subs that 
			are of acceptable size into the list bestOfProcessedSubs and return 
			that list.
CALLED BY:	main.c: main()
			subdue.c: Subdue()
*******************************************************************************/

PLIST_OF_SUBS Discover( PGRAPH_VARIABLES GV, PLIST_OF_SUBS parentList )
{
	ULONG numberOfProcessedSubs;
	PSUB currentSub;
	PSUB newSub;
	PLIST_OF_SUBS bestOfProcessedSubs;
	PLIST_OF_SUBS extendedSubs;
	PLIST_OF_SUBS childList;
	PLIST_OF_SUBS tempList;
	BOOLEAN done = FALSE;
	ULONG extensionsWithNoBest;		// number Of Extensions After Last Best Found
	BOOLEAN foundNewBest;
	DOUBLE worstValueOnChildList;
	
#ifdef DEBUG_TRACE
	printf( "%s: Discover()\n", __FILE__ );
#endif
	
	extensionsWithNoBest = 0;
	numberOfProcessedSubs = 0;
	bestOfProcessedSubs = CreateSubsList( GV->maxNumberOfBestSubs );
	extendedSubs = CreateSubsList( GV->beamLength );
	childList = CreateSubsList( GV->beamLength );
	
	do											// while there are subs in the parent list
	{
		foundNewBest = FALSE;
		while ( (currentSub = RemoveNextSub(parentList)) != NULL )		// get next sub
		{
			numberOfProcessedSubs++;
			if ( GV->prune2 ||												// if prune2 or
				 (numberOfProcessedSubs <= GV->limitOfProcessedSubs) )		// if within limit
			{
				if ( childList->tail && (childList->numberOfValues == GV->beamLength) )
					worstValueOnChildList = childList->tail->sub->value;
				else 
					worstValueOnChildList = 0;
				ExtendSub( GV, currentSub, extendedSubs, worstValueOnChildList);// extend the sub
				while( ( newSub = RemoveNextSub( extendedSubs ) ) != NULL )		// for all extended instances
				{
					if ( !GV->prune ||												// if not pruning
						 ( GV->prune && ( newSub->value >= currentSub->value ) ) )	// or pruning but value OK
					{
						if ( !Member( newSub, bestOfProcessedSubs ) &&		// if newSub is not one of best
							 !Member( newSub, childList ) &&				// and is not already in childList
							 ( !GV->limitedSize ||										// and size is OK
							newSub->definition->numberOfVertices <= GV->maxSize ) ) 
						{
							InsertInSubsListInOrder( GV, childList, newSub, TRUE );		// insert it to childList
						} 
						else {
							if ( GV->negativeGraph ) {
								FindAndDestroyNegativeSub(GV->negativeGraph, (ULONG)newSub);
							}
							
							DestroySub( newSub );
						}
					}
					else {
						if ( GV->negativeGraph ) {
							FindAndDestroyNegativeSub( GV->negativeGraph, (ULONG) newSub );
						}

						DestroySub( newSub );
					}
				}
			}
			else {												// limit reached
				printf( "\nProcessing limit of %lu substructures has been reached;\n",
					GV->limitOfProcessedSubs );
				printf( "  discovery halted.\n\n" );
				fflush( stdout );
				if ( AcceptableSize( GV, currentSub ) )
					InsertInSubsListInOrder( GV, bestOfProcessedSubs, currentSub, TRUE );

				while ( ( currentSub = RemoveNextSub( parentList ) ) != NULL ) {
					if ( AcceptableSize( GV, currentSub ) )
						InsertInSubsListInOrder( GV, bestOfProcessedSubs, currentSub, TRUE );
					while ( ( currentSub = RemoveNextSub( childList ) ) != NULL ) {
						if ( AcceptableSize( GV, currentSub ) ) {
							InsertInSubsListInOrder( GV, bestOfProcessedSubs, currentSub, TRUE);
						}
						done = TRUE;
						break;
					}
				}
			}

			// Plot compression info
			if (GV->plotFile && currentSub != NULL) {				// if there is a plot file
				fprintf(GV->plotFile, "%lu \t%lu \t%lu \t%f \t%f ",		// plot info
					GV->subNumber,
					numberOfProcessedSubs, 
					currentSub->definition->numberOfVertices,
					currentSub->inputGraphCompressedWithSubDL,
					currentSub->inputGraphCompressedWithSubDL / GV->inputGraphDL);
#ifdef _TIMING_
				fprintf(GV->plotFile, "\t%f \t%f \t%f \t%f \t%f \t%f \t%f",		// plot timing info
						GV->extendInstancesTm, GV->getBestSubsTm, 
						GV->getStrongClassesTm, GV->getBestTemplatesTm,
						GV->addENoiseTm, GV->addVENoiseTm, GV->fuzzyMatchTm);
#endif
				fprintf(GV->plotFile, " \n");						// end of line
				fflush(GV->plotFile);
			}

			// print current sub and timing
			if ( GV->outputLevel >= OL_INTERMEDIATE_SUBS && 
				currentSub != NULL && 
				AcceptableSize( GV, currentSub ) )
			{
				printf( "\nEvaluated substructure number %lu with %lu vertices\n",
					numberOfProcessedSubs, currentSub->definition->numberOfVertices );

				PrintSub( GV, currentSub, FALSE, FALSE );
				
#ifdef _TIMING_
				PrintTimings(GV);
#endif
			}
			
			// insert current sub into list of best subs
			if ( currentSub != NULL && 
				 AcceptableSize( GV, currentSub ) &&
				 !Member( currentSub, bestOfProcessedSubs ) ) 
			{
				// if current sub was not inserted into list of best subs
				if (InsertInSubsListInOrder( GV, bestOfProcessedSubs, currentSub, TRUE ) ) 
				{
					foundNewBest = TRUE;			// mark good insertion
				}
			}
			else {
				if ( GV->negativeGraph )
					FindAndDestroyNegativeSub( GV->negativeGraph, (ULONG) currentSub );
				DestroySub( currentSub );
			}
		}
		tempList = parentList;
		parentList = childList;
		childList = tempList;

		if (!foundNewBest) {
			extensionsWithNoBest++;			// count iterations that failed to produce a good sub
		}
		else {
			extensionsWithNoBest = 0;		// start over counting
		}
	} while ( (!done && parentList->currentLength > 0) &&
			  (!GV->prune2 || 
			  (GV->prune2 && (extensionsWithNoBest < GV->prune2Value))) );
	
	DestroySubsList( parentList );
	DestroySubsList( childList );
	DestroySubsList( extendedSubs );
	
#ifdef _TIMING_
	if ( GV->outputLevel <= OL_BEST_SUBS ) {
		PrintTimings(GV);
	}
#endif
	
	return bestOfProcessedSubs;
}


/*******************************************************************************
FUNCTION NAME: InitialSubs
INPUTS:		PGRAPH_VARIABLES GV
RETURNS:	PLIST_OF_SUBS 
PURPOSE:	For each uniquely labeled vertex in the global graph, generate a 
			subgraph definition consisting of that vertex, collect all the 
			instances of that subgraph in a list of subgraphs, and insert a 
			node containing the definition and the list of instances into a 
			list of subs, ordered by sub value.
CALLED BY:	subdue.c: Subdue()
*******************************************************************************/

PLIST_OF_SUBS InitialSubs( PGRAPH_VARIABLES GV )
{
	PLIST_OF_SUBS initialSubsList;
	ULONG index;
	PSUB newSub;
	PSUB *InitialSubsArray;
	PSUB_GRAPH newSubGraph;
	PGRAPH_VERTEX graphVertices;
	ULONG vertexLabelIndex;
	
#ifdef DEBUG_TRACE
	printf( "%s: InitialSubs()\n", __FILE__ );
#endif
	
	initialSubsList = CreateSubsList( GV->numberOfVertexLabels );
	graphVertices = GV->graph->vertices;
	InitialSubsArray = (PSUB *) Malloc( GV->numberOfLabels * sizeof( PSUB ) );
	for ( index = 0; index < GV->numberOfLabels; index++ )
		InitialSubsArray[index] = NULL;

	/* define indices for initial subs */
	for ( index = 0; index < GV->graph->numberOfVertices; index++ )
	{
		vertexLabelIndex = graphVertices[index].labelIndex;
		if ( InitialSubsArray[vertexLabelIndex] == NULL )
		{
			newSubGraph = CreateSubGraph( index );
			newSub = CreateSub( newSubGraph );
			InsertInSubGraphsList( newSub->instances, newSubGraph );
			InitialSubsArray[vertexLabelIndex] = newSub;
		}
		else
		{
			newSubGraph = CreateSubGraph( index );
			InsertInSubGraphsList( InitialSubsArray[vertexLabelIndex]->
				instances, newSubGraph );
		}
	}
	
	for ( index = 0; index < GV->numberOfLabels; index++ )
		if ( InitialSubsArray[index] != NULL )
			InsertInSubsListInOrder( GV, initialSubsList, InitialSubsArray[index], FALSE );
		
	Free( InitialSubsArray );
		
	return initialSubsList;
}


/*******************************************************************************
FUNCTION NAME: GetSubWithVertexLabel
INPUTS:		PGRAPH_VARIABLES GV, 
			ULONG vertexLabel
RETURNS:	PSUB
PURPOSE:	Create a sub of all the vertices in graph with a label that
			matches vertexLabel.
CALLED BY: subdue.c: GetBestInitialSub()
*******************************************************************************/

PSUB GetSubWithVertexLabel( PGRAPH_VARIABLES GV, ULONG vertexLabel )
{
	ULONG vertexIndex;
	PSUB newSub = NULL;
	PSUB_GRAPH newSubGraph;
	
#ifdef DEBUG_TRACE
	printf( "%s: GetSubWithVertexLabel()\n", __FILE__ );
#endif
	
	for ( vertexIndex = 0; vertexIndex < GV->graph->numberOfVertices ; 
	vertexIndex++ )
		if ( SameLabel( GV, GV->graph->vertices[vertexIndex].labelIndex, 
			vertexLabel ) )
		{
			if ( newSub == NULL )
			{
				newSubGraph = CreateSubGraph( vertexIndex );
				newSub = CreateSub( newSubGraph );
				InsertInSubGraphsList( newSub->instances, newSubGraph );
			}
			else
			{
				newSubGraph = CreateSubGraph( vertexIndex );
				InsertInSubGraphsList( newSub->instances, newSubGraph );
			}
		}
		return newSub;
}	


/*******************************************************************************
FUNCTION NAME: GetBestInitialSub
INPUTS:		PGRAPH_VARIABLES GV, 
			PABSTRACT_SUB abstractSub
RETURNS:	PSUB
PURPOSE: 
CALLED BY:	subdue.c: DiscoverAbstractSub()
*******************************************************************************/

PSUB GetBestInitialSub( PGRAPH_VARIABLES GV, PABSTRACT_SUB abstractSub )
{
	ULONG maxIndex;
	ULONG maxNumber;
	ULONG index;
	
#ifdef DEBUG_TRACE
	printf( "%s: GetBestInitialSub()\n", __FILE__ );
#endif
	
	maxNumber = 0;
	maxIndex = 0;
	for ( index = 0; index < abstractSub->numberOfVertices; index++ ) {
		if ( GV->numberOfVerticesWithLabel[abstractSub->vertices[index].labelIndex]
			> maxNumber )
		{
			maxIndex = index;
			maxNumber = 
				GV->numberOfVerticesWithLabel[abstractSub->vertices[index].labelIndex];
		}
	}
		
	if ( maxNumber == 0 ) {
		return NULL;
	} 
	else {
		/* covered with index number 0 of the sub graph definition */
		abstractSub->vertices[maxIndex].covered = 1;
		return GetSubWithVertexLabel( GV, 
			abstractSub->vertices[maxIndex].labelIndex );
	}
}


/*******************************************************************************
FUNCTION NAME: DiscoverAbstractSub
INPUTS:		PGRAPH_VARIABLES GV, 
			PABSTRACT_SUB abstractSub
RETURNS:	PSUB
PURPOSE: 
CALLED BY:	pvm.c: Communicate()
*******************************************************************************/

PSUB DiscoverAbstractSub( PGRAPH_VARIABLES GV, PABSTRACT_SUB abstractSub )
{
	PSUB currentSub;
	
#ifdef DEBUG_TRACE
	printf( "%s: DiscoverAbstractSub()\n", __FILE__ );
#endif
	
	currentSub = GetBestInitialSub( GV, abstractSub );
	if ( currentSub == NULL )
		return NULL;
	else 
		return DiscoverTargetSub( GV, currentSub, abstractSub );
}


/*******************************************************************************
FUNCTION NAME: DiscoverTargetSub
INPUTS:		PGRAPH_VARIABLES GV, 
			PSUB currentSub, 
			PABSTRACT_SUB abstractSub
RETURNS:	PSUB
PURPOSE: 
CALLED BY:	subdue.c: DiscoverAbstractSub()
*******************************************************************************/

PSUB DiscoverTargetSub( PGRAPH_VARIABLES GV, PSUB currentSub, 
						PABSTRACT_SUB abstractSub )
{
	PSUB newSub;
	DOUBLE noise;
	
#ifdef DEBUG_TRACE
	printf( "%s: DiscoverTargetSub()\n", __FILE__ );
#endif
	
	if ( GV->newLabel )
		ResizeExpandTemplates( GV );
	
	while ( !AllCovered( abstractSub ) )
	{
		ExtendInstances( GV, currentSub );
		GetStrongClasses( GV, currentSub->definition );
		newSub = GetBestTargetSub( GV, abstractSub, currentSub );
		DestroyExpTemp( GV );
		if ( newSub != NULL )
		{
			DestroySub( currentSub );
			currentSub = newSub;
		}
		else 
		{
			if ( GV->matchCostMaxPercent > 0.0 )
			{
				noise = FinalNoise( abstractSub );
				if ( noise <= GV->matchCostMaxPercent * 
					( abstractSub->numberOfVertices + abstractSub->numberOfEdges ) )
				{
					EvaluateSub( GV, currentSub, NULL );
					return currentSub;
				}
				else
				{
					DestroySub( currentSub );
					return NULL;
				}
			}
			else
			{
				DestroySub( currentSub );
				return NULL;
			}
		}
	}

	EvaluateSub( GV, currentSub, NULL );
	
	return currentSub;
}


/*******************************************************************************
FUNCTION NAME: DiscoverPredefSubs
INPUTS:		PGRAPH_VARIABLES GV, 
			PLIST_OF_ABSTRACT_SUBS predefSubs
RETURNS:	PLIST_OF_SUBS
PURPOSE:	Find instances of the predefSubs in the graph.
CALLED BY:	main.c: main()
*******************************************************************************/

PLIST_OF_SUBS DiscoverPredefSubs( PGRAPH_VARIABLES GV, 
								PLIST_OF_ABSTRACT_SUBS predefSubs )
{
	PABSTRACT_SUB currentPredefSub;
	PLIST_OF_SUBS discoveredSubs;
	PSUB newSub;
	
#ifdef DEBUG_TRACE
	printf( "%s: DiscoverPredefSubs()\n", __FILE__ );
#endif
	
	discoveredSubs = CreateSubsList( GV->numberOfPredefSubs );
	while ( ( currentPredefSub = RemoveNextAbstractSub( predefSubs ) ) != NULL )
	{
		newSub = DiscoverAbstractSub( GV, currentPredefSub );
		if ( newSub != NULL )
		{
			EvaluateSub( GV, newSub, NULL );
			InsertInSubsListInOrder( GV, discoveredSubs, newSub, TRUE );
		}
		DestroyAbstractSub( currentPredefSub );
	}
	Free( predefSubs );
	return discoveredSubs;
}



