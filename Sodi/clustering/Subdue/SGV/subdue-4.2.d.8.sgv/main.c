/********************************************************************
*
* SUBDUE
*
* FILE NAME: main.c
*
********************************************************************/


#include "subdue.h"


#ifdef _PVM_SUBDUE_                  /* main() for parallel version of Subdue */
/*******************************************************************************
FUNCTION NAME: main
INPUTS:		int argc, 
			char *argv[], 
RETURNS:	int
PURPOSE:	main for PARALLEL VERSION of Subdue 
*******************************************************************************/
// This currently does the unsupervised version; it is expecting a split graph.
//
int main( int argc, char *argv[] )
{
	PGRAPH_VARIABLES positiveGraph;
	PLIST_OF_SUBS bestSubs;
	PLIST_OF_ABSTRACT_SUBS predefSubs;
	PSUB bestSub;
	PGRAPH compressedGraph;
	char  fileName[MAX_TOKEN_LENGTH];
	char  path[MAX_TOKEN_LENGTH];
	char  *predefSubsFileName = NULL;
	int index;
	char **childArguments;
	int *childIDs;
	int numberOfChildren;
	BOOLEAN supervised = FALSE;

#ifdef _TIMING_
	DOUBLE discoveryTime;
	DOUBLE remoteTime;
	int bufferID;
#ifdef _UNIX_
	struct tms times1;
	struct tms times2;
	struct tms times3;
#else // WIN32
	clock_t time1, time2, time3;
#endif //_UNIX_
#endif
	
	positiveGraph = (PGRAPH_VARIABLES) Malloc( sizeof( GRAPH_VARIABLES ) );
	LoadDefaultGraphParameters(positiveGraph);
	
	///////////////
	// Set up PVM
	positiveGraph->procID = pvm_mytid();				// get ID of this process
	
	if ( positiveGraph->procID < 0 ) 
	{
		printf( "Error creating master process\n" );
		pvm_perror( argv[0] );
		ErrorFatal("Exiting Subdue...");
	}

	positiveGraph->masterProc = pvm_parent();			// get the master process ID
	
	if ( (positiveGraph->masterProc < 0) && 
		 (positiveGraph->masterProc != PvmNoParent) )
	{
		printf( "Error creating group\n" );
		pvm_perror( argv[0] );
		ErrorFatal("Exiting Subdue...");
	}
	
	if ( positiveGraph->masterProc == PvmNoParent )		// if there is no parent process
	{
		positiveGraph->masterProc = positiveGraph->procID;	// make this the parent process
	}
	
	// join the workgroup
	sprintf( positiveGraph->groupName, PVM_SUBDUE_GROUP_NAME );	
	positiveGraph->myGroupID = pvm_joingroup( positiveGraph->groupName );
	
	if ( positiveGraph->myGroupID < 0 )					// if there is an error joining the group
	{
		printf( "Error joining PVM work group\n" );			// print error and 
		pvm_perror( argv[0] );
		ErrorFatal("Exiting Subdue...");
	}
	
	ParseCommandLine( positiveGraph, argc, argv, &supervised, &predefSubsFileName );
//	positiveGraph->sSBeamLength = positiveGraph->beamLength;

	if ( positiveGraph->procID < 2 ) {						// check # of processes
		ErrorFatal( "Subdue: -nproc must be greater than 1" );
	}

	/* spawn Subdue processes on the other nodes */
	if ( positiveGraph->masterProc == positiveGraph->procID )	// if this is the main process
	{
		childArguments = (char **) Malloc ( sizeof(char*) * argc );  
		childIDs = (int *) Malloc( sizeof(int) * (positiveGraph->numberOfProcs - 1) );
		
		// shift arguments left for child arguments
		for ( index = 0; index < argc - 2; index++ )
			childArguments[index] = argv[index + 1];

		getcwd( path, MAX_TOKEN_LENGTH );

		// get file name
#ifdef WIN32
		sprintf( fileName, "%s\\%s", path, argv[argc - 1] );
#else // _UNIX_
		sprintf( fileName, "%s/%s", path, argv[argc - 1] );
#endif

		sprintf( fileName, "%s", argv[argc - 1] );
		childArguments[argc - 2] = fileName;
		childArguments[argc - 1] = NULL;
		pvm_catchout( stdout );

		// spawn other processes
		numberOfChildren = pvm_spawn( PVM_SUBDUE_NAME,				// task name
				childArguments,										// arguments
				PvmTaskDefault,										// flags
				//PvmTaskHost /*+ PvmTaskDebug*/ + PvmHostCompl,	
				"",													// where
				// ".",
				positiveGraph->numberOfProcs - 1,					// # tasks
				childIDs );											// tids
		
		// handle errors
		if ( numberOfChildren != ( positiveGraph->numberOfProcs - 1 ) )
		{
			printf( "Cannot spawn %d children %d %s\n", 
				positiveGraph->numberOfProcs - 1,
				numberOfChildren, 
				argv[0] );
			PrintPVMError(childIDs[0]);			// print PVM error for the first child
			ErrorFatal("Exiting Subdue...");
		}

		Free( childArguments );
		Free( childIDs );
		
		printf( "\nSubdue %s - Parallel version\n\n", VERSION_NUMBER );
	}

	// for all the processes

	/* read local graph file.  Graph file name is  name'.graph.'totalParts#'.part'#  */
	strcpy( fileName, argv[argc - 1] );
	sprintf( fileName, "%s.graph.%d.part%d", 
		fileName, 
		positiveGraph->numberOfProcs, 
		positiveGraph->myGroupID );

	ReadGraph( positiveGraph, fileName );
	//ReadGlobalGraph( fileName );
	
	if ( positiveGraph->alternativeGroups == TRUE && positiveGraph->numberOfGroups > 0 ) 
	{
		CheckGroups(positiveGraph);
		AssignGroups(positiveGraph);                     /* assign labels to groups */
	}
	
	if ( positiveGraph->limitOfProcessedSubs == 0 ) { 
		positiveGraph->limitOfProcessedSubs = ( positiveGraph->graph->numberOfVertices +
												positiveGraph->graph->numberOfEdges ) / 2;
	}
	
	positiveGraph->bestSubAtProc = (PSUB *) Malloc(sizeof(PSUB) * positiveGraph->numberOfProcs);
	positiveGraph->bestAbstractSubAtProc = (PABSTRACT_SUB *)Malloc(sizeof(PABSTRACT_SUB)
		* positiveGraph->numberOfProcs );
	positiveGraph->localValueOfSubAtProc = (DOUBLE *)Malloc(sizeof(DOUBLE) * 
		positiveGraph->numberOfProcs );
	
	// while the number of iterations has not been reached, or forever if clustering
	positiveGraph->subNumber = 1;
	while ( (positiveGraph->subNumber <= positiveGraph->numberOfCompressionLevels) ||
		(positiveGraph->cluster && positiveGraph->numberOfCompressionLevels == 0) )
	{
		pvm_barrier( positiveGraph->groupName, positiveGraph->numberOfProcs );
		if ( positiveGraph->numberOfCompressionLevels > 1 && 
			positiveGraph->outputLevel >= OL_BEST_SUBS) 
		{
			printf( "\n\n-----Iteration %d-----\n\n", positiveGraph->subNumber );
		}
		positiveGraph->inputGraphDL = GraphDL( positiveGraph, positiveGraph->graph );
		PrintParameters(positiveGraph);
		

#ifdef _TIMING_
#ifdef _UNIX_
			times( &times1 );
#else // WIN32
	time1 = clock();
#endif
#endif

		if ( predefSubsFileName != NULL )
		{                           /* find instances of predefined substructures */
			predefSubs = ReadPredefSubs( positiveGraph, predefSubsFileName );
			CreateGraphTemplate( positiveGraph );
			bestSubs = DiscoverPredefSubs( positiveGraph, predefSubs );	
			Free( predefSubsFileName );
			predefSubsFileName = NULL;
		}
		else
		{
			CreateGraphTemplate(positiveGraph);
			bestSubs = Subdue(positiveGraph);           /* run the Subdue algorithm */
		}
#ifdef _TIMING_
#ifdef _UNIX_
		times( &times2 );
#else // WIN32
	time2 = clock();
#endif
#endif
		
		for( index = 0; index < positiveGraph->numberOfProcs; index++ )
		{
			positiveGraph->bestSubAtProc[index] = NULL;
			positiveGraph->bestAbstractSubAtProc[index] = NULL;
			positiveGraph->localValueOfSubAtProc[index] = 0.0;
		}

		if ( positiveGraph->procID == positiveGraph->masterProc )
		{
			positiveGraph->globalValueOfSubAtProc = (DOUBLE *) Malloc( sizeof( DOUBLE ) *
				positiveGraph->numberOfProcs );
			for( index = 0; index < positiveGraph->numberOfProcs; index++ )
				positiveGraph->globalValueOfSubAtProc[index] = 0.0;
		}

		positiveGraph->bestSubAtProc[positiveGraph->myGroupID] = RemoveNextSub( bestSubs );
		printf( "\nBest substructure:\n" );
		PrintSub(positiveGraph, positiveGraph->bestSubAtProc[positiveGraph->myGroupID], 
			TRUE, FALSE );

		DestroySubsList( bestSubs );
		Communicate(positiveGraph);

#ifdef _TIMING_
#ifdef _UNIX_
		times( &times3 );
		
		discoveryTime = times2.tms_utime + times2.tms_stime - times1.tms_utime -
			times1.tms_stime;
		remoteTime = times3.tms_utime + times3.tms_stime - times2.tms_utime -
			times2.tms_stime;
#else // WIN32
		time3 = clock();
		discoveryTime = time2 - time1;
		remoteTime = time3 - time2;
#endif

		/* send local execution timings to masterProc */
		bufferID = pvm_initsend( PvmDataDefault );
		pvm_packf( "%f %f", discoveryTime, remoteTime );
		pvm_send( positiveGraph->masterProc, PVM_TIME_STAT );
#endif
		
		if ( positiveGraph->procID == positiveGraph->masterProc )
			PrintResults(positiveGraph);
		/* if we're doing iterations or clustering, compress the */
		/* local graph with the best global sub */
		if ( (positiveGraph->subNumber < positiveGraph->numberOfCompressionLevels) ||
			 (positiveGraph->cluster && positiveGraph->numberOfCompressionLevels == 0) )
		{ 
			bestSub = positiveGraph->bestSubAtProc[positiveGraph->bestSubGroupID];
			if ( bestSub != NULL )
			{
				positiveGraph->finalGraph = TRUE;
				compressedGraph = CompressUsing( positiveGraph, bestSub);
				positiveGraph->finalGraph = FALSE;
				if (positiveGraph->outputLevel >= OL_BEST_SUBS)
					printf( "\nGraph is compressed using best global substructure.\n" );
				
				DestroyGraphTemplate(positiveGraph);
				DestroyGraph( positiveGraph->graph );
				positiveGraph->graph = compressedGraph;
			}
			else
				DestroyGraphTemplate(positiveGraph);
			
			for ( index = 0; index < positiveGraph->numberOfProcs; index++ )
			{
				if ( positiveGraph->bestSubAtProc[index] != NULL )
					DestroySub( positiveGraph->bestSubAtProc[index] );
				if ( positiveGraph->bestAbstractSubAtProc[index] != NULL )
					DestroyAbstractSub( positiveGraph->bestAbstractSubAtProc[index] );
			}
		}
		
		positiveGraph->subNumber++;
	}                                                  /* end of iteration loop */
	/* Free memory */
	for ( index = 0; index < positiveGraph->numberOfProcs; index++ )
	{
		if ( positiveGraph->bestSubAtProc[index] != NULL )
			DestroySub( positiveGraph->bestSubAtProc[index] );
		if ( positiveGraph->bestAbstractSubAtProc[index] != NULL )
			DestroyAbstractSub( positiveGraph->bestAbstractSubAtProc[index] );
	}
	Free( positiveGraph->bestSubAtProc );
	Free( positiveGraph->bestAbstractSubAtProc );
	Free( positiveGraph->localValueOfSubAtProc );
	if ( positiveGraph->procID == positiveGraph->masterProc )
		Free( positiveGraph->globalValueOfSubAtProc );
	DestroyGraphTemplate(positiveGraph);
	DestroyGraph( positiveGraph->graph );
	DestroyLabelList(positiveGraph);
	/* shutdown */
	pvm_barrier( positiveGraph->groupName, positiveGraph->numberOfProcs );
	pvm_exit();
	
	ClosePlotFile(positiveGraph);

	return 0;
}




#else                                  
/*******************************************************************************
FUNCTION NAME: main
INPUTS:		int argc, 
			char *argv[], 
RETURNS:	int
PURPOSE:	main for SERIAL VERSION of Subdue 
*******************************************************************************/
int main( int argc, char *argv[] )
{
	PGRAPH_VARIABLES positiveGraph;
	PGRAPH_VARIABLES negativeGraph;
	PGRAPH compressedGraph;
	PLIST_OF_SUBS bestSubs;
	PLIST_OF_ABSTRACT_SUBS predefSubs;
	PSUB bestSub;
	PNEGATIVE_SUB negativeSub;
	DOUBLE currentGraphDL;
	char *predefSubsFileName = NULL;
	BOOLEAN supervised = FALSE;
	PCLASSIFICATION_LATTICE classLattice;
	DOUBLE compressionRatio;
	BOOLEAN switchedShape;
#ifdef _TIMING_
	clock_t startTotal;
	clock_t endTotal;
	clock_t start;
	clock_t end;
#ifdef _UNIX_
	struct tms tmsStart;
	struct tms tmsEnd;
#endif
#endif
	
#ifdef DEBUG_TRACE
	printf( "%s: main()\n", __FILE__ );
#endif
	
#ifdef _TIMING_
#ifdef _UNIX_
	start = times( &tmsStart );                              /* get system time */
	ASSERT( start > 0 );
#else // WIN32
	start = clock();
	startTotal = start;
#endif
#endif

	positiveGraph = (PGRAPH_VARIABLES) Malloc( sizeof( GRAPH_VARIABLES ) );
	LoadDefaultGraphParameters(positiveGraph);
	
	printf( "\nSubdue %s - Serial version\n\n", VERSION_NUMBER );

	ParseCommandLine( positiveGraph, argc, argv, &supervised, &predefSubsFileName );
	
	// Read graph(s) from file
	if ( supervised )
	{
		// Supervised version
		negativeGraph = CreateNegativeGraph( positiveGraph );	// create negative graph
		positiveGraph->negativeGraph = negativeGraph;

		// read positive graph
		sprintf( positiveGraph->graphFileName, "%s.%s", 
			argv[argc - 1], FILE_EXT_POSITIVE_GRAPH);
		ReadGraph( positiveGraph, positiveGraph->graphFileName );
		
		// read negative graph
		sprintf( negativeGraph->graphFileName, "%s.%s", 
			argv[argc - 1], FILE_EXT_NEGATIVE_GRAPH );
		ReadGraph( negativeGraph, negativeGraph->graphFileName );

		if ( positiveGraph->alternativeGroups == TRUE && 
			positiveGraph->numberOfGroups > 0 ) 
		{
			CheckGroups( positiveGraph );
			AssignGroups( positiveGraph );
			if ( negativeGraph->numberOfGroups > 0 )
			{
				CheckGroups( negativeGraph );
				AssignGroups( negativeGraph );
			}
		}
	}
	else
	{
		// Unsupervised version
		positiveGraph->negativeGraph = negativeGraph = NULL;	// no negative graph here

		sprintf( positiveGraph->graphFileName, "%s", argv[argc - 1]);
		ReadGraph( positiveGraph, positiveGraph->graphFileName );

                if(positiveGraph->display)
                {
                    positiveGraph->sockVar = (PSOCKET_VAR)Malloc(sizeof(SOCKET_VAR));
                    positiveGraph->sockVar->portNo = 5872;
                    StartServer(positiveGraph);  
                         // to connect to Visualizer at port specified
                }
		if ( positiveGraph->alternativeGroups == TRUE && 
			positiveGraph->numberOfGroups > 0 ) 
		{
			CheckGroups( positiveGraph );
			AssignGroups( positiveGraph );
		}
	}
	
	// set default processing limit
	if ( positiveGraph->limitOfProcessedSubs == 0 ) {
		positiveGraph->limitOfProcessedSubs = 
			( positiveGraph->graph->numberOfVertices + positiveGraph->graph->numberOfEdges ) / 2;
	}
	
#ifdef _TIMING_
#ifdef _UNIX_
	// Print timing info
	end = times( &tmsEnd );
	ASSERT( end > 0 );
	printf( "Time taken for initialization and input : \n" );
	printf( "\tReal time elapsed = %7.3f sec\n", 
		( end - start ) / (double) positiveGraph->clockTick );
	printf( "\tSystem CPU time   = %7.3f sec\n", 
		( tmsEnd.tms_stime - tmsStart.tms_stime ) / (double) positiveGraph->clockTick );
	printf( "\tUser CPU time     = %7.3f sec\n\n", 
		( tmsEnd.tms_utime - tmsStart.tms_utime ) / (double) positiveGraph->clockTick );
	start = times( &tmsStart );
	ASSERT( start > 0 );
#else // WIN32
	end = clock();
	printf( "Time taken for initialization and input : \n" );
	printf( "\tCPU time used by SUBDUE = %7.3f sec\n\n", 
		( end - start ) / (double) positiveGraph->clockTick );
	start = clock();
#endif
#endif

	if (positiveGraph->cluster)										// if clustering
		classLattice = CL_CreateLattice(positiveGraph->graphFileName);	// set up classification lattice
	switchedShape = FALSE;

	//////////////////
	// Discovery 
	/////////////////
	while ( (positiveGraph->subNumber <= positiveGraph->numberOfCompressionLevels) ||
			(positiveGraph->cluster && positiveGraph->numberOfCompressionLevels == 0))
	{
// Print input graph (for debugging only!)
//		PrintGraph( positiveGraph, positiveGraph->graph, TRUE, FALSE );

		// Print iteration number
		if ( positiveGraph->numberOfCompressionLevels > 1 && 
			positiveGraph->outputLevel >= OL_BEST_SUBS)
		{
			printf( "\n\n-----Iteration %d-----\n\n", positiveGraph->subNumber );
		}
		
		// calculate description length (DL) of the input graph
		positiveGraph->inputGraphDL = GraphDL( positiveGraph, positiveGraph->graph );

		if ( supervised ) {		// calculate DL for negative graph
			negativeGraph->inputGraphDL = GraphDL( negativeGraph, negativeGraph->graph );
		}
		
		PrintParameters( positiveGraph );
		
		// if there are predefined substructures
		if ( predefSubsFileName != NULL )
		{
			/* find instances of predefined substructures */
			predefSubs = ReadPredefSubs( positiveGraph, predefSubsFileName );
			CreateGraphTemplate( positiveGraph );

			// discovery
			bestSubs = DiscoverPredefSubs( positiveGraph, predefSubs );	

			Free( predefSubsFileName );
			predefSubsFileName = NULL;
		}
		else
		{
			// if there are no predefined substructures
			CreateGraphTemplate( positiveGraph );

			if ( supervised ) {								// supervised version
				CreateGraphTemplate( negativeGraph );
			}
			
			//////////////////////////////
			// run the Subdue algorithm //
			//////////////////////////////
			bestSubs = Subdue( positiveGraph ); 
		}
		
#ifdef _TIMING_
#ifdef _UNIX_
	end = times( &tmsEnd );                                /* get system time */
	ASSERT( end > 0 );
	printf( "\nTime taken for discovery : \n" );
	printf( "\tReal time elapsed = %7.3f sec\n", 
		( end - start ) / (double) positiveGraph->clockTick );
	printf( "\tSystem CPU time   = %7.3f sec\n", 
		( tmsEnd.tms_stime - tmsStart.tms_stime ) / (double) positiveGraph->clockTick );
	printf( "\tUser CPU time     = %7.3f sec\n", 
		( tmsEnd.tms_utime - tmsStart.tms_utime ) / (double) positiveGraph->clockTick );
#else // WIN32
	end = clock();
	printf( "\nTime taken for discovery : \n" );
	printf( "\tCPU time used by SUBDUE = %7.3f sec\n\n", 
		( end - start ) / (double) positiveGraph->clockTick );
#endif
#endif
		
		// Evaluate discovery

		if ( SubsListLength( bestSubs ) == 0 )
		{
			// We're done.  Deallocate graphs and bail out.
			printf( "\nNo substructures to replace.\n" );

			DestroyGraph( positiveGraph->graph );
			if ( supervised ) {								// for supervised
				DestroyGraph( negativeGraph->graph );		// destroy negative graph
			}
			DestroySubsList( bestSubs );
			break;											// through with discovery
		}
		else
		{
			// Print substructures in this iteration
			if (positiveGraph->outputLevel >= OL_BEST_SUBS)
				printf( "\n\nBest substructures:\n" );
			PrintSubsList( positiveGraph, bestSubs );
			
			InitSubsList( bestSubs );						// get head pointer of list
			bestSub = GetNextSub( bestSubs );				// get best sub
			
			if ( (positiveGraph->subNumber < positiveGraph->numberOfCompressionLevels) ||
				 (positiveGraph->cluster && positiveGraph->numberOfCompressionLevels == 0) )
			{ 
				currentGraphDL = bestSub->inputGraphCompressedWithSubDL;
				compressionRatio = currentGraphDL / positiveGraph->inputGraphDL;

				if ( (positiveGraph->exhaust && IsGraphExhausted( positiveGraph )) ||	// if exhaust and graph is exhausted
					 (!positiveGraph->exhaust &&					// or if not exhaust
					 (compressionRatio > 1.0  ||					// and best sub cannot compress
					 !AcceptableSize( positiveGraph, bestSub ))) )	// or best sub is out of range
				{
					// We're done.  Deallocate graphs and bail out.
					if ( compressionRatio > 1.0 )
						printf( "The best substructure was unable to compress the graph.\n" );
					else
						printf( "Size limitation has been exceeded.\n" );
					printf( "Discovery halted.\n\n" );
					DestroyGraph( positiveGraph->graph );
					if ( supervised ) {
						DestroyGraph( negativeGraph->graph );
					}
					DestroySubsList( bestSubs );
					break;									// through with discovery
				}
				else
				{
					if (!switchedShape) {
						if ( positiveGraph->exhaust && (compressionRatio > 1.0) ) {
							switchedShape = TRUE;
						}
					}
					
					//** SAVE BEST SUB HERE INTO CLASSIFICATION LATTICE **//
					if (positiveGraph->cluster)	{			// if clustering
						CL_Insert(positiveGraph, classLattice, bestSub, switchedShape);
						// save the classification lattice
/*						if (positiveGraph->saveCL && 
							(positiveGraph->subNumber % positiveGraph->saveCL) == 0)
						{
							positiveGraph->clusterFile = fopen(positiveGraph->clusterFileName, "w+");

							fprintf(positiveGraph->clusterFile, "%s G {\n", ATT_GRAPH_TYPE);
							fprintf(positiveGraph->clusterFile, "\tnode [style=filled];\n");
							
							CL_Print(positiveGraph, classLattice, 0, FALSE);// print lattice
							fprintf(positiveGraph->clusterFile, "}\n");		// close graph body
							fclose(positiveGraph->clusterFile);
							
						}
*/					}
							
					// Compressing graph
					positiveGraph->finalGraph = TRUE;
					compressedGraph = CompressUsing( positiveGraph, bestSub );
					positiveGraph->finalGraph = FALSE;
					if (positiveGraph->outputLevel >= OL_BEST_SUBS)
						printf( "\nGraph is compressed using best substructure.\n" );

					// deallocating graph
					DestroyGraphTemplate( positiveGraph );
					DestroyGraph( positiveGraph->graph );
					positiveGraph->graph = compressedGraph;
					
					if ( supervised )						// for supervised version
					{
						negativeSub = GetNegativeSub( &negativeGraph->negativeSubsList,
							(ULONG) bestSub );
						if ( negativeSub == NULL ) {
							printf( "ERROR: negativeSub for compression not found!\n" );
						}

						if ( negativeSub != NULL && negativeSub->sub != NULL )
						{
							negativeGraph->finalGraph = TRUE;
							compressedGraph = CompressUsing( negativeGraph, negativeSub->sub);
							negativeGraph->finalGraph = FALSE;
							DestroyGraph( negativeGraph->graph );
							negativeGraph->graph = compressedGraph;
						}

						negativeGraph->firstSetOfSubs = TRUE;
						DestroyNegativeSub( negativeSub );
						DestroyNegativeSubsList( &negativeGraph->negativeSubsList );
						DestroyGraphTemplate( negativeGraph );
					}

					UpdateLabels(positiveGraph);
					DestroySubsList( bestSubs );
				}
			}
			else
			{
				// We're done.  Deallocate and bail out.
//				CL_Finish(positiveGraph, classLattice);			// take care of lattice

				DestroyGraphTemplate( positiveGraph );
				DestroyGraph( positiveGraph->graph );

				if ( supervised ) {						// for supervised
					DestroyGraphTemplate( negativeGraph );
					DestroyGraph( negativeGraph->graph );
					DestroyNegativeSubsList( &negativeGraph->negativeSubsList );
					DestroyLabelList( negativeGraph );
				}
				DestroySubsList( bestSubs );
				DestroyLabelList( positiveGraph );
				break;
			}
		}

		positiveGraph->subNumber++;

		if ( supervised ) {
			negativeGraph->subNumber++;
		}
	} 

        if(positiveGraph->display)
          StopServer(positiveGraph);
	CL_Finish(positiveGraph, classLattice);			// take care of lattice
	ClosePlotFile(positiveGraph);
#ifdef _TIMING_
#ifdef WIN32
	endTotal = clock();
	printf( "\nTotal CPU time used by SUBDUE : %7.3f sec\n", 
		( endTotal - startTotal ) / (double) positiveGraph->clockTick );
#endif
#endif


  return 0;										// Exit Subdue
}
#endif /* _PVM_SUBDUE_ */

/*******************************************************************************
FUNCTION NAME: ParseCommandLine
INPUTS:		int argc, 
			char *argv[], 
			BOOLEAN *supervised, 
			char **predefSubsFileName
RETURNS:	none
PURPOSE:	Read Subdue parameters from the command line.
*******************************************************************************/

void ParseCommandLine( PGRAPH_VARIABLES GV, int argc, char *argv[], 
					  BOOLEAN *supervised, char **predefSubsFileName )
{
	char tempStr[STR_BUFFER_LENGTH];
	BOOLEAN iterationsSet;
	BOOLEAN outputLevelSet;

#ifdef DEBUG_TRACE
	printf( "%s: ParseCommandLine()\n", __FILE__ );
#endif
	
	iterationsSet = FALSE;
	outputLevelSet = FALSE;

	while ( ( --argc > 0 ) && ( (*++argv)[0] == '-' ) )
	{

#ifdef _PVM_SUBDUE_
		if ( strcmp( *argv, CLA_NPROC ) == 0 )									// nproc (PAR)
		{
			argv++;
			argc--;
			GV->numberOfProcs = ParseInt(*argv, CLA_NPROC, 0, 0);
			if ( GV->numberOfProcs < 2 ) {
				printf( "Subdue: %s must be at least 2.\n", CLA_NPROC );
				argc = 0;
			}		
		}
#else
		if ( strcmp( *argv, CLA_NPROC ) == 0 )									// nproc (SER)
		{
			printf("Subdue: This executable was built as the serial Subdue version;\n" );
			printf("        Must build the parallel version to use %s argument\n", CLA_NPROC);
			argc = 0;
		}
#endif
		else if ( strcmp( *argv, CLA_LIMIT ) == 0 )								// limit
		{
			argv++;
			argc--;
			GV->limitOfProcessedSubs = (ULONG) ParseInt(*argv, CLA_LIMIT, 0, 0);
		}
		else if ( strcmp( *argv, CLA_ITERATIONS ) == 0 )						// iterations
		{
			argv++;
			argc--;
			GV->numberOfCompressionLevels = (ULONG)ParseInt(*argv, CLA_ITERATIONS, 0, 0);
			iterationsSet = TRUE;
		}
		else if ( strcmp( *argv, CLA_NSUBS ) == 0 )								// nsubs
		{
			argv++;
			argc--;
			GV->maxNumberOfBestSubs = (ULONG)ParseInt(*argv, CLA_NSUBS, 0, 0);
		}
		else if ( strcmp( *argv, CLA_THRESHOLD ) == 0 )							// threshold
		{
			argv++; 
			argc--;
			GV->matchCostMaxPercent = ParseFloat(*argv, CLA_THRESHOLD, 0, 1);
		}
		else if ( strcmp( *argv, CLA_BEAM ) == 0 )								// beam
		{
			argv++;
			argc--;
			GV->beamLength = (ULONG) ParseInt(*argv, CLA_BEAM, 0, 0);
		}
/*		else if ( strcmp( *argv, CLA_SUB_BEAM ) == 0 )							// subBeam
		{
			argv++;
			argc--;
			GV->subBeamLength = (ULONG) ParseInt(*argv, CLA_SUB_BEAM, 0, 0);
		}
*/		else if ( strcmp( *argv, CLA_PRUNE ) == 0 )								// prune
		{
			GV->prune = TRUE;
		}
		else if ( strcmp( *argv, CLA_NO_MIN_ENCODE ) == 0 )						// nominencode
		{
			printf("\nOption -nominencode has been deprecated!\n");
			Usage();
			//GV->minEncode = FALSE;
		}
		else if ( strcmp( *argv, CLA_OVERLAP ) == 0 )							// overlap
		{
			GV->allowInstanceOverlap = TRUE;
		}
		else if ( strcmp( *argv, CLA_UNDIRECT ) == 0 )							// undirect
		{
			GV->edge_e_Directed = FALSE;
		}
		else if ( strcmp( *argv, CLA_SUPERVISED ) == 0 )						// supervised
		{
			*supervised = TRUE;
		}
		else if ( strcmp( *argv, CLA_SCRATCH ) == 0 )							// scratch
		{
			GV->fromScratch = TRUE;
		}
		else if ( strcmp( *argv, CLA_ALT_GROUPS ) == 0 )						// alt
		{
			GV->alternativeGroups = TRUE;
		}
		else if ( strcmp( *argv, CLA_EVAL_TYPE ) == 0 )							// evaluation type
		{
			GV->evalType = E_OLD;
		}
		else if ( strcmp( *argv, CLA_PRUNE2 ) == 0 )							// prune2
		{
			GV->prune2 = TRUE;
			argv++;
			argc--;
			GV->prune2Value = (ULONG) ParseInt(*argv, CLA_PRUNE2, 0, 0);
		}
		else if ( strcmp( *argv, CLA_CLUSTER ) == 0 )							// cluster
		{
			GV->cluster = TRUE;
			if (!iterationsSet)
				GV->maxNumberOfBestSubs = (ULONG)1;				// keep 1 sub per iteration
			if (!outputLevelSet)
				GV->outputLevel = OL_MINIMAL;					// minimal output by default
			if (!GV->prune2) {
				GV->prune2 = TRUE;								// prune2 2
				GV->prune2Value = 2;
			}

			// check if file can be opened
			sprintf(GV->clusterFileName, "%s.dot", argv[argc - 1]);
			GV->clusterFile = fopen(GV->clusterFileName, "w+");
			if (GV->clusterFile == NULL) {
				ErrorFatal("Error opening cluster (filename.dot) file for writing.");
			}
			fclose(GV->clusterFile);		// close file

		}
		else if ( strcmp( *argv, CLA_TRUE_LABEL ) == 0 )						// truelabel
		{
			if (GV->cluster)												// only if clustering
				GV->trueLabel = TRUE;
			else
				ErrorFatal("Option '-truelabel' is valid only with '-cluster' option.");
		}
		else if ( strcmp( *argv, CLA_EXHAUST ) == 0 )							// exhaust
		{
			if (GV->cluster)												// only if clustering
				GV->exhaust = TRUE;
			else
				ErrorFatal("Option '-exhaust' is valid only with '-cluster' option.");
		}
		else if ( strcmp( *argv, CLA_CONNECTIVITY ) == 0 )						// con
		{
			argv++; 
			argc--;
			GV->connectivityPower = ParseFloat(*argv, CLA_CONNECTIVITY, 0, 0);
		}
		else if ( strcmp( *argv,CLA_COMPACTNESS ) == 0 )						// com
		{
			argv++; 
			argc--;
			GV->compactnessPower = ParseFloat(*argv, CLA_COMPACTNESS, 0, 0);
		}
		else if ( strcmp( *argv, CLA_COVERAGE ) == 0 )							// cov
		{
			argv++; 
			argc--;
			GV->coveragePower = ParseFloat(*argv, CLA_COVERAGE, 0, 0);
		}
		else if ( strcmp( *argv, CLA_SIZE ) == 0 )								// size
		{

			GV->limitedSize = TRUE;
			argv++; 
			argc--;
			GV->minSize = (ULONG)ParseInt(*argv, CLA_SIZE, 0, 0);
			argv++; 
			argc--;
			GV->maxSize = (ULONG)ParseInt(*argv, CLA_SIZE, 0, 0);

			if ( GV->minSize < 0 || GV->maxSize <= 0 ) {
				ErrorFatal( "Minimum size has to be >=0, Maximum size has to be >0\n" );
			}

			if ( GV->minSize > GV->maxSize ) {
				ErrorFatal( "Minimum size should be smaller than maximum size.\n" );
			}
		}
		else if ( strcmp( *argv, CLA_OUTPUT ) == 0 )							// output
		{
			argv++;
			argc--;
			GV->outputLevel = (ULONG)ParseInt(*argv, CLA_OUTPUT, MIN_OUTPUT_LEVEL, MAX_OUTPUT_LEVEL);
			outputLevelSet = TRUE;
		}
		else if ( strcmp( *argv, CLA_PREDEF_SUBS ) == 0 )						// ps
		{
			argv++;
			argc--;
			*predefSubsFileName = (char *) Malloc( sizeof( char ) *	MAX_TOKEN_LENGTH ); 
			ParseString(*argv, CLA_PREDEF_SUBS, *predefSubsFileName);
		}
		else if ( strcmp( *argv, CLA_POSITIVE_EXAMPLES ) == 0 )					// numpos
		{
			argv++;
			argc--;
			GV->numPosExamples = (ULONG) ParseInt(*argv, CLA_POSITIVE_EXAMPLES, 0, 0);
			if ( GV->numPosExamples < 0 )
				{
					printf( "Subdue: %S option is %d; should be non-negative.\n", 
						CLA_POSITIVE_EXAMPLES, GV->numPosExamples );
					argc = 0;
				}		
		}
		else if ( strcmp( *argv, CLA_POS_EX_RATIO ) == 0 )						// minpercent
		{
			argv++;
			argc--;
			GV->minPercent = ParseFloat(*argv, CLA_POS_EX_RATIO, 0, 0);
		}
		else if ( strcmp( *argv, CLA_NEG_WEIGHT ) == 0 )					// negweight
		{
			argv++;
			argc--;
			GV->negativeWeight = ParseFloat(*argv, CLA_NEG_WEIGHT, 0, 0);	
		}
		else if ( strcmp( *argv, CLA_PLOT ) == 0 )							// plot
		{
			argv++;
			argc--;
			ParseString(*argv, CLA_PLOT, tempStr);
			GV->plotFile = fopen(tempStr, "w");
			if (GV->plotFile == NULL) {
				ErrorFatal("Error opening plot file for writing.");
			}
			fprintf(GV->plotFile, "Iteration \tSub \tVertices \tDL \tCompression");	// plot info
#ifdef _TIMING_
			fprintf(GV->plotFile, " \textendInstancesTm \tgetBestSubsTm \tgetStrongClassesTm");
			fprintf(GV->plotFile, " \tgetBestTemplatesTm \taddENoiseTm \taddVENoiseTm");
			fprintf(GV->plotFile, " \tfuzzyMatchTm");		// plot info
#endif
			fprintf(GV->plotFile, " \n");	// plot info
		}
                // for graphics display
                else if (strcmp ( *argv, CLA_DISPLAY) == 0)
                {
//                  argv++;
//                  argc--;
//                  GV->sockVar->portNo = (ULONG) ParseInt(*argv,CLA_DISPLAY,0,0);  
//                    GV->sockVar->portNo = 5872;
                    GV->display = TRUE;
                  

                }
		else
		{
			printf( "Subdue: illegal option %s\n", *argv );					//  illegal option
			argc = 0;
			break;
		}
	}

#ifdef _PVM_SUBDUE_
	if (GV->numberOfProcs < 2) {
		printf("\nPVMSubdue has to be explicitly specified the number of processes\n");
		printf("          by using the %s option.\n", CLA_NPROC);
		argc = 0;
	}
#endif

	if ( argc != 1 ) {
		Usage();									// print usage
	}

	if (GV->cluster && !iterationsSet) {		// for clustering: if iterations not set
		GV->numberOfCompressionLevels = 0;			// set it to 0
	}
}


/*******************************************************************************
FUNCTION NAME: ParseInt
INPUTS:		char *argument, 
			char *option, 
			INT min, 
			INT max
OUTPUTS:	none
RETURNS:	INT
PURPOSE:	Parses an integer argument to a command line parameter
CALLED BY:	main.c: ParseCommandLine()
*******************************************************************************/
INT ParseInt(char *argument, char *option, INT min, INT max )
{
	INT number;

#ifdef DEBUG_TRACE
	printf( "%s: ParseInt()\n", __FILE__ );
#endif

	if ( sscanf(argument,"%d",&number ) == 0 )
	{
		printf( "Subdue: Bad argument %s to %s option.\n", argument, option );
		Usage();									// print usage
	}

	if (min != 0 || max != 0) {
		if (number < min || number > max) {
			printf( "Subdue: Bad argument %s to %s option.\n", argument, option );
			printf( "        Argument should be between %d and %d.\n", min, max );
			Usage();
		}
	}

	return number;
}

/*******************************************************************************
FUNCTION NAME: ParseFloat
INPUTS:		char *argument, 
			char *option, 
			float min, 
			float max
OUTPUTS:	none
RETURNS:	float
PURPOSE:	Parses a float argument to a command line parameter
CALLED BY:	main.c: ParseCommandLine()
*******************************************************************************/
float ParseFloat(char *argument, char *option, float min, float max )
{
	float number;

#ifdef DEBUG_TRACE
	printf( "%s: ParseFloat()\n", __FILE__ );
#endif

	if ( sscanf(argument,"%f",&number ) == 0 )
	{
		printf( "Subdue: Bad argument %s to %s option.\n", argument, option );
		Usage();									// print usage
	}

	if (min != 0 || max != 0) {
		if (number < min || number > max) {
			printf( "Subdue: Bad argument %s to %s option.\n", argument, option );
			printf( "        Argument should be between %f and %f.\n", min, max );
			Usage();
		}
	}

	return number;
}

/*******************************************************************************
FUNCTION NAME: ParseString
INPUTS:		char *argument, 
			char *option, 
			char *output
OUTPUTS:	none
RETURNS:	int
PURPOSE:	Parses a string argument to a command line parameter
CALLED BY:	main.c: ParseCommandLine()
*******************************************************************************/
void ParseString(char *argument, char *option, char *output )
{

#ifdef DEBUG_TRACE
	printf( "%s: ParseString()\n", __FILE__ );
#endif

	if ( sscanf( argument, "%s", output ) == 0 )
	{
		printf( "Subdue: Bad argument %s to %s option.\n", argument, option );
		Usage();
	}

}

/*******************************************************************************
FUNCTION NAME: BadArgument
INPUTS:		char *option
			char *argument
OUTPUTS:	none
RETURNS:	none
PURPOSE:	Display bad argument message.
CALLED BY:	main.c: ParseCommandLine()
*******************************************************************************/
void BadArgument(char *option, char *argument)
{

#ifdef DEBUG_TRACE
	printf( "%s: BadArgument()\n", __FILE__ );
#endif

	printf( "Subdue: bad argument %s to %s option\n", argument, option );
}


/*******************************************************************************
FUNCTION NAME: LoadDefaultGraphParameters
INPUTS:		PGRAPH_VARIABLES
OUTPUTS:	PGRAPH_VARIABLES
RETURNS:	none
PURPOSE:	Load the default graph parameters.
CALLED BY:	main.c: main()
*******************************************************************************/
void LoadDefaultGraphParameters(PGRAPH_VARIABLES graph)
{

#ifdef DEBUG_TRACE
	printf( "%s: LoadDefaultGraphParameters()\n", __FILE__ );
#endif

	graph->count = 0;                    /* initialization */
	graph->numberOfLabels = 0;
	graph->numberOfVertexLabels = 0;
	graph->numberOfEdgeLabels = 0;
	graph->numberOfGroups = 0;
	graph->groupList = NULL;
	graph->labelList = NULL;
	graph->graphTemplate = NULL;
	graph->expandTemplates = NULL;
	graph->numberOfVerticesWithLabel = NULL;

	graph->maxNumberOfBestSubs = 3;
	graph->allowInstanceOverlap = FALSE;
	graph->matchCostMaxPercent = 0;
	graph->prune = FALSE;
	graph->minEncode = TRUE;
	graph->connectivityPower = 0.0;
	graph->coveragePower = 0.0;
	graph->compactnessPower = 0.0;
	graph->beamLength = DEFAULT_BEAM_LENGTH;
//	graph->subBeamLength = 1;
	graph->edge_e_Directed = TRUE;
	graph->finalGraph = FALSE;
	graph->numberOfCompressionLevels = 1;
	graph->alternativeGroups = FALSE;
	graph->limitedSize = FALSE;
	graph->limitOfProcessedSubs = 0;
	graph->outputLevel = DEFAULT_OUTPUT_LEVEL;
	graph->numberOfPredefSubs = 0;
	graph->negativeSubsList = NULL;
	graph->fromScratch = FALSE;
	graph->numPosExamples = 0;
	graph->minPercent = 0.0;
	graph->subNumber = 1;
	graph->negativeWeight = 1.0;
	graph->evalType = E_NEW;

	graph->plotFile = NULL;
	graph->cluster = FALSE;
	graph->clusterFile = NULL;
	graph->trueLabel = FALSE;
	graph->exhaust = FALSE;
	graph->saveCL = 0;
	graph->lgOfFact = NULL;
	graph->lgOfFactTableSize = 0;

	graph->prune2 = FALSE;
	graph->prune2Value = 0;
       
        graph->display = FALSE;    // for graphics display set to true;
                                   // default: set to FALSE
#ifdef _PVM_SUBDUE_
	graph->localValueOfSubAtProc = NULL;
	graph->numberOfProcs = 0;				// number of PVM processes (must be specified by user)
											// use -nproc # argument to specify more proc's
	graph->bestAbstractSubAtProc = NULL;
	graph->bestSubAtProc = NULL;
#endif

#ifdef _TIMING_
	graph->fuzzyMatchTm = 0.0;
	graph->extendInstancesTm = 0.0;
	graph->getStrongClassesTm = 0.0;
	graph->getBestTemplatesTm = 0.0;
	graph->addENoiseTm = 0.0;
	graph->addVENoiseTm = 0.0;
	graph->getBestSubsTm = 0.0;
#ifdef _UNIX_
	graph->clockTick = sysconf( _SC_CLK_TCK );               /* get system clock ticks */
	ASSERT( graph->clockTick > 0 );                                      /* per second */
#else // WIN32
	graph->clockTick = CLOCKS_PER_SEC;
	ASSERT( graph->clockTick > 0 );                                      /* per second */
#endif
#endif

}

/*******************************************************************************
FUNCTION NAME: Usage
INPUTS:		none 
RETURNS:	none
PURPOSE:	Print out command syntax and terminate program.
CALLED BY:	main.c: main()
*******************************************************************************/
void Usage( void )
{
	
#ifdef DEBUG_TRACE
	printf( "%s: Usage()\n", __FILE__ );
#endif

	printf("\n");								// print new line before usage

#ifdef _PVM_SUBDUE_
	printf("Usage: PVMSubdue ");
#else
	printf("Usage: Subdue ");
#endif
	printf("[%s subCount] [%s #] [%s #]\n", CLA_LIMIT, CLA_ITERATIONS, CLA_THRESHOLD);
	printf("              [%s] [%s] [%s #] [%s]\n", CLA_NSUBS, CLA_PRUNE, CLA_BEAM, CLA_OVERLAP);
	printf("              [%s] [%s connectivity] [%s compactness]\n", CLA_UNDIRECT, CLA_CONNECTIVITY, CLA_COMPACTNESS);
	printf("              [%s coverage] [%s] [%s lowerBound upperBound]\n", CLA_COVERAGE, CLA_ALT_GROUPS, CLA_SIZE);
	printf("              [%s processCount] [%s filename ] [%s level]\n", CLA_NPROC, CLA_PREDEF_SUBS, CLA_OUTPUT);
	printf("              [%s numpos] [%s percent] [%s] \n", CLA_POSITIVE_EXAMPLES, CLA_POS_EX_RATIO, CLA_SUPERVISED);
	printf("              [%s] [%s weight] [%s plotFile]\n", CLA_SCRATCH, CLA_NEG_WEIGHT, CLA_PLOT);
	printf("              [%s] [%s] [%s] [%s #]\n", CLA_CLUSTER, CLA_TRUE_LABEL, CLA_EXHAUST, CLA_PRUNE2);
	printf("              [%s]\n", CLA_EVAL_TYPE);
	printf("              graphfile\n");

	ErrorFatal("");
}

/*******************************************************************************
FUNCTION NAME: ClosePlotFile
INPUTS:		PGRAPH_VARIABLES graph
RETURNS:	none
PURPOSE:	Close the plot file, if any.
CALLED BY:	main.c: main()
*******************************************************************************/
void ClosePlotFile(PGRAPH_VARIABLES graph)
{

#ifdef DEBUG_TRACE
	printf( "%s: ClosePlotFile()\n", __FILE__ );
#endif

	if (graph->plotFile) {								// if there is a plot file
		fclose(graph->plotFile);						// close it
	}

}


/*******************************************************************************
FUNCTION NAME: CreateNegativeGraph
INPUTS:		PGRAPH_VARIABLES positiveGraph
RETURNS:	PGRAPH_VARIABLES (the newly created negative graph)
PURPOSE:	Create negative graph and copy the graph variables of positiveGraph
			to negativeGraph.
CALLED BY:	main.c: main()
*******************************************************************************/

PGRAPH_VARIABLES CreateNegativeGraph( PGRAPH_VARIABLES positiveGraph)
{
	PGRAPH_VARIABLES newNegativeGraph;

#ifdef DEBUG_TRACE
	printf( "%s: CreateNegativeGraph()\n", __FILE__ );
#endif
	
	newNegativeGraph = (PGRAPH_VARIABLES) Malloc( sizeof( GRAPH_VARIABLES ) );	// allocate
	memcpy( newNegativeGraph, positiveGraph, sizeof( GRAPH_VARIABLES ) );		// copyy
	newNegativeGraph->negativeGraph = NULL;									// negative has no negative
	return newNegativeGraph;
}


/*******************************************************************************
FUNCTION NAME: PrintParameters
INPUTS:		PGRAPH_VARIABLES GV
RETURNS:	none
PURPOSE:	Print the parameters of a graph
CALLED BY: main.c: main()
*******************************************************************************/

void PrintParameters( PGRAPH_VARIABLES GV )
{
	
#ifdef DEBUG_TRACE
	printf( "%s: PrintParameters()\n", __FILE__ );
#endif
	
	if ( GV->subNumber == 1 )					// print only for the first iteration
	{
		printf( "Subdue Parameters:\n" );
#ifdef _PVM_SUBDUE_
		printf( "\tProcessors         =   %d\n", GV->numberOfProcs );
#endif
		printf( "\tLimit              =   " );
		if (GV->prune2)
			printf( "No limit\n");
		else 
			printf( "%lu\n", GV->limitOfProcessedSubs );

		printf( "\tIterations         =   ");
		if (!GV->cluster || GV->numberOfCompressionLevels > 0)
			printf( "%lu\n", GV->numberOfCompressionLevels );
		else 
			printf( "As needed\n");

		printf( "\tThreshold          =   %4.2f\n", GV->matchCostMaxPercent );
		printf( "\tNsubs              =   %lu\n",    GV->maxNumberOfBestSubs );
		printf( "\tBeam Length        =   %lu\n",    GV->beamLength );
		printf( "\tConnectivity Power =   %4.2f\n", GV->connectivityPower );
		printf( "\tCoverage Power     =   %4.2f\n", GV->coveragePower );
		printf( "\tCompactness Power  =   %4.2f\n", GV->compactnessPower );
		if ( GV->limitedSize )
		{
			printf( "\tMinimum size       =   %lu\n", GV->minSize );
			printf( "\tMaximum size       =   %lu\n", GV->maxSize );
		}
		if ( GV->numPosExamples )
		{
			printf( "\tPositive examples  =   %d\n", GV->numPosExamples );
			printf( "\tMinPercentage      =   %4.2f\n", GV->minPercent );
		}
		if ( GV->negativeGraph )
			printf( "\tNegative Weight    =   %4.2f\n", GV->negativeWeight );
		if ( GV->prune ) 
			printf( "\tPruning\n" ); 
		if ( GV->prune2 ) 
			printf( "\tPrune2 %d\n",  GV->prune2Value); 
		if ( GV->minEncode )
			printf( "\tMinimum encoding\n" );
		else
			printf( "\tNo minimum encoding\n" );
		if ( GV->allowInstanceOverlap ) 
			printf( "\tOverlap allowed\n");
		// Cluster analysis settings
		if ( GV->cluster ) 
			printf( "\tCluster analysis\n");
		if ( GV->exhaust ) 
			printf( "\t\tExhaustive analysis\n");
		if ( GV->trueLabel ) 
			printf( "\t\tTrue labels\n");
	}

	if ( GV->outputLevel == OL_MINIMAL)			// for minmal output, this is not printed
		return;

																// Print for all iterations
	if ( GV->negativeGraph )
		printf( "\nPositive graph Statistics :\n" );
	else
		printf( "\nInput graph Statistics :\n" );

	printf( "\tNumber of vertices        =   %lu\n", GV->graph->numberOfVertices );
	printf( "\tNumber of edges           =   %lu\n", GV->graph->numberOfEdges );
	printf( "\tInput graph DL            =   %f\n", GV->inputGraphDL );	
	printf( "\tNumber of vertex labels   =   %lu\n", GV->numberOfVertexLabels );
	printf( "\tNumber of edge labels     =   %lu\n\n", GV->numberOfEdgeLabels );
	
	if ( GV->negativeGraph )								// Negative graph stats
	{
		printf( "Negative graph Statistics :\n" );
		printf( "\tNumber of vertices        =   %lu\n", GV->negativeGraph->graph->numberOfVertices );
		printf( "\tNumber of edges           =   %lu\n", GV->negativeGraph->graph->numberOfEdges );
		printf( "\tInput graph DL            =   %f\n", GV->negativeGraph->inputGraphDL );
		printf( "\tNumber of vertex labels   =   %lu\n", GV->negativeGraph->numberOfVertexLabels );
		printf( "\tNumber of edge labels     =   %lu\n\n", GV->negativeGraph->numberOfEdgeLabels );
	}

	printf( "\n" );
	fflush( stdout );
}

/*******************************************************************************
FUNCTION NAME: PrintTimings
INPUTS:		PGRAPH_VARIABLES GV
RETURNS:	none
PURPOSE:	Print the timing measurements
CALLED BY: Discover(), Subdue()
*******************************************************************************/

void PrintTimings( PGRAPH_VARIABLES GV )
{
	
#ifdef DEBUG_TRACE
	printf( "%s: PrintTimings()\n", __FILE__ );
#endif

#ifdef _TIMING_
	printf( "\nExtendInstances():  %10.4f sec\n", GV->extendInstancesTm );
	printf( "GetBestSubs():      %10.4f sec\n", GV->getBestSubsTm );
	printf( "GetStrongClasses(): %10.4f sec\n", GV->getStrongClassesTm );
	printf( "GetBestTemplates(): %10.4f sec\n", GV->getBestTemplatesTm );
	printf( "AddENoise():        %10.4f sec\n", GV->addENoiseTm );
	printf( "AddVENoise():       %10.4f sec\n", GV->addVENoiseTm );
	printf( "FuzzyMatch():       %10.4f sec\n\n", GV->fuzzyMatchTm );
	fflush( stdout );
	
	GV->fuzzyMatchTm = 0.0;
	GV->extendInstancesTm = 0.0;
	GV->getStrongClassesTm = 0.0;
	GV->getBestTemplatesTm = 0.0;
	GV->addENoiseTm = 0.0;
	GV->addVENoiseTm = 0.0;
	GV->getBestSubsTm = 0.0;
#endif

}

/*******************************************************************************
FUNCTION NAME: _Assert
INPUTS:		char *file, 
			unsigned line
RETURNS:	none
PURPOSE:	If DEBUG is defined, this function will be called to terminate 
			execution when the expresssion in the ASSERT() macro call is false.
*******************************************************************************/

void _Assert( char *file, unsigned line )
{
	fflush( NULL );
	fprintf( stderr, "\nAssertion failed: %s, line %u\n", file, line );
	fflush( stderr );
	ErrorFatal("");
	//exit( 2 );
}


