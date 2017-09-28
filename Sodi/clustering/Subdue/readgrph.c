/********************************************************************
*
* SUBDUE
*
* FILE NAME: readgrph.c
*
********************************************************************/



#include "subdue.h"


/*******************************************************************************
FUNCTION NAME: ErrorFatal
INPUTS:		char *message
RETURNS:	none
PURPOSE:	Print fatal error message and terminate the program
CALLED BY:	main.c: main()
			subsop.c: CreateSubsList()
			... and many others
*******************************************************************************/

void ErrorFatal( char *message )
{

#ifdef DEBUG_TRACE
	printf( "%s: ErrorFatal()\n", __FILE__ );
#endif

	printf("\n%s\n", message );

#ifdef _PVM_SUBDUE_
	pvm_exit();							// for PVM version, exit PVM too.
#endif // _PVM_SUBDUE_

	exit( EXIT_FAILURE );
}


/*******************************************************************************
FUNCTION NAME: ReadToken
INPUTS:		char *line, 
			ULONG *lineIndex, 
			char *str
RETURNS:	none
PURPOSE:	Read a token from line into a string, starting at line[lineIndex].
CALLED BY:	readgrph.c: ReadGlobalGraph()
*******************************************************************************/

void ReadToken( char *line, ULONG *lineIndex, char *str )
{
	char ch;
	ULONG strIndex = 0;
	
#ifdef DEBUG_TRACE
	printf( "%s: ReadToken()\n", __FILE__ );
#endif
	
	ch = line[(*lineIndex)++];           /* skip over whitespace and comments */
	while ( ( ch == ' ' ) || ( ch == '\n' ) || ( ch == '\t' ) ||
		( ch == COMMENT ) )
	{
		if ( ch == COMMENT || ch == '\n' )    /* end of line has been reached */
		{
			str[0] = '\0';
			return;
		}
		ch = line[(*lineIndex)++];
	}

	while ( ( ch != ' ' ) && ( ch != '\n' ) && ( ch != '\t' ) &&
		( ch != COMMENT ) )
	{
		str[strIndex++] = ch;
		ch = line[(*lineIndex)++];
	}

	str[strIndex] = '\0';
	return;
}

/*******************************************************************************
FUNCTION NAME: ReadNumber
INPUTS:		char *line, 
			ULONG *lineIndex
RETURNS:	ULONG
PURPOSE:	Try to read an unsigned integer from inputStream; exit if failure.
CALLED BY:	readgrph.c: ReadVertexData()
			readgrph.c:	ReadEdgeData()
*******************************************************************************/

ULONG ReadNumber( char *line, ULONG *lineIndex )
{
	char str[MAX_TOKEN_LENGTH];
	ULONG number;
	
#ifdef DEBUG_TRACE
	printf( "%s: ReadNumber()\n", __FILE__ );
#endif
	
	ReadToken( line, lineIndex, str );
	if ( str[0] == '\0' )
	{
		printf( "Error in line:\n%s\n", line );
		printf( "Expected to read an unsigned integer\n" );
		ErrorFatal("Exiting Subdue...");
	}

	if ( sscanf( str, "%lu", &number ) != 1 || number == 0 )
	{
		printf( "On line:\n%s\n", line );
		printf( "Read error: %s should be an unsigned integer > 0.\n", str );
		ErrorFatal("Exiting Subdue...");
	}

	return( number );
}


/*******************************************************************************
FUNCTION NAME: ReadVertexData
INPUTS:		char *line, ULONG *lineIndex, ULONG *ID, ULONG correctID
RETURNS:	none
PURPOSE:	Read vertex information from the input stream
CALLED BY:	readgrph.c: ReadGlobalGraph()
*******************************************************************************/

void ReadVertexData( char *line, ULONG *lineIndex, ULONG *ID,
					ULONG correctID )
{
	
#ifdef DEBUG_TRACE
	printf( "%s: ReadVertexData()\n", __FILE__ );
#endif
	
	*ID = ReadNumber( line, lineIndex );
	if ( *ID != correctID )
	{
		printf( "Error in input file:\n" );
		printf( "\n%s\nVertex ID %lu should be %lu\n", line, *ID, correctID );
		ErrorFatal( " Error parsing vertex " );
	} 
	return;
}


/*******************************************************************************
FUNCTION NAME: ReadEdgeData
INPUTS:		char *line, 
			ULONG *lineIndex,
			ULONG *fromID,					(pointer to the source vertex id)
			ULONG *toID,					(pointer to the target vertex id)
RETURNS:	none
PURPOSE:	Read edge information from the input stream
CALLED BY:	readgrph.c: ReadGlobalGraph()
			readgrph.c: ReadPredefinedSubstructures()
*******************************************************************************/

void ReadEdgeData( char *line, ULONG *lineIndex, ULONG *fromID,
				  ULONG *toID, ULONG maxID )
{
#ifdef DEBUG_TRACE
	printf( "%s: ReadEdgeData()\n", __FILE__ );
#endif
	
	*fromID = ReadNumber( line, lineIndex );
	*toID = ReadNumber( line, lineIndex );

	if ( *fromID > maxID )
	{
		printf( "Error in input file:\n" );
		printf( "\n%s\nSource vertex ID %lu is not a defined vertex.\n", line,
			*fromID );
		ErrorFatal( " " );
	}

	if ( *toID > maxID )
	{
		printf( "Error in input file:\n" );
		printf( "\n%s\nTarget vertex ID %lu is not a defined vertex.\n", line,
			*toID );
		ErrorFatal( " " );
	}

	return;
}


/*******************************************************************************
FUNCTION NAME: ReadGroup
INPUTS:		char *line, 
			ULONG *lineIndex
RETURNS:	none
PURPOSE:	Read a group of identical string labels
*******************************************************************************/

void ReadGroup( PGRAPH_VARIABLES GV, char *line, ULONG *lineIndex )
{
	char label[MAX_TOKEN_LENGTH + 1];
	ULONG labelCounter = 0;
	ULONG groupIndex;
	ULONG labelIndex;
	
#ifdef DEBUG_TRACE
	printf( "%s: ReadGroup()\n", __FILE__ );
#endif
	
	if ( GV->alternativeGroups == FALSE ) {     // option 'alternative groups' 
												// is not specified 
		printf("WARNING: Group specification encountered, while\n");
		printf("         option '-alt' is not specified.\n");
		return;
	}
	
	groupIndex = GV->numberOfGroups;
	GV->numberOfGroups++;
	/* allocate space */
	GV->groupList = (GROUP *) Realloc(GV->groupList, GV->numberOfGroups * sizeof(GROUP));
	
	GV->groupList[groupIndex].labels = NULL;
	
	ReadToken( line, lineIndex, label );
	while ( label[0] != '\0' )
	{
		labelIndex = labelCounter;
		labelCounter++;
		
		GV->groupList[groupIndex].labels = (char **) Realloc(GV->groupList[groupIndex].labels, 
			labelCounter * sizeof( char * ) );
		
		GV->groupList[groupIndex].labels[labelIndex] = 
			(char *) Malloc( ( strlen( label ) + 1 ) * sizeof( char ) );
		strcpy( GV->groupList[groupIndex].labels[labelIndex], label );
		
		ReadToken( line, lineIndex, label );
	}
	
	GV->groupList[groupIndex].numberLabels = labelCounter; 
	return;
}


/*******************************************************************************
FUNCTION NAME: CountVertices
INPUTS:		char *fileName
RETURNS:	ULONG 
PURPOSE:	count the number of vertices in the input file
CALLED BY:	readgrph.c: ReadGlobalGraph()
*******************************************************************************/

ULONG CountVertices( char *fileName )
{
	char ch = '\n';
	ULONG numberOfVertices;
	FILE *inputStream;
	
#ifdef DEBUG_TRACE
	printf( "%s: CountVertices()\n", __FILE__ );
#endif
	
	numberOfVertices = 0;
	
	if ( ( inputStream = fopen( fileName,"r" ) ) == NULL )
	{
		printf( "Unable to open %s.\n", fileName );
		ErrorFatal("Exiting Subdue...");
	}
	
	while ( ch != EOF )
	{                                  /* go to the next line in the input file */
		while ( ( ch != '\n' ) && ( ch != EOF ) )
			ch = (char)getc( inputStream );
		if ( ch != EOF )
		{
			ch = (char)getc( inputStream );
			if ( ch == 'v' )
				numberOfVertices++;
		}
	}
	fclose( inputStream );
	return numberOfVertices ;
}


/*******************************************************************************
FUNCTION NAME: ReadGraph
INPUTS:		PGRAPH_VARIABLES GV, char *fileName
RETURNS:	none
PURPOSE:	Read graph information from input file
CALLED BY:	main.c: main()
*******************************************************************************/
void ReadGraph( PGRAPH_VARIABLES GV, char *fileName )
{
	ULONG numberOfVertices;
	char line[MAX_TOKEN_LENGTH];
	char str[MAX_TOKEN_LENGTH];
	FILE *inputStream;                 
	ULONG newVertexID;
	ULONG edgeFromID;
	ULONG edgeToID;
	ULONG labelIndex;
	ULONG lineIndex;
	
#ifdef DEBUG_TRACE
	printf( "%s: ReadGraph()\n", __FILE__ );
#endif
	
	numberOfVertices = CountVertices( fileName );		/* get number of vertices */
	CalculateLgFactN( GV, numberOfVertices );			// generate lookup table: lg n!
	GV->graph = CreateGraph( numberOfVertices );		/* create graph */
	
	if ( ( inputStream = fopen( fileName, "r" ) ) == NULL )
	{
		printf( "Unable to open graph file %s\n", fileName );
		DestroyGraph( GV->graph );
		fclose( inputStream );
		ErrorFatal("Exiting Subdue...");
	}

	printf( "Parsing graph file: %s\n\n", fileName );

	/* read graph info */
	while ( fgets( line, MAX_TOKEN_LENGTH, inputStream ) != NULL )
	{
		lineIndex = 0;
		ReadToken( line, &lineIndex, str );

		if ( str[0] != '\0' ) {						// if first token is not NULL
			switch ( str[0] ) {							// process line
						/* VERTICES */
				case 'V': ;
				case 'v': 
					ReadVertexData( line, &lineIndex, &newVertexID,
							  GV->graph->numberOfVertices + 1  );
					labelIndex = AddLabelStr( GV, &line[lineIndex], line, TRUE, FALSE );
					AddVertex( GV->graph, newVertexID, labelIndex );
					break;

					/* EDGES */
				case 'E': ;
				case 'e': 
					ReadEdgeData( line, &lineIndex, &edgeFromID, &edgeToID,
							  GV->graph->numberOfVertices );
					labelIndex = AddLabelStr( GV, &line[lineIndex], line, FALSE, FALSE );
					AddEdge( GV->graph, edgeFromID - 1, edgeToID - 1,
						labelIndex, GV->edge_e_Directed, TRUE );
					break;
					
				case 'D': ;					// explicitly directed edges
				case 'd': 
					ReadEdgeData( line, &lineIndex, &edgeFromID, &edgeToID,
							  GV->graph->numberOfVertices );
					labelIndex = AddLabelStr( GV, &line[lineIndex], line, FALSE, FALSE );
					AddEdge( GV->graph, edgeFromID - 1, edgeToID - 1,
						labelIndex, TRUE, TRUE );
					break;
					
				case 'U': ;					// explicitly undirected edges
				case 'u': 
					ReadEdgeData( line, &lineIndex, &edgeFromID, &edgeToID,
							  GV->graph->numberOfVertices );
					labelIndex = AddLabelStr( GV, &line[lineIndex], line, FALSE, FALSE );
					AddEdge( GV->graph, edgeFromID - 1, edgeToID - 1,
						labelIndex, FALSE, TRUE );
					break;
					
				case 'G': ;
				case 'g': 
					ReadGroup( GV, line, &lineIndex );
					break;
					
				default:  
					printf( "Unrecognized line in graph file %s:\n%s\n%s", fileName, line, str );
					fclose( inputStream );
					ErrorFatal("Exiting Subdue...");
			}
		}
	}

	fclose( inputStream );
}


/*******************************************************************************
FUNCTION NAME: ReadPredefSubs
INPUTS:		PGRAPH_VARIABLES GV, 
			char *fileName
RETURNS:	PLIST_OF_ABSTRACT_SUBS
PURPOSE:	Read the predefined substructures from fileName and convert each one
			into an abstractSub; insert the AbstractSubs into a list and return 
			the list.
CALLED BY:	main.c: main()
*******************************************************************************/

PLIST_OF_ABSTRACT_SUBS ReadPredefSubs( PGRAPH_VARIABLES GV, char *fileName )
{
	char line[MAX_TOKEN_LENGTH];
	char str[MAX_TOKEN_LENGTH];
	FILE *inputStream;                 
	ULONG newVertexID;
	ULONG edgeFromID;
	ULONG edgeToID;
	ULONG labelIndex;
	ULONG lineIndex;
	PLIST_OF_ABSTRACT_SUBS predefSubs;
	PABSTRACT_SUB substructure = NULL;
	
#ifdef DEBUG_TRACE
	printf( "%s: ReadPredefSubs()\n", __FILE__ );
#endif
	
	if ( ( inputStream = fopen( fileName, "r" ) ) == NULL )
	{
		printf( "Unable to open predefined substructures file %s.\n", fileName );
		fclose( inputStream );
		ErrorFatal("Exiting Subdue...");
	}
	printf( "Predefined substructures file: %s\n", fileName );
	predefSubs = CreateListOfAbstractSubs();
	/* read graph info */
	while ( fgets( line, MAX_TOKEN_LENGTH, inputStream ) != NULL )
	{
		lineIndex = 0;
		ReadToken( line, &lineIndex, str );
		if ( str[0] != '\0' )
			switch ( str[0] )
		{
	case 'S': ;
	case 's': if ( substructure != NULL )
			  {
				  /* PrintAbstractSub( substructure ); */
				  InsertInAbstractSubsList( substructure, predefSubs );
			  }
		GV->numberOfPredefSubs++;
		substructure = NewAbstractSub();
		break;
		
	case 'V': ;
	case 'v': ReadVertexData( line, &lineIndex, &newVertexID,
				  substructure->numberOfVertices + 1 );
		labelIndex = AddLabelStr( GV, &line[lineIndex], line, TRUE,
			TRUE );
		AddVertexToAbstractSub( substructure, labelIndex );
		break;
		
	case 'E': ;
	case 'e': ReadEdgeData( line, &lineIndex, &edgeFromID, &edgeToID,
				  substructure->numberOfVertices );
		labelIndex = AddLabelStr( GV, &line[lineIndex], line, FALSE,
			TRUE );
		AddEdgeToAbstractSub( substructure, edgeFromID - 1,
			edgeToID - 1, labelIndex, GV->edge_e_Directed );
		break;
		
	case 'D': ;
	case 'd': ReadEdgeData( line, &lineIndex, &edgeFromID, &edgeToID,
				  substructure->numberOfVertices );
		labelIndex = AddLabelStr( GV, &line[lineIndex], line, FALSE,
			TRUE );
		AddEdgeToAbstractSub( substructure, edgeFromID - 1,
			edgeToID - 1, labelIndex, TRUE );
		break;
		
	case 'U': ;
	case 'u': ReadEdgeData( line, &lineIndex, &edgeFromID, &edgeToID,
				  substructure->numberOfVertices );
		labelIndex = AddLabelStr( GV, &line[lineIndex], line, FALSE,
			TRUE );
		AddEdgeToAbstractSub( substructure, edgeFromID - 1,
			edgeToID - 1, labelIndex, FALSE );
		break;
		
	default:  printf( "Unrecognized line in predefined substructures file:\n%s\n%s", line, str );
		fclose( inputStream );
		ErrorFatal("Exiting Subdue...");
		}
	}
	fclose( inputStream );
	if ( substructure != NULL )
	{
		/* PrintAbstractSub( substructure ); */
		InsertInAbstractSubsList( substructure, predefSubs );
	}
	return predefSubs;
}


