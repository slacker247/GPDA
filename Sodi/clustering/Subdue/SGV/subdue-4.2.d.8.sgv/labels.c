/********************************************************************
*
* SUBDUE
*
* FILE NAME: labels.c
*
********************************************************************/



#include "subdue.h"

/*******************************************************************************
FUNCTION NAME: AddLabelStr
INPUTS:		PGRAPH_VARIABLES GV, 
			char *labelStr, char *line, 
			BOOLEAN vertexLabel, 
			BOOLEAN predefSub
RETURNS:	ULONG
PURPOSE:	Scans the contents of labelStr into a label structure and searches 
			for an identical label in GV->labelList.  Returns the index of the 
			label in GV->labelList. If not found, adds label to the end of 
			GV->labelList. 
CALLED BY:	readgrph.c: ReadGlobalGraph()
			readgrph.c: ReadPredefSubs()
*******************************************************************************/

ULONG AddLabelStr( PGRAPH_VARIABLES GV, char *labelStr, char *line, 
				  BOOLEAN vertexLabel, BOOLEAN predefSub )
{
	LABEL label;
	char labelContent[MAX_TOKEN_LENGTH];
	char labelMatch;
	char errorMsg[MAX_TOKEN_LENGTH];
	DOUBLE labelValue;
	DOUBLE matchValue;
	int count;
	ULONG labelIndex;
	ULONG index;
	ULONG limit;
        ULONG paranIndx;

#ifdef DEBUG_TRACE
	printf( "%s: AddLabelStr()\n", __FILE__ );
#endif
	count = sscanf( labelStr, "%s %c %lf", labelContent, &labelMatch, &matchValue );
	if ( LabelIsNumeric( labelContent ) )                      /* numeric label */
	{
		label.labelType = 'n';
		sscanf( labelContent, "%lf", &labelValue );
		label.content.numericValue = labelValue;
		if ( count < 3 || labelMatch == 'e' || labelMatch == 'E' )
		{                                                   /* default match type */
			label.match.matchType = M_EXACT;
			label.matchValue = 0.0;
		}
		else
		{
			switch ( labelMatch )
			{
			case 'T': ;
			case 't': label.match.matchType = M_TOLERANCE;
				break;
			case 'D': ;
			case 'd': label.match.matchType = M_DIFFERENCE;
				break;
			default : sprintf( errorMsg, "Invalid match type for label: %s\n", 
						  labelStr );
				ErrorFatal( errorMsg );
			}
			label.matchValue = matchValue;
		}
		/* search for an identical numeric label in GV->labelList */
		for ( labelIndex = 0; labelIndex < GV->numberOfLabels; labelIndex++ )
			if ( GV->labelList[labelIndex].labelType == 'n' &&
				GV->labelList[labelIndex].content.numericValue ==
				label.content.numericValue &&
				GV->labelList[labelIndex].match.matchType ==
				label.match.matchType &&
				GV->labelList[labelIndex].matchValue == label.matchValue )
			{
				if ( vertexLabel && !predefSub )
					GV->numberOfVerticesWithLabel[labelIndex]++;
				return labelIndex;
			}
	}
	else                                                        /* string label */
	{
                paranIndx = StringFind(labelStr,'(');
                if(paranIndx >= 0)
                  labelStr[paranIndx] = '\0';
		while ( labelStr[0] == ' ' || labelStr[0] == '\t' || labelStr[0] == '\n' )
		{                                            /* remove leading whitespace */
			limit = strlen( labelStr );
			for ( index = 1; index <= limit; index++ )
				labelStr[index - 1] = labelStr[index];
		}
		limit = strlen( labelStr ) - 1;
		while ( labelStr[limit] == '\n' || labelStr[limit] == ' ' ||
			labelStr[limit] == '\t' )
		{                                           /* remove trailing whitespace */
			labelStr[limit] = '\0';
			limit--;
		}
		if ( strlen( labelStr ) == 0 )
		{
			sprintf( errorMsg, "graph file error - empty label on line:\n%s\n",
				line );
			ErrorFatal( errorMsg );
		}
		for ( labelIndex = 0; labelIndex < GV->numberOfLabels; labelIndex++ )
			if ( GV->labelList[labelIndex].labelType == 's' && 
				!strcmp( GV->labelList[labelIndex].content.stringValue, labelStr ) )
			{
				if ( vertexLabel && !predefSub )
					GV->numberOfVerticesWithLabel[labelIndex]++;
				return labelIndex;
			}
			
			label.labelType = 's';                         /* make a new string label */
			label.content.stringValue = (char *) 
				Malloc( ( strlen( labelStr ) + 1 ) * sizeof( char ) );
			strcpy( label.content.stringValue, labelStr );
			label.match.group = -1;
			label.matchValue = 0.0;
	}


	GV->numberOfLabels++;         /* label not found; add to end of GV->labelList */
	GV->newLabel = TRUE;                 /* flag to call ResizeExpandTemplates() */
	
	if ( vertexLabel )
		GV->numberOfVertexLabels++;
	else
		GV->numberOfEdgeLabels++;
	
	GV->labelList = (LABEL *) Realloc( GV->labelList, GV->numberOfLabels * sizeof( LABEL ) );

	GV->labelList[GV->numberOfLabels - 1] = label;
	
	GV->numberOfVerticesWithLabel = (ULONG *) Realloc( GV->numberOfVerticesWithLabel, 
			GV->numberOfLabels * sizeof( ULONG ) );
	if ( vertexLabel && !predefSub )
		GV->numberOfVerticesWithLabel[GV->numberOfLabels - 1] = 1;
	else
		GV->numberOfVerticesWithLabel[GV->numberOfLabels - 1] = 0;
	return ( GV->numberOfLabels - 1 );
}


/*******************************************************************************
StringFind
*******************************************************************************/

ULONG StringFind(char *mainStr,char c)
{
  int i;

  for(i=0;i<strlen(mainStr);i++)
    if(mainStr[i] == c)
      return i;
  return -1;    
  
}


/*******************************************************************************
FUNCTION NAME: AddLabel
INPUTS:		PGRAPH_VARIABLES GV, 
			LABEL label, 
			BOOLEAN vertexLabel, 
			BOOLEAN predefSub
RETURNS:	ULONG
PURPOSE:	Search GV->labelList for a label identical to label.  If found, 
			return its index; if not found, add it to the end of GV->labelList
			and return its index.
CALLED BY:	pvm.c: ReceiveAbstractSub() 
*******************************************************************************/

ULONG AddLabel( PGRAPH_VARIABLES GV, LABEL label, BOOLEAN vertexLabel, 
			   BOOLEAN predefSub )
{
	ULONG labelIndex;
	
#ifdef DEBUG_TRACE
	printf( "%s: AddLabel()\n", __FILE__ );
#endif
	
	if ( label.labelType == 'n' )
	{                  /* search for an identical numeric label in GV->labelList */
		for ( labelIndex = 0; labelIndex < GV->numberOfLabels; labelIndex++ )
			if ( GV->labelList[labelIndex].labelType == 'n' &&
				GV->labelList[labelIndex].content.numericValue ==
				label.content.numericValue &&
				GV->labelList[labelIndex].match.matchType ==
				label.match.matchType &&
				GV->labelList[labelIndex].matchValue == label.matchValue )
			{
				if ( vertexLabel && !predefSub )
					GV->numberOfVerticesWithLabel[labelIndex]++;
				return labelIndex;
			}
	}
	else
	{                                      /* search for identical string label */
		for ( labelIndex = 0; labelIndex < GV->numberOfLabels; labelIndex++ )
			if ( GV->labelList[labelIndex].labelType == 's' && 
				!strcmp( GV->labelList[labelIndex].content.stringValue,
				label.content.stringValue ) )
			{
				/*Free( label.content.stringValue );*/
				if ( vertexLabel && !predefSub )
					GV->numberOfVerticesWithLabel[labelIndex]++;
				return labelIndex;
			}
	}
	/* label wasn't found; add it to the end of GV->labelList */
	GV->numberOfLabels++;
	GV->newLabel = TRUE;                 /* flag to call ResizeExpandTemplates() */
	
	if ( vertexLabel )
		GV->numberOfVertexLabels++;
	else
		GV->numberOfEdgeLabels++;
	
	GV->labelList = (LABEL *) Realloc( GV->labelList, GV->numberOfLabels * sizeof( LABEL ) );

	GV->labelList[GV->numberOfLabels - 1] = label;
	GV->numberOfVerticesWithLabel = (ULONG *) Realloc( GV->numberOfVerticesWithLabel, 
		GV->numberOfLabels * sizeof( ULONG ) );

	if ( vertexLabel && !predefSub )
		GV->numberOfVerticesWithLabel[GV->numberOfLabels - 1] = 1;
	else
		GV->numberOfVerticesWithLabel[GV->numberOfLabels - 1] = 0;
	
	return ( GV->numberOfLabels - 1 );
}


/*******************************************************************************
FUNCTION NAME: LabelIsNumeric
INPUTS:		char *labelStr
RETURNS:	BOOLEAN
PURPOSE:	Determine whether labelStr contains a numeric or string label by 
			examining each character.
CALLED BY:	labels.c: AddLabelStr()
*******************************************************************************/

BOOLEAN LabelIsNumeric( char *labelStr )
{
	LONG limit;
	LONG index;
	BOOLEAN decimalPointSeen;
	BOOLEAN digitSeen;
	BOOLEAN plusSeen;
	BOOLEAN minusSeen;
	
	limit = strlen( labelStr );
	decimalPointSeen = digitSeen = plusSeen = minusSeen = FALSE;
	
	for ( index = 0; index < limit; index++ )
	{
		switch ( labelStr[index] )
		{
		case '.': 
			if ( decimalPointSeen )		// this is the second decimal point 
				return FALSE;
			else
				decimalPointSeen = TRUE;
			break;
			
		case '-': 
			if ( index > 0 )             // only a leading '-' is allowed 
				return FALSE;
			else
				minusSeen = TRUE;
			break;
			
		case '+': 
			if ( index > 0 )             // only a leading '+' is allowed 
				return FALSE;
			else
				plusSeen = TRUE;
			break;
			
		case '0': ;
		case '1': ;
		case '2': ;
		case '3': ;
		case '4': ;
		case '5': ;
		case '6': ;
		case '7': ;
		case '8': ;
		case '9': digitSeen = TRUE;
			break;
			
		default:						// every other character is illegal 
			return FALSE;
		}
	}
	
	// this is for labels consisting only of a single '+' or '-' 
	if ( ( plusSeen || minusSeen ) && !digitSeen )
		return FALSE;
	
	return TRUE;
}


/*******************************************************************************
FUNCTION NAME: RestoreLabelList
INPUTS:		PGRAPH_VARIABLES GV, 
			ULONG oldNumberOfLabels
RETURNS:	none
PURPOSE:	Restore the label list to its previous size by freeing space used by
			newly added labels for compressed instances (when using minimum 
			encoding).
CALLED BY:	eval.c: EvaluateSub()
*******************************************************************************/

void RestoreLabelList( PGRAPH_VARIABLES GV, ULONG oldNumberOfLabels )
{
	ULONG index;
	
#ifdef DEBUG_TRACE
	printf( "%s: RestoreLabelList()\n", __FILE__ );
#endif
	
	if ( GV->numberOfLabels > oldNumberOfLabels )
	{
		for( index = oldNumberOfLabels; index < GV->numberOfLabels; index++ )
			if ( GV->labelList[index].labelType == 's' ) {
				Free( GV->labelList[index].content.stringValue );
			}

			GV->labelList = (LABEL *) Realloc( GV->labelList, oldNumberOfLabels * sizeof( LABEL ) );
			GV->numberOfVerticesWithLabel = (ULONG *) Realloc( GV->numberOfVerticesWithLabel, 
				oldNumberOfLabels *	sizeof( ULONG ) );
			GV->numberOfLabels = oldNumberOfLabels;
	}
}


/*******************************************************************************
FUNCTION NAME: DestroyLabelList
INPUTS:		PGRAPH_VARIABLES GV
RETURNS:	none
PURPOSE:	Free GV->labelList and GV->numberOfVerticesWithLabel.
CALLED BY:	main.c: main()
*******************************************************************************/

void DestroyLabelList( PGRAPH_VARIABLES GV )
{
	ULONG index;
	
#ifdef DEBUG_TRACE
	printf( "%s: DestroyLabelList()\n", __FILE__ );
#endif
	
	for ( index = 0; index < GV->numberOfLabels; index++ )
		if ( GV->labelList[index].labelType == 's' )
			Free( GV->labelList[index].content.stringValue );
		Free( GV->labelList );
		Free( GV->numberOfVerticesWithLabel );
		return;
}


/*******************************************************************************
FUNCTION NAME: AssignGroups
INPUTS:		PGRAPH_VARIABLES GV
RETURNS:	none
PURPOSE:	Assign labels to their user-specified groups ( -alt option ).
CALLED BY:	main.c: main()
*******************************************************************************/

void AssignGroups( PGRAPH_VARIABLES GV )
{
	ULONG groupIndex;
	ULONG labelIndex;
	ULONG labelIndex2;
	
#ifdef DEBUG_TRACE
	printf( "%s: AssignGroups()\n", __FILE__ );
#endif
	
	for ( labelIndex = 0; labelIndex < GV->numberOfLabels; labelIndex++ )
	{                                                     /* assign label group */
		if ( GV->labelList[labelIndex].labelType == 's' )
		{
			for ( groupIndex = 0; groupIndex < GV->numberOfGroups; groupIndex++ )
				for ( labelIndex2 = 0;
				labelIndex2 < GV->groupList[groupIndex].numberLabels;
				labelIndex2++ )
					if ( !strcmp( GV->groupList[groupIndex].labels[labelIndex2],
						GV->labelList[labelIndex].content.stringValue ) )
					{
						GV->labelList[labelIndex].match.group = groupIndex;
						groupIndex = GV->numberOfGroups;  /* label can appear at most once */
						break;
					}
		}
	}
	return;
}


/*******************************************************************************
FUNCTION NAME: PrintGroups
INPUTS:		PGRAPH_VARIABLES GV
RETURNS:	none
PURPOSE:	Print group of labels 
*******************************************************************************/

void PrintGroups( PGRAPH_VARIABLES GV )
{
	ULONG i, j;
	
#ifdef DEBUG_TRACE
	printf( "%s: PrintGroups()\n", __FILE__ );
#endif
	
	for ( i = 0; i < GV->numberOfGroups; i++ )
	{
		printf( "GROUP %lu\n", i + 1 );
		for ( j = 0; j < GV->groupList[i].numberLabels; j++ )
		{
			printf( "\t\t%s\n", GV->groupList[i].labels[j] );
		}
	}
	return;
}


/*******************************************************************************
FUNCTION NAME: PrintLabels
INPUTS:		PGRAPH_VARIABLES GV
RETURNS:	none
PURPOSE:	Print the contents of GV->labelList
CALLED BY: main.c: main()
*******************************************************************************/

void PrintLabels( PGRAPH_VARIABLES GV )
{
	ULONG index;
	LABEL label;
	
	printf( "graph labels:\n\n" );
	for ( index = 0; index < GV->numberOfLabels; index++ )
	{
		label = GV->labelList[index];
		if ( label.labelType == 's' )
			printf( "\t%s %d\n", label.content.stringValue, label.match.group );
		else
		{
			printf( "\t%f ", label.content.numericValue );
			switch( label.match.matchType )
			{
			case M_EXACT:      printf( "e " );
				break;
			case M_TOLERANCE:  printf( "t " );
				break;
			case M_DIFFERENCE: printf( "d " );
			}
			printf( "%f\n", label.matchValue );
		}
	}
	printf( "\n\n" );
	fflush( stdout );
	return;
}


/*******************************************************************************
FUNCTION NAME: CheckGroups
INPUTS:		PGRAPH_VARIABLES GV
RETURNS:	none
PURPOSE:	Check all groups to make sure that a label appears only in one group
*******************************************************************************/

void CheckGroups( PGRAPH_VARIABLES GV )
{
	ULONG g1;
	ULONG g2;
	ULONG l1;
	ULONG l2;
	ULONG repeatedLabelCounter = 0;
	
#ifdef DEBUG_TRACE
	printf( "%s: CheckGroups()\n", __FILE__ );
#endif
	
	for ( g1 = 0; g1 < GV->numberOfGroups - 1; g1++ ) {
		for ( l1 = 0; l1 < GV->groupList[g1].numberLabels; l1++ ) {
			for ( g2 = g1 + 1; g2 < GV->numberOfGroups; g2++ ) {
				for ( l2 = 0; l2 < GV->groupList[g2].numberLabels; l2++ ) {

					if ( strcmp( GV->groupList[g1].labels[l1],  /* labels are identical */
						GV->groupList[g2].labels[l2] ) == 0 )
					{
						repeatedLabelCounter++;
						printf( "label %s appears in groups %lu and %lu\n",
							GV->groupList[g1].labels[l1], g1 + 1, g2 + 1 );
					}
				}
			}
		}
	}

	if ( repeatedLabelCounter > 0 ) {
		ErrorFatal("Each label can appear at most in one group. Execution terminated.\n" );
	}

	return;
}


/*******************************************************************************
FUNCTION NAME: LabelMatchFactor
INPUTS:		PGRAPH_VARIABLES GV, 
			ULONG label1Index,                                  (the first label)
			ULONG label2Index                                  (the second label)
RETURNS:	DOUBLE
PURPOSE:	Define cost factor for matching two labels depending on match type 
			and match parameters specified for the labels
CALLED BY:	fuzzymat.c: FuzzyMatch()
			fuzzymat.c: DeletedEdges()
*******************************************************************************/

DOUBLE LabelMatchFactor( PGRAPH_VARIABLES GV, ULONG label1Index, 
						ULONG label2Index )
{
	MATCH matchType;
	DOUBLE matchValue;
	DOUBLE difference;
	LABEL label1;
	LABEL label2;
	
#ifdef DEBUG_TRACE
	printf( "%s: LabelMatchFactor()\n", __FILE__ );
#endif
	
	/*  if ( label1Index >= GV->numberOfLabels || label2Index >= GV->numberOfLabels )
    return 1.0; */
	ASSERT( label1Index < GV->numberOfLabels && 
		label2Index < GV->numberOfLabels );
	
	if ( label1Index == label2Index )
		return 0.0;
	
	label1 = GV->labelList[label1Index];
	label2 = GV->labelList[label2Index];
	/* at least one label is not numeric */
	if ( label1.labelType == 's' || label2.labelType == 's' )
	{
		/* both labels are not numeric */
		if ( label1.labelType == 's' && label2.labelType == 's' ) 
		{                                                 /* labels are identical */
			if ( label1.match.group != -1 &&
				label1.match.group == label2.match.group )
				return 0.0;
			else                                        /* labels are not identical */
				return 1.0;
		}
		else		   /* one label is numeric and another is not numeric */
			return 1.0;
	}
	else				                   /* both labels are numeric */
	{
		matchType = MIN( label1.match.matchType, label2.match.matchType );
		switch ( matchType )
		{
		case M_EXACT:      if ( label1.content.numericValue == 
							 label2.content.numericValue )
							 return 0.0;                  /* labels are identical */
			return 1.0;        	  /* labels are not identical */
			
		case M_TOLERANCE:  matchValue = MIN( label1.matchValue, label2.matchValue );
			difference = fabs( label1.content.numericValue -	label2.content.numericValue );
			/* difference is within tolerance */
			if ( difference <= matchValue ) 
				return 0.0;
			return 1.0;	    /* difference is beyond tolerance */
			
		case M_DIFFERENCE: matchValue = MIN( label1.matchValue, label2.matchValue );
			if ( label1.content.numericValue ==
				label2.content.numericValue )
				return 0.0;
			if ( matchValue == 0.0 )
				return 1.0;
			difference = label1.content.numericValue -
				label2.content.numericValue;
			return ( 1.0 - exp( -1.0 * difference * difference /
				( matchValue * matchValue ) ) );
		default:	       ;
		}
	}
	return 1.0;
}


/*******************************************************************************
FUNCTION NAME: SameLabel
INPUTS:		PGRAPH_VARIABLES GV, 
			LabelInformation label1,                           (the first label)
			LabelInformation label2                           (the second label)
RETURNS:	BOOLEAN                            (TRUE - labels are the same
												FALSE - labels are not the same)
PURPOSE:	Define whether two labels are the same based on their content,
			match type, and match value
CALLED BY:	subgphop.c: SameReverseEdge()
			extemp.c: InsertInVETemplatesList()
			extemp.c: FastCompare()
*******************************************************************************/

BOOLEAN SameLabel( PGRAPH_VARIABLES GV, ULONG label1Index, ULONG label2Index )
{
	MATCH matchType;
	DOUBLE matchValue;
	DOUBLE difference;
	LABEL label1;
	LABEL label2;
	
#ifdef DEBUG_TRACE
	printf( "%s: SameLabel()\n", __FILE__ );
#endif
	
	ASSERT( label1Index < GV->numberOfLabels && label2Index < 
		GV->numberOfLabels );
	
	if ( label1Index == label2Index )
		return TRUE;
	
	label1 = GV->labelList[label1Index];
	label2 = GV->labelList[label2Index];
	/* at least one label is not numeric */
	if ( label1.labelType == 's' || label2.labelType == 's' )
	{
		/* neither label is numeric */
		if ( label1.labelType == 's' && label2.labelType == 's' ) 
		{
			/* labels are identical */
			if ( label1.match.group != -1 &&
				label1.match.group == label2.match.group )
			{
				printf( "Matching %s with %s\n", label1.content.stringValue,
					label2.content.stringValue );
				return TRUE;
			}
			else                                        /* labels are not identical */
				return FALSE;
		}
		else		        /* one label is numeric and another is string */
			return FALSE;
	}
	else				                   /* both labels are numeric */
	{
		matchType = MIN( label1.match.matchType, label2.match.matchType );
		switch ( matchType )
		{
		case M_EXACT:      if ( label1.content.numericValue ==
							 label2.content.numericValue )
							 return TRUE;		      /* labels are identical */
			return FALSE;		  /* labels are not identical */
			
		case M_TOLERANCE:  matchValue = MIN( label1.matchValue, label2.matchValue );
			difference = fabs( label1.content.numericValue - label2.content.numericValue );
			if ( difference <= matchValue ) 
				return TRUE;
			return FALSE;
			
		case M_DIFFERENCE: matchValue = MIN( label1.matchValue, label2.matchValue );
			difference = fabs( label1.content.numericValue - label2.content.numericValue );
			if ( difference <= matchValue )
				return TRUE;
			return FALSE;
		}
	}
	return FALSE;
}


/*******************************************************************************
FUNCTION NAME: ConvertLabels
INPUTS:		PGRAPH_VARIABLES positiveGraph, 
			PGRAPH_VARIABLES negativeGraph, 
			PABSTRACT_SUB abstractSub
RETURNS:	none
PURPOSE:	Convert the label indices in abstractSub (which are indices of the
			positiveGraph->labelList array) to the corresponding indices of the 
			negativeGraph->labelList array. Add new labels to 
			negativeGraph->labelList if necessary.
CALLED BY:	concept.c: GetNegativeValue()
*******************************************************************************/
void ConvertLabels( PGRAPH_VARIABLES positiveGraph, PGRAPH_VARIABLES negativeGraph, 
				   PABSTRACT_SUB abstractSub )
{
	ULONG vertexIndex;
	ULONG edgeIndex;
	
#ifdef DEBUG_TRACE
	printf( "%s: ConvertLabels()\n", __FILE__ );
#endif
	
	for ( vertexIndex = 0; vertexIndex < abstractSub->numberOfVertices;
	vertexIndex++ )
		abstractSub->vertices[vertexIndex].labelIndex = 
		AddLabel( negativeGraph, positiveGraph->labelList[abstractSub->
		vertices[vertexIndex].labelIndex], TRUE, TRUE );
	
	for ( edgeIndex = 0; edgeIndex < abstractSub->numberOfEdges; edgeIndex++ )
		abstractSub->edges[edgeIndex].labelIndex = 
		AddLabel( negativeGraph, positiveGraph->labelList[abstractSub->
		edges[edgeIndex].labelIndex], FALSE, TRUE );
}

/*******************************************************************************
FUNCTION NAME: UpdateLabels
INPUTS:		PGRAPH_VARIABLES GV, 
RETURNS:	none
PURPOSE:	Eliminate unused labels from the label list after compression.
CALLED BY:	main.c: main()
*******************************************************************************/
void UpdateLabels(PGRAPH_VARIABLES GV)
{
	PLABEL oldLabels;
	PLABEL newLabels;
	ULONG *newNumberOfVerticesWithLabel;
	ULONG *convTable;
	ULONG oldNumberOfLabels;
	PGRAPH graph;
	ULONG vIndex;
	ULONG eIndex;
	ULONG i;
	ULONG temp;
	PGRAPH_VERTEX vertex;

#ifdef DEBUG_TRACE
	printf( "%s: UpdateLabels()\n", __FILE__ );
#endif

	oldLabels = GV->labelList;
	convTable = (ULONG *)Calloc(GV->numberOfLabels, sizeof(ULONG));
	graph = GV->graph;
	oldNumberOfLabels = GV->numberOfLabels;
	GV->numberOfVertexLabels = 0;
	GV->numberOfEdgeLabels = 0;

	// mark all valid labels
	for (vIndex=0; vIndex < graph->numberOfVertices; vIndex++)	// for all the vertices
	{
		vertex = &graph->vertices[vIndex];
		if (!convTable[vertex->labelIndex]) {
			convTable[vertex->labelIndex]++;
			GV->numberOfVertexLabels++;
		}
		for (eIndex=0; eIndex < vertex->numberOfEdges; eIndex++)	// for all the edges in the vertex
		{
			if (!convTable[vertex->edges[eIndex].labelIndex])
			{
				convTable[vertex->edges[eIndex].labelIndex]++;
				GV->numberOfEdgeLabels++;
			}
		}
	}
	GV->numberOfLabels = GV->numberOfEdgeLabels + GV->numberOfVertexLabels;

	// assign new label indexes
	temp = 1;							// 0 means invalid, so indeces will be 1 off
	for (i=0; i < oldNumberOfLabels; i++)			// for all the valid-label markers
	{
		if (convTable[i])
		{
			convTable[i] = temp;
			temp++;
		}
	}

	// create new label list
	newLabels = (PLABEL)Malloc((temp - 1) * sizeof(LABEL));
	newNumberOfVerticesWithLabel = (ULONG *)Malloc((temp - 1) * sizeof(ULONG));
	for (i=0; i < oldNumberOfLabels; i++)			// for all the valid-label markers
	{
		if (convTable[i])
		{
			newLabels[convTable[i] - 1] = oldLabels[i];
			newNumberOfVerticesWithLabel[convTable[i] - 1] = 
				GV->numberOfVerticesWithLabel[i];
		}
	}

	// update graph with new labels
	for (vIndex=0; vIndex < graph->numberOfVertices; vIndex++)	// for all the vertices
	{
		vertex = &graph->vertices[vIndex];
		vertex->labelIndex = convTable[vertex->labelIndex] - 1;
		for (eIndex=0; eIndex < vertex->numberOfEdges; eIndex++)	// for all the edges in the vertex
		{
			vertex->edges[eIndex].labelIndex = convTable[vertex->edges[eIndex].labelIndex] - 1;
		}
	}

	Free(convTable);
	Free(oldLabels);
	Free(GV->numberOfVerticesWithLabel);
	GV->numberOfVerticesWithLabel = newNumberOfVerticesWithLabel;
	GV->labelList = newLabels;
}

