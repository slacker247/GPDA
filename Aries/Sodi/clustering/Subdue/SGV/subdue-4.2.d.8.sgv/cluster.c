/********************************************************************
*
* SUBDUE
*
* FILE NAME: cluster.c
*
********************************************************************/


#include "subdue.h"
#include "cluster.h"



/*******************************************************************************
FUNCTION NAME: CL_CreateLattice
INPUTS:			none
RETURNS:		PCLASSIFICATION_LATTICE
PURPOSE:		Create and return the classification lattice root node.
CALLED BY:		main.c: main()
*******************************************************************************/
PCLASSIFICATION_LATTICE CL_CreateLattice(char *latticeName)
{
	PCLASSIFICATION_LATTICE newCLRoot;
	char labelStr[MAX_TOKEN_LENGTH];
	int labelStrLen;
	int i;
	
#ifdef DEBUG_TRACE
	printf( "%s: CL_CreateLattice()\n", __FILE__ );
#endif
	
	newCLRoot = (PCLASSIFICATION_LATTICE) Malloc( sizeof(CLASSIFICATION_LATTICE) );
	
	sprintf(labelStr, latticeName );							// generating sub label
	labelStrLen = strlen(labelStr);								// get its length
	for (i=0; i < labelStrLen; i++) {
		if (labelStr[i] == '.' ||							// replacing ., -, \, and / with _
			labelStr[i] == '-' ||
			labelStr[i] == '\\' ||
			labelStr[i] == '/')
			labelStr[i] = '_';
		labelStr[i] = toupper(labelStr[i]);				// capitalize each letter
	}

	newCLRoot->subLabel = (char *)Malloc( labelStrLen + 1);
	strcpy(newCLRoot->subLabel, labelStr);
	newCLRoot->trueLabel = (char *)Malloc( labelStrLen + 1 );
	strcpy(newCLRoot->trueLabel, labelStr);


//	newCLRoot->definition = NULL;								// initializing members
//	newCLRoot->instances = NULL;
	newCLRoot->children = NULL;
	newCLRoot->numberOfChildren = 0;
	newCLRoot->numberOfParents = 1;								// root has dummy parent
																// this is used during destroy
	newCLRoot->referenceCount = 0;
	newCLRoot->shape = FALSE;

	return newCLRoot;
}

/*******************************************************************************
FUNCTION NAME: CL_CreateNode
INPUTS:			PGRAPH_VARIABLES GV
				PSUB sub
RETURNS:		PCLASSIFICATION_LATTICE
PURPOSE:		Create and return a classification lattice (CL) node.
CALLED BY:		main.c: main()
*******************************************************************************/
PCLASSIFICATION_LATTICE CL_CreateNode(PGRAPH_VARIABLES GV, PSUB sub)
{
	PCLASSIFICATION_LATTICE newCLNode;
	char labelStr[12000];			// this sould be really big, since it has to hold
									// potentially many node and edge labels
	char tstr[20];
//	PSUB_GRAPH definition;
//	ULONG i;
	
#ifdef DEBUG_TRACE
	printf( "%s: CL_CreateNode()\n", __FILE__ );
#endif
	
	newCLNode = (PCLASSIFICATION_LATTICE) Malloc( sizeof(CLASSIFICATION_LATTICE) );

	// create label of form "Sub_n"
	sprintf(labelStr, "%s%lu\0", SUB_LABEL_PREFIX, GV->subNumber, sub->instances->currentLength );	// generating sub label
	newCLNode->subLabel = (char *)Malloc( strlen(labelStr) + 1 );
	strcpy(newCLNode->subLabel, labelStr);

	// create label that describes the sub in words
	if (GV->trueLabel) {
		labelStr[0] = '\0';
		CL_MakeTrueLabel( GV, sub->definition, labelStr);			// generating sub label
		sprintf(tstr, " [%lu]\0", sub->instances->currentLength );	// generating sub label
		newCLNode->trueLabel = (char *)Malloc( strlen(labelStr) + 
			strlen(newCLNode->subLabel) + 3 + strlen(tstr));
		strcpy(newCLNode->trueLabel, newCLNode->subLabel);
		strcat(newCLNode->trueLabel, tstr);
		strcat(newCLNode->trueLabel, "\\n");
		strcat(newCLNode->trueLabel, labelStr);
	}
	else 
		newCLNode->trueLabel = NULL;

//	newCLNode->instances = NULL;								// initializing members
	newCLNode->children = NULL;
	newCLNode->numberOfChildren = 0;
	newCLNode->numberOfParents = 0;
	newCLNode->referenceCount = 0;
	newCLNode->shape = FALSE;

/*	newCLNode->definition = (PSUB_GRAPH) Malloc(sizeof(SUB_GRAPH));// allocate space for definition

	definition = newCLNode->definition;
	definition->numberOfVertices = sub->definition->numberOfVertices;
	definition->numberOfEdges = sub->definition->numberOfEdges;
	definition->matchingOrder = NULL;
	definition->matchCost = 0;
	definition->refCount = 0;

	//** this may not work. depends on how the graph is constructed and retained after 
	//   a compression. -\/-
	// Aslo, it may not be needed.

	// copy the vertices
	definition->vertices = (PSUB_GRAPH_VERTEX) 
		Malloc(definition->numberOfVertices * sizeof(SUB_GRAPH_VERTEX));

	for (i=0; i < definition->numberOfVertices; i++) {
		definition->vertices[i] = sub->definition->vertices[i];
	}

	// copy the vertices
	definition->edges = (PSUB_GRAPH_EDGE) 
		Malloc(definition->numberOfEdges * sizeof(SUB_GRAPH_EDGE));

	for (i=0; i < definition->numberOfEdges; i++) {
		definition->edges[i] = sub->definition->edges[i];
	}
*/

	return newCLNode;
}
/*******************************************************************************
FUNCTION NAME: CL_Insert
INPUTS:			PGRAPH_VARIABLES GV
				PCLASSIFICATION_LATTICE clRoot
				PSUB sub
				BOOLEAN shape
RETURNS:		BOOLEAN
PURPOSE:		Insert a sub into CL rooted at clRoot.
CALLED BY:		main.c: main()
*******************************************************************************/
BOOLEAN CL_Insert(PGRAPH_VARIABLES GV, PCLASSIFICATION_LATTICE clRoot, PSUB sub,
				  BOOLEAN shape)
{
	PCLASSIFICATION_LATTICE clNode;
	PCLASSIFICATION_LATTICE clParent;
	PSUB_GRAPH_VERTEX vertices;
	PSUB_GRAPH_VERTEX currentVertex;
	PGRAPH_VERTEX graphVertices;
	PGRAPH_VERTEX currentGraphVertex;
	LABEL label;
	ULONG parentCount;
	ULONG i;
//	ULONG j;
	
#ifdef DEBUG_TRACE
	printf( "%s: CL_Insert()\n", __FILE__ );
#endif

	clNode = CL_CreateNode(GV, sub);					// create new CL node based on sub
	vertices = sub->definition->vertices;				// get vertices
	parentCount = 0;
	clNode->shape = shape;

	for (i=0; i < sub->definition->numberOfVertices; i++) {	// for all the vertices of the new node
		graphVertices = GV->graph->vertices;					// get graph vertices
		currentVertex = &vertices[i];							// get sub vertex
		currentGraphVertex = &graphVertices[currentVertex->indexInGraph];	// get vertex in graph
		label = GV->labelList[currentGraphVertex->labelIndex];				// get label
		
		if ( label.labelType == 's' ) {							 // for string labels only
			clParent = CL_Find(clRoot, label.content.stringValue );	// find CL node

			if (clParent) {											// if found
				BOOLEAN alreadyThere = FALSE;
/*	// this code is removed, because we may want several edges between nodes
				// check for previous entry of this same node among children of parent
				for (j=0; j < clParent->numberOfChildren; j++) {	// for all the children of the parent
					if (clParent->children[j] == clNode) {			// if node is already there
						alreadyThere = TRUE;							// signal and quit
						break;
					}
				}
*/
				if (!alreadyThere) {
					CL_HookUp(clParent, clNode);					// hook it up
					parentCount++;
				}
			}
		}
	}

	if (parentCount == 0) {								// if node was not hooked up anywhere
		CL_HookUp(clRoot, clNode);							// hook it to the root
	}

	return TRUE;
}

/*******************************************************************************
FUNCTION NAME: CL_HookUp
INPUTS:			PCLASSIFICATION_LATTICE clParent
				PCLASSIFICATION_LATTICE clChild
RETURNS:		BOOLEAN
PURPOSE:		Hook up CL node as child of clParent.
CALLED BY:		cluster.c: CL_Insert()
*******************************************************************************/
BOOLEAN CL_HookUp(PCLASSIFICATION_LATTICE clParent, PCLASSIFICATION_LATTICE clChild)
{
	
#ifdef DEBUG_TRACE
	printf( "%s: CL_HookUp()\n", __FILE__ );
#endif
	
	clParent->numberOfChildren++;								// increase number of children
	clParent->children = (PCLASSIFICATION_LATTICE *) Realloc(clParent->children, 
		clParent->numberOfChildren * sizeof(PCLASSIFICATION_LATTICE));

	if (clParent->children == NULL) 
		return FALSE;											// hook up fails (no memory)

	clParent->children[clParent->numberOfChildren - 1] = clChild;
	clChild->numberOfParents++;									// increasing number of parents

	return TRUE;
}

/*******************************************************************************
FUNCTION NAME: CL_Find
INPUTS:			PCLASSIFICATION_LATTICE clRoot
				const char *label
RETURNS:		PCLASSIFICATION_LATTICE
PURPOSE:		Find a CL node in a CL pointed to by root.
CALLED BY:		cluster.c: CL_Insert()
*******************************************************************************/
PCLASSIFICATION_LATTICE CL_Find(PCLASSIFICATION_LATTICE clRoot, const char *label)
{
	PCLASSIFICATION_LATTICE CLNode;
	ULONG i;
	
#ifdef DEBUG_TRACE
	printf( "%s: CL_Find()\n", __FILE__ );
#endif

	if (strcmp(label, clRoot->subLabel) == 0) {					// if this is it
		return clRoot;
	}

	for (i=0; i < clRoot->numberOfChildren; i++) {				// for all the children
		CLNode = CL_Find(clRoot->children[i], label);			// find children recursively
		if (CLNode)												// if child retured not NULL
			return CLNode;										// we got it
	}

	return NULL;												// this branch does not have 'label'
}

/*******************************************************************************
FUNCTION NAME: CL_Print
INPUTS:			PGRAPH_VARIABLES GV
				PCLASSIFICATION_LATTICE clNode
				ULONG height
				BOOLEAN printOnScreen
RETURNS:		none
PURPOSE:		Print a CL preorder style on the screen, and construct the AT&T
				dot style graph.
CALLED BY:		main.c: main()
*******************************************************************************/
void CL_Print(PGRAPH_VARIABLES GV, PCLASSIFICATION_LATTICE clNode, 
			  ULONG height, BOOLEAN printOnScreen)
{
	int doFile;
	ULONG i;
		
#ifdef DEBUG_TRACE
	printf( "%s: CL_Print()\n", __FILE__ );
#endif

	clNode->referenceCount++;
	doFile = (clNode->referenceCount == clNode->numberOfParents) ? 1 : 0;
	
	if (printOnScreen) {
		// Printing lattice on screen
		for (i=0; i < height; i++)						// create appopriate tabulation
			printf("   ");

		printf("%s\n", clNode->subLabel);				// print sub label on screen
	}
	fprintf(GV->clusterFile, "%s;\n ", clNode->subLabel);	// print node label
	
	// Printing graph information into graph file
	if (GV->clusterFile && doFile) {							// if graph file is open
		fprintf(GV->clusterFile, "\t%s [color=%s",				// select color
			clNode->subLabel, ATT_COLOR[height % ATT_COLOR_COUNT]);
		if (GV->trueLabel)
			fprintf(GV->clusterFile, ", label=\"%s\"", clNode->trueLabel);
		if (clNode->shape)
			fprintf(GV->clusterFile, ", shape=%s", EXHAUSTED_SHAPE);
		fprintf(GV->clusterFile, "];\n");

		// go after children
		for (i=0; i < clNode->numberOfChildren; i++) {		// for all the children
			if (GV->clusterFile && doFile) {						// if file is open
				fprintf(GV->clusterFile, "\t%s -> ", clNode->subLabel);
			}
			// print children recursively
			CL_Print(GV, clNode->children[i], height + 1, printOnScreen);
		}
	}

}

/*******************************************************************************
FUNCTION NAME: CL_Destroy
INPUTS:			PCLASSIFICATION_LATTICE clNode
RETURNS:		none
PURPOSE:		Deallocate a CL (or a CL node and all its children).
CALLED BY:		main.c: main()
*******************************************************************************/
void CL_Destroy(PCLASSIFICATION_LATTICE clNode)
{
	ULONG i;
		
#ifdef DEBUG_TRACE
	printf( "%s: CL_Destroy()\n", __FILE__ );
#endif
	
	clNode->numberOfParents--;
	if ( clNode->numberOfParents == 0 ) {				// if no more parents

/*		if (clNode->definition)
			DestroySubGraph(clNode->definition);
*/
		for (i=0; i < clNode->numberOfChildren; i++) {		// for all the children
			CL_Destroy(clNode->children[i]);				// destroy children recursively
		}

		Free(clNode->children);
		Free(clNode->subLabel);
		Free(clNode->trueLabel);
		Free(clNode);										// deallocate this node
	}
}

/*******************************************************************************
FUNCTION NAME: CL_Finish
INPUTS:			PGRAPH_VARIABLES GV
				PCLASSIFICATION_LATTICE clNode
RETURNS:		none
PURPOSE:		Deallocate a CL (or a CL node and all its children).
CALLED BY:		main.c: main()
*******************************************************************************/
void CL_Finish(PGRAPH_VARIABLES GV, PCLASSIFICATION_LATTICE clNode)
{

#ifdef DEBUG_TRACE
	printf( "%s: CL_Finish()\n", __FILE__ );
#endif
	
	if (GV->cluster) {				// if clustering
		printf("Classification lattice:\n");

		GV->clusterFile = fopen(GV->clusterFileName, "w+");
		fprintf(GV->clusterFile, "%s G {\n", ATT_GRAPH_TYPE);
		fprintf(GV->clusterFile, "\tnode [style=filled];\n");
		
		CL_Print(GV, clNode, 0, TRUE);			// printing lattice

		fprintf(GV->clusterFile, "}\n");		// close graph body
		fclose(GV->clusterFile);				// close cluster file

		CL_Destroy(clNode);				// destroy classification lattice
	}
}

/*******************************************************************************
FUNCTION NAME: CL_MakeTrueLabel
INPUTS:		PGRAPH_VARIABLES GV, 
			PSUB_GRAPH subGraph, 
			char *trueLabel
RETURNS:	void
PURPOSE:	Make a label that describes the node by descriptions of edges and
			vertices.
CALLED BY:	cluster.c: CL_CreateNode() 
*******************************************************************************/

void CL_MakeTrueLabel( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph, char *trueLabel )
{
	ULONG vertexIndex;
	ULONG edgeIndex;
	PSUB_GRAPH_VERTEX vertices;
	PSUB_GRAPH_VERTEX currentVertex;
	ULONG currentVertexIndex;
	PSUB_GRAPH_EDGE currentSubEdges;
	PGRAPH_VERTEX graphVertices;
	PGRAPH_VERTEX currentGraphVertex;
	PGRAPH_EDGE currentEdge;
	char labelBuffer[MAX_TOKEN_LENGTH];

	
#ifdef DEBUG_TRACE
	printf( "%s: CL_MakeTrueLabel()\n", __FILE__ );
#endif
	
	vertices = subGraph->vertices;
	graphVertices = GV->graph->vertices;

	for ( vertexIndex = 0; vertexIndex < subGraph->numberOfVertices; vertexIndex++ )
	{
		currentVertex = &vertices[vertexIndex];
		currentVertexIndex = currentVertex->indexInGraph;
		currentGraphVertex = &graphVertices[currentVertexIndex];
		currentSubEdges = &subGraph->edges[currentVertex->edgesIndex];

		for ( edgeIndex = 0; edgeIndex < currentVertex->numberOfEdges; edgeIndex++ )
		{
			currentEdge = &currentGraphVertex->
						edges[currentSubEdges[edgeIndex].indexInGraphVertex];
//{U}
//			if ( currentEdge->directed ||									// if edge is directed
//				 ( currentEdge->targetVertexIndex >= currentVertexIndex ) )	// or target >= source
//			{

				CL_FormatEdge( GV, currentEdge,	graphVertices, 
							   currentVertexIndex, labelBuffer);
				strcat(trueLabel, labelBuffer);
				if ( edgeIndex < (currentVertex->numberOfEdges) )	// for all, but the last edge
					strcat(trueLabel, "\\n");							// add new line char in string
//			}
		}
	}
}

/*******************************************************************************
FUNCTION NAME: CL_FormatEdge
INPUTS:		PGRAPH_VARIABLES GV
			PGRAPH_EDGE edge, 
			PGRAPH_VERTEX vertices
			ULONG sourceVertexIndex
			char *trueLabel
RETURNS:	void
PURPOSE:	Format the true edge label for the classification lattice.
CALLED BY:	cluster.c: CL_MakeTrueLabel()
*******************************************************************************/

void CL_FormatEdge( PGRAPH_VARIABLES GV, PGRAPH_EDGE edge, 
				   PGRAPH_VERTEX vertices, ULONG sourceVertexIndex,
				   char *trueLabel )
{
	LABEL label;
	char vLabelBuffer[MAX_TOKEN_LENGTH];
	UCHAR labelTag[3];// = "    \0";
	
#ifdef DEBUG_TRACE
	printf( "%s: CL_FormatEdge()\n", __FILE__ );
#endif
	
	vLabelBuffer[0] = '\0';

	// print source vertex label
	label = GV->labelList[vertices[sourceVertexIndex].labelIndex];
	if ( label.labelType == 's' ) {                              /* string label */
		// if label is not a Sub_n
//		if ( strncmp(label.content.stringValue, SUB_LABEL_PREFIX, SUB_LABEL_PREFIX_LEN) != 0 )
			sprintf( vLabelBuffer, "%s", label.content.stringValue );
	}
	else														/* numeric label */
		sprintf( vLabelBuffer, "%f", label.content.numericValue );

	if (strncmp(vLabelBuffer, SUB_LABEL_PREFIX, SUB_LABEL_PREFIX_LEN) == 0) {
		labelTag[0] = (UCHAR)(97 + (sourceVertexIndex % 26));
		strncat( vLabelBuffer, labelTag, 1 );
	}
	strcpy( trueLabel, vLabelBuffer  );


	// print edge label
	label = GV->labelList[edge->labelIndex];
	if ( label.labelType == 's' )                               /* string label */
		sprintf( vLabelBuffer, " %s: ", label.content.stringValue );
	else			 											/* numeric label */
		sprintf( vLabelBuffer, " %f: ", label.content.numericValue );

	strcat( trueLabel, vLabelBuffer  );


	// print target vertex label
	label = GV->labelList[vertices[edge->targetVertexIndex].labelIndex];
	if ( label.labelType == 's' )                               /* string label */
		sprintf( vLabelBuffer, "%s", label.content.stringValue );
	else														/* numeric label */
		sprintf( vLabelBuffer, "%f", label.content.numericValue );

	if (strncmp(vLabelBuffer, SUB_LABEL_PREFIX, SUB_LABEL_PREFIX_LEN) == 0) {
		labelTag[0] = (UCHAR)(97 + (edge->targetVertexIndex % 26));
		strncat( vLabelBuffer, labelTag, 1 );
	}
	strcat( trueLabel, vLabelBuffer  );
}


/*******************************************************************************
FUNCTION NAME: IsGraphExhausted
INPUTS:		PGRAPH_VARIABLES GV
RETURNS:	BOOLEAN
PURPOSE:	Check to see if all vertices have label of the form "Sub_n"
CALLED BY:	main.c:	main() (serial)
*******************************************************************************/
BOOLEAN IsGraphExhausted( PGRAPH_VARIABLES GV )
{
	ULONG vertexIndex;
	PGRAPH graph;
	PGRAPH_VERTEX vertices;
	LABEL label;
	
#ifdef DEBUG_TRACE
	printf( "%s: IsGraphExhausted()\n", __FILE__ );
#endif
	
	graph = GV->graph;
	vertices = graph->vertices;
	
	for( vertexIndex = 0; vertexIndex < graph->numberOfVertices; vertexIndex++ )
	{
		label = GV->labelList[vertices[vertexIndex].labelIndex];
		if ( label.labelType == 's' )                               /* string label */
			if (strncmp(label.content.stringValue,			// if label doesn't start with Sub_
						SUB_LABEL_PREFIX, SUB_LABEL_PREFIX_LEN) != 0)
			{
				return FALSE;										// graph is not exhausted
			}
		else														/* numeric label */
			return FALSE;											// -> not Sub_n
	}

	return TRUE;
}

