//---------------------------------------------------------------------------
// discover.c
//
// Main Subdue discovery functions.
//
// Subdue 5
//---------------------------------------------------------------------------

#include "subdue.h"


//---------------------------------------------------------------------------
// NAME: DiscoverSubs
//
// INPUTS: (Parameters *parameters)
//
// RETURN: (SubList) - list of best discovered substructures
//
// PURPOSE: Discover the best substructures in the graphs according to
// the given parameters.  Note that we do not allow a single-vertex
// substructure of the form "SUB_#" on to the discovery list to avoid
// continually replacing "SUB_<n>" with "SUB_<n+1>".
//---------------------------------------------------------------------------

SubList *DiscoverSubs (Parameters *parameters)
{
  SubList *parentSubList;
  SubList *childSubList;
  SubList *extendedSubList;
  SubList *discoveredSubList;
  SubListNode *parentSubListNode;
  SubListNode *extendedSubListNode;
  Substructure *parentSub;
  Substructure *extendedSub;

  // parameters used
  ULONG limit          = parameters->limit;
  ULONG numBestSubs    = parameters->numBestSubs;
  ULONG beamWidth      = parameters->beamWidth;
  BOOLEAN valueBased   = parameters->valueBased;
  LabelList *labelList = parameters->labelList;
  BOOLEAN prune        = parameters->prune;
  ULONG maxVertices    = parameters->maxVertices;
  ULONG minVertices    = parameters->minVertices;
  ULONG outputLevel    = parameters->outputLevel;

  // get initial one-vertex substructures
  parentSubList = GetInitialSubs (parameters);

  discoveredSubList = AllocateSubList ();
  while ((limit > 0) && (parentSubList->head != NULL)) {
    parentSubListNode = parentSubList->head;
    childSubList = AllocateSubList ();
    // extend each substructure in parent list
    while ((limit > 0) && (parentSubListNode != NULL)) {
      parentSub = parentSubListNode->sub;
      parentSubListNode->sub = NULL;
      if (outputLevel > 4) {
	parameters->outputLevel = 1; // turn off instance printing
	printf ("\nConsidering ");
	PrintSub (parentSub, parameters);
	printf ("\n");
	parameters->outputLevel = outputLevel;
      }
      if (outputLevel > 3)
	printf ("%lu substructures left to be considered\n", limit);
      limit--;
      if (limit > 0) {
        extendedSubList = ExtendSub (parentSub, parameters);
	extendedSubListNode = extendedSubList->head;
        while (extendedSubListNode != NULL) {
          extendedSub = extendedSubListNode->sub;
	  extendedSubListNode->sub = NULL;
	  if (extendedSub->definition->numVertices <= maxVertices) {
	    // evaluate each extension and add to child list
	    EvaluateSub (extendedSub, parameters);
	    if (prune && (extendedSub->value < parentSub->value)) {
	      FreeSub (extendedSub);
	    } else {
	      SubListInsert (extendedSub, childSubList, beamWidth, valueBased,
			     labelList);
	    }
	  } else {
	    FreeSub (extendedSub);
	  }
	  extendedSubListNode = extendedSubListNode->next;
        }
	FreeSubList (extendedSubList);
      }
      // add parent substructure to final discovered list
      if (parentSub->definition->numVertices >= minVertices) {
	if (! SinglePreviousSub (parentSub, parameters)) {
	  if (outputLevel > 3)
	    PrintNewBestSub (parentSub, discoveredSubList, parameters);
	  SubListInsert (parentSub, discoveredSubList, numBestSubs, FALSE,
			 labelList);
	}
      } else {
	FreeSub (parentSub);
      }
      parentSubListNode = parentSubListNode->next;
    }
    FreeSubList (parentSubList);
    parentSubList = childSubList;
  }

  if (limit > 0)
    printf ("\nSubstructure queue empty.\n");

  // try to insert any remaining subs in parent list on to discovered list
  parentSubListNode = parentSubList->head;
  while (parentSubListNode != NULL) {
    parentSub = parentSubListNode->sub;
    parentSubListNode->sub = NULL;
    if (parentSub->definition->numVertices >= minVertices) {
      if (! SinglePreviousSub (parentSub, parameters)) {
	if (outputLevel > 3)
	  PrintNewBestSub (parentSub, discoveredSubList, parameters);
	SubListInsert (parentSub, discoveredSubList, numBestSubs, FALSE,
		       labelList);
      }
    } else {
      FreeSub (parentSub);
    }
    parentSubListNode = parentSubListNode->next;
  }
  FreeSubList (parentSubList);
  return discoveredSubList;
}


//---------------------------------------------------------------------------
// NAME: GetInitialSubs
//
// INPUTS: (Parameters *parameters)
//
// RETURN: (SubList *)
//
// PURPOSE: Return a list of substructures, one for each unique vertex
// label in the positive graph.
//---------------------------------------------------------------------------

SubList *GetInitialSubs (Parameters *parameters)
{
  BOOLEAN *labelUsed;
  SubList *initialSubs;
  ULONG i, j;
  ULONG vertexLabelIndex;
  Graph *g;
  Substructure *sub;
  Instance *instance;
  double subValue = 0.0;

  // parameters used
  Graph *posGraph      = parameters->posGraph;
  Graph *negGraph      = parameters->negGraph;
  LabelList *labelList = parameters->labelList;

  // create array of flags indicating labels' use
  labelUsed = (BOOLEAN *) malloc (sizeof (BOOLEAN) * labelList->numLabels);
  if (labelUsed == NULL)
    OutOfMemoryError ("GetInitialSubs:labelUsed");
  for (i = 0; i < labelList->numLabels; i++)
    labelUsed[i] = FALSE;
 
  initialSubs = AllocateSubList ();
  for (i = 0; i < posGraph->numVertices; i++) {
    vertexLabelIndex = posGraph->vertices[i].label;
    if (! labelUsed[vertexLabelIndex]) {

      // create one-vertex substructure definition
      g = AllocateGraph (1, 0);
      g->vertices[0].label = vertexLabelIndex;
      g->vertices[0].numEdges = 0;
      g->vertices[0].edges = NULL;
      // allocate substructure
      sub = AllocateSub ();
      sub->definition = g;
      sub->instances = AllocateInstanceList ();

      // collect instances in positive graph
      j = posGraph->numVertices;
      do {
	j--;
	if (posGraph->vertices[j].label == vertexLabelIndex) {
	  // ***** do inexact label matches here? (instance->minMatchCost
	  // ***** too)
	  instance = AllocateInstance (1, 0);
	  instance->vertices[0] = j;
	  instance->minMatchCost = 0.0;
	  InstanceListInsert (instance, sub->instances, FALSE);
	  sub->numInstances++;
	}
      }	while (j > i);

      if (negGraph != NULL) {
	// collect instances in negative graph
	sub->negInstances = AllocateInstanceList ();
	j = negGraph->numVertices;
	do {
	  j--;
	  if (negGraph->vertices[j].label == vertexLabelIndex) {
	    // ***** do inexact label matches here? (instance->minMatchCost
	    // ***** too)
	    instance = AllocateInstance (1, 0);
	    instance->vertices[0] = j;
	    instance->minMatchCost = 0.0;
	    InstanceListInsert (instance, sub->negInstances, FALSE);
	    sub->numNegInstances++;
	  }
	} while (j > i);
      }

      // All single-vertex substructures will have the same value, so
      // just compute it once for the first substructure and use the
      // same value for all
      if (i == 0) {
	EvaluateSub (sub, parameters);
	subValue = sub->value;
      } else sub->value = subValue;

      // add to initialSubs
      SubListInsert (sub, initialSubs, 0, FALSE, labelList);
      labelUsed[vertexLabelIndex] = TRUE;
    }
  }

  free (labelUsed);

  return initialSubs;
}


//---------------------------------------------------------------------------
// NAME: SinglePreviousSub
//
// INPUTS: (Substructure *sub) - substructure to check
//         (Parameters *parameters)
//
// RETURN: (BOOLEAN)
//
// PURPOSE: Returns TRUE if the given substructure is a single-vertex
// substructure and the vertex refers to a previously-discovered
// substructure, i.e., the vertex label is of the form "SUB_#".  This
// is used to prevent repeatedly compressing the graph by replacing a
// "SUB_<n>" vertex by a "SUB_<n+1>" vertex.
//---------------------------------------------------------------------------

BOOLEAN SinglePreviousSub (Substructure *sub, Parameters *parameters)
{
  BOOLEAN match;
  // parameters used
  LabelList *labelList = parameters->labelList;

  match = FALSE;
  if ((sub->definition->numVertices == 1) &&  // single-vertex sub?
      (SubLabelNumber (sub->definition->vertices[0].label, labelList) > 0)
      // valid substructure label
      )
    match = TRUE;

  return match;
}
