//---------------------------------------------------------------------------
// subops.c
//
// Substructure and substructure list operations.
//
// Subdue 5
//---------------------------------------------------------------------------

#include "subdue.h"


//---------------------------------------------------------------------------
// NAME: AllocateSubListNode
//
// INPUTS: (Substructure *sub) - substructure to be stored in node
//
// RETURN: (SubListNode *) - newly allocated SubListNode
//
// PURPOSE: Allocate a new SubListNode.
//---------------------------------------------------------------------------

SubListNode *AllocateSubListNode (Substructure *sub)
{
  SubListNode *subListNode;

  subListNode = (SubListNode *) malloc (sizeof (SubListNode));
  if (subListNode == NULL)
    OutOfMemoryError ("SubListNode");
  subListNode->sub = sub;
  subListNode->next = NULL;
  return subListNode;
}


//---------------------------------------------------------------------------
// NAME: FreeSubListNode
//
// INPUTS: (SubListNode *subListNode) - SubListNode to be freed
//
// RETURN: (void)
//
// PURPOSE: Deallocate memory of subListNode, including substructure
// if exists.
//---------------------------------------------------------------------------

void FreeSubListNode (SubListNode *subListNode)
{
  if (subListNode != NULL) {
    FreeSub (subListNode->sub);
    free (subListNode);
  }
}


//---------------------------------------------------------------------------
// NAME: AllocateSubList
//
// INPUTS: (void)
//
// RETURN: (SubList *) - newly-allocated substructure list
//
// PURPOSE: Allocate and return an empty list to hold substructures.
//---------------------------------------------------------------------------

SubList *AllocateSubList (void)
{
  SubList *subList;

  subList = (SubList *) malloc (sizeof (SubList));
  if (subList == NULL)
    OutOfMemoryError ("AllocateSubList:subList");
  subList->head = NULL;
  return subList;
}


//---------------------------------------------------------------------------
// NAME: SubListInsert
//
// INPUTS: (Substructure *sub) - substructure to be inserted
//         (SubList *subList) - list to be inserted in to
//         (ULONG max) - maximum number of substructures or different
//                       substructure values allowed on list;
//                       max = 0 means max = infinity
//         (BOOLEAN valueBased) - TRUE if list limited by different
//                                values; otherwise, limited by
//                                different substructures
//         (LabelList *labelList) - needed for checking sub equality
//
// RETURN: (void)
//
// PURPOSE: Inserts sub into subList, if not already there.  List is
// kept in decreasing order by substructure value.  If valueBased =
// TRUE, then max represents the maximum number of different valued
// substructures on the list; otherwise, max represents the maximum
// number of substructures on the list.  If sub is not inserted, then
// it is destroyed.  SubListInsert assumes given subList already
// conforms to maximums.
//---------------------------------------------------------------------------

void SubListInsert (Substructure *sub, SubList *subList, ULONG max,
		    BOOLEAN valueBased, LabelList *labelList)
{
  SubListNode *subIndex = NULL;
  SubListNode *subIndexPrevious = NULL;
  SubListNode *newSubListNode = NULL;
  ULONG numSubs = 0;
  ULONG numDiffVals = 0;
  BOOLEAN inserted = FALSE;

  newSubListNode = AllocateSubListNode (sub);

  // if subList empty, insert new sub and exit (no need to check maximums)
  if (subList->head == NULL) {
    subList->head = newSubListNode;
    return;
  }

  // if sub already on subList, destroy and exit
  subIndex = subList->head;
  while ((subIndex != NULL) && (subIndex->sub->value >= sub->value)) {
    if (subIndex->sub->value == sub->value) {
      if (GraphMatch (subIndex->sub->definition, sub->definition,
		      labelList, 0.0, NULL, NULL)) {
	FreeSubListNode (newSubListNode);
	return;
      }
    }
    subIndex = subIndex->next;
  }

  // sub is unique, so insert in appropriate place and check maximums
  subIndex = subList->head;
  while (subIndex != NULL) {

    if (! inserted) {
      if (subIndex->sub->value <= sub->value) {
	// the <= in the above test is important so that if two subs
	// have the same value, the later-generated (and possibly
	// larger) sub is first in the list
	newSubListNode->next = subIndex;
	if (subIndexPrevious != NULL)
	  subIndexPrevious->next = newSubListNode;
	else subList->head = newSubListNode;
	subIndex = newSubListNode;
	inserted = TRUE;
      } else if (subIndex->next == NULL) {
	// Special case where the potential spot is the end of the
	// list, so go ahead and put it there, but may get removed
	// next time through the loop if boundaries are exceeded.
	subIndex->next = newSubListNode;
	inserted = TRUE;
      }
    }
    
    // update counters on number of substructures and different values
    numSubs++;
    if (subIndexPrevious == NULL)
      numDiffVals = 1;
    else if (subIndexPrevious->sub->value != subIndex->sub->value)
      numDiffVals++;
    
    // check if maximum exceeded
    if ( (max > 0) && 
	 (((valueBased) && (numDiffVals > max)) ||
	  ((! valueBased) && (numSubs > max))) ) {
      // max exceeded, so delete rest of subList from subIndex on
      if (subIndexPrevious != NULL)
	subIndexPrevious->next = NULL;
      while (subIndex != NULL) {
	subIndexPrevious = subIndex;
	subIndex = subIndex->next;
	FreeSubListNode (subIndexPrevious);
      }
    } else {
      subIndexPrevious = subIndex;
      subIndex = subIndex->next;
    }
  }

  if (! inserted)
    FreeSubListNode (newSubListNode);
}


//---------------------------------------------------------------------------
// NAME: MemberOfSubList
//
// INPUTS: (Substructure *sub)
//         (SubList *subList)
//         (LabelList *labelList)
//
// RETURN: (BOOLEAN)
//
// PURPOSE: Check if the given substructure's definition graph exactly
// matches a definition of a substructure on the subList.
//---------------------------------------------------------------------------

BOOLEAN MemberOfSubList (Substructure *sub, SubList *subList,
			 LabelList *labelList)
{
  SubListNode *subListNode;
  BOOLEAN found = FALSE;

  if (subList != NULL) {
    subListNode = subList->head;
    while ((subListNode != NULL) && (! found)) {
      if (GraphMatch (sub->definition, subListNode->sub->definition,
		      labelList, 0.0, NULL, NULL))
	found = TRUE;
      subListNode = subListNode->next;
    }
  }
  return found;
}


//---------------------------------------------------------------------------
// NAME: FreeSubList
//
// INPUTS: (SubList *subList) - pointer to beginning of list to be freed
//
// RETURN:  void
//
// PURPOSE: Free memory in subList, including substructures pointed to.
//---------------------------------------------------------------------------

void FreeSubList (SubList *subList)
{
  SubListNode *subListNode1 = NULL;
  SubListNode *subListNode2 = NULL;

  if (subList != NULL) {
    subListNode1 = subList->head;
    while (subListNode1 != NULL) {
      subListNode2 = subListNode1;
      subListNode1 = subListNode1->next;
      FreeSub (subListNode2->sub);
      free (subListNode2);
    }
    free (subList);
  }
}


//---------------------------------------------------------------------------
// NAME: PrintSubList
//
// INPUTS: (SubList *subList) - list of substructures to print
//         (Graph *graph) - graph containing substructures
//         (Parameters *parameters)
//
// RETURN:  void
//
// PURPOSE: Print given list of substructures.
//---------------------------------------------------------------------------

void PrintSubList (SubList *subList, Parameters *parameters)
{
  ULONG counter = 1;
  SubListNode *subListNode = NULL;

  if (subList != NULL) {
    subListNode = subList->head;
    while (subListNode != NULL) {
      printf ("(%lu) ", counter);
      counter++;
      PrintSub (subListNode->sub, parameters);
      printf ("\n");
      subListNode = subListNode->next;
    }
  }
}


//---------------------------------------------------------------------------
// NAME: AllocateSubstructure
//
// INPUTS: void
//
// RETURN: (Substructure *) - pointer to newly allocated substructure.
//
// PURPOSE: Allocate and initialize new substructure.  A negative
// value indicates not yet computed.
//---------------------------------------------------------------------------

Substructure *AllocateSub ()
{
  Substructure *sub;

  sub = (Substructure *) malloc (sizeof (Substructure));
  if (sub == NULL)
    OutOfMemoryError ("substructure");
  sub->definition = NULL;
  sub->numInstances = 0;
  sub->instances = NULL;
  sub->numNegInstances = 0;
  sub->negInstances = NULL;
  sub->value = -1.0;

  return sub;
}


//---------------------------------------------------------------------------
// NAME: FreeSub
//
// INPUTS: (Substructure *sub) - Substructure to be freed.
//
// RETURN: void
//
// PURPOSE: Free memory used by given substructure.
//---------------------------------------------------------------------------

void FreeSub (Substructure *sub)
{
  if (sub != NULL) {
    FreeGraph (sub->definition);
    FreeInstanceList (sub->instances);
    FreeInstanceList (sub->negInstances);
    free (sub);
  }
}


//---------------------------------------------------------------------------
// NAME: PrintSub
//
// INPUTS: (Substructure *sub) - substructure to print
//         (Parameters *parameters) - parameters
//
// RETURN: void
//
// PURPOSE: Print given substructure's value, number of instances,
// definition, and possibly the instances.
//---------------------------------------------------------------------------

void PrintSub (Substructure *sub, Parameters *parameters)
{
  // parameters used
  Graph *negGraph = parameters->negGraph;
  LabelList *labelList = parameters->labelList;
  ULONG outputLevel = parameters->outputLevel;

  if (sub != NULL) {
    printf ("Substructure: value = %.*g, ", NUMERIC_OUTPUT_PRECISION,
	    sub->value);
    if (negGraph == NULL)
      printf ("instances = %lu\n", sub->numInstances);
    else printf ("pos instances = %lu, neg instances = %lu\n",
		 sub->numInstances, sub->numNegInstances);
    if (sub->definition != NULL) {
      PrintGraph (sub->definition, labelList);
    }
    // print instances if output level high enough
    if (outputLevel > 2) {
      if (negGraph == NULL)
	PrintPosInstanceList (sub, parameters);
      else {
	printf ("\n  Positive instances:\n");
	PrintPosInstanceList (sub, parameters);
	if (sub->numNegInstances > 0) {
	  printf ("\n  Negative instances:\n");
	  PrintNegInstanceList (sub, parameters);
	}
      }
    }
  }
}


//---------------------------------------------------------------------------
// NAME: PrintNewBestSub
//
// INPUTS: (Substructure *sub) - possibly new best substructure
//         (SubList *subList) - list of best substructures
//         (Parameters *parameters)
//
// RETURN: (void)
//
// PURPOSE: If sub is better than the best substructure on subList,
// then print it.  This should be called only if outputLevel > 3.
//---------------------------------------------------------------------------

void PrintNewBestSub (Substructure *sub, SubList *subList,
		      Parameters *parameters)
{
  ULONG outputLevel = parameters->outputLevel;

  if ((subList->head == NULL) ||
      (sub->value > subList->head->sub->value)) {
    parameters->outputLevel = 1; // turn off instance printing
    printf ("\nNew best ");
    PrintSub (sub, parameters);
    printf ("\n");
    parameters->outputLevel = outputLevel;
  }
}


//---------------------------------------------------------------------------
// NAME: AllocateInstance
//
// INPUTS: (ULONG v) - number of vertices in instance
//         (ULONG e) - number of edges in instance
//
// RETURN: (Instance *) - pointer to newly allocated instance
//
// PURPOSE: Allocate and return space for new instance.  matchCost
// should be set to some negative number indicating it has not yet
// been computed.
//---------------------------------------------------------------------------

Instance *AllocateInstance (ULONG v, ULONG e)
{
  Instance *instance;

  instance = (Instance *) malloc (sizeof (Instance));
  if (instance == NULL)
    OutOfMemoryError ("AllocateInstance:instance");
  instance->numVertices = v;
  instance->numEdges = e;
  instance->vertices = NULL;
  instance->edges = NULL;
  instance->newVertex = 0;
  instance->newEdge = 0;
  if (v > 0) {
    instance->vertices = (ULONG *) malloc (sizeof (ULONG) * v);
    if (instance->vertices == NULL)
      OutOfMemoryError ("AllocateInstance:instance->vertices");
  }
  if (e > 0) {
    instance->edges = (ULONG *) malloc (sizeof (ULONG) * e);
    if (instance->edges == NULL)
      OutOfMemoryError ("AllocateInstance:instance->edges");
  }
  instance->minMatchCost = MAX_DOUBLE;
  instance->refCount = 0;

  return instance;
}


//---------------------------------------------------------------------------
// NAME: FreeInstance
//
// INPUTS: (Instance *instance) - instance to free
//
// RETURN: (void)
//
// PURPOSE: Deallocate memory of given instance, if there are no more
// references to it.
//---------------------------------------------------------------------------

void FreeInstance (Instance *instance)
{
  if ((instance != NULL) && (instance->refCount == 0)) {
      free (instance->vertices);
      free (instance->edges);
      free (instance);
  }
}


//---------------------------------------------------------------------------
// NAME: AllocateInstanceListNode
//
// INPUTS: (Instance *instance) - instance to be stored in node
//
// RETURN: (InstanceListNode *) - newly allocated InstanceListNode
//
// PURPOSE: Allocate a new InstanceListNode.
//---------------------------------------------------------------------------

InstanceListNode *AllocateInstanceListNode (Instance *instance)
{
  InstanceListNode *instanceListNode;

  instanceListNode = (InstanceListNode *) malloc (sizeof (InstanceListNode));
  if (instanceListNode == NULL)
    OutOfMemoryError ("AllocateInstanceListNode:InstanceListNode");
  instanceListNode->instance = instance;
  instance->refCount++;
  instanceListNode->next = NULL;
  return instanceListNode;
}


//---------------------------------------------------------------------------
// NAME: FreeInstanceListNode
//
// INPUTS: (InstanceListNode *instanceListNode)
//
// RETURN: (void)
//
// PURPOSE: Free memory used by given instance list node.
//---------------------------------------------------------------------------

void FreeInstanceListNode (InstanceListNode *instanceListNode)
{
  if (instanceListNode != NULL) {
    if (instanceListNode->instance != NULL)
      instanceListNode->instance->refCount--;
    FreeInstance (instanceListNode->instance);
    free (instanceListNode);
  }
}


//---------------------------------------------------------------------------
// NAME: AllocateInstanceList
//
// INPUTS: (void)
//
// RETURN: (InstanceList *) - newly-allocated empty instance list
//
// PURPOSE: Allocate and return an empty instance list.
//---------------------------------------------------------------------------

InstanceList *AllocateInstanceList (void)
{
  InstanceList *instanceList;

  instanceList = (InstanceList *) malloc (sizeof (InstanceList));
  if (instanceList == NULL)
    OutOfMemoryError ("AllocateInstanceList:instanceList");
  instanceList->head = NULL;
  return instanceList;
}


//---------------------------------------------------------------------------
// NAME: FreeInstanceList
//
// INPUTS: (InstanceList *instanceList)
//
// RETURN: (void)
//
// PURPOSE: Deallocate memory of instance list.
//---------------------------------------------------------------------------

void FreeInstanceList (InstanceList *instanceList)
{
  InstanceListNode *instanceListNode;
  InstanceListNode *instanceListNode2;

  if (instanceList != NULL) {
    instanceListNode = instanceList->head;
    while (instanceListNode != NULL) {
      instanceListNode2 = instanceListNode;
      instanceListNode = instanceListNode->next;
      FreeInstanceListNode (instanceListNode2);
    }
    free (instanceList);
  }
}


//---------------------------------------------------------------------------
// NAME: MarkInstanceVertices
//
// INPUTS: (Instance *instance) - instance whose vertices to set
//         (Graph *graph) - graph containing instance
//         (BOOLEAN value) - value to set edge's used flag
//
// RETURN: (void)
//
// PURPOSE: Set the used flag to the given value for each vertex in
// instance.
//---------------------------------------------------------------------------

void MarkInstanceVertices (Instance *instance, Graph *graph, BOOLEAN value)
{
  ULONG v;

  for (v = 0; v < instance->numVertices; v++)
    graph->vertices[instance->vertices[v]].used = value;
}


//---------------------------------------------------------------------------
// NAME: MarkInstanceEdges
//
// INPUTS: (Instance *instance) - instance whose edges to set
//         (Graph *graph) - graph containing instance
//         (BOOLEAN value) - value to set edge's used flag
//
// RETURN: (void)
//
// PURPOSE: Set the used flag to the given value for each edge in
// instance.
//---------------------------------------------------------------------------

void MarkInstanceEdges (Instance *instance, Graph *graph, BOOLEAN value)
{
  ULONG e;

  for (e = 0; e < instance->numEdges; e++)
    graph->edges[instance->edges[e]].used = value;
}


//---------------------------------------------------------------------------
// NAME: PrintInstance
//
// INPUTS: (Instance *instance) - instance to print
//         (Graph *graph) - graph containing instance
//         (LabelList *labelList) - labels from graph
//
// RETURN: (void)
//
// PURPOSE: Print given instance.
//---------------------------------------------------------------------------

void PrintInstance (Instance *instance, Graph *graph, LabelList *labelList)
{
  ULONG i;

  if (instance != NULL) {
    for (i = 0; i < instance->numVertices; i++) {
      printf ("    ");
      PrintVertex (graph, instance->vertices[i], labelList);
    }
    for (i = 0; i < instance->numEdges; i++) {
      printf ("    ");
      PrintEdge (graph, instance->edges[i], labelList);
    }
  }
}


//---------------------------------------------------------------------------
// NAME: PrintInstanceList
//
// INPUTS: (InstanceList *instanceList) - list of instances
//         (Graph *graph) - graph containing instances
//         (LabelList *labelList) - labels used in input graph
//
// RETURN: (void)
//
// PURPOSE: Print array of instances.
//---------------------------------------------------------------------------

void PrintInstanceList (InstanceList *instanceList, Graph *graph,
                        LabelList *labelList)
{
  ULONG i = 0;
  InstanceListNode *instanceListNode;

  if (instanceList != NULL) {
    instanceListNode = instanceList->head;
    while (instanceListNode != NULL) {
      printf ("\n  Instance %lu:\n", i + 1);
      PrintInstance (instanceListNode->instance, graph, labelList);
      instanceListNode = instanceListNode->next;
      i++;
    }
  }
}


//---------------------------------------------------------------------------
// NAME: PrintPosInstanceList
//
// INPUTS: (Substructure *sub) - substructure containing positive instances
//         (Parameters *parameters)
//
// RETURN: (void)
//
// PURPOSE: Print array of sub's positive instances.
//---------------------------------------------------------------------------

void PrintPosInstanceList (Substructure *sub, Parameters *parameters)
{
  ULONG i;
  ULONG posEgNo;
  InstanceListNode *instanceListNode;

  // parameters used
  Graph *posGraph = parameters->posGraph;
  ULONG numPosEgs = parameters->numPosEgs;
  ULONG *posEgsVertexIndices = parameters->posEgsVertexIndices;
  LabelList *labelList = parameters->labelList;

  if (sub->instances != NULL) {
    instanceListNode = sub->instances->head;
    i = 1;
    while (instanceListNode != NULL) {
      printf ("\n  Instance %lu", i);
      if (numPosEgs > 1) {
	posEgNo = InstanceExampleNumber (instanceListNode->instance,
					 posEgsVertexIndices, numPosEgs);
	printf (" in positive example %lu:\n", posEgNo);
      } else printf (":\n");
      PrintInstance (instanceListNode->instance, posGraph, labelList);
      instanceListNode = instanceListNode->next;
      i++;
    }
  }
}


//---------------------------------------------------------------------------
// NAME: PrintNegInstanceList
//
// INPUTS: (Substructure *sub) - substructure containing negative instances
//         (Parameters *parameters)
//
// RETURN: (void)
//
// PURPOSE: Print array of sub's negative instances.
//---------------------------------------------------------------------------

void PrintNegInstanceList (Substructure *sub, Parameters *parameters)
{
  ULONG i;
  ULONG negEgNo;
  InstanceListNode *instanceListNode;

  // parameters used
  Graph *negGraph = parameters->negGraph;
  ULONG numNegEgs = parameters->numNegEgs;
  ULONG *negEgsVertexIndices = parameters->negEgsVertexIndices;
  LabelList *labelList = parameters->labelList;

  if (sub->negInstances != NULL) {
    instanceListNode = sub->negInstances->head;
    i = 1;
    while (instanceListNode != NULL) {
      printf ("\n  Instance %lu", i);
      if (numNegEgs > 1) {
	negEgNo = InstanceExampleNumber (instanceListNode->instance,
					 negEgsVertexIndices, numNegEgs);
	printf (" in negative example %lu:\n", negEgNo);
      } else printf (":\n");
      PrintInstance (instanceListNode->instance, negGraph, labelList);
      instanceListNode = instanceListNode->next;
      i++;
    }
  }
}


//---------------------------------------------------------------------------
// NAME: InstanceExampleNumber
//
// INPUTS: (Instance *instance) - instance to look for
//         (ULONG *egsVertexIndices) - vertex indices of graph's examples
//         (ULONG numEgs) - number of graph examples
//
// RETURN: (ULONG) - example number containing instance
//
// PURPOSE: Return which example contains the given instance.
//---------------------------------------------------------------------------

ULONG InstanceExampleNumber (Instance *instance, ULONG *egsVertexIndices,
			    ULONG numEgs)
{
  ULONG instanceVertexIndex;
  ULONG egNo;

  instanceVertexIndex = instance->vertices[0];
  egNo = 1;
  while ((egNo < numEgs) && (instanceVertexIndex >= egsVertexIndices[egNo]))
    egNo++;

  return egNo;
}


//---------------------------------------------------------------------------
// NAME: CountInstances
//
// INPUTS: (InstanceList *instanceList) - list of instances
//
// RETURN: (ULONG) - number of instances in instanceList.
//
// PURPOSE: Return number of instances in instance list.
//---------------------------------------------------------------------------

ULONG CountInstances (InstanceList *instanceList)
{
  ULONG i = 0;
  InstanceListNode *instanceListNode;

  if (instanceList != NULL) {
    instanceListNode = instanceList->head;
    while (instanceListNode != NULL) {
      i++;
      instanceListNode = instanceListNode->next;
    }
  }
  return i;
}


//---------------------------------------------------------------------------
// NAME: InstanceListInsert
//
// INPUTS: (Instance *instance) - instance to insert
//         (InstanceList *instanceList) - list to insert into
//         (BOOLEAN unique) - if TRUE, then instance inserted only if
//                            unique, and if not unique, deallocated
//
// RETURN: (void)
//
// PURPOSE: Insert given instance on to given instance list.  If
// unique=TRUE, then instance must not already exist on list, and if
// so, it is deallocated.  If unique=FALSE, then instance is merely
// inserted at the head of the instance list.
//---------------------------------------------------------------------------

void InstanceListInsert (Instance *instance, InstanceList *instanceList,
			 BOOLEAN unique)
{
  InstanceListNode *instanceListNode;

  if ((! unique) ||
      (unique && (! MemberOfInstanceList (instance, instanceList)))) {
    instanceListNode = AllocateInstanceListNode (instance);
    instanceListNode->next = instanceList->head;
    instanceList->head = instanceListNode;
  } else FreeInstance (instance);
}


//---------------------------------------------------------------------------
// NAME: MemberOfInstanceList
//
// INPUTS: (Instance *instance)
//         (InstanceList *instanceList)
//
// RETURN: (BOOLEAN)
//
// PURPOSE: Check if the given instance exactly matches an instance
// already on the given instance list.
//---------------------------------------------------------------------------

BOOLEAN MemberOfInstanceList (Instance *instance, InstanceList *instanceList)
{
  InstanceListNode *instanceListNode;
  BOOLEAN found = FALSE;

  if (instanceList != NULL) {
    instanceListNode = instanceList->head;
    while ((instanceListNode != NULL) && (! found)) {
      if (InstanceMatch (instance, instanceListNode->instance))
	found = TRUE;
      instanceListNode = instanceListNode->next;
    }
  }
  return found;
}

//---------------------------------------------------------------------------
// NAME: InstanceMatch
//
// INPUTS: (Instance *instance1)
//         (Instance *instance2)
//
// RETURN: (BOOLEAN) - TRUE if instance1 matches instance2
//
// PURPOSE: Determine if two instances are the same by checking if
// their vertices and edges arrays are the same.  NOTE: InstanceMatch
// assumes the instances' vertices and edges arrays are in increasing
// order.
//---------------------------------------------------------------------------

BOOLEAN InstanceMatch (Instance *instance1, Instance *instance2)
{
  ULONG i;

  // check that instances have same number of vertices and edges
  if ((instance1->numVertices != instance2->numVertices) ||
      (instance1->numEdges != instance2->numEdges))
    return FALSE;

  // check that instances have same edges
  for (i = 0; i < instance1->numEdges; i++)
    if (instance1->edges[i] != instance2->edges[i])
      return FALSE;
  
  // check that instances have same vertices
  for (i = 0; i < instance1->numVertices; i++)
    if (instance1->vertices[i] != instance2->vertices[i])
      return FALSE;

  return TRUE;
}


//---------------------------------------------------------------------------
// NAME: InstanceOverlap
//
// INPUTS: (Instance *instance1)
//         (Instance *instance2)
//
// RETURN: (BOOLEAN) - TRUE if instances overlap
//
// PURPOSE: Determine if given instances overlap, i.e., share at least
// one vertex.  NOTE: instance vertices arrays are assumed to be in
// increasing order.
//---------------------------------------------------------------------------

BOOLEAN InstanceOverlap (Instance *instance1, Instance *instance2)
{
  ULONG v1;
  ULONG i = 0;
  ULONG j = 0;
  BOOLEAN overlap = FALSE;
  ULONG nv1 = instance1->numVertices;
  ULONG nv2 = instance2->numVertices;

  while ((i < nv1) && (j < nv2) && (! overlap)) {
    v1 = instance1->vertices[i];
    while ((j < nv2) && (instance2->vertices[j] < v1))
      j++;
    if ((j < nv2) && (v1 == instance2->vertices[j]))
      overlap = TRUE;
    i++;
  }
  return overlap;
}


//---------------------------------------------------------------------------
// NAME: InstanceListOverlap
//
// INPUTS: (Instance *instance) - instance to check for overlap
//         (InstanceList *instanceList) - instances to check for overlap with
//                                        instance
//
// RETURN: (BOOLEAN)
//
// PURPOSE: Check if given instance overlaps at all with any instance
// in the given instance list.
//---------------------------------------------------------------------------

BOOLEAN InstanceListOverlap (Instance *instance, InstanceList *instanceList)
{
  InstanceListNode *instanceListNode;
  BOOLEAN overlap = FALSE;

  if (instanceList != NULL) {
    instanceListNode = instanceList->head;
    while ((instanceListNode != NULL) && (! overlap)) {
      if (instanceListNode->instance != NULL)
	if (InstanceOverlap (instance, instanceListNode->instance))
	  overlap = TRUE;
      instanceListNode = instanceListNode->next;
    }
  }
  return overlap;
}


//---------------------------------------------------------------------------
// NAME: InstancesOverlap
//
// INPUTS: (InstanceList *instanceList)
//
// RETURN: (BOOLEAN) - TRUE if any pair of instances overlap
//
// PURPOSE: Check if any two instances in the given list overlap.  If
// so, return TRUE, else return FALSE.
//---------------------------------------------------------------------------

BOOLEAN InstancesOverlap (InstanceList *instanceList)
{
  InstanceListNode *instanceListNode1;
  InstanceListNode *instanceListNode2;
  BOOLEAN overlap = FALSE;

  if (instanceList != NULL) {
    instanceListNode1 = instanceList->head;
    while ((instanceListNode1 != NULL) && (! overlap)) {
      instanceListNode2 = instanceListNode1->next;
      while ((instanceListNode2 != NULL) && (! overlap)) {
	if ((instanceListNode1->instance != NULL) &&
	    (instanceListNode2->instance != NULL) &&
	    (InstanceOverlap (instanceListNode1->instance,
			      instanceListNode2->instance)))
	  overlap = TRUE;
	instanceListNode2 = instanceListNode2->next;
      }
      instanceListNode1 = instanceListNode1->next;
    }
  }
  return overlap;
}


//---------------------------------------------------------------------------
// NAME: InstanceToGraph
//
// INPUTS: (Instance *instance) - instance to convert
//         (Graph *graph) - graph containing instance
//
// RETURN: (Graph *) - new graph equivalent to instance
//
// PURPOSE: Convert given instance to an equivalent Graph structure.
//---------------------------------------------------------------------------

Graph *InstanceToGraph (Instance *instance, Graph *graph)
{
  Graph *newGraph;
  Vertex *vertex;
  Edge *edge;
  ULONG i, j;
  ULONG v1, v2;
  BOOLEAN found1;
  BOOLEAN found2;

  v1 = 0;
  v2 = 0;
  newGraph = AllocateGraph (instance->numVertices, instance->numEdges);
 
  // convert vertices
  for (i = 0; i < instance->numVertices; i++) {
    vertex = & graph->vertices[instance->vertices[i]];
    newGraph->vertices[i].label = vertex->label;
    newGraph->vertices[i].numEdges = 0;
    newGraph->vertices[i].edges = NULL;
    newGraph->vertices[i].used = FALSE;
  }

  // convert edges
  for (i = 0; i < instance->numEdges; i++) {
    edge = & graph->edges[instance->edges[i]];
    // find new indices for edge vertices
    j = 0;
    found1 = FALSE;
    found2 = FALSE;
    while ((! found1) || (! found2)) {
      if (instance->vertices[j] == edge->vertex1) {
	v1 = j;
	found1 = TRUE;
      }
      if (instance->vertices[j] == edge->vertex2) {
	v2 = j;
	found2 = TRUE;
      }
      j++;
    }
    // set new edge information
    newGraph->edges[i].vertex1 = v1;
    newGraph->edges[i].vertex2 = v2;
    newGraph->edges[i].label = edge->label;
    newGraph->edges[i].directed = edge->directed;
    newGraph->edges[i].used = FALSE;
    // add edge to appropriate vertices
    vertex = & newGraph->vertices[v1];
    vertex->numEdges++;
    vertex->edges =
      (ULONG *) realloc (vertex->edges, sizeof (ULONG) * vertex->numEdges);
    if (vertex->edges == NULL)
      OutOfMemoryError ("InstanceToGraph:vertex1->edges");
    vertex->edges[vertex->numEdges - 1] = i;
    if (v1 != v2) {
      vertex = & newGraph->vertices[v2];
      vertex->numEdges++;
      vertex->edges =
	(ULONG *) realloc (vertex->edges, sizeof (ULONG) * vertex->numEdges);
      if (vertex->edges == NULL)
	OutOfMemoryError ("InstanceToGraph:vertex2->edges");
      vertex->edges[vertex->numEdges - 1] = i;
    }
  }
  return newGraph;
}
