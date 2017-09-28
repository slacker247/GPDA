//---------------------------------------------------------------------------
// extend.c
//
// Functions for extending a substructure.
//
// Subdue 5
//---------------------------------------------------------------------------

#include "subdue.h"


//---------------------------------------------------------------------------
// NAME: ExtendSub
//
// INPUTS: (Substructure *sub) - substructure to be extended
//         (Parameters *parameters)
//
// RETURN: (SubList *) - list of extended substructures
//
// PURPOSE: Return list of substructures representing extensions to
// the given substructure.  Extensions are constructed by adding an
// edge (or edge and new vertex) to each positive instance of the
// given substructure in all possible ways according to the graph.
// Matching extended instances are collected into new extended
// substructures, and all such extended substructures are returned.
// If the negative graph is present, then instances of the
// substructure in the negative graph are also collected.
//---------------------------------------------------------------------------

SubList *ExtendSub (Substructure *sub, Parameters *parameters)
{
  InstanceList *negInstanceList;
  InstanceList *newInstanceList;
  InstanceListNode *newInstanceListNode;
  Instance *newInstance;
  Substructure *newSub;
  SubList *extendedSubs;
  SubListNode *newSubListNode = NULL;

  // parameters used
  Graph *posGraph = parameters->posGraph;
  Graph *negGraph = parameters->negGraph;
  LabelList *labelList = parameters->labelList;

  extendedSubs = AllocateSubList ();
  newInstanceList = ExtendInstances (sub->instances, posGraph);
  negInstanceList = NULL;
  if (negGraph != NULL)
    negInstanceList = ExtendInstances (sub->negInstances, negGraph);
  newInstanceListNode = newInstanceList->head;
  while (newInstanceListNode != NULL) {
    newInstance = newInstanceListNode->instance;
    if (newInstance->minMatchCost != 0.0) {
      // minMatchCost=0.0 means the instance is an exact match to a
      // previously-generated sub, so a sub created from this instance
      // would be a duplicate of one already on the extendedSubs list
      newSub = CreateSubFromInstance (newInstance, posGraph);
      if (! MemberOfSubList (newSub, extendedSubs, labelList)) {
	AddPosInstancesToSub (newSub, newInstanceList, parameters);
	if (negInstanceList != NULL)
	  AddNegInstancesToSub (newSub, negInstanceList, parameters);
	// add newSub to head of extendedSubs list
	newSubListNode = AllocateSubListNode (newSub);
	newSubListNode->next = extendedSubs->head;
	extendedSubs->head = newSubListNode;
      }
    }
    newInstanceListNode = newInstanceListNode->next;
  }
  FreeInstanceList (negInstanceList);
  FreeInstanceList (newInstanceList);
  return extendedSubs;
}


//---------------------------------------------------------------------------
// NAME: ExtendInstances
//
// INPUTS: (InstanceList *instanceList) - instances to be extended
//         (Graph *graph) - graph containing substructure instances
//
// RETURN: (InstanceList *) - list of extended instances
//
// PURPOSE: Create and return a list of new instances by extending the
// given substructure's instances by one edge (or edge and new vertex)
// in all possible ways based on given graph.
//---------------------------------------------------------------------------

InstanceList *ExtendInstances (InstanceList *instanceList, Graph *graph)
{
  InstanceList *newInstanceList;
  InstanceListNode *instanceListNode;
  Instance *instance;
  Instance *newInstance;
  ULONG v;
  ULONG e;
  Vertex *vertex;
  Edge *edge;

  newInstanceList = AllocateInstanceList ();
  instanceListNode = instanceList->head;
  while (instanceListNode != NULL) {
    instance = instanceListNode->instance;
    MarkInstanceEdges (instance, graph, TRUE);
    for (v = 0; v < instance->numVertices; v++) {
      vertex = & graph->vertices[instance->vertices[v]];
      for (e = 0; e < vertex->numEdges; e++) {
        edge = & graph->edges[vertex->edges[e]];
        if (! edge->used) {
          // add new instance to list
          newInstance =
	    CreateExtendedInstance (instance, instance->vertices[v],
				    vertex->edges[e], graph);
	  InstanceListInsert (newInstance, newInstanceList, TRUE);
        }
      }
    }
    MarkInstanceEdges (instance, graph, FALSE);
    instanceListNode = instanceListNode->next;
  }
  return newInstanceList;
}


//---------------------------------------------------------------------------
// NAME: CreateExtendedInstance
//
// INPUTS: (Instance *instance) - instance being extended
//         (ULONG v) - vertex in graph where new edge being added
//         (ULONG e) - edge in graph being added to instance
//         (Graph *graph) - graph containing instance and new edge
//
// RETURN: (Instance *) - new extended instance
//
// PURPOSE: Create and return a new instance, which is a copy of the
// given instance extended by one edge e along vertex v.  Edge e may
// introduce a new vertex.  Make sure that instance's vertices and
// edges arrays are kept in increasing order, which is important for
// fast instance matching.
//---------------------------------------------------------------------------

Instance *CreateExtendedInstance (Instance *instance, ULONG v, ULONG e,
                                  Graph *graph)
{
  Instance *newInstance;
  ULONG v2;
  BOOLEAN found = FALSE;
  ULONG i;

  // get edge's other vertex
  if (graph->edges[e].vertex1 == v)
    v2 = graph->edges[e].vertex2;
  else v2 = graph->edges[e].vertex1;

  // check if edge's other vertex is already in instance
  for (i = 0; ((i < instance->numVertices) && (! found)); i++)
    if (instance->vertices[i] == v2)
      found = TRUE;
  if (! found)
    newInstance = AllocateInstance (instance->numVertices + 1,
				    instance->numEdges + 1);
  else newInstance = AllocateInstance (instance->numVertices,
				       instance->numEdges + 1);

  // set vertices of new instance, kept in increasing order
  for (i = 0; i < instance->numVertices; i++)
    newInstance->vertices[i] = instance->vertices[i];
  newInstance->newVertex = VERTEX_UNMAPPED;
  if (! found) {
    i = instance->numVertices;
    while ((i > 0) && (v2 < newInstance->vertices[i-1])) {
      newInstance->vertices[i] = newInstance->vertices[i-1];
      i--;
    }
    newInstance->vertices[i] = v2;
    newInstance->newVertex = i;
  }

  // set edges of new instance, kept in increasing order
  for (i = 0; i < instance->numEdges; i++)
    newInstance->edges[i] = instance->edges[i];
  i = instance->numEdges;
  while ((i > 0) && (e < newInstance->edges[i-1])) {
    newInstance->edges[i] = newInstance->edges[i-1];
    i--;
  }
  newInstance->edges[i] = e;
  newInstance->newEdge = i;

  return newInstance;
}


//---------------------------------------------------------------------------
// NAME: CreateSubFromInstance
//
// INPUTS: (Instance *instance) - instance
//         (Graph *graph) - graph containing instance
//
// RETURN: (Substructure *) - new substructure equivalent to instance
//
// PURPOSE: Create and return a new substructure based on the given
// instance.  Right now, the substructure is identical to the
// instance, but may be different. (*****)
//---------------------------------------------------------------------------

Substructure *CreateSubFromInstance (Instance *instance, Graph *graph)
{
  Substructure *newSub = AllocateSub ();
  newSub->definition = InstanceToGraph (instance, graph);
  return newSub;
}


//---------------------------------------------------------------------------
// NAME: AddPosInstancesToSub
//
// INPUTS: (Substructure *sub) - substructure to collect instances
//         (InstanceList *instanceList) - instances to collect from in
//           positive graph
//         (Parameters *parameters)
//
// RETURN: (void)
//
// PURPOSE: Add instance from instanceList to sub's positive
// instances if the instance matches sub's definition.  If
// allowInstanceOverlap=FALSE, then instances added only if they do
// not overlap with existing instances.
//---------------------------------------------------------------------------

void AddPosInstancesToSub (Substructure *sub, InstanceList *instanceList,
			   Parameters *parameters)
{
  InstanceListNode *instanceListNode;
  Instance *instance;
  Graph *instanceGraph;
  double thresholdLimit;
  double matchCost;

  // parameters used
  Graph *posGraph              = parameters->posGraph;
  LabelList *labelList         = parameters->labelList;
  BOOLEAN allowInstanceOverlap = parameters->allowInstanceOverlap;
  double threshold             = parameters->threshold;

  // collect positive instances of substructure
  if (instanceList != NULL) {
    sub->instances = AllocateInstanceList ();
    instanceListNode = instanceList->head;
    while (instanceListNode != NULL) {
      if (instanceListNode->instance != NULL) {
	instance = instanceListNode->instance;
	if (allowInstanceOverlap ||
	    (! InstanceListOverlap (instance, sub->instances))) {
	  thresholdLimit = threshold *
	    (instance->numVertices + instance->numEdges);
	  instanceGraph = InstanceToGraph (instance, posGraph);
	  if (GraphMatch (sub->definition, instanceGraph, labelList,
			  thresholdLimit, & matchCost, NULL)) {
	    if (matchCost < instance->minMatchCost)
	      instance->minMatchCost = matchCost;
	    InstanceListInsert (instance, sub->instances, FALSE);
	    sub->numInstances++;
	  }
	  FreeGraph (instanceGraph);
	}
      }
      instanceListNode = instanceListNode->next;
    }
  }
}


//---------------------------------------------------------------------------
// NAME: AddNegInstancesToSub
//
// INPUTS: (Substructure *sub) - substructure to collect instances
//         (InstanceList *instanceList) - instances to collect from in
//           negative graph
//         (Parameters *parameters)
//
// RETURN: (void)
//
// PURPOSE: Add instance from instanceList to sub's negative
// instances if the instance matches sub's definition.  If
// allowInstanceOverlap=FALSE, then instances added only if they do
// not overlap with existing instances.
//---------------------------------------------------------------------------

void AddNegInstancesToSub (Substructure *sub, InstanceList *instanceList,
			   Parameters *parameters)
{
  InstanceListNode *instanceListNode;
  Instance *instance;
  Graph *instanceGraph;
  double thresholdLimit;
  double matchCost;

  // parameters used
  Graph *negGraph              = parameters->negGraph;
  LabelList *labelList         = parameters->labelList;
  BOOLEAN allowInstanceOverlap = parameters->allowInstanceOverlap;
  double threshold             = parameters->threshold;

  // collect negative instances of substructure
  if (instanceList != NULL) {
    sub->negInstances = AllocateInstanceList ();
    instanceListNode = instanceList->head;
    while (instanceListNode != NULL) {
      if (instanceListNode->instance != NULL) {
	instance = instanceListNode->instance;
	if (allowInstanceOverlap ||
	    (! InstanceListOverlap (instance, sub->negInstances))) {
	  thresholdLimit = threshold *
	    (instance->numVertices + instance->numEdges);
	  instanceGraph = InstanceToGraph (instance, negGraph);
	  if (GraphMatch (sub->definition, instanceGraph, labelList,
			  thresholdLimit, & matchCost, NULL)) {
	    if (matchCost < instance->minMatchCost)
	      instance->minMatchCost = matchCost;
	    InstanceListInsert (instance, sub->negInstances, FALSE);
	    sub->numNegInstances++;
	  }
	  FreeGraph (instanceGraph);
	}
      }
      instanceListNode = instanceListNode->next;
    }
  }
}
