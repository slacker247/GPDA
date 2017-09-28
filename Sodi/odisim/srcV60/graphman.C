// graphman.C method file

#include <stdio.h>
#include <math.h>
#include "graphman.H"
#include "def.h"

/************************************************************************
* C_GRAPHMAN : construct a graphman object				*
************************************************************************/
C_GRAPHMAN::C_GRAPHMAN() {
  int i;
  C_BASETYPE *parameters;
  C_ITEM *item;

  parameters = graphics_parser->get_basetype("parameters");

  first_time = parameters->get_float("first_time");
  start_time = parameters->get_float("start_time");
  cycle_time = parameters->get_float("cycle_time");

  graphobj = new C_GRAPHOBJ();

  printf("GRAPHMAN object created\n");

//...... set up the initial blocking condition

  if (NODE == 0) {
    graphobj->set_blocking(); 
    item = new C_ITEM();
    item->set_time_tag(first_time);
    item->set_id(-graphobj->get_GLOBAL_ID());
    add_block(item);
  }

//...... tell SPEEDES about the graph

  N_TOT = N_NODES;
  N_LOC = 1;

  TOTAL_OBJECTS += N_TOT;
  reserve_n_objects(N_LOC);
  for (i=0; i<N_LOC; i++) define_object(&graphobj[i]);
  set_interact();

}

/************************************************************************
* init_events : initialize events for the grid element objects		*
************************************************************************/
void C_GRAPHMAN::init_events() {

  printf("GRAPHMAN initializing\n");

}



