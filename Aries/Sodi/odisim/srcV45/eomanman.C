// eomanman.C method file

#include <stdio.h>
#include <math.h>
#include "eomanman.H"
#include "def.h"

/************************************************************************
* C_EOMANMAN : construct a eomanman object				*
************************************************************************/
C_EOMANMAN::C_EOMANMAN() {
  int i;
  C_BASETYPE *basetype;

  EOMAN = object_type("EOMAN");

//...... create the eomans

  basetype = grid_parser->get_basetype("grid");
  n_eomans = basetype->get_int("n_eomans");
  eoman = new C_EOMAN[n_eomans];

  printf("EOMANMAN objects created (%d)\n",n_eomans);

//...... tell SPEEDES about the eoman

  N_TOT = N_NODES*n_eomans;
  N_LOC = n_eomans;

  TOTAL_OBJECTS += N_TOT;
  reserve_n_objects(N_LOC);
  for (i=0; i<N_LOC; i++) define_object(&eoman[i]);
  reset_interact();

}

/************************************************************************
* init_events : initialize events for the grid element objects		*
************************************************************************/
void C_EOMANMAN::init_events() {

  printf("EOMANMAN initializing\n");

}



