// nostateman.C method file

#include <stdio.h>
#include "nostateman.H"
#include "def.h"

/************************************************************************
* C_NOSTATEMAN : construct a nostateman object				*
************************************************************************/
C_NOSTATEMAN::C_NOSTATEMAN() {
  char s[80];
  char *no_name;

  NOSTATE = object_type("NOSTATE");
  nostateobj = new C_NOSTATEOBJ();

  sprintf(s,"NOSTATE%3.3d",NODE);
  no_name = strdup(s);
  nostateobj->set_NAME(no_name);

  printf("NOSTATEMAN object %s created\n",no_name);

//...... tell SPEEDES about the nostate

  N_TOT = N_NODES;
  N_LOC = 1;

  TOTAL_OBJECTS += N_TOT;
  reserve_n_objects(N_LOC);
  define_object(nostateobj);
  set_interact();

}

/************************************************************************
* init_events : initialize events for the grid element objects		*
************************************************************************/
void C_NOSTATEMAN::init_events() {

  printf("NOSTATEMAN initializing... (nothing to do)\n");

}



