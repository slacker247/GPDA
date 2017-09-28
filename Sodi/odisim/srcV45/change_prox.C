// change_prox.C method file

#include <stdio.h>
#include <time.h>
#include <math.h>
#include "add_e2e_mess.H"
#include "change_prox.H"
#include "gridman.H"
#include "eomanman.H"
#include "eom.H"
#include "def.h"
#include "mover.H"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

int C_CHANGE_PROX::done = 0;
int C_CHANGE_PROX::ADD_E2E = 0;
int C_CHANGE_PROX::NEXT_SCRIPT = 0;
int C_CHANGE_PROX::GRID = 0;
int C_CHANGE_PROX::EOMAN = 0;

/************************************************************************
* C_CHANGE_PROX : construct a change_prox object			*
************************************************************************/
C_CHANGE_PROX::C_CHANGE_PROX() {

  if (!done) {
    EOMAN = object_type("EOMAN");
    GRID = object_type("GRID");
    ADD_E2E = event_type("ADD_E2E");
    NEXT_SCRIPT = event_type("NEXT_SCRIPT");
    done = 1;
  }

}

/************************************************************************
* update_prox : update proximity detection based on the new script	*
************************************************************************/
void C_CHANGE_PROX::update_prox(double new_time, C_XQUEUE *script) {
  C_MOVER *mover;
  C_GRIDMAN *gridman;
  C_EOMANMAN *eomanman;
  int i, j, len;
  int bytes;
  int copybytes;
  C_EOM *e;
  double delay;
  double tsched;
  int hash_lid;
  char *bf;
  ADD_E2E_MESS *add_e2e_mess;

//...... determine the time for the new script to arive

  mover = (C_MOVER *)SIMOBJ;
  gridman = (C_GRIDMAN *)get_manager(GRID);
  delay = gridman->get_delay_m2e();
  tsched = TIME_TAG + delay;
  if (new_time < tsched) tsched = new_time;

//...... get the local hash id for the eoman

  eomanman = (C_EOMANMAN *)get_manager(EOMAN);
  hash_lid = eomanman->get_hash_lid(SIMOBJ->get_GLOBAL_ID());

//...... find out how many bytes to send

  len = script->get_length();
  e = (C_EOM *)script->get_top();
  bytes = 0;
  for (j=0; j<len; j++) {
    if (e->get_endtime() > tsched) {
      bytes += freeobjs.get_size(e);
    }
    e = (C_EOM *)e->get_link();
  }

//...... send script to nodes that need it

  for (i=0; i<N_NODES; i++) {
    if (mover->get_node_flag(i)) {

      add_e2e_mess = (ADD_E2E_MESS *)schedule(
	tsched,
	ADD_E2E,
	EOMAN,
	hash_lid,
	i,
	bytes,
	bf);

      add_e2e_mess->unique_id = SIMOBJ->get_GLOBAL_ID();
      add_e2e_mess->script_time = mover->get_script_time();

      copybytes = 0;
      e = (C_EOM *)script->get_top();
      for (j=0; j<len; j++) {
	if (e->get_endtime() > tsched) {
	  memcpy(&bf[copybytes], (char *)e, freeobjs.get_size(e));
	  copybytes += freeobjs.get_size(e);
	}
	e = (C_EOM *)e->get_link();
      }

    }

  }

//...... schedule an event to update the script

  e = (C_EOM *)script->get_top();
  schedule (e->get_endtime(), NEXT_SCRIPT, OBJECT_TYPE, LOCAL_ID, NODE);

}

