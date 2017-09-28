// add_s2m.C method file

#include <stdio.h>
#include <time.h>
#include <math.h>

#include "add_s2m.H"
#include "add_s2m_mess.H"
#include "add_e2e_mess.H"
#include "add_s2e_mess.H"
#include "gridman.H"
#include "eomanman.H"
#include "mover.H"
#include "oid.H"

#include "def.h"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

int C_ADD_S2M::done = 0;
int C_ADD_S2M::ADD_S2M = 0;
int C_ADD_S2M::ADD_E2E = 0;
int C_ADD_S2M::ADD_S2E = 0;
int C_ADD_S2M::GRID = 0;
int C_ADD_S2M::EOMAN = 0;
int C_ADD_S2M::size_node_flag = 0;

/************************************************************************
* C_ADD_S2M : construct a add_s2m object				*
************************************************************************/
C_ADD_S2M::C_ADD_S2M() {

  if (!done) {
    done = 1;
    GRID = object_type("GRID");
    EOMAN = object_type("EOMAN");
    ADD_S2M = event_type("ADD_S2M");
    ADD_E2E = event_type("ADD_E2E");
    ADD_S2E = event_type("ADD_S2E");
    size_node_flag = N_NODES/sizeof(int);
    if (N_NODES % sizeof(int)) size_node_flag++;
  }

  buff = NULL;
  node_flag = new int[size_node_flag];

}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_ADD_S2M::init(C_HEADER *header) {
  ADD_S2M_MESS *add_s2m_mess;
  char *buffin;

  add_s2m_mess = (ADD_S2M_MESS *)header;
  buffin = (char *)add_s2m_mess + sizeof(ADD_S2M_MESS);
  size = add_s2m_mess->bytes;

  if (size == 0) {
    buff = NULL;
  }else{
    buff = new char[size];
    memcpy(buff,buffin,size);
  }

}

/************************************************************************
* exchange : exchange state variables                                   *
************************************************************************/
void C_ADD_S2M::exchange() {
  C_MOVER *mover;

  mover = (C_MOVER *)SIMOBJ;
  mover->exchange_tsensors(tsensors);
  mover->exchange_node_flag(node_flag);

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_ADD_S2M::temp_process() {
  int i, j, len, n_sensors;
  double delay;
  double gone;
  C_MOVER *mover;
  C_XQUEUE *sensors;
  C_XQUEUE *script;
  C_OID *oid;
  C_ID_ARRAY *id_array;
  C_GRIDMAN *gridman;
  ADD_E2E_MESS *add_e2e_mess;
  ADD_S2E_MESS *add_s2e_mess;
  C_EOM *e;
  char *bf;
  C_EOMANMAN *eomanman;
  int hash_lid;
  char s[120];
  int bytes;

//...... initialize the free list manager

  RB_DEFINE_FREE(&freeobjs);

//...... get the eoman information

  eomanman = (C_EOMANMAN *)get_manager(EOMAN);
  hash_lid = eomanman->get_hash_lid(SIMOBJ->get_GLOBAL_ID());

//...... get the list of sensors from the mover object

  gridman = (C_GRIDMAN *)get_manager(GRID);
  delay = gridman->get_delay_m2e();

  mover = (C_MOVER *)SIMOBJ;
  sensors = mover->get_sensors();
  script = mover->get_script();

  if (size == 0) {
    tsensors = mover->get_tsensors();
  }else{
    tsensors = TIME_TAG;
  }

//...... get an initial copy of the node flag

  mover->copy_node_flag(node_flag);

//...... create a new oid object and give it to the mover list

  n_sensors = size / sizeof(C_ID_ARRAY);
  id_array = (C_ID_ARRAY *)buff;
  gone = GONE;

  for (i=0; i<n_sensors; i++) {

    oid = (C_OID *)RB_FREE_NEW(OID);

    oid->set_object_id(id_array[i].object_id);
    oid->set_object_type(id_array[i].object_type);
    oid->set_object_node(id_array[i].object_node);
    oid->set_unique_id(id_array[i].unique_id);

/*
  if ((SIMOBJ->get_GLOBAL_ID() == 2387) 
	&& (oid->get_unique_id() == 2781)
	&& (TIME_TAG > 47574.5)) {
    sprintf(s,"Adding sensor 2781 to mover 2387 at %f\n",TIME_TAG);
    RB_PRINT(stderr,s);
  }
*/

//...... update the eoman/sensor objects

    if (sensors->find(id_array[i].unique_id)) {

//...... it is already there so do nothing

      if (mover->get_node_flag(id_array[i].object_node) == 0) {
        sprintf(s,"Error (ADD_S2M)\n");
        RB_PRINT(stderr,s);
	mover->set_node_flag(id_array[i].object_node);
      }

    }else{

//...... this is a new sensor so we must add it to the list

      e = (C_EOM *)script->get_bot();
      if (e != NULL) {

        add_s2e_mess = (ADD_S2E_MESS *)schedule(
		gone * (TIME_TAG + delay),
		ADD_S2E,
		EOMAN,
		hash_lid,
		oid->get_object_node());

        add_s2e_mess->object_id = oid->get_object_id();
        add_s2e_mess->object_type = oid->get_object_type();
        add_s2e_mess->object_node = oid->get_object_node();
        add_s2e_mess->unique_id = oid->get_unique_id();
        add_s2e_mess->mover_unique_id = mover->get_GLOBAL_ID();

      }

//...... send the current script over to the eoman

      if (!mover->get_node_flag(id_array[i].object_node)) {

	len = script->get_length();
        e = (C_EOM *)script->get_top();
	bytes = 0;
	for (j=0; j<len; j++) {
	  if (e->get_endtime() > (TIME_TAG+delay)) {
	    bytes += freeobjs.get_size(e);
	  }
	  e = (C_EOM *)e->get_link();
	}

	mover->set_node_flag(id_array[i].object_node);
        add_e2e_mess = (ADD_E2E_MESS *)schedule(
		TIME_TAG + delay,
		ADD_E2E,
		EOMAN,
		hash_lid,
		oid->get_object_node(),
		bytes,
		bf);

	add_e2e_mess->unique_id = SIMOBJ->get_GLOBAL_ID();
	add_e2e_mess->script_time = mover->get_script_time();

	bytes = 0;
        e = (C_EOM *)script->get_top();
	for (j=0; j<len; j++) {
	  if (e->get_endtime() > (TIME_TAG+delay)) {
	    memcpy(&bf[bytes], (char *)e, freeobjs.get_size(e));
	    bytes += freeobjs.get_size(e);
	  }
	  e = (C_EOM *)e->get_link();
	}

      }

    }

    *sensors += oid;

  }

  mover->exchange_node_flag(node_flag);

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_ADD_S2M::perm_process() {

}

/************************************************************************
* cleanup : clean up data structures used                               *
************************************************************************/
void C_ADD_S2M::cleanup() {

  if (size != 0) delete [] buff; //RVI 2/18/98
  buff = NULL;

}
