// add_s2g.C method file

#include <stdio.h>
#include <time.h>
#include <math.h>

#include "add_s2g.H"
#include "add_s2g_mess.H"
#include "add_s2m_mess.H"
#include "gridman.H"
#include "oid.H"

#include "def.h"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

int C_ADD_S2G::done = 0;
int C_ADD_S2G::ADD_S2G = 0;
int C_ADD_S2G::ADD_S2M = 0;
int C_ADD_S2G::GRID = 0;

/************************************************************************
* C_ADD_S2G : construct a add_s2g object				*
************************************************************************/
C_ADD_S2G::C_ADD_S2G() {

  if (!done) {
    done = 1;
    ADD_S2G = event_type("ADD_S2G");
    ADD_S2M = event_type("ADD_S2M");
    GRID = object_type("GRID");
  }

  set_lazy();

}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_ADD_S2G::init(C_HEADER *header) {
  ADD_S2G_MESS *add_s2g_mess;

  add_s2g_mess = (ADD_S2G_MESS *)header;

  obj_id = add_s2g_mess->object_id;
  obj_type = add_s2g_mess->object_type;
  obj_node = add_s2g_mess->object_node;
  unique_id = add_s2g_mess->unique_id;

}

/************************************************************************
* exchange : exchange state variables                                   *
************************************************************************/
void C_ADD_S2G::exchange() {
  C_GRID *grid;

  grid = (C_GRID *)SIMOBJ;
  grid->exchange_tsensors(tsensors);

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_ADD_S2G::temp_process() {
  C_GRIDMAN *gridman;
  int len,i;
  C_GRID *grid;
  C_XQUEUE *sensors;
  C_XQUEUE *movers;
  C_OID *sensor_oid;
  C_OID *mover_oid;
  char s[120];
  C_ID_ARRAY *id_array;
  double tsched;
  char *bf;
  //ADD_S2M_MESS *add_s2m_mess;

//...... initialize the free list manager

  RB_DEFINE_FREE(&freeobjs);

//...... get the list of moving objects from the grid

  grid = (C_GRID *)SIMOBJ;
  gridman = (C_GRIDMAN *)get_manager(GRID);
  sensors = grid->get_sensors();
  tsensors = TIME_TAG;
  grid->update_sensor_counter();
  lazy_tmovers = grid->get_tmovers();
  lazy_mover_counter = grid->get_mover_counter();

//...... create a new oid object and give it to the mover list

  if (!sensors->find(unique_id)) {

    sensor_oid = (C_OID *)RB_FREE_NEW(OID);
    sensor_oid->set_object_id(obj_id);
    sensor_oid->set_object_type(obj_type);
    sensor_oid->set_object_node(obj_node);
    sensor_oid->set_unique_id(unique_id);

    *sensors += sensor_oid;

    movers = grid->get_movers();
    len = movers->get_length();
    mover_oid = (C_OID *)movers->get_top();
    tsched = TIME_TAG + gridman->get_delay_g2m();

    for (i=0; i<len; i++) {

      if (sensor_oid->get_unique_id() != mover_oid->get_unique_id()) {
        /*add_s2m_mess = (ADD_S2M_MESS *)*/schedule(
		tsched,
		ADD_S2M,
		mover_oid->get_object_type(),
		mover_oid->get_object_id(),
		mover_oid->get_object_node(),
		sizeof(C_ID_ARRAY),
		bf);

        id_array = (C_ID_ARRAY *)bf;
        id_array->object_id = sensor_oid->get_object_id();
        id_array->object_type = sensor_oid->get_object_type();
        id_array->object_node = sensor_oid->get_object_node();
        id_array->unique_id = sensor_oid->get_unique_id();
      }

      mover_oid = (C_OID *)mover_oid->get_link();

    }


  }else{

    sscanf(s,"Error (ADD_S2G) sensor %d already there\n",&unique_id);
    RB_PRINT(stderr,s);

  }

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_ADD_S2G::perm_process() {

}

/************************************************************************
* cleanup : clean up data structures used                               *
************************************************************************/
void C_ADD_S2G::cleanup() {

}

/************************************************************************
* lazy : lazy cancellation check                                        *
************************************************************************/
int C_ADD_S2G::lazy() {
  C_GRID *grid;

  grid = (C_GRID *)SIMOBJ;

  if (lazy_mover_counter == grid->get_mover_counter()) {
//    fprintf(stderr,"ADD_S2G> Good news! Passed lazy cancellation\n");
    return 1;
  }

  return 0;

}
