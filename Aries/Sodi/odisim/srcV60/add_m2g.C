// add_m2g.C method file

#include <stdio.h>
#include <time.h>
#include <math.h>

#include "add_m2g.H"
#include "add_m2g_mess.H"
#include "add_s2m_mess.H"
#include "gridman.H"
#include "oid.H"

#include "def.h"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;
extern int GRID;
extern int ADD_M2G;
extern int ADD_S2M;

/************************************************************************
* C_ADD_M2G : construct a add_m2g object				*
************************************************************************/
C_ADD_M2G::C_ADD_M2G() {
  set_lazy();
}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_ADD_M2G::init(C_HEADER *header) {
  ADD_M2G_MESS *add_m2g_mess;

  add_m2g_mess = (ADD_M2G_MESS *)header;
  obj_id = add_m2g_mess->object_id;
  obj_type = add_m2g_mess->object_type;
  obj_node = add_m2g_mess->object_node;
  unique_id = add_m2g_mess->unique_id;

}

/************************************************************************
* exchange : exchange state variables                                   *
************************************************************************/
void C_ADD_M2G::exchange() {
  C_GRID *grid;

  grid = (C_GRID *)SIMOBJ;
  grid->exchange_tmovers(tmovers);

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_ADD_M2G::temp_process() {
  C_GRIDMAN *gridman;
  C_GRID *grid;
  C_XQUEUE *movers;
  C_XQUEUE *sensors;
  C_OID *oid;
  C_ID_ARRAY *id_array;
  int i;
  int len;
  //ADD_S2M_MESS *add_s2m_mess;
  char *bf;
  //char s[120];
  int n_send;
  int index;

//...... initialize the free list manager

  RB_DEFINE_FREE(&freeobjs);

//...... get the list of moving objects from the grid

  gridman = (C_GRIDMAN *)get_manager(GRID);
  grid = (C_GRID *)SIMOBJ;
  movers = grid->get_movers();
  tmovers = TIME_TAG;
  grid->update_mover_counter();
  lazy_tsensors = grid->get_tsensors();
  lazy_sensor_counter = grid->get_sensor_counter();

//...... create a new oid object and give it to the mover list

  oid = (C_OID *)RB_FREE_NEW(OID);
  oid->set_object_id(obj_id);
  oid->set_object_type(obj_type);
  oid->set_object_node(obj_node);
  oid->set_unique_id(unique_id);

  *movers += oid;

//...... send the sensor list to the mover

  sensors = grid->get_sensors();
  len = sensors->get_length();
  n_send = len;

  oid = (C_OID *)sensors->get_top();
  for (i=0; i<len; i++) {
    if (oid->get_unique_id() == unique_id) {
      n_send--;
      break;
    }
    oid = (C_OID *)oid->get_link();
  }

  if (n_send > 0) {

    /*add_s2m_mess = (ADD_S2M_MESS *)*/schedule(
		LONE * (TIME_TAG + gridman->get_delay_g2m()),
		ADD_S2M,
		obj_type,
		obj_id,
		obj_node,
		n_send*sizeof(C_ID_ARRAY),
		bf);

    id_array = (C_ID_ARRAY *)bf;

    oid = (C_OID *)sensors->get_top();
    index = 0;
    for (i=0; i<len; i++) {
      if (oid->get_unique_id() != unique_id) {
        id_array[index].object_id = oid->get_object_id();
        id_array[index].object_type = oid->get_object_type();
        id_array[index].object_node = oid->get_object_node();
        id_array[index].unique_id = oid->get_unique_id();
	index++;
      }
      oid = (C_OID *)oid->get_link();
    }

  }

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_ADD_M2G::perm_process() {

}

/************************************************************************
* cleanup : clean up data structures used                               *
************************************************************************/
void C_ADD_M2G::cleanup() {


}

/************************************************************************
* lazy : lazy cancellation check                                        *
************************************************************************/
int C_ADD_M2G::lazy() {
  C_GRID *grid;

  grid = (C_GRID *)SIMOBJ;

  if (lazy_sensor_counter == grid->get_sensor_counter()) {
//    fprintf(stderr,"ADD_M2G> Good news! Passed lazy cancellation\n");
    return 1;
  }

  return 0;

}
