// del_mfg.C method file

#include <stdio.h>
#include <time.h>
#include <math.h>

#include "del_mfg.H"
#include "del_mfg_mess.H"
#include "del_sfm_mess.H"
#include "gridman.H"
#include "oid.H"

#include "def.h"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

int C_DEL_MFG::done = 0;
int C_DEL_MFG::DEL_MFG = 0;
int C_DEL_MFG::DEL_SFM = 0;
int C_DEL_MFG::GRID = 0;

/************************************************************************
* C_DEL_MFG : construct a del_mfg object				*
************************************************************************/
C_DEL_MFG::C_DEL_MFG() {

  if (!done) {
    done = 1;
    DEL_MFG = event_type("DEL_MFG");
    DEL_SFM = event_type("DEL_SFM");
    GRID = object_type("GRID");
  }

  set_lazy();

}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_DEL_MFG::init(C_HEADER *header) {
  DEL_MFG_MESS *del_mfg_mess;

  del_mfg_mess = (DEL_MFG_MESS *)header;
  obj_id = del_mfg_mess->object_id;
  obj_type = del_mfg_mess->object_type;
  obj_node = del_mfg_mess->object_node;
  unique_id = del_mfg_mess->unique_id;

}

/************************************************************************
* exchange : exchange state variables                                   *
************************************************************************/
void C_DEL_MFG::exchange() {
  C_GRID *grid;

  grid = (C_GRID *)SIMOBJ;
  grid->exchange_tmovers(tmovers);

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_DEL_MFG::temp_process() {
  C_GRIDMAN *gridman;
  C_GRID *grid;
  C_XQUEUE *movers;
  C_XQUEUE *sensors;
  C_OID *oid;
  C_ID_ARRAY *id_array;
  int i;
  int len;
  //DEL_SFM_MESS *del_sfm_mess;
  char *bf;
  char s[120];
  int index;
  int n_send;

//...... initialize the free list manager

  RB_DEFINE_FREE(&freeobjs);

//...... get the list of moving objects from the grid

  gridman = (C_GRIDMAN *)get_manager(GRID);
  grid = (C_GRID *)SIMOBJ;
  movers = grid->get_movers();
  tmovers = time_tag;
  grid->update_mover_counter();
  lazy_tsensors = grid->get_tsensors();
  lazy_sensor_counter = grid->get_sensor_counter();

//...... create a new oid object and give it to the mover list

  oid = (C_OID *)movers->find(unique_id);
  if (oid == NULL) {
    sprintf(s, "Error (DEL_MFG) could not find %d\n",unique_id);
    RB_PRINT(stderr,s);
  }else{
    *movers -= oid;
    RB_FREE_DELETE(oid);
  }

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

    /*del_sfm_mess = (DEL_SFM_MESS *)*/schedule(
		GONE * (time_tag + gridman->get_delay_g2m()),
		DEL_SFM,
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
void C_DEL_MFG::perm_process() {

}

/************************************************************************
* cleanup : clean up data structures used                               *
************************************************************************/
void C_DEL_MFG::cleanup() {


}

/************************************************************************
* lazy : lazy cancellation check                                        *
************************************************************************/
int C_DEL_MFG::lazy() {
  C_GRID *grid;

  grid = (C_GRID *)SIMOBJ;

  if (lazy_sensor_counter == grid->get_sensor_counter()) {
//    fprintf(stderr,"ADD_M2G> Good news! Passed lazy cancellation\n");
    return 1;
  }

  return 0;

}
