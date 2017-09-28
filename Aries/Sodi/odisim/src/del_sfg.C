// del_sfg.C method file

#include <stdio.h>
#include <time.h>
#include <math.h>

#include "del_sfg.H"
#include "del_sfg_mess.H"
#include "del_sfm_mess.H"
#include "gridman.H"
#include "oid.H"

#include "def.h"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

extern int DEL_SFM;
extern int GRID;

/************************************************************************
* C_DEL_SFG : construct a del_sfg object				*
************************************************************************/
C_DEL_SFG::C_DEL_SFG() {
  set_lazy();
}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_DEL_SFG::init(C_HEADER *header) {
  DEL_SFG_MESS *del_sfg_mess;

  del_sfg_mess = (DEL_SFG_MESS *)header;
  unique_id = del_sfg_mess->unique_id;

}

/************************************************************************
* exchange : exchange state variables                                   *
************************************************************************/
void C_DEL_SFG::exchange() {
  C_GRID *grid;

  grid = (C_GRID *)SIMOBJ;
  grid->exchange_tsensors(tsensors);

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_DEL_SFG::temp_process() {
  C_GRIDMAN *gridman;
  int len,i;
  C_GRID *grid;
  C_XQUEUE *movers;
  C_XQUEUE *sensors;
  C_OID *sensor_oid;
  C_OID *mover_oid;
  char s[120];
  C_ID_ARRAY *id_array;
  double tsched;
  char *bf;
  //DEL_SFM_MESS *del_sfm_mess;

//...... get the list of moving objects from the grid

  grid = (C_GRID *)SIMOBJ;
  gridman = (C_GRIDMAN *)get_manager(GRID);
  sensors = grid->get_sensors();
  tsensors = TIME_TAG;
  grid->update_sensor_counter();
  lazy_tmovers = grid->get_tmovers();
  lazy_mover_counter = grid->get_mover_counter();

//...... create a new oid object and give it to the mover list

  sensor_oid = (C_OID *)sensors->find(unique_id);

  if (sensor_oid != NULL) {

    movers = grid->get_movers();
    len = movers->get_length();
    mover_oid = (C_OID *)movers->get_top();
    tsched = TIME_TAG + gridman->get_delay_g2m();

    for (i=0; i<len; i++) {

      if (sensor_oid->get_unique_id() != mover_oid->get_unique_id()) {
        /*del_sfm_mess = (DEL_SFM_MESS *)*/schedule(
		tsched,
		DEL_SFM,
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

    *sensors -= sensor_oid;
    RB_FREE_DELETE(sensor_oid);

  }else{
    sscanf(s,"Error (DEL_SFG) could not find sensor %d\n",&unique_id);
    RB_PRINT(stderr,s);
  }

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_DEL_SFG::perm_process() {

}

/************************************************************************
* cleanup : clean up data structures used                               *
************************************************************************/
void C_DEL_SFG::cleanup() {

}

/************************************************************************
* lazy : lazy cancellation check                                        *
************************************************************************/
int C_DEL_SFG::lazy() {
  C_GRID *grid;

  grid = (C_GRID *)SIMOBJ;

  if (lazy_mover_counter == grid->get_mover_counter()) {
//    fprintf(stderr,"DEL_SFG> Good news! Passed lazy cancellation\n");
    return 1;
  }

  return 0;

}
