// del_sfm.C method file

#include <stdio.h>
#include <time.h>
#include <math.h>

#include "del_sfm.H"
#include "del_sfm_mess.H"
#include "del_sfe_mess.H"
#include "gridman.H"
#include "eomanman.H"
#include "mover.H"
#include "oid.H"
#include "eom.H"

#include "def.h"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

int C_DEL_SFM::done = 0;
int C_DEL_SFM::size_node_flag = 0;

extern int GRID;
extern int EOMAN;
extern int DEL_SFM;
extern int DEL_SFE;

/************************************************************************
* C_DEL_SFM : construct a del_sfm object				*
************************************************************************/
C_DEL_SFM::C_DEL_SFM() {

  if (!done) {
    done = 1;
    size_node_flag = N_NODES/sizeof(int);
    if (N_NODES % sizeof(int)) size_node_flag++;
  }

  buff = NULL;
  node_flag = new int[size_node_flag];

}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_DEL_SFM::init(C_HEADER *header) {
  DEL_SFM_MESS *del_sfm_mess;
  char *buffin;

  del_sfm_mess = (DEL_SFM_MESS *)header;
  buffin = (char *)del_sfm_mess + sizeof(DEL_SFM_MESS);
  size = del_sfm_mess->bytes;

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
void C_DEL_SFM::exchange() {
  C_MOVER *mover;

  mover = (C_MOVER *)SIMOBJ;
  mover->exchange_tsensors(tsensors);
  mover->exchange_node_flag(node_flag);

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_DEL_SFM::temp_process() {
  int i, n_sensors;
  C_MOVER *mover;
  C_XQUEUE *sensors;
  C_XQUEUE *script;
  C_OID *oid;
  C_ID_ARRAY *id_array;
  DEL_SFE_MESS *del_sfe_mess;
  C_EOM *e;
  double delay;
  double gone;
  C_GRIDMAN *gridman;
  C_EOMANMAN *eomanman;
  int hash_lid;
  char s[120];

//...... initialize the free list manager

  RB_DEFINE_FREE(&freeobjs);

//...... get the eoman information

  eomanman = (C_EOMANMAN *)get_manager(EOMAN);
  hash_lid = eomanman->get_hash_lid(SIMOBJ->get_GLOBAL_ID());

//...... get the grid manager

  gridman = (C_GRIDMAN *)get_manager(GRID);

//...... get the list of sensors from the mover object

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
  e = mover->get_current_segment();

//...... create a new oid object and give it to the mover list

  n_sensors = size / sizeof(C_ID_ARRAY);
  id_array = (C_ID_ARRAY *)buff;
  gone = GONE5;

  for (i=0; i<n_sensors; i++) {

    oid = (C_OID *)sensors->find(id_array[i].unique_id);
    if (oid != NULL) {

      *sensors -= oid;
      RB_FREE_DELETE(oid);

      if (sensors->find(id_array[i].unique_id) == NULL) {

        delay = gridman->get_delay_m2e();

//...... we must delete the sensor from the eoman

        e = (C_EOM *)script->get_bot();
//	gone *= GONE;
        if (e != NULL) {

          del_sfe_mess = (DEL_SFE_MESS *)schedule(
		gone * (TIME_TAG + delay),
		DEL_SFE,
		EOMAN,
		hash_lid,
		oid->get_object_node());

          del_sfe_mess->object_id = oid->get_object_id();
          del_sfe_mess->object_type = oid->get_object_type();
          del_sfe_mess->object_node = oid->get_object_node();
          del_sfe_mess->unique_id = oid->get_unique_id();
          del_sfe_mess->mover_unique_id = mover->get_GLOBAL_ID();

        }

//...... first see if we have to delete the script from the node

        if (mover->check_node(id_array[i].object_node) == 0) {

	  mover->reset_node_flag(id_array[i].object_node);

        }

      }else{

//...... we dont have to delete the sensor so do nothing

        if (mover->get_node_flag(id_array[i].object_node) == 0) {
          sprintf(s,"Error (DEL_SFM)\n");
          RB_PRINT(stderr,s);
	  mover->set_node_flag(id_array[i].object_node);
        }

      }

    }else{

      sprintf(s,"Error (DEL_SFM)\n");
      RB_PRINT(stderr,s);

    }

  }

  mover->exchange_node_flag(node_flag);

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_DEL_SFM::perm_process() {

}

/************************************************************************
* cleanup : clean up data structures used                               *
************************************************************************/
void C_DEL_SFM::cleanup() {

  if (size != 0) delete [] buff; //RVI 2/18/98
  buff = NULL;

}
