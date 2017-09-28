// add_s2e.C method file

#include <strstream.h>
#include <stdio.h>
#include <time.h>
#include <math.h>

#include "add_s2e.H"
#include "add_s2e_mess.H"
#include "add_m2s_mess.H"
#include "eoman.H"
#include "eom_holder.H"
#include "xhash.H"
#include "xqueue.H"
#include "oid.H"

#include "def.h"
#include "def_prox.h"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;
extern int ADD_S2E;
extern int ADD_M2S;

/************************************************************************
* C_ADD_S2E : construct a add_s2e object				*
************************************************************************/
C_ADD_S2E::C_ADD_S2E() {}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_ADD_S2E::init(C_HEADER *header) {
  ADD_S2E_MESS *add_s2e_mess;

  add_s2e_mess = (ADD_S2E_MESS *)header;
  obj_id = add_s2e_mess->object_id;
  obj_type = add_s2e_mess->object_type;
  obj_node = add_s2e_mess->object_node;
  unique_id = add_s2e_mess->unique_id;
  mover_unique_id = add_s2e_mess->mover_unique_id;

  if (obj_type == 0) {
    fprintf(stderr,"Error, (ADD_S2E) bad object type\n");
  }

}

/************************************************************************
* exchange : exchange state variables                                   *
************************************************************************/
void C_ADD_S2E::exchange() {
  //C_EOMAN *eoman;

  //eoman = (C_EOMAN *)SIMOBJ;

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_ADD_S2E::temp_process() {
  C_EOMAN *eoman;
  C_OID *oid;
  C_XHASH *eoms;
  C_XQUEUE *sensors;
  C_EOM_HOLDER *eom_holder;
  ADD_M2S_MESS *add_m2s_mess;

//...... get the list of moving objects from the grid

  eoman = (C_EOMAN *)SIMOBJ;
  eoms = eoman->get_eoms();

  eom_holder = (C_EOM_HOLDER *)eoms->find(mover_unique_id);
  if (eom_holder) {
    sensors = eom_holder->get_sensors();

/*
    if (sensors->check_integrity()) {
      oid = (C_OID *)sensors->find(unique_id);
    }
*/
    oid = (C_OID *)sensors->find(unique_id);

    if (oid) {
      ostrstream so;
      so << "Error (ADD_S2E) sensor (m " << mover_unique_id
	 << ", s " << unique_id << ") already there at "
	 << TIME_TAG << "\n" << ends;
      RB_PRINT(stderr,so.str());

    }else{

//...... add the sensor to the eom list

      oid = (C_OID *)RB_FREE_NEW(OID);
      oid->set_object_id(obj_id);
      oid->set_object_type(obj_type);
      oid->set_object_node(obj_node);
      oid->set_unique_id(unique_id);
      *sensors += oid;

//...... add the eom to the sensor

      if (eom_holder->get_eom() != NULL) {

#ifdef PROX_SENSOR
        add_m2s_mess = (ADD_M2S_MESS *)schedule(
		GONE * TIME_TAG,
		ADD_M2S,
		oid->get_object_type(),
		oid->get_object_id(),
		oid->get_object_node());

        add_m2s_mess->unique_id = mover_unique_id;
        add_m2s_mess->peom = eom_holder->get_eom();
#endif

      }else{

//	fprintf(stderr,"Error, (ADD_S2E) bad sensor id\n");

      }
    }

  }else{

    ostrstream so;
    so << "Error (ADD_S2E) eom (m " << mover_unique_id
       << ", s " << unique_id << ") not there at " << TIME_TAG << "\n" << ends;
    RB_PRINT(stderr,so.str());

  }

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_ADD_S2E::perm_process() {

}

/************************************************************************
* cleanup : clean up data structures used                               *
************************************************************************/
void C_ADD_S2E::cleanup() {


}
