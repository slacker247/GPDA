// eoman_script.C method file

#include <strstream.h>
#include <stdio.h>
#include <time.h>
#include <math.h>
#include "eoman.H"
#include "oid.H"
#include "eom_holder.H"
#include "eoman_script.H"
#include "eoman_script_mess.H"
#include "add_m2s_mess.H"
#include "del_mfs_mess.H"
#include "def.h"
#include "def_prox.h"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

int C_EOMAN_SCRIPT::done = 0;
int C_EOMAN_SCRIPT::EOMAN_SCRIPT = 0;
int C_EOMAN_SCRIPT::ADD_M2S = 0;
int C_EOMAN_SCRIPT::DEL_MFS = 0;
int C_EOMAN_SCRIPT::EOMAN = 0;

/************************************************************************
* C_EOMAN_SCRIPT : construct a eoman_script object			*
************************************************************************/
C_EOMAN_SCRIPT::C_EOMAN_SCRIPT() {

  if (!done) {
    done = 1;
    EOMAN = object_type("EOMAN");
    EOMAN_SCRIPT = event_type("EOMAN_SCRIPT");
    ADD_M2S = event_type("ADD_M2S");
    DEL_MFS = event_type("DEL_MFS");
  }

//  set_lazy();

}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_EOMAN_SCRIPT::init(C_HEADER *header) {
  EOMAN_SCRIPT_MESS *eoman_script_mess;

  eoman_script_mess = (EOMAN_SCRIPT_MESS *)header;
  uid = eoman_script_mess->unique_id;

}



/************************************************************************
* exchange : exchange state variables					*
************************************************************************/
void C_EOMAN_SCRIPT::exchange() {

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_EOMAN_SCRIPT::temp_process() {
  int i, len;
  C_XQUEUE *script;
  C_XQUEUE *sensors;
  C_EOMAN *eoman;
  C_EOM *oldeom;
  C_EOM *eom;
  C_XHASH *eoms;
  C_EOM_HOLDER *eom_holder;
  EOMAN_SCRIPT_MESS *eoman_script_mess;
  ADD_M2S_MESS *add_m2s_mess;
  DEL_MFS_MESS *del_mfs_mess;
  C_OID *oid;
  C_OID *next_oid;

//...... initialize the free list manager

  RB_DEFINE_FREE(&freeobjs);

//...... get the script out of the eoman object

  eoman = (C_EOMAN *)SIMOBJ;
  eoms = eoman->get_eoms();
  eom_holder = (C_EOM_HOLDER *)eoms->find(uid);
  if (eom_holder == NULL) {
//  sprintf(s,"Error (EOMAN_SCRIPT) no holder\n");
//  RB_PRINT(stderr,s);
    return;
  }

//...... check that the end time for the eom is right

  script = eom_holder->get_script();

  oldeom = (C_EOM *)script->get_top();
  if (oldeom != NULL) {

//...... return if the time isnt right

    if (fabs(oldeom->get_endtime()-TIME_TAG) > 0.001) {
      return;
    }

//...... remove the eom from the script

    *script -= oldeom;

  }

//...... reschedule this event for the next time if script not empty

  eom = (C_EOM *)script->get_top();
  if (eom != NULL) {
    eoman_script_mess = (EOMAN_SCRIPT_MESS *) schedule(
	eom->get_endtime(),
	EOMAN_SCRIPT,
	OBJECT_TYPE,
	LOCAL_ID,
	NODE);

    eoman_script_mess->unique_id = uid;

  }

//...... distribute the eom to the sensors

  sensors = eom_holder->get_sensors();
  oid = (C_OID *)sensors->get_top();
  len = sensors->get_length();

#ifdef PROX_SENSOR
  if (eom != NULL) {

    for (i=0; i<len; i++) {
      add_m2s_mess = (ADD_M2S_MESS *)schedule(
		GONE * TIME_TAG,
		ADD_M2S,
		oid->get_object_type(),
		oid->get_object_id(),
		oid->get_object_node());

      add_m2s_mess->unique_id = eom->get_id();
      add_m2s_mess->peom = eom;
      oid = (C_OID *)oid->get_link();
    }

  }else{

    if (oldeom != NULL) {
      for (i=0; i<len; i++) {
        del_mfs_mess = (DEL_MFS_MESS *)schedule(
		GONE * TIME_TAG,
		DEL_MFS,
		oid->get_object_type(),
		oid->get_object_id(),
		oid->get_object_node());

        del_mfs_mess->unique_id = oldeom->get_id();
        del_mfs_mess->peom = oldeom;
        oid = (C_OID *)oid->get_link();
      }
    }

  }
#endif

//...... cleanup if end of script

  if (eom == NULL) {

    sensors = eom_holder->get_sensors();
    if (sensors->get_length() == 0) {

//...... trap

      if (eom_holder->get_id() == TRAP_MOVER) {
	ostrstream so;
	so << "EOMAN_SCRIPT deleting " << eom_holder->get_id()
	   << " at time " << TIME_TAG << "\n" << ends;
        RB_PRINT(stderr,so.str());
      }

      *eoms -= eom_holder;

      oid = (C_OID *)sensors->get_top();
      for (i=0; i<len; i++) {
        next_oid = (C_OID *)oid->get_link();
        RB_FREE_DELETE(oid);
        oid = next_oid;
      }

      RB_FREE_DELETE(eom_holder);

    }

  }

//...... delete the old equation of motion

  if (oldeom) RB_FREE_DELETE(oldeom);

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_EOMAN_SCRIPT::perm_process() {

}

/************************************************************************
* cleanup : clean up data structures used                               *
************************************************************************/
void C_EOMAN_SCRIPT::cleanup() {

}
