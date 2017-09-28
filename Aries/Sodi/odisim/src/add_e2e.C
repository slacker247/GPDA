// add_e2e.C method file

#include <strstream.h>
#include <stdio.h>
#include <time.h>
#include <math.h>

#include "add_e2e.H"
#include "add_e2e_mess.H"
#include "add_m2s_mess.H"
#include "eoman_script_mess.H"
#include "eoman.H"
#include "eom_holder.H"
#include "eom.H"
#include "oid.H"

#include "def.h"
#include "def_prox.h"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

extern int ADD_E2E;
extern int ADD_M2S;
extern int EOMAN_SCRIPT;

/************************************************************************
* C_ADD_E2E : construct a add_e2e object				*
************************************************************************/
C_ADD_E2E::C_ADD_E2E() {}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_ADD_E2E::init(C_HEADER *header) {
  char *buff1;
  ADD_E2E_MESS *add_e2e_mess;

  add_e2e_mess = (ADD_E2E_MESS *)header;
  unique_id = add_e2e_mess->unique_id;
  script_time = add_e2e_mess->script_time;

  size = header->bytes;
  buff = new char[size];
  buff1 = (char *)header;
  memcpy(buff, &buff1[sizeof(ADD_E2E_MESS)], size);

}

/************************************************************************
* exchange : exchange state variables                                   *
************************************************************************/
void C_ADD_E2E::exchange() {

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_ADD_E2E::temp_process() {
  C_EOMAN *eoman;
  C_XHASH *eoms;
  C_EOM_HOLDER *eom_holder;
  C_EOM *eom;
  C_EOM *next_eom;
  C_OID *oid;
  C_EOM_HOLDER *e_holder;
  C_XQUEUE *script;
  C_XQUEUE *old_script;
  C_XQUEUE *sensors;
  C_XQUEUE *new_sensors;
  int bytes;
  int i, len;
  EOMAN_SCRIPT_MESS *eoman_script_mess;
  ADD_M2S_MESS *add_m2s_mess;

//...... reconstruct the script in the eom_holder

  eom_holder = (C_EOM_HOLDER *)RB_FREE_NEW(EOM_HOLDER);
  eom_holder->reset();
  script = eom_holder->get_script();
  bytes = 0;

  while (bytes < size) {
    eom = (C_EOM *)RB_FREE_GENERATE(&buff[bytes]);
    if (eom->get_endtime() < time_tag) {
      RB_FREE_DELETE(eom);
    }else{
      script->push_bot(eom);
    }
    bytes += freeobjs.get_size(eom);
  }

  eom_holder->set_id(unique_id);
  eom_holder->set_time_tag(script_time);

/*
  if (script->get_length() == 0) {
    RB_FREE_DELETE(eom_holder);
    return;
  }
*/

/*
  fprintf(stderr,"ADD_E2E (m %d), script %d, time %f\n",
	eom_holder->get_id(), script->get_length(), time_tag);
  script->print();
*/

//...... trap

  if (eom_holder->get_id() == TRAP_MOVER) {
    ostrstream os;
    os << "ADD_E2E adding " << eom_holder->get_id()
       << " at time " << time_tag << "\n" << ends;
    RB_PRINT(stderr,os.str());
  }

//...... add the eom holder to the eoman object

  eoman = (C_EOMAN *)SIMOBJ;
  eoms = eoman->get_eoms();
  e_holder = (C_EOM_HOLDER *)eoms->find(unique_id);
  if (e_holder) {

    if (e_holder->get_time_tag() <= eom_holder->get_time_tag()) {

//...... new script is the same as the old one so do nothing
/*
      sprintf(s,"Error (ADD_E2E) two eom holders (%d) at time %f (%f, %f)\n",
	unique_id, time_tag,
	e_holder->get_time_tag(), eom_holder->get_time_tag());
      RB_PRINT(stderr,s);
*/

      len = script->get_length();
      eom = (C_EOM *)script->get_top();

      for (i=0; i<len; i++) {
	next_eom = (C_EOM *)eom->get_link();
	RB_FREE_DELETE(eom);
	eom = next_eom;
      }

      RB_FREE_DELETE(eom_holder);

      return;

    }else{

//...... put in the new script here and tell the sensors

      sensors = e_holder->get_sensors();
      len = sensors->get_length();

#ifdef PROX_SENSOR
      eom = (C_EOM *)script->get_top();
      if (eom != NULL) {

        oid = (C_OID *)sensors->get_top();
        for (i=0; i<len; i++) {

          add_m2s_mess = (ADD_M2S_MESS *)schedule(
		GONE * time_tag,
		ADD_M2S,
		oid->get_object_type(),
		oid->get_object_id(),
		oid->get_object_node());

          add_m2s_mess->unique_id = unique_id;
          add_m2s_mess->peom = eom_holder->get_eom();

	  oid = (C_OID *)oid->get_link();

        }

      }
#endif

//...... put the new list of sensors in the new eom_holder

      new_sensors = eom_holder->get_sensors();
      RB_XMEM((char *)new_sensors, (char *)sensors, sizeof(C_XQUEUE));

//...... clean up the old script

      old_script = e_holder->get_script();
      len = old_script->get_length();
      eom = (C_EOM *)old_script->get_top();

      for (i=0; i<len; i++) {
	next_eom = (C_EOM *)eom->get_link();
	RB_FREE_DELETE(eom);
	eom = next_eom;
      }

      *eoms -= e_holder;
      RB_FREE_DELETE(e_holder);

    }

  }

  *eoms += eom_holder;

//...... start up the event to update the eoman script

  eom = (C_EOM *)script->get_top();
  if (eom != NULL) {
    eoman_script_mess = (EOMAN_SCRIPT_MESS *)schedule(
	eom->get_endtime(),
	EOMAN_SCRIPT,
	OBJECT_TYPE,
	LOCAL_ID,
	NODE);

    eoman_script_mess->unique_id = eom->get_id();
  }
}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_ADD_E2E::perm_process() {

}

/************************************************************************
* cleanup : clean up data structures used                               *
************************************************************************/
void C_ADD_E2E::cleanup() {

  delete [] buff; //RVI 2/18/98
  buff = NULL;

}
