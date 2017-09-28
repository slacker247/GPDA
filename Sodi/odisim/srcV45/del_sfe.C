// del_sfe.C method file

#include <strstream.h>
#include <stdio.h>
#include <time.h>
#include <math.h>

#include "del_sfe.H"
#include "del_sfe_mess.H"
#include "del_mfs_mess.H"
#include "eoman.H"
#include "eom_holder.H"
#include "xhash.H"
#include "xqueue.H"
#include "oid.H"

#include "def.h"
#include "def_prox.h"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

int C_DEL_SFE::done = 0;
int C_DEL_SFE::DEL_SFE = 0;
int C_DEL_SFE::DEL_MFS = 0;

/************************************************************************
* C_DEL_SFE : construct a del_sfe object				*
************************************************************************/
C_DEL_SFE::C_DEL_SFE() {

  if (!done) {
    done = 1;
    DEL_SFE = event_type("DEL_SFE");
    DEL_MFS = event_type("DEL_MFS");
  }

}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_DEL_SFE::init(C_HEADER *header) {
  DEL_SFE_MESS *del_sfe_mess;

  del_sfe_mess = (DEL_SFE_MESS *)header;
  obj_id = del_sfe_mess->object_id;
  obj_type = del_sfe_mess->object_type;
  obj_node = del_sfe_mess->object_node;
  unique_id = del_sfe_mess->unique_id;
  mover_unique_id = del_sfe_mess->mover_unique_id;

}

/************************************************************************
* exchange : exchange state variables                                   *
************************************************************************/
void C_DEL_SFE::exchange() {
  //C_EOMAN *eoman;

  //eoman = (C_EOMAN *)SIMOBJ;

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_DEL_SFE::temp_process() {
  int i, len;
  C_EOMAN *eoman;
  C_OID *oid;
  C_EOM *eom;
  C_EOM *nexteom;
  C_XHASH *eoms;
  C_XQUEUE *sensors;
  C_XQUEUE *script;
  C_EOM_HOLDER *eom_holder;
  DEL_MFS_MESS *del_mfs_mess;

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

//...... remove the sensor from the eom list

      *sensors -= oid;

//...... remove the mover from the sensor

#ifdef PROX_SENSOR
      if (eom_holder->get_eom() != NULL) {
        del_mfs_mess = (DEL_MFS_MESS *)schedule(
		GONE * TIME_TAG,
		DEL_MFS,
		oid->get_object_type(),
		oid->get_object_id(),
		oid->get_object_node());

        del_mfs_mess->unique_id = mover_unique_id;
        del_mfs_mess->peom = eom_holder->get_eom();
      }
#endif

//...... delete the oid

      RB_FREE_DELETE(oid);

//...... delete the eom_holder if this is the last sensor

      if (sensors->get_length() == 0) {

//...... trap

        if (mover_unique_id == -1) {
          cerr << "DEL_SFE deleting (m " << mover_unique_id
	       << " s " << unique_id << ") at time " << TIME_TAG << "\n";
        }

        script = eom_holder->get_script();
        len = script->get_length();
        eom = (C_EOM *)script->get_top();

        for (i=0; i<len; i++) {
          nexteom = (C_EOM *)eom->get_link();
	  RB_FREE_DELETE(eom);
	  eom = nexteom;
        }

        *eoms -= eom_holder;
	RB_FREE_DELETE(eom_holder);

      }

    }else{
      ostrstream so;
      so << "Error (DEL_SFE) sensor (m " << mover_unique_id
	 << ", s " << unique_id << ") not there at " << TIME_TAG
	 << "\n" << ends;
      RB_PRINT(stderr,so.str());

    }
  }else{
    ostrstream so;
    so << "Error (DEL_SFE) eom (m " << mover_unique_id
       << ", s " << unique_id << ") not there at " << TIME_TAG << "\n" << ends;
    RB_PRINT(stderr,so.str());

  }

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_DEL_SFE::perm_process() {

}

/************************************************************************
* cleanup : clean up data structures used                               *
************************************************************************/
void C_DEL_SFE::cleanup() {


}
