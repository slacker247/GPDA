// next_script.C method file

#include <stdio.h>
#include <time.h>
#include <math.h>
#include "next_script.H"
#include "next_script_mess.H"
#include "def.h"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

int C_NEXT_SCRIPT::done = 0;
int C_NEXT_SCRIPT::NEXT_SCRIPT = 0;

/************************************************************************
* C_NEXT_SCRIPT : construct a next_script object			*
************************************************************************/
C_NEXT_SCRIPT::C_NEXT_SCRIPT() {

  if (!done) {
    NEXT_SCRIPT = event_type("NEXT_SCRIPT");
    done = 1;
  }

//  set_lazy();

}

/************************************************************************
* exchange : exchange state variables					*
************************************************************************/
void C_NEXT_SCRIPT::exchange() {
  C_MOVER *mover;

  mover = (C_MOVER *)SIMOBJ;
  mover->exchange_alive(alive);

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_NEXT_SCRIPT::temp_process() {
  C_XQUEUE *script;
  C_MOVER *mover;
  C_EOM *eom;

//...... trap the object with a specified id

  if (SIMOBJ->get_GLOBAL_ID() == -1) {
    cerr << "next_script -1 trapped at time " << TIME_TAG << "\n";
  }

//...... initialize the free list manager

  RB_DEFINE_FREE(&freeobjs);

//...... get the script out of the mover object

  mover = (C_MOVER *)SIMOBJ;
  script = mover->get_script();
  alive = mover->get_alive();

//...... check that the time of the next eom is right

  eom = (C_EOM *)script->get_top();
  if (eom == NULL) return;

  if (fabs(TIME_TAG - eom->get_endtime()) > 0.001) {
    return;
  }

//...... pull the eom out of the script and delete it

  *script -= eom;
  RB_FREE_DELETE(eom);

//...... reschedule this event for the next time

  eom = (C_EOM *)script->get_top();
  if (eom != NULL) {
    schedule(eom->get_endtime(), NEXT_SCRIPT, OBJECT_TYPE, LOCAL_ID, NODE);
  }else{
    alive = 0;
  }

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_NEXT_SCRIPT::perm_process() {

}

/************************************************************************
* cleanup : clean up data structures used                               *
************************************************************************/
void C_NEXT_SCRIPT::cleanup() {

}
