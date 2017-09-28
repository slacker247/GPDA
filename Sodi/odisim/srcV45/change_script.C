// change_script.C method file

#include <stdio.h>
#include <time.h>
#include <math.h>
#include "change_script.H"
#include "change_script_mess.H"
#include "mover.H"
#include "gridman.H"
#include "ranman.H"
#include "eom.H"
#include "greatcirc.H"
#include "def.h"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

int C_CHANGE_SCRIPT::done = 0;
int C_CHANGE_SCRIPT::CHANGE_SCRIPT = 0;
int C_CHANGE_SCRIPT::RANDOM_AIR = 0;

/************************************************************************
* C_CHANGE_SCRIPT : construct a change_script object			*
************************************************************************/
C_CHANGE_SCRIPT::C_CHANGE_SCRIPT() {

  if (!done) {
    done = 1;
    RANDOM_AIR = object_type("RANDOM_AIR");
    CHANGE_SCRIPT = event_type("CHANGE_SCRIPT");
  }

}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_CHANGE_SCRIPT::init(C_HEADER *header) {
  CHANGE_SCRIPT_MESS *change_script_mess;

  change_script_mess = (CHANGE_SCRIPT_MESS *)header;
  repeat_flag = change_script_mess->repeat_flag;

}

/************************************************************************
* exchange : exchange state variables					*
************************************************************************/
void C_CHANGE_SCRIPT::exchange() {
  C_MOVER *mover;

  SIMOBJ->EXCHANGE_SEED(sd);

  mover = (C_MOVER *)SIMOBJ;
  mover->exchange_script(script);
  mover->exchange_script_time(script_time);

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_CHANGE_SCRIPT::temp_process() {
  int i,len;
  CHANGE_SCRIPT_MESS *change_script_mess;
  C_RANMAN *ranman;
  //C_RANOBJ *ranobj;
  C_MOVER *mover;
  double change_script_rate;
  double new_eom_time;
  double next_time;
  C_EOM *eom;
  C_EOM *old_eom;
  C_EOM *new_eom;
  int size_eom;
  double time;
  double *R;
  C_XQUEUE *old_script;
  C_XQUEUE *new_script;

//...... set up the new script time

  script_time = TIME_TAG;

//...... set up the random number generator

  sd = SIMOBJ->get_SEED();
  RANDOM->set_seed(sd);

//...... get the object managers required

  //ranobj = (C_RANOBJ *)SIMOBJ;
  mover = (C_MOVER *)SIMOBJ;
  ranman = (C_RANMAN *)get_manager(RANDOM_AIR);
  change_script_rate = ranman->get_change_script_rate();

//...... generate some random numbers

  RANDOM->set_exp(1.0/change_script_rate);
  next_time = TIME_TAG + RANDOM->get_random_exp() + 0.001;
  new_eom_time = TIME_TAG + RANDOM->get_random_exp() + 0.001;

//...... generate a new script based on the new_eom_time

  new_script = RB_NEW_C_XQUEUE();
  old_script = mover->get_script();

  eom = NULL;
  len = old_script->get_length();
  old_eom = (C_EOM *)old_script->get_top();
  if (old_eom) {
    time = old_eom->get_start_time();
  }else{
    time = TIME_TAG;
  }

  for (i=0; i<len; i++) {

    new_eom = (C_EOM *)RB_FREE_NEW(old_eom->get_freeid());

    if (time < new_eom_time) {

      size_eom = freeobjs.get_size(old_eom);
      memcpy((char *)new_eom, (char *)old_eom, size_eom);

      if (new_eom_time < new_eom->get_endtime()) {
	new_eom->change_endtime(new_eom_time);
      }

      time = new_eom->get_endtime();

    }else{

      R = eom->get_endpos();
      mover->fill_random_segment(time,R,new_eom);
      time = new_eom->get_endtime();

    }

    new_script->push_bot(new_eom);
    eom = new_eom;
    old_eom = (C_EOM *)old_eom->get_link();

  }

//...... make sure that the script is not empty

  if (new_script->get_length() != 0) {

//...... check script

    eom = (C_EOM *)new_script->get_top();
    time = eom->get_start_time();
    for (i=0; i<len; i++) {
      if (fabs(time - eom->get_start_time()) > 0.001) {
        fprintf(stderr,"Error (CHANGE_SCRIPT) bad script\n");
      }
      time = eom->get_endtime();
      eom = (C_EOM *)eom->get_link();
    }

//...... call the update prox method to get the new script into action

    update_prox(new_eom_time, new_script);

//...... delete the old script

    old_eom = (C_EOM *)old_script->get_top();
    for (i=0; i<len; i++) {
      eom = old_eom;
      old_eom = (C_EOM *)old_eom->get_link();
      RB_FREE_DELETE(eom);
    }
    RB_DELETE_C_XQUEUE(old_script);
    script = new_script;

//...... reschedule this event again

    if (repeat_flag) {

      change_script_mess = (CHANGE_SCRIPT_MESS *)schedule(
	TIME_TAG + next_time,
	CHANGE_SCRIPT,
	OBJECT_TYPE,
	LOCAL_ID,
	NODE);

      change_script_mess->repeat_flag = repeat_flag;

    }

  }

  RB_DELETE_C_XQUEUE(old_script);
  script = new_script;

  if ((script->get_length() > 10) || (script->get_length() < 0)) {
    fprintf(stderr,"Error (CHANGE_SCRIPT) bad script length\n");
  }

//...... get the new seed to be exchanged with the object

  sd = RANDOM->get_seed();

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_CHANGE_SCRIPT::perm_process() {

//  fprintf(stderr,"CHANGE_SCRIPT at time %f\n",TIME_TAG);

}

/************************************************************************
* cleanup : clean up data structures used                               *
************************************************************************/
void C_CHANGE_SCRIPT::cleanup() {

}
