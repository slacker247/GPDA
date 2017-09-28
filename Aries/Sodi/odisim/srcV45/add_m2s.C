// add_m2s.C method file

#include <stdio.h>
#include <time.h>
#include <math.h>

#include "add_m2s.H"
#include "add_m2s_mess.H"
#include "sensobj.H"
#include "eom.H"
#include "oid.H"

#include "def.h"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

int C_ADD_M2S::done = 0;
int C_ADD_M2S::ADD_M2S = 0;
int C_ADD_M2S::SCAN = 0;

/************************************************************************
* C_ADD_M2S : construct a add_m2s object				*
************************************************************************/
C_ADD_M2S::C_ADD_M2S() {

  if (!done) {
    done = 1;
    ADD_M2S = event_type("ADD_M2S");
    SCAN = event_type("SCAN");
  }

}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_ADD_M2S::init(C_HEADER *header) {
  ADD_M2S_MESS *add_m2s_mess;

  add_m2s_mess = (ADD_M2S_MESS *)header;
  unique_id = add_m2s_mess->unique_id;
  peom = add_m2s_mess->peom;

}

/************************************************************************
* exchange : exchange state variables                                   *
************************************************************************/
void C_ADD_M2S::exchange() {
  C_SENSOBJ *sensobj;

  SIMOBJ->EXCHANGE_SEED(seed);

  sensobj = (C_SENSOBJ *)SIMOBJ;
  sensobj->exchange_scanning(scanning);

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_ADD_M2S::temp_process() {
  C_SENSOBJ *sensobj;
  C_XQUEUE *movers;
  C_OID *oid;
  C_OID *oid1;
  char s[120];
  int scan_type;
  double temp;

//...... initialize the free list manager

  RB_DEFINE_FREE(&freeobjs);

//...... get the seed from the sensor

  seed = SIMOBJ->get_SEED();

//...... check that the eom has the right id

  if ( ((C_EOM *)peom)->get_id() != unique_id) {
    sprintf(s,"Error (ADD_M2S) unique id %d not right for peom\n",unique_id);
    RB_PRINT(stderr,s);
  }

//...... get the list of moving objects from the sensor

  sensobj = (C_SENSOBJ *)SIMOBJ;
  movers = sensobj->get_movers();

//...... create a new oid object and give it to the mover list

  oid = (C_OID *)RB_FREE_NEW(OID);
  oid->set_unique_id(unique_id);
  oid->set_peom(peom);

  oid1 = (C_OID *)movers->find(unique_id);
  if (oid1) {
    *movers -= oid1;
    RB_FREE_DELETE(oid1);
  }

  *movers += oid;

//...... check if the scan event needs to be started

  scanning = sensobj->get_scanning();
  if ((!scanning) && (sensobj->get_on_off())) {

    scanning = 1;
    scan_type = sensobj->get_scan_type();

    RANDOM->set_seed(seed);
    RANDOM->set_float_limits(0.0, sensobj->get_scan_time());
    temp = RANDOM->get_random_float();

    if (scan_type == -1) {
      schedule (time_tag+temp, SCAN, OBJECT_TYPE, LOCAL_ID, NODE);
    }else{
      schedule (time_tag+temp, scan_type, OBJECT_TYPE, LOCAL_ID, NODE);
    }
  }

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_ADD_M2S::perm_process() {

}

/************************************************************************
* cleanup : clean up data structures used                               *
************************************************************************/
void C_ADD_M2S::cleanup() {


}
