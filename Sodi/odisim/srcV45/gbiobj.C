// gbiobj.C method file

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "def.h"
#include "gbiobj.H"

#include "stop.H"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

/************************************************************************
* aim : aim the missile							*
************************************************************************/
void C_GBIOBJ::aim() {

}

/************************************************************************
* make_script : take the missile stages and make a script		*
************************************************************************/
void C_GBIOBJ::make_script(double ktime) {
  int i;
  int len;
  int nstages;
  double X[3], V[3];
  C_EOM *eom_temp;
  C_STOP *stop;

//  kill_time RB= ktime;
  kill_time = ktime;

  nstages = missile->get_nstages();
  for (i=0; i<=nstages; i++) {
    eom_temp = missile->get_stage(i);
//    script->push_bot(eom_temp);
    *script += eom_temp;
  }

  missile->get_pos_vel(kill_time,X,V);
//  stop = (C_STOP *)freeobjs.new_object(STOP);
  stop = (C_STOP *)RB_FREE_NEW(STOP);
  stop->init(kill_time,X);
  stop->set_icon(37);
//  script->push_bot(stop);
  *script += stop;

  eom_temp = (C_EOM *)script->get_top();
  len = script->get_length();
  for (i=0; i<len; i++) {
    eom_temp->set_sequence(i);
    eom_temp->set_object_type(OBJECT_TYPE);
    eom_temp->set_object_id(LOCAL_ID);
    eom_temp->set_object_node(NODE);
    eom_temp->set_id(GLOBAL_ID);
    eom_temp = (C_EOM *)eom_temp->get_link();
  }

}

