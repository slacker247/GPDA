// test_prox.C method file

#include <stdio.h>
#include <time.h>
#include <math.h>
#include "test_prox.H"
#include "test_prox_mess.H"
#include "sensman.H"
#include "ranman.H"
#include "oid.H"
#include "def.h"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

int C_TEST_PROX::done = 0;
int C_TEST_PROX::SENSOR = 0;
int C_TEST_PROX::RANDOM_AIR = 0;
int C_TEST_PROX::TEST_PROX = 0;

/************************************************************************
* C_TEST_PROX : construct a test_prox object				*
************************************************************************/
C_TEST_PROX::C_TEST_PROX() {

  if (!done) {
    done = 1;
    RANDOM_AIR = object_type("RANDOM_AIR");
    SENSOR = object_type("SENSOR");
    TEST_PROX = event_type("TEST_PROX");
  }

}

/************************************************************************
* exchange : exchange state variables					*
************************************************************************/
void C_TEST_PROX::exchange() {

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_TEST_PROX::temp_process() {
  C_SENSMAN *sensman;
  C_SENSOBJ *sensobj;
  C_RANMAN *ranman;
  C_MOVER *mover;
  C_XQUEUE *script;
  int n_movers;
  int i;
  double Ps[3], Vs[3];
  double Pm[3], Vm[3];
  double d0,d1,d2,d;
  double rmax;
  int muid;
  C_XQUEUE *movers;
  C_OID *oid;
  C_EOM *eom;

//...... get the sensor object along with the sensor and ranman object managers

  sensman = (C_SENSMAN *)get_manager(SENSOR);
  ranman = (C_RANMAN *)get_manager(RANDOM_AIR);
  sensobj = (C_SENSOBJ *)SIMOBJ;

//...... loop over the number of random movers

  movers = sensobj->get_movers();
  rmax = sensobj->get_rmax();
  sensobj->get_pos_vel(TIME_TAG,Ps,Vs);
  n_movers = ranman->get_N_TOT();

  if (!sensobj->get_alive()) return;

  for (i=0; i<n_movers; i++) {

    mover = (C_MOVER *)ranman->get_obj(i);

    script = mover->get_script();
    if (script->get_length()) {

      if (mover->get_GLOBAL_ID() != sensobj->get_GLOBAL_ID()) {

        mover->get_pos_vel(TIME_TAG,Pm,Vm);
        d0 = Ps[0]-Pm[0];
        d1 = Ps[1]-Pm[1];
        d2 = Ps[2]-Pm[2];
        d = d0*d0 + d1*d1 + d2*d2;

        if (d <= rmax) {
          muid = mover->get_GLOBAL_ID();
          oid = (C_OID *)movers->find(muid);
          if (!oid) {
	    fprintf(stderr,"Error (TEST_PROX) sensor %d could not find %d\n",
		sensobj->get_GLOBAL_ID(), muid);
          }else{
	    eom = (C_EOM *)oid->get_peom();
	    if (eom->get_endtime() < TIME_TAG) {
	      fprintf(stderr,"ERROR (TEST_PROX) eom time out of range\n");
	    }
          }

        }

      }

    }

  }

//...... reschedule this event again

  schedule (	TIME_TAG + sensman->get_test_prox_time(),
		TEST_PROX,
		OBJECT_TYPE,
		LOCAL_ID,
		NODE	);

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_TEST_PROX::perm_process() {

}

/************************************************************************
* cleanup : clean up data structures used                               *
************************************************************************/
void C_TEST_PROX::cleanup() {

}
