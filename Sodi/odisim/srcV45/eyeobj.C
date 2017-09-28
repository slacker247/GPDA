// eyeobj.C method file

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "def.h"
#include "eyeobj.H"
#include "bus.H"

#define FREEON

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

/************************************************************************
* C_EYEOBJ : construct a eyeobj object					*
************************************************************************/
C_EYEOBJ::C_EYEOBJ() {

}

/************************************************************************
* init_eom : initialize the kepler motion of the B.E. sensor		*
************************************************************************/
void C_EYEOBJ::init_eom(double time, int n_rings,
				int ring_arg, int n_per_ring_arg,
				int n, double alt, double inc, int stag) {
  double aa,ee,ii,oo,ww,mm;
  double stagger;

  aa = RE + alt;
  ee = 0.0;
  ii = inc;
  oo = TWOPI * double(ring_arg) / double(n_rings);
  ww = 0.0;
  mm = TWOPI * double(n) / double(n_per_ring_arg);

  if (stag) {
    stagger = (TWOPI / double(n_per_ring_arg)) * double(ring_arg)
			/ double(n_rings);
    mm += stagger;
  }

  init_eom(time,aa,ee,ii,oo,ww,mm);

  cruise_vel = sqrt(GM/aa);
  max_vel = 1.1 * cruise_vel;

  period = TWOPI * aa / cruise_vel;

//...... test that the period is ok

/*
  double X[3], V[3];
  double Ttest;

  eom->set_ECI(1);
  Ttest = -period;
  eom->get_pos_vel(Ttest,X,V);
  fprintf(stderr,"\nEYE Period -> T=%f, \tX=(%f %f %f), V=(%f %f %f)\n",
	Ttest,X[0],X[1],X[2],V[0],V[1],V[2]);
  Ttest += period;
  eom->get_pos_vel(Ttest,X,V);
  fprintf(stderr,"EYE Period 0> T=%f, \tX=(%f %f %f), V=(%f %f %f)\n",
	Ttest,X[0],X[1],X[2],V[0],V[1],V[2]);
  Ttest += period;
  eom->get_pos_vel(Ttest,X,V);
  fprintf(stderr,"EYE Period +> T=%f, \tX=(%f %f %f), V=(%f %f %f)\n\n",
	Ttest,X[0],X[1],X[2],V[0],V[1],V[2]);
  eom->set_ECI(0);
*/

}

/************************************************************************
* init_eom : initialize the kepler motion of the B.E. sensor		*
************************************************************************/
void C_EYEOBJ::init_eom(double time, double X[3], double V[3]) {
  C_KEPLER *kepler;
  double aa;

  kepler = (C_KEPLER *)freeobjs.new_object(BUS);
  eom = kepler;

  kepler->init(X,V);

//...... test that the motion is correct

/*
  double Ttest, Xtest[3], Vtest[3];

  Ttest = 20000.0;
  kepler->get_pos_vel(Ttest,Xtest,Vtest);
  fprintf(stderr,"EYE 1> T=%f, X=(%f %f %f), V=(%f %f %f)\n",
	Ttest,Xtest[0],Xtest[1],Xtest[2],Vtest[0],Vtest[1],Vtest[2]);

  Ttest += 43200;
  kepler->set_start_time(43200);

  kepler->get_pos_vel(Ttest,Xtest,Vtest);
  fprintf(stderr,"EYE 2> T=%f, X=(%f %f %f), V=(%f %f %f)\n",
	Ttest,Xtest[0],Xtest[1],Xtest[2],Vtest[0],Vtest[1],Vtest[2]);
*/

//...... initialize the start time and the end time (1 year from now)

  kepler->set_start_time(time);
  kepler->set_endtime(31536000.0);

  kepler->set_ECI(0);

//...... set up the eom (kepler) information

  eom->set_sequence(0);
  eom->set_object_type(OBJECT_TYPE);
  eom->set_object_id(LOCAL_ID);
  eom->set_object_node(NODE);
  eom->set_id(GLOBAL_ID);

//...... put the eom into the script (one equation of motion)

  script->push_bot(eom);

  aa = sqrt(X[0]*X[0]+X[1]*X[1]+X[2]*X[2]);
  cruise_vel = sqrt(GM/aa);
  max_vel = 1.1 * cruise_vel;

  period = TWOPI * aa / cruise_vel;

}

/************************************************************************
* init_eom : initialize the kepler motion of the B.E. sensor		*
************************************************************************/
void C_EYEOBJ::init_eom(double time,
			double aa,
			double ee,
			double ii,
			double oo,
			double ww,
			double mm	) {

  C_KEPLER *kepler;

#ifdef FREEON
  kepler = (C_KEPLER *)freeobjs.new_object(BUS);
  if (kepler == NULL) {
    fprintf(stderr,"Error (EYEOBJ), could not create kepler object\n");
    exit(1);
  }
#else
  kepler = new C_BUS();
  kepler->set_freeid(BUS);
#endif

  kepler->init_aeiowm(aa,ee,ii,oo,ww,mm);

  eom = kepler;

//...... test that the motion is correct

/*
  double Ttest, Xtest[3], Vtest[3];

  Ttest = 20000.0;
  kepler->get_pos_vel(Ttest,Xtest,Vtest);
  fprintf(stderr,"EYE 1> T=%f, X=(%f %f %f), V=(%f %f %f)\n",
	Ttest,Xtest[0],Xtest[1],Xtest[2],Vtest[0],Vtest[1],Vtest[2]);

  Ttest += 43200;
  kepler->set_start_time(43200);

  kepler->get_pos_vel(Ttest,Xtest,Vtest);
  fprintf(stderr,"EYE 2> T=%f, X=(%f %f %f), V=(%f %f %f)\n",
	Ttest,Xtest[0],Xtest[1],Xtest[2],Vtest[0],Vtest[1],Vtest[2]);
*/

//...... initialize the start time and the end time (1 year from now)

  kepler->set_start_time(time);
  kepler->set_endtime(31536000.0);

  kepler->set_ECI(0);

//...... set up the eom (kepler) information

  eom->set_sequence(0);
  eom->set_object_type(OBJECT_TYPE);
  eom->set_object_id(LOCAL_ID);
  eom->set_object_node(NODE);
  eom->set_id(GLOBAL_ID);

//...... put the eom into the script (one equation of motion)

  script->push_bot(eom);

}

/************************************************************************
* get_pos_vel : get the position and velocity of the B.E.		*
************************************************************************/
void C_EYEOBJ::get_pos_vel(double time, double X[3], double V[3]) {
  double t;

  t = time;
  while (t < 0.0) t += period;
  eom->get_pos_vel(t,X,V);

}
