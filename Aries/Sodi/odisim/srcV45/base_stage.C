// base_stage.C method file

#include <stdio.h>
#include <math.h>
#include "base_stage.H"
#include "def.h"

#define GM 398601.3
#define RADDEG  0.0174532925199432958

/************************************************************************
* C_BASE_STAGE : construct a stage object				*
************************************************************************/
C_BASE_STAGE::C_BASE_STAGE() {

  manuever = 0;
  t0 = 0.0;
  ECI = 1;
  coe = 1.0;
  soe = 0.0;

}

/************************************************************************
* init_pars : initialize the parameters for this stage			*
************************************************************************/
void C_BASE_STAGE::init_pars(double thrust, double thet, double tburn,
	double tcoast, double mrocket, double mfuel, double mshell) {

  Thrust = thrust;
  Theta = thet * RADDEG;
  Tb = tburn;
  Tcoast = tcoast;
  Mr = mrocket;
  Mf = mfuel;
  Ms = mshell;

  if (Theta >= 0.0) {
    variable_theta = 0;
  }else{
    variable_theta = 1;
  }

  Ctheta = 0.0;

}

/************************************************************************
* init_ANGS : initialize the initial and final boost angles		*
************************************************************************/
void C_BASE_STAGE::init_ANGS(double a0[3], double ab[3]) {
  int i;

  for (i=0; i<3; i++) {
    S0[i] = a0[i];
    Sb[i] = ab[i];
  }

}

/************************************************************************
* man_pos_vel : add the manuever to the position and velocity		*
************************************************************************/
void C_BASE_STAGE::man_pos_vel(double t, double rt[3], double vt[3]) {
  double t1, t1sq_div2;
  double x1, v1;
  double x,v;
  int i;

  if (t > Tb) return;

  t1 = fmod(t,mantau);
  i = int(t1/mantau4);
  if (i>=4) i = 0;

  t1 = fmod(t1,mantau4);
  t1sq_div2 = (t1*t1)/2.0;

  v1 = 0.0098*ngs*mantau4;
  x1 = v1*mantau4/2.0;

  switch (i) {

    case 0:
      x = 0.0098*ngs*t1sq_div2;
      v = 0.0098*ngs*t1;
      break;

    case 1:
      x = x1 + v1*t1 - 0.0098*ngs*t1sq_div2;
      v = v1 - 0.0098*ngs*t1;
      break;

    case 2:
      x = 2*x1 - 0.0098*ngs*t1sq_div2;
      v = -0.0098*ngs*t1;
      break;

    case 3:
      x = x1 - v1*t1 + 0.0098*ngs*t1sq_div2;
      v = -v1 + 0.0098*ngs*t1;
      break;
  }

  rt[0] += x*mandir[0];
  rt[1] += x*mandir[1];
  rt[2] += x*mandir[2];

  vt[0] += v*mandir[0];
  vt[1] += v*mandir[1];
  vt[2] += v*mandir[2];

//  printf("manuever at time %f is x = %f, and v = %f\n",t,x,v);

}

/************************************************************************
* coast : coast the stage by integrating				*
************************************************************************/
void C_BASE_STAGE::coast(double t, double rt[3], double vt[3]) {
  int i,j;
  double c;
  double r;
  double dt;
  int nsteps;
  double at;
  double friction[3];

//...... initialize the position and velocity vectors

  for (i=0; i<3; i++) {
    vt[i] = V1[i];
    rt[i] = R1[i];
  }

//...... set up the time and integration time steps

  dt = t-Tb-t0;

  if (dt > 1.0) {
    nsteps = int(dt) + 1;
    dt /= double(nsteps);
  }else{
    nsteps = 1;
  }

//...... loop and integrate

  for (i=0; i<nsteps; i++) {

    r = sqrt(rt[0]*rt[0] + rt[1]*rt[1] + rt[2]*rt[2]);
    c = - GM / (r*r*r);

    get_friction(rt,vt,get_massrocket(Tb),friction);

    for (j=0; j<3; j++) {
      at = c*rt[j] + friction[j];
      rt[j] += vt[j]*dt + at*(dt*dt)/2.0;
      vt[j] += at*dt;
    }

  }

}

/************************************************************************
* get duration : get the duration of this stage				*
************************************************************************/
double C_BASE_STAGE::get_duration() {

  return Tb+Tcoast;

}


