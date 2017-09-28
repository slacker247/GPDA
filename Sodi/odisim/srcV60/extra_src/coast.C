// coast.C method file

#include <stdio.h>
#include <math.h>
#include "coast.H"

#define GM 398601.3

/************************************************************************
* C_COAST : construct a coast object					*
************************************************************************/
C_COAST::C_COAST() {

}

/************************************************************************
* init_R0 : initialize the initial position for this coast		*
************************************************************************/
void C_COAST::init_R0(double r0[3]) {
  double r;

  R0[0] = r0[0];
  R0[1] = r0[1];
  R0[2] = r0[2];

  r = sqrt(R0[0]*R0[0] + R0[1]*R0[1] + R0[2]*R0[2]);
  c2 = - GM / (r*r*r);

}

/************************************************************************
* get pos_vel : get the position and velocity at a given time		*
************************************************************************/
void C_COAST::get_pos_vel(double t, double rt[3], double vt[3]) {
  int i;

  for (i=0; i<3; i++) {
    vt[i] = V0[i] + (R0[i]*c2)*t;
    rt[i] = R0[i]*(1.0 + c2*(t*t)/2.0) + V0[i]*t;
  }

}

/************************************************************************
* get duration : get the duration of the coast 				*
************************************************************************/
double C_COAST::get_duration() {

  return Tc;

}


