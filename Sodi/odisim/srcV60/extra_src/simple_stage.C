// stage.C method file

#include <stdio.h>
#include <math.h>
#include "stage.H"

#define GM 398601.3

/************************************************************************
* C_STAGE : construct a stage object					*
************************************************************************/
C_STAGE::C_STAGE() {

}

/************************************************************************
* init_pars : initialize the parameters for this stage			*
************************************************************************/
void C_STAGE::init_pars(float thrust, float tburn, float mrocket, float mfuel,
			float mshell) {

  T = thrust;
  Tb = tburn;
  Mr = mrocket;
  Mf = mfuel;
  Ms = mshell;

  mu = Mf/Mr;
  beta = Tb*T/(Mr*mu);

}

/************************************************************************
* init_R0 : initialize the initial position for this stage		*
************************************************************************/
void C_STAGE::init_R0(float r0[3]) {
  float r;

  R0[0] = r0[0];
  R0[1] = r0[1];
  R0[2] = r0[2];

  r = sqrt(R0[0]*R0[0] + R0[1]*R0[1] + R0[2]*R0[2]);
  gamma = - Tb * GM / (r*r*r);

}

/************************************************************************
* init_R0 : initialize the initial position for this stage		*
************************************************************************/
void C_STAGE::init_R0(float r0[3], float r) {

  R0[0] = r0[0];
  R0[1] = r0[1];
  R0[2] = r0[2];

  gamma = - (Tb*GM) / (r*r*r);

}

/************************************************************************
* get pos_vel : get the position and velocity at a given time		*
************************************************************************/
void C_STAGE::get_pos_vel(float t, float rt[3], float vt[3]) {
  float alpha;
  float almu;
  float logterm;
  int i;

  alpha = t / Tb;
  almu = alpha*mu;
  logterm = log(1.0-almu);

  for (i=0; i<3; i++) {
    vt[i] = V0[i] + gamma*alpha*R0[i] - beta*logterm*A0[i];
    rt[i] = R0[i]*(1.0+gamma*alpha*alpha/2.0)
	  + V0[i]*alpha
	  + (beta/mu)*A0[i]*((1.0-almu)*logterm + almu);
  }

}


