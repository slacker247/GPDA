// int_bus.C method file

#include <stdio.h>
#include <math.h>
#include "int_bus.H"
#include "def.h"

#define GM 398601.3
#define RADDEG  0.0174532925199432958
#define TSPLINE 50.0

/************************************************************************
* C_INT_BUS : construct a bus object					*
************************************************************************/
C_INT_BUS::C_INT_BUS() {
  int i;

  area = 0.000001;	// cross sectional area is 1 meter squared
//  area = 0.0;		// cross sectional area is 0 meter squared

  resolution = 10.0;	// default resolution is 10 km
  maxstep = 1.0;	// default max time step is 10 seconds

  nsplines = NSPIBUS;	// initialize the number of splines
  for (i=0; i<NSPIBUS; i++) tspline[i] = 0.0;

  set_ballistic(1);
  set_icon(6);
  massrocket = 1000.0;

  t0 = 0.0;
  ECI = 1;
  coe = 1.0;
  soe = 0.0;

}

/************************************************************************
* init_values : initialize the initial position for this bus		*
************************************************************************/
void C_INT_BUS::init(double r[3], double v[3]) {
  int i;

  for (i=0; i<3; i++) {
    R0[i] = r[i];
    V0[i] = v[i];
  }

//...... initialize the rest of the values

  init_values();

}

/************************************************************************
* init_values : initialize the initial position for this bus		*
************************************************************************/
void C_INT_BUS::init_values() {
  int i;

  ECI = 1;
  coe = 1.0;
  soe = 0.0;
  for (i=0; i<NSPIBUS; i++) tspline[i] = 0.0;

//...... integrate

  integrate();

}

/************************************************************************
* get pos_vel : get the position and velocity at a given time		*
************************************************************************/
void C_INT_BUS::get_pos_vel(double tt, double rt[3], double vt[3]) {
  int i;
  double t;
  int loopflag;
  int spline_index;

//...... set the time variable so that zero is the start time

  t = tt-t0;

  loopflag = 1;
  while (loopflag) {

//...... check if the time is near the beginning

    if (fabs(t) < 0.000001) {
      for (i=0; i<3; i++) {
        rt[i] = R0[i];
        vt[i] = V0[i];
      }
      break;
    }

//...... check if the time is near the impact time

    if (fabs(t-TT) < 0.000001) {
      for (i=0; i<3; i++) {
        rt[i] = Rend[i];
        vt[i] = Vend[i];
      }
      break;
    }

//...... check if the time is less than the burn time

    if (t < TT) {
      for (spline_index = 0; spline_index < NSPIBUS; spline_index++) {
        if (t <= tspline[spline_index]) {
          splines[spline_index].set_rotate(coe,soe);
          splines[spline_index].set_ECI(ECI);
          splines[spline_index].getposvel(tt,rt,vt);
	  return;
        }
      }
    }

  }

//...... rotate ECI position and velocity by the launch position

  rotate(rt,vt);

//...... rotate to ECR coordinates if not ECI

  if (!ECI) {
    eci_to_ecr(tt,rt,vt);
  }

}

/************************************************************************
* integrate : integrate the bus						*
************************************************************************/
void C_INT_BUS::integrate() {
  int i,j;
  int loopflag;
  double rt[3];
  double vt[3];
  double at[3];
  double gt[3];
  double Rt;
  double Rt3;
  double Vt;
  double dt;
  double t;
  double rold[3];
  double vold[3];
  double aold[3];
  double friction[3];
  double dr;
  double rdot;

//...... initialize the position and velocity and time for integration

  t = 0.0;

  for (i=0; i<3; i++) {
    rt[i] = R0[i];
    vt[i] = V0[i];
  }

//...... initialize the initial x,v,a terms for the spline

  Rt = sqrt(rt[0]*rt[0]+rt[1]*rt[1]+rt[2]*rt[2]);
  Rt3 = Rt*Rt*Rt;
  get_friction(rt,vt,get_massrocket(0.0),friction);

  for (j=0; j<3; j++) {

    gt[j] = -GM * rt[j] / Rt3;

    rold[j] = rt[j];
    vold[j] = vt[j];
    aold[j] = gt[j] + friction[j];

  }

//...... first loop if in atmosphere until impact or exit atmosphere

  if (Rt-RE < ATM_ALTITUDE) {

    loopflag = 1;

    while (loopflag) {

      Rt = sqrt(rt[0]*rt[0]+rt[1]*rt[1]+rt[2]*rt[2]);
      Vt = sqrt(vt[0]*vt[0]+vt[1]*vt[1]+vt[2]*vt[2]);

      if (loopflag == 1) {
        if (Rt-RE < 0.0) loopflag = 2;
        if (Rt-RE > ATM_ALTITUDE) loopflag = 3;
	if (loopflag == 1) {
          dt = resolution / (Vt+0.001);
          if (dt > maxstep) dt = maxstep;
        }
      }

      Rt3 = Rt*Rt*Rt;
      get_friction(rt,vt,get_massrocket(t),friction);

      for (j=0; j<3; j++) {
        gt[j] = -GM * rt[j] / Rt3;
        at[j] = gt[j] + friction[j];
      }

      if (loopflag == 2) {
        dr = Rt-RE;
	if (fabs(dr) < 0.001) break;
        rdot = (vt[0]*rt[0] + vt[1]*rt[1] + vt[2]*rt[2])/Rt;
        dt = - dr / rdot;
      }

      if (loopflag == 3) {
        dr = Rt-RE-ATM_ALTITUDE;;
	if (fabs(dr) < 0.001) break;
        rdot = (vt[0]*rt[0] + vt[1]*rt[1] + vt[2]*rt[2])/Rt;
        dt = - dr / rdot;
      }

      for (j=0; j<3; j++) {

        gt[j] = -GM * rt[j] / Rt3;
        at[j] = gt[j] + friction[j];

        rt[j] += vt[j]*dt + at[j]*(dt*dt)/2.0;
        vt[j] += at[j]*dt;

      }

      t += dt;

    }

    Rt = sqrt(rt[0]*rt[0]+rt[1]*rt[1]+rt[2]*rt[2]);
    Rt3 = Rt*Rt*Rt;
    get_friction(rt,vt,get_massrocket(t),friction);
    for (j=0; j<3; j++) {
      gt[j] = -GM * rt[j] / Rt3;
      at[j] = gt[j] + friction[j];
    }

    tspline[0] = t;

    splines[0].fit_spline6(t0, t0+t, rold, vold, aold, rt, vt, at);
    splines[0].set_rotate(1.0,0.0);
    splines[0].set_ECI(1);

    for (j=0; j<3; j++) {
      rold[j] = rt[j];
      vold[j] = vt[j];
      aold[j] = at[j];
    }

  }

  if (loopflag == 2) {
    Tend = t0 + t;
    TT = t;
    for (i=0; i<3; i++) {
      Rend[i] = rt[i];
      Vend[i] = vt[i];
    }
    return;
  }

//...... second loop while out of atmosphere until re-entry

  loopflag = 1;

  while (loopflag) {

    Rt = sqrt(rt[0]*rt[0]+rt[1]*rt[1]+rt[2]*rt[2]);
    Vt = sqrt(vt[0]*vt[0]+vt[1]*vt[1]+vt[2]*vt[2]);

    if (loopflag == 1) {
      if (Rt-RE < ATM_ALTITUDE) loopflag = 2;
      if (loopflag == 1) {
        dt = resolution / (Vt+0.001);
//        if (dt > maxstep) dt = maxstep;
      }
    }

    Rt3 = Rt*Rt*Rt;

    for (j=0; j<3; j++) {
      gt[j] = -GM * rt[j] / Rt3;
      at[j] = gt[j];
    }

    if (loopflag == 2) {
      dr = Rt-RE-ATM_ALTITUDE;
      if (fabs(dr) < 0.001) break;
      rdot = (vt[0]*rt[0] + vt[1]*rt[1] + vt[2]*rt[2])/Rt;
      dt = - dr / rdot;
    }

    for (j=0; j<3; j++) {

      gt[j] = -GM * rt[j] / Rt3;
      at[j] = gt[j] + friction[j];

      rt[j] += vt[j]*dt + at[j]*(dt*dt)/2.0;
      vt[j] += at[j]*dt;

    }

    t += dt;

  }

  Rt = sqrt(rt[0]*rt[0]+rt[1]*rt[1]+rt[2]*rt[2]);
  Rt3 = Rt*Rt*Rt;
  for (j=0; j<3; j++) {
    gt[j] = -GM * rt[j] / Rt3;
    at[j] = gt[j];
  }

  tspline[1] = t;

  splines[1].fit_spline6(t0+tspline[0], t0+t, rold, vold, aold, rt, vt, at);
  splines[1].set_rotate(1.0,0.0);
  splines[1].set_ECI(1);

  for (j=0; j<3; j++) {
    rold[j] = rt[j];
    vold[j] = vt[j];
    aold[j] = at[j];
  }

//...... third loop while in atmosphere until impact

  loopflag = 1;

  while (loopflag) {

    Rt = sqrt(rt[0]*rt[0]+rt[1]*rt[1]+rt[2]*rt[2]);
    Vt = sqrt(vt[0]*vt[0]+vt[1]*vt[1]+vt[2]*vt[2]);

    if (loopflag == 1) {
      if (Rt-RE < 0.0) loopflag = 2;
      if (loopflag == 1) {
        dt = resolution / (Vt+0.001);
        if (dt > maxstep) dt = maxstep;
      }
    }

    Rt3 = Rt*Rt*Rt;
    get_friction(rt,vt,get_massrocket(t),friction);

    for (j=0; j<3; j++) {
      gt[j] = -GM * rt[j] / Rt3;
      at[j] = gt[j] + friction[j];
    }

    if (loopflag == 2) {
      dr = Rt-RE;
      if (fabs(dr) < 0.000001) break;
      rdot = (vt[0]*rt[0] + vt[1]*rt[1] + vt[2]*rt[2])/Rt;
      dt = - dr / rdot;
    }

    for (j=0; j<3; j++) {

      gt[j] = -GM * rt[j] / Rt3;
      at[j] = gt[j] + friction[j];

      rt[j] += vt[j]*dt + at[j]*(dt*dt)/2.0;
      vt[j] += at[j]*dt;

    }

    t += dt;

  }

  Rt = sqrt(rt[0]*rt[0]+rt[1]*rt[1]+rt[2]*rt[2]);
  Rt3 = Rt*Rt*Rt;
  get_friction(rt,vt,get_massrocket(t),friction);
  for (j=0; j<3; j++) {
    gt[j] = -GM * rt[j] / Rt3;
    at[j] = gt[j] + friction[j];
  }

  tspline[2] = t;

  splines[2].fit_spline6(t0+tspline[1], t0+t, rold, vold, aold, rt, vt, at);
  splines[2].set_rotate(1.0,0.0);
  splines[2].set_ECI(1);


//...... initialize the final position and velocity vectors

  for (i=0; i<3; i++) {
    Rend[i] = rt[i];
    Vend[i] = vt[i];
  }

  Tend = t0 + t;
  TT = t;

}

/************************************************************************
* check_spline6 : print out the difference using kep, spline		*
************************************************************************/
void C_INT_BUS::check_spline6() {
  int i,j;
  int loopflag;
  double rt[3];
  double vt[3];
  double at[3];
  double gt[3];
  double Rt;
  double Rt3;
  double Vt;
  double dt;
  double t;
  double d0,d1,d2,d,dd;
  double dmax;
  double ddmax;
  double tmax;
  double gpos[3];
  double gvel[3];
  double ipos[3];
  double ivel[3];
  double friction[3];

//...... initialize the position and velocity

  for (i=0; i<3; i++) {
    rt[i] = R0[i];
    vt[i] = V0[i];
  }

//...... loop over time

  t = 0.0;
  loopflag = 1;
  dmax = 0.0;

  while (loopflag) {

    Rt = sqrt(rt[0]*rt[0]+rt[1]*rt[1]+rt[2]*rt[2]);
    Vt = sqrt(vt[0]*vt[0]+vt[1]*vt[1]+vt[2]*vt[2]);

    dt = resolution / (Vt+0.001);
    if (dt > maxstep) {
      if (Rt-RE < ATM_ALTITUDE) dt = maxstep;
    }

    if (loopflag == 1) {
      if (t+dt >= TT) {
        dt = TT-t;
        loopflag = 2;
      }
    }

    if (loopflag == 2) {
      if (t >= TT - 0.000001) {
        dt = TT-t;
        loopflag = 0;
      }
    }

    get_friction(rt,vt,get_massrocket(t),friction);

    Rt3 = Rt*Rt*Rt;

    for (j=0; j<3; j++) {

      gt[j] = -GM * rt[j] / Rt3;
      at[j] = gt[j] + friction[j];

      rt[j] += vt[j]*dt + at[j]*(dt*dt)/2.0;
      vt[j] += at[j]*dt;

    }

    t += dt;

//...... check the difference

    for (j=0; j<3; j++) {
      ipos[j] = rt[j];
      ivel[j] = vt[j];
    }

    rotate(ipos,ivel);
    if (!ECI) eci_to_ecr(t+t0,ipos,ivel);

    get_pos_vel(t+t0,gpos,gvel);

    d0 = gpos[0]-ipos[0];
    d1 = gpos[1]-ipos[1];
    d2 = gpos[2]-ipos[2];
    d = sqrt(d0*d0+d1*d1+d2*d2);

    d0 = gvel[0]-ivel[0];
    d1 = gvel[1]-ivel[1];
    d2 = gvel[2]-ivel[2];
    dd = sqrt(d0*d0+d1*d1+d2*d2);

    if (d > dmax) {
      dmax = d;
      ddmax = dd;
      tmax = t;
    }
/*
    fprintf(stderr,"Time %f, Position Error %f, Velocity Error %f\n",
	t,d,dd);
*/
  }

  fprintf(stderr,"Time %f, Position Error %f, Velocity Error %f\n",
	tmax,dmax,ddmax);

}

