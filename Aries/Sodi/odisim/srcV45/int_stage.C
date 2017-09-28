// int_stage.C method file

#include <stdio.h>
#include <math.h>
#include "int_stage.H"
#include "def.h"

#define GM 398601.3
#define RADDEG  0.0174532925199432958
#define TSPLINE 50.0

/************************************************************************
* C_INT_STAGE : construct a stage object				*
************************************************************************/
C_INT_STAGE::C_INT_STAGE() {

  resolution = 10.0;	// default resolution is 10 km
  maxstep = 1.0;	// default max time step is 10 seconds
  tspline = TSPLINE;	// default 50 second max time for spline
  nsplines = NSP;	// initialize 0 splines

  area = 0.000001;	// cross sectional area is 1 meter squared

}

/************************************************************************
* init_values : initialize the initial position for this stage		*
************************************************************************/
void C_INT_STAGE::init_values() {
  double temp;

//...... compute the maneuver vectors

  if (manuever) {
    mandir[0] = S0[1]*Sb[2] - S0[2]*Sb[1];
    mandir[1] = S0[2]*Sb[0] - S0[0]*Sb[2];
    mandir[2] = S0[0]*Sb[1] - S0[1]*Sb[0];
    temp = sqrt(mandir[0]*mandir[0]+mandir[1]*mandir[1]+mandir[2]*mandir[2]);
    mandir[0] /= temp;
    mandir[1] /= temp;
    mandir[2] /= temp;
  }

//...... compute the normal an the plane to the initial thrust vector

  temp = S0[0]*Sb[0] + S0[1]*Sb[1] + S0[2]*Sb[2];
  if (fabs(temp-Ctheta) > 0.000001) {
    fprintf(stderr,"Error, (INT_STAGE) bad cos(theta)\n");
  }

  N[0] = Sb[0]-Ctheta*S0[0];
  N[1] = Sb[1]-Ctheta*S0[1];
  N[2] = Sb[2]-Ctheta*S0[2];
  temp = sqrt(N[0]*N[0]+N[1]*N[1]+N[2]*N[2]);
  N[0] /= temp;
  N[1] /= temp;
  N[2] /= temp;

  temp = N[0]*S0[0]+N[1]*S0[1]+N[2]*S0[2];
  if (fabs(temp) > 0.000001) {
    fprintf(stderr,"Error, (INT_STAGE) bad N vector\n");
  }

//...... create the splines

  if (nsplines == 0) {
//    nsplines = int(Tb/TSPLINE) + 1;
//    splines = new C_SPLINE6[nsplines];
//    tspline = Tb / nsplines;
  }else{
    tspline = Tb/nsplines;
  }

//...... integrate

  TT = Tb + Tcoast;
  integrate();

}

/************************************************************************
* get_thrust : get the thrust as a function of time			*
************************************************************************/
void C_INT_STAGE::get_thrust(double t, double St[3]) {
  double angle;
  double ca;
  double sa;
  double temp;
  double Mt0;
  double Mt;

//...... check if we are coasting (no thrust)

  if (t >= Tb) {
    St[0] = 0.0;
    St[1] = 0.0;
    St[2] = 0.0;
    return;
  }

//...... compute the thrust angle

  angle = Theta*(t/TT);
  ca = cos(angle);
  sa = sin(angle);

  St[0] = S0[0]*ca + N[0]*sa;
  St[1] = S0[1]*ca + N[1]*sa;
  St[2] = S0[2]*ca + N[2]*sa;

//...... check that the magnitude is still 1

  temp = St[0]*St[0] + St[1]*St[1] + St[2]*St[2];
  if (fabs(temp-1.0) > 0.000001) {
    fprintf(stderr,"Error (INT_STAGE) bad thrust magnitude\n");
  }

//...... make the units right

  Mt = get_massrocket(t);
  Mt0 = get_massrocket(0.0);
  temp = Thrust*0.0098*Mt0/Mt;

  St[0] *= temp;
  St[1] *= temp;
  St[2] *= temp;

}

/************************************************************************
* get pos_vel : get the position and velocity at a given time		*
************************************************************************/
void C_INT_STAGE::get_pos_vel(double tt, double rt[3], double vt[3]) {
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

//...... check if the time is near the end of boost

    if (fabs(t-Tb) < 0.000001) {
      for (i=0; i<3; i++) {
        rt[i] = R1[i];
        vt[i] = V1[i];
      }
      break;
    }

//...... check if the time is near the end of stage

    if (fabs(t-TT) < 0.000001) {
      for (i=0; i<3; i++) {
        rt[i] = Rend[i];
        vt[i] = Vend[i];
      }
      break;
    }

//...... check if the time is less than the burn time

    if (t < Tb) {

      spline_index = int(t/tspline);
      if (spline_index >= nsplines) spline_index = nsplines-1;
      splines[spline_index].set_rotate(coe,soe);
      splines[spline_index].set_ECI(ECI);
      splines[spline_index].getposvel(tt,rt,vt);
      return;

    }

//...... time is in the coast phase

    coast(tt,rt,vt);
    loopflag = 0;
    break;

  }

//...... throw in a manuever if the missile is manuevering

  if (manuever) man_pos_vel(t, rt, vt);

//...... rotate ECI position and velocity by the launch position

  rotate(rt,vt);

//...... rotate to ECR coordinates if not ECI

  if (!ECI) {
    eci_to_ecr(tt,rt,vt);
  }

}

/************************************************************************
* integrate : integrate the stage					*
************************************************************************/
void C_INT_STAGE::integrate() {
  int i,j;
  int loopflag;
  double rt[3];
  double vt[3];
  double at[3];
  double St[3];
  double gt[3];
  double Rt;
  double Rt3;
  double Vt;
  double dt;
  double t;
  double tendspline;
  int spline_index;
  int spline_flag;
  double rold[3];
  double vold[3];
  double aold[3];
  double friction[3];

//...... initialize the position and velocity for integration

  for (i=0; i<3; i++) {
    rt[i] = R0[i];
    vt[i] = V0[i];
  }

//...... initialize the initial x,v,a terms for the spline

  Rt = sqrt(rt[0]*rt[0]+rt[1]*rt[1]+rt[2]*rt[2]);
  Rt3 = Rt*Rt*Rt;
  get_thrust(0.0,St);
  get_friction(rt,vt,get_massrocket(0.0),friction);

  for (j=0; j<3; j++) {

    gt[j] = -GM * rt[j] / Rt3;

    rold[j] = rt[j];
    vold[j] = vt[j];
    aold[j] = St[j] + gt[j] + friction[j];

  }

//...... loop over time

  tendspline = tspline;
  spline_index = 0;

  t = 0.0;
  loopflag = 1;

  while (loopflag) {

    spline_flag = 0;

    Rt = sqrt(rt[0]*rt[0]+rt[1]*rt[1]+rt[2]*rt[2]);
    Vt = sqrt(vt[0]*vt[0]+vt[1]*vt[1]+vt[2]*vt[2]);

    dt = resolution / (Vt+0.001);
    if (dt > maxstep) dt = maxstep;

    if (loopflag == 1) {

      if (t+dt >= tendspline) {
        dt = tendspline-t;
	spline_flag = 1;
      }

      if (spline_index == nsplines) {
        loopflag = 2;
      }

    }

    if (loopflag == 2) {
      if (t+dt >= TT) {
        dt = TT-t;
        loopflag = 0;
      }
    }

    get_thrust(t,St);
    get_friction(rt,vt,get_massrocket(t),friction);

    Rt3 = Rt*Rt*Rt;

    for (j=0; j<3; j++) {

      gt[j] = -GM * rt[j] / Rt3;
      at[j] = St[j] + gt[j] + friction[j];

      rt[j] += vt[j]*dt + at[j]*(dt*dt)/2.0;
      vt[j] += at[j]*dt;

    }

    t += dt;

//...... fit spline if necessary

    if (spline_flag) {
      Rt = sqrt(rt[0]*rt[0]+rt[1]*rt[1]+rt[2]*rt[2]);
      Rt3 = Rt*Rt*Rt;
      get_thrust(t-0.000001,St);
      get_friction(rt,vt,get_massrocket(t),friction);
      for (j=0; j<3; j++) {
        gt[j] = -GM * rt[j] / Rt3;
        at[j] = St[j] + gt[j] + friction[j];
      }

      splines[spline_index].fit_spline6(t0+t-tspline, t0+t,
					rold, vold, aold,
					rt, vt, at);
      splines[spline_index].set_rotate(1.0,0.0);
      splines[spline_index].init_maneuver(ngs, int(sman));
      splines[spline_index].set_ECI(1);

      for (j=0; j<3; j++) {
        rold[j] = rt[j];
        vold[j] = vt[j];
        aold[j] = at[j];
      }

      tendspline += tspline;
      spline_index++;

    }

//...... set up initial values for coasting

    if (fabs(t-Tb) < 0.000001) {
      for (j=0; j<3; j++) {
        R1[j] = rt[j];
        V1[j] = vt[j];
      }
    }

  }

//...... initialize the final position and velocity vectors

  for (i=0; i<3; i++) {
    Rend[i] = rt[i];
    Vend[i] = vt[i];
  }

}

/************************************************************************
* check_spline6 : print out the difference using kep, spline		*
************************************************************************/
void C_INT_STAGE::check_spline6() {
  int i,j;
  int loopflag;
  double rt[3];
  double vt[3];
  double at[3];
  double St[3];
  double gt[3];
  double Rt;
  double Rt3;
  double Vt;
  double dt;
  double t;
  double d0,d1,d2,d,dd;
  double gpos[3];
  double gvel[3];
  double ipos[3];
  double ivel[3];
  double friction[3];
  double dmax;
  double ddmax;
  double tmax;

//...... initialize the position and velocity

  for (i=0; i<3; i++) {
    rt[i] = R0[i];
    vt[i] = V0[i];
  }

//...... loop over time

  t = 0.0;
  dmax = 0.0;
  loopflag = 1;

  while (loopflag) {

    Rt = sqrt(rt[0]*rt[0]+rt[1]*rt[1]+rt[2]*rt[2]);
    Vt = sqrt(vt[0]*vt[0]+vt[1]*vt[1]+vt[2]*vt[2]);

    dt = resolution / (Vt+0.001);
    if (dt > maxstep) dt = maxstep;

    if (loopflag == 1) {
      if (t+dt >= Tb) {
        dt = Tb-t + 0.000001;
        loopflag = 2;
      }
    }

    if (loopflag == 2) {
      if (t+dt >= TT) {
        dt = TT-t;
        loopflag = 0;
      }
    }

    get_thrust(t,St);
    get_friction(rt,vt,get_massrocket(t),friction);

    Rt3 = Rt*Rt*Rt;

    for (j=0; j<3; j++) {

      gt[j] = -GM * rt[j] / Rt3;
      at[j] = St[j] + gt[j] + friction[j];

      rt[j] += vt[j]*dt + at[j]*(dt*dt)/2.0;
      vt[j] += at[j]*dt;

    }

    t += dt;

//...... check the difference

    for (j=0; j<3; j++) {
      ipos[j] = rt[j];
      ivel[j] = vt[j];
    }

    if (manuever) man_pos_vel(t+t0, ipos, ivel);
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

