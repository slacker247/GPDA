// spline6.C method file

#include <stdio.h>
#include <math.h>
#include "spline6.H"

/************************************************************************
* fit_spline6 : fit the cubic spline parameters				*
************************************************************************/
void C_SPLINE6::fit_spline6(	double T0, double T1,
				double X0[3], double V0[3], double A0[3],
				double X1[3], double V1[3], double A1[3]) {
  double alpha, beta, gamma;
  double t,t2,t3;
  int j;

//...... initialize some values

  t0 = T0;
  t1 = T1;

  TT = t1-t0;
  Tend = t1;

  for (j=0; j<3; j++) {
    init_pos[j] = X0[j];
    init_vel[j] = V0[j];
    init_acc[j] = A0[j];
  }

//...... get some powers of time

  t = t1-t0;
  t2 = t*t;
  t3 = t*t2;

//...... compute the spline coefficients

  for (j=0; j<3; j++) {

    alpha = X1[j] - X0[j] - V0[j]*t - init_acc[j]*t2/2.0;
    beta  = V1[j] - V0[j] - A0[j]*t;
    gamma = A1[j] - A0[j];

    a_spline[j] = (alpha*10.0 - beta*4.0*t + gamma*t2/2.0) / t3;
    b_spline[j] = (-alpha*15.0/t + beta*7.0 - gamma*t) / t3;
    c_spline[j] = (alpha*6.0/t2 - beta*3.0/t + gamma/2.0) / t3;

  }

}

/************************************************************************
* get_pos_vel : get the position and velocity using spline		*
************************************************************************/
void C_SPLINE6::get_pos_vel(double t, double rt[3], double vt[3]) {
  getposvel(t,rt,vt);
}

/************************************************************************
* getposvel : get the position and velocity using spline		*
************************************************************************/
void C_SPLINE6::getposvel(double t, double rt[3], double vt[3]) {
  int j;
  double tt,t2,t3,t4,t5;

  tt = t-t0;

//...... compute powers of t

  t2 = tt*tt;
  t3 = tt*t2;
  t4 = tt*t3;
  t5 = tt*t4;

//...... compute the spline

  for (j=0; j<3; j++) {

    rt[j] = init_pos[j] + init_vel[j]*tt + init_acc[j]*t2/2.0
	+ a_spline[j]*t3 + b_spline[j]*t4 + c_spline[j]*t5;

    vt[j] = init_vel[j] + init_acc[j]*tt
	+ 3.0*a_spline[j]*t2 + 4.0*b_spline[j]*t3 + 5.0*c_spline[j]*t4;

  }

//...... throw in a manuever if the missile is manuevering

  if (sman) man_pos_vel(t, rt, vt);

//...... rotate ECI position and velocity by the launch position

  rotate(rt,vt);

//...... rotate to ECR coordinates if not ECI

  if (!ECI) {
    eci_to_ecr(t,rt,vt);
  }

}

/************************************************************************
* init_maneuver : initialize the manuever 				*
************************************************************************/
void C_SPLINE6::init_maneuver(double ng, int nst) {
  double rt[3];
  double vt[3];
  double temp;

  sman = 0;
  get_pos_vel(t1,rt,vt);

  ngs = ng;
  sman = nst;

  mandir[0] = init_pos[1]*rt[2] - init_pos[2]*rt[1];
  mandir[1] = init_pos[2]*rt[0] - init_pos[0]*rt[2];
  mandir[2] = init_pos[0]*rt[1] - init_pos[1]*rt[0];

  temp = sqrt(mandir[0]*mandir[0]+mandir[1]*mandir[1]+mandir[2]*mandir[2]);

  mandir[0] /= temp;
  mandir[1] /= temp;
  mandir[2] /= temp;

}

/************************************************************************
* man_pos_vel : add the manuever to the position and velocity		*
************************************************************************/
void C_SPLINE6::man_pos_vel(double t, double rt[3], double vt[3]) {
  double tt, ttsq_div2;
  double x1, v1;
  double x,v;
  int i;
  double mantau;
  double mantau4;

  if (t > t1) return;

//...... compute some time values

  mantau = (t1-t0) / sman;
  mantau4 = mantau/4.0;
  tt = t-t0;

//...... get in the right quadrant

  tt = fmod(tt,mantau);
  i = int(tt/mantau4);
  if (i>=4) i = 0;

  tt = fmod(tt,mantau4);
  ttsq_div2 = (tt*tt)/2.0;

  v1 = 0.0098*ngs*mantau4;
  x1 = v1*mantau4/2.0;

  switch (i) {

    case 0:
      x = 0.0098*ngs*ttsq_div2;
      v = 0.0098*ngs*tt;
      break;

    case 1:
      x = x1 + v1*tt - 0.0098*ngs*ttsq_div2;
      v = v1 - 0.0098*ngs*tt;
      break;

    case 2:
      x = 2*x1 - 0.0098*ngs*ttsq_div2;
      v = -0.0098*ngs*tt;
      break;

    case 3:
      x = x1 - v1*tt + 0.0098*ngs*ttsq_div2;
      v = -v1 + 0.0098*ngs*tt;
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
* rotate : rotate the ECI position and velocity to start position	*
************************************************************************/
void C_SPLINE6::rotate(double rt[3], double vt[3]) {
  double x,y,vx,vy;

  x = rt[0]*coe - rt[1]*soe;
  y = rt[0]*soe + rt[1]*coe;

  vx = vt[0]*coe - vt[1]*soe;
  vy = vt[0]*soe + vt[1]*coe;

  rt[0] = x;
  rt[1] = y;

  vt[0] = vx;
  vt[1] = vy;

}
