// greatcirc.C method file

#include <stdio.h>
#include <math.h>
#include "def.h"
#include "greatcirc.H"

/************************************************************************
* set_course : set the great circle course				*
************************************************************************/
void C_GREATCIRC::set_course(	double Pos0[3], double Pos1[3],
				double velocity, double tstart) {
  double alt0, alt1;
  int i;
  double temp1, temp2;
  double rmin;
  double d;

//...... convert Pos0 and Pos1 into R0 and R1 unit vectors

  temp1 = sqrt(Pos0[0]*Pos0[0] + Pos0[1]*Pos0[1] + Pos0[2]*Pos0[2]);
  temp2 = sqrt(Pos1[0]*Pos1[0] + Pos1[1]*Pos1[1] + Pos1[2]*Pos1[2]);

  R0[0] = Pos0[0] / temp1;
  R0[1] = Pos0[1] / temp1;
  R0[2] = Pos0[2] / temp1;

  R1[0] = Pos1[0] / temp2;
  R1[1] = Pos1[1] / temp2;
  R1[2] = Pos1[2] / temp2;

  alt0 = temp1 - RE;
  alt1 = temp2 - RE;

  vel = velocity;
  t0 = tstart;
  altitude = alt0;

  costheta = R0[0]*R1[0] + R0[1]*R1[1] + R0[2]*R1[2];
  theta = acos(costheta);
  sintheta = sin(theta);

  Rrot[0] = (R0[1]*R1[2] - R0[2]*R1[1]) / sintheta;
  Rrot[1] = (R0[2]*R1[0] - R0[0]*R1[2]) / sintheta;
  Rrot[2] = (R0[0]*R1[1] - R0[1]*R1[0]) / sintheta;

  for (i=0; i<3; i++) {
    R0[i] *= RE + alt0;
    R1[i] *= RE + alt1;
  }

  if (alt0 < alt1) {
    rmin = RE + alt0;
  }else{
    rmin = RE + alt1;
  }

  temp1 = rmin*theta;
  temp2 = alt1-alt0;

  d = sqrt(temp1*temp1 + temp2*temp2);
  TT = d / vel;
  Valt = temp2/TT;
  Vcirc = temp1 / TT;

  xmax = 2.0*sin(theta/2.0);

  Tend = t0 + TT;

  Rend[0] = R1[0];
  Rend[1] = R1[1];
  Rend[2] = R1[2];

}

/************************************************************************
* set_course : set the great circle course				*
************************************************************************/
void C_GREATCIRC::set_course(	double lat0, double lon0, double alt0,
				double lat1, double lon1, double alt1,
				double velocity, double tstart	) {
  int i;
  double temp1, temp2;
  double rmin;
  double d;

  vel = velocity;
  t0 = tstart;
  altitude = alt0;

  latlon_to_xyz(lat0, lon0, R0);
  latlon_to_xyz(lat1, lon1, R1);

  costheta = (R0[0]*R1[0] + R0[1]*R1[1] + R0[2]*R1[2]) / RE2;
  theta = acos(costheta);
  sintheta = sin(theta);

  temp1 = sintheta*RE2;

  Rrot[0] = (R0[1]*R1[2] - R0[2]*R1[1]) / temp1;
  Rrot[1] = (R0[2]*R1[0] - R0[0]*R1[2]) / temp1;
  Rrot[2] = (R0[0]*R1[1] - R0[1]*R1[0]) / temp1;

  for (i=0; i<3; i++) {
    R0[i] *= (RE + alt0)/RE;
    R1[i] *= (RE + alt1)/RE;
  }

  if (alt0 < alt1) {
    rmin = RE + alt0;
  }else{
    rmin = RE + alt1;
  }

  temp1 = rmin*theta;
  temp2 = alt1-alt0;

  d = sqrt(temp1*temp1 + temp2*temp2);
  TT = d / vel;
  Valt = temp2/TT;
  Vcirc = temp1 / TT;

  xmax = 2.0*sin(theta/2.0);

  Tend = t0 + TT;

  Rend[0] = R1[0];
  Rend[1] = R1[1];
  Rend[2] = R1[2];

}

/************************************************************************
* set_course : set the great circle course				*
************************************************************************/
void C_GREATCIRC::set_course_TT( double lat0, double lon0, double alt0,
				 double lat1, double lon1, double alt1,
				 double tstart, double tend ) {

  int i;
  double temp1, temp2;
  double rmin;
  double d,d0,d1,d2,d3;

  t0 = tstart;
  TT = tend - tstart;
  altitude = alt0;

  latlon_to_xyz(lat0, lon0, R0);
  latlon_to_xyz(lat1, lon1, R1);

  d0 = fabs(R1[0] - R0[0]);
  d1 = fabs(R1[1] - R0[1]);
  d2 = fabs(R1[2] - R0[2]);
  d3 = fabs(alt1 - alt0);

  d = d0 + d1 + d2 + d3;
  if (d < .001) {
    vel = 0.0;
    return;
  }

  costheta = (R0[0]*R1[0] + R0[1]*R1[1] + R0[2]*R1[2]) / RE2;
  theta = acos(costheta);
  sintheta = sin(theta);

  temp1 = sintheta*RE2;
  if (temp1 == 0.0) temp1 = 0.000001;

  Rrot[0] = (R0[1]*R1[2] - R0[2]*R1[1]) / temp1;
  Rrot[1] = (R0[2]*R1[0] - R0[0]*R1[2]) / temp1;
  Rrot[2] = (R0[0]*R1[1] - R0[1]*R1[0]) / temp1;

  for (i=0; i<3; i++) {
    R0[i] *= (RE + alt0)/RE;
    R1[i] *= (RE + alt1)/RE;
  }

  if (alt0 < alt1) {
    rmin = RE + alt0;
  }else{
    rmin = RE + alt1;
  }

  temp1 = rmin*theta;
  temp2 = alt1-alt0;

  d = sqrt(temp1*temp1 + temp2*temp2);
  vel = d / TT;
  Valt = temp2/TT;
  Vcirc = temp1 / TT;

  xmax = 2.0*sin(theta/2.0);

  Tend = t0 + TT;

  Rend[0] = R1[0];
  Rend[1] = R1[1];
  Rend[2] = R1[2];

}

/************************************************************************
* get_pos_vel : get the position and velocity at any time		*
************************************************************************/
void C_GREATCIRC::get_pos_vel(double t, double R[3], double V[3]) {
  double a;
  double x;
  double temp1;

  if (t < t0) {
    fprintf(stderr,"Error, negative time for greatcirc\n");
  }

  if (t-Tend > 0.1) {
    fprintf(stderr,"Error, out of range time for greatcirc %f %f\n",t,Tend);
  }

  if (vel == 0.0) {
    R[0] = R0[0];
    R[1] = R0[1];
    R[2] = R0[2];

    V[0] = 0.0;
    V[1] = 0.0;
    V[2] = 0.0;

    return;
  }

  if (theta > 0.000001) {

    a = theta*((t-t0)/TT);
    x = (sin(a) / cos(a-theta/2.0)) / xmax;

    R[0] = R0[0] + x*(R1[0] - R0[0]);
    R[1] = R0[1] + x*(R1[1] - R0[1]);
    R[2] = R0[2] + x*(R1[2] - R0[2]);
    temp1 = sqrt(R[0]*R[0] + R[1]*R[1] + R[2]*R[2]);
    R[0] /= temp1;
    R[1] /= temp1;
    R[2] /= temp1;

    V[0] = Valt*R[0] + Vcirc*(Rrot[1]*R[2] - Rrot[2]*R[1]);
    V[1] = Valt*R[1] + Vcirc*(Rrot[2]*R[0] - Rrot[0]*R[2]);
    V[2] = Valt*R[2] + Vcirc*(Rrot[0]*R[1] - Rrot[1]*R[0]);

    temp1 = RE + altitude + Valt*(t-t0);

    R[0] *= temp1;
    R[1] *= temp1;
    R[2] *= temp1;

  }else{

    a = (t-t0)/TT;

    R[0] = R0[0] + a*(R1[0]-R0[0]);
    R[1] = R0[1] + a*(R1[1]-R0[1]);
    R[2] = R0[2] + a*(R1[2]-R0[2]);

  }

}

/************************************************************************
* get_tdist : get the minimum time for traveling a certain distance	*
************************************************************************/
double C_GREATCIRC::get_tdist(double dist) {

  return (dist/vel);

}

