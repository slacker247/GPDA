// convert.C method file

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <float.h>
#include "def.h"
#include "convert.H"

#define OMEGAE  7.2921158279144062e-5

/************************************************************************
* C_CONVERT : construct a convert object					*
************************************************************************/
C_CONVERT::C_CONVERT() {

  gsam = 0.0;

}

/************************************************************************
* latlon_to_xyz : convert latitude, longitude to ECR xyz			*
************************************************************************/
void C_CONVERT::latlon_to_xyz(double lat, double lon, double x[3]) {
  double re_cos_lat;
  double sidereal_angle;

  re_cos_lat = RE * cos(lat);
  sidereal_angle = lon;

  x[0] = re_cos_lat * cos(sidereal_angle);
  x[1] = re_cos_lat * sin(sidereal_angle);
  x[2] = RE * sin(lat);
}

/************************************************************************
* latlon_to_xyz : convert latitude, longitude to ECI xyz at time t.	*
************************************************************************/
void C_CONVERT::latlon_to_xyz(double t, double lat, double lon, double x[3]) {
  double re_cos_lat;
  double sidereal_angle;

  re_cos_lat = RE * cos(lat);
  sidereal_angle = lon + gsam + t*OMEGAE;

  x[0] = re_cos_lat * cos(sidereal_angle);
  x[1] = re_cos_lat * sin(sidereal_angle);
  x[2] = RE * sin(lat);
}

/************************************************************************
* xyz_to_latlon : convert ECR xyz to latitude, longitude.		*
************************************************************************/
void C_CONVERT::xyz_to_latlon(double x[3], double &lat, double &lon) {
  double xmag;

  xmag = sqrt(x[0]*x[0] + x[1]*x[1] + x[2]*x[2]);

  if (xmag > 50000.0) {
    cerr << "Error, xmag too big" << endl;
    abort();
  }
  if (xmag < 6000.0) {
    cerr << "Error, xmag too small "
	 << x[0] << " " << x[1] << " " << x[2] << endl;
    abort();
  }

  double rp;

  rp = hypot(x[0], x[1]);

  if (fabs(rp) < 0.001) {
    fprintf(stderr,"Error, hypot not working\n");
    rp = sqrt(x[0]*x[0] + x[1]*x[1]);
  }
  if (fabs(rp) < 0.001) {
    fprintf(stderr,"Error, input for xyz to latlon is bad\n");
    lat = PI/2;
  }else{
    lat = atan(x[2]/rp);
  }

  lon = atan2(x[1],x[0]);
  lon = fmod(lon,TWOPI);
  if (lon < -M_PI) lon += TWOPI;
  if (lon >  M_PI) lon -= TWOPI;

/*
  if (fabs(x[0]) < 0.1) {
    fprintf(stderr,"Error, x[0] is too small\n");
  }

  if (fabs(x[1]) < 0.1) {
    fprintf(stderr,"Error, x[1] is too small\n");
  }

  if (fabs(rp) < 0.001) {
    fprintf(stderr,"Error, rp is too small\n");
  }

*/

}

/************************************************************************
* xyz_to_latlon : convert ECI xyz at time t to latitude, longitude.	*
************************************************************************/
void C_CONVERT::xyz_to_latlon(double t, double x[3], double &lat, double &lon) {
  double rp;

  rp = hypot(x[0], x[1]);
  lat = atan(x[2]/rp);
  lon = atan2(x[1],x[0]) - gsam - t*OMEGAE;
  lon = fmod(lon,TWOPI);
  if (lon < -M_PI) lon += TWOPI;
  if (lon >  M_PI) lon -= TWOPI;

}

/************************************************************************
* eci_to_ecr : convert from eci to ecr					*
************************************************************************/
void C_CONVERT::eci_to_ecr(double t, double x[3], double v[3]) {
  double co,so;
  double x0, x1;
  double v0, v1;

//...... compute rotation cosine and sine

  co = cos(t*OMEGAE);
  so = sin(t*OMEGAE);

//...... rotate x and y from the earth

  x0 = co*x[0] + so*x[1];
  x1 = - so*x[0] + co*x[1];

//...... rotate vx and vy from the earth

  v0 = co*v[0] + so*v[1]   - OMEGAE*(so*x[0] - co*x[1]);
  v1 = - so*v[0] + co*v[1] - OMEGAE*(co*x[0] + so*x[1]);

//...... store the values

  x[0] = x0;
  x[1] = x1;

  v[0] = v0;
  v[1] = v1;

}

/************************************************************************
* ecr_to_eci : convert from ecr to eci					*
************************************************************************/
void C_CONVERT::ecr_to_eci(double t, double x[3], double v[3]) {
  double co,so;
  double x0, x1;
  double v0, v1;

//...... compute rotation cosine and sine

  co = cos(t*OMEGAE);
  so = sin(t*OMEGAE);

//...... rotate x and y from the earth

  x0 = co*x[0] - so*x[1];
  x1 = so*x[0] + co*x[1];

//...... rotate vx and vy from the earth

  v0 = co*v[0] - so*v[1] - OMEGAE*(so*x[0] + co*x[1]);
  v1 = so*v[0] + co*v[1] - OMEGAE*(-co*x[0] + so*x[1]);

//...... store the values

  x[0] = x0;
  x[1] = x1;

  v[0] = v0;
  v[1] = v1;

}

