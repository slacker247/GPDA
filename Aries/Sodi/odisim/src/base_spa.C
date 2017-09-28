// base_space.C method file

#include <stdio.h>
#include <math.h>
#include "base_space.H"
#include "def.h"

#define SOUND 0.344
#define DENSITY 1000.0

/************************************************************************
* get_friction : get the friction due to atmospheric drag		*
************************************************************************/
void C_BASE_SPACE::get_friction(double r[3], double v[3],
				double massrocket, double friction[3]) {
  double altitude;
  double range;
  double velocity;
  double density;
  double normal[3];
  double fmag;
  double sound;

//...... initialize the friction to zero as a default

  friction[0] = 0.0;
  friction[1] = 0.0;
  friction[2] = 0.0;
  if (area == 0.0) return;

//...... compute altitude and velocity

  velocity = sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);
  if (velocity == 0.0) return;

  range = sqrt(r[0]*r[0] + r[1]*r[1] + r[2]*r[2]);
  altitude = range - RE;

//...... check if out of the atmosphere

  if (altitude > RE + ATM_ALTITUDE) return;

//...... compute the atmospheric density

  density = (altitude/ATM_ALTITUDE)*DENSITY;

//...... compute the speed of sound

  sound = SOUND;

//...... compute a normalized vector opposite to the velocity vector

  normal[0] = -v[0]/velocity;
  normal[1] = -v[1]/velocity;
  normal[2] = -v[2]/velocity;

//...... subsonic case

  if (velocity < sound) {
    fmag = 2.0*area*velocity*density/massrocket;
  }

//...... supersonic case

  if (velocity >= sound) {
    fmag = 2.0*area*velocity*density/massrocket;
  }

//...... friction is fmag x normal vector

  friction[0] = fmag*normal[0];
  friction[1] = fmag*normal[1];
  friction[2] = fmag*normal[2];

}

/************************************************************************
* rotate : rotate the ECI position and velocity to start position	*
************************************************************************/
void C_BASE_SPACE::rotate(double rt[3], double vt[3]) {
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

