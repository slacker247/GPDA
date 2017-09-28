// locobj.C method file

#include <stdio.h>
#include <math.h>
#include "def.h"
#include "locobj.H"

int C_LOCOBJ::done = 0;
double C_LOCOBJ::VMAX = 0.0;
C_CONVERT *C_LOCOBJ::convert = NULL;

/************************************************************************
* C_LOCOBJ : construct a locobj object					*
************************************************************************/
C_LOCOBJ::C_LOCOBJ() {

  alive = 1;
  max_vel = 0.0;
  max_alt = 0.0;
  flyout_range = 1000.0;
  icon = -1;
  glob_gridid = -1;
  NAME = NULL;

  if (!done) {
    done = 1;
    convert = new C_CONVERT();
    VMAX = 0.0;
  }

}

/************************************************************************
* get_pos_vel : get the position and velocity at a given time 		*
************************************************************************/
void C_LOCOBJ::get_pos_vel(double /*t*/, double pos[3], double vel[3]) {
  
  pos[0] = start_position[0];
  pos[1] = start_position[1];
  pos[2] = start_position[2];

  vel[0] = 0.0;
  vel[1] = 0.0;
  vel[2] = 0.0;

}

/************************************************************************
* set_start_position : set the starting position of the object	 	*
************************************************************************/
void C_LOCOBJ::set_start_position(double R[3]) {

  start_position[0] = R[0];
  start_position[1] = R[1];
  start_position[2] = R[2];

}

/************************************************************************
* random_position : generate a random position				*
************************************************************************/
void C_LOCOBJ::random_position(double R[3]) {
  double mag, alt;

  RANDOM->get_random_unit_vector(R);

  if (max_alt != 0.0) {
    RANDOM->set_float_limits(0.0,max_alt);
    alt = RANDOM->get_random_float(25);
    mag = (RE + alt);
  }else{
    mag = RE;
  }

  R[0] *= mag;
  R[1] *= mag;
  R[2] *= mag;

}

/************************************************************************
* random_next_position : randomly get a next position  			*
************************************************************************/
void C_LOCOBJ::random_next_position(double R0[3], double R1[3]) {
  double mag, alt;

  RANDOM->get_random_mag_vector(flyout_range, R1);
  R1[0] += R0[0];
  R1[1] += R0[1];
  R1[2] += R0[2];

  mag = sqrt(R1[0]*R1[0] + R1[1]*R1[1] + R1[2]*R1[2]);

  if (max_alt != 0.0) {
    RANDOM->set_float_limits(0.0,max_alt);
    alt = RANDOM->get_random_float(25);
    mag = (RE + alt) / mag;
  }else{
    mag = RE / mag;
  }

  R1[0] *= mag;
  R1[1] *= mag;
  R1[2] *= mag;

}

