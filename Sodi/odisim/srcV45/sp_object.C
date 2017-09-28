// speedes_object.C method file

#include <stdio.h>
#include "speedes_object.H"
#include "freetypes.H"

/************************************************************************
* speedes_object - constructor						*
************************************************************************/
C_SPEEDES_OBJECT::C_SPEEDES_OBJECT() {
  int i;

  alive = 1;
  eom = NULL;
  GR_object = NULL;
  icon = -1;
  id = -1;

  Rmax = -1.0;
  Rmin = -1.0;
  time = -1.0e20;

  for (i=0; i<3; i++) {
    X[i] = 0.0;
    V[i] = 0.0;
  }

}

/************************************************************************
* get_pos_vel - get the position and velocity of this object		*
************************************************************************/
void C_SPEEDES_OBJECT::get_pos_vel(double t, double Pos[3], double Vel[3]) {
  int i;

  if (eom != NULL) {
    if (time != t) {
      eom->get_pos_vel(t,X,V);
      for (i=0; i<3; i++) {
        Pos[i] = X[i];
        Vel[i] = V[i];
      }
    }
  }

//...... store the current time for this position

  time = t;

//...... return the position and velocities at this time

  for (i=0; i<3; i++) {
    Pos[i] = X[i];
    Vel[i] = V[i];
  }

}

/************************************************************************
* set_pos_vel - set the position and velocity of this object		*
************************************************************************/
void C_SPEEDES_OBJECT::set_pos_vel(double t, double Pos[3], double Vel[3]) {
  int i;

//...... store the current time for this position

  time = t;

//...... return the position and velocities at this time

  for (i=0; i<3; i++) {
    X[i] = Pos[i];
    V[i] = Vel[i];
  }



}

/************************************************************************
* get_scale - get a scale factor for the current eom			*
************************************************************************/
double C_SPEEDES_OBJECT::get_scale() {

  if (eom == NULL) return 1.0;

  if (eom->get_freeid() == STOP) {
    if (time < eom->get_endtime()) {
      return ( (time - eom->get_start_time())
	/ (eom->get_endtime() - eom->get_start_time()) + 0.050);
    }else{
      if (icon == 90) {
	return 1.0;
      }else{
        return 0.001;
      }
    }
  }else{
    return 1.0;
  }

}

