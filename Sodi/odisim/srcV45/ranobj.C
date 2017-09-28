// ranobj.C method file

#include <stdio.h>
#include <math.h>
#include "def.h"
#include "ranobj.H"

/************************************************************************
* C_RANOBJ : construct a ranobj object					*
************************************************************************/
C_RANOBJ::C_RANOBJ() {

  rancourse = 1;
  fixed_script = 0;
  luminosity = 0.0;

}

/************************************************************************
* init_rancraft : initialize the ranobj object				*
************************************************************************/
void C_RANOBJ::init_ranobj(C_BASETYPE *rantype) {

//  printf("rantype %s created\n",rantype->get_name());

  NAME = rantype->get_name();

  icon = rantype->get_int("icon");
  max_vel = rantype->get_float("max_vel");
  cruise_vel = rantype->get_float("cruise_vel");
  max_alt = rantype->get_float("max_alt");
  flyout_range = rantype->get_float("flyout_range");
  cross_section = rantype->get_float("cross_section");

  random_init();

  fixed_script = 1;
  rancourse = 0;

}

