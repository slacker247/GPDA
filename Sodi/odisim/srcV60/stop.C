// stop.C method file

#include <stdio.h>
#include "stop.H"

/************************************************************************
* C_STOP : construct a stop object					*
************************************************************************/
C_STOP::C_STOP() {
  set_icon(90);
}

/************************************************************************
* init : initialize the stop object					*
************************************************************************/
void C_STOP::init(double t, double x[3]) {
  int i;

  for (i=0; i<3; i++) {
    R0[i] = x[i];
    Rend[i] = x[i];
  }

  vel = 0.0;
  t0 = t;
  TT = 100.0;
  Tend = t+TT;
  luminosity = 0.0;
  cross_section = 0.0;

}


