// loiter.C method file

#include <stdio.h>
#include "loiter.H"

/************************************************************************
* C_LOITER : construct a loiter object					*
************************************************************************/
C_LOITER::C_LOITER() {
  set_icon(90);
}

/************************************************************************
* init : initialize the loiter object					*
************************************************************************/
void C_LOITER::init(double t, double x[3], double v) {
  int i;

  for (i=0; i<3; i++) {
    R0[i] = x[i];
    Rend[i] = x[i];
  }

  vel = v;
  t0 = t;
  TT = 31536000.0;
  Tend = t+TT;
  luminosity = 0.0;
  cross_section = 0.0;

}


