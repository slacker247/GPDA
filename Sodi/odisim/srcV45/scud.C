// scud.C method file

#include <stdio.h>
#include "scud.H"

int C_SCUD::done = 0;
double C_SCUD::Range_Angle[180];
double C_SCUD::Time_Angle[180];

/************************************************************************
* C_SCUD : construct a scud object					*
************************************************************************/
C_SCUD::C_SCUD() {
  double thrust;
  double theta;
  double tburn;
  double tcoast;
  double mrocket;
  double mfuel;
  double mshell;
  C_STAGE *stage;

//...... SCUD is a one stage object

  create_stages(1);

//...... SCUD has no rvs

  create_rvs(0);

//...... initialize the first stage parameters

  thrust = 1.9;
  theta = -1.0;
  tburn = 120.0;
  mrocket = 400.0;
  mfuel = 300.0;
  mshell = 20.0;
  tcoast = 10.0;

  stage = get_stage(0);
  stage->init_pars(thrust, theta, tburn, tcoast, mrocket, mfuel, mshell);
  stage->set_icon(17);

//...... fill up the range and time vs angle array

  range(Range_Angle, Time_Angle, done);

}
