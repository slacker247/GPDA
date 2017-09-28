// ss09.C method file

#include <stdio.h>
#include "ss09.H"

int C_SS09::done = 0;
double C_SS09::Range_Angle[180];
double C_SS09::Time_Angle[180];

/************************************************************************
* C_SS09 : construct a ss09 object					*
************************************************************************/
C_SS09::C_SS09() {
  double thrust;
  double theta;
  double tburn;
  double tcoast;
  double mrocket;
  double mfuel;
  double mshell;
  C_STAGE *stage;

//...... SS09 is a one stage object

  create_stages(1);

//...... SS09 has no rvs

  create_rvs(0);

//...... initialize the first stage parameters

  thrust = 2.1;
  theta = -1.0;
  tburn = 150.0;
  mrocket = 400.0;
  mfuel = 200.0;
  mshell = 20.0;
  tcoast = 10.0;

  stage = get_stage(0);
  stage->init_pars(thrust, theta, tburn, tcoast, mrocket, mfuel, mshell);
  stage->set_icon(17);

//...... fill up the range and time vs angle array

  range(Range_Angle, Time_Angle, done);

}
