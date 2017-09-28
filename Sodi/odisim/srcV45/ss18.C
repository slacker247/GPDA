// ss18.C method file

#include <stdio.h>
#include "ss18.H"

int C_SS18::done = 0;
double C_SS18::Range_Angle[180];
double C_SS18::Time_Angle[180];

/************************************************************************
* C_SS18 : construct a ss18 object					*
************************************************************************/
C_SS18::C_SS18() {
  double thrust;
  double theta;
  double tburn;
  double tcoast;
  double mrocket;
  double mfuel;
  double mshell;
  double ngs_man;
  double nsturns;
  C_STAGE *stage;

//...... SS18 is a three stage object

  create_stages(2);

//...... SS18 has 10 rvs

  create_rvs(10);

//...... initialize the first stage parameters

  mrocket = 1000.0;
  mfuel = 732.2;
  mshell = 67.8;
  thrust = 2.081575;
  theta = -1.0;
  tburn = 107.5;
  tcoast = 10.0;

  stage = get_stage(0);
  stage->init_pars(thrust, theta, tburn, tcoast, mrocket, mfuel, mshell);
  stage->set_icon(2);

//...... initialize the second stage parameters

  mrocket -= mfuel + mshell;
  mfuel = 163.34;
  mshell = 16.66;
  thrust = 1.512956;
  theta = -1.0;
  tcoast = 10.0;
  tburn = 174.5;

  stage = get_stage(1);
  stage->init_pars(thrust, theta, tburn, tcoast, mrocket, mfuel, mshell);
  stage->set_icon(17);

  ngs_man = 1.0;
  nsturns = 3.0;
  stage->init_maneuver(ngs_man, nsturns);

//...... fill up the range and time vs angle array

  range(Range_Angle, Time_Angle, done);

}


