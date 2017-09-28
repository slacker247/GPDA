// ss24.C method file

#include <stdio.h>
#include "ss24.H"

int C_SS24::done = 0;
double C_SS24::Range_Angle[180];
double C_SS24::Time_Angle[180];

/************************************************************************
* C_SS24 : construct a ss24 object					*
************************************************************************/
C_SS24::C_SS24() {
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

//...... SS24 is a three stage object

  create_stages(3);

//...... SS24 has 3 rvs

  create_rvs(3);

//...... initialize the first stage parameters

  thrust = 1.5;
  theta = 45;
  tburn = 200.0;
  tcoast = 10.0;
  mrocket = 500.0;
  mfuel = 200.0;
  mshell = 20.0;

  stage = get_stage(0);
  stage->init_pars(thrust, theta, tburn, tcoast, mrocket, mfuel, mshell);
  stage->set_icon(2);

//...... initialize the second stage parameters

//  thrust = 2.0;
  thrust = 2.0;
  theta = -1.0;
  tburn = 150.0;
  tcoast = 10.0;
  mrocket -= mfuel + mshell;
  mfuel = 150.0;
  mshell = 15.0;

  stage = get_stage(1);
  stage->init_pars(thrust, theta, tburn, tcoast, mrocket, mfuel, mshell);
  stage->set_icon(2);

  ngs_man = 1.0;
  nsturns = 5.0;
  stage->init_maneuver(ngs_man, nsturns);

//...... initialize the third stage parameters

//  thrust = 4.0;
  thrust = 2.0;
  theta = -1.0;
  tburn = 50.0;
  tcoast = 0.0;
  mrocket -= mfuel + mshell;
  mfuel = 50.0;
  mshell = 5.0;

  stage = get_stage(2);
  stage->init_pars(thrust, theta, tburn, tcoast, mrocket, mfuel, mshell);
  stage->set_icon(17);

  ngs_man = 1.0;
  nsturns = 1.0;
  stage->init_maneuver(ngs_man, nsturns);

//...... fill up the range and time vs angle array

  range(Range_Angle, Time_Angle, done);

}


