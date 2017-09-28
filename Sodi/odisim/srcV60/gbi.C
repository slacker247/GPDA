// gbi.C method file

#include <stdio.h>
#include "gbi.H"

int C_GBI::done = 0;
C_QUEUE **C_GBI::Down_Range = 0;
double C_GBI::Range_Angle[180];
double C_GBI::Time_Angle[180];

/************************************************************************
* C_GBI : construct a gbi object					*
************************************************************************/
C_GBI::C_GBI() {
  double thrust;
  double theta;
  double tburn;
  double tcoast;
  double mrocket;
  double mfuel;
  double mshell;
  C_STAGE *stage;
  C_BUS *bus;

//...... GBI is a one stage object

  create_stages(1);

//...... GBI has no rvs

  create_rvs(0);

//...... initialize the first stage parameters

  thrust = 4.0;
  theta = -1.0;
  tburn = 90.0;
  mrocket = 110.0;
  mfuel = 90.0;
  mshell = 10.0;
  tcoast = 10.0;

  stage = get_stage(0);
  stage->init_pars(thrust, theta, tburn, tcoast, mrocket, mfuel, mshell);
  stage->set_icon(150);

//...... set the icon for the bus

  bus = (C_BUS *)get_bus();
  bus->set_icon(151);

//...... fill up the range and time vs angle array

  if (!done) {
    range(Range_Angle, Time_Angle, done);
    down_range(Down_Range,Nr,Na,Malt,RESOLUTION);
  }else{
    range(Range_Angle, Time_Angle, done);
    init_GBI(Down_Range,Nr,Na,Malt,RESOLUTION);
  }

}
