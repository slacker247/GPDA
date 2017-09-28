// intgbi.C method file

#include <stdio.h>
#include "intgbi.H"

int C_INTGBI::done = 0;
C_QUEUE **C_INTGBI::Down_Range = 0;
double C_INTGBI::Range_Angle[180];
double C_INTGBI::Time_Angle[180];

/************************************************************************
* C_INTGBI : construct a intgbi object					*
************************************************************************/
C_INTGBI::C_INTGBI() {
  double thrust;
  double theta;
  double tburn;
  double tcoast;
  double mrocket;
  double mfuel;
  double mshell;
  C_INT_STAGE *stage;
  C_INT_BUS *bus;

//...... INTGBI is a one stage object

  create_int_stages(1);

//...... INTGBI has no rvs

  create_rvs(0);

//...... initialize the first stage parameters

  thrust = 4.0;
  theta = -1.0;
  tburn = 90.0;
  mrocket = 110.0;
  mfuel = 90.0;
  mshell = 10.0;
  tcoast = 10.0;

  stage = get_int_stage(0);
  stage->init_pars(thrust, theta, tburn, tcoast, mrocket, mfuel, mshell);
  stage->set_icon(150);

//...... set the icon for the bus

  bus = (C_INT_BUS *)get_bus();
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
