#include <stdio.h>
#include <math.h>
#include "gbi.H"
#include "ss24.H"

#define RADDEG  0.0174532925199432958
#define RE      6378.145  

main() {
  C_SS24 *ss24;
  C_GBI *gbi;
  double r0[3], r1[3];
  double time;
  double altitude;
  int nits;
  double latgbi;
  double longbi;
  C_EOM *eom;
  double launch_time;
  double kill_time;
  double current_time;

  ss24 = new C_SS24();
  nits = ss24->aim_latlon(
	43200.0,
	RADDEG*55.0,
	RADDEG*156.0,
	RADDEG*38.55,
	-RADDEG*104.5
	);

  printf("N iterations for aiming = %d\n",nits);

  gbi = new C_GBI();

  time = 10000.0;
  altitude = 200.0;

  r0[0] = 0.0;
  r0[1] = 0.0;
  r0[2] = RE;

  r1[0] = (altitude+RE)*0.0;
  r1[1] = (altitude+RE)*cos(60.0*RADDEG);
  r1[2] = (altitude+RE)*cos(30.0*RADDEG);

  latgbi =  55.0*RADDEG;
  longbi = -80.0*RADDEG;

  eom = (C_BUS *)ss24->get_bus();
  current_time = eom->get_start_time();
  kill_time = gbi->aim_gbi(current_time, latgbi ,longbi, eom);
  launch_time = gbi->get_start_time();

  printf("Launch at %f, and intercept at %f\n",launch_time, kill_time);

//...... for fun, create another GBI

  printf("\n\nstarting over...\n\n");

  gbi = new C_GBI();
  kill_time = gbi->aim_gbi(current_time, latgbi ,longbi, eom);
  launch_time = gbi->get_start_time();
  printf("Launch at %f, and intercept at %f\n",launch_time, kill_time);


  eom = (C_EOM *)gbi->get_stage(0);
  printf("Stage 0, start %f, end %f\n",
	eom->get_start_time(),eom->get_endtime());

  eom = (C_EOM *)gbi->get_bus();
  printf("Stage 1, start %f, end %f\n",
	eom->get_start_time(),eom->get_endtime());

}
