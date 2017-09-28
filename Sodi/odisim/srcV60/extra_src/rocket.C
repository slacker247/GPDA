#include <stdio.h>
#include <math.h>
#include <time.h>

#include "ss18.H"
#include "ss24.H"
#include "ss09.H"
#include "random.H"

#define RE 6378.145
#define RADDEG  0.0174532925199432958

main() {
  long timer;
  double lati,loni,latf,lonf,lat,lon;
  C_SS24 *ss24;
  C_SS18 *ss18;
  C_SS09 *ss09;
  C_RANDOM random_lat;
  C_RANDOM random_lon;
  double t, dt, t0, timp;
  double alt;
  double rt[3], vt[3];
  char *buff;
  int size;
  int status;

  ss24 = new C_SS24();
  ss18 = new C_SS18();
  ss09 = new C_SS09();

  lati = 33.57 * RADDEG;
  loni = 117.23 * RADDEG;
  latf = 38.55 * RADDEG;
  lonf = 104.5 * RADDEG;

  timer = clock();
  ss09->aim_latlon(3600.0,lati,loni,latf,lonf);
  ss18->aim_latlon(3600.0,lati,loni,latf,lonf);
  ss24->aim_latlon(3600.0,lati,loni,latf,lonf);
  timer = clock() - timer;

/*
  ss09->init_t0(3600.0);
  ss18->init_t0(7200.0);
  ss24->init_t0(10800.0);
*/

  fprintf(stderr,"Time to aim = %f\n",timer/1000000.0);

//...... print out the position of the ss24
//  ss24->init_t0(0.0);
  t0 = ss24->get_t0();
  timp = ss24->get_timp();
  dt = (timp-t0) / 10.0;

  for (t=t0; t<=timp; t+=dt) {
    ss24->get_pos_vel(t,rt,vt);
    alt = sqrt(rt[0]*rt[0] + rt[1]*rt[1] + rt[2]*rt[2]) - 6378.145;
    printf("t = %7.1f, alt = %7.1f, x = %7.1f, y = %7.1f, z = %7.1f\n",
	t,alt,rt[0],rt[1],rt[2]);
  }

//...... check that it did hit the proper lat and lon

  ss24->set_ECR();
  ss24->get_pos_vel(timp,rt,vt);
  ss24->xyz_to_latlon(rt,lat,lon);
//  ss24->xyz_to_latlon(timp,rt,lat,lon);
  printf("delta lat = %f, delta lon = %f\n",
	lat-latf, lon-lonf);

//...... test that the buffer can be saved and loaded back

  buff = ss24->make_buff(size);
  ss24 = new C_SS24();
  ss24->init_missile(buff);

  for (t=t0; t<=timp; t+=dt) {
    ss24->get_pos_vel(t,rt,vt);
    alt = sqrt(rt[0]*rt[0] + rt[1]*rt[1] + rt[2]*rt[2]) - 6378.145;
    printf("t = %7.1f, alt = %7.1f, x = %7.1f, y = %7.1f, z = %7.1f\n",
	t,alt,rt[0],rt[1],rt[2]);
  }

//...... test random launches

/*
  random_lat.set_seed(123456);
  random_lon.set_seed(123457);
  random_lat.set_float_limits(-90.0*RADDEG, 90.0*RADDEG);
  random_lon.set_float_limits(-180.0*RADDEG, 180.0*RADDEG);

  while (1) {
    lati = random_lat.get_random_float();
    loni = random_lon.get_random_float();
    while (1) {
      latf = random_lat.get_random_float();
      lonf = random_lon.get_random_float();
      status = ss24->aim_latlon(3600.0,lati,loni,latf,lonf);
      if (status > 0) {
        printf("Good aim... %d\n",status);
	break;
      }
    }
  }
*/

}
