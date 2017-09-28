// test integrated bus file

#include <stdio.h>
#include <stdlib.h>

#include "gbi.H"
#include "ss18.H"
#include "intss18.H"
#include "int_bus.H"
#include "kepler.H"
#include "random.H"
#include "def.h"

#include "freeobjs.H"
C_FREEOBJS freeobjs;

#define NSORT 100

main() {
  C_INT_BUS int_bus;
  C_KEPLER kepler;
  C_GBI gbi1;
  C_GBI gbi2;
  C_SS18 ss18;
  C_INTSS18 intss18;
  C_RANDOM random;
  int i,j;
  double r[3];
  double v[3];
  double rimp[3];
  double vimp[3];
  double timp;
  double alt;
  double vel;
  double lati, loni;
  double latf, lonf;
  int nits;
  C_BASE_SPACE *ss18_bus;
  C_BASE_SPACE *intss18_bus;
  double tnow;
  double Pimp[3];
  double Vimp[3];
  double latimp, lonimp;
  double d;
  C_ITEM item[NSORT];
  C_QUEUE queue;
  long cpu;
  double time;

//...... test sort

  for (i=0; i<NSORT; i++) {
    item[i].set_id(i);
    item[i].set_time_tag(random.get_random_float());
    queue.push_bot(&item[i]);
  }
  cpu = clock();
//  queue.shell_sort();
  cpu = clock()-cpu;
  time = (cpu/1000000.0) / NSORT;
  printf("Time to shell sort 1 is %f seconds\n",time);

  queue.reset();
  for (i=0; i<NSORT; i++) {
    queue.push_bot(&item[i]);
  }
  cpu = clock();
  queue.sort();
  cpu = clock()-cpu;
  time = (cpu/1000000.0) / NSORT;
  printf("Time to merge sort 1 is %f seconds\n",time);

/*
  queue.print();
*/

//...... test random number generator

  random.set_int_limits(12,201);
  random.his_int();
  random.set_float_limits(112.0, 1201.0);
  random.his_double();

  for (i=0; i<3; i++) {

    random.set_float_limits(1.0,1500.0);
    alt = random.get_random_float();

    random.set_float_limits(0.0,7.0);
    vel = random.get_random_float();

    random.get_random_unit_vector(r);
    random.get_random_unit_vector(v);
    for (j=0; j<3; j++) {
      r[j] *= (RE+alt);
      v[j] *= vel;
    }

    kepler.init(r,v);
    kepler.update_impact();
    timp = kepler.get_timp();
    kepler.get_pos_vel(timp,rimp,vimp);

    fprintf(stderr,"Missile(%d): r0 = (%f %f %f), v0 = (%f %f %f)\n",
	i, r[0], r[1], r[2], v[0], v[1], v[2]);

    fprintf(stderr,"Kepler: timp = %f ", timp);
    fprintf(stderr,"rimp = (%f %f %f) ", rimp[0], rimp[1], rimp[2]);
    fprintf(stderr,"vimp = (%f %f %f)\n", vimp[0], vimp[1], vimp[2]);

    int_bus.init(r,v);
    int_bus.update_impact();
    timp = int_bus.get_timp();
    int_bus.get_pos_vel(timp,rimp,vimp);

    fprintf(stderr,"Intbus: timp = %f ", timp);
    fprintf(stderr,"rimp = (%f %f %f) ", rimp[0], rimp[1], rimp[2]);
    fprintf(stderr,"vimp = (%f %f %f)\n", vimp[0], vimp[1], vimp[2]);

    int_bus.check_spline6();

    fprintf(stderr,"\n");

  }

//...... test aiming code

  lati = 55.0*RADDEG;
  loni = 156.0*RADDEG;
  latf = 34.0*RADDEG;
  lonf = -118.3*RADDEG;

  nits = ss18.aim_latlon(43200.0, lati, loni, latf, lonf);
  printf("ss18 aimed in %d iterations\n",nits);
  ss18.set_ECR();
  ss18_bus = ss18.get_bus();
  timp = ss18_bus->get_timp();
  ss18_bus->get_pos_vel(timp, Pimp, Vimp);
  d = sqrt(Pimp[0]*Pimp[0]+Pimp[1]*Pimp[1]+Pimp[2]*Pimp[2]);

  ss18.xyz_to_latlon(Pimp, latimp, lonimp);
  printf("Impact at time %f, lat %f, lon %f, alt %f\n",
	timp, latimp*DEGRAD, lonimp*DEGRAD, d-RE);


  nits = intss18.aim_latlon(43200.0, lati, loni, latf, lonf);
  printf("integrated ss18 aimed in %d iterations\n",nits);
  intss18.set_ECR();
  intss18_bus = intss18.get_bus();
  timp = intss18_bus->get_timp();
  intss18_bus->get_pos_vel(timp, Pimp, Vimp);
  d = sqrt(Pimp[0]*Pimp[0]+Pimp[1]*Pimp[1]+Pimp[2]*Pimp[2]);

  intss18.xyz_to_latlon(Pimp, latimp, lonimp);
  printf("Impact at time %f, lat %f, lon %f, alt %f\n",
	timp, latimp*DEGRAD, lonimp*DEGRAD, d-RE);

//...... shoot a gbi at the ss18

  ss18.set_ECI();
  tnow = ss18_bus->get_start_time() + 10.0;
  timp = gbi1.aim_gbi(tnow, latf + 0.1, lonf + 0.1, ss18_bus);

  intss18.set_ECI();
  intss18_bus = intss18.get_bus();
  tnow = intss18_bus->get_start_time() + 10.0;
  timp = gbi2.aim_gbi(tnow, latf + 0.1, lonf + 0.1, intss18_bus);

}
