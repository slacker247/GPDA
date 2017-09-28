// missman.C method file

#include <stdio.h>
#include <memory.h>
#include <math.h>
#include "missman.H"
#include "def.h"
#include "defunc.H"

int PRINT_TRAJECTORY = 1;

/************************************************************************
* C_MISSMAN : construct a missman object				*
************************************************************************/
C_MISSMAN::C_MISSMAN() {
  int i;
  C_BASETYPE *basetype;
  C_BASETYPE *missiles;
  C_BASETYPE *missile_name;
  C_BASETYPE *base_position;
  char *position;
  char *nm;
  char *ty;
  int offset;
  int index;
  int sd;
  double X[3];
  double Xran[3];
  double temp;
  double lati, loni;
  double latf, lonf;
  double lat_deg, lat_min, lat_sec;
  double lon_deg, lon_min, lon_sec;
  double launch_time;
  double vmax;
  double random_distance;

  MISSILE = object_type("MISSILE");

  basetype = parameters_parser->get_basetype("parameters");
  vmax = basetype->get_float("VMAX");
  test_prox = basetype->get_logical("test_prox");

  missiles = missile_parser->get_basetype("missiles");
  N_TOT = missiles->get_ntypes();

  first_id = TOTAL_OBJECTS;
  N_LOC = deal_me(N_TOT, offset);
  missobj = new C_MISSOBJ[N_LOC];
  missobj[0].set_VMAX(vmax);

//...... initialize the missiles

  index = 0;
  missile_name = missiles->get_first_type();
  for (i=0; i<N_TOT; i++) {

    if ((i-offset+N_NODES)%N_NODES == 0) {

      nm = missile_name->get_name();
      ty = missile_name->get_string("missile_type");
      missobj[index].init_missobj(nm, ty);

      launch_time = missile_name->get_float("launch_time");

//...... get the launch point

      position = missile_name->get_string("init_position");
      base_position = missile_parser->get_basetype(position);
      lat_deg = base_position->get_float("lat_deg");
      lat_min = base_position->get_float("lat_min");
      lat_sec = base_position->get_float("lat_sec");
      lon_deg = base_position->get_float("lon_deg");
      lon_min = base_position->get_float("lon_min");
      lon_sec = base_position->get_float("lon_sec");
      lati = (lat_deg + lat_min/60.0 + lat_sec/3600.0) * 0.017453292;
      loni = (lon_deg + lon_min/60.0 + lon_sec/3600.0) * 0.017453292;
      if (!base_position->get_logical("north")) lati *= -1.0;
      if (!base_position->get_logical("east")) loni *= -1.0;

//...... get the impact point

      position = missile_name->get_string("final_position");
      base_position = missile_parser->get_basetype(position);
      lat_deg = base_position->get_float("lat_deg");
      lat_min = base_position->get_float("lat_min");
      lat_sec = base_position->get_float("lat_sec");
      lon_deg = base_position->get_float("lon_deg");
      lon_min = base_position->get_float("lon_min");
      lon_sec = base_position->get_float("lon_sec");
      latf = (lat_deg + lat_min/60.0 + lat_sec/3600.0) * 0.017453292;
      lonf = (lon_deg + lon_min/60.0 + lon_sec/3600.0) * 0.017453292;
      if (!base_position->get_logical("north")) latf *= -1.0;
      if (!base_position->get_logical("east")) lonf *= -1.0;

//...... randomize the impact point

      random_distance = missile_name->get_float("random_distance");
      sd = missobj[index].get_SEED();
      RANDOM->set_seed(sd);
      RANDOM->get_random_mag_vector(random_distance,Xran);
      missobj[index].latlon_to_xyz(latf,lonf,X);
      X[0] += Xran[0];
      X[1] += Xran[1];
      X[2] += Xran[2];
      temp = RE / sqrt(X[0]*X[0]+X[1]*X[1]+X[2]*X[2]);
      X[0] *= temp;
      X[1] *= temp;
      X[2] *= temp;
      missobj[index].xyz_to_latlon(X,latf,lonf);
      sd = RANDOM->get_seed();
      missobj[index].set_SEED(sd);

      if (PRINT_TRAJECTORY) {
         fprintf(stderr, "---------------------------------------------------------\n");
         fprintf(stderr, "Missile name: %s\n", nm);
         fprintf(stderr, "Missile type: %s\n", ty);
         fprintf(stderr, "Launch Time:  %f\n", launch_time);
         fprintf(stderr, "Launch Point: %f %f\n", lati*180.0/M_PI, loni*180.0/M_PI);
         fprintf(stderr, "Impact Point: %f %f\n", latf*180.0/M_PI, lonf*180.0/M_PI);
      }

//...... aim the missile

      missobj[index].aim(launch_time,lati,loni,latf,lonf);
      missobj[index].make_script();

      if (PRINT_TRAJECTORY) {
         missobj[index].print_trajectory(100);
         missobj[index].test_trajectory();
         fprintf(stderr, "---------------------------------------------------------\n");
      }

      index++;

    }

    missile_name = missiles->get_next_type();

  }

//...... tell SPEEDES about the missile objects

  TOTAL_OBJECTS += N_TOT;
  reserve_n_objects(N_LOC);
  for (i=0; i<N_LOC; i++) define_object(&missobj[i]);
  set_interact();

  printf("MISSMAN %4d objects created (%d tot)\n",
	N_LOC, N_TOT);

}

/************************************************************************
* init_events : initialize events for the grid element objects		*
************************************************************************/
void C_MISSMAN::init_events() {
  int i;
  //double tim;
  C_EOM *e;
  int NEXT_SCRIPT;
  int UPDATE_GRID;
  int MOVING_COVERAGE;
  int TEST_PROX;
  int EXT_GRAPHICS_SCRIPT;

  printf("MISSMAN initializing ...\n");

  NEXT_SCRIPT	  = event_type("NEXT_SCRIPT");
  UPDATE_GRID	  = event_type("UPDATE_GRID");
  MOVING_COVERAGE = event_type("MOVING_COVERAGE");
  TEST_PROX = event_type("TEST_PROX");
  EXT_GRAPHICS_SCRIPT = event_type("EXT_GRAPHICS_SCRIPT");

  for (i=0; i<N_LOC; i++) {
    e = missobj[i].get_current_segment();
    if (e != NULL) {
      //tim = missobj[i].get_start_time();

      schedule (0.0, EXT_GRAPHICS_SCRIPT, MISSILE, i, NODE);

      schedule (e->get_endtime(), NEXT_SCRIPT, MISSILE, i, NODE);

//      schedule (e->get_start_time()-600.0, UPDATE_GRID, MISSILE, i, NODE);
      schedule (e->get_start_time()-0.0, UPDATE_GRID, MISSILE, i, NODE);

      if (missobj[i].get_sensor_model() != NULL) {
        schedule (GONE * (e->get_start_time()), MOVING_COVERAGE,
				MISSILE, i, NODE);

        if ((N_NODES == 1) && (test_prox)) {
          schedule (1000.0, TEST_PROX, MISSILE, i, NODE);
        }

      }

    }
  }

}
