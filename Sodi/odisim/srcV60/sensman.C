// sensman.C method file

#include <stdio.h>
#include <memory.h>
#include <math.h>
#include "sensman.H"
#include "fixed_coverage.H"
#include "def.h"
#include "defunc.H"
#include "ground_radar.H"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

extern int EXT_GRAPHICS_DEFINE;
extern int SENSOR;
extern int TEST_PROX;
extern int FIXED_COVERAGE;

/************************************************************************
* C_SENSMAN : construct a sensman object				*
************************************************************************/
C_SENSMAN::C_SENSMAN() {
  int i;
  int offset;
  int on_off;
  int index;
  int n_random, n_names;
  double R[3];
  double latitude, longitude;
  double lat_deg, lat_min, lat_sec;
  double lon_deg, lon_min, lon_sec;
  double d;
  double VMAX;
  //int error_flag;
  char *random_sensor_type;
  char *random_name;
  //char *random_output;
  C_BASETYPE *basetype;
  C_BASETYPE *parameters;
  C_BASETYPE *sensor_names;
  C_BASETYPE *sensor_name;
  C_BASETYPE *sensor_type;
  C_GROUND_RADAR *ground_radar;

//...... get stuff out of the standard parser first

  basetype = parameters_parser->get_basetype("parameters");
  //error_flag = basetype->get_logical("error_flag");
  on_off = basetype->get_logical("on_off");
  VMAX = basetype->get_float("VMAX");
  test_prox = basetype->get_logical("test_prox");
  test_prox_time = basetype->get_float("test_prox_time");

//...... build the sensor object parser


  parameters = ground_sensor_parser->get_basetype("parameters");
  n_random = parameters->get_int("n_random");
  random_sensor_type = parameters->get_string("random_sensor_type");
  //random_output = parameters->get_string("random_output");
  sensor_com_delay = parameters->get_float("sensor_com_delay");
  com_rate = parameters->get_float("com_rate");

  sensor_names = ground_sensor_parser->get_basetype("sensor_names");
  n_names = sensor_names->get_ntypes();

  N_TOT = n_random + n_names;

//...... construct the sensor simulation objects


  N_LOC = deal_me(N_TOT, offset);
  sensobj = new C_SENSOBJ[N_LOC];
  ground_radar = new C_GROUND_RADAR[N_LOC];

  if (offset == 0) {
    start_uid = sensobj[0].get_GLOBAL_ID();
  }else{
    start_uid = 0;
  }
  start_uid = SpComm_GlobalSum(start_uid);
  //scombine(&start_uid,SUMINT,sizeof(int),1);

  sensobj[0].set_VMAX(VMAX);

//...... initialize the named sensors first;

  index = 0;
  sensor_name = sensor_names->get_first_type();
  for (i=0; i<n_names; i++) {

    set_communications(sensor_name, i, offset, start_uid);

    if ((i-offset+N_NODES)%N_NODES == 0) {

      check_communications(sensobj[index].get_GLOBAL_ID(), index);

      sensor_type = sensor_name->get_first_type();
      ground_radar[index].init(sensor_type);
      sensobj[index].set_sensor_model(&ground_radar[index]);

      lat_deg = sensor_name->get_float("lat_deg");
      lat_min = sensor_name->get_float("lat_min");
      lat_sec = sensor_name->get_float("lat_sec");

      lon_deg = sensor_name->get_float("lon_deg");
      lon_min = sensor_name->get_float("lon_min");
      lon_sec = sensor_name->get_float("lon_sec");

      latitude  = (lat_deg + lat_min/60.0 + lat_sec/3600.0) * 0.017453292;
      longitude = (lon_deg + lon_min/60.0 + lon_sec/3600.0) * 0.017453292;

      if (!sensor_name->get_logical("north")) latitude *= -1.0;
      if (!sensor_name->get_logical("east")) longitude *= -1.0;

      sensobj[index].latlon_to_xyz(latitude,longitude,R);

      d = sqrt(R[0]*R[0]+R[1]*R[1]+R[2]*R[2])/RE;
      if ((d < 0.99) || (d > 1.01)) {
	fprintf(stderr,"Error, bad initial position for sensor\n");
      }

      sensobj[index].set_start_position(R);
      sensobj[index].set_on_off(on_off);
      sensobj[index].set_com_rate(com_rate);
      sensobj[index].set_NAME(sensor_name->get_name());

      index++;

    }

    sensor_name = sensor_names->get_next_type();
  }

//...... initialize the remaining random sensors

  sensor_type = ground_sensor_parser->get_basetype(random_sensor_type);

  random_name = new char[20];
  strcpy(random_name,"RANDOM SENSOR");

  while (index < N_LOC) {

    ground_radar[index].init(sensor_type);
    sensobj[index].set_sensor_model(&ground_radar[index]);

    sensobj[index].random_position(R);
    sensobj[index].set_start_position(R);
    sensobj[index].set_on_off(on_off);
    sensobj[index].set_NAME(random_name);

    d = sqrt(R[0]*R[0]+R[1]*R[1]+R[2]*R[2])/RE;
    if ((d < 0.99) || (d > 1.01)) {
      fprintf(stderr,"Error, bad initial position for sensor\n");
    }

    index++;

  }

  TOTAL_OBJECTS += N_TOT;
  reserve_n_objects(N_LOC);
  for (i=0; i<N_LOC; i++) define_object(&sensobj[i]);
  set_interact();

  printf("SENSMAN object created with %4d objects (%4d loc)\n", N_TOT, N_LOC);

}

/************************************************************************
* init_events : initialize events for the grid element objects		*
************************************************************************/
void C_SENSMAN::init_events() {
  int i;
  int temp;
  double tt;
  int FIXED_COVERAGE;
  int EXT_GRAPHICS_DEFINE;
  int TEST_PROX;

  printf("SENSMAN initializing\n");

  FIXED_COVERAGE = event_type("FIXED_COVERAGE");
  EXT_GRAPHICS_DEFINE = event_type("EXT_GRAPHICS_DEFINE");
  TEST_PROX = event_type("TEST_PROX");

//...... schedule the fixed coverage initial event

  for (i=0; i<N_LOC; i++) {

    temp = sensobj[i].get_GLOBAL_ID() - start_uid;
    tt = 10.0 * double(temp) / double(N_TOT);
    temp = int(tt);
    tt = -(100.0*temp) - 100.0;

    schedule (tt, FIXED_COVERAGE, SENSOR, i, NODE);
  }

//...... start up the test prox event

  if ((N_NODES == 1) && (test_prox)) {
    for (i=0; i<N_LOC; i++) {
      schedule (1000.0, TEST_PROX, SENSOR, i, NODE);
    }
  }

//...... start up the graphics "define" event

  for (i=0; i<N_LOC; i++) {
    schedule (0.0, EXT_GRAPHICS_DEFINE, SENSOR, i, NODE);
  }

}
