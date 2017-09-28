// dspman.C method file

#include <stdio.h>
#include <memory.h>
#include <math.h>
#include "dspman.H"
#include "fixed_coverage.H"
#include "dsp_model.H"
#include "eye_model.H"
#include "def.h"
#include "defunc.H"

#define RDSP 42164.2
#define RFACTOR (42164.2) / RE

/************************************************************************
* C_DSPMAN : construct a dspman object					*
************************************************************************/
C_DSPMAN::C_DSPMAN() {
  int i;
  int offset;
  int on_off;
  int index;
  int n_names;
  double R[3];
  double latitude, longitude;
  double d;
  double theta1, theta2, theta;
  double rmax;
  double rmaxcoverage;
  double VMAX;
  //int error_flag;
  double maxalt;
  C_BASETYPE *basetype;
  C_BASETYPE *parameters;
  C_BASETYPE *dsp_names;
  C_BASETYPE *dsp_name;
  C_BASETYPE *dsp_type;
  C_EYE_MODEL *dsp_model;

//...... get stuff out of the standard parser first

  basetype = parameters_parser->get_basetype("parameters");
  //error_flag = basetype->get_logical("error_flag");
  on_off = basetype->get_logical("on_off");
  VMAX = basetype->get_float("VMAX");
  test_prox = basetype->get_logical("test_prox");
  test_prox_time = basetype->get_float("test_prox_time");
  maxalt = basetype->get_float("maxalt");

//...... build the dsp object from the parser

  DSP = object_type("DSP");

  parameters = dsp_parser->get_basetype("parameters");
  if (on_off) on_off = parameters->get_logical("on_off");

  dsp_names = dsp_parser->get_basetype("dsp_names");
  n_names = dsp_names->get_ntypes();

  N_TOT = n_names;

//...... construct the sensor simulation objects


  N_LOC = deal_me(N_TOT, offset);
  dspobj = new C_DSPOBJ[N_LOC];
  dsp_model = new C_EYE_MODEL[N_LOC];

  if (offset == 0) {
    start_uid = dspobj[0].get_GLOBAL_ID();
  }else{
    start_uid = -1;
  }
  start_uid = SpComm_GlobalMax(start_uid);
  //scombine(&start_uid,SP_MAXINT,sizeof(int),1);

  dspobj[0].set_VMAX(VMAX);

//...... initialize the dsp sensors;

  theta1 = acos(RE/RDSP);
  theta2 = acos(RE/(RE+maxalt));
  theta = theta1+theta2;
  rmaxcoverage = RE*theta;

  rmax = sqrt(RDSP*RDSP - RE2) + sqrt((RE+maxalt)*(RE+maxalt) - RE2);

//...... initialize the dsp sensors;

  index = 0;
  latitude = 0.0;

  dsp_name = dsp_names->get_first_type();

  for (i=0; i<n_names; i++) {

    set_communications(dsp_name, i, offset, start_uid);

    if ((i-offset+N_NODES)%N_NODES == 0) {

      check_communications(dspobj[index].get_GLOBAL_ID(), index);

      dsp_type = dsp_name->get_first_type();
      dsp_model[index].init(dsp_type);
      dsp_model[index].set_NOISE_DSP();
      dsp_model[index].set_sensor_id(
		dspobj[index].get_GLOBAL_ID());

      dsp_model[index].set_rmax(rmax);
      dsp_model[index].set_rmaxcoverage(rmaxcoverage);

      dspobj[index].set_sensor_model(&dsp_model[index]);
      dspobj[index].set_n_per_ring(n_names);
      dspobj[index].set_ring(0);
      dspobj[index].set_id_in_ring(i);

      longitude = dsp_name->get_float("longitude") * 0.017453292;
      dspobj[index].latlon_to_xyz(latitude,longitude,R);

      d = sqrt(R[0]*R[0]+R[1]*R[1]+R[2]*R[2])/RE;
      if ((d < 0.99) || (d > 1.01)) {
	fprintf(stderr,"Error, bad initial position for sensor\n");
      }

      R[0] *= RFACTOR;
      R[1] *= RFACTOR;
      R[2] *= RFACTOR;

      dspobj[index].set_start_position(R);
      dspobj[index].set_on_off(on_off);
      dspobj[index].set_NAME(dsp_name->get_name());

      index++;

    }

    dsp_name = dsp_names->get_next_type();
  }

  TOTAL_OBJECTS += N_TOT;
  reserve_n_objects(N_LOC);
  for (i=0; i<N_LOC; i++) define_object(&dspobj[i]);
  set_interact();

  printf("DSPMAN object created with %4d objects (%4d loc)\n", N_TOT, N_LOC);

}

/************************************************************************
* init_events : initialize events for the grid element objects		*
************************************************************************/
void C_DSPMAN::init_events() {
  int i;
  int temp;
  double tt;
  int FIXED_COVERAGE;
  int EXT_GRAPHICS_DEFINE;
  int TEST_PROX;
  int scan_type;

  printf("DSPMAN initializing\n");

  FIXED_COVERAGE = event_type("FIXED_COVERAGE");
  EXT_GRAPHICS_DEFINE = event_type("EXT_GRAPHICS_DEFINE");
  TEST_PROX = event_type("TEST_PROX");

  scan_type = event_type("SCAN_EYE");

//...... schedule the fixed coverage initial event

  for (i=0; i<N_LOC; i++) {

    dspobj[i].set_scan_type(scan_type);

    temp = dspobj[i].get_GLOBAL_ID() - start_uid;
    tt = 10.0 * double(temp) / double(N_TOT);
    temp = int(tt);
    tt = -(100.0*temp) - 100.0;

    schedule (tt, FIXED_COVERAGE, DSP, i, NODE);
  }

//...... start up the test prox event

  if ((N_NODES == 1) && (test_prox)) {
    for (i=0; i<N_LOC; i++) {
      schedule (1000.0, TEST_PROX, DSP, i, NODE);
    }
  }

//...... start up the graphics "define" event

  for (i=0; i<N_LOC; i++) {
    schedule (0.0, EXT_GRAPHICS_DEFINE, DSP, i, NODE);
  }

}

