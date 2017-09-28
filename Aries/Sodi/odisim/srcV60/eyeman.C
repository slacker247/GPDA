// eyeman.C method file

#include <stdio.h>
#include <memory.h>
#include <math.h>
#include "eyeman.H"
#include "fixed_coverage.H"
#include "eye_model.H"
#include "def.h"
#include "defunc.H"

extern int MOVING_COVERAGE;
extern int EYE;
extern int TEST_PROX;
extern int EXT_GRAPHICS_SCRIPT;

/************************************************************************
* C_EYEMAN : construct a eyeman object					*
************************************************************************/
C_EYEMAN::C_EYEMAN() {
  int i,j,k;
  int offset;
  int on_off;
  int index;
  double theta1, theta2, theta;
  //double rmax;
  //double rmaxcoverage;
  double VMAX;
  //int error_flag;
  double maxalt;
  C_BASETYPE *basetype;
  C_BASETYPE *sensor_type;
  C_BASETYPE *constellation;
  C_EYE_MODEL *eye_model;
  double ivec[3], evec[3], vec[3];
  double X[3], V[3];
  double cp, sp;
  int local_index;
  double delta_theta;
  double temp;
  double delta_phi;
  double phi;
  char s[120];
  char *name;

//...... get stuff out of the standard parser first

  basetype = parameters_parser->get_basetype("parameters");
  //error_flag = basetype->get_logical("error_flag");
  on_off = basetype->get_logical("on_off");
  VMAX = basetype->get_float("VMAX");
  test_prox = basetype->get_logical("test_prox");
  test_prox_time = basetype->get_float("test_prox_time");
  maxalt = basetype->get_float("maxalt");

//...... build the eye object from the parser


  constellation = eye_parser->get_basetype("constellation");
  icon = constellation->get_int("icon");
  n_rings = constellation->get_int("n_rings");
  n_per_ring = constellation->get_int("n_per_ring");
  inclination = constellation->get_float("inclination") * RADDEG;
  altitude = constellation->get_float("altitude");
  time = constellation->get_float("time");
  turn_on_time = constellation->get_float("turn_on_time");
  if (on_off) on_off = constellation->get_logical("on_off");
  stagger = constellation->get_logical("stagger");

//...... get the sensor base type

  sensor_type = eye_parser->get_basetype("BRILLIANT_EYE_SENSOR");

  REYE = altitude + RE;
  RFACTOR = REYE / RE;
  velocity = sqrt(GM / REYE);
  N_TOT = n_rings*n_per_ring;

//  fprintf(stderr,"Velocity of B.E. = %f\n",velocity);

//...... construct the sensor simulation objects


  N_LOC = deal_me(N_TOT, offset);
  eyeobj = new C_EYEOBJ[N_LOC];

//...... for now, put in a stupid radar

  eye_model = new C_EYE_MODEL[N_LOC];

//...... make sure that we have the same starting unique id

  if (offset == 0) {
    start_uid = eyeobj[0].get_GLOBAL_ID();
  }else{
    start_uid = -1;
  }
  start_uid = SpComm_GlobalMax(start_uid);
  //scombine(&start_uid,SP_MAXINT,sizeof(int),1);
//  fprintf(stderr,"EYE> starting unique id = %d\n",start_uid);

  eyeobj[0].set_VMAX(VMAX);

//...... initialize the eye sensors;

  theta1 = acos(RE/REYE);
  theta2 = acos(RE/(RE+maxalt));
  theta = theta1+theta2;
  //rmaxcoverage = RE*theta;

  //rmax = sqrt(REYE*REYE - RE2) + sqrt((RE+maxalt)*(RE+maxalt) - RE2);

//...... initialize the eye sensors;

  index = 0;
  local_index = 0;
  delta_theta = TWOPI/n_rings;
  delta_phi = TWOPI/n_per_ring;
  theta = 0.0;

  for (i=0; i<n_rings; i++) {

    evec[0] = cos(theta);
    evec[1] = sin(theta);
    evec[2] = 0.0;

    ivec[2] = cos(inclination);
    temp = (1.0-(ivec[2]*ivec[2])) / (1.0+(evec[1]*evec[1])/(evec[0]*evec[0]));
    ivec[1] = sqrt(temp);
    ivec[0] = - ivec[1] * evec[1] / evec[0];

    vec[0] = evec[1]*ivec[2] - evec[2]*ivec[1];
    vec[1] = evec[2]*ivec[0] - evec[0]*ivec[2];
    vec[2] = evec[0]*ivec[1] - evec[1]*ivec[0];
/*
    temp = evec[0]*ivec[0]+evec[1]*ivec[1]+evec[2]*ivec[2];
    fprintf(stderr,"check vector (should be 0) %f\n",temp);

    temp = ivec[0]*ivec[0]+ivec[1]*ivec[1]+ivec[2]*ivec[2];
    fprintf(stderr,"check vector (should be 1) %f\n",temp);

    temp = vec[0]*vec[0]+vec[1]*vec[1]+vec[2]*vec[2];
    fprintf(stderr,"check vector (should be 1) %f\n",temp);
*/
    phi = 0.0;
    for (j=0; j<n_per_ring; j++) {

//    sprintf(s,"BRILLIANT_EYE%4.4d\n",index);
      sprintf(s,"BRILLIANT_EYE_%3.3d_%3.3d",i,j);
      name = strdup(s);
      set_communications(name, index, offset, start_uid);

      if ((index-offset+N_NODES)%N_NODES == 0) {

        check_communications(eyeobj[local_index].get_GLOBAL_ID(), local_index);

	cp = cos(phi);
	sp = sin(phi);
	X[0] = cp*evec[0] + sp*ivec[0];
	X[1] = cp*evec[1] + sp*ivec[1];
	X[2] = cp*evec[2] + sp*ivec[2];
/*
        temp = X[0]*X[0]+X[1]*X[1]+X[2]*X[2];
        fprintf(stderr,"check vector (should be 1) %f\n",temp);
*/
	V[0] = vec[1]*X[2] - vec[2]*X[1];
	V[1] = vec[2]*X[0] - vec[0]*X[2];
	V[2] = vec[0]*X[1] - vec[1]*X[0];
/*
        temp = V[0]*V[0]+V[1]*V[1]+V[2]*V[2];
        fprintf(stderr,"check vector (should be 1) %f\n",temp);
*/

        for(k=0; k<3; k++) {
	  X[k] *= REYE;
	  V[k] *= velocity;
        }

//...... initialize the local eye objects

//	eyeobj[local_index].init_eom(time,X,V);
	eyeobj[local_index].init_eom(time, n_rings, i, n_per_ring, j,
		altitude, inclination, stagger);

	eyeobj[local_index].init_icon(icon);
	eyeobj[local_index].set_NAME(name);
	eyeobj[local_index].set_on_off(on_off);

//...... set up the sensor model

        eye_model[local_index].init(sensor_type);
        eye_model[local_index].set_sensor_id(
		eyeobj[local_index].get_GLOBAL_ID());
	eye_model[local_index].set_NOISE_BE_SCAN();
        eyeobj[local_index].set_sensor_model(&eye_model[local_index]);

//...... set up the constellation information

	eyeobj[local_index].set_ring(i);
	eyeobj[local_index].set_id_in_ring(j);
	eyeobj[local_index].set_n_per_ring(n_per_ring);

        local_index++;

      }

      index++;
      phi += delta_phi;

    }

    theta += delta_theta;

  }

//...... reserve the Brilliant Eye objects for SPEEDES

  TOTAL_OBJECTS += N_TOT;
  reserve_n_objects(N_LOC);
  for (i=0; i<N_LOC; i++) define_object(&eyeobj[i]);
  set_interact();

  printf("EYEMAN object created with %4d objects (%4d loc)\n", N_TOT, N_LOC);

}

/************************************************************************
* init_events : initialize events for the grid element objects		*
************************************************************************/
void C_EYEMAN::init_events() {
  int i;
  int temp;
  double tt;
  int MOVING_COVERAGE;
  int EXT_GRAPHICS_SCRIPT;
  int TEST_PROX;
  int scan_type;

  printf("EYEMAN initializing\n");

  MOVING_COVERAGE = event_type("MOVING_COVERAGE");
  TEST_PROX = event_type("TEST_PROX");
  EXT_GRAPHICS_SCRIPT = event_type("EXT_GRAPHICS_SCRIPT");

  scan_type = event_type("SCAN_EYE");

//...... schedule the fixed coverage initial event

  for (i=0; i<N_LOC; i++) {

    eyeobj[i].set_scan_type(scan_type);

    temp = eyeobj[i].get_GLOBAL_ID() - start_uid;
    tt = 10.0 * double(temp) / double(N_TOT);
    temp = int(tt);
    tt = -(100.0*temp) - 100.0;
    tt += turn_on_time;
    if (tt < 0.001) tt = 0.001;

    schedule (tt, MOVING_COVERAGE, EYE, i, NODE);
  }

//...... start up the test prox event

  if ((N_NODES == 1) && (test_prox)) {
    for (i=0; i<N_LOC; i++) {
      schedule (1000.0, TEST_PROX, EYE, i, NODE);
    }
  }

//...... start up the graphics "define" event

  for (i=0; i<N_LOC; i++) {
    schedule (0.0, EXT_GRAPHICS_SCRIPT, EYE, i, NODE);
  }

}

