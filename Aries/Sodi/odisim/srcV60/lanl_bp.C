#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h> 
#include <time.h> 

#include "parser.H"
#include "kepler.H"
#include "host_user.H"
#include "ext_lanl_bp_mess.H"
#include "lanl_bp_input.H"
#include "lanl_bp_output.H"

#ifdef CPP20
#define EXTERN_C extern "C"
#else
#define EXTERN_C extern
#endif

#ifndef IRIS4D
//EXTERN_C int clock();
#endif

#define RADDEG  0.0174532925199432958

char *process(char *inbuff, int insize, int &outsize,
	double start_time, double end_time);

C_HOST_USER *host_user;

int main() {
  int id;
  float cycle_time;
  char *name;
  EXT_LANL_BP_MESS *out_mess;
  EXT_LANL_BP_MESS *in_mess;
  C_PARSER *lanl_bp_parser;
  C_BASETYPE *parameters;
  float start_time;
  float tend;
  float end_time;
  long cpu_start;
  char *inbuff;
  char *outbuff;
  char *buff;
  int insize;
  int outsize;
  long cpu_time;

//...... read in the parser input

  lanl_bp_parser = new C_PARSER("lanl_bp.par");
  parameters = lanl_bp_parser->get_basetype("parameters");
  name = parameters->get_string("command_center");
  cycle_time = parameters->get_float("cycle_time");
  start_time = parameters->get_float("start_time");
  tend = parameters->get_float("tend");
  cpu_time = parameters->get_int("cpu_time");

//...... initialize

  host_user = new C_HOST_USER();
  id = host_user->getid(name);
  out_mess = new EXT_LANL_BP_MESS();
  out_mess->time_tag = start_time;
  out_mess->EM_done_time = cycle_time;

//...... loop while simulation time is less than the end time

  while (start_time < tend) {


    out_mess->objid = id;

    in_mess = (EXT_LANL_BP_MESS *)host_user->blocking_module(out_mess);
    delete out_mess;

    start_time = in_mess->time_tag;
    end_time = in_mess->EM_done_time;

    inbuff = (char *)in_mess;
    inbuff += sizeof(EXT_LANL_BP_MESS);
    insize = in_mess->data_bytes -
	(sizeof(EXT_LANL_BP_MESS) - sizeof(C_EM_HEADER));

    fprintf(stderr,"LANL BP input: %d bytes starting at time %f ...\n",
	insize, start_time);

    cpu_start = clock();
    while ((clock() - cpu_start) < cpu_time*1000000);

//...... process the input data by LANL BP and return an output buffer

    outbuff = process(inbuff, insize, outsize, start_time, end_time);
    delete in_mess;

//...... send the answer back to SPEEDES

    fprintf(stderr,"... Done %f\n\n",end_time);

    out_mess = (EXT_LANL_BP_MESS *)
	(new char[sizeof(EXT_LANL_BP_MESS)+outsize]);
    out_mess->init(outsize);

    buff = (char *)out_mess;
    buff += sizeof(EXT_LANL_BP_MESS);
    memcpy(buff, outbuff, outsize);

    out_mess->EM_done_time = cycle_time;
    out_mess->time_tag = end_time;

  }

  return 1;

}

char *process(char *inbuff, int insize, int &outsize,
	double start_time, double end_time) {
  char *outbuff;
  C_LANL_BP_INPUT *lanl_bp_input;
  C_LANL_BP_OUTPUT *lanl_bp_output;
  int n_tracks;
  int i;
  double *X;
  double *V;
  C_KEPLER kepler;
  double timp;
  double lat,lon;
  int n_assignments;

  kepler.set_ECI(1);

  lanl_bp_input = (C_LANL_BP_INPUT *)inbuff;
  n_tracks = insize / sizeof(C_LANL_BP_INPUT);

//...... process the input and generate the output

  for (i=0; i<n_tracks; i++) {
    X = lanl_bp_input[i].get_Xtrack();
    V = lanl_bp_input[i].get_Vtrack();

    printf("\nTrack %d, time = %f, (%f seconds old)\n",
	lanl_bp_input[i].get_id(),
	lanl_bp_input[i].get_time(),
	start_time - lanl_bp_input[i].get_time());

    printf("Pos = (%f %f %f), Vel = (%f %f %f), Err = %f\n",
	X[0], X[1], X[2],
	V[0], V[1], V[2],
	lanl_bp_input[i].get_Xerror());

    kepler.init(X,V);
    kepler.update_impact();
    timp = kepler.get_timp();
    X = kepler.get_imp_pos();
    V = kepler.get_imp_vel();
    printf("Impact Pos = (%f %f %f), Vel = (%f %f %f), Speed = %f\n",
	X[0], X[1], X[2],
	V[0], V[1], V[2],
	sqrt(V[0]*V[0]+V[1]*V[1]+V[2]*V[2]));

    kepler.eci_to_ecr(lanl_bp_input[i].get_time()+timp,X,V);
    kepler.ecr_to_eci(lanl_bp_input[i].get_time()+timp,X,V);
    kepler.eci_to_ecr(lanl_bp_input[i].get_time()+timp,X,V);
    kepler.xyz_to_latlon(X,lat,lon);
    kepler.ecr_to_eci(lanl_bp_input[i].get_time()+timp,X,V);
/*
    kepler.get_pos_vel(timp/2.0,X,V);
    kepler.init(X,V);
    kepler.update_impact();
*/
    printf("Impact Pos = (%f %f %f), Vel = (%f %f %f), Speed = %f\n",
	X[0], X[1], X[2],
	V[0], V[1], V[2],
	sqrt(V[0]*V[0]+V[1]*V[1]+V[2]*V[2]));
    printf("impact at time %f, lat lon = %f, %f\n",lanl_bp_input[i].get_time()+timp,
	lat/RADDEG,lon/RADDEG);

  }

//...... generate the output

/*
  outsize = 0;
  outbuff = NULL;
*/

  n_assignments = n_tracks;
  lanl_bp_output = new C_LANL_BP_OUTPUT[n_assignments];
  outbuff = (char *)lanl_bp_output;
  outsize = n_assignments * sizeof(C_LANL_BP_OUTPUT);

  for (i=0; i<n_assignments; i++) {
    lanl_bp_output[i].set_threat_id(lanl_bp_input[i].get_id());
    lanl_bp_output[i].set_asset_id(11);  // use GBI_FARM_11 (NORAD)
    lanl_bp_output[i].set_launch_time(end_time+60.0);
  }

//...... end of LANL process

  return outbuff;

}

