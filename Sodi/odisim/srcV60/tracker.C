#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h> 

#include "parser.H"
#include "host_user.H"
#include "ext_tracker_mess.H"
#include "stereo_track.H"
#include "detection.H"
#include "def.h"

#ifdef CPP20
#define EXTERN_C extern "C"
#else
#define EXTERN_C extern
#endif

EXTERN_C double walltime();

char *process(char *inbuff, int &outsize,
	double start_time, int ntrk);

int print_flag;
int launch_position;

int main() {
  int id;
  float cycle_time;
  char *name;
  C_HOST_USER *host_user;
  EXT_TRACKER_MESS *out_mess;
  EXT_TRACKER_MESS *in_mess;
  C_PARSER *tracker_parser;
  C_BASETYPE *parameters;
  float start_time;
  float tend;
  float end_time;
  double cpu_start;
  char *inbuff;
  char *outbuff;
  char *buff;
  int insize;
  int outsize;
  long min_cpu_time;
  long max_cpu_time;
  int n_tracks;
  int n_tracks_updated;
  double track_cpu;

//...... read in the parser input

  tracker_parser = new C_PARSER("tracker.par");
  parameters = tracker_parser->get_basetype("parameters");
  name = parameters->get_string("command_center");
  cycle_time = parameters->get_float("cycle_time");
  start_time = parameters->get_float("start_time");
  tend = parameters->get_float("tend");
  max_cpu_time = parameters->get_int("max_cpu_time");
  min_cpu_time = parameters->get_int("min_cpu_time");
  n_tracks = parameters->get_int("n_tracks");
  print_flag = parameters->get_logical("print_flag");
  launch_position = parameters->get_logical("launch_position");

  n_tracks_updated = 0;

//...... initialize

  host_user = new C_HOST_USER();
  id = host_user->getid(name);
  out_mess = new EXT_TRACKER_MESS();
  out_mess->time_tag = start_time;
  out_mess->EM_done_time = cycle_time;

//...... loop while simulation time is less than the end time

  while (start_time < tend) {

    out_mess->objid = id;
    out_mess->n_tracks = n_tracks;
    out_mess->n_tracks_updated = n_tracks_updated;

    in_mess = (EXT_TRACKER_MESS *)host_user->aggressive_module(out_mess);
    n_tracks_updated = in_mess->n_tracks;

    delete out_mess;

    start_time = in_mess->time_tag;
    end_time = in_mess->EM_done_time;

    inbuff = (char *)in_mess;
    inbuff += sizeof(EXT_TRACKER_MESS);
    insize = in_mess->data_bytes -
	(sizeof(EXT_TRACKER_MESS) - sizeof(C_EM_HEADER));

    fprintf(stderr,"TRACKER input: %d bytes starting at time %f ...\n",
	insize, start_time);

    cpu_start = walltime();

//...... process the input data by TRACKER and return an output buffer

    outbuff = process(inbuff, outsize, start_time, n_tracks_updated);

//...... look at the timing

    if (n_tracks_updated) {
      track_cpu = (walltime() - cpu_start) / (n_tracks_updated);
    }else{
      track_cpu = 0.500;
    }

    n_tracks = int(0.51 + double(max_cpu_time) / (track_cpu + 0.025));

    delete in_mess;

//...... dont go too fast

    while ((walltime() - cpu_start) < min_cpu_time);

//...... send the answer back to SPEEDES

    fprintf(stderr,"... Done %f (%f sec per track)\n\n",end_time,track_cpu);

    out_mess = (EXT_TRACKER_MESS *)
	(new char[sizeof(EXT_TRACKER_MESS)+outsize]);
    out_mess->init(outsize);

    buff = (char *)out_mess;
    buff += sizeof(EXT_TRACKER_MESS);
    memcpy(buff, outbuff, outsize);

    out_mess->EM_done_time = cycle_time;
    out_mess->time_tag = end_time;

  }

  return 1;

}

char *process(char *inbuff, int &outsize, double start_time, int ntrk) {
  char *outbuff;
  C_STEREO_TRACK stereo_track;
  C_XQUEUE *detections;
  C_XQUEUE *pdetections;
  C_DETECTION *detection;
  int n_detections;
  char *pbuff;
  char *poutbuff;
  int i,j;
  char *start_pointer;
  int ignore_bytes;
  int copy_bytes;
  double time;
  double lat;
  double lon;

  fprintf(stderr,"%d tracks being processed at time %f\n",
	ntrk, start_time);

//...... create the output track buffer

  outsize = ntrk*sizeof(C_STEREO_TRACK);
  outbuff = NULL;
  if (ntrk == 0) return NULL;

  outbuff = new char[outsize];
  poutbuff = outbuff;

  pbuff = inbuff;
  detections = (C_XQUEUE *)stereo_track.get_detections();

//...... get the copy information for the stereo_track

  start_pointer = stereo_track.get_start_pointer();
  ignore_bytes = start_pointer - (char *)&stereo_track;
  copy_bytes = stereo_track.get_st_bytes() + sizeof(C_XQUEUE);

  if (print_flag) {
    fprintf(stderr,"stereo track ignore %d bytes, copy %d bytes\n",
	ignore_bytes, copy_bytes);
  }

//...... update each track

  for (i=0; i<ntrk; i++) {

    stereo_track.init((C_STEREO_TRACK *)pbuff);
    pdetections = ((C_STEREO_TRACK *)pbuff)->get_detections();
    n_detections = pdetections->get_length();
    pbuff += sizeof(C_STEREO_TRACK);

    if (print_flag) {
      fprintf(stderr, "%d detections for track %d\n",
	n_detections, stereo_track.get_id());
    }

    detections->reset();
    detection = new C_DETECTION[n_detections];
    for (j=0; j<n_detections; j++) {
      memcpy((char *)&detection[j], pbuff, sizeof(C_DETECTION));
      pbuff += sizeof(C_DETECTION);
      detections->push_bot(&detection[j]);
    }

//...... update the track weighting current detections more heavily

    stereo_track.set_weight_current();
    stereo_track.update_state(0);
    if (print_flag) stereo_track.print();

//...... copy the updated track into the output buffer

    memcpy(poutbuff, (char *)&stereo_track, sizeof(C_STEREO_TRACK));
    poutbuff += sizeof(C_STEREO_TRACK);

//...... check if determining launch_position

    if (launch_position) {
      stereo_track.set_weight_old();
      stereo_track.update_state(0);
      stereo_track.launch_position(time, lat, lon);
      if (time != -1.0) {
        fprintf(stderr,"track %d, Launch time %f, Latitude %f, Longitude %f\n",
		stereo_track.get_id(), time, lat*DEGRAD, lon*DEGRAD);
      }
    }

    delete [] detection; //RVI 2/18/98

  }

  return outbuff;

}

