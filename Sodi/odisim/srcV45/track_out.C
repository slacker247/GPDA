// track_out.C method file

#include <stdio.h>

#include "track_out.H"
#include "track_out_mess.H"
#include "track_proc_mess.H"
#include "sensobj.H"
#include "stereo_track.H"
#include "detection.H"

#include "def.h"

int C_TRACK_OUT::done = 0;
int C_TRACK_OUT::TRACK_OUT = 0;
int C_TRACK_OUT::TRACK_PROC = 0;
int C_TRACK_OUT::NOSTATE = 0;

#define NDETMAX 100

/************************************************************************
* C_TRACK_OUT : construct a track_out object				*
************************************************************************/
C_TRACK_OUT::C_TRACK_OUT() {

  if (!done) {
    TRACK_OUT = event_type("TRACK_OUT");
    TRACK_PROC = event_type("TRACK_PROC");
    NOSTATE = object_type("NOSTATE");
    done = 1;
  }

}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_TRACK_OUT::init(C_HEADER *) {

}

/************************************************************************
* exchange : exchange state variables                                   *
************************************************************************/
void C_TRACK_OUT::exchange() {
  C_SENSOBJ *sensobj;

  sensobj = (C_SENSOBJ *)SIMOBJ;
  sensobj->exchange_trackup_node(trackup_node);

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_TRACK_OUT::temp_process() {
  int i,j,k,n;
  C_SENSOBJ *sensobj;
  TRACK_PROC_MESS *track_proc_mess;
  int outsize;
  //char s[120];
  C_XQUEUE *tracks;
  C_XQUEUE *detections;
  C_XQUEUE *buffdet;
  C_STEREO_TRACK *stereo_track;
  C_STEREO_TRACK *stereo_track1;
  C_DETECTION *detection;
  int n_detections;
  char *bf;
  char *buff;
  int n_tracks;
  int n_per_node;
  int remainder;
  int trackup_node_old;

  sensobj = (C_SENSOBJ *)SIMOBJ;
  trackup_node_old = sensobj->get_trackup_node();
  trackup_node = trackup_node_old;

  tracks = sensobj->get_tracks();

  n_tracks = tracks->get_length();
  n_per_node = n_tracks / N_NODES;
  remainder = n_tracks % N_NODES;

  stereo_track = (C_STEREO_TRACK *)tracks->get_top();

  for (i=0; i<N_NODES; i++) {

    n = n_per_node;
    if (0 < remainder) {
      n++;
    }else{
      if (remainder == 0) trackup_node = (trackup_node_old + i) % N_NODES;
    }
    remainder--;

    if (n==0) break;

/*
    sprintf(s,"NO STATE TRACKER at time %f sending %d tracks to node %d\n",
	TIME_TAG, n, (trackup_node_old+i)%N_NODES);
    RB_PRINT(stderr,s);
*/

    stereo_track1 = stereo_track;
    n_detections = 0;
    for (j=0; j<n; j++) {
      detections = stereo_track1->get_detections();
      if (detections->get_length() > NDETMAX) {
        n_detections += NDETMAX;
      }else{
        n_detections += detections->get_length();
      }
      stereo_track1 = (C_STEREO_TRACK *)stereo_track1->get_link();
    }

    outsize = n_detections*sizeof(C_DETECTION)
		+ n*sizeof(C_STEREO_TRACK);

    track_proc_mess = (TRACK_PROC_MESS *)schedule(
		TIME_TAG+10.0,
		TRACK_PROC,
		NOSTATE,
		0,
		(trackup_node_old+i)%N_NODES,
		outsize,
		bf);

    track_proc_mess->object_type = OBJECT_TYPE;
    track_proc_mess->object_id = LOCAL_ID;
    track_proc_mess->object_node = NODE;
    track_proc_mess->n_tracks = n;

    stereo_track1 = stereo_track;
    buff = bf;

    for (j=0; j<n; j++) {

      memcpy(buff, (char *)stereo_track1, sizeof(C_STEREO_TRACK));
      buffdet = (C_XQUEUE *) ((C_STEREO_TRACK *)buff)->get_detections();
      buff += sizeof(C_STEREO_TRACK);

      detections = stereo_track1->get_detections();
      n_detections = detections->get_length();
      if (NDETMAX < n_detections) n_detections = NDETMAX;
      buffdet->set_length(n_detections);

//      detection = (C_DETECTION *)detections->get_top();
      detection = (C_DETECTION *)detections->get_bot();
      for (k=0; k<n_detections; k++) {
        memcpy(buff, (char *)detection, sizeof(C_DETECTION));
        buff += sizeof(C_DETECTION);
//        detection = (C_DETECTION *)detection->get_link();
        detection = (C_DETECTION *)detection->get_backlink();
      }

      stereo_track1 = (C_STEREO_TRACK *)stereo_track1->get_link();
    }

    stereo_track = stereo_track1;

  }

  if (buff != bf+outsize) {
    fprintf(stderr,"Error (TRACK_OUT) bad buffer pointers\n");
  }

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_TRACK_OUT::perm_process() {

}


/************************************************************************
* cleanup : cleanup this event event					*
************************************************************************/
void C_TRACK_OUT::cleanup() {

}


