// track_proc.C method file

#include <stdio.h>

#include "track_proc.H"
#include "track_proc_mess.H"
#include "track_in_mess.H"
#include "stereo_track.H"
#include "detection.H"

#include "def.h"

int C_TRACK_PROC::done = 0;
int C_TRACK_PROC::TRACK_PROC = 0;
int C_TRACK_PROC::TRACK_IN = 0;

/************************************************************************
* C_TRACK_PROC : construct a track_proc object				*
************************************************************************/
C_TRACK_PROC::C_TRACK_PROC() {

  if (!done) {
    TRACK_PROC = event_type("TRACK_PROC");
    TRACK_IN = event_type("TRACK_IN");
    done = 1;
  }

  set_lazy();

}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_TRACK_PROC::init(C_HEADER *header) {
  int bytes;
  char *buff;

  bytes = header->bytes + sizeof(TRACK_PROC_MESS);
  buff = new char[bytes];
  track_proc_mess = (TRACK_PROC_MESS *)buff;

  if (buff == NULL) {
    fprintf(stderr,"Error (TRACK_PROC) could not create memory\n");
    return;
  }

  memcpy((char *)track_proc_mess, (char *)header, bytes);

}

/************************************************************************
* exchange : exchange state variables                                   *
************************************************************************/
void C_TRACK_PROC::exchange() {

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_TRACK_PROC::temp_process() {
  int i,j;
  //char s[120];
  char *buff;
  char *buffend;
  int outsize;
  int n_tracks;
  C_STEREO_TRACK stereo_track;
  C_XQUEUE *detections;
  C_DETECTION *detection;
  int n_detections;
  char *bf;
  char *bfend;
  TRACK_IN_MESS *track_in_mess;

/*
  sprintf(s,"TRACK PROC at time %f (%d %d %d)\n",
	time_tag,
	track_proc_mess->object_type,
	track_proc_mess->object_id,
	track_proc_mess->object_node);

  RB_PRINT(stderr,s);
*/

  buff = (char *)track_proc_mess;
  buff += sizeof(TRACK_PROC_MESS);
  buffend = buff + track_proc_mess->bytes;
  n_tracks = track_proc_mess->n_tracks;
  outsize = n_tracks*sizeof(C_STEREO_TRACK);

  track_in_mess = (TRACK_IN_MESS *)schedule(
	time_tag + 10.0,
	TRACK_IN,
	track_proc_mess->object_type,
	track_proc_mess->object_id,
	track_proc_mess->object_node,
	outsize,
	bf	);

  bfend = bf + outsize;

  track_in_mess->n_tracks = n_tracks;

  detections = (C_XQUEUE *)stereo_track.get_detections();

  for (i=0; i<n_tracks; i++) {

    memcpy((char *)&stereo_track, buff, sizeof(C_STEREO_TRACK));
    buff += sizeof(C_STEREO_TRACK);

    n_detections = detections->get_length();
    detection = new C_DETECTION[n_detections];
    detections->reset();

    for (j=0; j<n_detections; j++) {
      memcpy((char *)&detection[j], buff, sizeof(C_DETECTION));
      buff += sizeof(C_DETECTION);
      detections->push_bot(&detection[j]);
    }

    stereo_track.update_state(0);

    memcpy(bf, (char *)&stereo_track, sizeof(C_STEREO_TRACK));
    bf += sizeof(C_STEREO_TRACK);

    delete [] detection; //RVI 2/17/98

  }

  if (bf != bfend) {
    fprintf(stderr,"Error (TRACK_PROC) bad bf pointer\n");
  }

  if (buff != buffend) {
    fprintf(stderr,"Error (TRACK_PROC) bad bf pointer\n");
  }

}

/************************************************************************
* lazy - do something if the event is reprocessed			*
************************************************************************/
int C_TRACK_PROC::lazy() {

  return 1;

}



/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_TRACK_PROC::perm_process() {

}


/************************************************************************
* cleanup : cleanup this event event					*
************************************************************************/
void C_TRACK_PROC::cleanup() {

  delete track_proc_mess;

}


