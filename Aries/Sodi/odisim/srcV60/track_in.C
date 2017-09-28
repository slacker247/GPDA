// track_in.C method file

#include <stdio.h>

#include "sensobj.H"
#include "track_in.H"
#include "track_in_mess.H"
#include "stereo_track.H"

#include "def.h"

extern int TRACK_IN;

/************************************************************************
* C_TRACK_IN : construct a track_in object				*
************************************************************************/
C_TRACK_IN::C_TRACK_IN() {
  set_lazy();
}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_TRACK_IN::init(C_HEADER *header) {
  int bytes;
  char *buff;

  bytes = header->bytes + sizeof(TRACK_IN_MESS);
  buff = new char[bytes];
  track_in_mess = (TRACK_IN_MESS *)buff;

  if (buff == NULL) {
    fprintf(stderr,"Error (TRACK_IN) could not create memory\n");
    return;
  }

  memcpy((char *)track_in_mess, (char *)header, bytes);

}

/************************************************************************
* exchange : exchange state variables                                   *
************************************************************************/
void C_TRACK_IN::exchange() {

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_TRACK_IN::temp_process() {
  //char s[120];
  C_SENSOBJ *sensobj;
  C_XQUEUE *tracks;
  char *buff;
  char *buffend;
  int n_tracks;
  int i;
  int track_id;
  C_STEREO_TRACK *stereo_track;

/*
  sprintf(s,"TRACK IN at time %f\n", TIME_TAG);
  RB_PRINT(stderr,s);
*/

  sensobj = (C_SENSOBJ *)SIMOBJ;
  tracks = sensobj->get_tracks();
  n_tracks = track_in_mess->n_tracks;

//...... update the track state using the input buffer

  buff = (char *)track_in_mess;
  buff += sizeof(TRACK_IN_MESS);
  buffend = buff + track_in_mess->bytes;

  for (i=0; i<n_tracks; i++) {

    track_id = ((C_STEREO_TRACK *)buff)->get_id();
    stereo_track = (C_STEREO_TRACK *)tracks->find(track_id);

    if (stereo_track != NULL) {
      stereo_track->init((C_STEREO_TRACK *)buff);
    }

    buff += sizeof(C_STEREO_TRACK);

  }

  if (buff != buffend) {
    fprintf(stderr,"Error (TRACK IN) bad buffer pointer\n");
  }

}

/************************************************************************
* lazy - do something if the event is reprocessed			*
************************************************************************/
int C_TRACK_IN::lazy() {

  return 1;

}



/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_TRACK_IN::perm_process() {

}


/************************************************************************
* cleanup : cleanup this event event					*
************************************************************************/
void C_TRACK_IN::cleanup() {

  delete track_in_mess;

}


