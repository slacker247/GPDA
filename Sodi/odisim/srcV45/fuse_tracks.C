// fuse_tracks.C method file

#include <stdio.h>
#include <stdlib.h>

#include "track.H"
#include "centobj.H"
#include "fuse_tracks.H"
#include "fuse_tracks_mess.H"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

int C_FUSE_TRACKS::done = 0;
int C_FUSE_TRACKS::FUSE_TRACKS = 0;
int C_FUSE_TRACKS::REMOVE_TRACKS = 0;

/************************************************************************
* C_FUSE_TRACKS : construct a fuse_tracks object			*
************************************************************************/
C_FUSE_TRACKS::C_FUSE_TRACKS() {

  if (!done) {
    done = 1;
    FUSE_TRACKS = event_type("FUSE_TRACKS");
    REMOVE_TRACKS = event_type("REMOVE_TRACKS");
  }

//  set_lazy();

}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_FUSE_TRACKS::init(C_HEADER *header) {
  int size;

  size = header->bytes + sizeof(FUSE_TRACKS_MESS);
  fuse_tracks_mess = (FUSE_TRACKS_MESS *)new char[size];
  if (fuse_tracks_mess == NULL) {
    fprintf(stderr,"Error (FUSE_TRACKS) no more memory\n");
    exit(1);
  }
  memcpy((char *)fuse_tracks_mess, (char *)header, size);

  if (fuse_tracks_mess->nbytes != fuse_tracks_mess->bytes) {
    fprintf(stderr,"Error (FUSE_TRACKS) bad init\n");
  }

}

/************************************************************************
* exchange : exchange state variables                                   *
************************************************************************/
void C_FUSE_TRACKS::exchange() {
  C_CENTOBJ *centobj;

  centobj = (C_CENTOBJ *)SIMOBJ;
  centobj->exchange_remove_tracks_on(remove_tracks_on);

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_FUSE_TRACKS::temp_process() {
  C_XQUEUE *tracks;
  C_CENTOBJ *centobj;
  char *buff;
  char *maxbuf;
  C_TRACK *track;
  C_TRACK *old_track;
  int trackid;

  centobj = (C_CENTOBJ *)SIMOBJ;
  tracks = centobj->get_tracks();
  remove_tracks_on = centobj->get_remove_tracks_on();

  buff = (char *)fuse_tracks_mess;
  buff += sizeof(FUSE_TRACKS_MESS);
  maxbuf = buff + fuse_tracks_mess->bytes;

  while (buff < maxbuf) {
    track = (C_TRACK *)RB_FREE_GENERATE(buff);
    trackid = track->get_id();

    old_track = (C_TRACK *)tracks->find(trackid);
    if (old_track) {
      *tracks -= old_track;
      RB_FREE_DELETE(old_track);
    }

    *tracks += track;
    buff += freeobjs.get_size(track);

  }

//...... start up the remove tracks event if not already started

  if ((remove_tracks_on == 0) && (tracks->get_length())) {
    remove_tracks_on = 1;
    schedule(
	TIME_TAG + centobj->get_stale_track_time(),
	REMOVE_TRACKS,
	OBJECT_TYPE,
	LOCAL_ID,
	NODE);
  }

//  fprintf(stderr,"Command Center has %d tracks\n",tracks->get_length());

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_FUSE_TRACKS::perm_process() {


}

/************************************************************************
* cleanup : clean up data structures used                               *
************************************************************************/
void C_FUSE_TRACKS::cleanup() {


}
