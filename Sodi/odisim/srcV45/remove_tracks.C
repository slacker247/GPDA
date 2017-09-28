// remove_tracks.C method file

#include <stdio.h>

#include "track.H"
#include "centobj.H"
#include "remove_tracks.H"
#include "remove_tracks_mess.H"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

int C_REMOVE_TRACKS::done = 0;
int C_REMOVE_TRACKS::REMOVE_TRACKS = 0;

/************************************************************************
* C_REMOVE_TRACKS : construct a remove_tracks object			*
************************************************************************/
C_REMOVE_TRACKS::C_REMOVE_TRACKS() {

  if (!done) {
    done = 1;
    REMOVE_TRACKS = event_type("REMOVE_TRACKS");
  }

//  set_lazy();

}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_REMOVE_TRACKS::init(C_HEADER *) {

}

/************************************************************************
* exchange : exchange state variables                                   *
************************************************************************/
void C_REMOVE_TRACKS::exchange() {
  C_CENTOBJ *centobj;

  centobj = (C_CENTOBJ *)SIMOBJ;
  centobj->exchange_remove_tracks_on(remove_tracks_on);

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_REMOVE_TRACKS::temp_process() {
  int i,len;
  C_XQUEUE *tracks;
  C_CENTOBJ *centobj;
  C_TRACK *track;
  double stale_track_time;
  double time;

  centobj = (C_CENTOBJ *)SIMOBJ;
  tracks = centobj->get_tracks();
  stale_track_time = centobj->get_stale_track_time();

  len = tracks->get_length();
  for (i=0; i<len; i++) {
    track = (C_TRACK *)tracks->pop_top();
    time = track->get_time_tag();
    if ((TIME_TAG - time) > stale_track_time) {
      tracks->push_top(track);
      *tracks -= track;
      RB_FREE_DELETE(track);
    }else{
      tracks->push_bot(track);
    }
  }

//...... reschedule this event if there are still tracks

  len = tracks->get_length();
  if (len) {
    remove_tracks_on = 1;
    schedule(
	TIME_TAG + centobj->get_stale_track_time(),
	REMOVE_TRACKS,
	OBJECT_TYPE,
	LOCAL_ID,
	NODE);
  }else{
    remove_tracks_on = 0;
  }

//  fprintf(stderr,"Command Center has %d tracks\n",tracks->get_length());

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_REMOVE_TRACKS::perm_process() {


}

/************************************************************************
* cleanup : clean up data structures used                               *
************************************************************************/
void C_REMOVE_TRACKS::cleanup() {


}
