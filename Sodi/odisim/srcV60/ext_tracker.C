// ext_tracker.C method file

#include <strstream.h>
#include <stdio.h>

#include "ext_tracker.H"
#include "ext_tracker_mess.H"
#include "ccc_plan_mess.H"
#include "centobj.H"
#include "stereo_track.H"
#include "detection.H"

#include "lanl_bp_input.H"
#include "lanl_bp_output.H"
#include "host_user.H"

#include "def.h"

extern int EXT_TRACKER;
extern int CENTER;

/************************************************************************
* C_EXT_TRACKER : construct a ext_tracker object			*
************************************************************************/
C_EXT_TRACKER::C_EXT_TRACKER() {
  set_lazy();
}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_EXT_TRACKER::init(C_HEADER *header) {
  C_EM_MODULE *em_module;

  em_module = (C_EM_MODULE *)header;
  size = sizeof(C_EM_MODULE) + em_module->data_bytes;
  ext_tracker_mess = (EXT_TRACKER_MESS *) new char[size];
  if (size != 0) memcpy((char *)ext_tracker_mess, (char *)header, size);

}

/************************************************************************
* exchange : exchange state variables                                   *
************************************************************************/
void C_EXT_TRACKER::exchange() {

  if (toggle) {
    SIMOBJ->add_block(item);
    toggle = 0;
  }else{
    SIMOBJ->remove_block(item);
    toggle = 1;
  }

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_EXT_TRACKER::temp_process() {
  int i,j,len;
  C_SENSOBJ *sensobj;
  EXT_TRACKER_MESS *out_mess;
  int outsize;
  C_XQUEUE *tracks;
  C_XQUEUE *detections;
  C_STEREO_TRACK *stereo_track;
  C_DETECTION *detection;
  int n_detections;
  char *pbuff;
  int track_id;

  sensobj = (C_SENSOBJ *)SIMOBJ;
  tracks = sensobj->get_tracks();

//...... set up the blocking condition for next time

  toggle = 1;

  item = RB_NEW_C_ITEM();
  item->set_time_tag(time_tag + ext_tracker_mess->EM_done_time);
  item->set_id(ext_tracker_mess->EM_socket);

//...... update the track state using the input buffer

  pbuff = (char *)ext_tracker_mess;
  pbuff += sizeof(EXT_TRACKER_MESS);

  for (i=0; i<ext_tracker_mess->n_tracks_updated; i++) {

    track_id = ((C_STEREO_TRACK *)pbuff)->get_id();
    stereo_track = (C_STEREO_TRACK *)tracks->find(track_id);

    if (stereo_track != NULL) {
      stereo_track->init((C_STEREO_TRACK *)pbuff);
    }else{
//      fprintf(stderr,"Warning (EXT_TRACKER), missing track %d ?\n",track_id);
    }

//    stereo_track->print();
    pbuff += sizeof(C_STEREO_TRACK);

  }

//...... determine how many bytes to send out

  outsize = 0;
  stereo_track = (C_STEREO_TRACK *)tracks->get_top();

  len = tracks->get_length();
  if (len > ext_tracker_mess->n_tracks) len = ext_tracker_mess->n_tracks;

  n_detections = 0;
  for (i=0; i<len; i++) {
    detections = stereo_track->get_detections();
    n_detections += detections->get_length();
    stereo_track = (C_STEREO_TRACK *)stereo_track->get_link();
  }

  outsize = n_detections*sizeof(C_DETECTION) + len*sizeof(C_STEREO_TRACK);

//...... create the output message

  out_mess = (EXT_TRACKER_MESS *)
	RB_NEW_ARRAY_char(outsize+sizeof(EXT_TRACKER_MESS));
  out_mess->init(outsize);
  out_mess->n_tracks = len;

//...... fill up the track output buffer

  pbuff = (char *)out_mess;
  pbuff += sizeof(EXT_TRACKER_MESS);

  for (i=0; i<len; i++) {

    stereo_track = (C_STEREO_TRACK *)tracks->pop_top();

    memcpy(pbuff, (char *)stereo_track, sizeof(C_STEREO_TRACK));
    pbuff += sizeof(C_STEREO_TRACK);

    detections = stereo_track->get_detections();
    n_detections = detections->get_length();
    detection = (C_DETECTION *)detections->get_top();
    for (j=0; j<n_detections; j++) {
      memcpy(pbuff, (char *)detection, sizeof(C_DETECTION));
      pbuff += sizeof(C_DETECTION);
      detection = (C_DETECTION *)detection->get_link();
    }

    tracks->push_bot(stereo_track);

  }

//...... fill the header information

  out_mess->time_tag = time_tag;
  out_mess->evtype = EXT_TRACKER;
  out_mess->objid = -1;
  out_mess->obtype = OBJECT_TYPE;
  out_mess->ext = 0;
//  out_mess->data_bytes = sizeof(EXT_TRACKER_MESS) - sizeof(C_EM_HEADER);
  out_mess->EM_interaction = EM_MODULE;
  out_mess->EM_socket = ext_tracker_mess->EM_socket;

  out_mess->EM_done_time = time_tag + ext_tracker_mess->EM_done_time;
  buff = (char *)out_mess;

  ostrstream so;
  so << "EXT TRACKER at time " << time_tag
     << " sending " << out_mess->n_tracks << " tracks\n" << ends;
  RB_PRINT(stderr,so.str());

//...... if aggressive mode, send the data out right away

  if (ext_tracker_mess->EM_interaction & EM_AGGRESSIVE) {
    HOST_USER->send_message(out_mess);
  }

}

/************************************************************************
* lazy - do something if the event is reprocessed			*
************************************************************************/
int C_EXT_TRACKER::lazy() {

  cerr << "LAZY EXT TRACKER at time " << time_tag << "\n";
  return 1;

}



/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_EXT_TRACKER::perm_process() {
  EXT_TRACKER_MESS *out_mess;

//...... send the output back to LANL

  if (!(ext_tracker_mess->EM_interaction & EM_AGGRESSIVE)) {
    out_mess = (EXT_TRACKER_MESS *)buff;
    HOST_USER->send_message(out_mess);
  }

}


/************************************************************************
* cleanup : cleanup this event event					*
************************************************************************/
void C_EXT_TRACKER::cleanup() {

  //delete buff; //RVI 2/18/98
  RB_DELETE_ARRAY_char(buff); //RVI 2/18/98
  delete ext_tracker_mess;

}


