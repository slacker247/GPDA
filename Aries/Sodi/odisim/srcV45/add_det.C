// add_det.C method file

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include "add_det.H"
#include "sensobj.H"
#include "detection.H"
#include "def.h"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

int C_ADD_DET::done = 0;
int C_ADD_DET::ADD_DET = 0;
int C_ADD_DET::TRACK_OUT = 0;
int C_ADD_DET::CENTER = 0;

#define STALE_TRACK_TIME 30.0

/************************************************************************
* C_ADD_DET : construct a add_det object				*
************************************************************************/
C_ADD_DET::C_ADD_DET() {

  if (!done) {
    ADD_DET = event_type("ADD_DET");
    TRACK_OUT = event_type("TRACK_OUT");
    CENTER = object_type("CENTER");
    done = 1;
  }

}

/************************************************************************
* init : initialize the event						*
************************************************************************/
void C_ADD_DET::init(C_HEADER *header) {
  int size;

  size = header->bytes + sizeof(ADD_DET_MESS);
  add_det_mess = (ADD_DET_MESS *) new char[size];
  if (add_det_mess == NULL) {
    fprintf(stderr,"Error (ADD_DET) no more memory\n");
    exit(1);
  }

  memcpy((char *)add_det_mess, (char *)header, size);

}

/************************************************************************
* exchange : exchange state variables					*
************************************************************************/
void C_ADD_DET::exchange() {
  C_SENSOBJ *sensobj;

  sensobj = (C_SENSOBJ *)SIMOBJ;
  sensobj->exchange_trackup_time(trackup_time);

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_ADD_DET::temp_process() {
  int i,len;
  char *buff;
  C_DETECTION *detection;
  C_SENSOBJ *sensobj;
  C_SENSOR_MODEL *sensor_model;
  C_XQUEUE det;
  C_XQUEUE *tracks;

  len = add_det_mess->bytes / sizeof(C_DETECTION);

  buff = (char *)add_det_mess;
  buff += sizeof(ADD_DET_MESS);

  for (i=0; i<len; i++) {
    detection = (C_DETECTION *)RB_FREE_GENERATE(buff);
    det.push_bot(detection);
    buff += sizeof(C_DETECTION);
  }

  sensobj = (C_SENSOBJ *)SIMOBJ;
  tracks = sensobj->get_tracks();
  sensor_model = sensobj->get_sensor_model();
  sensor_model->set_rbq(rbq);
  sensor_model->update_tracks(TIME_TAG, &det, tracks);

  trackup_time = sensobj->get_trackup_time();
  if (OBJECT_TYPE == CENTER) {
    if (trackup_time + STALE_TRACK_TIME < TIME_TAG) {
      schedule(GONE*TIME_TAG, TRACK_OUT, OBJECT_TYPE, LOCAL_ID, NODE);
      trackup_time = TIME_TAG;
    }
  }

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_ADD_DET::perm_process() {

//  fprintf(stderr,"adding detection...\n");

}

/************************************************************************
* cleanup : clean up data structures used                               *
************************************************************************/
void C_ADD_DET::cleanup() {

  delete add_det_mess;

}

