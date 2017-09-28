// scan.C method file

#include <strstream.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include "scan.H"
#include "scan_mess.H"
#include "send_message_mess.H"
#include "sensobj.H"
#include "graphman.H"
#include "eom.H"
#include "oid.H"
#include "track.H"
#include "detection.H"
#include "def.h"
#include "host_user.H"

#include "ext_mess.H"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

extern int COM;
extern int GRAPHICS;
extern int SEND_MESSAGE;
extern int SCAN;

static int CCMESS = 1; /* If set to 1, it allows the lanl_bp to shoot.
			  Also slows down the simulation significantly
			  if it is set to 1 */

/************************************************************************
* C_SCAN : construct a scan object					*
************************************************************************/
C_SCAN::C_SCAN() {}

/************************************************************************
* exchange : exchange state variables					*
************************************************************************/
void C_SCAN::exchange() {
  C_SENSOBJ *sensobj;

  sensobj = (C_SENSOBJ *)SIMOBJ;
  sensobj->EXCHANGE_SEED(seed);
  sensobj->exchange_scanning(scanning);
  sensobj->exchange_last_tcom(last_tcom);
  sensobj->exchange_tracks(new_tracks);

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_SCAN::temp_process() {
  C_SENSOBJ *sensobj;
  C_SENSOR_MODEL *sensor_model;
  C_XQUEUE *movers;
  C_XQUEUE good_movers;
  C_XQUEUE bad_movers;
  C_XQUEUE det;
  C_XQUEUE *old_tracks;
  C_OID *oid;
  C_EOM *eom;
  int len;
  int i;
  int good_flag;
  double Ps[3], Vs[3];
  double Pm[3], Vm[3];
  double Rvec[3], Rdotvec[3];
  double R, Rdot;
  double signal;
  double luminosity;
  double R2, R4;
  char s[120];
  C_SQ_ITEM *top_mover;
  C_SQ_ITEM *bot_mover;
  C_TRACK *track;
  C_DETECTION *detection;
  SEND_MESSAGE_MESS *send_message_mess;
  int size;
  int size1;
  char *buff;
  char *buff1;

//...... get the sensor object

  sensobj = (C_SENSOBJ *)SIMOBJ;
  sensor_model = sensobj->get_sensor_model();
  sensobj->get_pos_vel(TIME_TAG,Ps,Vs);
  seed = sensobj->get_SEED();
  RANDOM->set_seed(seed);

//...... get the last time com. was sent

  last_tcom = sensobj->get_last_tcom();

//...... filter the movers first

  movers = sensobj->get_movers();
  top_mover = movers->get_top();
  bot_mover = movers->get_bot();

  len = movers->get_length();
//  oid = (C_OID *)movers->get_top();

  for (i=0; i<len; i++) {

    oid = (C_OID *)movers->pop_top();
    good_flag = 1;

    eom = (C_EOM *)oid->get_peom();

    if (eom->get_endtime() + 0.001 < TIME_TAG) {
      ostrstream so;
      so << "Error (SCAN) eom " << eom->get_id()
	 << " at time " << TIME_TAG << " is out of range\n" << ends;
      RB_PRINT(stderr,so.str());
      good_flag = 0;
    }else{
      eom->get_pos_vel(TIME_TAG,Pm,Vm);
    }


//...... test if good detection

    while (good_flag) {

//...... check range

      Rvec[0] = Pm[0]-Ps[0];
      Rvec[1] = Pm[1]-Ps[1];
      Rvec[2] = Pm[2]-Ps[2];
      R = sqrt(Rvec[0]*Rvec[0] + Rvec[1]*Rvec[1] + Rvec[2]*Rvec[2]);

      if (R > sensor_model->get_rmax()) {
	good_flag = 0;
        break;
      }

      if (R < sensor_model->get_rmin()) {
	good_flag = 0;
	break;
      }

//...... check range rate

      Rdotvec[0] = Vm[0]-Vs[0];
      Rdotvec[1] = Vm[1]-Vs[1];
      Rdotvec[2] = Vm[2]-Vs[2];
      Rdot = sqrt(  Rdotvec[0]*Rdotvec[0]
		  + Rdotvec[1]*Rdotvec[1]
		  + Rdotvec[2]*Rdotvec[2]  );

      if (Rdot < sensor_model->get_rdotmin()) {
	good_flag = 0;
	break;
      }

//...... check signal strength

      R2 = R*R;
      R4 = R2*R2;
      signal = eom->get_cross_section() / R4;
      if (signal < sensor_model->get_signal()) {
	good_flag = 0;
	break;
      }

      luminosity = eom->get_luminosity() / R2;
      if (luminosity < sensor_model->get_luminosity()) {
	good_flag = 0;
	break;
      }

//...... check line of sight

      if (sensor_model->get_los()) {
        if (!sensor_model->check_los(Ps,Pm)) {
	  good_flag = 0;
	  break;
        }
      }

//...... check mover has stopped

      if (eom->get_freeid() == STOP) {
	good_flag = 0;
	break;
      }

//...... break out of the while statement

      good_flag = 1;
      break;

    }

//...... put detection in a good or bad list

    if (good_flag) {
      good_movers.push_bot(oid);
    }else{
      bad_movers.push_bot(oid);
    }

//    oid = (C_OID *)oid->get_link();

  }

//...... generate detections for the command center

  sensor_model->set_random(RANDOM);
  sensor_model->set_rbq(rbq);

  sensobj->ecr_to_eci(TIME_TAG, Ps, Vs);
  sensor_model->generate_detections(TIME_TAG, Ps, Vs, &good_movers, &det);
  sensobj->eci_to_ecr(TIME_TAG, Ps, Vs);

  if (det.get_length()) {

//...... send to command center

    if (CCMESS) {

      len = det.get_length();
      size = len * sizeof(C_DETECTION);
      send_message_mess = (SEND_MESSAGE_MESS *)schedule(
	GONE*TIME_TAG,
	SEND_MESSAGE,
	COM,
	sensobj->get_com_local_id(),
	NODE,
	size,
	buff);

      sensobj->get_next_name(s);
      strcpy(send_message_mess->source, SIMOBJ->get_NAME());
      strcpy(send_message_mess->destination, "NORAD_COMMAND_CENTER");
      strcpy(send_message_mess->message_type, "DETECTIONS");
      send_message_mess->time = TIME_TAG;
      send_message_mess->priority = 3;
      send_message_mess->protocol = 0;
      send_message_mess->nbytes = size;

      detection = (C_DETECTION *)det.get_top();
      buff1 = buff;
      for (i=0; i<len; i++) {
        memcpy(buff1, (char *)detection, sizeof(C_DETECTION));
        buff1 += sizeof(C_DETECTION);
        detection = (C_DETECTION *)detection->get_link();
      }

    }

    len = det.get_length();
    for (i=0; i<len; i++) {
      detection = (C_DETECTION *)det.pop_top();
      freeobjs.delete_object(detection);
    }

  }

//...... perform the sensor scan

  old_tracks = sensobj->get_tracks();
  sensor_model->scan(TIME_TAG, Ps, Vs, &good_movers, old_tracks, new_tracks);

//...... send any new object tracks out to graphics

  size_link_output = 0;
  graphics(old_tracks, new_tracks);

//...... check statistics

  sensor_model->set_prox_statistics(
	bad_movers.get_length() + good_movers.get_length(),
	good_movers.get_length()	);

/*
  if ((sensor_model->get_n_scans() % 1000 == 0) && (NODE == 0)) {
    sprintf(s,"Prox ratio = %f\n",sensor_model->get_prox_statistics());
    RB_PRINT(stderr,s);
  }
*/

//...... delete the old tracks

  len = old_tracks->get_length();
  for (i=0; i<len; i++) {
    track = (C_TRACK *)old_tracks->get_top();
    *old_tracks -= track;
    RB_FREE_DELETE(track);
  }

//...... send the new tracks to the communications network

  send_message_mess = NULL;

  if ( (old_tracks->get_length() || new_tracks->get_length())
	&& CCMESS && ((TIME_TAG - last_tcom) > sensobj->get_com_rate())) {

    last_tcom = TIME_TAG;

    size = 0;
    len = new_tracks->get_length();
    track = (C_TRACK *)new_tracks->get_top();
    for (i=0; i<len; i++) {
      size += freeobjs.get_size(track);
      track = (C_TRACK *)track->get_link();
    }

    send_message_mess = (SEND_MESSAGE_MESS *)schedule(
	GONE*TIME_TAG,
	SEND_MESSAGE,
	COM,
	sensobj->get_com_local_id(),
	NODE,
	size,
	buff);

    strcpy(send_message_mess->source, SIMOBJ->get_NAME());
    strcpy(send_message_mess->destination, "NORAD_COMMAND_CENTER");
    strcpy(send_message_mess->message_type, "TRACKS");
    send_message_mess->time = TIME_TAG;
    send_message_mess->priority = 3;
    send_message_mess->protocol = 0;
    send_message_mess->nbytes = size;

    track = (C_TRACK *)new_tracks->get_top();
    for (i=0; i<len; i++) {
      size1 = freeobjs.get_size(track);
      buff1 = track->fill_buff(buff, size1);
      buff = buff1;
      track = (C_TRACK *)track->get_link();
    }

  }

  if (send_message_mess != NULL) {
    if (send_message_mess->bytes != send_message_mess->nbytes) {
      fprintf(stderr,"Error (SCAN) output message is bad\n");
    }  
  }

//...... restore the mover list

  movers->reset();
  movers->join(&bad_movers);
  movers->join(&good_movers);
  if (movers->get_length()) {
    movers->remove(top_mover);
    if (bot_mover != top_mover) movers->remove(bot_mover);
    movers->push_top(top_mover);
    if (bot_mover != top_mover) movers->push_bot(bot_mover);

    if (movers->get_length() != 
	(bad_movers.get_length()+good_movers.get_length())) {
      sprintf(s,"Error (SCAN) mover list corrupted\n");
      RB_PRINT(stderr,s);
    }
  }

//...... reschedule this event again

  if ((movers->get_length()) && (sensobj->get_on_off())) {
    scanning = 1;
    schedule (	TIME_TAG+sensobj->get_scan_time(),
		SCAN,
		OBJECT_TYPE,
		LOCAL_ID,
		NODE);
  }else{
    scanning = 0;
  }

  seed = RANDOM->get_seed();

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_SCAN::perm_process() {
  C_GRAPHMAN *graphman;
  C_GRAPHOBJ *graphobj;

  if (size_link_output == 0) return;

/*
  fprintf(stderr,"External Graphics script %d bytes at time %f\n",
	ext_graphics_script_output->data_bytes, TIME_TAG);
*/

  graphman = (C_GRAPHMAN *)get_manager(GRAPHICS);
  graphobj = (C_GRAPHOBJ *)graphman->get_obj(0);

  ext_graphics_link_output->EM_socket = graphobj->get_socket();

  HOST_USER->send_message(ext_graphics_link_output);
  delete ext_graphics_link_output;
  
}

/************************************************************************
* cleanup : clean up data structures used                               *
************************************************************************/
void C_SCAN::cleanup() {
  
}

