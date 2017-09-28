// scan_eye.C method file

#include <strstream.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include "scan_eye.H"
#include "scan_eye_mess.H"
#include "send_message_mess.H"
#include "sensobj.H"
#include "graphman.H"
#include "eom.H"
#include "oid.H"
#include "track.H"
#include "detection.H"
#include "def.h"
#include "host_user.H"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

extern int COM;
extern int DSP;
extern int GRAPHICS;
extern int SCAN_EYE;
extern int SEND_MESSAGE;

//#define RANDOM_RING

static int CCMESS = 1;

/************************************************************************
* C_SCAN_EYE : construct a scan_eye object				*
************************************************************************/
C_SCAN_EYE::C_SCAN_EYE() {}

/************************************************************************
* exchange : exchange state variables					*
************************************************************************/
void C_SCAN_EYE::exchange() {
  C_SENSOBJ *sensobj;

  sensobj = (C_SENSOBJ *)SIMOBJ;
  sensobj->EXCHANGE_SEED(seed);
  sensobj->exchange_scanning(scanning);

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_SCAN_EYE::temp_process() {
  C_SENSOBJ *sensobj;
  C_SENSOR_MODEL *sensor_model;
  C_XQUEUE *movers;
  C_XQUEUE good_movers;
  C_XQUEUE bad_movers;
  C_XQUEUE det;
  C_XQUEUE *tracks;
  C_SQ_ITEM *track;
  C_XQUEUE *links;
  C_SQ_ITEM *link_temp;
  C_XQUEUE links1;
  C_SQ_ITEM *link1;
  C_OID *oid;
  C_EOM *eom;
  int len;
  int i;
  int good_flag;
  double Ps[3], Vs[3];
  double Pm[3], Vm[3];
  double Rvec[3], Rdotvec[3];
  double R, Rdot;
  double alt;
  double signal;
  double luminosity;
  double R2, R4;
  char s[120];
  C_SQ_ITEM *top_mover;
  C_SQ_ITEM *bot_mover;
  SEND_MESSAGE_MESS *send_message_mess;
  char *buff;
  char *buff1;
  C_DETECTION *detection;
  int size;
  //int ring;
  //int id_in_ring;
  //int n_per_ring;

//...... get the sensor object

  sensobj = (C_SENSOBJ *)SIMOBJ;
  sensor_model = sensobj->get_sensor_model();

  seed = sensobj->get_SEED();
  RANDOM->set_seed(seed);

  sensobj->get_pos_vel(TIME_TAG,Ps,Vs);


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

    if (eom->get_endtime() < TIME_TAG) {
      ostrstream os;
      os << "Error (SCAN_EYE) eom " << eom->get_id()
	 << " at time " << TIME_TAG << " is out of range\n" << ends;
      RB_PRINT(stderr,os.str());
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

//...... check luminosity

      luminosity = eom->get_luminosity() / R2;
      if (luminosity < sensor_model->get_luminosity()) {
	good_flag = 0;
	break;
      }

//...... check if ballistic

      if (OBJECT_TYPE == DSP) {
        if (eom->get_ballistic()) {
	  good_flag = 0;
	  break;
	}
      }

//...... check line of sight

      if (sensor_model->get_los()) {
        if (!sensor_model->check_los(Ps,Pm)) {
	  good_flag = 0;
	  break;
        }
      }

//...... check if out of atmosphere

      alt = sqrt(Pm[0]*Pm[0]+Pm[1]*Pm[1]+Pm[2]*Pm[2]) - RE;
      if (alt < (8.0)) {
	good_flag = 0;
	break;
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

//...... initialize the sensor model

  tracks = sensobj->get_tracks();
  links = sensobj->get_links();

  sensor_model->set_random(RANDOM);
  sensor_model->set_rbq(rbq);

//...... get the ECI position of the sensor

#ifdef RANDOM_RING
  random->set_float_limits(-100.0, 100.0);
  sensobj->get_pos_vel(TIME_TAG+random->get_random_float(),Ps,Vs);
//  if (random->get_random_bit()) {
//    sensobj->get_pos_vel(TIME_TAG+100.0,Ps,Vs);
//  }else{
//    sensobj->get_pos_vel(TIME_TAG,Ps,Vs);
//  }
#endif
  sensobj->ecr_to_eci(TIME_TAG, Ps, Vs);

//...... perform the sensor scan_eye

//  sensor_model->eye_scan(TIME_TAG, Ps, Vs, &good_movers, tracks);

//...... generate the detections and send them out

    sensor_model->generate_detections(TIME_TAG, Ps, Vs, &good_movers, &det);

    if (det.get_length()) {

//...... send to next B.E. in ring

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

      //ring = sensobj->get_ring();
      //id_in_ring = sensobj->get_id_in_ring();
      //n_per_ring = sensobj->get_n_per_ring();

      sensobj->get_next_name(s);
/*
      sprintf(s,"BRILLIANT_EYE_%3.3d_%3.3d\n",
	ring, (id_in_ring+1) % n_per_ring);
*/
      strcpy(send_message_mess->source, SIMOBJ->get_NAME());
      strcpy(send_message_mess->destination, s);
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

//...... send to previous B.E. in ring

      send_message_mess = (SEND_MESSAGE_MESS *)schedule(
	GONE*TIME_TAG,
	SEND_MESSAGE,
	COM,
	sensobj->get_com_local_id(),
	NODE,
	size,
	buff);

      sensobj->get_previous_name(s);

/*
      sprintf(s,"BRILLIANT_EYE_%3.3d_%3.3d\n",
	ring, (id_in_ring-1+n_per_ring) % n_per_ring);
*/
      strcpy(send_message_mess->source, SIMOBJ->get_NAME());
      strcpy(send_message_mess->destination, s);
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

//...... send to command center

      if (CCMESS) {

        send_message_mess = (SEND_MESSAGE_MESS *)schedule(
	  GONE*TIME_TAG,
	  SEND_MESSAGE,
	  COM,
	  sensobj->get_com_local_id(),
	  NODE,
	  size,
	  buff);

        sensobj->get_previous_name(s);

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

    }

//...... send any new object detections out to graphics

  size_link_output = 0;
  graphics(links, &det);

//...... compute the new links

  link_temp = (C_SQ_ITEM *)links->get_top();
  if (link_temp != NULL) RB_DELETE_ARRAY_C_SQ_ITEM(link_temp);

  link1 = RB_NEW_ARRAY_C_SQ_ITEM(det.get_length());

  track = (C_SQ_ITEM *)det.get_top();
  for (i=0; i<det.get_length(); i++) {
    link1[i].set_id(track->get_id());
    links1.push_bot(&link1[i]);
    track = (C_SQ_ITEM *)track->get_link();
  }
  RB_MEMCPY((char *)links, (char *)&links1, sizeof(C_XQUEUE));

//...... update the tracks

  sensor_model->update_tracks(TIME_TAG, &det, tracks);

//...... check statistics

  sensor_model->set_prox_statistics(
	bad_movers.get_length() + good_movers.get_length(),
	good_movers.get_length()	);

/*
  if ((sensor_model->get_n_scan_eyes() % 1000 == 0) && (NODE == 0)) {
    sprintf(s,"Prox ratio = %f\n",sensor_model->get_prox_statistics());
    RB_PRINT(stderr,s);
  }
*/

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
      sprintf(s,"Error (SCAN_EYE) mover list corrupted\n");
      RB_PRINT(stderr,s);
    }
  }

//...... reschedule this event again

  if ((movers->get_length()) && (sensobj->get_on_off())) {
    scanning = 1;
    schedule (	TIME_TAG+sensobj->get_scan_time(),
		SCAN_EYE,
		OBJECT_TYPE,
		LOCAL_ID,
		NODE	);
  }else{
    scanning = 0;
  }

  seed = RANDOM->get_seed();

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_SCAN_EYE::perm_process() {
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
void C_SCAN_EYE::cleanup() {
  
}

