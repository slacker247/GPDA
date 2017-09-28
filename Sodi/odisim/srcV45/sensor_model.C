// sensor_model.C method file

#include <stdio.h>
#include <math.h>
#include "def.h"
#include "sensor_model.H"
#include "track1.H"
#include "oid.H"
#include "eom.H"
#include "detection.H"
#include "stereo_track.H"
#include "def_rbq.H"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

#define XNOISE 1.000
#define VNOISE 0.100
#define SYSNOISE (0.100*G)

int C_SENSOR_MODEL::n_movers = 0;
int C_SENSOR_MODEL::n_prox = 0;
int C_SENSOR_MODEL::n_scans = 0;
C_RBQ *C_SENSOR_MODEL::rbq = NULL;

/************************************************************************
* C_SENSOR_MODEL : construct a sensor_model base class object		*
************************************************************************/
C_SENSOR_MODEL::C_SENSOR_MODEL() {
  sensor_id = -1;
  track_flag = 1;
  fuse_flag = 0;  
  print_flag = 0;  
}

/************************************************************************
* init_sensor : initialize the sensor_model object			*
************************************************************************/
void C_SENSOR_MODEL::init(C_BASETYPE *sensor) {

  scan_time = sensor->get_float("scan_time");
  rmin = sensor->get_float("rmin");
  rmax = sensor->get_float("rmax");
  rmaxcoverage = rmax;
  rdotmin = sensor->get_float("rdotmin");
  signal = sensor->get_float("signal");
  luminosity = sensor->get_float("luminosity");
  fixed = sensor->get_logical("fixed");
  los = sensor->get_logical("los");
  error = sensor->get_float("error");
  icon = sensor->get_int("icon");
  type = sensor->get_name();

}

/************************************************************************
* check_los : check if unit is in range                                *
************************************************************************/
int C_SENSOR_MODEL::check_los(double Xs[3], double Xt[3]) {
  double Rs,Rt;
  double P[3];
  double d0,d1,d2;
  double A,B,C,D;
  double t;

  Rs = sqrt(Xs[0]*Xs[0] + Xs[1]*Xs[1] + Xs[2]*Xs[2]);
  if (Rs < RE+8.0) {
    D = 0.0;
  }else{
    D = 8.0;
  }

  Rt = sqrt(Xt[0]*Xt[0] + Xt[1]*Xt[1] + Xt[2]*Xt[2]);
  if (Rt < RE+D) return 0;

  d0 = Xt[0] - Xs[0];
  d1 = Xt[1] - Xs[1];
  d2 = Xt[2] - Xs[2];

  A = (d0 * Xs[0]) + (d1 * Xs[1]) + (d2 * Xs[2]);
  B = (d0 * d0) + (d1 * d1) + (d2 * d2);
  t = -A/B;

  if (t < 0) {
    return 1;
  }

  if (t > 1) {
    return 1;
  }

  P[0] = Xs[0] + d0*t;
  P[1] = Xs[1] + d1*t;
  P[2] = Xs[2] + d2*t;

  C = P[0]*P[0]+P[1]*P[1]+P[2]*P[2] - (RE+D)*(RE+D);
  if (C < 0) return 0;

  return 1;

}

/************************************************************************
* eye_scan : perform a sensor scan                                      *
************************************************************************/
void C_SENSOR_MODEL::eye_scan(	double ,
				double [3],
				double [3],
				C_XQUEUE *,
				C_XQUEUE *	) {

}


/************************************************************************
* generate_detections : generate the detections for a sensor            *
************************************************************************/
void C_SENSOR_MODEL::generate_detections(	double ,
						double [3],
						double [3],
						C_XQUEUE *,
						C_XQUEUE *) {

}

/************************************************************************
* update_tracks : update the tracks for a sensor                        *
************************************************************************/
void C_SENSOR_MODEL::update_tracks(	double time,
					C_XQUEUE *det,
					C_XQUEUE *tracks) {
  int i,j,len,len1;
  int track_id;
  C_XQUEUE *detections;
  C_DETECTION *detection;
  C_DETECTION *dummy_detection;
  C_DETECTION *next_detection;
  C_STEREO_TRACK *stereo_track;
  C_STEREO_TRACK *next_stereo_track;
  double *P;
  double *V;
  int flag;
  double window;
  double Pe[3], Ve[3];
  double det_time;
  int all_ballistic;
  int n_mine;
  int n_others;
  double temp;

//...... mark the current time for the tracks

  len = tracks->get_length();
  stereo_track = (C_STEREO_TRACK *)tracks->get_top();
  for (i=0; i<len; i++) {
    stereo_track->set_tcurrent(-1.0);
    stereo_track = (C_STEREO_TRACK *)stereo_track->get_link();
  }

  len = det->get_length();
  for (i=0; i<len; i++) {

    detection = (C_DETECTION *)det->pop_top();
    track_id = detection->get_id();

//...... add the detection to the stereo track

    stereo_track = (C_STEREO_TRACK *)tracks->find(track_id);
    if (!stereo_track) {
      stereo_track = (C_STEREO_TRACK *)RB_FREE_NEW(STEREO_TRACK);
      stereo_track->reset();
      stereo_track->set_id(track_id);
      *tracks += stereo_track;
    }
    stereo_track->set_tcurrent(time);

    detections = stereo_track->get_detections();
    *detections += detection;

//...... clean up the detection list

    window = stereo_track->get_window();

    while (1) {

      all_ballistic = 1;
      n_mine = 0;
      n_others = 0;

      flag = 1;
      dummy_detection = (C_DETECTION *)detections->get_top();
      len1 = detections->get_length();

      for (j=0; j<len1; j++) {
        if (dummy_detection->get_time() < (time-window)) {
	  flag = 0;
	  break;
        }
        if (!dummy_detection->get_ballistic()) all_ballistic = 0;
        if (dummy_detection->get_sensor_id() == sensor_id) {
	  n_mine++;
	}else{
	  n_others++;
	}
        dummy_detection = (C_DETECTION *)dummy_detection->get_link();
      }

      if (flag) break;
      *detections -= dummy_detection;

    }

//...... update the state

    temp = fabs(double(n_mine-n_others) / double(n_mine+n_others));

    if (	   (all_ballistic || 1)
		&& ((temp < 0.80) || (fuse_flag))
		&& (track_flag)
							) {

      stereo_track->update_state(print_flag);

//...... check track

      if (print_flag) {

        det_time = detection->get_time_tag();
        stereo_track->get_pos_vel(det_time,Pe,Ve);
        P = detection->get_Xtrue();
        V = detection->get_Vtrue();

        fprintf(stderr,"Stereo Track %d: t = %f\n",
	  track_id, det_time);

        fprintf(stderr,"Estim: X = %f %f %f, V = %f %f %f\n",
	  Pe[0], Pe[1], Pe[2], Ve[0], Ve[1], Ve[2]);

        fprintf(stderr,"Truth: X = %f %f %f, V = %f %f %f\n",
	  P[0], P[1], P[2], V[0], V[1], V[2]);

        fprintf(stderr,"Error: X = %f %f %f, V = %f %f %f\n",
	  Pe[0]-P[0], Pe[1]-P[1], Pe[2]-P[2],
	  Ve[0]-V[0], Ve[1]-V[1], Ve[2]-V[2]);

        fprintf(stderr,"True Range = %f, Stereo Range = %f\n",
	  sqrt(P[0]*P[0]+P[1]*P[1]+P[2]*P[2]), 
	  sqrt(Pe[0]*Pe[0]+Pe[1]*Pe[1]+Pe[2]*Pe[2]));

        fprintf(stderr,"True Velocity = %f, Stereo Velocity = %f\n\n",
	  sqrt(V[0]*V[0]+V[1]*V[1]+V[2]*V[2]), 
	  sqrt(Ve[0]*Ve[0]+Ve[1]*Ve[1]+Ve[2]*Ve[2]));

      }

    }

  }

//...... remove tracks that dont have a current detection
/*
  len = tracks->get_length();
  stereo_track = (C_STEREO_TRACK *)tracks->get_top();
  for (i=0; i<len; i++) {

    next_stereo_track = (C_STEREO_TRACK *)stereo_track->get_link();

    if (stereo_track->get_tcurrent() == -1.0) {

      detections = stereo_track->get_detections();
      len1 = detections->get_length();
      detection = (C_DETECTION *)detections->get_top();

      for (j=0; j<len1; j++) {
	next_detection = (C_DETECTION *)detection->get_link();
	RB_FREE_DELETE(detection);
	detection = next_detection;
      }

      *tracks -= stereo_track;
      RB_FREE_DELETE(stereo_track);

    }

    stereo_track = next_stereo_track;

  }
*/

//...... remove stale tracks

  len = tracks->get_length();
  stereo_track = (C_STEREO_TRACK *)tracks->get_top();
  for (i=0; i<len; i++) {

    next_stereo_track = (C_STEREO_TRACK *)stereo_track->get_link();

    detections = stereo_track->get_detections();
    detection = (C_DETECTION *)detections->get_top();
    len1 = detections->get_length();
    det_time = -1.0e20;

    for (j=0; j<len1; j++) {
      if (det_time < detection->get_time_tag())
	det_time = detection->get_time_tag();
      detection = (C_DETECTION *)detection->get_link();
    }

    if (time - det_time > 30.0) {
      detection = (C_DETECTION *)detections->get_top();
      for (j=0; j<len1; j++) {
        next_detection = (C_DETECTION *)detection->get_link();
        RB_FREE_DELETE(detection);
        detection = next_detection;
      }
      *tracks -= stereo_track;
      RB_FREE_DELETE(stereo_track);
    }

    stereo_track = next_stereo_track;

  }

}

/************************************************************************
* scan : perform a sensor scan                                          *
************************************************************************/
void C_SENSOR_MODEL::scan(	double time,
				double *,
				double *,
				C_XQUEUE *movers,
				C_XQUEUE *old_tracks,
				C_XQUEUE *new_tracks	) {
  int i,j,len;
  C_TRACK1 *track1;
  C_TRACK1 *old_track1;
  C_OID *oid;
  C_EOM *eom;
  double P[3];
  double V[3];
  double Pe[3];
  double Ve[3];
  int track_id;
  C_KALMAN2 *kalman2;
  double Xmeas[3];
  double Vmeas[3];
  double *state;
  double P11;
  double sysfactor;
  double L[3];
  double Range2, Range3;

  len = movers->get_length();
  oid = (C_OID *)movers->get_top();

  for (i=0; i<len; i++) {

    random->set_float_limits(-XNOISE, XNOISE);
    for (j=0; j<3; j++) {
      Xmeas[j] = random->get_random_float();
    }

    random->set_float_limits(-VNOISE, VNOISE);
    for (j=0; j<3; j++) {
      Vmeas[j] = random->get_random_float();
    }

    track_id = oid->get_id();
    eom = (C_EOM *)oid->get_peom();
    eom->get_pos_vel(time,P,V);

    for (j=0; j<3; j++) {
      Xmeas[j] += P[j];
      Vmeas[j] += V[j];
    }

    track1 = (C_TRACK1 *)RB_FREE_NEW(TRACK1);

    track1->set_object_id(eom->get_object_id());
    track1->set_object_node(eom->get_object_node());
    track1->set_object_type(eom->get_object_type());

    kalman2 = track1->get_kalman2();

    old_track1 = (C_TRACK1 *)old_tracks->find(track_id);
    if (old_track1) {
      *track1 = *old_track1;
      track1->increment_cycle();
      sysfactor = time - track1->get_time_tag();
    }else{
      sysfactor = 10.0;
      for (j=0; j<3; j++) {
        track1->set_id(track_id);
        track1->set_cycle(1);
	kalman2[j].reset_cycle();
	kalman2[j].set_DT(scan_time);
	kalman2[j].set_R(XNOISE*XNOISE/3.0,VNOISE*VNOISE/3.0);
	kalman2[j].set_U(SYSNOISE*sysfactor);
      }
    }

//...... if ballistic, decrease the system noise factor

    if (eom->get_ballistic()) {

      if (time - eom->get_start_time() > scan_time) {
        sysfactor /= 10.0;
      }else{
        track1->set_cycle(1);
      }

      track1->get_est_state(Pe,Ve);
      Range2 = Pe[0]*Pe[0]+Pe[1]*Pe[1]+Pe[2]*Pe[2];
      Range3 = Range2 * sqrt(Range2);
      L[0] = - GM * (Pe[0] / Range3);
      L[1] = - GM * (Pe[1] / Range3);
      L[2] = - GM * (Pe[2] / Range3);
    }

//...... update the kalman filter

    P11 = 0.0;
    for (j=0; j<3; j++) {

      if (eom->get_ballistic()) {
        kalman2[j].set_gravity();
        kalman2[j].set_L2(L[j]);
      }else{
        kalman2[j].reset_gravity();
      }

      kalman2[j].update(Xmeas[j],Vmeas[j]);
      state = kalman2[j].get_state();
      Pe[j] = state[0];
      Ve[j] = state[1];
      P11 += kalman2[j].get_error();
    }

    track1->set_est_state(Pe,Ve);
    track1->set_true_state(P,V);
    track1->set_total_error(sqrt(P11));
    track1->set_error(	sqrt(kalman2[0].get_error()),
			sqrt(kalman2[1].get_error()),
			sqrt(kalman2[2].get_error()) );

//    if (track1->get_cycle() >= 10) track1->print();
//    if (track_id == 2946) track1->print();

    track1->set_time_tag(time);
    track1->set_ballistic(eom->get_ballistic());

    *new_tracks += track1;

    oid = (C_OID *)oid->get_link();
  }

}

