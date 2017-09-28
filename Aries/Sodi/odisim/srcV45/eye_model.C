// eye_model.C method file

#include <math.h>
#include <stdio.h>

#include "eye_model.H"
#include "detection.H"
#include "stereo_track.H"
#include "eom.H"
#include "oid.H"
#include "def_rbq.H"

#include "def.h"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;


//#define RANDOM_SENSOR
//#define RANDOM_RANGE
//#define ENABLE_RANGE_RATE
#define NOISY_DATA

#define TRUE_EOM
//#define LINEAR_EOM
//#define FIXED_EOM

#define NOISE_RR 0.001

/************************************************************************
* C_EYE_MODEL : construct an eye_model sensor				*
************************************************************************/
C_EYE_MODEL::C_EYE_MODEL() {

//  NOISE = 0.00175;	// 0.1 degree accuracy
//  NOISE = 0.000175;	// 0.01 degree accuracy
    NOISE = 0.000050;	// 50 microradians for scanning B.E. sensor
//  NOISE = 0.000020;	// 20 microradians for telescope B.E. sensor
//  NOISE = 0.000015;	// 15 microradians for DSP
//  NOISE = 0.000002;	// 2 microradians for FEWS

}

/************************************************************************
* generate_detections : generate a list of detections for this scan     *
************************************************************************/
void C_EYE_MODEL::generate_detections(	double time,
					double Psen[3],
					double Vsen[3],
					C_XQUEUE *movers,
					C_XQUEUE *detections) {

  int i,len;
  C_OID *oid;
  C_EOM *eom;
  int track_id;
  int ECI_flag;
  C_DETECTION *detection;
  double vhat[3];
  double temp;
  double P[3];
  double V[3];
  double xy[3];
  double xyz[3];
  double rvec[3];
  double random_angle;
  double rc, rs;
  double random_magnitude;
  double range;
  double range_rate;

  len = movers->get_length();
  oid = (C_OID *)movers->get_top();

  for (i=0; i<len; i++) {

//...... get the moving objects position and velocity

    track_id = oid->get_id();
    eom = (C_EOM *)oid->get_peom();
    ECI_flag = eom->get_ECI();
    eom->set_ECI(1);

#ifdef LINEAR_EOM
    eom->get_pos_vel(eom->get_start_time(),P,V);
    P[0] += V[0]*(time-eom->get_start_time());
    P[1] += V[1]*(time-eom->get_start_time());
    P[2] += V[2]*(time-eom->get_start_time());
#endif

#ifdef FIXED_EOM
    eom->get_pos_vel(eom->get_start_time(),P,V);
    V[0] = 0.0;
    V[1] = 0.0;
    V[2] = 0.0;
#endif

#ifdef TRUE_EOM
    eom->get_pos_vel(time,P,V);
#endif

//...... get the direction unit vector (perfect for now)

#ifdef RANDOM_SENSOR
    random->set_float_limits(-1.0,1.0);
    Psen[0] = P[0] + random->get_random_float();
    Psen[1] = P[1] + random->get_random_float();
    Psen[2] = P[2] + random->get_random_float();
#endif

    vhat[0] = P[0] - Psen[0];
    vhat[1] = P[1] - Psen[1];
    vhat[2] = P[2] - Psen[2];
    range = sqrt(vhat[0]*vhat[0]+vhat[1]*vhat[1]+vhat[2]*vhat[2]);
    vhat[0] /= range;
    vhat[1] /= range;
    vhat[2] /= range;

    range_rate =  (V[0]-Vsen[0])*vhat[0]
		+ (V[1]-Vsen[1])*vhat[1]
		+ (V[2]-Vsen[2])*vhat[2];

//...... add noise to the detection

#ifdef NOISY_DATA

    temp = vhat[0]*vhat[0] / (vhat[1]*vhat[1]);
    xy[0] = 1.0 / sqrt(1.0+temp);
    xy[1] = -xy[0]*vhat[0]/vhat[1];
    xy[2] = 0.0;
/*
    temp = xy[0]*xy[0]+xy[1]*xy[1];
    if (fabs(temp-1.0) > 1.0e-10) {
      fprintf(stderr,"Error (EYE_MODEL) bad xy vector\n");
    }
    temp = xy[0]*vhat[0] + xy[1]*vhat[1];
    if (fabs(temp) > 1.0e-10) {
      fprintf(stderr,"Error (EYE_MODEL) bad xy vector\n");
    }
*/
    xyz[0] = vhat[1]*xy[2] - vhat[2]*xy[1];
    xyz[1] = vhat[2]*xy[0] - vhat[0]*xy[2];
    xyz[2] = vhat[0]*xy[1] - vhat[1]*xy[0];
/*
    temp = xyz[0]*xyz[0]+xyz[1]*xyz[1]+xyz[2]*xyz[2];
    if (fabs(temp-1.0) > 1.0e-10) {
      fprintf(stderr,"Error (EYE_MODEL) bad xyz vector\n");
    }
    temp = xyz[0]*vhat[0] + xyz[1]*vhat[1] + xyz[2]*vhat[2];
    if (fabs(temp) > 1.0e-10) {
      fprintf(stderr,"Error (EYE_MODEL) bad xyz vector\n");
    }
    temp = xyz[0]*xy[0] + xyz[1]*xy[1] + xyz[2]*xy[2];
    if (fabs(temp) > 1.0e-10) {
      fprintf(stderr,"Error (EYE_MODEL) bad xyz vector\n");
    }
*/
    random->set_float_limits(0.0,TWOPI);
    random_angle = random->get_random_float();
    rc = cos(random_angle);
    rs = sin(random_angle);
    random->set_float_limits(0.0, NOISE);
    random_magnitude = random->get_random_float();

    rvec[0] = random_magnitude*(rc*xy[0] + rs*xyz[0]);
    rvec[1] = random_magnitude*(rc*xy[1] + rs*xyz[1]);
    rvec[2] = random_magnitude*(rc*xy[2] + rs*xyz[2]);

    vhat[0] += rvec[0];
    vhat[1] += rvec[1];
    vhat[2] += rvec[2];

    temp = sqrt(vhat[0]*vhat[0]+vhat[1]*vhat[1]+vhat[2]*vhat[2]);
    vhat[0] /= temp;
    vhat[1] /= temp;
    vhat[2] /= temp;
#endif

//...... create the detection and add it to the list

    detection = (C_DETECTION *)RB_FREE_NEW(DETECTION);
    detections->push_bot(detection);

//...... put range information into the detection if testing range

#ifdef RANDOM_RANGE

    if (random->get_random_bit()) {

      detection->set_detection(time, track_id, sensor_id, P, V, Psen, Vsen,
	vhat, range, range_rate, NOISE, eom->get_ballistic());
    }else{

      detection->set_detection(time, track_id, sensor_id, P, V, Psen, Vsen,
	vhat, 0.0, range_rate, NOISE, eom->get_ballistic());
    }

#else

    detection->set_detection(time, track_id, sensor_id, P, V, Psen, Vsen, vhat,
	0.0, range_rate, NOISE, eom->get_ballistic());

#endif

#ifdef ENABLE_RANGE_RATE
    detection->set_valid_range_rate();
    detection->set_Sigma_rr(NOISE_RR);
#endif

//...... set up the local track identifiers

    detection->set_object_type(eom->get_object_type());
    detection->set_object_id(eom->get_object_id());
    detection->set_object_node(eom->get_object_node());

//...... get the next one

    eom->set_ECI(ECI_flag);
    oid = (C_OID *)oid->get_link();

  }

}

/************************************************************************
* eye_scan : perform a sensor scan                                      *
************************************************************************/
void C_EYE_MODEL::eye_scan(	double time,
				double Psen[3],
				double Vsen[3],
				C_XQUEUE *movers,
				C_XQUEUE *tracks	) {

  int i,j,len,len1;
  C_OID *oid;
  C_EOM *eom;
  int track_id;
  int ECI_flag;
  C_XQUEUE *detections;
  C_DETECTION *detection;
  C_DETECTION *next_detection;
  C_STEREO_TRACK *stereo_track;
  C_STEREO_TRACK *next_stereo_track;
  double vhat[3];
  double temp;
  double P[3];
  double V[3];
  int flag;
  double window;
  double xy[3];
  double xyz[3];
  double rvec[3];
  double random_angle;
  double rc, rs;
  double random_magnitude;
  double Pe[3], Ve[3];
  double range;
  double range_rate;

//...... mark the current time for the tracks

  len = tracks->get_length();
  stereo_track = (C_STEREO_TRACK *)tracks->get_top();
  for (i=0; i<len; i++) {
    stereo_track->set_tcurrent(-1.0);
    stereo_track = (C_STEREO_TRACK *)stereo_track->get_link();
  }

  len = movers->get_length();
  oid = (C_OID *)movers->get_top();

  for (i=0; i<len; i++) {

//...... get the moving objects position and velocity

    track_id = oid->get_id();
    eom = (C_EOM *)oid->get_peom();
    ECI_flag = eom->get_ECI();
    eom->set_ECI(1);

#ifdef LINEAR_EOM
    eom->get_pos_vel(eom->get_start_time(),P,V);
    P[0] += V[0]*(time-eom->get_start_time());
    P[1] += V[1]*(time-eom->get_start_time());
    P[2] += V[2]*(time-eom->get_start_time());
#endif

#ifdef FIXED_EOM
    eom->get_pos_vel(eom->get_start_time(),P,V);
    V[0] = 0.0;
    V[1] = 0.0;
    V[2] = 0.0;
#endif

#ifdef TRUE_EOM
    eom->get_pos_vel(time,P,V);
#endif

//...... get the direction unit vector (perfect for now)

#ifdef RANDOM_SENSOR
    random->set_float_limits(-1.0,1.0);
    Psen[0] = P[0] + random->get_random_float();
    Psen[1] = P[1] + random->get_random_float();
    Psen[2] = P[2] + random->get_random_float();
#endif

    vhat[0] = P[0] - Psen[0];
    vhat[1] = P[1] - Psen[1];
    vhat[2] = P[2] - Psen[2];
    range = sqrt(vhat[0]*vhat[0]+vhat[1]*vhat[1]+vhat[2]*vhat[2]);
    vhat[0] /= range;
    vhat[1] /= range;
    vhat[2] /= range;

    range_rate =  (V[0]-Vsen[0])*vhat[0]
		+ (V[1]-Vsen[1])*vhat[1]
		+ (V[2]-Vsen[2])*vhat[2];

//...... add noise to the detection

#ifdef NOISY_DATA

    temp = vhat[0]*vhat[0] / (vhat[1]*vhat[1]);
    xy[0] = 1.0 / sqrt(1.0+temp);
    xy[1] = -xy[0]*vhat[0]/vhat[1];
    xy[2] = 0.0;
/*
    temp = xy[0]*xy[0]+xy[1]*xy[1];
    if (fabs(temp-1.0) > 1.0e-10) {
      fprintf(stderr,"Error (EYE_MODEL) bad xy vector\n");
    }
    temp = xy[0]*vhat[0] + xy[1]*vhat[1];
    if (fabs(temp) > 1.0e-10) {
      fprintf(stderr,"Error (EYE_MODEL) bad xy vector\n");
    }
*/
    xyz[0] = vhat[1]*xy[2] - vhat[2]*xy[1];
    xyz[1] = vhat[2]*xy[0] - vhat[0]*xy[2];
    xyz[2] = vhat[0]*xy[1] - vhat[1]*xy[0];
/*
    temp = xyz[0]*xyz[0]+xyz[1]*xyz[1]+xyz[2]*xyz[2];
    if (fabs(temp-1.0) > 1.0e-10) {
      fprintf(stderr,"Error (EYE_MODEL) bad xyz vector\n");
    }
    temp = xyz[0]*vhat[0] + xyz[1]*vhat[1] + xyz[2]*vhat[2];
    if (fabs(temp) > 1.0e-10) {
      fprintf(stderr,"Error (EYE_MODEL) bad xyz vector\n");
    }
    temp = xyz[0]*xy[0] + xyz[1]*xy[1] + xyz[2]*xy[2];
    if (fabs(temp) > 1.0e-10) {
      fprintf(stderr,"Error (EYE_MODEL) bad xyz vector\n");
    }
*/
    random->set_float_limits(0.0,TWOPI);
    random_angle = random->get_random_float();
    rc = cos(random_angle);
    rs = sin(random_angle);
    random->set_float_limits(0.0, NOISE);
    random_magnitude = random->get_random_float();

    rvec[0] = random_magnitude*(rc*xy[0] + rs*xyz[0]);
    rvec[1] = random_magnitude*(rc*xy[1] + rs*xyz[1]);
    rvec[2] = random_magnitude*(rc*xy[2] + rs*xyz[2]);

    vhat[0] += rvec[0];
    vhat[1] += rvec[1];
    vhat[2] += rvec[2];

    temp = sqrt(vhat[0]*vhat[0]+vhat[1]*vhat[1]+vhat[2]*vhat[2]);
    vhat[0] /= temp;
    vhat[1] /= temp;
    vhat[2] /= temp;
#endif

//...... create the detection

    detection = (C_DETECTION *)RB_FREE_NEW(DETECTION);

//...... put range information into the detection if testing range

#ifdef RANDOM_RANGE

    if (random->get_random_bit()) {

      detection->set_detection(time, track_id, sensor_id, P, V, Psen, Vsen,
	vhat, range, range_rate, NOISE, eom->get_ballistic());
    }else{

      detection->set_detection(time, track_id, sensor_id, P, V, Psen, Vsen,
	vhat, 0.0, range_rate, NOISE, eom->get_ballistic());
    }

#else

    detection->set_detection(time, track_id, sensor_id, P, V, Psen, Vsen, vhat,
	0.0, range_rate, NOISE, eom->get_ballistic());

#endif

#ifdef ENABLE_RANGE_RATE
    detection->set_valid_range_rate();
    detection->set_Sigma_rr(NOISE_RR);
#endif

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

      flag = 1;
      detection = (C_DETECTION *)detections->get_top();
      len1 = detections->get_length();

      for (j=0; j<len1; j++) {
        if (detection->get_time() < (time-window)) {
	  flag = 0;
	  break;
        }
        detection = (C_DETECTION *)detection->get_link();
      }

      if (flag) break;
      *detections -= detection;

    }

//...... update the state

    if (eom->get_ballistic()) stereo_track->update_state(print_flag);

//...... check track

    if ((len1 >= 6) && (eom->get_ballistic()) &&
	((time - eom->get_start_time()) > window) ) {

      stereo_track->get_pos_vel(time,Pe,Ve);

      fprintf(stderr,"Stereo Track %d: t = %f\n",
	track_id, time);

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

//...... get the next one

    eom->set_ECI(ECI_flag);
    oid = (C_OID *)oid->get_link();

  }

//...... remove tracks that dont have a current detection

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

}
