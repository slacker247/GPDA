// ground_radar.C method file

#include <math.h>
#include <stdio.h>

#include "ground_radar.H"
#include "detection.H"
#include "track2.H"
#include "eom.H"
#include "oid.H"
#include "RB.H"

#include "def.h"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

#define NOISY_ANGLE
#define NOISY_RANGE
#define NOISY_RANGE_RATE

/************************************************************************
* C_GROUND_RADAR : construct a ground_radar base class object		*
************************************************************************/
C_GROUND_RADAR::C_GROUND_RADAR() {

  RNOISE = 0.015;
  RDOTNOISE = 0.01;
  AZNOISE = 0.0035;
  ELNOISE = 0.0035;
  SYSNOISE = (0.01*G);

}

/************************************************************************
* generate_detections : generate a list of detections for this scan     *
************************************************************************/
void C_GROUND_RADAR::generate_detections(	double time,
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
    eom->get_pos_vel(time,P,V);

//...... get the direction unit vector, range, and range rate (perfect for now)

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

//...... add noise to the angle measurement

#ifdef NOISY_ANGLE

    temp = vhat[0]*vhat[0] / (vhat[1]*vhat[1]);
    xy[0] = 1.0 / sqrt(1.0+temp);
    xy[1] = -xy[0]*vhat[0]/vhat[1];
    xy[2] = 0.0;

    xyz[0] = vhat[1]*xy[2] - vhat[2]*xy[1];
    xyz[1] = vhat[2]*xy[0] - vhat[0]*xy[2];
    xyz[2] = vhat[0]*xy[1] - vhat[1]*xy[0];

    random->set_float_limits(0.0,TWOPI);
    random_angle = random->get_random_float();
    rc = cos(random_angle);
    rs = sin(random_angle);
    random->set_float_limits(0.0, AZNOISE);
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

//...... add noise to the range measurement

#ifdef NOISY_RANGE

    random->set_float_limits(-RNOISE, RNOISE);
    range += random->get_random_float();

#endif

//...... add noise to the range rate measurement

#ifdef NOISY_RANGE_RATE

    random->set_float_limits(-RDOTNOISE, RDOTNOISE);
    range_rate += random->get_random_float();

#endif

//...... create the detection and add it to the list

    detection = (C_DETECTION *)freeobjs.new_object(DETECTION);
    detections->push_bot(detection);

    detection->set_detection(time, track_id, sensor_id, P, V, Psen, Vsen,
	vhat, range, range_rate, AZNOISE, eom->get_ballistic());

    detection->set_valid_range_rate();
    detection->set_Sigma_rr(RDOTNOISE);

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
* scan : perform a sensor scan                                          *
************************************************************************/
void C_GROUND_RADAR::scan(	double time,
				double Psen[3],
				double Vsen[3],
				C_XQUEUE *movers,
				C_XQUEUE *old_tracks,
				C_XQUEUE *new_tracks	) {
  int i,j,k,len;
  C_TRACK2 *track2;
  C_TRACK2 *old_track2;
  C_OID *oid;
  C_EOM *eom;
  double P[3], V[3];
  double Ps[3], Vs[3];
  double Pe[3], Ve[3];
  double R[3], Range, Range2, Range3, Range_Rate;
  //double Range_meas, Range_Rate_meas;
  double Nr[3], Naz[3], Nel[3];
  double Pmerr[3];
  int track_id;
  C_KALMAN1 *kalman1;
  C_KALMAN2 *kalman2;
  double Xmeas[3];
  //double Vmeas[3];
  double *state;
  double P11;
  double temp;
  int ECI_flag;
  double sysfactor;
  double L[3];

  Ps[0] = Psen[0];
  Ps[1] = Psen[1];
  Ps[2] = Psen[2];
  Vs[0] = Vsen[0];
  Vs[1] = Vsen[1];
  Vs[2] = Vsen[2];
  eom = NULL;
  eom->ecr_to_eci(time,Ps,Vs);

  len = movers->get_length();
  oid = (C_OID *)movers->get_top();

  for (i=0; i<len; i++) {

//...... get the moving objects position and velocity

    track_id = oid->get_id();
    eom = (C_EOM *)oid->get_peom();
    ECI_flag = eom->get_ECI();
    eom->set_ECI(1);
    eom->get_pos_vel(time,P,V);

    if (eom->get_ballistic()) {
//      fprintf(stderr,"Ballistic\n");
    }

//...... set up the new track object

    track2 = (C_TRACK2 *)RB_FREE_NEW(TRACK2);

    track2->set_object_id(eom->get_object_id());
    track2->set_object_node(eom->get_object_node());
    track2->set_object_type(eom->get_object_type());

    kalman1 = track2->get_kalman1();
    kalman2 = track2->get_kalman2();

    old_track2 = (C_TRACK2 *)old_tracks->find(track_id);
    if (old_track2) {
      *track2 = *old_track2;
      track2->increment_cycle();
      sysfactor = time - track2->get_time_tag();
    }else{
      sysfactor = 10.0;
      track2->set_id(track_id);
      track2->set_cycle(1);
      for (k=0; k<3; k++) {
        kalman1[k].reset_cycle();
        kalman1[k].set_DT(scan_time);
      }
      kalman2->reset_cycle();
      kalman2->set_DT(scan_time);

      kalman2->set_R(RNOISE*RNOISE/3.0, RDOTNOISE*RDOTNOISE/3.0);
//      kalman2->set_R(RNOISE*RNOISE/3.0);
      kalman2->set_U(SYSNOISE*sysfactor);
    }

//...... if ballistic, decrease the system noise factor

  if (eom->get_ballistic()) {

    if (time - eom->get_start_time() > scan_time) {
      sysfactor /= 10.0;
    }else{
      track2->set_cycle(1);
    }

    track2->get_est_state(Pe,Ve);
    Range2 = Pe[0]*Pe[0]+Pe[1]*Pe[1]+Pe[2]*Pe[2] + 0.000001;
    Range3 = Range2 * sqrt(Range2);
    L[0] = - GM * (Pe[0] / Range3);
    L[1] = - GM * (Pe[1] / Range3);
    L[2] = - GM * (Pe[2] / Range3);
  }

//...... get range and range Rate

    for (j=0; j<3; j++) R[j] = P[j]-Ps[j];
    Range = sqrt(R[0]*R[0]+R[1]*R[1]+R[2]*R[2]);

    Range_Rate = 0;
    for (j=0; j<3; j++) Range_Rate += R[j]*V[j];
    Range_Rate /= Range;

//...... get the normalized range vector

    for (j=0; j<3; j++) Nr[j] = R[j] / Range;

//...... get the normalized azimuth vector = Ps x P / |Ps x P|

    Naz[0] = Ps[1]*P[2] - Ps[2]*P[1];
    Naz[1] = Ps[2]*P[0] - Ps[0]*P[2];
    Naz[2] = Ps[0]*P[1] - Ps[1]*P[0];
    temp = sqrt(Naz[0]*Naz[0]+Naz[1]*Naz[1]+Naz[2]*Naz[2]);
    for (j=0; j<3;j++) Naz[j] /= temp;

//...... get the normalized elevation vector = Nr x Naz

    Nel[0] = Nr[1]*Naz[2] - Nr[2]*Naz[1];
    Nel[1] = Nr[2]*Naz[0] - Nr[0]*Naz[2];
    Nel[2] = Nr[0]*Naz[1] - Nr[1]*Naz[0];

//...... get the estimated measured position error

    for (j=0; j<3; j++) {
      Pmerr[j] = sqrt(	  RNOISE*Nr[j]*RNOISE*Nr[j]
			+ Range*AZNOISE*Naz[j]*Range*AZNOISE*Naz[j]
			+ Range*ELNOISE*Nel[j]*Range*ELNOISE*Nel[j]	);
    }

//...... construct the measurement

    for (j=0; j<3; j++) {
      random->set_float_limits(-Pmerr[j], Pmerr[j]);
      Xmeas[j] = P[j] + random->get_random_float();
    }

    //for (j=0; j<3; j++) {
    //  Vmeas[j] = 0.0;
    //}

//...... do the kalman filter on the position

    P11 = 0.0;
    for (j=0; j<3; j++) {
      if (eom->get_ballistic()) {
        kalman1[j].set_gravity();
        kalman1[j].set_L2(L[j]);
      }else{
        kalman1[j].reset_gravity();
      }
      kalman1[j].set_R(Pmerr[j]*Pmerr[j]/3.0);
      kalman1[j].set_U(SYSNOISE*sysfactor);
      kalman1[j].update(Xmeas[j]);
      state = kalman1[j].get_state();
      Pe[j] = state[0];
      Ve[j] = state[1];
      P11 += kalman1[j].get_error();
    }

    track2->set_est_state(Pe,Ve);
    track2->set_true_state(P,V);
    track2->set_total_error(sqrt(P11));
    track2->set_error(	sqrt(kalman1[0].get_error()),
			sqrt(kalman1[1].get_error()),
			sqrt(kalman1[2].get_error()) );

//...... run the range filter using range and range Rate information

    //Range_meas = sqrt(	 (Xmeas[0]-Ps[0]) * (Xmeas[0]-Ps[0])
    //			+(Xmeas[1]-Ps[1]) * (Xmeas[1]-Ps[1])
    //			+(Xmeas[2]-Ps[2]) * (Xmeas[2]-Ps[2])	);

    random->set_float_limits(-RDOTNOISE, RDOTNOISE);
    //Range_Rate_meas = Range_Rate + random->get_random_float();
/*
    kalman2->update(Range_meas, Range_Rate_meas);
    state = kalman2->get_state();
    track2->set_range(state[0]);
    track2->set_range_rate(state[1]);
*/
    track2->set_time_tag(time);
    track2->set_ballistic(eom->get_ballistic());

    *new_tracks += track2;

    eom->set_ECI(ECI_flag);
    oid = (C_OID *)oid->get_link();

  }



}

