// dsp_model.C method file

#include <stdio.h>
#include <math.h>
#include "def.h"
#include "dsp_model.H"
#include "track3.H"
#include "oid.H"
#include "eom.H"
#include "def_rbq.H"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

#define ANGLE_NOISE 0.000050
#define SYSNOISE (0.1*G)

/************************************************************************
* C_DSP_MODEL : construct a dsp_model base class object			*
************************************************************************/
C_DSP_MODEL::C_DSP_MODEL() {

}

/************************************************************************
* scan : perform a sensor scan                                          *
************************************************************************/
void C_DSP_MODEL::scan(	double time,
				double Ps[3],
				double *,
				C_XQUEUE *movers,
				C_XQUEUE *old_tracks,
				C_XQUEUE *new_tracks	) {
  int i,j,len;
  C_TRACK3 *track3;
  C_TRACK3 *old_track3;
  C_OID *oid;
  C_EOM *eom;
  double P[3];
  double V[3];
  double R[3];
  double Rhat[3];
  double Range;
  double Pe[3];
  double Ve[3];
  int track_id;
  C_KALMAN1 *kalman1;
  double *state;
  double az, el;
  double temp1;
  double N[3];
  double Nproj, Zproj;
  double Nmeas, Zmeas;
  double noise;
  double Zest/*, Zerr*/;
  double Nest/*, Nerr*/;
  double Pt[3],Vt[3];
  double azest, elest;
  double P11;

  len = movers->get_length();
  oid = (C_OID *)movers->get_top();

  for (i=0; i<len; i++) {

    track_id = oid->get_id();
    eom = (C_EOM *)oid->get_peom();
    eom->get_pos_vel(time,P,V);

    for (j=0; j<3; j++) R[j] = P[j]-Ps[j];
    Range = sqrt(R[0]*R[0]+R[1]*R[1]+R[2]*R[2]);
    for (j=0; j<3; j++) Rhat[j] = R[j] / Range;

//......set up the temp scaler

    temp1 = -(RGEO*RGEO)/(Rhat[0]*Ps[0] + Rhat[1]*Ps[1] + Rhat[2]*Ps[2]);

//...... get the z projection

    Zproj = Rhat[2] * temp1;

    noise = ANGLE_NOISE*RGEO;
    random->set_float_limits(-noise,noise);
    Zmeas = Zproj + random->get_random_float();

//...... get the N projection

    N[0] = -Ps[1]/RGEO;
    N[1] = Ps[0]/RGEO;
    N[2] = 0.0;

    if (fabs(sqrt(N[0]*N[0]+N[1]*N[1]+N[2]*N[2]) - 1.0) > 0.000001) {
      fprintf(stderr,"Error (DSP_MODEL) bad N vector\n");
    }

    Nproj = (N[0]*Rhat[0] + N[1]*Rhat[1]) * temp1;
    Nmeas = Nproj + random->get_random_float();

//...... get the old track from last time

    track3 = (C_TRACK3 *)RB_FREE_NEW(TRACK3);

    track3->set_object_id(eom->get_object_id());
    track3->set_object_node(eom->get_object_node());
    track3->set_object_type(eom->get_object_type());

    kalman1 = track3->get_kalman1();

    old_track3 = (C_TRACK3 *)old_tracks->find(track_id);
    if (old_track3) {
      *track3 = *old_track3;
      track3->increment_cycle();
    }else{
      for (j=0; j<2; j++) {
        track3->set_id(track_id);
        track3->set_cycle(1);
	kalman1[j].reset_cycle();
	kalman1[j].set_DT(scan_time);
	kalman1[j].set_R(noise/3.0);
	kalman1[j].set_U(SYSNOISE);
      }
    }

//...... update the Kalman filters

    kalman1[0].update(Zmeas);
    kalman1[1].update(Nmeas);

    state = kalman1[0].get_state();
    Zest = state[0];
    //Zerr = kalman1[0].get_error();

    state = kalman1[1].get_state();
    Nest = state[1];
    //Nerr = kalman1[1].get_error();

    elest = atan(sqrt(Nest*Nest + Zest*Zest) / RGEO);
    azest = atan2(Zest,Nest) + PI;
    if (azest > TWOPI) azest -= TWOPI;

    Pe[0] = elest;
    Pe[1] = azest;
    Pe[2] = 0.0;

    Ve[0] = 0.0;
    Ve[1] = 0.0;
    Ve[2] = 0.0;

    track3->set_est_state(Pe,Ve);

//...... set the true values in the track3 object

    el = atan(sqrt(Nproj*Nproj + Zproj*Zproj) / RGEO);
    az = atan2(Zproj,Nproj) + PI;
    if (az > TWOPI) az -= TWOPI;

    Pt[0] = el;
    Pt[1] = az;
    Pt[2] = 0.0;

    Vt[0] = 0.0;
    Vt[1] = 0.0;
    Vt[2] = 0.0;
    track3->set_true_state(Pt,Vt);

    P11 = sqrt(kalman1[0].get_error() + kalman1[1].get_error()) / RGEO;
    track3->set_total_error(sqrt(P11));
    track3->set_error(	sqrt(kalman1[0].get_error()),
			sqrt(kalman1[1].get_error()),
			0.0	);

    track3->set_time_tag(time);
    track3->set_ballistic(eom->get_ballistic());

    *new_tracks += track3;

    oid = (C_OID *)oid->get_link();
  }

}

