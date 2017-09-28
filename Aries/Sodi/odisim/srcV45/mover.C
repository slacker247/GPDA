// mover.C method file

#include <stdio.h>
#include <math.h>
#include "def.h"
#include "mover.H"
#include "greatcirc.H"
#include "oid.H"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

int C_MOVER::done = 0;
int C_MOVER::magnet = 0;
int C_MOVER::size_node_flag = 0;

/************************************************************************
* C_MOVER : construct a mover object					*
************************************************************************/
C_MOVER::C_MOVER() {
  int i;

  if (!done) {
    done = 1;
    magnet = 0;
    size_node_flag = N_NODES/sizeof(int);
    if (N_NODES % sizeof(int)) size_node_flag++;
  }

  rancourse = 0;
  fixed_script = 1;
  script_time = -1.0e20;

  script = new C_XQUEUE();

//  gridid = NULL;

  tsensors = -INFINITY;

  node_flag = new int[size_node_flag];
  for (i=0; i<size_node_flag; i++) node_flag[i] = 0;

}

/************************************************************************
* random_init : randomly initialize the mover object			*
************************************************************************/
void C_MOVER::random_init() {
  int i;
  double R[3];
  double *P;
  double tt;
  C_EOM *e;
  int sequence;

  sequence = 0;
  RANDOM->set_seed(SEED);

  //random_position(start_position);
  R[0] = start_position[0];
  R[1] = start_position[1];
  R[2] = start_position[2];
  tt = start_time;

  for (i=0; i<3; i++) {
    e = (C_EOM *)freeobjs.new_object(GREATCIRC);
    fill_random_segment(tt,R,e);
    e->set_sequence(sequence);
    script->push_bot(e);
    sequence++;
    tt = e->get_endtime();
    P = e->get_endpos();
    R[0] = P[0];
    R[1] = P[1];
    R[2] = P[2];
  }

  SEED = RANDOM->get_seed();

}

/************************************************************************
* fill_random_segment : fill a random segment				*
************************************************************************/
void C_MOVER::fill_random_segment(double t, double R[3], C_EOM *e) {
  double R1[3];

//...... generate the next random potition for great circle trajectories

  random_next_position(R,R1);
  ((C_GREATCIRC *)e)->set_course(R, R1, cruise_vel, t);

//...... fill stuff for generic eom

  e->set_object_type(OBJECT_TYPE);
  e->set_object_id(LOCAL_ID);
  e->set_object_node(NODE);
  e->set_id(GLOBAL_ID);
  e->set_icon(icon);

  e->set_cross_section(cross_section);
  e->set_luminosity(luminosity);

}

/************************************************************************
* get_current_segment : get the current segment for this mover		*
************************************************************************/
C_EOM *C_MOVER::get_current_segment() {

  return (C_EOM *)script->get_top();

}

/************************************************************************
* get_next_segment : get the next segment for this mover		*
************************************************************************/
C_EOM *C_MOVER::get_next_segment() {
  C_EOM *e;

  if (script->get_length() <= 1) return NULL;

  e = (C_EOM *)script->get_top();
  return (C_EOM *)e->get_link();

}

/************************************************************************
* get_last_segment : get the last segment for this mover		*
************************************************************************/
C_EOM *C_MOVER::get_last_segment() {

  return (C_EOM *)script->get_bot();

}

/************************************************************************
* get_eoman_segment : get the segment for the eoman of this mover	*
************************************************************************/
C_EOM *C_MOVER::get_eoman_segment(double t, double &d) {
  C_EOM *e;

  e = get_current_segment();

  if (t+d >= e->get_endtime()) {
//    d = 0.9 * (e->get_endtime() - t);
    e = get_next_segment();
    if (e != NULL) {
      d = e->get_start_time() - t;
    }

  }

  return e;

}

/************************************************************************
* get_pos_vel : get the position and velocity of the moving object	*
************************************************************************/
void C_MOVER::get_pos_vel(double t, double X[3], double V[3]) {
  C_EOM *e;
  double *P;

  e = get_current_segment();

  if (e == NULL) {
    C_LOCOBJ::get_pos_vel(t,X,V);
    return;
  }

  while (1) {

    if (check_segment(e,t)) {

      e->get_pos_vel(t, X, V);
      return;

    }else{

      if (e == get_last_segment()) {

        P = e->get_endpos();
        X[0] = P[0];
        X[1] = P[1];
        X[2] = P[2];
        V[0] = 0.0;
        V[1] = 0.0;
        V[2] = 0.0;
	return;

      }else{

        e = (C_EOM *)e->get_link();

      }

    }


  }

}

/************************************************************************
* check_segment : check if good current eom				*
************************************************************************/
int C_MOVER::check_segment(C_EOM *e, double t) {

  if (e == NULL) return 0;
  if (t > e->get_endtime()) return 0;
  if ( t < e->get_start_time()) return 0;

  return 1;

}

/************************************************************************
* get_tdist : get the minimum time for traveling a certain distance	*
************************************************************************/
double C_MOVER::get_tdist(double dist) {

  return (dist/max_vel);

}

/************************************************************************
* get_node_flag : get the node flag (on or off)				*
************************************************************************/
int C_MOVER::get_node_flag(int n, int *nflg) {
  int index;
  int bit;
  int mask;

  index = n / sizeof(int);
  bit = n % sizeof(int);

  mask = 1;
  mask <<= bit;

  return (nflg[index]&mask);

}

/************************************************************************
* set_node_flag : set the node flag to on				*
************************************************************************/
void C_MOVER::set_node_flag(int n, int *nflg) {
  int index;
  int bit;
  int mask;

  index = n / sizeof(int);
  bit = n % sizeof(int);

  mask = 1;
  mask <<= bit;

  nflg[index]|= mask;

}

/************************************************************************
* reset_node_flag : reset the node flag to off				*
************************************************************************/
void C_MOVER::reset_node_flag(int n, int *nflg) {
  int index;
  int bit;
  int mask;

  index = n / sizeof(int);
  bit = n % sizeof(int);

  mask = 1;
  mask <<= bit;

  nflg[index] &= (~mask);

/*
  if (unique_id == 2387) {
    fprintf(stderr,"mover 2387 resetting node flag\n");
  }
*/

}

/************************************************************************
* reset_node_flag : reset the entire node flag 				*
************************************************************************/
void C_MOVER::reset_node_flag(int *nflg) {
  int i;

  for (i=0; i<size_node_flag; i++) {
    nflg[i] = 0;
  }

}

/************************************************************************
* copy_node_flag : copy the node flag 					*
************************************************************************/
void C_MOVER::copy_node_flag(int *n) {
  int i;

  for (i=0; i<size_node_flag; i++) {
    n[i] = node_flag[i];
  }

}

/************************************************************************
* exchange_node_flag : exchange the node flag 				*
************************************************************************/
void C_MOVER::exchange_node_flag(int *&n) {
  int *temp;

  temp = node_flag;
  node_flag = n;
  n = temp;

}

/************************************************************************
* exhange_script : exchange the script	 				*
************************************************************************/
void C_MOVER::exchange_script(C_XQUEUE *&s) {
  C_XQUEUE *temp;

  temp = script;
  script = s;
  s = temp;

}

/************************************************************************
* check_node : check if the equation of motion is on a node		*
************************************************************************/
int C_MOVER::check_node(int n) {
  int i;
  int len;
  C_OID *oid;

  len = sensors.get_length();
  oid = (C_OID *)sensors.get_top();

  for (i=0; i<len; i++) {
    if (oid->get_object_node() == n) return 1;
    oid = (C_OID *)oid->get_link();
  }

  return 0;

}

