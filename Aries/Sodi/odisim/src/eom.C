// eom.C method file

#include <stdio.h>
#include "eom.H"

void C_EOM::get_pos_vel(double /*t*/, double R[3], double V[3]) {
  int i;
  for (i=0; i<3; i++) {
    R[i] = Rend[i];
    V[i] = 0.0;
  }
}
double C_EOM::get_tdist(double /*dist*/) {return -1.0;}
double C_EOM::get_start_time() {return t0;}
double C_EOM::get_endtime() {return Tend;}
double *C_EOM::get_startpos() {return R0;}
double *C_EOM::get_endpos() {return Rend;}

/************************************************************************
* change_endtime : change the end time of this eom			*
************************************************************************/
void C_EOM::change_endtime(double te) {
  double X[3];
  double V[3];

  get_pos_vel(te,X,V);

  Tend = te;

  Rend[0] = X[0];
  Rend[1] = X[1];
  Rend[2] = X[2];

}

/************************************************************************
* print : print out the eom values					*
************************************************************************/
void C_EOM::print() {

  cerr << "id " << object_id
       << ", type " << object_type
       << ", node " << object_node
       << ", gid " << glob_gid
       << ", sequence " << sequence
       << ", t0 " << t0
       << ", Te " << Tend
       << ", tt " << time_tag << "\n";

}

