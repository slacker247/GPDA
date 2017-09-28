// track.C method file

#include <stdio.h>
#include <math.h>

#include "track.H"

/************************************************************************
* TRACK : TRACK constructor						*
************************************************************************/
C_TRACK::C_TRACK() {
  set_ballistic(0);
}

/************************************************************************
* print : print this track						*
************************************************************************/
void C_TRACK::print() {
  double Perr[3];
  double Verr[3];
  double Terr;
  int i;

  for (i=0; i<3; i++) {
    Perr[i] = Pt[i]-Pe[i];
    Verr[i] = Vt[i]-Ve[i];
  }

  fprintf(stderr,"Track %d, cycle %d, Pos = (%f %f %f), Error = (%f %f %f)\n",
	id,cycle,Pe[0],Pe[1],Pe[2],Perr[0],Perr[1],Perr[2]);

  fprintf(stderr,"Track %d, cycle %d, Vel = (%f %f %f), Error = (%f %f %f)\n",
	id,cycle,Ve[0],Ve[1],Ve[2],Verr[0],Verr[1],Verr[2]);

  Terr = sqrt(Perr[0]*Perr[0]+Perr[1]*Perr[1]+Perr[2]*Perr[2]);
  fprintf(stderr,"Error %f, True Error %f\n\n",error,Terr);

}


