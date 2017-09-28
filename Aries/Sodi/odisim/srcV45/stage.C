// stage.C method file

#include <stdio.h>
#include <math.h>
#include "stage.H"
#include "def.h"

#define GM 398601.3
#define RADDEG  0.0174532925199432958

/************************************************************************
* C_STAGE : construct a stage object					*
************************************************************************/
C_STAGE::C_STAGE() {


}

/************************************************************************
* init_values : initialize the initial position for this stage		*
************************************************************************/
void C_STAGE::init_values() {
  int i;
  double r;
  double b,c,d,e;
  double A, B;
  double maxerr;
  double correction;
  double ct;
  double ct2;
  double temp;
  double Vend[3];

  Tcor = Thrust*Mr*0.0098;

  ct = S0[0]*Sb[0] + S0[1]*Sb[1] + S0[2]*Sb[2];
  ct2 = sqrt((1.0+ct)/2.0);
  b = 1.0 - ct;

  maxerr = (1.0 + b * 0.25) * ct2;
  correction = (0.57827) + (0.31670)*maxerr + (0.10501)*maxerr*maxerr;

  r = sqrt(R0[0]*R0[0] + R0[1]*R0[1] + R0[2]*R0[2]);

  mu = Mf/Mr;
  c1 = Tb * (Tcor/correction) / Mr;
  c2 = - GM / (r*r*r);

  e = -b/(mu*mu);
  c = 1.0 + e*(1.0-mu);
  d = e*(mu-2.0);

  for (i=0; i<3; i++) {
    B = (S0[i]-Sb[i])/mu;
    A = S0[i] - B;
    C[i] = -A*c/mu;
    D[i] =  A*(d+e) + B*(c+d+e);
    E[i] =  -mu*(B*(d+2.0*e) + A*e)/2.0;
    FF[i] =  e*B*mu*mu/2.0;
  }

  if (manuever) {
    mandir[0] = S0[1]*Sb[2] - S0[2]*Sb[1];
    mandir[1] = S0[2]*Sb[0] - S0[0]*Sb[2];
    mandir[2] = S0[0]*Sb[1] - S0[1]*Sb[0];
    temp = sqrt(mandir[0]*mandir[0]+mandir[1]*mandir[1]+mandir[2]*mandir[2]);
    mandir[0] /= temp;
    mandir[1] /= temp;
    mandir[2] /= temp;
  }

  get_pos_vel(Tb+t0,R1,V1);
  TT = Tb + Tcoast;
  get_pos_vel(TT+t0,Rend,Vend);

}

/************************************************************************
* get pos_vel : get the position and velocity at a given time		*
************************************************************************/
void C_STAGE::get_pos_vel(double tt, double rt[3], double vt[3]) {
  double alpha,alpha2,alpha3,alpha4;
  double am;
  double logterm;
  double t;
  int i;

  t = tt-t0;

  if (t > Tb) {

    coast(tt,rt,vt);

  }else{

    alpha = t / Tb;
    alpha2 = alpha*alpha;
    alpha3 = alpha2*alpha;
    alpha4 = alpha3*alpha;

    am = alpha*mu;
    logterm = log(1.0-am);

    for (i=0; i<3; i++) {

      vt[i] = V0[i] + (R0[i]*c2)*t
	+ c1*(C[i]*logterm + alpha*D[i] + alpha2*E[i] + alpha3*FF[i]);

      rt[i] = R0[i]*(1.0 + c2*(t*t)/2.0) + V0[i]*t
	+ c1*Tb*( -C[i]*((1.0-am)*logterm + am)/mu 
		  +D[i]*alpha2/2.0
		  +E[i]*alpha3/3.0
		  +FF[i]*alpha4/4.0 );

    }

    if (manuever) man_pos_vel(t, rt, vt);
  }

//...... rotate ECI position and velocity by the launch position

  rotate(rt,vt);

//...... rotate to ECR coordinates if not ECI

  if (!ECI) {
    eci_to_ecr(tt,rt,vt);
  }

}

