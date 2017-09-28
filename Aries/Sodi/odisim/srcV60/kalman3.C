// Kalman1.C method file

#include <stdio.h>
#include <math.h>
#include "kalman3.H"

/************************************************************************
* C_KALMAN3 : construct a kinematic Kalman Filter Object		*
************************************************************************/
C_KALMAN3::C_KALMAN3() {
  int i,j;

  cycle = 0;

  for (i=0; i<4; i++) {
    for (j=i; j<4; j++) P[i][j] = 0.0;
  }  

  for (i=0; i<1; i++) {
    for (j=i; j<4; j++) H[i][j] = 0.0;
  }  

  for (i=0; i<4; i++) {
    for (j=0; j<4; j++) Phi[i][j] = 0.0;
  }

  for (i=0; i<4; i++) {
    for (j=0; j<4; j++) Q[i][j] = 0;
  }

  H[0][0] = 1.0;
  SIGMA = 0.0;

}

/************************************************************************
* set_T : set the time step						*
************************************************************************/
void C_KALMAN3::set_DT(double t) {

  DT = t;

  Phi[0][0] = 1.0;
  Phi[0][1] = DT;
  Phi[0][2] = DT*DT/2.0;
  Phi[0][3] = DT*DT*DT/6.0;

  Phi[1][1] = 1.0;
  Phi[1][2] = DT;
  Phi[1][3] = DT*DT/2.0;

  Phi[2][2] = 1.0;
  Phi[2][3] = DT;

  Phi[3][3] = 1.0;

}

/************************************************************************
* set_U : set the unexpected maneuver parameter				*
************************************************************************/
void C_KALMAN3::set_U(double u) {

  Q[3][3] = u*u;

}

/************************************************************************
* update : update the Kalman Filter					*
************************************************************************/
void C_KALMAN3::update(double Xmeas) {
  int i,j,k;
  double temp;
  double TX[4];
  double inv[1][1];
  double HTI[4][1];
  double O_KH[4][4];
  double O_KHP[4][4];
  double PPT[4][4];
  double S1,S2,S3;

  cycle++;

//...... first time

  if (cycle == 1) {
    X[0] = Xmeas;
    X[1] = 0.0;
    X[2] = 0.0;
    X[3] = 0.0;
    return;
  }

//...... second time

  if (cycle == 2) {
    X[1] = (Xmeas - X[0]) / DT;
    X[0] = Xmeas;
    X[2] = 0.0;
    X[3] = 0.0;
    return;
  }

//...... third time

  if (cycle == 3) {

    X[2] = ((Xmeas - X[0]) / DT - X[1])/DT;
    X[1] = (Xmeas - X[0]) / DT;
    X[0] = Xmeas;
    X[3] = 0.0;

    P[0][0] = R[0];			// use the measurement error / 2
    P[1][1] = R[0] / (DT*DT);		// use the measurement error / DT*DT
    P[2][2] = R[0] / (DT*DT*DT);	// use the measurement error / DT*DT
    P[3][3] = 10.0*Q[3][3];

    return;
  }


//...... normal case

  Z[0] = Xmeas;

  if (SIGMA != 0.0) {
    temp = exp(-DT*SIGMA);
    S1 = SIGMA;
    S2 = SIGMA*SIGMA;
    S3 = SIGMA*SIGMA*SIGMA;
    Phi[0][3] =  DT*DT/(2.0*S1) - DT/S2 + (1.0-temp)/S3;
    Phi[1][3] = DT/S1 - (1.0-temp)/S2;
    Phi[2][3] = (1.0-temp)/S1;
    Phi[3][3] = temp;
  }else{
    Phi[0][3] = 1.0;
    Phi[1][3] = DT;
    Phi[2][3] = DT*DT/2.0;
    Phi[3][3] = DT*DT*DT/6.0;
  }

//...... extrapolate the state to now

  for (i=0; i<4; i++) {
    temp = 0.0;
    for (j=0; j<4; j++) temp += Phi[i][j]*X[j];
    TX[i] = temp;
  }

  for (i=0; i<4; i++) X[i] = TX[i];


//...... extrapolate the covariance matrix to now

  for (i=0; i<4; i++) {
    for (j=0; j<4; j++) {
      PPT[i][j] = 0.0;
      for (k=0; k<4; k++) {
	PPT[i][j] += P[i][k]*Phi[j][k];
      }
    }
  }

  for (i=0; i<4; i++) {
    for (j=0; j<4; j++) {
      P[i][j] = Q[i][j];
      for (k=0; k<4; k++) {
	P[i][j] += Phi[i][k]*PPT[k][j];
      }
    }
  }

  for (i=0; i<4; i++) {
    for (j=i+1; j<4; j++) {
      temp = (P[i][j] + P[j][i]) / 2.0;
      if (P[i][j] < 0.0) {
	fprintf(stderr,"Kalman 1 covariance negative (cycle = %d)\n",cycle);
//	P[i][j] = 0.0;
//	P[j][i] = 0.0;
      }else{
        P[i][j] = temp;
        P[j][i] = temp;
      }
    }
  }

//...... get Kalman gains K

  inv[0][0] = 1.0 / (P[0][0] + R[0]);

  for (i=0; i<4; i++) {
    for (j=0; j<1; j++) {
      HTI[i][j] = 0.0;
      for (k=0; k<1; k++) {
	HTI[i][j] += H[k][i]*inv[k][j];
      }
    }
  }

  for (i=0; i<4; i++) {
    for (j=0; j<1; j++) {
      K[i][j] = 0.0;
      for (k=0; k<4; k++) {
	K[i][j] += P[i][k] * HTI[k][j];
      }
      if (K[i][j] < 0.0) {
	fprintf(stderr,"Kalman 1 gains negative (cycle = %d)\n",cycle);
//	K[i][j] = 0.0;
      }
    }
  }


//...... now get the new covariance matrix P

  for (i=0; i<4; i++) {
    for (j=0; j<4; j++) {
      O_KH[i][j] = 0.0;
      for (k=0; k<1; k++) {
	O_KH[i][j] -= K[i][k] * H[k][j];
      }
    }
  }

  for (i=0; i<4; i++) O_KH [i][i] += 1.0;

  for (i=0; i<4; i++) {
    for (j=0; j<4; j++) {
      O_KHP[i][j] = 0.0;
      for (k=0; k<4; k++) {
	O_KHP[i][j] += O_KH[i][k] * P[k][j];
      }
    }
  }

  for (i=0; i<4; i++) {
    for (j=0; j<4; j++) {
      P[i][j] = O_KHP[i][j];
    }
  }

  for (i=0; i<4; i++) {
    for (j=i+1; j<4; j++) {
      temp = (P[i][j] + P[j][i]) / 2.0;
      if (P[i][j] < 0.0) {
	fprintf(stderr,"Kalman 1 covariance negative (cycle = %d)\n",cycle);
//	P[i][j] = 0.0;
//	P[j][i] = 0.0;
      }else{
        P[i][j] = temp;
        P[j][i] = temp;
      }
    }
  }


//...... update the state


  for (i=0; i<4; i++) {
    temp = X[i];
    for (j=0; j<1; j++) temp += K[i][j]*(Z[j]-X[j]);
    TX[i] = temp;
  }

  for (i=0; i<4; i++) X[i] = TX[i];
  if (gravity) {
    X[0] += L2*DT*DT/2.0;
    X[1] += L2*DT;
  }
}

