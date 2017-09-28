// Kalman1.C method file

#include <stdio.h>
#include <math.h>
#include "kalman2.H"

#define SIGMA 0.01
#define SIGMAG 0.1

/************************************************************************
* C_KALMAN2 : construct a kinematic Kalman Filter Object		*
************************************************************************/
C_KALMAN2::C_KALMAN2() {
  int i,j;

  cycle = 0;

  for (i=0; i<3; i++) {
    for (j=i; j<3; j++) P[i][j] = 0.0;
  }  

  for (i=0; i<2; i++) {
    for (j=0; j<3; j++) H[i][j] = 0.0;
  }  

  for (i=0; i<3; i++) {
    for (j=0; j<3; j++) Phi[i][j] = 0.0;
  }

  for (i=0; i<3; i++) {
    for (j=0; j<3; j++) Q[i][j] = 0;
  }

  H[0][0] = 1.0;
  H[1][1] = 1.0;

}

/************************************************************************
* set_T : set the time step						*
************************************************************************/
void C_KALMAN2::set_DT(double t) {

  DT = t;

  Phi[0][0] = 1.0;
  Phi[0][1] = DT;
  Phi[0][2] = DT*DT/2.0;

  Phi[1][1] = 1.0;
  Phi[1][2] = DT;

  Phi[2][2] = 1.0;

}

/************************************************************************
* set_U : set the unexpected maneuver parameter				*
************************************************************************/
void C_KALMAN2::set_U(double u) {
  int i,j;

//...... this assumes a constant correlated jerk u = J x DT

/*
  U[2] = u;
  U[1] = u*DT / 2.0;
  U[0] = U[1]*DT/3.0;

  for (i=0; i<3; i++) Q[i][i] = U[i]*U[i];
*/

//...... this assumes a constant uncorrelated jerk u = J x DT

  double T2,T3,T4;
  double u2;

  u2 = u*u;

  T2 = DT*DT;
  T3 = DT*T2;
  T4 = DT*T3;

  Q[0][0] = u2 * T4/4.0;
  Q[0][1] = u2 * T3/2.0;
  Q[0][2] = u2 * T2/2.0;

  Q[1][0] = Q[0][1];
  Q[1][1] = u2 * T2;
  Q[1][2] = u2 * DT;

  Q[2][0] = Q[0][2];
  Q[2][1] = Q[1][2];
  Q[2][2] = u2;

  for (i=0; i<3; i++) U[i] = sqrt(Q[i][i]);

  for (i=0; i<3; i++) {
    for (j=0; j<3; j++) {
      if (i != j) Q[i][j] = 0.0;
    }
  }
  Q[0][0] = 0.0;
  Q[1][1] = 0.0;

}

/************************************************************************
* update : update the Kalman Filter					*
************************************************************************/
void C_KALMAN2::update(double Xmeas, double Xdotmeas) {
  int i,j,k;
  double temp;
  double TX[3];
  double inv[2][2];
  double HTI[3][2];
  double O_KH[3][3];
  double O_KHP[3][3];
  double PPT[3][3];

//...... check if this is the first time

  cycle++;

  if (cycle == 1) {

    X[0] = Xmeas;
    X[1] = Xdotmeas;
    X[2] = 0.0;

    for (i=0; i<3; i++) {
      for (j=0; j<3; j++) P[i][j] = 0.0;
    }

    P[0][0] = R[0];	// use the measurement error
    P[1][1] = R[1];	// use the measurement error
    P[2][2] = 0.1;	// use large error for acceleration (10 g error)

    return;
  }

//...... normal case

  Z[0] = Xmeas;
  Z[1] = Xdotmeas;
  if (gravity) {
    temp = exp(-DT*SIGMAG);
    Phi[0][2] = DT/SIGMAG - (1.0-temp)/(SIGMAG*SIGMAG);
    Phi[1][2] = (1.0-temp)/SIGMAG;
    Phi[2][2] = temp;
  }else{
    temp = exp(-DT*SIGMA);
    Phi[0][2] = DT/SIGMA - (1.0-temp)/(SIGMA*SIGMA);
    Phi[1][2] = (1.0-temp)/SIGMA;
    Phi[2][2] = temp;
  }

//...... extrapolate the state to now

  for (i=0; i<3; i++) {
    temp = 0.0;
    for (j=0; j<3; j++) temp += Phi[i][j]*X[j];
    TX[i] = temp;
  }

  for (i=0; i<3; i++) X[i] = TX[i];


//...... extrapolate the covariance matrix to now

  for (i=0; i<3; i++) {
    for (j=0; j<3; j++) {
      PPT[i][j] = 0.0;
      for (k=0; k<3; k++) {
	PPT[i][j] += P[i][k]*Phi[j][k];
      }
    }
  }

  for (i=0; i<3; i++) {
    for (j=0; j<3; j++) {
      if (cycle > 3) {
        P[i][j] = Q[i][j];
      }else{
	if (i==j) {
	  P[i][j] = Q[i][j];
	}else{
	  P[i][j] = 0.0;
	}
      }
      for (k=0; k<3; k++) {
	P[i][j] += Phi[i][k]*PPT[k][j];
      }
    }
  }

  for (i=0; i<3; i++) {
    for (j=i+1; j<3; j++) {
      temp = (P[i][j] + P[j][i]) / 2.0;
      if (P[i][j] < 0.0) {
	if (fabs(P[i][j]) < 1.0e-10) {
	  P[i][j] = 0.0;
	  P[j][i] = 0.0;
	}else{
	  fprintf(stderr,"Kalman 2 covariance negative (cycle = %d)\n",cycle);
//	  P[i][j] = 0.0;
//	  P[j][i] = 0.0;
        }
      }else{
        P[i][j] = temp;
        P[j][i] = temp;
      }
    }
  }

//...... get Kalman gains K

  temp = (P[0][0] + R[0]) * (P[1][1] + R[1]) - P[0][1]*P[1][0];
  inv[0][0] = (P[1][1] + R[1]) / temp;
  inv[0][1] = -P[0][1] / temp;
  inv[1][0] = inv[0][1];
  inv[1][1] = (P[0][0] + R[0]) / temp;

  if (!check_inv(inv)) {
    fprintf(stderr,"Kalman 2 bad inverse, cycle = %d\n",cycle);
  }

  for (i=0; i<3; i++) {
    for (j=0; j<2; j++) {
      HTI[i][j] = 0.0;
      for (k=0; k<2; k++) {
	HTI[i][j] += H[k][i]*inv[k][j];
      }
    }
  }


  for (i=0; i<3; i++) {
    for (j=0; j<2; j++) {
      K[i][j] = 0.0;
      for (k=0; k<3; k++) {
	K[i][j] += P[i][k] * HTI[k][j];
      }
      if (K[i][j] < 0.0) {
	if (fabs(K[i][j]) < 1.0e-10) {
	  K[i][j] = 0.0;
	}else{
	  fprintf(stderr,"Kalman 2 gains negative (cycle = %d)\n",cycle);
//	  K[i][j] = 0.0;
        }
      }
    }
  }


//...... now get the new covariance matrix P

  for (i=0; i<3; i++) {
    for (j=0; j<3; j++) {
      O_KH[i][j] = 0.0;
      for (k=0; k<2; k++) {
	O_KH[i][j] -= K[i][k] * H[k][j];
      }
    }
  }

  for (i=0; i<3; i++) O_KH [i][i] += 1.0;

  for (i=0; i<3; i++) {
    for (j=0; j<3; j++) {
      O_KHP[i][j] = 0.0;
      for (k=0; k<3; k++) {
	O_KHP[i][j] += O_KH[i][k] * P[k][j];
      }
    }
  }

  for (i=0; i<3; i++) {
    for (j=0; j<3; j++) {
      P[i][j] = O_KHP[i][j];
    }
  }

  for (i=0; i<3; i++) {
    for (j=i+1; j<3; j++) {
      temp = (P[i][j] + P[j][i]) / 2.0;
      if (P[i][j] < 0.0) {
	if (fabs(P[i][j]) < 1.0e-10) {
	  P[i][j] = 0.0;
	  P[j][i] = 0.0;
	}else{
	  fprintf(stderr,"Kalman 2 covariance negative (cycle = %d)\n",cycle);
//	  P[i][j] = 0.0;
//	  P[j][i] = 0.0;
        }
      }else{
        P[i][j] = temp;
        P[j][i] = temp;
      }
    }
  }


//...... update the state


  for (i=0; i<3; i++) {
    temp = X[i];
    for (j=0; j<2; j++) temp += K[i][j]*(Z[j]-X[j]);
    TX[i] = temp;
  }

  for (i=0; i<3; i++) X[i] = TX[i];
  if (gravity) {
    X[0] += L2*DT*DT/2.0;
    X[1] += L2*DT;
  }

}

/************************************************************************
* check_inv : check that the inverse is ok				*
************************************************************************/
int C_KALMAN2::check_inv(double inv[2][2]) {
  int i,j,k;
  double array[2][2], Iarray[2][2], I[2][2];

  for (i=0; i<2; i++) {
    for (j=0; j<2; j++) {
      I[i][j] = 0.0;
      array[i][j] = P[i][j];
    }
  }
  array[0][0] += R[0];
  array[1][1] += R[1];
  I[0][0] = 1.0;
  I[1][1] = 1.0;

  for (i=0; i<2; i++) {
    for (j=0; j<2; j++) {
      Iarray[i][j] = 0.0;
      for (k=0; k<2; k++) {
        Iarray[i][j] += inv[i][k]*array[k][j];
      }
      if (fabs(Iarray[i][j]-I[i][j]) > 1.0e-10) {
        return 0;
      }
    }
  }

  return 1;

}
