// stereo_track.C method file

#include <string.h>
#include <stdio.h>
#include <math.h>
#include "def.h"
#include "stereo_track.H"
#include "matrix_equation.H"

// This undoes the DOUBLE_CHECK stuff that is defined in matrix_equation.H

#undef DOUBLE_CHECK
#define DOUBLE_CHECK

//#define COPLANER
//#define SIGMA1

#define TIME_INC 10.0
#define ALLBOOSTER 0

/************************************************************************
* STEREO_TRACK : STEREO_TRACK constructor				*
************************************************************************/
C_STEREO_TRACK::C_STEREO_TRACK() {

  window = 300.0;
  told = -1.0e20;
  tcurrent = -1.0e20;
  balistic = 0;
  weight_flag = 1;

}

/************************************************************************
* init : initialize the stereo track from another one			*
************************************************************************/
void C_STEREO_TRACK::init(C_STEREO_TRACK *st) {
  char *pst;
  char *pthis;
  int nb;

  pst = st->get_start_pointer();
  pthis = get_start_pointer();

  nb = get_st_bytes();
  memcpy(pthis, pst, nb);
  time_tag = st->get_time_tag();
  id = st->get_id();

/*
  pthis = (char *)this;
  pthis += sizeof(C_SQ_ITEM);
  pst = (char *)st;
  pst += sizeof(C_SQ_ITEM);

  memcpy(pthis, pst,
	sizeof(C_STEREO_TRACK) - sizeof(C_SQ_ITEM) - sizeof(C_XQUEUE));

  time_tag = st->get_time_tag();
*/

}

/************************************************************************
* get_pos_vel : get the position and velocity of the track		*
************************************************************************/
void C_STEREO_TRACK::get_pos_vel(double t, double X[3], double V[3]) {
  double DT1, DT2, DT3, DT4;
  int i;

  DT1 = t-told;
  DT2 = DT1*DT1;
  DT3 = DT2*DT1;
  DT4 = DT3*DT1;

  for (i=0; i<3; i++) {
    X[i] = a[i] + b[i]*DT1 + c[i]*DT2 + d[i]*DT3 + e[i]*DT4;
    V[i] = b[i] + 2.0*c[i]*DT1 + 3.0*d[i]*DT2 + 4*e[i]*DT3;
  }

}

/************************************************************************
* get_poly_order : get the order of the polynomial			*
************************************************************************/
int C_STEREO_TRACK::get_poly_order() {
  double temp;

  temp = a[0]*a[0]+a[1]*a[1]+a[2]*a[2];
  if (temp == 0.0) return -1;

  temp = b[0]*b[0]+b[1]*b[1]+b[2]*b[2];
  if (temp == 0.0) return 0;

  temp = c[0]*c[0]+c[1]*c[1]+c[2]*c[2];
  if (temp == 0.0) return 1;

  temp = d[0]*d[0]+d[1]*d[1]+d[2]*d[2];
  if (temp == 0.0) return 2;

  temp = e[0]*e[0]+e[1]*e[1]+e[2]*e[2];
  if (temp == 0.0) return 3;

  return 4;

}

/************************************************************************
* update_state : update the state of the stereo track			*
************************************************************************/
void C_STEREO_TRACK::update_state(int print_flag) {
  int i,j,k,n;
  int kmax;
  int index;
  int n_detections;
  BIG_FLOAT **A;
  BIG_FLOAT B[15];
  BIG_FLOAT X[15];
  C_DETECTION *detection;
  BIG_FLOAT tk;
  double *S;
  double *SV;
  double *V;
  BIG_FLOAT SdotV;
  BIG_FLOAT SVdotV;
  BIG_FLOAT DT1,DT2,DT3,DT4;
  BIG_FLOAT sigma2;
  BIG_FLOAT range;
  BIG_FLOAT range_rate;
  BIG_FLOAT range2;
  BIG_FLOAT temp;
  //C_DOUBLE dbl;
  double Xe[3];
  double Ve[3];
  double *Xtrue;
  double *Vtrue;
  double Xerror;
  double Verror;
  BIG_FLOAT chi_squared;
  BIG_FLOAT d0,d1,d2;
  BIG_FLOAT Rdotv;
  BIG_FLOAT bias[3];
  double weight;
  double tau;

  tau = 1.0;
  balistic = 0;

  n_detections = detections.get_length();

  if (n_detections < 3) {

    a[0] = 0.0;
    a[1] = 0.0;
    a[2] = 0.0;

    b[0] = 0.0;
    b[1] = 0.0;
    b[2] = 0.0;

    c[0] = 0.0;
    c[1] = 0.0;
    c[2] = 0.0;

    d[0] = 0.0;
    d[1] = 0.0;
    d[2] = 0.0;

    e[0] = 0.0;
    e[1] = 0.0;
    e[2] = 0.0;

    return;

  }

//  n = n_detections + n_detections;
  n = n_detections;

//...... earliest and latest detection time and set the type, id, and node

  detection = (C_DETECTION *)detections.get_top();

  object_type = detection->get_object_type();
  object_id = detection->get_object_id();
  object_node = detection->get_object_node();

  told = detection->get_time();
  tcurrent = told;

  for (i=0; i<n_detections; i++) {
    if (detection->get_time() < told) told = detection->get_time();
    if (detection->get_time() > tcurrent) tcurrent = detection->get_time();
    if (detection->get_range() > 0.0) n++;
    detection = (C_DETECTION *)detection->get_link();
  }

  tupdate = tcurrent;

//...... set the size of the matrix

  n -= n%3;
  if (n > 15) n = 15;
//  if (n > 6) n = 6;
  kmax = n/3;

  A = new BIG_FLOAT*[n];
  if (A == NULL) {
    fprintf(stderr,"Warning (STEREO_TRACK), out of memory\n");
    return;
  }

  for (i=0; i<n; i++) {
    A[i] = new BIG_FLOAT[n];
    if (A[i] == NULL) {
      fprintf(stderr,"Warning (STEREO_TRACK), out of memory\n");
      return;
    }
    for (j=0; j<n; j++) A[i][j] = 0.0;
    B[i] = 0.0;
    X[i] = 0.0;
  }

//...... loop over the detections and fill A and B

  detection = (C_DETECTION *)detections.get_top();
  for (i=0; i<n_detections; i++) {

    DT1 = detection->get_time() - told;
    DT2 = DT1*DT1;
    DT3 = DT1*DT2;
    DT4 = DT2*DT2;

    S = detection->get_S();
    V = detection->get_V();
    SdotV = S[0]*V[0]+S[1]*V[1]+S[2]*V[2];

    range = detection->get_range();
    range_rate = detection->get_range_rate();

    if (weight_flag) {
      temp = (window-DT1)/window;
    }else{
      temp = 2.3*(DT1)/window;
    }

    if (temp > 0.0) {
      weight = exp(-tau*temp);
    }else{
      weight = 0.00001;
    }
//    weight = 1.0;

//...... check if measurement has range information

    if (range > 0.0) {

//...... get the sigma squared for measurements with range

      temp = detection->get_Sigma() * range;
      sigma2 = temp*temp;

#ifdef SIGMA1
      sigma2 = 1.0;
#endif

      if (detection->get_ballistic()) {
        if (weight_flag) {
	  sigma2 /= 10000.0;
        }else{
	  sigma2 *= 10000.0;
        }
        balistic = 1;
      }

      sigma2 /= weight;

//...... construct the A matrix and the B vector for measurement with range

      tk = 1.0;
      for (k=0; k<kmax; k++) {
        for (j=0; j<3; j++) {
          index = k*3 + j;
	  B[index] += DOUBLE_CHECK (tk*S[j]/sigma2);
	  B[index] += DOUBLE_CHECK (tk*range*V[j]/sigma2);
//	  B[index] += DOUBLE_CHECK (tk*(S[j]+range*V[j])/sigma2);
          if (n >= 3)  A[index][j]    += DOUBLE_CHECK (tk/sigma2);
	  if (n >= 6)  A[index][j+3]  += DOUBLE_CHECK (tk*DT1/sigma2);
	  if (n >= 9)  A[index][j+6]  += DOUBLE_CHECK (tk*DT2/sigma2);
	  if (n >= 12) A[index][j+9]  += DOUBLE_CHECK (tk*DT3/sigma2);
	  if (n >= 15) A[index][j+12] += DOUBLE_CHECK (tk*DT4/sigma2);
        }
	tk *= DT1;
      }

    }else{

//...... get the sigma squared for measurements without range

      get_pos_vel(detection->get_time(), Xe, Ve);
      if ((fabs(Xe[0])+fabs(Xe[0])+fabs(Xe[0])) < 0.001) {
        range2 = 6000.0*6000.0;
      }else{
        d0 = Xe[0]-S[0];
        d1 = Xe[1]-S[1];
        d2 = Xe[2]-S[2];
        range2 = d0*d0 + d1*d1 + d2*d2;
      }

      temp = detection->get_Sigma();
      sigma2 = temp*temp*range2;

#ifdef SIGMA1
      sigma2 = 1.0;
#endif

      if (detection->get_ballistic()) {
        if (weight_flag) {
          sigma2 /= 10000.0;
	}else{
          sigma2 *= 10000.0;
	}
        balistic = 1;
      }
      sigma2 /= weight;

//...... construct the A matrix and the B vector for measurement without range

      tk = 1.0;
      for (k=0; k<kmax; k++) {
        for (j=0; j<3; j++) {

          index = k*3 + j;

	  B[index] += DOUBLE_CHECK (tk*S[j]/sigma2);
	  B[index] += DOUBLE_CHECK (-tk*V[j]*SdotV/sigma2);
//	  B[index] += DOUBLE_CHECK (tk*(S[j]-V[j]*SdotV)/sigma2);

	  if (n >= 3) {
            A[index][0]   += DOUBLE_CHECK (-tk*V[j]*V[0]/sigma2);
            A[index][1]   += DOUBLE_CHECK (-tk*V[j]*V[1]/sigma2);
            A[index][2]   += DOUBLE_CHECK (-tk*V[j]*V[2]/sigma2);
            A[index][j]   += DOUBLE_CHECK (tk/sigma2);
          }

	  if (n >= 6) {
            A[index][3]   += DOUBLE_CHECK (-DT1*tk*V[j]*V[0]/sigma2);
            A[index][4]   += DOUBLE_CHECK (-DT1*tk*V[j]*V[1]/sigma2);
            A[index][5]   += DOUBLE_CHECK (-DT1*tk*V[j]*V[2]/sigma2);
	    A[index][j+3] += DOUBLE_CHECK (tk*DT1/sigma2);
          }

	  if (n >= 9) {
            A[index][6]   += DOUBLE_CHECK (-DT2*tk*V[j]*V[0]/sigma2);
            A[index][7]   += DOUBLE_CHECK (-DT2*tk*V[j]*V[1]/sigma2);
            A[index][8]   += DOUBLE_CHECK (-DT2*tk*V[j]*V[2]/sigma2);
	    A[index][j+6] += DOUBLE_CHECK (tk*DT2/sigma2);
          }

	  if (n >= 12) {
            A[index][9]   += DOUBLE_CHECK (-DT3*tk*V[j]*V[0]/sigma2);
            A[index][10]  += DOUBLE_CHECK (-DT3*tk*V[j]*V[1]/sigma2);
            A[index][11]  += DOUBLE_CHECK (-DT3*tk*V[j]*V[2]/sigma2);
	    A[index][j+9] += DOUBLE_CHECK (tk*DT3/sigma2);
          }

	  if (n >= 15) {
            A[index][12]  += DOUBLE_CHECK (-DT4*tk*V[j]*V[0]/sigma2);
            A[index][13]  += DOUBLE_CHECK (-DT4*tk*V[j]*V[1]/sigma2);
            A[index][14]  += DOUBLE_CHECK (-DT4*tk*V[j]*V[2]/sigma2);
	    A[index][j+12]+= DOUBLE_CHECK (tk*DT4/sigma2);
          }

        }
	tk *= DT1;
      }

    }

//...... check if measurement has range_rate information

    if (detection->get_valid_range_rate()) {

      SV = detection->get_SV();
      SVdotV = SV[0]*V[0]+SV[1]*V[1]+SV[2]*V[2];

//...... get the sigma squared for measurements with range rate

      sigma2 = detection->get_Sigma_rr();
#ifdef SIGMA1
      sigma2 = 1.0;
#endif

      if (detection->get_ballistic()) {
        if (weight_flag) {
	  sigma2 /= 10000.0;
	}else{
	  sigma2 *= 10000.0;
	}
        balistic = 1;
      }

      sigma2 /= weight;
      sigma2 = 0.000001;

//...... construct the A matrix and the B vector for range rate

      tk = 1.0;
      for (k=1; k<kmax; k++) {
        for (j=0; j<3; j++) {

          index = k*3 + j;
	  B[index] += DOUBLE_CHECK (k*tk*V[j]*(SVdotV+range_rate)/sigma2);

	  if (n >= 6) {
            A[index][3]   += DOUBLE_CHECK (k*tk*V[j]*V[0]/sigma2);
            A[index][4]   += DOUBLE_CHECK (k*tk*V[j]*V[1]/sigma2);
            A[index][5]   += DOUBLE_CHECK (k*tk*V[j]*V[2]/sigma2);
          }

	  if (n >= 9) {
            A[index][6]   += DOUBLE_CHECK (k*tk*V[j]*2.0*DT1*V[0]/sigma2);
            A[index][7]   += DOUBLE_CHECK (k*tk*V[j]*2.0*DT1*V[1]/sigma2);
            A[index][8]   += DOUBLE_CHECK (k*tk*V[j]*2.0*DT1*V[2]/sigma2);
          }

	  if (n >= 12) {
            A[index][9]   += DOUBLE_CHECK (k*tk*V[j]*3.0*DT2*V[0]/sigma2);
            A[index][10]  += DOUBLE_CHECK (k*tk*V[j]*3.0*DT2*V[1]/sigma2);
            A[index][11]  += DOUBLE_CHECK (k*tk*V[j]*3.0*DT2*V[2]/sigma2);
          }

	  if (n >= 15) {
            A[index][12]  += DOUBLE_CHECK (k*tk*V[j]*4.0*DT3*V[0]/sigma2);
            A[index][13]  += DOUBLE_CHECK (k*tk*V[j]*4.0*DT3*V[1]/sigma2);
            A[index][14]  += DOUBLE_CHECK (k*tk*V[j]*4.0*DT3*V[2]/sigma2);
          }

        }

	tk *= DT1;

      }
    }

//...... get the next detection

    detection = (C_DETECTION *)detection->get_link();

  }

//...... solve the matrix equation

  matrix_equation(A,B,X,n);
//  iterate(A,B,X,n);

//...... store the answer

  if (n >= 3) {
    a[0] = X[0];
    a[1] = X[1];
    a[2] = X[2];
  }else{
    a[0] = 0.0;
    a[1] = 0.0;
    a[2] = 0.0;
  }

  if (n >= 6) {
    b[0] = X[3];
    b[1] = X[4];
    b[2] = X[5];
  }else{
    b[0] = 0.0;
    b[1] = 0.0;
    b[2] = 0.0;
  }

  if (n >= 9) {
    c[0] = X[6];
    c[1] = X[7];
    c[2] = X[8];
  }else{
    c[0] = 0.0;
    c[1] = 0.0;
    c[2] = 0.0;
  }

  if (n >= 12) {
    d[0] = X[9];
    d[1] = X[10];
    d[2] = X[11];
  }else{
    d[0] = 0.0;
    d[1] = 0.0;
    d[2] = 0.0;
  }

  if (n >= 15) {
    e[0] = X[12];
    e[1] = X[13];
    e[2] = X[14];
  }else{
    e[0] = 0.0;
    e[1] = 0.0;
    e[2] = 0.0;
  }

#ifdef COPLANER

//...... check for coplaner stuff

  double Vperp_est[3];
  double Vperp_true[3];
  double *Xtrue;
  double *Vtrue;

//...... estimate

  Vperp_est[0] = a[1]*b[2] - a[2]*b[1];
  Vperp_est[1] = a[2]*b[0] - a[0]*b[2];
  Vperp_est[2] = a[0]*b[1] - a[1]*b[0];
  temp = sqrt(  Vperp_est[0]*Vperp_est[0]
	      + Vperp_est[1]*Vperp_est[1]
	      + Vperp_est[2]*Vperp_est[2]);
  Vperp_est[0] /= temp;
  Vperp_est[1] /= temp;
  Vperp_est[2] /= temp;

//...... truth

  detection = (C_DETECTION *)detections.get_bot();
  Xtrue = detection->get_Xtrue();
  Vtrue = detection->get_Vtrue();
  Vperp_true[0] = Xtrue[1]*Vtrue[2] - Xtrue[2]*Vtrue[1];
  Vperp_true[1] = Xtrue[2]*Vtrue[0] - Xtrue[0]*Vtrue[2];
  Vperp_true[2] = Xtrue[0]*Vtrue[1] - Xtrue[1]*Vtrue[0];
  temp = sqrt(  Vperp_true[0]*Vperp_true[0]
	      + Vperp_true[1]*Vperp_true[1]
	      + Vperp_true[2]*Vperp_true[2]);
  Vperp_true[0] /= temp;
  Vperp_true[1] /= temp;
  Vperp_true[2] /= temp;

  temp =   Vperp_true[0]*Vperp_est[0]
	 + Vperp_true[1]*Vperp_est[1]
	 + Vperp_true[2]*Vperp_est[2];

  temp = acos(temp);
  fprintf(stderr,"coplaner angle = %f degrees\n",temp*DEGRAD);

#endif

//...... compute chi squared and bias to check the fit

  rms = 0.0;
  chi_squared = 0.0;
  bias[0] = 0.0;
  bias[1] = 0.0;
  bias[2] = 0.0;

  detection = (C_DETECTION *)detections.get_top();
  for (i=0; i<n_detections; i++) {

    DT1 = detection->get_time() - told;
    DT2 = DT1*DT1;
    DT3 = DT1*DT2;
    DT4 = DT2*DT2;

    S = detection->get_S();
    V = detection->get_V();
    sigma2 = detection->get_Sigma() * detection->get_Sigma() + 0.000001;

    get_pos_vel(detection->get_time(),Xe,Ve);

    range = detection->get_range();
    if (range > 0.0) {

      sigma2 *= range*range;

#ifdef SIGMA1
      sigma2 = 1.0;
#endif

      d0 = Xe[0] - S[0] - range*V[0];
      d1 = Xe[1] - S[1] - range*V[1];
      d2 = Xe[2] - S[2] - range*V[2];
      rms += (d0*d0 + d1*d1 + d2*d2) / sigma2;

      bias[0] += d0;
      bias[1] += d1;
      bias[2] += d2;

    }else{

      d0 = Xe[0]-S[0];
      d1 = Xe[1]-S[1];
      d2 = Xe[2]-S[2];

      sigma2 *= d0*d0 + d1*d1 + d2*d2;

#ifdef SIGMA1
      sigma2 = 1.0;
#endif

      Rdotv = d0*V[0] + d1*V[1] + d2*V[2];
      chi_squared += (d0*d0 + d1*d1 + d2*d2 - Rdotv*Rdotv) / sigma2;
      rms += d0*d0 + d1*d1 + d2*d2 - Rdotv*Rdotv;

      bias[0] += Xe[0] - S[0] - Rdotv*V[0];
      bias[1] += Xe[1] - S[1] - Rdotv*V[1];
      bias[2] += Xe[2] - S[2] - Rdotv*V[2];

    }

    detection = (C_DETECTION *)detection->get_link();

  }

  rms /= n_detections;
/*
  if (rms < -0.001) {
    fprintf(stderr,"Error (stereo_track) bad rms %g\n",rms);
  }
*/
  if (rms < 0.0) {
    rms = 0.0;
  }else{
    rms = sqrt(rms);
  }

  bias[0] /= n_detections;
  bias[1] /= n_detections;
  bias[2] /= n_detections;

//...... print out the chi squared and the bias

  if (print_flag) {
    fprintf(stderr,"Chi Squared with %d measurements = %lg\n",
	n_detections, chi_squared);

    fprintf(stderr,"RMS with %d measurements = %g\n",
	n_detections, rms);

    fprintf(stderr,"Bias = %lg, %lg, %lg\n",bias[0],bias[1],bias[2]);
  }

//...... delete the A matrix

  for (i=0; i<n; i++) delete [] A[i]; //RVI 2/18/98 
  delete [] A; //RVI 2/18/98 

//...... print out the coefficients

  if (print_flag) {
    fprintf(stderr,"a[0] = %9g\t, a[1] = %9g\t, a[2] = %9g\n",a[0],a[1],a[2]);
    fprintf(stderr,"b[0] = %9g\t, b[1] = %9g\t, b[2] = %9g\n",b[0],b[1],b[2]);
    fprintf(stderr,"c[0] = %9g\t, c[1] = %9g\t, c[2] = %9g\n",c[0],c[1],c[2]);
    fprintf(stderr,"d[0] = %9g\t, d[1] = %9g\t, d[2] = %9g\n",d[0],d[1],d[2]);
    fprintf(stderr,"e[0] = %9g\t, e[1] = %9g\t, e[2] = %9g\n",e[0],e[1],e[2]);
  }

//...... print out the truth and estimate for the most recent detection

  if (print_flag) {

    detection = (C_DETECTION *)detections.get_bot();
    get_pos_vel(detection->get_time_tag(), Xe, Ve);
    fprintf(stderr,"Estimated X = %f %f %f, V = %f %f %f\n",
	Xe[0], Xe[1], Xe[2], Ve[0], Ve[1], Ve[2]);

    Xtrue = detection->get_Xtrue();
    Vtrue = detection->get_Vtrue();
    fprintf(stderr,"Truth     X = %f %f %f, V = %f %f %f\n",
	Xtrue[0], Xtrue[1], Xtrue[2], Vtrue[0], Vtrue[1], Vtrue[2]);

    d0 = Xtrue[0] - Xe[0];
    d1 = Xtrue[1] - Xe[1];
    d2 = Xtrue[2] - Xe[2];
    Xerror = sqrt(d0*d0+d1*d1+d2*d2);

    d0 = Vtrue[0] - Ve[0];
    d1 = Vtrue[1] - Ve[1];
    d2 = Vtrue[2] - Ve[2];
    Verror = sqrt(d0*d0+d1*d1+d2*d2);

    cerr << "Time " << detection->get_time_tag()
	 << ", Xerror " << Xerror << ", Verror " << Verror << "\n\n";

  }


}

/************************************************************************
* get_true_state : get the true state of the track			*
************************************************************************/
void C_STEREO_TRACK::get_true_state(double P[3], double V[3]) {
  C_DETECTION *detection1;
  C_DETECTION *detection;
  double *Xtrue;
  double *Vtrue;
  int i,len;
  double maxtime;

  len = detections.get_length();
  if (len == 0) {
    fprintf(stderr,"Error (stereo_track) get_true_state has no detections\n");
    for (i=0; i<3; i++) {
      P[i] = 0.0;
      V[i] = 0.0;
    }
    return;
  }

  detection = NULL;
  detection1 = (C_DETECTION *)detections.get_top();
  maxtime = -1.0e20;

  for (i=0; i<len; i++) {
    if (maxtime < detection1->get_time_tag()) {
      maxtime = detection1->get_time_tag();
      detection = detection1;
    }
    detection1 = (C_DETECTION *)detection1->get_link();
  }

  if (detection == NULL) {
    fprintf(stderr,"Error (stereo_track) get_true_state has NULL detection\n");
    for (i=0; i<3; i++) {
      P[i] = 0.0;
      V[i] = 0.0;
    }
    return;
  }

  Xtrue = detection->get_Xtrue();
  Vtrue = detection->get_Vtrue();

  for (i=0; i<3; i++) {
    P[i] = Xtrue[i];
    V[i] = Vtrue[i];
  }

  tdetection = maxtime;

}

/************************************************************************
* get_est_state : get the extimated state of the track			*
************************************************************************/
void C_STEREO_TRACK::get_est_state(double P[3], double V[3]) {

  get_pos_vel(tupdate,P,V);

}

/************************************************************************
* print : print the track						*
************************************************************************/
void C_STEREO_TRACK::print() {
  double Xe[3];
  double Ve[3];
  double *Xtrue;
  double *Vtrue;
  double d0,d1,d2;
  double Xerror;
  double Verror;
  C_DETECTION *detection;

  fprintf(stderr,"a[0] = %9g\t, a[1] = %9g\t, a[2] = %9g\n",a[0],a[1],a[2]);
  fprintf(stderr,"b[0] = %9g\t, b[1] = %9g\t, b[2] = %9g\n",b[0],b[1],b[2]);
  fprintf(stderr,"c[0] = %9g\t, c[1] = %9g\t, c[2] = %9g\n",c[0],c[1],c[2]);
  fprintf(stderr,"d[0] = %9g\t, d[1] = %9g\t, d[2] = %9g\n",d[0],d[1],d[2]);
  fprintf(stderr,"e[0] = %9g\t, e[1] = %9g\t, e[2] = %9g\n",e[0],e[1],e[2]);

//...... print out the truth and estimate for the most recent detection

  detection = (C_DETECTION *)detections.get_bot();

  cerr << "Time " << detection->get_time_tag() << ", id " << id
       << ", n_detections " << detections.get_length() << "\n";

  get_pos_vel(detection->get_time_tag(), Xe, Ve);
  fprintf(stderr,"Estimated X = %f %f %f, V = %f %f %f\n",
	Xe[0], Xe[1], Xe[2], Ve[0], Ve[1], Ve[2]);

  Xtrue = detection->get_Xtrue();
  Vtrue = detection->get_Vtrue();
  fprintf(stderr,"Truth     X = %f %f %f, V = %f %f %f\n",
	Xtrue[0], Xtrue[1], Xtrue[2], Vtrue[0], Vtrue[1], Vtrue[2]);

  d0 = Xtrue[0] - Xe[0];
  d1 = Xtrue[1] - Xe[1];
  d2 = Xtrue[2] - Xe[2];
  Xerror = sqrt(d0*d0+d1*d1+d2*d2);

  d0 = Vtrue[0] - Ve[0];
  d1 = Vtrue[1] - Ve[1];
  d2 = Vtrue[2] - Ve[2];
  Verror = sqrt(d0*d0+d1*d1+d2*d2);

  fprintf(stderr,"Xerror %f, Verror %f\n\n", Xerror, Verror);

}


/************************************************************************
* launch_position : get the launch position				*
************************************************************************/
void C_STEREO_TRACK::launch_position(double &time, double &lat, double &lon) {
  int i;
  int n_detections;
  double Xe[3];
  double Ve[3];
  double Range;
  double Range_Rate;
  C_DETECTION *detection;
  double altitude;
  double error;
  double tmax;
  double tmin;
  double dt;
  int all_booster;
  int ballistic;
  int nits;
  int n_ballistic;
  int n_boosting;

//...... tmax  (upper bound) is the earliest time tag

  detection = (C_DETECTION *)detections.get_top();
  tmax = 1.0e20;
  n_detections = detections.get_length();
  ballistic = 1;
  all_booster = 1;
  n_boosting = 0;
  n_ballistic = 0;

  for (i=0; i<n_detections; i++) {

    if (detection->get_time_tag() < tmax) {
      tmax = detection->get_time_tag();
      ballistic = detection->get_ballistic();
    } 

    if (detection->get_ballistic()) {
      all_booster = 0;
      n_ballistic++;
    }else{
      n_boosting++;
    }

    detection = (C_DETECTION *)detection->get_link();

  }

//...... exit if no boosting detections

  if (ballistic) {
    time = -1.0;
    lat  = -1.0;
    lon  = -1.0;
    return;
  }

//...... exit if requiring all boosting detections

  if (!all_booster & ALLBOOSTER) {
    time = -1.0;
    lat  = -1.0;
    lon  = -1.0;
    return;
  }

//...... exit if too low of a percentage of boosting detections

  if (double(n_boosting)/double(n_detections) < 0.50) {
    time = -1.0;
    lat  = -1.0;
    lon  = -1.0;
    return;
  }

//...... loop in ten second increments to bound the upper time window

  nits = 0;
  while (1) {

    get_pos_vel(tmax, Xe, Ve);
    error = (Xe[0]*Xe[0] + Xe[1]*Xe[1] + Xe[2]*Xe[2]) - RE2;
    if (error > 0.0) break;
    tmax += TIME_INC;

    nits++;

    if (nits > 1000) {
      time = -1.0;
      lat  = 0.0;
      lon  = 0.0;
      return;
    }

  }

  nits = 0;
  while (1) {

    get_pos_vel(tmax-TIME_INC, Xe, Ve);
    error = (Xe[0]*Xe[0] + Xe[1]*Xe[1] + Xe[2]*Xe[2]) - RE2;
    if (error < 0.0) break;
    tmax -= TIME_INC;

    nits++;

    if (nits > 1000) {
      time = -1.0;
      lat  = 0.0;
      lon  = 0.0;
      return;
    }

  }

  tmin = tmax - TIME_INC;
  time = tmax - TIME_INC/2.0;

//...... now iterate until the altitude is zero

  nits = 0;
  while (1) {

    get_pos_vel(time, Xe, Ve);
    Range = sqrt(Xe[0]*Xe[0] + Xe[1]*Xe[1] + Xe[2]*Xe[2]);
    altitude = Range - RE;

    if (fabs(altitude) < 0.001) break;

    if (altitude < 0.0) {
      tmin = time;
    }else{
      tmax = time;
    }

    Range_Rate = (Xe[0]*Ve[0] + Xe[1]*Ve[1] + Xe[2]*Ve[2]) / Range;    
    dt = - (altitude / Range_Rate);

    time += dt;

    if ((time > tmax) || (time < tmin)) {
      time = (tmax+tmin)/2.0;
    }

    nits++;

    if (nits > 1000) {
      time = -2.0;
      lat  = 0.0;
      lon  = 0.0;
      return;
    }

  }

//...... convert the ECI position to latitude and longitude

  xyz_to_latlon(time, Xe, lat, lon);

}
