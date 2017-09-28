// kalman1.H header file

#ifndef kalman3_object
#define Kalman3_object

#include <math.h>

#ifndef INFINITY
#define INFINITY 1.0e20
#endif

/************************************************************************
*	Kalman Filter Object - Kinematic				*
************************************************************************/
class C_KALMAN3 {

  private:

    double DT;		// time step between filters
    double R[1];	// measurement noise
    double X[4];	// state vector of the filter
    double Z[1];	// measurement vector
    double P[4][4];	// covariance matrix
    double Q[4][4];	// system noise
    double K[4][1];	// Kalman gain
    double H[1][4];	// measurement matrix
    double Phi[4][4];	// transition matrix
    double U[4];	// unexpected (but assumed detected) maneuver
    int gravity;	// gravity flag for free flight
    double L2;		// gravity term
    int cycle;		// cycle number
    double SIGMA;	// Jerk decay factor

  protected:

  public:

    C_KALMAN3();
    void reset_cycle() {cycle = 0;}
    void set_SIGMA(double s) {SIGMA = s;}
    void set_DT(double t);
    void set_R(double r0) {R[0] = r0;}
    void set_U(double u);
    void set_L2(double l2) {L2 = l2;}
    void set_gravity() {gravity = 1;}
    void reset_gravity() {gravity = 0;}
    double *get_state() {return X;}
    double get_error() {return P[0][0];}
    
    void update(double Xmeas);

};


#endif

