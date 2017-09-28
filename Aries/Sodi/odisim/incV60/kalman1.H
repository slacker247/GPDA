// kalman1.H header file

#ifndef kalman1_object
#define Kalman1_object

#include <math.h>

#ifndef INFINITY
#define INFINITY 1.0e20
#endif

/************************************************************************
*	Kalman Filter Object - Kinematic				*
************************************************************************/
class C_KALMAN1 {

  private:

    double DT;		// time step between filters
    double R[1];	// measurement noise
    double X[3];	// state vector of the filter
    double Z[1];	// measurement vector
    double P[3][3];	// covariance matrix
    double Q[3][3];	// system noise
    double K[3][1];	// Kalman gain
    double H[1][3];	// measurement matrix
    double Phi[3][3];	// transition matrix
    double U[3];	// unexpected (but assumed detected) maneuver
    int gravity;	// gravity flag for free flight
    double L2;		// gravity term
    int cycle;

  protected:

  public:

    C_KALMAN1();
    void reset_cycle() {cycle = 0;}
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

