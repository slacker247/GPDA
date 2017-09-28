// spline6.H header file

#ifndef spl6_object
#define spl6_object

#include "eom.H"

/************************************************************************
*	spline6 object fits a 5th order polynomial to data		*
************************************************************************/
class C_SPLINE6 : public C_EOM {

  private:

//    double t0;			// initial time
    double t1;			// final time
    double a_spline[3];		// spline parameters;
    double b_spline[3];		// spline parameters;
    double c_spline[3];		// spline parameters;
    double init_pos[3];		// initial acceleration for spline
    double init_vel[3];		// initial acceleration for spline
    double init_acc[3];		// initial acceleration for spline

    double coe;			// cos(OMEGAE*t0)
    double soe;			// sin(OMEGAE*t0)

    double mandir[3];		// unit vector for manuever direction
    double ngs;			// number of gs for manuever
    int sman;			// number of s turns in manuever

  protected:

  public:

    C_SPLINE6() {sman = 0; coe=1.0; soe=0.0;}

    void fit_spline6(	double T0, double T1,
			double X0[3], double V0[3], double A0[3],
			double X1[3], double V1[3], double A1[3]);

    virtual void get_pos_vel(double t, double rt[3], double vt[3]);
    void getposvel(double t, double rt[3], double vt[3]);

    void set_rotate(double ce, double se) {coe = ce; soe = se;}
    void rotate(double rt[3], double vt[3]);

    void man_pos_vel(double t, double rt[3], double vt[3]);
    void init_maneuver(double ng, int nst);

};


#endif
