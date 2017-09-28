// int_stage.H header file

#ifndef intstag_object
#define intstag_object

#include <math.h>
#include "base_stage.H"
#include "spline6.H"

#define NSP 5

/************************************************************************
*	int_stage object 						*
************************************************************************/
class C_INT_STAGE : public C_BASE_STAGE {

  private:

    double resolution;		// resolution for integration
    double maxstep;		// maximum time step
    double N[3];		// orthogonal vector in thrust plane
    double Vend[3];		// velocity at the end of boosting
    C_SPLINE6 splines[NSP];	// array of splines for stage
    int nsplines;		// number of splines for stage
    double tspline;		// time per spline

  protected:

  public:

    C_INT_STAGE();

    void set_resolution(double rs) {resolution = rs;}
    void set_maxstep(double ms) {maxstep = ms;}
    void integrate();
    void get_thrust(double t, double St[3]);
    void init_values();
    virtual void check_spline6();
    virtual void get_pos_vel(double t, double rt[3], double vt[3]);
    virtual int get_n_splines() {return nsplines;}
    virtual C_SPLINE6 *get_splines() {return splines;}

};


#endif
