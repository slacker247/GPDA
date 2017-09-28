// int_bus.H header file

#ifndef intbus_object
#define intbus_object

#include <math.h>
#include "base_space.H"
#include "spline6.H"

#define NSPIBUS 3

/************************************************************************
*	int_bus object 							*
************************************************************************/
class C_INT_BUS : public C_BASE_SPACE {

  private:

    double resolution;		// resolution for integration
    double maxstep;		// maximum time step
    double V0[3];		// velocity at the start of the bus
    double Vend[3];		// velocity at impact
    C_SPLINE6 splines[NSPIBUS];	// array of splines for bus
    int nsplines;		// number of splines for bus
    double tspline[NSPIBUS];	// time per spline
    double massrocket;		// mass of the rocket
    int nrvs;			// number of rvs

  protected:

  public:

    C_INT_BUS();
    void set_resolution(double rs) {resolution = rs;}
    void set_maxstep(double ms) {maxstep = ms;}
    void set_massrocket(double mr) {massrocket = mr;}
    double get_massrocket(double ) {return massrocket;}
    void integrate();
    void init_values();
    void update_impact() {;}

//...... virtual functions

    virtual void check_spline6();
    virtual void get_pos_vel(double t, double rt[3], double vt[3]);
    virtual int get_n_splines() {return nsplines;}
    virtual C_SPLINE6 *get_splines() {return splines;}
    virtual void create_rvs(int n) {nrvs = n;}
    virtual void init(double r[3], double v[3]);
    virtual double get_timp() {return Tend;}
    virtual double *get_imp_pos() {return Rend;}
    virtual double *get_imp_vel() {return Vend;}

};


#endif
