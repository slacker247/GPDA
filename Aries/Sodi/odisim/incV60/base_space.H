// base_space.H header file

#ifndef bspce_object
#define bspce_object

#include "eom.H"
#include "spline6.H"

#define ATM_ALTITUDE 100.0

/************************************************************************
*	base_space object 						*
************************************************************************/
class C_BASE_SPACE : public C_EOM {

  private:

  protected:

    double area;	// area of the rockets face for friction
    double coe;		// cos(OMEGAE*t0)
    double soe;		// sin(OMEGAE*t0)

  public:

    C_BASE_SPACE() {area = 0.0;}
    void get_friction(double r[3], double v[3],
			double massrocket, double friction[3]);

    void set_rotate(double ce, double se) {coe = ce; soe = se;}
    void rotate(double rt[3], double vt[3]);

//...... virtual functions

    virtual void create_rvs(int) {;}
    virtual void init(double [3], double [3]) {;}
    virtual void update_impact() {;}
    virtual double get_timp() {return 0.0;}
    virtual double *get_imp_pos() {return NULL;}
    virtual double *get_imp_vel() {return NULL;}
    virtual void check_spline6() {;}
    virtual C_SPLINE6 *get_splines() {return NULL;}
    virtual int get_n_splines() {return 0;}

};


#endif
