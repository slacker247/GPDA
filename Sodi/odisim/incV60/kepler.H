// kepler.H header file

#ifndef kepler_object
#define kepler_object

#include "base_space.H"

/************************************************************************
*	kepler object - contains kepler parameters, etc.		*
************************************************************************/
class C_KEPLER : public C_BASE_SPACE {

  private:

    double a;			// semi-major axis
    double e;			// eccentricity
    double i;			// inclination
    double o;			// big omega
    double w;			// little omega
    double m;			// mean anomaly

    double init_pos[3];
    double init_vel[3];
    double init_acc[3];

    double cur_pos[3];
    double cur_vel[3];
    double tcur;

    double imp_pos[3];
    double imp_vel[3];
    double timp;			// impact time

    double a_spline[3];		// spline parameters;
    double b_spline[3];		// spline parameters;
    double c_spline[3];		// spline parameters;

    double man;			// temp mean anomily
    double truean;		// true anomily
    double ean;
    double p;
    double av;                   // angular velocity
    double si,ci;                // sin and cos of inclination
    double so,co;                // sin and cos of big omega
    double sw,cw;                // sin and cos of little omega
    double soci,coci;            // so*si and co*ci

    int truan();

  protected:

  public:

    C_KEPLER();
    void integrate_impact();

    void set_rotate(double ce, double se) {coe = ce; soe = se;}
    void rotate(double rt[3], double vt[3]);

    void fit_spline6();
    void get_fast_pos_vel(double t, double rt[3], double vt[3]);
    void integrate_pos_vel(double t, double rt[3], double vt[3]);
    void integrate();

//...... virtual functions

    virtual double get_duration();
    virtual void get_pos_vel(double t, double rt[3], double vt[3]);
    virtual void fast_pos_vel(double t, double rt[3], double vt[3]);
    virtual void init(double x[3], double v[3]);
    virtual void init_aeiowm(double aa, double ee, double ii, double oo,
			double ww, double mm);
    virtual void update_impact();
    virtual double *get_imp_pos() {return imp_pos;}
    virtual double *get_imp_vel() {return imp_vel;}
    virtual double get_timp() {return timp + t0;}
    virtual void check_spline6();

};


#endif
