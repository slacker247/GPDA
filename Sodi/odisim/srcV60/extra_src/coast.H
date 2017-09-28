// coast.H header file

#ifndef coast_object
#define coast_object

#include "fly.H"

/************************************************************************
*	coast object 							*
************************************************************************/
class C_COAST : public C_FLY {

  private:

    double R0[3];		// position at start of coast
    double V0[3];		// velocity at start of coast
    double Tc;			// coast time
    double c2;			// - GM / (r*r*r)

  protected:


  public:

    C_COAST();
    void init_pars(double tcoast) {Tc = tcoast;}
    void init_R0(double r0[3]);
    void init_V0(double v0[3]) {V0[0] = v0[0]; V0[1] = v0[1]; V0[2] = v0[2];}

    virtual double get_duration();
    virtual void get_pos_vel(double t, double rt[3], double vt[3]);

};


#endif
