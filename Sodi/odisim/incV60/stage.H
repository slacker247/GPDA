// stage.H header file

#ifndef stag_object
#define stag_object

#include <math.h>
#include "base_stage.H"

/************************************************************************
*	stage object 							*
************************************************************************/
class C_STAGE : public C_BASE_STAGE {

  private:

    double C[3];		// useful vector
    double D[3];		// useful vector
    double E[3];		// useful vector
    double FF[3];		// useful vector
    double mu;			// the mass ratio Mf/M0
    double c1;			// Tb*T/Mr
    double c2;			// - GM / (r*r*r)

  protected:

  public:

    C_STAGE();

    virtual double get_tburn() {return Tb;}
    virtual void init_values();
    virtual double get_duration() {return (Tb+Tcoast);}
    virtual void get_pos_vel(double t, double rt[3], double vt[3]);

};


#endif
