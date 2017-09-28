// base_stage.H header file

#ifndef bstag_object
#define bstag_object

#include <math.h>
#include "spline6.H"
#include "base_space.H"

/************************************************************************
*	stage object 							*
************************************************************************/
class C_BASE_STAGE : public C_BASE_SPACE {

  protected:

    double V0[3];		// velocity at start of stage
    double R1[3];		// position at end of stage
    double V1[3];		// velocity at end of stage
    double S0[3];		// thrust angle at start of stage
    double Sb[3];		// thrust angle at end of stage
    int manuever;		// manuever flag
    double ngs;			// number of Gs for manuever
    double sman;		// number of S turns for maneuver
    double mantau;		// duration of an S turn for a manuever
    double mantau4;		// duration of piece of an S turn for a manuever
    double mandir[3];		// direction of manuever
    int variable_theta;		// variable theta flag
    double Ctheta;		// cosine of theta

    double Thrust;		// constant thrust throughout the stage
    double Tcor;		// constant thrust throughout the stage
    double Tb;			// burn time of stage
    double Mr;			// mass of the rocket including this stage
    double Mf;			// mass of the fuel for this stage
    double Ms;			// mass of the shell
    double Theta;		// thrust bending angle
    double Tcoast;		// coast time for after stage

  public:

    C_BASE_STAGE();
    void init_pars(double thrust, double thet, double tburn,
	double tcoast,double mrocket, double mfuel, double mshell);

    void init_maneuver(double ng, double nst) {ngs = ng; sman = nst;
					mantau = Tb/nst;
					mantau4 = mantau/4.0; manuever = 1;}

    void init_R0(double r0[3]) {R0[0] = r0[0]; R0[1] = r0[1]; R0[2] = r0[2];}
    void init_V0(double v0[3]) {V0[0] = v0[0]; V0[1] = v0[1]; V0[2] = v0[2];}
    void init_V1(double v1[3]) {V1[0] = v1[0]; V1[1] = v1[1]; V1[2] = v1[2];}
    void init_A0(double a0[3]) {S0[0] = a0[0]; S0[1] = a0[1]; S0[2] = a0[2];}
    void init_AB(double ab[3]) {Sb[0] = ab[0]; Sb[1] = ab[1]; Sb[2] = ab[2];}
    void init_ANGS(double a0[3], double Ab[3]);

    double get_mass() {return (Ms+Mf);}
    double get_massrocket(double t) {return (Mr - Mf*t/Tb);}
    double get_theta() {return Theta;}
    void set_theta(double theta) {Theta = theta; Ctheta = cos(Theta);}
    int get_variable_theta() {return variable_theta;}
    void get_V1(double v1[3]) {v1[0] = V1[0]; v1[1] = V1[1]; v1[2] = V1[2];}
    void get_R1(double r1[3]) {r1[0] = R1[0]; r1[1] = R1[1]; r1[2] = R1[2];}

    virtual double get_tburn() {return Tb;}
    virtual void init_values() {;}
    virtual double get_duration();
    virtual void coast(double t, double rt[3], double vt[3]);

    void man_pos_vel(double t, double rt[3], double vt[3]);

};


#endif
