// greatcirc.H header file

#ifndef greatcirc_object
#define greatcirc_object

#include "eom.H"

/************************************************************************
*	greatcirc object 						*
************************************************************************/
class C_GREATCIRC : public C_EOM {

  private:

    double altitude;		// initial altitude
    double Rrot[3];		// rotation axis unit vector
    double Valt;		// velocity up or down
    double Vcirc;		// velocity along the rmin circle
    double xmax;		// 2sin(theta/2)
    double theta;		// the angle between R0 and R1
    double sintheta;		// sine of the angle between R0 and R1
    double costheta;		// cosine of the angle between R0 and R1
    double R1[3];		// final position

  protected:

  public:

//    C_GREATCIRC();

    void set_course(	double lat0, double lon0, double alt0,
			double lat1, double lon1, double alt1,
			double velocity, double tstart);

    void set_course_TT(	double lat0, double lon0, double alt0,
			double lat1, double lon1, double alt1,
			double tstart, double tend);

    void set_course(double Pos0[3],double Pos1[3],double velocity,double tstart);

    virtual void get_pos_vel(double t, double R[3], double V[3]);
    virtual double get_tdist(double dist);
    virtual double get_start_time() {return t0;}
    virtual double get_endtime() {return (Tend);}
    virtual double *get_startpos() {return R0;}
    virtual double *get_endpos() {return Rend;}

};


#endif
