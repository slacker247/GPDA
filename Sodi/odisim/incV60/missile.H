// missile.H header file

#ifndef misil_object
#define misil_object

#include "kepler.H"
#include "bus.H"
#include "stage.H"
#include "int_stage.H"
#include "int_bus.H"
#include "eom.H"
#include "queue.H"

#ifndef MISSALONE
#include "rbq.H"
#endif

/************************************************************************
*	missile object 							*
************************************************************************/
class C_MISSILE {

  private:

    int nstages;
    int stage_type;
    double t0;
    double timp;
    double coe, soe;
    C_EOM **eom;
    int min_index;
    double *ta;
    double *ra;
    double the1, the2, the3;
    double err1, err2, err3;
    int ntries;
    double max_range;
    double min_range;
    int ECI;

    int nr;
    int na;
    double resolution;
    double max_altitude;
    C_QUEUE **dr;

  protected:

#ifndef MISSALONE
    static C_RBQ *rbq;
#else
    static void *rbq;
#endif

    void create_stages(int n);
    void create_int_stages(int n);

    void create_rvs(int n);
    void rotate(double rt[3], double vt[3]);
    void unrotate(double rt[3], double vt[3]);
    void set_rotate(double t);
    void init_bus();
    void init_GBI(C_QUEUE **Drng, int Nr, int Na, double Malt, double res) {
	dr = Drng; nr = Nr; na = Na; max_altitude = Malt; resolution = res;}

  public:

    C_MISSILE();
    void set_ECI() {ECI = 1; fill_ECI();}
    void set_ECR() {ECI = 0; fill_ECI();}
    int get_ECI() {return ECI;}
    void fill_ECI();
    int get_nstages() {return nstages;}

    C_STAGE *get_stage(int s) {return (C_STAGE *)eom[s];}
    C_BASE_STAGE *get_base_stage(int s) {return (C_BASE_STAGE *)eom[s];}
    C_INT_STAGE *get_int_stage(int s) {return (C_INT_STAGE *)eom[s];}

    C_BASE_SPACE *get_bus() {return (C_BASE_SPACE *)eom[nstages];}
    void init_t0(double t);
    double get_t0() {return t0;}
    double get_start_time() {return t0;}
    double get_end_time() {return eom[nstages]->get_endtime();}
    void range(double Range_Angle[180], double Time_Angle[180], int &done);
    void down_range(C_QUEUE **&Down_Range, int Nr, int Na,
			double Malt, double res);
    double get_theta_gbi(double r0[3], double r1[3], double &time);
    double get_theta_gbi(double latgbi, double longbi,
			C_EOM *eom, double launch_time, double &kill_time);
    double aim_gbi(double current_time, double lat, double lon, C_EOM *eom);
    double mindist(double current_time, double lat, double lon,
	C_EOM *eom, double &dist);

    int aim_eci(double t, double r0[3], double r1[3], 
                double latf, double lonf, double apogee, double tof);
    int aim_latlon(double t, double lati, double loni, double latf,
			double lonf, double apogee, double tof);
    double set_aim_angles(double theta, double &phi, double vscale,
			  double r0[3], double rimp[3], double latf,
                          double lonf, double apogee, double tof);
    double get_timp() {return (timp+t0);}

    double get_max_range() {return max_range;}
    double get_min_range() {return min_range;}

    char *make_buff(int &size);
    void init_missile(char *buff);
    double next_theta(double error, double theta);

    void latlon_to_xyz(double lat, double lon, double x[3]) {
	eom[0]->latlon_to_xyz(lat, lon, x);}

    void latlon_to_xyz(double t, double lat, double lon, double x[3]) {
	eom[0]->latlon_to_xyz(t, lat, lon, x);}

    void xyz_to_latlon(double x[3], double &lat, double &lon) {
	eom[0]->xyz_to_latlon(x, lat, lon);}

    void xyz_to_latlon(double t, double x[3], double &lat, double &lon) {
	eom[0]->xyz_to_latlon(t, x, lat, lon);}

    virtual void get_pos_vel(double t, double rt[3], double vt[3]);

#ifndef MISSALONE
    void set_rbq(C_RBQ *r) {rbq = r;}
#endif


};


#endif
