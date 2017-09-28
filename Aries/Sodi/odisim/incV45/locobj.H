// locobj.H header file

#ifndef locobj_object
#define locobj_object

#include "simobj.H"
#include "convert.H"

/************************************************************************
*	locobj object - this object knows how to convert lat,lon to	*
*			 xyz, etc. It needs to know gsam.		*
************************************************************************/
class C_LOCOBJ : public C_SIMOBJ {

  private:

  protected:

    int glob_gridid;		// global grid id
    double start_time;		// starting time for object
    double start_position[3];	// xyz position of object (valid at start)
    double cruise_vel;		// cruise velocity for object
    double max_vel;		// maximum velocity for object
    double max_alt;		// maximum altitude for object
    double flyout_range;	// range for flyout in km
    int icon;			// icon id for simulation
    int com_local_id;		// local com id for this object
    int alive;			// flag telling if the object is alive

    static int done;
    static double VMAX;		// maximum velocity of all objects
    static C_CONVERT *convert;	// latlon conversion object

  public:

    C_LOCOBJ();			// constructor

//...... set / get methods

    void set_com_local_id(int cli) {com_local_id = cli;}
    int get_com_local_id() {return com_local_id;}

    void set_start_time(double st) {start_time = st;}
    double get_start_time() {return start_time;}

    void set_start_position(double R[3]);
    double *get_start_position() {return start_position;}

    void set_max_vel(double vm) {max_vel = vm; if (vm > VMAX) VMAX = vm;}
    double get_max_vel() {return max_vel;}

    void set_VMAX(double vm) {VMAX = vm;}
    double get_VMAX() {return VMAX;}

    void set_flyout_range(double fr) {flyout_range = fr;}
    double get_flyout_range() {return flyout_range;}

    void set_icon(int i) {icon = i;}
    int get_icon() {return icon;}

    void set_glob_gridid(int glob_gid) {glob_gridid = glob_gid;}
    int get_glob_gridid() {return glob_gridid;}

    void set_alive(int a) {alive = a;}
    int get_alive() {return alive;}

    virtual double get_rmax() {return 0.0;}
    virtual double get_rmin() {return 0.0;}

//...... conversion methods

    void latlon_to_xyz(double lat, double lon, double x[3])
	{convert->latlon_to_xyz(lat, lon, x);}

    void latlon_to_xyz(double t, double lat, double lon, double x[3])
	{convert->latlon_to_xyz(t, lat, lon, x);}


    void xyz_to_latlon(double x[3], double &lat, double &lon)
	{convert->xyz_to_latlon(x, lat, lon);}

    void xyz_to_latlon(double t, double x[3], double &lat, double &lon)
	{convert->xyz_to_latlon(t, x, lat, lon);}

    void ecr_to_eci(double t, double x[3], double v[3])
	{convert->ecr_to_eci(t, x, v);}

    void eci_to_ecr(double t, double x[3], double v[3])
	{convert->eci_to_ecr(t, x, v);}

//...... exchange methods

    void exchange_glob_gridid(int &glob_gid) {EXCHANGE(glob_gridid,glob_gid);}
    void exchange_alive(int &a) {EXCHANGE(alive,a);}

//...... random position methods

    void random_position(double R[3]);
    void random_next_position(double R0[3], double R1[3]);

//...... virtual functions

    virtual void get_pos_vel(double t, double pos[3], double vel[3]);
    virtual void fast_pos_vel(double t, double pos[3], double vel[3]) {
      				get_pos_vel(t,pos,vel);}
    virtual double get_tdist(double dist) {return (dist/299792.458);}

};


#endif
