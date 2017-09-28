// gridman.H header file

#ifndef gridman_object
#define gridman_object

#include "grid.H"
#include "parsman.H"
#include "xqueue.H"

/************************************************************************
*	gridman object - manager for space objects covering the earth	*
************************************************************************/
class C_GRIDMAN : public C_PARSMAN {

  private:

    double d;      	        // distance of side of gridman
    int N;        	        // number of delta longitudes
    double dtheta;		// delta theta
    int tot;			// total number of gridman elements
    int mytot;			// total number of gridman elements on my node
    int *M;			// number of delta phis for each theta
    int *Mcum;			// cumulative number of delta phis for theta
    double *dphi;		// delta phi for each delta theta
    double grid_resolution;	// resolution for grids
    double sensor_resolution;	// resolution for sensing
    double time_resolution;	// basic time resolution
    double delay_g2m;		// delay from grid to mover
    double delay_m2g;		// delay from grid to mover
    double delay_m2e;		// delay from mover to sensor
    double delay_s2g;		// delay from sensor to grid
    C_GRID *grid;		// local grids
    double mem_time;		// memory update time
    int show_mem;		// memory update flag

  protected:

  public:

    C_GRIDMAN();
    void get_nm(double theta, double phi, int &n, int &m);
    void get_pt(double lat, double lon, double &theta, double &phi);
    void get_idnd(double lat, double lon, int &nd, int &eid);
    int get_glob_gid(int n, int i) {return (i*N_NODES + n);}

    double get_grid_resolution() {return grid_resolution;}
    double get_sensor_resolution() {return sensor_resolution;}
    double get_time_resolution() {return time_resolution;}

    double get_delay_m2g() {return delay_m2g;}
    double get_delay_g2m() {return delay_g2m;}
    double get_delay_m2e() {return delay_m2e;}
    double get_delay_s2g() {return delay_s2g;}
    double get_delay() {return time_resolution;}

    void get_gridids(double lat, double lon, double size, C_QUEUE *q);
    void get_gridids(double lat, double lon, double size, C_XQUEUE *q);
    void print();

    void set_mem_time(double t) {mem_time = t;}
    double get_mem_time() {return mem_time;}
    void set_show_mem(int sm) {show_mem = sm;}
    int get_show_mem() {return show_mem;}

    virtual void init_events();

};


#endif
