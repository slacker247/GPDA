// sensor_model.H header file

#ifndef sensor_model_object
#define sensor_model_object

#include "parser.H"
#include "xqueue.H"
#include "random.H"
#include "rbq.H"

/************************************************************************
*	sensor_model object 						*
************************************************************************/
class C_SENSOR_MODEL {

  private:

  protected:

    double scan_time;		// scan time of sensor in sec.
    double rmin;		// minimum range in km
    double rmax;		// maximum range in km
    double rmaxcoverage;	// maximum range in km for coverage
    double rdotmin;		// minimum Rdot in km/sec
    double signal;		// signal threshold for detection
    double luminosity;		// luminosity threshold for detection
    int fixed;			// fixed sensor flag (not moving)
    int los;			// line of sight flag
    double error;		// position error parameter for detection
    char *type;			// name of sensor type
    int icon;			// icon for sensor type
    int sensor_id;		// sensor unique id

    C_RANDOM *random;		// random number generator
    static C_RBQ *rbq;		// rollback queue for optimistic processing

    static int n_movers;	// total number of movers
    static int n_prox;		// number of movers that passed the cuts
    static int n_scans;		// number of scans

    int track_flag;		// flag for updating tracks
    int fuse_flag;		// flag for track fusion
    int print_flag;		// flag for printing

  public:

    C_SENSOR_MODEL();
    void init(C_BASETYPE *sensor);
    void set_rbq(C_RBQ *r) {rbq = r;}
    void set_random(C_RANDOM *r) {random = r;}

    void set_track_flag() {track_flag = 1;}
    void reset_track_flag() {track_flag = 0;}
    int get_track_flag() {return track_flag;}

    void set_fuse_flag() {fuse_flag = 1;}
    void reset_fuse_flag() {fuse_flag = 0;}
    int get_fuse_flag() {return fuse_flag;}

    void set_print_flag() {print_flag = 1;}
    void reset_print_flag() {print_flag = 0;}
    int get_print_flag() {return print_flag;}

    void set_sensor_id(int i) {sensor_id = i;}
    int get_sensor_id() {return sensor_id;}

    void set_icon(int i) {icon = i;}
    int get_icon() {return icon;}

    double get_scan_time() {return scan_time;}
    double get_rmin() {return rmin;}
    double get_rmax() {return rmax;}
    double get_rdotmin() {return rdotmin;}
    double get_signal() {return signal;}
    double get_luminosity() {return luminosity;}
    int get_fixed() {return fixed;}
    int get_los() {return los;}
    double get_error() {return error;}
    char *get_type() {return type;}

    void set_rmax(double rm) {rmax = rm;}
    void set_rmaxcoverage(double rmc) {rmaxcoverage = rmc;}
    double get_rmaxcoverage() {return rmaxcoverage;}

    int check_los(double Xs[3], double Xt[3]);

    virtual void scan(	double time,
			double Ps[3],
			double Vs[3],
			C_XQUEUE *movers,
			C_XQUEUE *old_tracks,
			C_XQUEUE *new_tracks	);

    virtual void eye_scan(	double time,
			double Ps[3],
			double Vs[3],
			C_XQUEUE *movers,
			C_XQUEUE *tracks	);

    virtual void generate_detections(	double time,
					double Ps[3],
					double Vs[3],
					C_XQUEUE *movers,
					C_XQUEUE *det);

    virtual void update_tracks(		double time,
					C_XQUEUE *det,
					C_XQUEUE *tracks);

    void set_prox_statistics(int nm, int np) {
		n_movers += nm; n_prox += np; n_scans++;}
    int get_n_scans() {return n_scans;}
    double get_prox_statistics() {return (double(n_prox)/double(n_movers));}

};


#endif
