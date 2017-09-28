// stereo_track.H header file

#ifndef stereo_track_object
#define stereo_track_object

#include "convert.H"
#include "xqueue.H"
#include "detection.H"

/************************************************************************
*	stereo_track object 						*
************************************************************************/
class C_STEREO_TRACK : public C_CONVERT {

  private:

//...... first variable must be window ..................................
    double window;		// time window for detections
//.......................................................................

    double told;		// reference time for the polynomial
    double tcurrent;		// latest detection time
    double tdetection;		// latest detection time for truth
    double tupdate;		// track update time
    int balistic;		// balistic flag

    int object_type;		// object type of the tracked object
    int object_id;		// local id of the tracked object
    int object_node;		// node of the tracked object

    double a[3];		// zeroeth order polynomial coefficients
    double b[3];		// first order polynomial coefficients
    double c[3];		// second order polynomial coefficients
    double d[3];		// third order polynomial coefficients
    double e[3];		// fourth order polynomial coefficients

    double rms;			// root mean squared
    int weight_flag;		// 1 means weight current, 0 means weight old
    int dummy_pad;		// pad for the KSR1

//...... last variable must be detections ...............................
    C_XQUEUE detections;	// list of detections
//.......................................................................

  protected:

  public:

    C_STEREO_TRACK();
    void init(C_STEREO_TRACK *st);
    char *get_start_pointer() {return (char *)&window;}
    int get_st_bytes() {return (int)((char *)&detections - (char *)&window);}

    void reset() {detections.reset(); tcurrent = -1.0; tupdate = -1.0e20;}

    void set_window(double w) {window = w;}
    double get_window() {return window;}
    double get_told() {return told;}

    void set_weight_current() {weight_flag = 1;}
    void set_weight_old() {weight_flag = 0;}

    void set_tcurrent(double t) {tcurrent = t;}
    double get_tcurrent() {return tcurrent;}

    void set_tupdate(double t) {tupdate = t;}
    double get_tupdate() {return tupdate;}

    void set_tdetection(double t) {tdetection = t;}
    double get_tdetection() {return tdetection;}

    C_XQUEUE *get_detections() {return &detections;}
    void get_pos_vel(double t, double X[3], double V[3]);
    void update_state(int print_flag);
    virtual void print();

    double get_rms() {return rms;}
    double get_total_error() {return rms;}
    double get_Xerror() {return rms;}
    double get_Verror() {return rms / (tcurrent - told + 0.001);}

    int get_balistic() {return balistic;}
    int get_ballistic() {return balistic;}
    void set_balistic(int ba) {balistic = ba;}
    void set_balistic() {balistic = 1;}
    void reset_balistic() {balistic = 0;}
    void set_ballistic(int ba) {balistic = ba;}
    void set_ballistic() {balistic = 1;}
    void reset_ballistic() {balistic = 0;}

    int get_n_detections() {return detections.get_length();}
    void get_true_state(double P[3], double V[3]);
    void get_est_state(double P[3], double V[3]);

    void set_object_type(int ot) {object_type = ot;}
    int get_object_type() {return object_type;}

    void set_object_id(int oi) {object_id = oi;}
    int get_object_id() {return object_id;}

    void set_object_node(int on) {object_node = on;}
    int get_object_node() {return object_node;}

    int get_poly_order();

    void launch_position(double &time, double &lat, double &lon);

};


#endif
