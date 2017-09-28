// detection.H header file

#ifndef detobject
#define detobject

#include "sq_item.H"
#define VALID_ANGLE 1
#define VALID_RANGE 2
#define VALID_RANGE_RATE 4

/************************************************************************
*	detection object 						*
************************************************************************/
class C_DETECTION : public C_SQ_ITEM {

  private:

    double S[3];	// position of the sensor
    double SV[3];	// velocity of the sensor
    double V[3];	// unit vector from the sensor to the target
    double Xtrue[3];	// true position of the detection
    double Vtrue[3];	// true velocity of the detection
    double Sigma;	// error associated with the detection
    double Sigma_r;	// error associated with the range
    double Sigma_rr;	// error associated with the range rate
    double range;	// range associated with the detection
    double range_rate;	// range rate associated with the detection
    int sensor_id;	// sensor unique id
    int ballistic;	// ballistic flag
    int valid_flag;	// flag for valid measurements

    int object_type;	// object type of the tracked object
    int object_id;	// local id of the tracked object
    int object_node;	// node of the tracked object

  protected:

  public:

    C_DETECTION(){;}

    void set_detection(double t, int tid, int sid, double xt[3], double vt[3],
	double s[3], double sv[3], double v[3], double r, double rr,
	double sigma, int b) {

	time_tag = t;
	range = r;
	range_rate = rr;
	Sigma = sigma;
	id = tid;
	sensor_id = sid;
	ballistic = b;

	Xtrue[0] = xt[0];
	Xtrue[1] = xt[1];
	Xtrue[2] = xt[2];

	Vtrue[0] = vt[0];
	Vtrue[1] = vt[1];
	Vtrue[2] = vt[2];

	S[0] = s[0];
	S[1] = s[1];
	S[2] = s[2];

	SV[0] = sv[0];
	SV[1] = sv[1];
	SV[2] = sv[2];

	V[0] = v[0];
	V[1] = v[1];
	V[2] = v[2];

        Sigma_r = 0.0;
        Sigma_rr = 0.0;
        valid_flag = VALID_ANGLE;
        if (range > 0.0) valid_flag |= VALID_RANGE;

    }

    double *get_S() {return S;}
    double *get_SV() {return SV;}
    double *get_V() {return V;}
    double *get_Xtrue() {return Xtrue;}
    double *get_Vtrue() {return Vtrue;}
    double get_range() {return range;}
    double get_range_rate() {return range_rate;}
    double get_Sigma() {return Sigma;}
    double get_time() {return time_tag;}
    int get_ballistic() {return ballistic;}
    int get_target_id() {return id;}
    int get_sensor_id() {return sensor_id;}

    void set_valid_range_rate() {valid_flag |= VALID_RANGE_RATE;}
    int get_valid_range_rate() {return (valid_flag & VALID_RANGE_RATE);}

    void set_Sigma_r(double sr) {Sigma_r = sr;}
    double get_Sigma_r() {return Sigma_r;}

    void set_Sigma_rr(double srr) {Sigma_rr = srr;}
    double get_Sigma_rr() {return Sigma_rr;}

    void set_object_type(int ot) {object_type = ot;}
    int get_object_type() {return object_type;}

    void set_object_id(int oi) {object_id = oi;}
    int get_object_id() {return object_id;}

    void set_object_node(int on) {object_node = on;}
    int get_object_node() {return object_node;}

};


#endif
