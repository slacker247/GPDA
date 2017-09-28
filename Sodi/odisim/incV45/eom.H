// eom.H header file

#ifndef eom_object
#define eom_object

#include "convert.H"

/************************************************************************
*	eom object 							*
************************************************************************/
class C_EOM : public C_CONVERT {

  private:

  protected:

    int object_node;
    int object_type;
    int object_id;
    int glob_gid;
    int unique_id;
    int icon;
    double cross_section;
    double luminosity;
    int ballistic;
    int sequence;
    double vel;

    double R0[3];		// initial position
    double Rend[3];		// final position
    double t0;			// initial time
    double TT;			// duration of motion
    double Tend;		// end time of motion
    double delay;		// time delay for sending messages

    int ECI;			// flag for ECI (or ECR) coordinates

    int eom_allign;		// allign

  public:

    C_EOM() {ballistic = 0;}

    char *get_data_address() {return (char *)&link;}

    void set_object_node(int nd) {object_node = nd;}
    void set_object_type(int ty) {object_type = ty;}
    void set_object_id(int i) {object_id = i;}
    void set_cross_section(double cs) {cross_section = cs;}
    void set_luminosity(double l) {luminosity = l;}
    void set_ballistic(int b) {ballistic = b;}
    void set_glob_gid(int g) {glob_gid = g;}
    void set_unique_id(int ui) {unique_id = ui;}
    void set_icon(int ic) {icon = ic;}
    void set_sequence(int s) {sequence = s;}
    void set_velocity(double v) {vel = v;}
    void set_delay(double d) {delay = d;}
    void set_ECI(int e) {ECI = e;}
    void set_endtime(double te) {Tend = te;}

    int get_object_node() {return object_node;}
    int get_object_type() {return object_type;}
    int get_object_id() {return object_id;}
    double get_cross_section() {return cross_section;}
    double get_luminosity() {return luminosity;}
    int get_ballistic() {return ballistic;}
    int get_glob_gid() {return glob_gid;}
    int get_unique_id() {return unique_id;}
    int get_icon() {return icon;}
    int get_sequence() {return sequence;}
    double get_velocity() {return vel;}
    double get_delay() {return delay;};
    int get_ECI() {return ECI;};

    void set_start_time(double tstart) {t0 = tstart;} 

    virtual void get_pos_vel(double t, double R[3], double V[3]);
    virtual void change_endtime(double te);
    virtual double get_tdist(double dist);
    virtual double get_start_time();
    virtual double get_endtime();
    virtual double *get_startpos();
    virtual double *get_endpos();

    virtual void set_duration(double d) {TT = d;}
    virtual double get_duration() {return TT;}

    virtual void print();

};


#endif
