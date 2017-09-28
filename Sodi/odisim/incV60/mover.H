// mover.H header file

#ifndef mover_object
#define mover_object

#include "sensobj.H"
#include "gridid.H"
#include "eom.H"

/************************************************************************
*	mover object 							*
************************************************************************/
class C_MOVER : public C_SENSOBJ {

  private:

  protected:

    C_XQUEUE *script;		// equation of motion script
    C_XQUEUE sensors;		// list of sensors who are sensing me
    double tsensors;		// time of last sensor update
    double script_time;		// time that script was generated

    int rancourse;		// random course flag
    int fixed_script;		// fixed_script_flag
    double cross_section;	// radar cross section
    double luminosity;		// burning brightness
    C_GRIDID gridid;		// grid id object
    int *node_flag;		// flag for node usage
    double eoman_delay;		// delay from mover to eoman

    static int done;		// done flag
    static int magnet;		// magnet flag directs aircraft to L.A.
    static int size_node_flag;	// size of flag for node usage

  public:

    C_MOVER();

    C_XQUEUE *get_sensors() {return &sensors;}
    int get_nsensors() {return sensors.get_length();}
    double get_tsensors() {return tsensors;}
    void exchange_tsensors(double &t) {EXCHANGE(tsensors,t);}

//...... node flag stuff

    int get_node_flag(int n, int *nflg);
    int get_node_flag(int n) {return get_node_flag(n,node_flag);}

    void set_node_flag(int n, int *nflg);
    void set_node_flag(int n) {set_node_flag(n,node_flag);}

    void reset_node_flag(int n, int *nflg);
    void reset_node_flag(int n) {reset_node_flag(n,node_flag);}
    void reset_node_flag(int *nflg);

    void copy_node_flag(int *n);
    void exchange_node_flag(int *&n);
    int check_node(int n);


    double get_eoman_delay() {return eoman_delay;}
    void set_eoman_delay(double d) {eoman_delay = d;}
    void exchange_eoman_delay(double &d) {EXCHANGE(eoman_delay,d);}

    void set_magnet(int m) {magnet = m;}

    int check_segment(C_EOM *e, double t);

    void random_init();
    void fill_random_segment(double t, double R[3], C_EOM *e);
    int get_rancourse() {return rancourse;}
    void set_rancourse() {rancourse = 1;}
    int get_fixed_script() {return fixed_script;}
    void set_fixed_script() {fixed_script = 1;}
    void reset_fixed_script() {fixed_script = 0;}

    C_XQUEUE *get_script() {return script;}
    void exchange_script(C_XQUEUE *&s);
    void set_script_time(double st) {script_time = st;}
    double get_script_time() {return script_time;}
    void exchange_script_time(double &t) {EXCHANGE(script_time,t);}

    C_EOM *get_current_segment();
    C_EOM *get_next_segment();
    C_EOM *get_last_segment();
    C_EOM *get_eoman_segment(double t, double &d);

    virtual void get_pos_vel(double t, double X[3], double V[3]);
    virtual double get_tdist(double dist);

    C_GRIDID *get_gridid() {return &gridid;}
//    void exchange_gridid(void *&g) {EXCHANGE(gridid,g);}

};


#endif
