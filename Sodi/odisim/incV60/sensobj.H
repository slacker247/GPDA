// sensobj.H header file

#ifndef sensobj_object
#define sensobj_object

#include "locobj.H"
#include "xqueue.H"
#include "sensor_model.H"

/************************************************************************
*	sensobj object 							*
************************************************************************/
class C_SENSOBJ : public C_LOCOBJ {

  private:

    C_SENSOR_MODEL *sensor_model;

    int scan_type;
    int scanning;
    int on_off;
    double last_tcom;
    double com_rate;
    double trackup_time;
    int trackup_node;

    C_XQUEUE *coverage;
    C_XQUEUE *movers;
    C_XQUEUE *tracks;
    C_XQUEUE *links;

  protected:

    int ring;
    int id_in_ring;
    int n_per_ring;

  public:

    C_SENSOBJ();
    void set_sensor_model(C_SENSOR_MODEL *sm);
    C_SENSOR_MODEL *get_sensor_model() {return sensor_model;}

    C_XQUEUE *get_movers() {return movers;}
    C_XQUEUE *get_coverage() {return coverage;}
    C_XQUEUE *get_tracks() {return tracks;}
    C_XQUEUE *get_links() {return links;}

    void exchange_links(C_XQUEUE *&l) {
	C_XQUEUE *temp;
	temp = links;
	links = l;
	l = temp;
    }

    void exchange_coverage(C_XQUEUE *&c) {
	C_XQUEUE *temp;
	temp = coverage;
	coverage = c;
	c = temp;
    }

    void exchange_tracks(C_XQUEUE *&trks) {
	C_XQUEUE *temp;
	temp = tracks;
	tracks = trks;
	trks = temp;
    }

    virtual void get_next_name(char *) {;}
    virtual void get_previous_name(char *) {;}

    void set_ring(int r) {ring = r;}
    int get_ring() {return ring;}

    void set_id_in_ring(int i) {id_in_ring = i;}
    int get_id_in_ring() {return id_in_ring;}

    void set_n_per_ring(int n) {n_per_ring = n;}
    int get_n_per_ring() {return n_per_ring;}

    void set_scan_type(int s) {scan_type = s;}
    int get_scan_type() {return scan_type;}

    void set_scanning(int s) {scanning = s;}
    int get_scanning() {return scanning;}
    void exchange_scanning(int &s) {EXCHANGE(scanning,s);}

    void set_trackup_time(double tup) {trackup_time = tup;}
    double get_trackup_time() {return trackup_time;}
    void exchange_trackup_time(double &tup) {EXCHANGE(trackup_time,tup);}

    void set_trackup_node(int tun) {trackup_node = tun;}
    int get_trackup_node() {return trackup_node;}
    void exchange_trackup_node(int &tun) {EXCHANGE(trackup_node,tun);}

    void set_on_off(int onof) {on_off = onof;}
    int get_on_off() {return on_off;}
    void exchange_on_off(int &onof) {EXCHANGE(onof,on_off);}

    void set_com_rate(double cr) {com_rate = cr;}
    double get_com_rate() {return com_rate;}

    void set_last_tcom(double tc) {last_tcom = tc;}
    double get_last_tcom() {return last_tcom;}
    void exchange_last_tcom(double &tc) {EXCHANGE(last_tcom,tc);}

    double get_scan_time();
    virtual double get_rmax();
    double get_rmaxcoverage();

};


#endif
