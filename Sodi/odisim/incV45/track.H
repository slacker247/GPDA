// track.H header file

#ifndef track_object
#define track_object

#include "sq_item.H"
#include "kalman1.H"
#include "kalman2.H"
#include <string.h>

/************************************************************************
*	track object 							*
************************************************************************/
class C_TRACK : public C_SQ_ITEM {

  private:

    double Pe[3];
    double Ve[3];
    double range;
    double range_rate;
    double Pt[3];
    double Vt[3];
    double error[3];
    double total_error;
    int ballistic;
    int cycle;

    int object_node;
    int object_id;
    int object_type;

  protected:

  public:

    C_TRACK();

    void set_object_node(int n) {object_node = n;}
    void set_object_id(int i) {object_id = i;}
    void set_object_type(int t) {object_type = t;}

    int get_object_node() {return object_node;}
    int get_object_id() {return object_id;}
    int get_object_type() {return object_type;}

    void set_true_state(double p[3], double v[3]) {
      Pt[0] = p[0];
      Pt[1] = p[1];
      Pt[2] = p[2];
      Vt[0] = v[0];
      Vt[1] = v[1];
      Vt[2] = v[2];
    }

    void get_true_state(double p[3], double v[3]) {
      p[0] = Pt[0];
      p[1] = Pt[1];
      p[2] = Pt[2];
      v[0] = Vt[0];
      v[1] = Vt[1];
      v[2] = Vt[2];
    }

    void set_est_state(double p[3], double v[3]) {
      Pe[0] = p[0];
      Pe[1] = p[1];
      Pe[2] = p[2];
      Ve[0] = v[0];
      Ve[1] = v[1];
      Ve[2] = v[2];
    }

    void get_est_state(double p[3], double v[3]) {
      p[0] = Pe[0];
      p[1] = Pe[1];
      p[2] = Pe[2];
      v[0] = Ve[0];
      v[1] = Ve[1];
      v[2] = Ve[2];
    }

    void set_ballistic(int b) {ballistic = b;}
    int get_ballistic() {return ballistic;}

    void set_range(double r) {range = r;}
    double get_range() {return range;}

    void set_range_rate(double r) {range_rate = r;}
    double get_range_rate() {return range_rate;}

    double get_total_error() {return total_error;}
    void set_total_error(double e) {total_error = e;}

    double *get_error() {return error;}
    void set_error(double e0, double e1, double e2) {	error[0] = e0;
							error[1] = e1;
							error[2] = e2;}
    void set_cycle(int c) {cycle = c;}
    void increment_cycle() {cycle++;}
    int get_cycle() {return cycle;}

    virtual void print();

    char *fill_buff(char *buff, int size) {
	memcpy(buff,(char *)this,size);
	return (buff + size);
    }

};


#endif
