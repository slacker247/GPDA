// speedes_object.H header file

#ifndef sp_object_object
#define sp_object_object

#include "sq_item.H"
#include "queue.H"
#include "eom.H"

/************************************************************************
*	speedes_object object 						*
************************************************************************/
class C_SPEEDES_OBJECT : public C_SQ_ITEM {

  private:

  protected:

    int icon;
    double Rmax;		// maximum range for sensor
    double Rmin;		// minimum range for sensor
    double X[3];		// position
    double V[3];		// velocity
    double time;		// time of position and velocity
    C_QUEUE links;		// queue of links to other objects
    void *GR_object;		// graphics object for RSD
    int alive;			// alive flag

    C_EOM *eom;			// current equation of motion
    double send_time;		// time that the simulation sent the eom

  public:

    C_SPEEDES_OBJECT();

    void set_GR_object(void *G) {GR_object = G;}
    void *get_GR_object() {return GR_object;}

    int get_alive() {return alive;}
    void set_alive(int a) {alive = a;}

    int get_unique_id() {return id;}
    void set_unique_id(int ui) {id = ui;}

    int get_icon() {return icon;}
    void set_icon(int ic) {icon = ic;}

    double get_Rmax() {return Rmax;}
    void set_Rmax(double Rm) {Rmax = Rm;}

    double get_Rmin() {return Rmin;}
    void set_Rmin(double Rm) {Rmin = Rm;}

    void set_pos_vel(double t, double Pos[3], double Vel[3]);
    void get_pos_vel(double t, double Pos[3], double Vel[3]);

    void set_eom(C_EOM *e) {eom = e;}
    C_EOM *get_eom() {return eom;}

    void set_send_time(double t) {send_time = t;}
    double get_send_time() {return send_time;}

    double get_scale();

};


#endif
