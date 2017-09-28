// sensman.H header file

#ifndef sensman_object
#define sensman_object

#include "parsman.H"
#include "sensobj.H"

/************************************************************************
*	sensman object - manager for space objects covering the earth	*
************************************************************************/
class C_SENSMAN : public C_PARSMAN {

  private:

    C_SENSOBJ *sensobj;		// sensor objects
    int start_uid;		// starting id for sensors
    double sensor_com_delay;	// comm delay for sensor
    double com_rate;		// comm rate for sensor
    int test_prox;		// test proximity detection on 1 node flag
    double test_prox_time;	// test proximity detection time interval

  protected:

  public:

    C_SENSMAN();

    virtual void init_events();
    double get_sensor_com_delay() {return sensor_com_delay;}
    int get_test_prox() {return test_prox;}
    double get_test_prox_time() {return test_prox_time;}
    double get_com_rate() {return com_rate;}
    double get_com_delay() {return sensor_com_delay;}

};


#endif
