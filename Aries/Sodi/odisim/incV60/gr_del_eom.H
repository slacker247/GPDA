// gr_del_eom.H header file

#ifndef gr_del_eom_object
#define gr_del_eom_object

#include "gr_event.H"
#include "speedes_object.H"
#include "ext_graphics_define_output.H"
#include "eom.H"

/************************************************************************
*	gr_del_eom - del an equation of motion to the speedes state	*
************************************************************************/
class C_GR_DEL_EOM : public C_GR_EVENT {

  private:

    C_SPEEDES_OBJECT *old_speedes_object;	// old SPEEDES object
    int good_sequence;				// good sequence flag
    double send_time;				// send time of this event
    int unique_id;				// unique id to remove

  public:

    C_GR_DEL_EOM() {good_sequence = 1;}
    void set_send_time(double t) {send_time = t;}
    void set_unique_id(int uid) {unique_id = uid;}
    virtual void process();
    virtual void unprocess();

};

#endif

