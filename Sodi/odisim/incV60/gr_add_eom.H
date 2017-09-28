// gr_add_eom.H header file

#ifndef gr_add_eom_object
#define gr_add_eom_object

#include "gr_event.H"
#include "speedes_object.H"
#include "ext_graphics_define_output.H"
#include "eom.H"

/************************************************************************
*	gr_add_eom - add an equation of motion to the speedes state	*
************************************************************************/
class C_GR_ADD_EOM : public C_GR_EVENT {

  private:

    C_SPEEDES_OBJECT new_speedes_object;	// new SPEEDES object
    C_SPEEDES_OBJECT *old_speedes_object;	// old SPEEDES object
    int good_sequence;				// good sequence flag

  public:

    C_GR_ADD_EOM() {;}
    void init(C_EOM *eom);
    void init(EXT_GRAPHICS_DEFINE_OUTPUT *mess);
    void set_send_time(double t) {new_speedes_object.set_send_time(t);}
    virtual void process();
    virtual void unprocess();

};

#endif

