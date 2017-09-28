// gr_kill.H header file

#ifndef gr_kill_object
#define gr_kill_object

#include "gr_event.H"
#include "speedes_object.H"

/************************************************************************
*	gr_kill - kill an object in the speedes state			*
************************************************************************/
class C_GR_KILL : public C_GR_EVENT {

  private:

    int kill_object_id;			// killed object unique id
    int previous_status;		// previous status of speedes object
    C_SPEEDES_OBJECT *speedes_object;	// speedes object to kill

  public:

    C_GR_KILL() {speedes_object = NULL;}
    void init(int uid) {kill_object_id = uid;}

    virtual void process();
    virtual void unprocess();

};

#endif

