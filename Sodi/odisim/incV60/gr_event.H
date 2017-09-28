// gr_event.H header file

#ifndef gr_event_object
#define gr_event_object

#include "sq_item.H"
#include "speedes_state.H"

/************************************************************************
*	gr_event are graphic event objects				*
************************************************************************/
class C_GR_EVENT : public C_SQ_ITEM {

  private:

  protected:

    static C_SPEEDES_STATE *speedes_state; 	// state of the simulation

  public:

    C_GR_EVENT() {reset_processed();}

    void set_rqueue(void *rq);
    void set_speedes_state(C_SPEEDES_STATE *ss) {speedes_state = ss;}

    void schedule(C_GR_EVENT *gr_event);
    void reschedule(double time);

    int check_processed(int pf);

    virtual void process();
    virtual void unprocess();

};

#endif

