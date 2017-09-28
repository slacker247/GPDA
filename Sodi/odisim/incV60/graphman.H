// graphman.H header file

#ifndef graphman_object
#define graphman_object

#include "graphobj.H"
#include "parsman.H"

/************************************************************************
*	graphman object - manager for equations of motion		*
************************************************************************/
class C_GRAPHMAN : public C_PARSMAN {

  private:

    C_GRAPHOBJ *graphobj;	// graphics object (one per node)

    double first_time;		// graphics first time
    double start_time;		// graphics start time
    double cycle_time;		// graphics cycle time

  protected:

  public:

    C_GRAPHMAN();
    virtual void init_events();

    double get_first_time() {return first_time;}
    double get_start_time() {return start_time;}
    double get_cycle_time() {return cycle_time;}

};


#endif
