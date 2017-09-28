// add_s2e.H header file

#ifndef add_s2e_object
#define add_s2e_object

#include "event.H"

/************************************************************************
*	add_s2e object 							*
************************************************************************/
class C_ADD_S2E : public C_EVENT {

  private:

    int obj_id;			// id of sensor
    int obj_type;		// type of sensor
    int obj_node;		// node of sensor
    int unique_id;		// unique_id of sensor
    int mover_unique_id;	// unique id of mover
    double tmovers;		// time of updating the mover list

    static int done;

  protected:

  public:

    C_ADD_S2E();
    virtual void init(C_HEADER *);
    virtual void exchange();
    virtual void temp_process();
    virtual void perm_process();
    virtual void cleanup();

};


#endif
