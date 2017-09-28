// add_s2g.H header file

#ifndef add_s2g_object
#define add_s2g_object

#include "event.H"

/************************************************************************
*	add_s2g object 							*
************************************************************************/
class C_ADD_S2G : public C_EVENT {

  private:

    int obj_id;			// id of object
    int obj_type;		// type of object
    int obj_node;		// node of object
    int unique_id;		// unique_id of object

    double tsensors;		// time of updating the mover list
    double lazy_tmovers;	// lazy time of updating the mover list
    int lazy_mover_counter;	// lazy mover counter

    static int done;

  protected:

  public:

    C_ADD_S2G();
    virtual void init(C_HEADER *);
    virtual void exchange();
    virtual void temp_process();
    virtual void perm_process();
    virtual void cleanup();
    virtual int lazy();

};


#endif
