// add_m2g.H header file

#ifndef add_m2g_object
#define add_m2g_object

#include "event.H"

/************************************************************************
*	add_m2g object 							*
************************************************************************/
class C_ADD_M2G : public C_EVENT {

  private:

    int obj_id;			// id of object
    int obj_type;		// type of object
    int obj_node;		// node of object
    int unique_id;		// unique_id of object

    double tmovers;		// time of updating the mover list
    double lazy_tsensors;	// lazy time of updating the sensor list
    int lazy_sensor_counter;	// lazy_sensor counter

    static int done;
    static int GRID;
    static int ADD_M2G;
    static int ADD_S2M;

  protected:

  public:

    C_ADD_M2G();
    virtual void init(C_HEADER *);
    virtual void exchange();
    virtual void temp_process();
    virtual void perm_process();
    virtual void cleanup();
    virtual int lazy();

};


#endif
