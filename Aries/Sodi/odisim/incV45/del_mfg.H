// del_mfg.H header file

#ifndef del_mfg_object
#define del_mfg_object

#include "event.H"

/************************************************************************
*	del_mfg object 							*
************************************************************************/
class C_DEL_MFG : public C_EVENT {

  private:

    int obj_id;			// id of object
    int obj_type;		// type of object
    int obj_node;		// node of object
    int unique_id;		// unique id of object

    double tmovers;		// time of updating the mover list
    double lazy_tsensors;	// lazy time of updating the sensor list
    int lazy_sensor_counter;	// lazy_sensor counter

    static int done;
    static int GRID;
    static int DEL_MFG;
    static int DEL_SFM;

  protected:

  public:

    C_DEL_MFG();
    virtual void init(C_HEADER *);
    virtual void exchange();
    virtual void temp_process();
    virtual void perm_process();
    virtual void cleanup();
    virtual int lazy();

};


#endif
