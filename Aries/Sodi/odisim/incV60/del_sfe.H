// del_sfe.H header file

#ifndef del_sfe_object
#define del_sfe_object

#include "event.H"

/************************************************************************
*	del_sfe object 							*
************************************************************************/
class C_DEL_SFE : public C_EVENT {

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

    C_DEL_SFE();
    virtual void init(C_HEADER *);
    virtual void exchange();
    virtual void temp_process();
    virtual void perm_process();
    virtual void cleanup();

};


#endif
