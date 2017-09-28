// del_sfg.H header file

#ifndef del_sfg_object
#define del_sfg_object

#include "event.H"

/************************************************************************
*	del_sfg object 							*
************************************************************************/
class C_DEL_SFG : public C_EVENT {

  private:

    int unique_id;		// unique_id of object

    double tsensors;		// time of updating the mover list
    double lazy_tmovers;	// lazy time of updating the mover list
    int lazy_mover_counter;	// lazy mover counter

    static int done;
    static int DEL_SFG;
    static int DEL_SFM;
    static int GRID;

  protected:

  public:

    C_DEL_SFG();
    virtual void init(C_HEADER *);
    virtual void exchange();
    virtual void temp_process();
    virtual void perm_process();
    virtual void cleanup();
    virtual int lazy();

};


#endif
