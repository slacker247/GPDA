// del_sfm.H header file

#ifndef del_sfm_object
#define del_sfm_object

#include "event.H"

/************************************************************************
*	del_sfm object 							*
************************************************************************/
class C_DEL_SFM : public C_EVENT {

  private:

    double tsensors;		// time of updating the sensor list
    char *buff;			// buffer of input sensor data
    int size;			// size of buffer
    int *node_flag;		// flag for node usage

    static int done;
    static int GRID;
    static int EOMAN;
    static int DEL_SFE;
    static int DEL_SFM;
    static int size_node_flag;	// size of flag for node usage

  protected:

  public:

    C_DEL_SFM();
    virtual void init(C_HEADER *);
    virtual void exchange();
    virtual void temp_process();
    virtual void perm_process();
    virtual void cleanup();

};


#endif
