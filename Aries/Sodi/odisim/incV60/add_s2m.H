// add_s2m.H header file

#ifndef add_s2m_object
#define add_s2m_object

#include "event.H"

/************************************************************************
*	add_s2m object 							*
************************************************************************/
class C_ADD_S2M : public C_EVENT {

  private:

    double tsensors;		// time of updating the sensor list
    char *buff;			// buffer of input sensor data
    int size;			// size of buffer
    int *node_flag;		// flag for node usage
    int error_flag;		// error flag

    static int done;
    static int size_node_flag;	// size of flag for node usage

  protected:

  public:

    C_ADD_S2M();
    virtual void init(C_HEADER *);
    virtual void exchange();
    virtual void temp_process();
    virtual void perm_process();
    virtual void cleanup();

};


#endif
