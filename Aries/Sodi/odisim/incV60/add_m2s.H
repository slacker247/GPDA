// add_m2s.H header file

#ifndef add_m2s_object
#define add_m2s_object

#include "event.H"

/************************************************************************
*	add_m2s object 							*
************************************************************************/
class C_ADD_M2S : public C_EVENT {

  private:

    void *peom;			// pointer to the eom of the object
    int unique_id;		// unique_id of object
    int scanning;		// scanning flag for sensor
    int seed;			// seed for random number

    static int done;

  protected:

  public:

    C_ADD_M2S();
    virtual void init(C_HEADER *);
    virtual void exchange();
    virtual void temp_process();
    virtual void perm_process();
    virtual void cleanup();

};


#endif
