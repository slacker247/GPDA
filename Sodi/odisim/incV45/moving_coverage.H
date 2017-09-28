// moving_coverage.H header file

#ifndef moving_coverage_object
#define moving_coverage_object

#include "event.H"
#include "sensobj.H"
#include "xqueue.H"

/************************************************************************
*	moving_coverage object 						*
************************************************************************/
class C_MOVING_COVERAGE : public C_EVENT {

  private:

    C_XQUEUE *old_coverage;
    C_XQUEUE *new_coverage;

    static int done;
    static int MOVING_COVERAGE;
    static int ADD_S2G;
    static int DEL_SFG;
    static int GRID;

  protected:

  public:

    C_MOVING_COVERAGE();
    virtual void init(C_HEADER *header);
    virtual void exchange();
    virtual void temp_process();
    virtual void perm_process();
    virtual void cleanup();

};


#endif
