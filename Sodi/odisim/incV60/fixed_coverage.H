// fixed_coverage.H header file

#ifndef fixed_coverage_object
#define fixed_coverage_object

#include "event.H"
#include "sensobj.H"
#include "xqueue.H"

/************************************************************************
*	fixed_coverage object 						*
************************************************************************/
class C_FIXED_COVERAGE : public C_EVENT {
  private:

    C_XQUEUE *coverage;

  public:

    C_FIXED_COVERAGE();
    virtual void init(C_HEADER *header);
    virtual void exchange();
    virtual void temp_process();
    virtual void perm_process();
    virtual void cleanup();

};


#endif
