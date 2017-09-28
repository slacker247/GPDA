// scan.H header file

#ifndef scan_object
#define scan_object

#include "base_scan.H"
#include "xqueue.H"
#include "ext_graphics_link_output.H"

/************************************************************************
*	scan object 							*
************************************************************************/
class C_SCAN : public C_BASE_SCAN {

  private:

    static int done;
    static int SCAN;

  protected:

  public:

    C_SCAN();
    virtual void exchange();
    virtual void temp_process();
    virtual void perm_process();
    virtual void cleanup();

};


#endif
