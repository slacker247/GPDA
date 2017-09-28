// scan_gbi.H header file

#ifndef scan_gbi_object
#define scan_gbi_object

#include "base_scan.H"
#include "xqueue.H"

/************************************************************************
*	scan_gbi object 						*
************************************************************************/
class C_SCAN_GBI : public C_BASE_SCAN {

  private:

    static int done;
    static int SCAN_GBI;

  protected:

  public:

    C_SCAN_GBI();
    virtual void exchange();
    virtual void temp_process();
    virtual void perm_process();
    virtual void cleanup();

};


#endif
