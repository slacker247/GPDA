// scan_eye.H header file

#ifndef scan_eye_object
#define scan_eye_object

#include "base_scan.H"
#include "xqueue.H"

/************************************************************************
*	scan_eye object 						*
************************************************************************/
class C_SCAN_EYE : public C_BASE_SCAN {

  private:

    static int done;
    static int SCAN_EYE;
    static int DSP;

  protected:

  public:

    C_SCAN_EYE();
    virtual void exchange();
    virtual void temp_process();
    virtual void perm_process();
    virtual void cleanup();

};


#endif
