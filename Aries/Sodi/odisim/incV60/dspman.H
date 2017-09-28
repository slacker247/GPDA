// dspman.H header file

#ifndef dspman_object
#define dspman_object

#include "parsman.H"
#include "dspobj.H"

/************************************************************************
*	dspman object - manager for space objects covering the earth	*
************************************************************************/
class C_DSPMAN : public C_PARSMAN {

  private:

    C_DSPOBJ *dspobj;		// dsp objects
    int start_uid;		// starting id for dsp objects
    int test_prox;		// test proximity detection on 1 node flag
    double test_prox_time;	// test proximity detection time interval

  protected:

  public:

    C_DSPMAN();
    virtual void init_events();

};


#endif
