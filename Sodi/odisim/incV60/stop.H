// stop.H header file

#ifndef stop_object
#define stop_object

#include "eom.H"

/************************************************************************
*	stop object 							*
************************************************************************/
class C_STOP : public C_EOM {

  private:

  protected:

  public:

    C_STOP();
    void init(double t, double x[3]);

};


#endif
