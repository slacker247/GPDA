// loiter.H header file

#ifndef loiter_object
#define loiter_object

#include "eom.H"

/************************************************************************
*	loiter object 							*
************************************************************************/
class C_LOITER : public C_EOM {

  private:

  protected:

  public:

    C_LOITER();
    void init(double t, double x[3], double v);

};


#endif
