// bus.H header file

#ifndef bus_object
#define bus_object

#include "kepler.H"

/************************************************************************
*	bus object 							*
************************************************************************/
class C_BUS : public C_KEPLER {

  private:

    int nrvs;

  protected:

  public:

    C_BUS();
    virtual void create_rvs(int n);

};


#endif
