// change_prox.H header file

#ifndef changeprox_object
#define changeprox_object

#include "event.H"
#include "xqueue.H"

/************************************************************************
*	change_prox object 						*
************************************************************************/
class C_CHANGE_PROX : public C_EVENT {

  private:

    static int done;
    static int EOMAN;
    static int GRID;
    static int ADD_E2E;
    static int NEXT_SCRIPT;

  protected:

  public:

    C_CHANGE_PROX();
    void update_prox(double new_time, C_XQUEUE *script);

};


#endif
