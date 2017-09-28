// test_prox.H header file

#ifndef test_prox_object
#define test_prox_object

#include "event.H"

/************************************************************************
*	test_prox object 						*
************************************************************************/
class C_TEST_PROX : public C_EVENT {
  public:

    C_TEST_PROX();
    virtual void exchange();
    virtual void temp_process();
    virtual void perm_process();
    virtual void cleanup();

};


#endif
