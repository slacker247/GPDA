// missman.H header file

#ifndef missman_object
#define missman_object

#include "parsman.H"
#include "missobj.H"

/************************************************************************
*	missman object - manager for space objects covering the earth	*
************************************************************************/
class C_MISSMAN : public C_PARSMAN {

  private:

    int n_missile_types;	// number of missile types
    C_MISSOBJ *missobj;		// missile simulation objects
    int first_id;
    int first_node;
    int test_prox;

  protected:

  public:

    C_MISSMAN();
    virtual void init_events();

};


#endif
