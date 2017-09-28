// nostateman.H header file

#ifndef nostateman_object
#define nostateman_object

#include "objman.H"
#include "nostateobj.H"

/************************************************************************
*	nostateman object - manager for equations of motion		*
************************************************************************/
class C_NOSTATEMAN : public C_OBJMAN {

  private:

    int NOSTATE;		// no state (stateless) object type
    C_NOSTATEOBJ *nostateobj;	// no state object (one per node)

  protected:

  public:

    C_NOSTATEMAN();
    virtual void init_events();

};


#endif
