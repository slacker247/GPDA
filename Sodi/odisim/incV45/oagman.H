// oagman.H header file

#ifndef oagman_object
#define oagman_object

#include "parsman.H"
#include "oagobj.H"

/************************************************************************
*	oagman object - manager for OAG objects covering the earth	*
************************************************************************/
class C_OAGMAN : public C_PARSMAN {

  private:

    int OAG;			// id for oag mover types
    C_OAGOBJ *oagobj;		// oag mover simulation objects
    int my_flights;		// number of flights for this manager

  protected:

  public:

    C_OAGMAN();

    virtual void init_events();

};


#endif
