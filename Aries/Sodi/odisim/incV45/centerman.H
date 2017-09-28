// centerman.H header file

#ifndef centerman_object
#define centerman_object

#include "centobj.H"
#include "parsman.H"

/************************************************************************
*	centerman object - manager for space objects covering the earth	*
************************************************************************/
class C_CENTERMAN : public C_PARSMAN {

  private:

    int CENTER;			// command center object type
    C_CENTOBJ *centobj;		// local command center objects
    int icon;			// icon for command center objects

  protected:

  public:

    C_CENTERMAN();
    virtual void init_events();

};


#endif
