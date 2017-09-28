// eyeman.H header file

#ifndef eyeman_object
#define eyeman_object

#include "parsman.H"
#include "eyeobj.H"

/************************************************************************
*	eyeman object - manager for space objects covering the earth	*
************************************************************************/
class C_EYEMAN : public C_PARSMAN {

  private:

    C_EYEOBJ *eyeobj;		// eye objects

    int icon;			// graphics icon for B.E.
    int n_rings;		// number of B.E. rings
    int n_per_ring;		// number of B.E.s per ring
    double inclination;		// inclination of each ring
    double altitude;		// altitude of each B.E.
    double time;		// time for architecture
    double velocity;		// velocity of each B.E.
    double REYE;		// range from center of the earth to B.E.
    double RFACTOR;		// REYE / RE
    int stagger;		// flag for staggering the starting B.E.
    double turn_on_time;	// time for B.E.s to turn on

    int start_uid;		// starting id for eye objects
    int test_prox;		// test proximity detection on 1 node flag
    double test_prox_time;	// test proximity detection time interval

  protected:

  public:

    C_EYEMAN();
    virtual void init_events();

};


#endif
