// track1.H header file

#ifndef track1_object
#define track1_object

#include "track.H"

/************************************************************************
*	track1 object 							*
************************************************************************/
class C_TRACK1 : public C_TRACK {

  private:

    C_KALMAN2 kalman2[3];	// kinematic Kalman Filter Object

  protected:

  public:

    C_TRACK1();
    C_KALMAN2 *get_kalman2() {return kalman2;}

};


#endif
