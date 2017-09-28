// track2.H header file

#ifndef track2_object
#define track2_object

#include "track.H"

/************************************************************************
*	track2 object 							*
************************************************************************/
class C_TRACK2 : public C_TRACK {

  private:

    C_KALMAN1 kalman1[3];	// kinematic Kalman Filter Object
    C_KALMAN2 kalman2;		// kinematic Kalman Filter Object

  protected:

  public:

    C_TRACK2();
    C_KALMAN1 *get_kalman1() {return kalman1;}
    C_KALMAN2 *get_kalman2() {return &kalman2;}

};


#endif
