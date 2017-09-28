// track3.H header file

#ifndef track3_object
#define track3_object

#include "track.H"

/************************************************************************
*	track3 object 							*
************************************************************************/
class C_TRACK3 : public C_TRACK {

  private:

    C_KALMAN1 kalman1[2];	// kinematic Kalman Filter Object

  protected:

  public:

    C_TRACK3();
    C_KALMAN1 *get_kalman1() {return kalman1;}

};


#endif
