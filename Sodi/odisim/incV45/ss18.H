// ss18.H header file

#ifndef ss18_object
#define ss18_object

#include "missile.H"

/************************************************************************
*	ss18 object 							*
************************************************************************/
class C_SS18 : public C_MISSILE {

  private:

    static int done;
    static double Range_Angle[180];
    static double Time_Angle[180];

  protected:

  public:

    C_SS18();

};


#endif
