// ss24.H header file

#ifndef ss24_object
#define ss24_object

#include "missile.H"

/************************************************************************
*	ss24 object 							*
************************************************************************/
class C_SS24 : public C_MISSILE {

  private:

    static int done;
    static double Range_Angle[180];
    static double Time_Angle[180];

  protected:

  public:

    C_SS24();

};


#endif
