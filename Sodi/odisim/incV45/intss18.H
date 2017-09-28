// intss18.H header file

#ifndef iss18_object
#define iss18_object

#include "missile.H"

/************************************************************************
*	intss18 object 							*
************************************************************************/
class C_INTSS18 : public C_MISSILE {

  private:

    static int done;
    static double Range_Angle[180];
    static double Time_Angle[180];

  protected:

  public:

    C_INTSS18();

};


#endif
