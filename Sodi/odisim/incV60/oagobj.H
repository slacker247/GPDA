// oagobj.H header file

#ifndef oagobj_object
#define oagobj_object

#include "mover.H"

/************************************************************************
*	oagobj object 							*
************************************************************************/
class C_OAGOBJ : public C_MOVER {

  private:

  protected:

  public:

    C_OAGOBJ();
    void init_oagobj( int sequence,
	double start_lat, double start_lon, double start_altitude,
	double end_lat,   double end_lon,   double end_altitude,
	double start_t, double end_t );

};


#endif
