// convert.H header file

#ifndef convert_object
#define convert_object

#include "sq_item.H"

/************************************************************************
*	convert object - this object knows how to convert lat,lon to	*
*			 xyz, etc. It needs to know gsam.		*
************************************************************************/
class C_CONVERT : public C_SQ_ITEM {

  private:

  protected:

    double gsam;			// Greenwich sidereal angle

  public:

    C_CONVERT();

    void set_gsam(double g) {gsam = g;}

    void latlon_to_xyz(double lat, double lon, double x[3]);
    void latlon_to_xyz(double t, double lat, double lon, double x[3]);

    void xyz_to_latlon(double x[3], double &lat, double &lon);
    void xyz_to_latlon(double t, double x[3], double &lat, double &lon);

    void eci_to_ecr(double t, double x[3], double v[3]);
    void ecr_to_eci(double t, double x[3], double v[3]);

};


#endif
