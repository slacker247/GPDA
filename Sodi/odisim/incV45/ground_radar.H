// ground_radar.H header file

#ifndef ground_radar_object
#define ground_radar_object

#include "sensor_model.H"

/************************************************************************
*	ground_radar object 						*
************************************************************************/
class C_GROUND_RADAR : public C_SENSOR_MODEL {

  private:

    double RNOISE;
    double RDOTNOISE;
    double AZNOISE;
    double ELNOISE;
    double SYSNOISE;

  protected:

  public:

    C_GROUND_RADAR();

    void set_RNOISE(double rn) {RNOISE = rn;}
    void set_RDOTNOISE(double rdn) {RDOTNOISE = rdn;}
    void set_AZNOISE(double an) {AZNOISE = an;}
    void set_ELNOISE(double en) {ELNOISE = en;}
    void set_SYSNOISE(double sn) {SYSNOISE = sn;}

    virtual void scan(	double time,
			double Ps[3],
			double Vs[3],
			C_XQUEUE *movers,
			C_XQUEUE *old_tracks,
			C_XQUEUE *new_tracks	);

    virtual void generate_detections(	double time,
					double Ps[3],
					double Vs[3],
					C_XQUEUE *movers,
					C_XQUEUE *det);

};


#endif
