// eye_model.H header file

#ifndef eye_model_object
#define eye_model_object

#include "sensor_model.H"

/************************************************************************
*	eye_model object 						*
************************************************************************/
class C_EYE_MODEL : public C_SENSOR_MODEL {

  private:

    double NOISE;

  protected:

  public:

    C_EYE_MODEL();
    void set_NOISE(double n) {NOISE = n;}
    void set_NOISE_BE_SCAN()		{NOISE = 0.000050;}
    void set_NOISE_BE_TELESCOPE()	{NOISE = 0.000020;}
    void set_NOISE_DSP()		{NOISE = 0.000015;}
    void set_NOISE_FEWS()		{NOISE = 0.000002;}

    virtual void generate_detections(	double time,
					double Ps[3],
					double Vs[3],
					C_XQUEUE *movers,
					C_XQUEUE *det);

    virtual void eye_scan(		double time,
					double Ps[3],
					double Vs[3],
					C_XQUEUE *movers,
					C_XQUEUE *tracks);

};


#endif
