// dsp_model.H header file

#ifndef dsp_model_object
#define dsp_model_object

#include "sensor_model.H"

/************************************************************************
*	dsp_model object 						*
************************************************************************/
class C_DSP_MODEL : public C_SENSOR_MODEL {

  private:

  protected:

  public:

    C_DSP_MODEL();

    virtual void scan(	double time,
			double Ps[3],
			double Vs[3],
			C_XQUEUE *movers,
			C_XQUEUE *old_tracks,
			C_XQUEUE *new_tracks	);

};


#endif
