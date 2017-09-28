//file: AzimuthEQ.H

#ifndef AZIMUTHEQ
#define AZIMUTHEQ

#include "GR_Spherical.H"

class GR_AzimuthEQ : public GR_Spherical
{
	private:
		long
		p_calculate_xy (float lat, float lon, float& x, float& y);

	public :
		GR_AzimuthEQ(){}
};
#endif
