#ifndef C_SPHERICAL
#define	C_SPHERICAL

#include "GR_MapProjection.H"

#define	S_CHECK_LAT(lat)											\
	(																						\
		(lat < 0.0 && fabs(lat) > 0.000001) ? 		\
		P_NOT_VISIBLE:P_NORMAL										\
	)

class GR_Spherical : public GR_MapProjection
{
	private:
		long
		p_calculate_xy (float lat, float lon, float& x, float& y);

		long
		p_latlontoxy (float lat, float lon, float& x, float& y);

		void
		p_offset_latlon (float& lat, float& lon);

	protected:
		long	p_twist;
	public:
		GR_Spherical ();

		void
		set_twist (float twist)
		{
			p_twist = (long)twist;
		}
};
#endif
