#ifndef C_CYLINDRICAL
#define C_CYLINDRICAL

#include "GR_MapProjection.H"

#define		C_LON_WRAP 		0
#define		C_LAT_WRAP		1

class GR_Cylindrical : public GR_MapProjection
{
	private:
		long
		p_calculate_xy (float lat, float lon, float &x, float& y)
		{
			x = lon / M_PI;
			y = lat / M_PI_2;
			return P_NORMAL;
		}

		long
		p_latlontoxy (float lat, float lon, float& x, float& y);

		void
		p_offset_latlon (float& lat, float& lon);

	protected:
		long		p_wraptype;
	public:
		GR_Cylindrical ();

		void
		set_lon_wrap ()
		{
			p_wraptype = C_LON_WRAP;
		}

		void
		set_lat_wrap ()
		{
			p_wraptype = C_LAT_WRAP;
		}
};

#endif
