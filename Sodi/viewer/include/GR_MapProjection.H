#ifndef GR_MAPPROJECTION
#define GR_MAPPROJECTION

#include <math.h>

#include "GR_Map.H"

#define		P_NORMAL			0
#define		P_LAST_POINT	1
#define		P_FIRST_POINT	2
#define		P_SPLIT				3
#define		P_NOT_VISIBLE	4

#define		P_DEG_TO_RAD(degrees)		((degrees / 180.0) * M_PI)
#define		P_RAD_TO_DEG(radians)		((radians / M_PI) * 180.0)

struct Lineseg
{
	union
	{
		float	x1;
		float	lon1;
	};

	union
	{
		float	y1;
		float	lat1;
	};

	union
	{
		float	x2;
		float	lon2;
	};

	union
	{
		float	y2;
		float	lat2;
	};
};

class GR_MapProjection
{
	private:
		virtual long
		p_calculate_xy (float lat, float lon, float &x, float& y)
		{
			x = lon;
			y = lat;

			return P_NORMAL;
		}

		virtual void
		p_offset_latlon (float& lat, float& lon)
		{
			lat = lat;
			lon = lon;
		}

		virtual long	
		p_latlontoxy (float lat, float lon, float& x, float& y)
		{
			x = lon;
			y = lat;
			return	P_NOT_VISIBLE;
		}


	protected:
		long				p_first_flag;
		long				p_last_status;
		long				p_save_flag;
		float				p_first_x;
		float				p_first_y;
		float				p_second_x;
		float				p_second_y;
		float				p_center_lat;
		float				p_center_lon;
		float				p_last_lat;
		float				p_last_lon;
		float				p_southlimit;
		float				p_northlimit;
		float				p_westlimit;
		float				p_eastlimit;

	public:
		GR_MapProjection ();

		GR_Map*
		convert_map (GR_Map *map, char *mapfile = 0, long noread = 0);

		Lineseg*
		convert_line (Lineseg, long& count);

		void
		set_center (float lat, float lon)
		{
			p_center_lat = P_DEG_TO_RAD(lat);
			p_center_lon = P_DEG_TO_RAD(lon);
		}

		void
		get_center (float &lat, float &lon)
		{
			lat = p_center_lat;
			lon = p_center_lon;
		}

		void
		set_lat_limits (float southlimit, float northlimit);

		void
		set_lon_limits (float westlimit, float eastlimit);

		long
		calculate_xy (float lat, float lon, float &x, float& y);

		void
		set_write ()
		{
			p_save_flag = 1;
		}

		void
		set_nowrite ()
		{
			p_save_flag = 0;
		}

		long
		writestate ()
		{
			return p_save_flag;
		}
};

#endif
