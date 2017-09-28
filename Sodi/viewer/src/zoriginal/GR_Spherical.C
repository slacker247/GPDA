#include "GR_Spherical.H"
#include "GR_Interface.H"

GR_Spherical::GR_Spherical ()
{
	GR_MapProjection::p_center_lat = M_PI_2;
	p_twist = 0;
}

void
GR_Spherical::p_offset_latlon (float& lat, float& lon)
{
	float 	xnew, ynew, znew;
	float		x, y, z, latz;
	float		coslat, coslon, sinlat, sinlon;

	if (M_PI_2 - fabs (p_center_lat) < 0.0000001)
	{
		if (p_center_lat < 0.0)
		{
			lat = -lat;
			lon = -lon;
		}
		lon += (float)p_twist;
		return;
	}

	lon -= p_center_lon;

	coslat = fcos(lat);
	coslon = fcos(lon);
	sinlat = fsin(lat);
	sinlon = fsin(lon);

	x = coslat * coslon;
	y = coslat * sinlon;
	z = sinlat;

	latz = fasin (z);
	if (x < 0.0)
		if (latz > 0.0)
			latz = M_PI - latz;
		else
			latz = -M_PI + latz;

	xnew = x * fcos (M_PI_2 - p_center_lat) - z * fsin (M_PI_2 - p_center_lat);
	ynew = y;
	znew = x * fsin (M_PI_2 - p_center_lat) + z * fcos (M_PI_2 - p_center_lat);

	lat = fasin( znew );
	// fsqrt used here to insure proper conversion to float
	// As of 4.0.1, using sqrt causes a double to be assigned to the
	// lvalue location thus resulting in NaN
	lon = facos( xnew / fsqrt (xnew * xnew + ynew * ynew));

	if ( ynew < 0.0 ) 
		lon = -lon;

	lon += (float)p_twist - M_PI_2;
}

long
GR_Spherical::p_calculate_xy (float lat, float lon, float& x, float& y)
{
	float r;
	
	if (S_CHECK_LAT(lat) != P_NORMAL)
		return P_NOT_VISIBLE;

	r = fcos (lat);
	x = r * fcos (lon); 
	y = r * fsin (lon); 

	return	P_NORMAL;
}

long
GR_Spherical::p_latlontoxy (float lat, float lon, float& x, float& y)
{
	long		status;

	status = p_calculate_xy (lat, lon, x, y);

	if (GR_MapProjection::p_first_flag)
		return	status;

	switch (GR_MapProjection::p_last_status)
	{
		case	P_NORMAL:
		case	P_FIRST_POINT:
			switch (status)
			{
				case	P_NORMAL:
					return status;

				case	P_NOT_VISIBLE:
					// clip to equator
					p_calculate_xy (0.0, lon, x, y);
					return	P_LAST_POINT;
			}
			break;

		case	P_NOT_VISIBLE:
			switch (status)
			{
				case	P_NOT_VISIBLE:
					return	P_NOT_VISIBLE;

				case	P_NORMAL:
					// clip to equator
					p_calculate_xy (
						0.0, p_last_lon, 
						GR_MapProjection::p_first_x, GR_MapProjection::p_first_y
					);
					return	P_FIRST_POINT;
			}
	}
}
