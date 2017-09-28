//file: ProjDispList.H

#ifndef PROJDISPLIST
#define PROJDISPLIST

#define INLINE inline

#include "GR_MapProjection.H"
#include "GR_DispList.H"
#include "GR_DispObj.H"

class GR_ProjListObj : public GR_DispObj
{
	protected: 
		float				p_lat;
		float				p_lon;
		GR_MapProjection 	*p_projection;
		void calc_xy();
		virtual void set_proj ();

	public:
		GR_ProjListObj(float lat, float lon, GR_MapProjection *projection = 0 );

		void new_proj( GR_MapProjection *projection );
		void use_proj ();
		void latlon (float lat, float lon);
		void lat (float value);
		void lon (float value);
};

INLINE void
GR_ProjListObj::latlon (float lat, float lon)
{
	p_lat = lat;
	p_lon = lon;
}

INLINE void
GR_ProjListObj::lat (float value)
{
	p_lat = value;
}

INLINE void
GR_ProjListObj::lon (float value)
{
	p_lon = value;
}

class GR_ProjDispList : public GR_DispList
{
	private :
		unsigned				p_xformflag : 1;
		GR_MapProjection	*p_projection;
		
	public :
		GR_ProjDispList( GR_MapProjection *projection = 0);

		void new_projection( GR_MapProjection *projection );
		void add_object( GR_DispObj& object );
		void add_object( GR_ProjListObj& object );
		GR_ProjListObj*	delete_cur ();
		GR_ProjListObj* enumerate (long flag = 0);

		void xform (long flag);
};

INLINE void
GR_ProjDispList::xform (long tf)
{
	if (tf)
		p_xformflag = 1;
	else
		p_xformflag = 0;

	new_projection (p_projection);
}

#endif
