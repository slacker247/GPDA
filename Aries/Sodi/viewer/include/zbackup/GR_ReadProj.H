#ifndef READPROJ
#define READPROJ

#define INLINE inline

#include	"GR_MapWindow.H"
#include	"GR_MapProjection.H"
#include	"GR_Mercator.H"
#include	"GR_Spherical.H"
#include	"GR_AzimuthEQ.H"

class	MProj
{
	public:
		GR_MapProjection		*p_projection;
		long							p_type;
		char							*p_name;
		MProj							*next;
};

class	GR_ReadProj
{
	private:
		MProj	*p_projlist;
		GR_MapWindow		*p_mapwindow;
		long						p_proj_type;
		long						p_writestate;
		long						p_lat_deg;
		long						p_lat_min;
		long						p_lat_sec;
		long						p_lon_deg;
		long						p_lon_min;
		long						p_lon_sec;

		/*
		friend void			set_projname (void*, void*);
		friend void			projtype (C_PopUp*, long, char*, void*);
		friend void			set_projname (C_PopUp*, long, char*, void*);
		friend long			commit_proj (C_PanelButton*, void*, void*);
		friend long			cancel_proj (C_PanelButton*, void*, void*);
		friend long			set_writestate (C_PanelButton*, void*, void*);
		friend long			save_config (C_PanelButton*, void*, void*);
		friend long			cancel_save (C_PanelButton*, void*, void*);
		*/

	public:
		GR_ReadProj (char *filename);

		//void edit_projection (char *projname);

		void make_projections (GR_MapWindow&);
		void make_projections (GR_MapWindow*);
};

INLINE void
GR_ReadProj::make_projections (GR_MapWindow &window)
{
	this->make_projections (&window);
}
#endif
