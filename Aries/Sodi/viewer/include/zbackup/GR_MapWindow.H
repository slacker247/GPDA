/*	@(#)MapWindow.H	1.14		4/20/92		*/
#ifndef		GR_MAPWINDOW
#define		GR_MAPWINDOW

#define INLINE inline

#include	"GR_Shell.H"
#include	"GR_Window.H"
#include	"GR_ProjDispList.H"
#include	"GR_DispObj.H"
#include 	"GR_MapProjection.H"
#include	"GR_Map.H"

#define		ZOOM_OFF		0
#define		ZOOM_ON			1

#define		MAP					0
#define		PBOUND			1
#define		GRID				2

#define		MW_UNZOOM_ONE		0
#define		MW_UNZOOM_ALL		1

#define   CURS_ARROW      0
#define   CURS_CROSSHAIR  1
#define   CURS_HOURGLASS  2

struct ProjElem
{
	char *name;
	GR_MapProjection *projection;
	ProjElem		*next;
};

struct	ViewSize
{
		float		left;
		float		right;
		float		bottom;
		float		top;
		ViewSize	*next;
};

class GR_MapWindow : public GR_Window
{
	private:
//		GR_Shell				*p_shell;
		ProjElem				*p_projlist;
		ProjElem 				*p_curproj;
		ViewSize				*p_viewlist;
		GR_Window				*p_window;
		Widget						p_proj_pulldown;
		Widget						p_zoom_w;
		Widget						p_unzoom_w;
		Widget						p_unzoom_one_w;
		Widget						p_cancel_w;
		GR_Map						p_mainmap;
		GR_Map						p_mainpolimap;
		GR_Map						p_maingrid;
		GR_Map						*p_map;
		GR_Map						*p_polimap;
		GR_Map						*p_grid;
		long						p_mapcolor;
		long						p_gridcolor;
		long						p_pboundcolor;
		long						p_gridsegcount;
		unsigned						p_mapvis : 1;
		unsigned						p_gridvis : 1;
		unsigned						p_pboundvis : 1;
		float						p_latres;
		float						p_lonres;
		char						*p_mapfile;
		char						*p_boundaryfile;
		void						p_make_grid ();
		void set_sourcefile(char* filename, int type = MAP);    
		void rectEvent (short x1, short y1, short x2, short y2);

	protected:
		virtual void
			proj_change ();

	public:
		GR_MapWindow ();

		void set_mapfile (char *filename);
		void set_boundaryfile (char *filename);
		void makemap(int type = MAP, long force = 0);
		void makegrid();
		void set_mapcolor(short	red, short grn, short blu);
		void set_pboundcolor(short	red, short grn, short blu);
		void set_gridcolor(short	red, short grn, short blu);
		void set_gridvis(long visible);
		void set_gridres (float latres, float lonres);
		void set_pboundvis(long visible);
		void set_mapvis(long visible);
		void addDispList (GR_ProjDispList*, char *name = 0);
		void addDispList (GR_ProjDispList&, char *name = 0);
		void addMapProjection (GR_MapProjection *, char *name);
		void addMapProjection (GR_MapProjection &, char *name);
		void setMapProjection (char *name);
		void add_object (GR_ProjListObj &, char *name);
		void add_object (GR_ProjListObj *, char *name);
		void delete_all (char *name);
		void delete_object (GR_ProjListObj*, char *name);
		void delete_object (GR_ProjListObj&, char *name);
		GR_MapProjection* getMapProjection ();
		void drawBackground ();
		long gridvis ();
		long pboundvis ();
		long mapvis ();
		void zoom ();
		void cancel_zoom ();
		void unzoom ();
		void unzoom_one ();
};

INLINE void
GR_MapWindow::zoom ()
{
	XtVaSetValues (p_cancel_w,
		XmNsensitive, TRUE,
		NULL);

	XtVaSetValues (p_zoom_w,
		XmNsensitive, FALSE,
		NULL);

	GR_Window::getRectangle (GR_KEEP_ASPECT);
}

INLINE void
GR_MapWindow::cancel_zoom ()
{
	XtVaSetValues (p_cancel_w,
		XmNsensitive, FALSE,
		NULL);

	XtVaSetValues (p_zoom_w,
		XmNsensitive, TRUE,
		NULL);
	GR_Window::cancel_getRectangle ();
}

INLINE void
GR_MapWindow::add_object (GR_ProjListObj &object, char *name)
{
	this->add_object (&object, name);
}

INLINE void
GR_MapWindow::delete_object (GR_ProjListObj &object, char *name)
{
	this->delete_object (&object, name);
}

INLINE void
GR_MapWindow::addDispList (GR_ProjDispList *displist, char *name)
{
	if (p_curproj)
		displist->new_projection (p_curproj->projection);
	GR_Window::addDispList (displist, name);
}

INLINE void
GR_MapWindow::addDispList (GR_ProjDispList &displist, char *name)
{
	if (p_curproj)
		displist.new_projection (p_curproj->projection);
	GR_Window::addDispList (&displist, name);
}

INLINE void
GR_MapWindow::addMapProjection (GR_MapProjection &proj, char *name)
{
	this->addMapProjection (&proj, name);
}

INLINE void
GR_MapWindow::set_mapfile (char *filename)
{
	if (p_mapfile != 0)
		delete p_mapfile;

	p_mapfile = new char [strlen (filename) + 1];
	strcpy (p_mapfile, filename);
	set_sourcefile (p_mapfile, MAP);
}

INLINE void
GR_MapWindow::set_boundaryfile (char *filename)
{
	if (p_boundaryfile != 0)
		delete p_boundaryfile;

	p_boundaryfile = new char [strlen (filename) + 1];
	strcpy (p_boundaryfile, filename);
	set_sourcefile (p_boundaryfile, PBOUND);
}

INLINE void
GR_MapWindow::set_mapcolor(short	red, short grn, short blu)
{
	p_mapcolor	=	GR_get_color (red, grn, blu);
}

INLINE void
GR_MapWindow::set_pboundcolor(short	red, short grn, short blu)
{
	p_pboundcolor	=	GR_get_color (red, grn, blu);
}

INLINE void
GR_MapWindow::set_gridcolor(short	red, short grn, short blu)
{
	p_gridcolor	=	GR_get_color (red, grn, blu);
}

INLINE void
GR_MapWindow::set_gridvis(long visible)
{
	if (visible)
		p_gridvis = 1;
	else
		p_gridvis = 0;
}		

INLINE void
GR_MapWindow::set_mapvis(long visible)
{
	if (visible)
		p_mapvis = 1;
	else
		p_mapvis = 0;
}		

INLINE void
GR_MapWindow::set_pboundvis(long visible)
{
	if (visible)
		p_pboundvis = 1;
	else
		p_pboundvis = 0;
}		


INLINE long
GR_MapWindow::gridvis ()
{
	return p_gridvis;
}

INLINE long
GR_MapWindow::mapvis ()
{
	return p_mapvis;
}

INLINE long
GR_MapWindow::pboundvis ()
{
	return p_pboundvis;
}

INLINE void
GR_MapWindow::proj_change ()
{ }

#endif
