/*	@(#)MapWindow.C	1.24		4/20/92		*/

#include	"GR_MapWindow.H"
#include	<ctype.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeB.h>
#include <Xm/PushB.h>
#include <stdlib.h>

#define	XMSTRING(t, s) \
	XtVaTypedArg, t, XmRString, s, strlen(s) + 1

void
quitapp (Widget, caddr_t, caddr_t)
{
	exit (0);
}

void
zoomfunc (Widget, caddr_t client_data, caddr_t)
{
	GR_MapWindow		*window;

	window = (GR_MapWindow *) client_data;

	window->zoom ();
}

void
cancel_zoomfunc (Widget, caddr_t client_data, caddr_t)
{
	GR_MapWindow		*window;

	window = (GR_MapWindow *) client_data;

	window->cancel_zoom ();

}

void
unzoomfunc (Widget, caddr_t client_data, caddr_t)
{
	GR_MapWindow		*window;

	window = (GR_MapWindow *) client_data;

	window->unzoom ();
}

void
unzoom_onefunc (Widget, caddr_t client_data, caddr_t)
{
	GR_MapWindow		*window;

	window = (GR_MapWindow *) client_data;

	window->unzoom_one ();
}

unsigned short hourcursor [128] =
{
  0x3FFF,0xFFFC,0x3FFF,0xFFFC,
  0x3DAF,0xBF5C,0x3375,0xFFFC,
  0x38F6,0x99CC,0x180F,0xFF0C,
  0x1807,0x701C,0x0C07,0xE018,
  0x0603,0xC030,0x0301,0xC060,
  0x0180,0x80C0,0x00C0,0x8180,
  0x0070,0x8700,0x0018,0x8E00,
  0x000C,0x9800,0x0006,0x3000,
  0x0007,0xB000,0x0007,0xF000,
  0x000D,0xD800,0x000F,0xDC00,
  0x003F,0xFE00,0x00E4,0x5B80,
  0x01BB,0xBFC0,0x03B0,0x0360,
  0x0780,0x0030,0x0C00,0x0018,
  0x1800,0x000C,0x1800,0x000C,
  0x3000,0x0006,0x3000,0x0006,
  0x3000,0x0006,0x3FFF,0xFFFE,
};

struct MapWCBstruct
{
	GR_MapWindow	*mapwindow;
	char					*data;
};

void 
set_proj (Widget, caddr_t client_data, caddr_t)
{
	MapWCBstruct		*cbstruct;

	cbstruct = (MapWCBstruct *) client_data;

	cbstruct->mapwindow->setMapProjection (cbstruct->data);
}

#define		MW_XMIN			(-1.0)
#define		MW_XMAX			(1.0)
#define		MW_YMIN			(-1.0)
#define		MW_YMAX			(1.0)

void     gridfunc (Widget, caddr_t, caddr_t);
void     pboundfunc (Widget, caddr_t, caddr_t);
void     mapfunc (Widget, caddr_t, caddr_t);
void     quitfunc (Widget, caddr_t, caddr_t);

class	C_ViewSize
{
	public:
		float		p_xmin;
		float		p_xmax;
		float		p_ymin;
		float		p_ymax;

	C_ViewSize::C_ViewSize (float xmin, float xmax, float ymin, float ymax);
};

C_ViewSize::C_ViewSize (float xmin, float xmax, float ymin, float ymax)
{
	p_xmin = xmin;
	p_xmax = xmax;
	p_ymin = ymin;
	p_ymax = ymax;
}

GR_MapWindow::GR_MapWindow ()
{
	Widget	form, menubar, cbutton, button, pulldownmenu;

	p_shell = new GR_Shell;
	p_shell->createWidget("MapWindowShell");

	form = XtVaCreateManagedWidget (NULL, xmFormWidgetClass, p_shell->widget(), NULL);
	this->doublebuffer ();
	this->rgbmode ();
	this->createWidget ("MapWindow", form);

	menubar = XmCreateMenuBar (form, NULL, NULL, 0);
	XtVaSetValues (menubar,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
	XtManageChild (menubar);

	pulldownmenu = XmCreatePulldownMenu (menubar, NULL, NULL, 0);
	cbutton = XtVaCreateManagedWidget (NULL, xmCascadeButtonWidgetClass, 
		menubar,
		XMSTRING (XmNlabelString, "File "),
		XmNsubMenuId, pulldownmenu,
		NULL);

	button = XtVaCreateManagedWidget (NULL, xmPushButtonWidgetClass,
		pulldownmenu,
		XMSTRING (XmNlabelString, "Quit"),
		NULL);

	XtAddCallback (button, XmNactivateCallback, quitapp, 0);

	pulldownmenu = XmCreatePulldownMenu (menubar, NULL, NULL, 0);
	cbutton = XtVaCreateManagedWidget (NULL, xmCascadeButtonWidgetClass, 
		menubar,
		XMSTRING (XmNlabelString, "View "),
		XmNsubMenuId, pulldownmenu,
		NULL);

	p_zoom_w = XtVaCreateManagedWidget (NULL, xmPushButtonWidgetClass,
		pulldownmenu,
		XMSTRING (XmNlabelString, "Zoom"),
		NULL);

	XtAddCallback (p_zoom_w, XmNactivateCallback, zoomfunc, (XtPointer)this);

	p_cancel_w = XtVaCreateManagedWidget (NULL, xmPushButtonWidgetClass,
		pulldownmenu,
		XMSTRING (XmNlabelString, "Cancel Zoom"),
		XmNsensitive, FALSE,
		NULL);

	XtAddCallback (p_cancel_w, XmNactivateCallback, cancel_zoomfunc, (XtPointer)this);

	p_unzoom_one_w = XtVaCreateManagedWidget (NULL, xmPushButtonWidgetClass,
		pulldownmenu,
		XMSTRING (XmNlabelString, "UnZoom One"),
		XmNsensitive, FALSE,
		NULL);

	XtAddCallback (p_unzoom_one_w, XmNactivateCallback, unzoom_onefunc, (XtPointer)this);

	p_unzoom_w = XtVaCreateManagedWidget (NULL, xmPushButtonWidgetClass,
		pulldownmenu,
		XMSTRING (XmNlabelString, "UnZoom"),
		XmNsensitive, FALSE,
		NULL);

	XtAddCallback (p_unzoom_w, XmNactivateCallback, unzoomfunc, (XtPointer)this);

	p_proj_pulldown = XmCreatePulldownMenu (menubar, NULL, NULL, 0);
	cbutton = XtVaCreateManagedWidget (NULL, xmCascadeButtonWidgetClass, 
		menubar,
		XMSTRING (XmNlabelString, "Projections "),
		XmNsubMenuId, p_proj_pulldown,
		NULL);

	XtVaSetValues (this->widget(),
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, menubar,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);

	this->color (0, 0, 0);
	this->world (MW_XMIN, MW_XMAX, MW_YMIN, MW_YMAX);
	//p_zoomstate = ZOOM_OFF;
	//p_zoomlevel = 0;
	p_curproj = 0;
	p_projlist = 0;
	p_viewlist = 0;
	p_map = 0;
	p_grid = 0;
	p_polimap = 0;
	p_mapfile = 0;
	p_boundaryfile = 0;
	set_gridres (10.0, 10.0);

	p_gridvis = 1;
	p_mapvis = 1;
	p_pboundvis = 1;

	/*
	curstype (CCROSS);
	defcursor (CURS_CROSSHAIR, (unsigned short[128]) 0);
	curstype (C32X1);
	defcursor (CURS_HOURGLASS, hourcursor);
	*/

	p_pboundcolor = GR_get_color (255, 0, 0);
	p_gridcolor = GR_get_color (55, 55, 0);
	p_mapcolor = GR_get_color (255, 255, 255);

	/*
	p_mainmenu = new C_PopUp ();
	p_projmenu = new C_PopUp ("PROJECTIONS");
	p_mainmenu->add_entry ("Zoom", zoomfunc, (char *) 0, this);
	p_mainmenu->add_entry ("Projections", p_projmenu);
	p_mainmenu->add_entry ("Custom Projection", customprojfunc, (char *) 0, this);
	p_mainmenu->add_entry ("Toggle Map", mapfunc, (char *) 0, this);
	p_mainmenu->add_entry ("Toggle Grid", gridfunc, (char *) 0, this);
	p_mainmenu->add_entry ("Print Grid", printgrid, (char *) 0, this);
	p_mainmenu->add_entry ("Toggle Boundaries", pboundfunc, (char *) 0, this);
	quitmenu = new C_PopUp ();
	quitmenu->add_entry ("Confirm", quitfunc, (char *) 1, this);
	//quitmenu->add_entry ("Local", quitfunc, (char *) 0, this);
	p_mainmenu->add_entry ("Quit", quitmenu);
	*/
	p_shell->realize ();
}

/*
void
GR_MapWindow::realize ()
{
	MapWCBstruct	*cbstruct;
	ProjElem		*ptr;
	Widget		button;

	ptr = p_projlist;

	while (ptr)
	{
		button = XtVaCreateManagedWidget (NULL, xmPushButtonWidgetClass,
			p_proj_pulldown,
			XMSTRING (XmNlabelString, ptr->name),
			NULL);
		
		cbstruct = new MapWCBstruct;
		cbstruct->mapwindow = this;
		cbstruct->data = ptr->name;

		XtAddCallback (button, XmNactivateCallback, set_proj, (XtPointer) cbstruct);
		ptr = ptr->next;
	}
	p_shell->realize ();
}
*/

GR_MapProjection*
GR_MapWindow::getMapProjection ()
{
	if (p_curproj)
		return	p_curproj->projection;
	else
		return 0;
}

void
GR_MapWindow::drawBackground ()
{
	if (!GR_pickmode ())
	{
		GR_linewidth (1);
		if (p_mapvis && p_map != 0)
		{
			GR_color (p_mapcolor);
			p_map->drawmap ();
		}

		if (p_mapvis && p_pboundvis && p_polimap != 0)
		{
			GR_color (p_pboundcolor);
			p_polimap->drawmap ();
		}

		if (p_gridvis && p_grid != 0)
		{
			GR_color (p_gridcolor);
			p_grid->drawmap ();
		}
	}
}

#ifdef MENUSDONE
void
quitfunc (C_PopUp*, long , char*, void*)
{
	GR_exit ();
}

void
gridfunc (C_PopUp*, long , char*, void* objectptr)
{
	C_MapWindow		*window;

	window = (C_MapWindow *) objectptr;
	window->set_gridvis (!(window->gridvis()));
}

void
mapfunc (C_PopUp*, long , char*, void* objectptr)
{
	C_MapWindow		*window;

	window = (C_MapWindow *) objectptr;
	window->set_mapvis (!(window->mapvis()));
}

void
pboundfunc (C_PopUp*, long , char*, void* objectptr)
{
	C_MapWindow		*window;

	window = (C_MapWindow *) objectptr;
	window->set_pboundvis (!(window->pboundvis()));
}

void
customprojfunc (C_PopUp* menu, long , char*, void* objectptr)
{
	C_MapWindow		*window;

	window = (C_MapWindow *) objectptr;

	if (window->p_projpanel)
		window->p_projpanel->visible (1);
}

void
unzoomfunc (C_PopUp*, long, char*, void* objectptr)
{
	C_MapWindow 	*window;

	window = (C_MapWindow *) objectptr;

	window->unzoom (MW_UNZOOM_ALL);
}

void
onefunc (C_PopUp*, long, char*, void* objectptr)
{
	C_MapWindow 	*window;

	window = (C_MapWindow *) objectptr;

	window->unzoom (MW_UNZOOM_ONE);
}

void
projfunc (C_PopUp*, long, char* projname, void* objectptr)
{
	C_MapWindow	*window;

	window = (C_MapWindow *) objectptr;

	window->set_proj (projname);
}
#endif


void
GR_MapWindow::set_sourcefile(char* name, int type)
{
	switch (type)
	{
		case	MAP:
			p_mainmap.read_map (name);
			break;
		case	PBOUND:
			p_mainpolimap.read_map (name);
			break;
	}
	if (p_curproj != 0)
		this->makemap (type);
}

void
GR_MapWindow::makemap(int type, long force)
{
	GR_MapProjection	*projection;
	GR_Map					**map, *mainmap;
	Point					*array = 0, *tmparray = 0;
	long					i, j;
	char					string[80];

	projection = (GR_MapProjection *) p_curproj->projection;

	for (i=0, j=0; i<strlen(p_curproj->name); i++)
	{
		if (!isspace(p_curproj->name[i]))
		{
			string[j] = p_curproj->name[i];
			j++;
		}
	}

	switch (type)
	{
		case	MAP:
			mainmap = &p_mainmap;
			map = &p_map;
			strcpy (&string[j], ".MAP");
			break;
		case	PBOUND:
			mainmap = &p_mainpolimap;
			map = &p_polimap;
			strcpy (&string[j], ".PBOUND");
			break;
		case	GRID:
			mainmap = &p_maingrid;
			map = &p_grid;
			strcpy (&string[j], ".GRID");
			break;
	}

	if (*map != 0)
		delete *map;
	
	*map = projection->convert_map (mainmap, string, force);
}

void
GR_MapWindow::setMapProjection(char* name)
{
	ProjElem			*ptr;
	GR_DispListElem *dlptr;
	GR_ProjDispList	*displist;

	this->unzoom ();

	//setcursor (CURS_HOURGLASS, 0, 0);
	ptr =  p_projlist;

	while (ptr)
	{
		if (strcmp(ptr->name, name) == 0)
		{
			p_curproj = ptr;	
			if (p_curproj != 0)
			{
				this->makemap (MAP, 1);
				this->makemap (PBOUND, 1);
				this->makemap (GRID, 1);
			}
			this->proj_change ();
			dlptr = GR_Window::p_displist_list;
			while( dlptr )
			{
				displist = (GR_ProjDispList *) dlptr->displist;
				displist->new_projection( p_curproj->projection );
				dlptr = dlptr->next;
			}
			break;
		}
		ptr = ptr->next;
	}
	//setcursor (CURS_ARROW, 0, 0);
}

void
GR_MapWindow::set_gridres (float latres, float lonres)
{
	p_latres = P_DEG_TO_RAD(fabs(latres));
	p_lonres = P_DEG_TO_RAD(fabs(lonres));
	p_make_grid ();

}

void
GR_MapWindow::p_make_grid ()
{
	float		lat, lon;
	Point		*array;
	long		pnt_idx;

	array = new Point[(long)(2*M_PI / p_lonres) + 1];
	for (lat = -M_PI_2 + p_latres; lat <= M_PI_2 - p_latres; lat += p_latres)
	{
		pnt_idx = 0;
		for (lon = -M_PI; lon <= M_PI; lon += p_lonres)
		{
			array[pnt_idx].longitude = lon;
			array[pnt_idx].latitude = lat;
			pnt_idx++;
		}
		p_maingrid.add_polyline (array, pnt_idx);
	}

	delete array;

	array = new Point[(long)(M_PI / p_latres) + 1];
	for (lon = -M_PI; lon <= M_PI; lon += p_lonres)
	{
		pnt_idx = 0;
		for (lat = -M_PI_2 + p_latres; lat <= M_PI_2 - p_latres; lat += p_latres)
		{
			array[pnt_idx].longitude = lon;
			array[pnt_idx].latitude = lat;
			pnt_idx++;
		}
		p_maingrid.add_polyline (array, pnt_idx);
	}
}

void
GR_MapWindow::add_object (GR_ProjListObj *object, char *name)
{
	GR_DispListElem		*listelem;

	listelem = GR_Window::p_displist_list;

	while (listelem)
	{
		if (strcmp (listelem->name, name) == 0)
		{
			listelem->displist->add_object (object);
			return;
		}
		listelem = listelem->next;
	}
}

void
GR_MapWindow::delete_all (char *name )
{
	GR_DispListElem		*listelem;

	listelem = GR_Window::p_displist_list;

	while (listelem)
	{
		if (strcmp (listelem->name, name) == 0)
		{
			listelem->displist->delete_objects ();
		}
		listelem = listelem->next;
	}
}

void
GR_MapWindow::delete_object (GR_ProjListObj *object, char *name)
{
	GR_DispListElem		*listelem;

	listelem = GR_Window::p_displist_list;

	while (listelem)
	{
		if (strcmp (listelem->name, name) == 0)
		{
			listelem->displist->delete_object (object);
			return;
		}
		listelem = listelem->next;
	}
}

/*
void
GR_MapWindow::set_projpanel (C_PanelWindow *panel)
{
	p_projpanel = panel;
}
*/

/*
void
printgrid (C_PopUp*, long, char*, void* pointer)
{
	C_MapWindow *mapwindow;

	mapwindow = (C_MapWindow *) pointer;
	printf ("main grid:\n");
	mapwindow->p_maingrid.print_map ();
	printf ("transformed grid:\n");
	mapwindow->p_grid->print_map ();
}
*/

void
GR_MapWindow::addMapProjection (GR_MapProjection *projection, char *name)
{
	ProjElem	*ptr;
	Widget button;
	MapWCBstruct	*cbstruct;

	ptr = p_projlist;

	if (!projection)
		return;

	if (!ptr)
	{
		p_projlist = new ProjElem;
		p_projlist->projection = projection;
		p_projlist->name = new char [strlen (name) + 1];
		p_projlist->next = 0;
		strcpy (p_projlist->name, name);
		ptr = p_projlist;
		this->setMapProjection (name);
	}
	else
		while (ptr)
		{
			if (strcmp (ptr->name, name) == 0)
			{
				ptr->projection = projection;
				return;
			}
			else if (ptr->next == 0)
			{
				ptr->next = new ProjElem;
				ptr = ptr->next;
				ptr->projection = projection;
				ptr->name = new char [strlen (name) + 1];
				ptr->next = 0;
				strcpy (ptr->name, name);
				if (!p_curproj)
					p_curproj = ptr;
				break;
			}
			ptr = ptr->next;
		}

	button = XtVaCreateManagedWidget (NULL, xmPushButtonWidgetClass,
		p_proj_pulldown,
		XMSTRING (XmNlabelString, ptr->name),
		NULL);
	
	cbstruct = new MapWCBstruct;
	cbstruct->mapwindow = this;
	cbstruct->data = ptr->name;

	XtAddCallback (button, XmNactivateCallback, set_proj, (XtPointer) cbstruct);
	XtRealizeWidget (button);
}

void
GR_MapWindow::rectEvent (short x1, short y1, short x2, short y2)
{
	ViewSize		*saveview;
	float				left, right, bottom, top;
	short tmp;

	saveview = new ViewSize;
	saveview->next = 0;
	saveview->left = this->left ();
	saveview->right = this->right ();
	saveview->bottom = this->bottom ();
	saveview->top = this->top ();

	if (p_viewlist == 0)
	{
		p_viewlist = saveview;
		XtVaSetValues (p_unzoom_w,
			XmNsensitive, TRUE,
			NULL);
	}
	else
	{
		if (p_viewlist->next == 0)
			XtVaSetValues (p_unzoom_one_w,
				XmNsensitive, TRUE,
				NULL);
		saveview->next = p_viewlist;
		p_viewlist = saveview;
	}

	if (x1 > x2)
	{
		tmp = x1;
		x1 = x2;
		x2 = tmp;
	}

	if (y2 > y1)
	{
		tmp = y1;
		y1 = y2;
		y2 = tmp;
	}

	XtVaSetValues (p_zoom_w,
		XmNsensitive, TRUE,
		NULL);

	XtVaSetValues (p_cancel_w,
		XmNsensitive, FALSE,
		NULL);

	this->convert_screen2 (x1, y1, left, bottom);
	this->convert_screen2 (x2, y2, right, top);

	this->world (left, right, bottom, top);
	this->draw ();
}

void
GR_MapWindow::unzoom ()
{
	ViewSize		*ptr, *dptr;

	dptr = p_viewlist;
	if (dptr)
	{
		ptr = dptr->next;
		delete dptr;
	}
	else
		return;

	while (ptr)
	{
		dptr = ptr;
		ptr = dptr->next;
		delete dptr;
	}

	XtVaSetValues (p_unzoom_w,
		XmNsensitive, FALSE,
		NULL);

	XtVaSetValues (p_unzoom_one_w,
		XmNsensitive, FALSE,
		NULL);

	p_viewlist = 0;
	this->world (MW_XMIN, MW_XMAX, MW_YMIN, MW_YMAX);
}

void
GR_MapWindow::unzoom_one ()
{
	ViewSize *ptr;

	ptr = p_viewlist;

	if (ptr)
	{
		p_viewlist = ptr->next;
		if (p_viewlist == 0)
		{
			XtVaSetValues (p_unzoom_w,
				XmNsensitive, FALSE,
				NULL);

			XtVaSetValues (p_unzoom_one_w,
				XmNsensitive, FALSE,
				NULL);
		}

		this->world (ptr->left, ptr->right, ptr->bottom, ptr->top);
	}
}
