#include "GR_ReadProj.H"
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define		RP_UNDEFINE	0
#define		RP_PROJTYPE	100
#define		RP_PROJNAME	200
#define		RP_CENTER		300
#define		RP_WRITE		400

#define		RP_MERCATOR		101
#define		RP_RECTANGULAR	102
#define		RP_POLAR				103
#define		RP_AZIMUTH_EQ	104

//long open_save_panel (C_PanelButton*, void*, void*);
//long cancel_save (C_PanelButton*, void*, void*);


struct ParseTable
{
	char	*tokenname;
	long	tokentype;
};

long
parseit (ParseTable parsetable[], char *token)
{
	long i, length, state;
	char	parse_token[80];

	//	Set all characters to upper case

	length = strlen (token);
	for (i=0; i<length; i++)
		parse_token[i] = toupper(token[i]);

	parse_token[i] = 0;

	//	Scan the parsetable for a token match
	for (i=0;; i++)
	{
		if (parsetable[i].tokentype == RP_UNDEFINE)
			return RP_UNDEFINE;

		if (	
			strncmp (
				parse_token, 
				parsetable[i].tokenname, 
				strlen(parsetable[i].tokenname)
			) 
			== 0
		)
		{
			state = parsetable[i].tokentype;
			break;
		}
	}

	return state;
}

long
scanwhite (char *string)
{
	long	index = 0;

	while (isspace(string[index]) && string[index] != 0)
		index++;
	
	return index;
}

ParseTable	
tokentable[] = 
{
	"PROJECTION", RP_PROJTYPE,
	"NAME", RP_PROJNAME,
	"CENTER", RP_CENTER,
	"WRITE", RP_WRITE,
	"", RP_UNDEFINE						// Table end marker
};

ParseTable
classtable[] =
{
	"MERCATOR", RP_MERCATOR,
	"RECTANGULAR", RP_RECTANGULAR,
	"CYLINDRICAL", RP_RECTANGULAR,
	"POLAR", RP_POLAR,
	"SPHERICAL", RP_POLAR,
	"AZIMUTH_EQ", RP_AZIMUTH_EQ,
	"AZIMUTHEQ", RP_AZIMUTH_EQ,
	"", RP_UNDEFINE
};

GR_ReadProj::GR_ReadProj (char *filename)
{
	FILE		*fileptr;
	char		line[81];
	char		token[81];
	long		index, length, state, token_num = 0, linenum;
	long		needtoken, i, parsing = 1;
	float		lat, lon;
	MProj		*projelem, *lastelem;
	char		*tokenarray [32];

	p_mapwindow = 0;
	p_lat_deg = 0;
	p_lat_min = 0;
	p_lat_sec = 0;
	p_lon_deg = 0;
	p_lon_min = 0;
	p_lon_sec = 0;
	p_writestate = 0;

	//p_make_panel ();
	//p_savefile->setvalue (filename);

	fileptr = fopen (filename, "r");
	if (fileptr == NULL)
	{
		fprintf (stderr, "Unable to open %s\n", filename);
		return;
	}

	state = RP_UNDEFINE;
	projelem = 0;
	p_projlist = 0;
	lastelem = 0;
	needtoken = 1;
	for (linenum = 1; parsing ; linenum++)
	{
		if (fgets (line, 80, fileptr) == NULL)
		{
			line [0] = 0;
			token [0] = 0;
			parsing = 0;
		}

		index = 0;

		for (;;)
		{
			if (needtoken)
			{
				index += scanwhite (&line[index]);
				if (line[index] != 0 || !parsing)
				{
					if (line[index] == ';')
						break;

					if (index > strlen(line))
						length = 0;
					else
					{
						sscanf (&line[index], "%s", token);
						length = strlen (token);
					}
					index += length;
				}
				else
					break;
			}


			if (state == RP_UNDEFINE)
			{
				state = parseit (tokentable, token);
				if (state == RP_UNDEFINE)
				{
					fprintf (stderr, "ERROR: undefined token, %s in file, %s\n", token, filename);
					return;
				}

				for (i=0; i<token_num; i++)
					delete tokenarray[i];

				token_num = 0;
				needtoken = 1;
			}
			else
			{
				if (!parsing || parseit (tokentable, token) != RP_UNDEFINE)
				{
					switch (state)
					{
						case		RP_PROJTYPE:
							//	Previous projection definition is added to the list
							if (projelem != 0)
							{
								if (lastelem)
									lastelem->next = projelem;
								else
									p_projlist = projelem;

								lastelem = projelem;
							}

							projelem = new MProj;
							projelem->next = 0;
							if (token_num < 1)
							{
								fprintf (stderr, "ERROR projection type expected on line %d\n", linenum);
								return;
							}
							projelem->p_type =  parseit (classtable, tokenarray[0]);
							switch (projelem->p_type)
							{
								case		RP_MERCATOR:
									projelem->p_projection = new GR_Mercator;
									break;
								case		RP_RECTANGULAR:
									projelem->p_projection = new GR_Cylindrical;
									break;
								case		RP_POLAR:
									projelem->p_projection = new GR_Spherical;
									break;
								case		RP_AZIMUTH_EQ:
									projelem->p_projection = new GR_AzimuthEQ;
									break;
							}
							break;

						case		RP_PROJNAME:
							if (projelem == 0)
							{
								fprintf (stderr, "ERROR: No projection to name on line %d\n", linenum);
								return;
							}
							if (token_num < 1)
							{
								fprintf (stderr, "ERROR no name given on line %s\n", linenum);
								return;
							}

							projelem->p_name = new char [strlen (tokenarray[0]) + 1];
							strcpy (projelem->p_name, tokenarray[0]);
							break;
						case		RP_CENTER:
							if (projelem == 0)
							{
								fprintf (stderr, "ERROR: No projection to set center on line %d\n", linenum);
								return;
							}
							if (token_num < 2)
							{
								fprintf (stderr, "ERROR: Must have both lat and lon on line %d\n", linenum);
								return;
							}
							sscanf (tokenarray[0], "%f", &lat);
							sscanf (tokenarray[1], "%f", &lon);
							projelem->p_projection->set_center (lat, lon);
							break;
						
						case		RP_WRITE:
							if (projelem == 0)
							{
								fprintf (stderr, "ERROR: No projection to set write on line %d\n", linenum);
								return;
							}
							projelem->p_projection->set_write ();
					}
					state = RP_UNDEFINE;
					needtoken = 0;
				}
				else
				{
					tokenarray[token_num] = new char [strlen (token) + 1];
					strcpy (tokenarray[token_num], token);
					token_num++;
					needtoken = 1;
				}
			}
			if (length == 0)
				break;
		}
	}

	// Get the last projection definition
	if (projelem != 0)
	{
		if (lastelem)
			lastelem->next = projelem;
		else
			p_projlist = projelem;
	}
}

void
GR_ReadProj::make_projections (GR_MapWindow* mapwindow)
{
	MProj		*projelem;

	p_mapwindow = mapwindow;

	//p_mapwindow->set_projpanel (p_projpanel);

	projelem = p_projlist;

	while (projelem)
	{
		printf ("adding %s projection\n", projelem->p_name);
		mapwindow->addMapProjection (projelem->p_projection, projelem->p_name);
		/*
		if (mapwindow.addMapProjection (projelem->p_projection, projelem->p_name)
			p_editmenu->add_entry (projelem->p_name, set_projname, projelem->p_name, this);
			*/
		projelem = projelem->next;
	}
}

#ifdef PANELSTUFF
void
GR_ReadProj::p_make_panel ()
{
	GR_WindowList		windowlist;
	long row = 0;
	C_PanelButton		*button;
	C_TextField			*textfield;
	C_IntField			*ifield;
	C_PopUp					*menu;
	C_PanelTogBut		*togglebutton;
	C_PanelWindow		*savepanel;

	p_projpanel = new C_PanelWindow (5, 5);
	p_projpanel->title ("Projection Panel");
	p_projpanel->visible (0);
	p_projpanel->position (640, 512);

	savepanel = new C_PanelWindow (2, 4);
	savepanel->title ("Save Config");
	savepanel->visible (0);
	savepanel->position (512, 512);

	p_savefile = new C_TextField (0, 32);
	p_savefile->height (PANELBUTTON_H);
	p_savefile->width (4.0 * PANELBUTTON_W + 3.0 * PANELBUTTON_SPACE);
	p_savefile->set_label ("FILENAME:");
	savepanel->add_object (0, 0, p_savefile);

	button = new C_PanelButton;
	button->set_text ("SAVE");
	button->set_function (save_config, this);
	button->set_data (savepanel);
	savepanel->add_object (1, 0, button);

	button = new C_PanelButton;
	button->set_text ("CANCEL");
	button->set_function (cancel_save, savepanel);
	savepanel->add_object (1, 3, button);

	row = 0;
	button = new C_PanelButton;
	button->set_text ("EDIT");
	p_editmenu = new C_PopUp ("Projections...");
	button->set_menu (p_editmenu);
	p_projpanel->add_object (row, 0, button);

	p_projname = new C_TextField(0, 32);
	p_projname->height (PANELBUTTON_H);
	p_projname->width (4.0 * PANELBUTTON_W + 3.0 * PANELBUTTON_SPACE);
	p_projname->set_label ("NAME:");
	p_projname->set_function (set_projname, this);
	p_projpanel->add_object (row, 1, p_projname);

	menu = new C_PopUp ("PROJECTIONS");
	menu->add_entry ("RECTANGULAR", projtype, (char*) RP_RECTANGULAR, this);
	menu->add_entry ("MERCATOR", projtype, (char*) RP_MERCATOR, this);
	menu->add_entry ("SPHERICAL", projtype, (char*) RP_POLAR, this);
	menu->add_entry ("AZIMUTHAL EQUIDISTANT", projtype, (char*) RP_AZIMUTH_EQ, this);

	row++;
	button = new C_PanelButton;
	button->set_text ("TYPE");
	button->set_menu (menu);
	p_projpanel->add_object (row, 0, button);

	p_projtype = new C_TextField(0, 32);
	p_projtype->height (PANELBUTTON_H);
	p_projtype->width (4.0 * PANELBUTTON_W + 3.0 * PANELBUTTON_SPACE);
	p_projtype->setvalue ("RECTANGULAR");
	p_proj_type = RP_RECTANGULAR;
	p_projtype->pickable (0);
	p_projpanel->add_object (row, 1, p_projtype);

	row++;
	textfield = new C_TextField("CENTER LAT", 10);
	textfield->height (PANELBUTTON_H);
	textfield->width (1.75 * PANELBUTTON_W + PANELBUTTON_SPACE);
	textfield->pickable (0);
	p_projpanel->add_object (row, 0, textfield);

	ifield = new C_IntField (&p_lat_deg, "%4d", 4);
	ifield->height (PANELBUTTON_H);
	ifield->width (1.25 * PANELBUTTON_W);
	ifield->set_label ("D:");
	p_projpanel->add_object (row, 1.75, ifield);

	ifield = new C_IntField (&p_lat_min, "%3d", 3);
	ifield->height (PANELBUTTON_H);
	ifield->width (PANELBUTTON_W);
	ifield->set_label ("M:");
	p_projpanel->add_object (row, 3, ifield);

	ifield = new C_IntField (&p_lat_sec, "%3d", 3);
	ifield->height (PANELBUTTON_H);
	ifield->width (PANELBUTTON_W);
	ifield->set_label ("S:");
	p_projpanel->add_object (row, 4, ifield);

	row++;
	textfield = new C_TextField("CENTER LON", 10);
	textfield->height (PANELBUTTON_H);
	textfield->width (1.75 * PANELBUTTON_W + PANELBUTTON_SPACE);
	textfield->pickable (0);
	p_projpanel->add_object (row, 0, textfield);

	ifield = new C_IntField (&p_lon_deg, "%4d", 4);
	ifield->height (PANELBUTTON_H);
	ifield->width (1.25 * PANELBUTTON_W);
	ifield->set_label ("D:");
	p_projpanel->add_object (row, 1.75, ifield);

	ifield = new C_IntField (&p_lon_min, "%3d", 3);
	ifield->height (PANELBUTTON_H);
	ifield->width (PANELBUTTON_W);
	ifield->set_label ("M:");
	p_projpanel->add_object (row, 3, ifield);

	ifield = new C_IntField (&p_lon_sec, "%3d", 3);
	ifield->height (PANELBUTTON_H);
	ifield->width (PANELBUTTON_W);
	ifield->set_label ("S:");
	p_projpanel->add_object (row, 4, ifield);

	row++;
	button = new C_PanelButton;
	button->set_text ("DONE");
	button->set_function (cancel_proj, this);
	p_projpanel->add_object (row, 0, button);

	button = new C_PanelButton;
	button->set_text ("DO IT");
	button->set_function (commit_proj, this);
	p_projpanel->add_object (row, 1, button);

	p_writebutton = new C_PanelTogBut;
	p_writebutton->set_text ("WRITE");
	p_writebutton->set_on_function (set_writestate, this);
	p_writebutton->set_on_data ((void *)1);
	p_writebutton->set_off_function (set_writestate, this);
	p_writebutton->set_off_data ((void *)0);
	p_projpanel->add_object (row, 2, p_writebutton);

	button = new C_PanelButton;
	button->button_width (2);
	button->set_text ("SAVE CONFIG");
	button->set_function (open_save_panel, savepanel);
	p_projpanel->add_object (row, 3, button);

	windowlist.add_window (*p_projpanel);
	windowlist.add_window (*savepanel);
}

void
projtype (C_PopUp*, long , char* type, void* objectptr)
{
	C_ReadProj		*readproj;

	readproj = (C_ReadProj *) objectptr;

	readproj->p_proj_type = (long)type;
	switch (readproj->p_proj_type)
	{
		case RP_RECTANGULAR:
			readproj->p_projtype->setvalue ("RECTANGULAR");
			break;
		case RP_MERCATOR:
			readproj->p_projtype->setvalue ("MERCATOR");
			break;
		case RP_POLAR:
			readproj->p_projtype->setvalue ("SPHERICAL");
			break;
		case RP_AZIMUTH_EQ:
			readproj->p_projtype->setvalue ("AZIMUTHAL_EQUIDISTANT");
			break;
	}
}

long
cancel_proj (C_PanelButton*, void* object, void*)
{
	C_ReadProj		*readproj;

	readproj = (C_ReadProj *) object;

	readproj->p_projpanel->visible (0);
}

long
commit_proj (C_PanelButton*, void* object, void*)
{
	C_MapWindow	*mapwindow;
	C_ReadProj	*readproj;
	C_ProjDispList		*displist;
	C_ListElem				*listelem;
	C_MapProjection			*projection;
	float		lat, lon, direction;
	MProj		*projelem;
	char		*projname;

	readproj = (C_ReadProj *) object;
	mapwindow = readproj->p_mapwindow;

	switch (readproj->p_proj_type)
	{
		case	RP_RECTANGULAR:
			projection = new C_Cylindrical;
			break;
		case	RP_MERCATOR:
			projection = new C_Mercator;
			break;
		case	RP_POLAR:
			projection = new C_Spherical;
			break;
		case	RP_AZIMUTH_EQ:
			projection = new C_AzimuthEQ;
			break;
	}

	direction = (readproj->p_lat_deg < 0) ? -1.0 : 1.0;
	lat = readproj->p_lat_deg 
				+ direction 
				* (readproj->p_lat_min / 60.0 + readproj->p_lat_sec / (60.0 * 60.0));

	direction = (readproj->p_lon_deg < 0) ? -1.0 : 1.0;
	lon = readproj->p_lon_deg 
				+ direction 
				* (readproj->p_lon_min / 60.0 + readproj->p_lon_sec / (60.0 * 60.0));

	projection->set_center (lat, lon);
	if (readproj->p_writestate)
		projection->set_write ();
	else
		projection->set_nowrite ();

	projname = readproj->p_projname->getvalue ();
	if (mapwindow->add_proj (projname, *projection))
	{
			readproj->p_editmenu->add_entry (projname, set_projname, projname, readproj);
			projelem = new MProj;

			projelem->p_name = new char [strlen (projname) + 1];
			strcpy (projelem->p_name, projname);
			readproj->p_projlist.add_tail (projelem);
	}	
	else
	{
		projelem = (MProj *) readproj->p_projlist.head ();
		while (projelem)
		{
			if (strcmp (projelem->p_name, projname) == 0)
				break;

			projelem = (MProj *) readproj->p_projlist.next ();
		}
		if (!projelem)
			fprintf (stderr, "WARNING: edited projection, %s not found in ReadProj::p_projlist!!!\n", projname);
	}

	if (projelem)
	{
		projelem->p_projection = projection;
		projelem->p_type = readproj->p_proj_type;
	}

	mapwindow->set_proj (projname, 1);
}

void
set_projname (void* data, void *object)
{
	C_ReadProj		*readproj;
	char					*name;

	readproj = (C_ReadProj *) object;
	name = (char *) data;

	readproj->edit_projection (name);

}

void
set_projname (C_PopUp*, long, char* name, void* object)
{
	C_ReadProj	*readproj;

	readproj = (C_ReadProj *) object;

	readproj->edit_projection (name);
}

void
C_ReadProj::edit_projection (char* projname)
{
	MProj		*projelem;
	float		lat, lon;
	float		min, sec;

	projelem = (MProj *) p_projlist.head ();

	while (projelem)
	{
		if (strcmp (projname, projelem->p_name) == 0)
			break;
		projelem = (MProj *) p_projlist.next ();
	}

	if (!projelem)
		return;

	p_projname->setvalue (projname);
	projelem->p_projection->get_center (lat, lon);
	p_writebutton->set_state (projelem->p_projection->writestate());

	min = (lat - (long)lat) * 60.0;
	sec = (min - (long)min) * 60.0;
	p_lat_deg = (long) lat;
	p_lat_min = (long) min;
	p_lat_sec = (long) sec;

	min = (lon - (long)lon) * 60.0;
	sec = (min - (long)min) * 60.0;
	p_lon_deg = (long) lon;
	p_lon_min = (long) min;
	p_lon_sec = (long) sec;

	switch (projelem->p_type)
	{
		case		RP_MERCATOR:
			p_projtype->setvalue ("MERCATOR");
			break;
		case		RP_RECTANGULAR:
			p_projtype->setvalue ("RECTANGULAR");
			break;
		case		RP_POLAR:
			p_projtype->setvalue ("SPHERICAL");
			break;
		case		RP_AZIMUTH_EQ:
			p_projtype->setvalue ("AZIMUTH_EQ");
			break;
	}
	p_proj_type = projelem->p_type;
}

long
set_writestate (C_PanelButton* button, void* object, void* state)
{
	C_ReadProj		*readproj;

	readproj = (C_ReadProj *) object;
	readproj->p_writestate = (long) state;
}

long
save_config (C_PanelButton*, void* object, void* data)
{
	MProj		*projelem;
	C_ReadProj		*readproj;
	C_PanelWindow	*panel;
	char				line [80];
	float		lat, lon;
	char		*filename;
	FILE		*fileptr;

	panel = (C_PanelWindow *) data;
	readproj = (C_ReadProj *) object;
	filename = readproj->p_savefile->getvalue ();
	if (!filename)
		return 1;

	fileptr = fopen (filename, "w");
	if (fileptr == NULL)
	{
		fprintf (stderr, "Failed to Open %s\n", filename);
		return 1;
	}

	projelem = (MProj *) readproj->p_projlist.head ();
	while (projelem)
	{
		fprintf (fileptr, "Projection: ");
		switch (projelem->p_type)
		{
			case		RP_MERCATOR:
				fprintf (fileptr, "Mercator\n");
				break;
			case		RP_RECTANGULAR:
				fprintf (fileptr, "Rectangular\n");
				break;
			case		RP_POLAR:
				fprintf (fileptr, "Polar\n");
				break;
			case		RP_AZIMUTH_EQ:
				fprintf (fileptr, "Azimuth_EQ\n");
				break;
		}

		fprintf (fileptr, "\tName: %s\n", projelem->p_name);
		projelem->p_projection->get_center (lat, lon);
		fprintf (fileptr, "\tCenter: %8.4f %8.4f\n", lat, lon);
		if (projelem->p_projection->writestate ())
			fprintf (fileptr, "\tWrite\n");
		
		fprintf (fileptr, "\n");

		projelem = (MProj *) readproj->p_projlist.next ();
	}
	fclose (fileptr);
	panel->visible (0);
}

long
open_save_panel (C_PanelButton*, void* object, void*)
{
	C_PanelWindow		*panel;

	panel = (C_PanelWindow *) object;

	panel->visible (1);
}

long
cancel_save (C_PanelButton*, void* object, void*)
{
	C_PanelWindow		*panel;

	panel = (C_PanelWindow *) object;

	panel->visible (0);
}
#endif
