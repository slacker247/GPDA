#include "GR_Map.H"
#include <memory.h>
#include <math.h>
#include "GR_Interface.H"
#include <stdio.h>

GR_PolyLine::GR_PolyLine (char type)
{
	p_type = type;
	p_count = 0;
	p_pointarray = 0;
	p_arrow_length = .01;
	p_arrow_height = .005;
	p_arrowobj = -1;
	p_next = 0;
}

GR_PolyLine::~GR_PolyLine ()
{
	if (p_pointarray != 0)
		delete p_pointarray;

	if (p_arrowobj >= 0)
		GR_delobj (p_arrowobj);
}

Point*
GR_PolyLine::pointarray (long& count)
{
	//fprintf (stderr, "returning %d points\n", p_count);
	count = p_count;
	return p_pointarray;
}

void
GR_PolyLine::pointarray (Point *parray, long count)
{
	if (parray == 0 || count == 0)
		return;

	if (p_pointarray != 0)
		delete p_pointarray;

	if (p_arrowobj >= 0)
	{
		GR_delobj (p_arrowobj);
		p_arrowobj = -1;
	}

	p_count = count;

	p_pointarray = new Point [count];

	if (p_pointarray == 0)
	{
		fprintf (stderr, "GR_PolyLine::pointarray MEMORY ALLOCATE ERROR (failure to allocate space for %d points)\n", count);
		exit (0);
	}

	memcpy (p_pointarray, parray, count * sizeof (Point));
	//fprintf (stderr, "%d points created\n", p_count);
}

void
GR_PolyLine::set_color (char red, char grn, char blu)
{
	p_type = p_type | PL_USE_COLOR; // set color bit to on
	p_red = red;
	p_grn = grn;
	p_blu = blu;

	p_polycolor = GR_color ((short) red, (short) grn, (short) blu);
}

void
GR_PolyLine::set_type (GR_PolyLine* polyline)
{
	p_type = polyline->p_type;
	p_arrow_length = polyline->p_arrow_length;
	p_arrow_height = polyline->p_arrow_height;
	if (p_type & PL_USE_COLOR)
	{
		p_polycolor = polyline->p_polycolor;
		p_red = polyline->p_red;
		p_grn = polyline->p_grn;
		p_blu = polyline->p_blu;
	}
}

void
GR_PolyLine::drawpoly (float	depth)
{
	long i;
	GR_Vector3f	vector;

	if (p_pointarray == 0 || p_count == 0)
		return;

	if (p_type & PL_USE_COLOR) // test color bit
		GR_color (p_polycolor);

	if (p_type & PL_FILL) // test for fill bit
		GR_bgnpolygon ();
	else if (p_type & PL_CLOSED)		// test for closed line
		GR_bgnclosedline ();
	else
		GR_bgnline ();

	vector [2] = depth;

	for (i=0; i<p_count; i++)
	{
		vector [0] = p_pointarray[i].x;
		vector [1] = p_pointarray[i].y;

		GR_v3f (vector);
	}

	if (p_type & PL_FILL) // test for fill bit
		GR_endpolygon ();
	else if (p_type & PL_CLOSED)		// test for closed line
		GR_endclosedline ();
	else
		GR_endline ();

	if (p_type & PL_END_ARROW)
	{
		float	angle, dx, dy;

		if (p_arrowobj < 0)
		{

			p_arrowobj = GR_makeobj ();
			dx = p_pointarray[p_count - 1].x - p_pointarray[p_count - 2].x;
			dy = p_pointarray[p_count - 1].y - p_pointarray[p_count - 2].y;

			angle = (180.0 / M_PI) * atan2 (dy, dx);
			GR_pushmatrix ();
			GR_translate (p_pointarray [p_count - 1].x, p_pointarray[p_count - 1].y);
			GR_rot (angle, 'z');
			GR_bgnpolygon ();

			vector [0] = 0.0;
			vector [1] = 0.0;
			vector [2] = depth;
			GR_v3f (vector);

			vector [0] = -p_arrow_length;
			vector [1] = p_arrow_height;
			vector [2] = depth;
			GR_v3f (vector);

			vector [0] = -p_arrow_length;
			vector [1] = -p_arrow_height;
			vector [2] = depth;
			GR_v3f (vector);

			GR_endpolygon ();
			GR_popmatrix ();
			GR_closeobj ();
		}

		GR_callobj (p_arrowobj);

	}
}

void
GR_PolyLine::printpoly ()
{
	long i;

	if (p_pointarray == 0 || p_count == 0)
		return;

	for (i=0; i<p_count; i++)
		printf ("#%d\tx = %f\ty = %f\n", i, p_pointarray[i].x, p_pointarray[i].y);
}

GR_Map::GR_Map (char *filename)
{
	p_z_depth = 0.0;
	p_maplist = 0;
	p_lastline = 0;
	p_curline = 0;

	if (filename != 0)
		read_map (filename);
}

GR_Map::~GR_Map ()
{
	this->delete_map ();
}

long
GR_Map::read_map (char *filename)
{
	GR_PolyLine	*lineptr;
	FILE		*fileptr;
	long		numread, count = 0, oldsize = 0;
	Point		*pointsbuf = 0;

	fileptr = fopen (filename, "rb");
	if (fileptr == NULL)
		return		NULL;

	delete_map ();

	while (fread ((char *) &count, sizeof (long), 1, fileptr) != NULL)
	{
		//fprintf (stderr, "reading %d points\n", count);
		lineptr = new GR_PolyLine;
		if (p_maplist == 0)
			p_maplist = lineptr;
		else
			p_lastline->next (lineptr);

		p_lastline = lineptr;

		if (oldsize < count)
		{
			if (pointsbuf != 0)
				delete pointsbuf;

			pointsbuf = new Point [count];
			oldsize = count;
		}

		numread = fread ((char *)pointsbuf, sizeof (struct Point), count, fileptr);
		if (numread != count)
		{
			fprintf (stderr, "GR_Map:: read_map FILE READ FAILURE, read %d points out of %d requested\n", numread, count);
			exit (0);
		}

		lineptr->pointarray (pointsbuf, count);
	}

	fclose (fileptr);
	return	!NULL;
}

void
GR_Map::write_map (char *filename)
{
	FILE		*fileptr;
	GR_PolyLine	*lineptr;
	Point		*pointsbuf;
	long		count;

	fileptr = fopen (filename, "wb");
	if (fileptr == NULL)
	{
		fprintf (stderr, "GR_Map::write_map FAILURE to open file, %s\n", filename);
		return ();
	}


	lineptr = p_maplist;

	while (lineptr)
	{
		pointsbuf = lineptr->pointarray (count);

		fwrite ((char *)&count, sizeof (long), 1, fileptr);
		fwrite ((char *)pointsbuf, sizeof (struct Point), count, fileptr);
		lineptr = lineptr->next ();
	}

	fclose (fileptr);
}

void
GR_Map::add_polyline (GR_PolyLine*	polyline)
{
	if (p_maplist == 0)
		p_maplist = polyline;
	else
		p_lastline->next (polyline);

	p_lastline = polyline;
}

void
GR_Map::add_polyline (Point *array, long count)
{
	GR_PolyLine	*lineptr;

	lineptr = new GR_PolyLine;

	if (lineptr == 0)
	{
		fprintf (stderr, "GR_Map::add_polyline MEMORY ALLOCATE ERROR\n");
		exit (0);
	}

	lineptr->pointarray (array, count);

	if (p_maplist == 0)
		p_maplist = lineptr;
	else
		p_lastline->next (lineptr);

	p_lastline = lineptr;
}

void
GR_Map::drawmap ()
{
	GR_PolyLine		*polyline;

	polyline = p_maplist;

	while (polyline)
	{
		polyline->drawpoly (p_z_depth);
		polyline = polyline->next ();
	}
}

void
GR_Map::print_map ()
{
	GR_PolyLine		*polyline;

	polyline = p_maplist;

	while (polyline)
	{
		printf ("\n");
		polyline->printpoly ();
		polyline = polyline->next ();
	}
}

Point*
GR_Map::get_first_polyline (long& count)
{
	GR_PolyLine		*lineptr;
	Point					*pointarray;

	lineptr = p_maplist;
	p_curline = lineptr;

	if (lineptr)
	{
		pointarray = lineptr->pointarray (count);
		return pointarray;
	}
	else
	{
		count = 0;
		return 0;
	}
}

Point*
GR_Map::get_next_polyline (long& count)
{
	Point					*pointarray;

	if (p_curline == 0)
		return 0;

	p_curline = p_curline->next ();

	if (p_curline)
	{
		pointarray = p_curline->pointarray (count);
		return pointarray;
	}
	else
	{
		count = 0;
		return 0;
	}
}

GR_PolyLine*
GR_Map::get_next_polyline ()
{
	GR_PolyLine *lineptr;

	if (p_curline == 0)
		return 0;

	p_curline = p_curline->next ();

	return p_curline;
}

void
GR_Map::delete_map ()
{
	GR_PolyLine	*lineptr, *dptr;

	dptr = p_maplist;

	if (dptr)
	{
		lineptr = dptr->next();
		delete dptr;
	}
	else
		return;

	while (lineptr)
	{
		dptr = lineptr;
		lineptr = dptr->next ();
		delete dptr;
	}
}
