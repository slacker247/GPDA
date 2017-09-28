#ifndef GR_MAP
#define GR_MAP

#define INLINE inline

#define		PL_LINE						0
#define		PL_CLOSED					1
#define		PL_FILL						3
#define		PL_USE_COLOR			4
#define		PL_END_ARROW			8
#define		PL_BEG_ARROW			16

struct Point
{
	union	
	{
		float	longitude;
		float	x;
	};

	union
	{
		float	latitude;
		float	y;
	};
};

class GR_PolyLine
{
	private:
		long		p_count;
		long		p_polycolor;
		long		p_arrowobj;
		float		p_arrow_length;
		float		p_arrow_height;
		char		p_type;
		char		p_red;
		char		p_grn;
		char		p_blu;
		Point		*p_pointarray;
		GR_PolyLine		*p_next;

	public:
		GR_PolyLine (char	type = PL_LINE);

		~GR_PolyLine ();

		void pointarray (Point *pointarray, long count);

		Point* pointarray (long &count);

		void set_color (char red, char grn, char blu);

		void set_type (char type);

		void set_type (GR_PolyLine*);

		void set_arrow (float length = 0.01, float height = 0.005);

		void drawpoly (float depth = 0.0);
		void printpoly ();

		GR_PolyLine* next ();
		void next (GR_PolyLine *);
		void next (GR_PolyLine &);
};

INLINE GR_PolyLine*
GR_PolyLine::next ()
{
	return p_next;
}

INLINE void
GR_PolyLine::next (GR_PolyLine *next_line)
{
	p_next = next_line;
}

INLINE void
GR_PolyLine::next (GR_PolyLine &next_line)
{
	p_next = &next_line;
}

INLINE void
GR_PolyLine::set_type (char type)
{
	p_type = type;
}

INLINE void
GR_PolyLine::set_arrow (float length, float height)
{
	p_arrow_length = length;
	p_arrow_height = height;
}

class GR_Map
{
	private:
		GR_PolyLine		*p_maplist;
		GR_PolyLine		*p_lastline;
		GR_PolyLine		*p_curline;
		float			p_z_depth;

	public:
		GR_Map (char *mapfile = 0);
		~GR_Map ();

		long read_map (char *filename);

		void write_map (char *filename);

		void print_map ();

		void add_polyline (Point *array, long size);

		void add_polyline (GR_PolyLine& polyline);

		void add_polyline (GR_PolyLine* polyline);

		Point* get_first_polyline (long &size);

		GR_PolyLine* get_first_polyline ();

		Point* get_next_polyline (long &size);

		GR_PolyLine* get_next_polyline ();

		void drawmap ();

		void set_depth (float depth);

		void delete_map ();
};

INLINE void
GR_Map::add_polyline (GR_PolyLine& polyline)
{
	this->add_polyline (&polyline);
}

INLINE GR_PolyLine*
GR_Map::get_first_polyline ()
{
	p_curline = p_maplist;
	return p_curline;
}

INLINE void
GR_Map::set_depth (float depth)
{
	p_z_depth = depth;
}

#endif
