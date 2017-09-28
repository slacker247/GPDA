/*		@(#)GR_Interface.C	1.11		11/13/92		*/
#include "GR_Interface.H"
#include "GR_Window.H"
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <sys/types.h>
#include <time.h>

#define GR_MODULE
//#define ONYX
#define	GR_PERSPECTIVE		3 //make it consistent with other files;
#define	GR_ORTHO		1
#define	GR_MAX_OBJS		(64 * 1024 - 1)

static float	GR_worldxmin = 0.0;
static float	GR_worldxmax = 0.0;
static float	GR_worldymin = 0.0;
static float	GR_worldymax = 0.0;
static long	GR_viewtype = -1;
static long	GR_init_done = 0;
static void	*GR_obj_table [GR_MAX_OBJS]; /* Table for identifying objects */
static short	GR_name_buffer [GR_MAX_OBJS]; /* Name buffer for picking */
static unsigned short	GR_nameidx = 0;	/* Index for assigning places into the obj_table */
static void	*GR_hit_table [GR_MAX_OBJS]; /* Table to store resulting pick hits */
static long	GR_numhits;
long		GR_pickflag = 0;

#ifndef ONYX
static long	GR_useMessagerFlag = 0;
#include "Messager.H"
#endif

Widget  	GR_toplevel;
XtAppContext	GR_appcontext;
String 		*GR_fallback_resources = 0;

static struct WindowElem
{
	GR_Window *window;
	WindowElem *next;
}*GR_windowlist;
	

void
GR_registerWindow (GR_Window *window)
{
	WindowElem *ptr;

	if (!window)
		return;

	if (!GR_windowlist)
	{
		GR_windowlist = new WindowElem;
		GR_windowlist->window = window;
		GR_windowlist->next = 0;
	}
	else
	{
		ptr = new WindowElem;
		ptr->next = GR_windowlist;
		ptr->window = window;
		GR_windowlist = ptr;
	}
}

long		
GR_get_color (short red, short green, short blue)
{
	long color_index;

	color_index = red + 256*green + 256*256*blue;
	return color_index;
}

long		
GR_color (short red, short green, short blue)
{
	long color_index;

	color_index = red + 256*green + 256*256*blue;
	if (GR_init_done)
//		cpack (color_index);
		glColor3s(red, green, blue);
	return color_index;
}

void	
GR_color (long color_idx)
{
	short red, green, blue;

	red = color_idx & 0xff;
	green = (color_idx >> 8) & 0xff;
	blue  = (color_idx >> 16) & 0xff;
	glColor3s(red, green, blue);
}

long
GR_defpattern (char* pattern)
{
//	return GR_display->p_patterntable->setpattern (pattern);
   	return 0; // to supress compiler warning;
}

long
GR_deffont (char *font)
{
//	return GR_display->p_fonttable->setfont (font);
   	return 0; // to supress compiler warning;
}

void
GR_polf (long n, float varray[][3])
{
	long i;

	glBegin(GL_POLYGON);
	for (i=0; i<n; i++)
		glVertex3f(varray[i][0], varray[i][1], varray[i][2]);
	glEnd();
}

void
GR_poly (long n, float varray[][3])
{
	long i;

	glBegin(GL_LINE_LOOP);
	for (i=0; i<n; i++)
		glVertex3f(varray[i][0], varray[i][1], varray[i][2]);
	glEnd();
}

void
GR_polf2 (long n, float varray[][2])
{
	long i;

	glBegin(GL_POLYGON);
	for (i=0; i<n; i++)
		glVertex2f(varray[i][0], varray[i][1]);
	glEnd();
}

void
GR_poly2 (long n, float varray[][2])
{
	long i;

	glBegin(GL_LINE_LOOP);
	for (i=0; i<n; i++)
		glVertex2f(varray[i][0], varray[i][1]);
	glEnd();
}

void
GR_circf (float x, float y, float radius)
{
        GLUquadricObj* circleobj;

        circleobj = gluNewQuadric();
        glTranslatef(x, y, 0.0);
        gluQuadricDrawStyle(circleobj, GLU_FILL);
        gluDisk(circleobj, 0.0, (GLdouble)radius, (GLint)180, (GLint)1);
}

void
GR_circ (float x, float y, float radius)
{
        GLUquadricObj* circleobj;

        circleobj = gluNewQuadric();
        glTranslatef(x, y, 0.0);
        gluQuadricDrawStyle(circleobj, GLU_SILHOUETTE);
        gluDisk(circleobj, 0.0, (GLdouble)radius, (GLint)180, (GLint)1);
}

void
GR_arcf(float x, float y, float radius, float sangle, float eangle)
{
	GLUquadricObj* arcobj;

	arcobj = gluNewQuadric();
	glTranslatef(x, y, 0.0);
	gluQuadricDrawStyle(arcobj, GLU_FILL);
        gluPartialDisk(arcobj, (GLdouble)0.0, (GLdouble)radius, 
                       (GLint)180, (GLint)1,
                       (GLdouble)(90.0-(0.1*sangle)),
                       (GLdouble)(0.1*(eangle-sangle)));
}

void
GR_arc(float x, float y, float radius, float sangle, float eangle)
{
	GLUquadricObj* arcobj;

	arcobj = gluNewQuadric();
	glTranslatef(x, y, 0.0);
	gluQuadricDrawStyle(arcobj, GLU_SILHOUETTE);
	gluPartialDisk(arcobj, (GLdouble)0.0, (GLdouble)radius,
		       (GLint)180, (GLint)1,
	               (GLdouble)(90.0-(0.1*sangle)),
		       (GLdouble)(0.1*(eangle-sangle)));
}

void
GR_ortho (float left, float right, float bottom, float top, float near, float far)
{
	GR_viewtype = GR_ORTHO;
	GR_worldxmin = left;
	GR_worldxmax = right;
	GR_worldymin = bottom;
	GR_worldymax = top;
	glOrtho((GLdouble)left, (GLdouble)right, (GLdouble)bottom,
                (GLdouble)top,  (GLdouble)near,  (GLdouble)far);
}

void
GR_ortho2 (float left, float right, float bottom, float top)
{
	GR_viewtype = GR_ORTHO;
	GR_worldxmin = left;
	GR_worldxmax = right;
	GR_worldymin = bottom;
	GR_worldymax = top;
	gluOrtho2D((GLdouble)left, (GLdouble)right, (GLdouble)bottom, (GLdouble)top);
}

void
GR_perspective (long fovy, float aspect, float near, float far)
{
	GR_viewtype = GR_PERSPECTIVE;
	gluPerspective ((GLdouble)fovy, (GLdouble)aspect, (GLdouble)near, (GLdouble)far);
}

IMAGE*
iopen(const char* filname, const char* mode)
{
//	sprintf(stderr, "Image handling not implemented: iopen");
}

void
iclose(IMAGE* image)
{
//
}

void
rgbatocpack (unsigned short* r, unsigned short* g, unsigned short* b,
             unsigned short* a, unsigned long*  l, int n)
{
   while (n>=8)
   {
      l[0] = r[0] | (g[0]<<8) | (b[0]<<16) | (a[0]<<24);
      l[1] = r[1] | (g[1]<<8) | (b[1]<<16) | (a[1]<<24);
      l[2] = r[2] | (g[2]<<8) | (b[2]<<16) | (a[2]<<24);
      l[3] = r[3] | (g[3]<<8) | (b[3]<<16) | (a[3]<<24);
      l[4] = r[4] | (g[4]<<8) | (b[4]<<16) | (a[4]<<24);
      l[5] = r[5] | (g[5]<<8) | (b[5]<<16) | (a[5]<<24);
      l[6] = r[6] | (g[6]<<8) | (b[6]<<16) | (a[6]<<24);
      l[7] = r[7] | (g[7]<<8) | (b[7]<<16) | (a[7]<<24);
      l += 8;
      r += 8;
      g += 8;
      b += 8;
      a += 8;
      n -= 8;
   }
   while (n--)
     *l++ = *r++ | ((*g++)<<8) | ((*b++)<<16) | ((*a++)<<24);
}

void
rgbtocpack (unsigned short* r, unsigned short* g, unsigned short* b,
            unsigned long* l, int n)
{
   while (n>=8)
   {
      l[0] = r[0] | (g[0]<<8) | (b[0]<<16);
      l[1] = r[1] | (g[1]<<8) | (b[1]<<16);
      l[2] = r[2] | (g[2]<<8) | (b[2]<<16);
      l[3] = r[3] | (g[3]<<8) | (b[3]<<16);
      l[4] = r[4] | (g[4]<<8) | (b[4]<<16);
      l[5] = r[5] | (g[5]<<8) | (b[5]<<16);
      l[6] = r[6] | (g[6]<<8) | (b[6]<<16);
      l[7] = r[7] | (g[7]<<8) | (b[7]<<16);
      l += 8;
      r += 8;
      g += 8;
      b += 8;
      n -= 8;
   }
   while (n--)
     *l++ = *r++ | ((*g++)<<8) | ((*b++)<<16);
}

void
getrow(IMAGE* image, unsigned short* buffer, int row, int column)
{
//	sprintf(stderr, "Image handling not implemented: getrow");
}

void
GR_screen_to_world (short xscreen, short yscreen, float &x, float &y)
{
	long		xsize, ysize;
	long		xorg, yorg;

	switch (GR_viewtype)
	{
		case	GR_PERSPECTIVE:
			x = (float) xscreen;
			y = (float) yscreen;
			break;
		
		case	GR_ORTHO:
			GR_getsize (&xsize, &ysize);
			GR_getorigin (&xorg, &yorg);
			xscreen -= (short) xorg;
			yscreen -= (short) yorg;


			x = GR_worldxmin + ((float)xscreen / (float)xsize) * (GR_worldxmax - GR_worldxmin);

			y = GR_worldymin + ((float)yscreen / (float)ysize) * (GR_worldymax - GR_worldymin);

			break;
	}
}

void
GR_world_to_screen (float xworld, float yworld, short &xscreen, short &yscreen)
{
	long	xsize, ysize;
	long	xorg, yorg;

	GR_getsize (&xsize, &ysize);
	GR_getorigin (&xorg, &yorg);

	xworld -= GR_worldxmin;
	yworld -= GR_worldymin;

	xscreen = (short) (xorg + (xworld / (GR_worldxmax - GR_worldxmin)) * xsize);
	yscreen = (short) (yorg + (yworld / (GR_worldymax - GR_worldymin)) * ysize);

}

short
GR_x_pixelsize (float width)
{
	long	xsize, ysize;
	long xorg, yorg;
	short		xpixel;

	GR_getsize (xsize, ysize);
	GR_getorigin (xorg, yorg);

	xpixel = (short) (width / ((GR_worldxmax - GR_worldxmin)) * xsize);

	return xpixel;
}

short
GR_y_pixelsize (float height)
{
	long	xsize, ysize;
	long xorg, yorg;
	short		ypixel;

	GR_getsize (xsize, ysize);
	GR_getorigin (xorg, yorg);

	ypixel = (short) (height / ((GR_worldymax - GR_worldymin)) * ysize);

	return ypixel;
}

float
GR_x_worldsize (short pixelwidth)
{
	long	xsize, ysize;
	long xorg, yorg;
	float		xworld;

	GR_getsize (xsize, ysize);
	GR_getorigin (xorg, yorg);

	xworld = ((float) pixelwidth / (float) xsize) * (GR_worldxmax - GR_worldxmin);

	return xworld;

}

float
GR_y_worldsize (short pixelheight)
{
	long	xsize, ysize;
	long xorg, yorg;
	float		yworld;

	GR_getsize (xsize, ysize);
	GR_getorigin (xorg, yorg);

	yworld = ((float) pixelheight / (float) ysize) * (GR_worldymax - GR_worldymin);

	return yworld;

}

void
GR_register_object (void *object)
{
	if (GR_pickflag)
	{
		if (GR_nameidx == GR_MAX_OBJS)
		{
			printf ("WARNING: GR_pushobject - GR_MAX_OBJS reached (call ignored)\n");
			return;
		}
		GR_obj_table [GR_nameidx] = object;
		glLoadName (GR_nameidx);
		GR_nameidx++;
	}
}

void
GR_bgnpick ()
{
	GR_nameidx = 0;	/* start assigning names at 0 */
	GR_pickflag = 1; /* set pickmode to on */
	GR_name_buffer [0] = 0;

	glInitNames();
//	pick (GR_name_buffer, GR_MAX_OBJS);
}

long
GR_endpick ()
{
	long nitems, index, item;

// 	GR_numhits = endpick (GR_name_buffer);
	GR_pickflag = 0;

	/*
	printf ("Hits = %d {", GR_numhits);
	for (item = 0, index = 0; item< GR_numhits;item++)
	{
		nitems = GR_name_buffer [index] + index + 1;
		printf ("(");
		for (index++; index < nitems; index++)
		{
			if (index == nitems - 1)
				printf ("%0X", GR_obj_table [GR_name_buffer [index]]);
			else
				printf ("%0X ", GR_obj_table [GR_name_buffer [index]]);
		}
		printf (")");
	}
	printf ("}\n");
	*/

	return    GR_numhits;
}

/****************************************************************************
	GR_top_hit --

	Returns the object registered which was the last object drawn under the
	cursor. This routine is appropriate for 2D applications where the objects
	are drawn in order from back to front on the screen.
*******************************************************************************/
void*
GR_top_hit ()
{
	long hit, index, nitems;
	long	last_item = -1;

	for (index=0, hit = 0; hit<GR_numhits; index += nitems+1, hit++)
	{
		nitems = GR_name_buffer [index];
		if (nitems != 0)
			last_item = GR_name_buffer [index + nitems];
	}

	if (last_item != -1)
	{
		//printf ("returning %d\n", GR_obj_table [last_item]);
		return GR_obj_table [last_item];
	}
	else
		return 0;
}

/****************************************************************************
	GR_get_all_hits --

	Returns an array of pointers to void containing all the registered objects
	picked during the last pick session. The first element in the array is the
	number of objects stored. Use this routine for 3D applications to get all
	the objects under the cursor. By simply traversing through the array and
	checking the z-depth, or distance from the viewpoint, the object on top
	can be generally discovered.

	NOTE: 
	The array returned is statically allocated in this module and had better not
	be deleted!!!
*******************************************************************************/

#ifdef LATER
void**
GR_get_all_hits ()
{
	C_List	namelist;
	long		objcount = 0;
	void*		objptr;
	long hit, index, nitems, j;

	for (index=0, hit = 0; hit<GR_numhits; index += nitems+1, hit++)
	{
		nitems = GR_name_buffer [index];
		for (j=1; j<=nitems; j++)
		{
			if (namelist.add_unique_tail (GR_obj_table [GR_name_buffer [index + j]]))
				objcount++;
		}
	}

	GR_hit_table [0] = (void *) objcount;

	objptr = namelist.head ();
	index = 1;
	while (objptr)
	{
		GR_hit_table [index] = objptr;
		index++;
		objptr = namelist.next ();
	}

	return GR_hit_table;
}
#endif


GR_Matrix::GR_Matrix ()
{
	identity ();
}

void
GR_Matrix::identity ()
{
	register long i, j;

	for (i=0; i<4; i++)
		for (j=0; j<4; j++)
			if (i==j)
				p_matrix [i][j] = 1.0;
			else
				p_matrix [i][j] = 0.0;
}

void
GR_Matrix::zero ()
{
	register long i, j;

	for (i=0; i<4; i++)
		for (j=0; j<4; j++)
			p_matrix [i][j] = 0.0;
}

GR_Matrix
GR_Matrix::operator = (GR_Matrix &matrix)
{
	register long i, j;

	for (i=0; i<4; i++)
		for (j=0; j<4; j++)
			p_matrix [i][j] = matrix.p_matrix [i][j];

	return *this;
}

GR_Matrix
GR_Matrix::operator * (GR_Matrix &matrix)
{
	GR_Matrix result;
	register long i, j, k;

	result.zero ();

	for (i=0; i<4; i++)
		for (j=0; j<4; j++)
			for (k=0; k<4; k++)
				result.p_matrix [i][j] += p_matrix [i][k] * matrix.p_matrix [k][j]; 
	return result;
}

void
GR_Matrix::rotate_x (float angle)
{
	float cosvalue;
	float sinvalue;
	float	a0, a1, a2, a3;

	angle *= M_PI / 180.0;
	cosvalue = cos (angle);
	sinvalue = sin (angle);

	a0 = p_matrix [0][1];
	a1 = p_matrix [1][1];
	a2 = p_matrix [2][1];
	a3 = p_matrix [3][1];

	p_matrix [0][1] = a0 * cosvalue - p_matrix [0][2] * sinvalue;
	p_matrix [1][1] = a1 * cosvalue - p_matrix [1][2] * sinvalue;
	p_matrix [2][1] = a2 * cosvalue - p_matrix [2][2] * sinvalue;
	p_matrix [3][1] = a3 * cosvalue - p_matrix [3][2] * sinvalue;

	p_matrix [0][2] = a0 * sinvalue + p_matrix [0][2] * cosvalue;
	p_matrix [1][2] = a1 * sinvalue + p_matrix [1][2] * cosvalue;
	p_matrix [2][2] = a2 * sinvalue + p_matrix [2][2] * cosvalue;
	p_matrix [3][2] = a3 * sinvalue + p_matrix [3][2] * cosvalue;

}

void
GR_Matrix::rotate_y (float angle)
{
	float cosvalue;
	float sinvalue;
	float a0, a1, a2, a3;

	angle *= M_PI / 180.0;
	cosvalue = cos (angle);
	sinvalue = sin (angle);

	a0 = p_matrix [0][0];
	a1 = p_matrix [1][0];
	a2 = p_matrix [2][0];
	a3 = p_matrix [3][0];

	p_matrix [0][0] = a0 * cosvalue + p_matrix [0][2] * sinvalue;
	p_matrix [1][0] = a1 * cosvalue + p_matrix [1][2] * sinvalue;
	p_matrix [2][0] = a2 * cosvalue + p_matrix [2][2] * sinvalue;
	p_matrix [3][0] = a3 * cosvalue + p_matrix [3][2] * sinvalue;

	p_matrix [0][2] = -a0 * sinvalue + p_matrix [0][2] * cosvalue;
	p_matrix [1][2] = -a1 * sinvalue + p_matrix [1][2] * cosvalue;
	p_matrix [2][2] = -a2 * sinvalue + p_matrix [2][2] * cosvalue;
	p_matrix [3][2] = -a3 * sinvalue + p_matrix [3][2] * cosvalue;
}

void
GR_Matrix::rotate_z (float angle)
{
	float cosvalue;
	float sinvalue;
	float a0, a1, a2, a3;

	angle *= M_PI / 180.0;
	cosvalue = cos (angle);
	sinvalue = sin (angle);

	a0 = p_matrix [0][0];
	a1 = p_matrix [1][0];
	a2 = p_matrix [2][0];
	a3 = p_matrix [3][0];

	p_matrix [0][0] = a0 * cosvalue - p_matrix [0][1] * sinvalue;
	p_matrix [1][0] = a1 * cosvalue - p_matrix [1][1] * sinvalue;
	p_matrix [2][0] = a2 * cosvalue - p_matrix [2][1] * sinvalue;
	p_matrix [3][0] = a3 * cosvalue - p_matrix [3][1] * sinvalue;

	p_matrix [0][1] = a0 * sinvalue + p_matrix [0][1] * cosvalue;
	p_matrix [1][1] = a1 * sinvalue + p_matrix [1][1] * cosvalue;
	p_matrix [2][1] = a2 * sinvalue + p_matrix [2][1] * cosvalue;
	p_matrix [3][1] = a3 * sinvalue + p_matrix [3][1] * cosvalue;

}

void
GR_Matrix::translate (float x, float y, float z)
{
	register int i;

	for (i=0; i<4; i++)
	{
		p_matrix [i][0] = p_matrix [i][0] + p_matrix [i][3] * x;
		p_matrix [i][1] = p_matrix [i][1] + p_matrix [i][3] * y;
		p_matrix [i][2] = p_matrix [i][2] + p_matrix [i][3] * z;
	}
}

void
GR_Matrix::scale (float x, float y, float z)
{
	register int i;

	for (i=0; i<4; i++)
	{
		p_matrix [i][0] *= x;
		p_matrix [i][1] *= y;
		p_matrix [i][2] *= z;
	}
}

void
GR_Matrix::set_matrix (GR_matrix newmat)
{
	register int i, j;

	for (i=0; i<4; i++)
		for (j=0; j<4; j++)
			p_matrix [i][j] = newmat [i][j];
}

GR_Point
GR_Point::cross (GR_Point vec2)
{
	GR_Point result;

	result.x = y * vec2.z - z * vec2.y;
	result.y = -(x * vec2.z - z * vec2.x);
	result.z = x * vec2.y - y * vec2.x;

	return result;
}

float
GR_Point::operator * (GR_Point &vec2)
{
	float result;

	result = x * vec2.x + y * vec2.y + z * vec2.z;
	return result;
}

float
GR_Point::length ()
{
	return sqrt (length2());
}

GR_Point
GR_Point::normalize ()
{
	GR_Point	result;
	float			rootxyz;

	rootxyz = sqrt ( x*x + y*y + z*z);

	result.x = x / rootxyz;
	result.y = y / rootxyz;
	result.z = z / rootxyz;

	return result;
}

GR_Point
GR_Point::operator * (GR_Matrix& gmat)
{
	GR_Point	result;

	result.x = 
			x * gmat.element (0, 0) 
		+ y * gmat.element (1, 0) 
		+ z * gmat.element (2, 0)
		+ gmat.element (3, 0);

	result.y = 
			x * gmat.element (0, 1) 
		+ y * gmat.element (1, 1) 
		+ z * gmat.element (2, 1)
		+ gmat.element (3, 1);

	result.z = 
			x * gmat.element (0, 2) 
		+ y * gmat.element (1, 2) 
		+ z * gmat.element (2, 2)
		+ gmat.element (3, 2);

	return result;
}

GR_Point
GR_Point::operator + (GR_Point& point)
{
	GR_Point result;

	result.x = x + point.x;
	result.y = y + point.y;
	result.z = z + point.z;

	return result;
}

GR_Point
GR_Point::operator - (GR_Point& point)
{
	GR_Point result;

	result.x = x - point.x;
	result.y = y - point.y;
	result.z = z - point.z;

	return result;
}

GR_Point
GR_Point::operator - ()
{
	GR_Point result;

	result.x = -x;
	result.y = -y;
	result.z = -z;

	return result;
}

Boolean
GR_backgroundProc (XtPointer)
{
	WindowElem *ptr;
	GR_Window *window;
	long curtime;

#ifndef ONYX
	if (GR_useMessagerFlag)
// DRE		msg_read_message ();
#endif

	ptr = GR_windowlist;
	curtime = time (0);
	while (ptr)
	{
		window = ptr->window;
		if (curtime - window->lastDrawTime() > 0)
	        {
          		window->draw ();
                }

		ptr = ptr->next;
	}

	return FALSE;
}

void
GR_useMessager (long tf)
{
#ifndef ONYX
	GR_useMessagerFlag = tf;
#endif
}

void
GR_startup (int argc, char *argv[])
{

	GR_windowlist = 0;
	GR_toplevel = XtAppInitialize (&GR_appcontext, argv[0],
					NULL, 0, (Cardinal *)&argc, argv,
					GR_fallback_resources, NULL, 0);
	GR_init_done = 1;
}
