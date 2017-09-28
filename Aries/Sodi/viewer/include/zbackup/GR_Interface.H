//		@(#)GR_Interface.H	1.10		11/13/9
#ifndef	GR_INTERFACE_H
#define GR_INTERFACE_H

#include <math.h>
#include <Xm/Xm.h>
#include <X11/Xlib.h>
#include <GL/gl.h>
#include <GL/glx.h>
#include <GL/glu.h>

#define INLINE		inline
#define	GR_XSCREEN_SIZE	(XMAXSCREEN + 1)
#define	GR_YSCREEN_SIZE	(YMAXSCREEN + 1)

#define BEZIER		1
#define CARDINAL	2
#define BSPLINE		3
#define	GR_SOLID	0
#define	GR_DASH		1
#define	GR_DOTTED	2

#ifndef GR_MODULE
extern long GR_pickflag;
extern Widget GR_toplevel;
extern XtAppContext GR_appcontext;
extern String *GR_fallback_resources;
#endif

#define MVIEWING	GL_MODELVIEW
#define MPROJECTION	GL_PROJECTION

#define GR_LEFTMOUSE	1
#define GR_MIDDLEMOUSE	2
#define GR_RIGHTMOUSE	3

#define GD_TEXTURE	GL_TEXTURE_2D
#define GD_BLEND	GL_BLEND
#define GD_ZMAX		GL_DEPTH_CLEAR_VALUE

#define TX_TEXTURE_0	GL_TEXTURE_2D
#define TX_MINFILTER	GL_TEXTURE_MIN_FILTER
#define TX_POINT	GL_NEAREST
#define TX_MAGFILTER	GL_TEXTURE_MAG_FILTER
#define TX_BILINEAR	GL_LINEAR
#define TX_WRAP		14.0
#define TX_CLAMP	GL_CLAMP
#define TX_REPEAT	GL_REPEAT
#define TX_WRAP_S	GL_TEXTURE_WRAP_S
#define TX_WRAP_T	GL_TEXTURE_WRAP_T
#define TX_TILE		19.0
#define TX_NULL		0.0

#define TV_ENV0		GL_TEXTURE_ENV
#define TV_MODULATE	GL_MODULATE
#define TV_BLEND	GL_BLEND
#define TV_DECAL	GL_DECAL
#define TV_COLOR	GL_REPLACE
#define TV_NULL		0.0

#define BF_ZERO		GL_ZERO
#define BF_ONE		GL_ONE
#define BF_DC		GL_DST_COLOR
#define BF_MDC		GL_ONE_MINUS_DST_COLOR
#define BF_SA		GL_SRC_ALPHA
#define BF_MSA		GL_ONE_MINUS_SRC_ALPHA
#define BF_DA		GL_DST_ALPHA
#define BF_MDA		GL_ONE_MINUS_DST_ALPHA
#define BF_SC		GL_SRC_COLOR
#define BF_MSC		GL_ONE_MINUS_SRC_COLOR

#define GR_QUADRIC_NULL		0
#define GR_QUADRIC_STYLE	1
#define GR_QUADRIC_NORMALS	2
#define GR_QUADRIC_ORIENT	3
#define GR_QUADRIC_TEXTURE	4

#define SEEK_SET        0

#define LcdDEFAULT      0
#define LcdRED		1
#define LcdGREEN	2
#define LcdBLUE		3
#define LcdYELLOW       4
#define LcdCYAN         5
#define LcdBLACK        6
#define LcdWHITE        7

typedef struct {
    unsigned short imagic;
    unsigned short type;
    unsigned short dim;
    unsigned short xsize;
    unsigned short ysize;
    unsigned short zsize;
    unsigned int   min;
    unsigned int   max;
    unsigned int   wastebytes;
    char           name[80];
    unsigned int   colormap;
    int 	   file;
    unsigned short flags;
    short	   dorev;
    short	   x;
    short	   y;
    short	   z;
    short	   cnt;
    unsigned char  *tmp;
    unsigned char  *tmpR;
    unsigned char  *tmpG;
    unsigned char  *tmpB;
    unsigned long  rleend;		// for RLE images
    unsigned int   *rowstart;		// for RLE images
    int		   *rowsize;		// for RLE images
} IMAGE;

typedef float GR_matrix[16];
typedef struct {
    float x;
    float y;
    float z;
} GR_point;

struct GR_MouseEvent
{
		short		x;	// x position of cursor
		short		y;	// y position of cursor
		short		lastx; // last x position for drag
		short		lasty; // last y position for drag
		short		downx; // x position where button pressed
		short		downy; // y position where button pressed
		unsigned button : 2; // Button which caused the event
		unsigned moving : 1; // Flag for mouse move detection
		unsigned down : 1;
		struct
		{
			unsigned left : 1; // leftmouse button state 1 = down
			unsigned middle : 1; // middlemouse button state 1 = down
			unsigned right : 1; // rightmouse button state 1 = down
		}buttonstate;
};

class	GR_Point
{
	public:
		float		x;
		float		y;
		float		z;
		GR_Point (float x = 0.0, float y = 0.0, float z = 0.0);

		GR_Point operator * (GR_matrix&);
		float operator * (GR_Point&);
		GR_Point operator + (GR_Point&);
		GR_Point operator - (GR_Point&);
		GR_Point operator - ();
		GR_Point cross (GR_Point);
		GR_Point normalize ();
		float		 length ();
		float		 length2 ();
		void setXYZ (float [3]);
};

INLINE float
GR_Point::length2 ()
{
	return x*x + y*y + z*z;
}

INLINE
GR_Point::GR_Point (float ix, float iy, float iz)
{
	x = ix;
	y = iy;
	z = iz;
}

INLINE void 
GR_Point::setXYZ (float array[3])
{
	x = array [0];
	y = array [1];
	z = array [2];
}

typedef double  GR_Vector3d [3];
typedef float   GR_Vector3f [3];
typedef long	GR_Vector3i [3];

typedef double  GR_Vector2d [2];
typedef float   GR_Vector2f [2];
typedef long	GR_Vector2i [2];

class GR_Window;

float GR_sinf(float value);
float GR_cosf(float value);
float GR_tanf(float value);

void rgbatocpack (unsigned char* r, unsigned char* g, unsigned char* b,
                  unsigned char* a, unsigned long*  l, int n);
void rgbtocpack (unsigned char* r, unsigned char* g, unsigned char* b,
                 unsigned long* l, int n);

void GR_errors();
void GR_startup (int argc, char *argv[]);
void GR_useMessager (long tf);
void GR_registerWindow (GR_Window*);
void GR_registerWindow (GR_Window&);
void GR_enable(GLenum flag);
void GR_disable(GLenum flag);
void GR_pushattributes();
void GR_popattributes();
long GR_genobj();
void GR_makeobj (long object_idx);
void GR_isobj(long	object_idx);
void GR_callobj (long	object_idx);
void GR_closeobj ();
void GR_editobj(long object_idx);
void GR_delobj (long	object_idx);
void GR_flipxyz (long n, float ** varray);
void GR_cursoff();
void GR_curson();
void GR_backface(long boolean	);
void GR_viewport (long left, long right, long bottom, long top);
long GR_getdisplaymode ();
void GR_loadidentity ();
void GR_pushmatrix ();
void GR_popmatrix ();
void GR_loadmatrix (GR_matrix matrix);
void GR_multmatrix (GR_matrix matrix);
void GR_getmatrix (GR_matrix matrix);
void GR_mmode(short tf);
void GR_ortho (float left, float right, float bottom, float top, float near, float far);
void GR_ortho2 (float left, float right, float bottom, float top);
void GR_perspective (long fovy, float aspect, float near, float far);
void GR_lookat (GR_point viewpoint, GR_point target, long twist);
void GR_scale (float x, float y, float z = 1.0);
void GR_translate (float x, float y, float z = 0.0);
void GR_rotate (long angle, char xyz);
void GR_rot (float angle, char xy);
void GR_mapcolor (unsigned short color_idx, short red, short grn, short blu);
void GR_color (short red, short grn, short blu);
long GR_get_color (short red, short grn, short blu);
void GR_RGBcolor (short red, short grn, short blu);
void GR_cpack(unsigned long color);
void GR_cmode ();
void GR_RGBmode ();
void GR_RGBmodeflag (long tf);
void GR_ginit();
void GR_clear();
void czclear(unsigned long cval, long zval);
void GR_swapbuffers();
void GR_mswapbuffers(long fbuf);
void GR_singlebuffer ();
void GR_doublebuffer ();
void GR_zbuffer (long tf);
void GR_gconfig ();
void GR_frontbuffer (long truefalse);
void GR_backbuffer (long truefalse);
void GR_popname ();
void GR_pushname (short name);
void GR_initnames ();
void GR_loadname (short name);
void GR_picksize(short deltax, short deltay);
void GR_pick (int *buffer, long numnames);
long GR_endpick (short *buffer);
void GR_bgnpick ();
long GR_endpick ();
long GR_qtest ();
void GR_qdevice (unsigned short device);
long GR_qread (short* val);
void GR_qreset ();
long GR_getvaluator ( unsigned short dev );
void GR_tie (unsigned short device, unsigned short v1, unsigned short v2);
void GR_charstr (char* string);
void GR_cmov (GR_Point vec );
void GR_cmov (float x, float y, float z);
void GR_cmov2 (float x, float y);
void GR_move2 (float x, float y);
void GR_move (float x, float y, float z);
void GR_draw2 (float x, float y);
void GR_draw (float x, float y, float z);
//long GR_defpattern (char* pattern);
void GR_defpattern (short n, short size, unsigned long pattern[]);
void GR_setpattern (short n);
long GR_getpattern ();
long GR_deffont (char* font);
void GR_defrasterfont (long n, long ht, long nc, void* chars, long nr, void* raster);
void GR_font (long n);
long GR_getfont ();
void GR_LcdMakeFont (int color, char *PathToGifs);
long GR_LcdGetWidth ();
long GR_LcdGetHeight ();
void GR_LcdPrint(float *position, String message, int nchars, long color, int orient);
void GR_prefposition (long x1, long x2, long y1, long y2);
void GR_winset (long id);
void GR_reshapeviewport ();
long GR_winopen (char* title);
void GR_getsize (long* x, long* y);
void GR_getorigin (long* x, long* y);
void GR_getsize (long& x, long& y);
void GR_getorigin (long& x, long& y);
long GR_getlwidth();
void GR_pmv (float x, float y, float z);
void GR_pdr (float x, float y, float z);
void GR_pnt (float x, float y, float z);
void GR_pclos ();
void GR_bgnline ();
void GR_endline ();
void GR_bgnclosedline ();
void GR_endclosedline ();
void GR_bgnpoint ();
void GR_endpoint ();
void GR_bgnpolygon ();
void GR_endpolygon ();
void GR_bgntmesh ();
void GR_endtmesh ();
void GR_swaptmesh();
void GR_bgnqstrip ();
void GR_endqstrip ();
void GR_v2i (GR_Vector2i vector);
void GR_v2f (GR_Vector2f vector);
void GR_v2d (GR_Vector2d vector);
void GR_v3i (GR_Vector3i vector);
void GR_v3f (GR_Vector3f vector);
void GR_v3d (GR_Vector3d vector);
void GR_n3f (GR_Vector3f vector);
void GR_line2 (float x1, float y1, float x2, float y2);
void GR_poly (long n, float varray[][3]);
void GR_polf (long n, float varray[][3]);
void GR_poly2 (long n, float** varray);
void GR_poly2 (long n, float varray[][2]);
void GR_polf2 (long n, float varray[][2]);
void GR_rect (float x1, float y1, float x2, float y2);
void GR_rectf (float x1, float y1, float x2, float y2);
void GR_circ (float x, float y, float radius);
void GR_circf (float x, float y, float radius);
void GR_arc(float x, float y, float radius, float sangle, float eangle);
void GR_arcf(float x, float y, float radius, float sangle, float eangle);
void GR_setslices(int slices);
void GR_setstacks(int slices);
void GR_sphere(float radius, int flags[10]);
void GR_torus(float iradius, float oradius);
void GR_cone(float base, float height, int flags[10]);
void GR_c3s(short cv[3]);
void GR_crv(float points[4][3]);
void GR_curvebasis(short basisid);
void GR_curveprecision(short nsegs);
void GR_defbasis(short basisid, GR_matrix mat);
void GR_linewidth (short width);
void GR_linewidthf(float width);
void GR_setlinestyle (short style);
void GR_deflinestyle (short style, short pattern);
void GR_lsrepeat (long factor);
long GR_getheight ();
void GR_screen_to_world (short xscreen, short yscreen, float &x, float &y);
void GR_world_to_screen (float xworld, float yworld, short &x, short &y);
short GR_x_pixelsize (float width);
short GR_y_pixelsize (float height);
float GR_x_worldsize (short pixelwidth);
float GR_y_worldsize (short pixelheight);
void GR_register_object (void* object);
void** GR_get_all_hits ();
void* GR_top_hit ();
long GR_pickmode ();
void GR_shademodel(long model);
void GR_exit ();

int  getgdesc(int inquiry);
void tevbind(long target, long index);
void tevdef(long index, long np, float props[]);
void t2f(float vector[]);
void texbind(long target, long index);
void GR_texbind(long target, long index);
void texdef2d(long index, long nc, long width, long height, unsigned long *image, long np, float *props);

void GR_blendfunction(long sfactor, long dfactor);

IMAGE* iopen(const char* filname, const char* mode);
void iclose(IMAGE* image);
void getrow(IMAGE* image, unsigned char* buffer, int row, int column);
void getrowbw(IMAGE* image, unsigned short* buffer, int row, int column);

// INLINE FUNCTIONS

INLINE float
GR_sinf(float value)
{
	return sin(value);
}

INLINE float
GR_cosf(float value)
{
	return cos(value);
}

INLINE float
GR_tanf(float value)
{
	return tan(value);
}

INLINE void
GR_registerWindow (GR_Window &window)
{
	GR_registerWindow (&window);
}

INLINE void
GR_enable(GLenum flag)
{
	glEnable(flag);
}

INLINE void
GR_disable(GLenum flag)
{
	glDisable(flag);
}

INLINE void
GR_pushattributes()
{
	glPushAttrib(GL_ALL_ATTRIB_BITS);
}

INLINE void
GR_popattributes()
{
	glPopAttrib();
}

INLINE void 
GR_cursoff()
{
//	cursoff();
}

INLINE void 
GR_curson()
{
//	curson();
}

INLINE void	
GR_backface(long truefalse)
{
	if (truefalse == TRUE)
	    glEnable(GL_CULL_FACE);
	  else
	    glDisable(GL_CULL_FACE);
}

INLINE long
GR_genobj ()
{
        return (long)glGenLists(1);
}

INLINE void
GR_makeobj (long object_idx)
{
        glNewList((GLuint)object_idx, GL_COMPILE);
}

INLINE void	
GR_callobj (long object_idx)
{
	glCallList((GLuint)object_idx);
}

INLINE void	
GR_isobj(long object_idx)
{
	glIsList((GLuint)object_idx);
}

INLINE void	
GR_closeobj ()
{
	glEndList();
}

INLINE void	
GR_editobj(long object_idx)
{
//	editobj(object_idx);
}

INLINE void	
GR_delobj (long	object_idx)
{
	glDeleteLists((GLuint)object_idx, 1);
}

INLINE void	
GR_viewport (long left, long right, long bottom, long top)
{
	glViewport((GLint)left, (GLint)bottom,
	           (GLint)right,(GLint)top);
}

INLINE long		
GR_getdisplaymode ()
{
//	return	getdisplaymode ();
	return 1;
}

INLINE void
GR_loadidentity ()
{
	glLoadIdentity();
}

INLINE void	
GR_pushmatrix ()
{
	glPushMatrix ();
}

INLINE void	
GR_popmatrix	()
{
	glPopMatrix ();
}

INLINE void
GR_loadmatrix (GR_matrix matrix)
{
	glLoadMatrixf((GLfloat *)matrix);
}

INLINE void
GR_multmatrix (GR_matrix matrix)
{
	glMultMatrixf ((GLfloat *)matrix);
}

INLINE void
GR_getmatrix (GR_matrix matrix)
{
	GR_matrix gmat;

	glGetFloatv(GL_MODELVIEW_MATRIX, (GLfloat *)matrix);
//        matrix = gmat;
//	matrix.set_matrix (gmat);
}

INLINE void	
GR_lookat (GR_point viewpoint, GR_point	target, long twist)
{
	gluLookAt (
	(GLdouble)viewpoint.x, (GLdouble)viewpoint.y,  (GLdouble)viewpoint.z, 
	(GLdouble)target.x,    (GLdouble)target.y,     (GLdouble)target.z,
	(GLdouble)sin(twist*M_PI/180.0),
        (GLdouble)cos(twist*M_PI/180.0),
        (GLdouble)0.0);
}

INLINE void	
GR_scale (float x, float y, float z)
{
	glScalef((GLfloat)x, (GLfloat)y, (GLfloat)z);
}

INLINE void	
GR_translate (float x, float y, float z)
{
	glTranslatef((GLfloat)x, (GLfloat)y, (GLfloat)z);
}

INLINE long GR_getvaluator ( unsigned short dev )
{
//	return getvaluator (dev);
	return 0;
}

INLINE void	
GR_rotate (long angle, char xyz)
{
	GLfloat xaxis, yaxis, zaxis;

	xaxis = 0.0; yaxis=0.0; zaxis=0.0;
	if (xyz == 'x') xaxis = 1.0;
	if (xyz == 'y') yaxis = 1.0;
	if (xyz == 'z') zaxis = 1.0;
	glRotatef(0.1*(GLfloat)angle, xaxis, yaxis, zaxis);
}

INLINE void
GR_rot (float angle, char xy)
{
	GLfloat xaxis, yaxis, zaxis;

	xaxis = 0.0; yaxis = 0.0, zaxis=0.0;
	if (xy == 'x') xaxis = 1.0;
	if (xy == 'y') yaxis = 1.0;
	if (xy == 'z') zaxis = 1.0;
	glRotatef((GLfloat)angle, xaxis, yaxis, zaxis);
}

INLINE void
GR_poly2 (long n, float** varray)
{
//	poly2 (n, (Coord[][2]) varray);
}

INLINE void	
GR_RGBcolor (short red, short green, short blue)
{
        glColor3f((GLfloat)(red/255.0), (GLfloat)(green/255.0), 
                  (GLfloat)(blue/255.0));
}

INLINE void
GR_color (short red, short green, short blue)
{
	glColor3f((GLfloat)(red/255.0), (GLfloat)(green/255.0),
                  (GLfloat)(blue/255.0));
}

INLINE void
GR_cpack(unsigned long color)
{
	GLshort red, green, blue, alpha;

	red = color & 0xff;
	green = (color >> 8) & 0xff;
	blue  = (color >> 16) & 0xff;
	alpha = (color >> 24) & 0xff;
	glColor4s(red, green, blue, alpha);
}

INLINE void
GR_mapcolor(unsigned short index, short red, short green, short blue)
{
//	glutSetColor((int)index, (GLfloat)red/255.0, (GLfloat)green/255.0,
//	             (GLfloat)blue/255.0);
}

INLINE void	
GR_cmode ()
{
#ifdef	IRIS4D
//	cmode ();
#endif
}

INLINE void	
GR_RGBmode ()
{
//	RGBmode ();
}

INLINE void	
GR_ginit()
{
//	ginit ();
}

INLINE void	
GR_clear ()
{
	glIndexi(0);
	glClearIndex(0);
	glClear(GL_COLOR_BUFFER_BIT);
}

INLINE void	
GR_czclear (unsigned long cval, long zval)
{
	glIndexi(0);
	glClearIndex(0);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}

INLINE void	
GR_swapbuffers()
{
//	swapbuffers ();
}

INLINE void
GR_mswapbuffers(long fbuf)
{
//
}

INLINE void	
GR_singlebuffer ()
{
//	singlebuffer ();
}

INLINE void	
GR_doublebuffer ()
{
//	doublebuffer ();
}

INLINE void
GR_mmode(short tf)
{
	glMatrixMode((GLenum)tf);
}

INLINE void
GR_zbuffer (long tf)
{
	if (tf == TRUE)
	    glEnable(GL_DEPTH_TEST);
	  else
	    glDisable(GL_DEPTH_TEST);
}

INLINE void	
GR_gconfig ()
{
//	gconfig ();
}

INLINE void	
GR_frontbuffer (long truefalse)
{
	glDrawBuffer(GL_FRONT);
}

INLINE void	
GR_backbuffer (long truefalse)
{
	glDrawBuffer(GL_BACK);
}

INLINE void	
GR_loadname (short name)
{
	glLoadName((GLuint)name);
}

INLINE void	
GR_popname ()
{
	glPopName();
}

INLINE void	
GR_pushname (short name)
{
	glPushName((GLuint)name);
}

INLINE void	
GR_initnames ()
{
	glInitNames();
}

INLINE long	
GR_qtest ()
{
//	return	qtest ();
	return 0;
}

INLINE void	
GR_qdevice (unsigned short device)
{
//	qdevice (device);
}

INLINE long	
GR_qread (short* val)
{
//	return 	qread (val);
	return 0;
}

INLINE void	
GR_qreset ()
{
//	qreset ();
}

INLINE void	
GR_tie (unsigned short device, unsigned short v1, unsigned short v2)
{
//	tie (device, v1, v2);
}

INLINE void	
GR_rect (float x1, float y1, float x2, float y2)
{
	glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
	glRectf((GLfloat)x1, (GLfloat)y1, (GLfloat)x2, (GLfloat)y2);
}

INLINE void	
GR_rectf (float x1, float y1, float x2, float y2)
{
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	glRectf((GLfloat)x1, (GLfloat)y1, (GLfloat)x2, (GLfloat)y2);
}

INLINE void
GR_charstr (char* string)
{
	int savebase;

	glGetIntegerv(GL_LIST_BASE, &savebase);
	glListBase(2000);
	glCallLists(strlen(string), GL_UNSIGNED_BYTE, string);
	glListBase(savebase);
}

INLINE void
GR_cmov (GR_Point vec )
{
	glRasterPos3f((GLfloat)vec.x, (GLfloat)vec.y, (GLfloat)vec.z);
}

INLINE void
GR_cmov (float x, float y, float z)
{
        glRasterPos3f((GLfloat)x, (GLfloat)y, (GLfloat)z);
}

INLINE void
GR_cmov2 (float x, float y)
{
	glRasterPos2f((GLfloat)x, (GLfloat)y);
}

INLINE void
GR_move2 (float x, float y)
{
	glBegin(GL_LINES);
	glVertex2f(x, y);
}

INLINE void
GR_move (float x, float y, float z)
{
	glBegin(GL_LINES);
	glVertex3f(x, y, z);
}

INLINE void
GR_draw2 (float x, float y)
{
	glVertex2f(x, y);
	glEnd();
}

INLINE void
GR_draw (float x, float y, float z)
{
	glVertex3f(x, y, z);
	glEnd();
}

INLINE void
GR_defrasterfont (long n, long ht, long nc, void* chars, long nr, void* raster)
{
//	defrasterfont ((short) n, (short) ht, (short) nc, 
//			(Fontchar*) chars, (short) nr, (unsigned short[]) raster);
}

INLINE void
GR_font (long n)
{
//	font ((short) n);
}

INLINE long
GR_getfont ()
{
//	return (long) getfont ();
	return 0;
}

INLINE void
GR_prefposition (long x1, long x2, long y1, long y2)
{
//	prefposition (x1, x2, y1, y2);
}

INLINE void
GR_winset (long id)
{
//	winset (id);
}

INLINE void
GR_reshapeviewport ()
{
//	reshapeviewport ();
}

INLINE long
GR_winopen (char* title)
{
	long		id;
//	id = winopen (title);
	id = 0;
	return	id;
}

INLINE void
GR_getsize (long& x, long& y)
{
GLint viewport[4];

	glGetIntegerv (GL_VIEWPORT, viewport);
	x = (long)viewport[2];
        y = (long)viewport[3];
}

INLINE void
GR_getsize (long* x, long* y)
{
//	getsize (x, y);
}

INLINE void
GR_getorigin (long &x, long &y)
{
//	getorigin (&x, &y);
}

INLINE void
GR_getorigin (long* x, long* y)
{
//	getorigin (x, y);
}

INLINE long
GR_getlwidth()
{
	int value;

	glGetIntegerv(GL_LINE_WIDTH, &value);
	return (long)value;
}

INLINE void
GR_pmv (float x, float y, float z)
{
	glBegin(GL_LINE_LOOP);
	glVertex3f(x, y, z);
}

INLINE void
GR_pdr (float x, float y, float z)
{
	glVertex3f(x, y, z);
}

INLINE void
GR_pclos ()
{
	glEnd();
}

INLINE void
GR_pnt (float x, float y, float z)
{
	glBegin(GL_POINTS);
	glVertex3f(x, y, z);
	glEnd();
}

INLINE void
GR_bgnline ()
{
	glBegin(GL_LINE_STRIP);
}

INLINE void
GR_endline ()
{
	glEnd();
}

INLINE void
GR_bgnclosedline ()
{
	glBegin(GL_LINE_LOOP);
}

INLINE void
GR_endclosedline ()
{
	glEnd();
}

INLINE void
GR_bgnpoint ()
{
	glBegin(GL_POINTS);
}

INLINE void
GR_endpoint ()
{
	glEnd();
}

INLINE void
GR_bgnpolygon ()
{
	glBegin(GL_POLYGON);
}

INLINE void
GR_endpolygon ()
{
	glEnd();
}

INLINE void
GR_bgntmesh ()
{
	glBegin(GL_TRIANGLE_STRIP);
}

INLINE void
GR_endtmesh ()
{
	glEnd();
}

INLINE void
GR_bgnqstrip ()
{
	glBegin(GL_QUAD_STRIP);
}

INLINE void
GR_endqstrip ()
{
	glEnd();
}

INLINE void
GR_v2i (GR_Vector2i vector)
{
	glVertex2i(vector[0], vector[1]);
}

INLINE void
GR_v2f (GR_Vector2f vector)
{
	glVertex2f(vector[0], vector[1]);
}

INLINE void
GR_v2d (GR_Vector2d vector)
{
	glVertex2d(vector[0], vector[1]);
}

INLINE void
GR_v3i (GR_Vector3i vector)
{
	glVertex3i(vector[0], vector[1], vector[2]);
}

INLINE void
GR_v3f (GR_Vector3f vector)
{
	glVertex3f(vector[0], vector[1], vector[2]);
}

INLINE void
GR_v3d (GR_Vector3d vector)
{
	glVertex3d(vector[0], vector[1], vector[2]);
}

INLINE void
GR_n3f (GR_Vector3f vector)
{
	glNormal3f(vector[0], vector[1], vector[2]);
}

INLINE void
GR_linewidth (short width)
{
	glLineWidth((GLfloat)width);
}

INLINE void
GR_linewidthf(float width)
{
	glLineWidth(width);
}

INLINE void
GR_line2 (float x1, float y1, float x2, float y2)
{
	glBegin(GL_LINES);
	glVertex2f(x1,y1);
	glVertex2f(x2,y2);
	glEnd();
}

INLINE long
GR_getheight ()
{
//	return getheight();
	return 0;
}

INLINE long
GR_strwidth (char *string)
{
//	return strwidth (string);
	return 0;
}

INLINE long
GR_pickmode ()
{
	return GR_pickflag;
}

INLINE void
GR_shademodel (long model)
{
	glShadeModel((GLenum)model);
}

//----------------------------------------------------------------\\

INLINE int
getgdesc(int inquiry)
{
GLint param;

//	glGetIntegerv((GLenum)inquiry, &param);
	return 1; //param;
}

INLINE void
GR_c3s(short cv[])
{
//
}

INLINE void
GR_swaptmesh()
{
//
}

INLINE void
tevbind(long target, long index)
{
//
}

INLINE void
tevdef(long index, long np, float props[])
{
//
}

INLINE void
t2f(float vector[2])
{
	glTexCoord2f(vector[0], vector[1]);
}

INLINE void
texbind(long target, long index)
{
//
}

INLINE void
GR_texbind(long target, long index)
{
#ifdef IRIX64
	glBindTextureEXT((GLenum)target, (GLuint)index);
#else
	glBindTexture((GLenum)target, (GLuint)index);
#endif
}

INLINE void
texdef2d(long index, long nc, long width, long height, unsigned long *image, long np, float *props)
{
//
}

INLINE void
GR_blendfunction(long sfactor, long dfactor)
{
	glEnable(GL_BLEND);
	glBlendFunc((GLenum)sfactor, (GLenum)dfactor);
}

#endif
