/*                                                                   */
/*       Draw a heightfield on the screen using OpenGL graphics      */
/*                                                                   */

#include <stdio.h>               /* fopen() sprintf() etc. */
#include <stdlib.h>              /* strtod() */
#include <math.h>                /* math stuff */
#include <string.h>              /* strcpy() */
#include <ctype.h>               /* tolower() */
#include "GL/gl.h"

#include "hcon.h"
#include "hf.h"

#define MAIN
#include "typy.h"

#define ESC      '/033'
#define FALSE    0
#define TRUE     1

#define Elmod(vv, xq, yq)  vv[(((y10+yq) % ysize) * xsize) + \
			   ((x10 +xq) % xsize)]  /* elmt of array */

struct Point3 {
 double x, y, z;                 /* three-dimensional point */
};

double        ex, ey, ez;        /* eye coordinate */
double        zcenter;
double        zfar, znear;
double        zsf;               /* z coordinate scale factor */
double        hs;                /* vertical (y) coordinate scale factor */
double        ycorr;             /* y offset correction */
double        zeps;              /* z epsilon */
int           yoff;              /* screen coords y-offset */
int           x10, y10;          /* hf size * 10 */
short         sxmid;             /* center coordinates of view window (eg 640x480) */
short         symid;
int           nskip;             /* number of hf elements to skip */
int           maxcol = 127;      /* maximum color index */
struct Point3 light;             /* position of light source */
double        bright;            /* intensity of light  */
int           maxcon;            /* maximize contrast? (boolean) */
int           cmin, cmax;        /* max and min color index value in this image */
int           erasewin = TRUE;   /* erase window between displays? */
int           updatewin = TRUE;
GLfloat       mycolors[256][3];
float         Xpos=1.0, Ypos=1.0, Zpos=0.5;

GLint         fogmode;
GLfloat       fogColor[4] = { 0.3, 0.4, 0.5, 1.0 };

VOID X_Clear();
VOID X_Point(INT x,INT y);       /* drawable point */
VOID X_Poly(INT N, POINT2 point[]);
VOID X_FillPoly(INT N,POINT2 point[],INT CO,INT CV);

		                 /* draw two triangles joined along hypotenuse */
void tri3d(double x0, double z0, double x1, double z1,
           double y0, double y1, double y2, double y3, int s);
void drawhf(PTYPE *d1, int maxx, int maxz, int s, int t);
int  drawfrac();


int GetMaxX()
{
float     windW, windH, windX, windY, winsize[4];

   glGetFloatv(GL_VIEWPORT, (GLfloat *)winsize);
   windX = winsize[0];
   windY = winsize[1];
   windW = winsize[2];
   windH = winsize[3];
   return (int)windW;
}

int GetMaxY()
{
float     windW, windH, windX, windY, winsize[4];

   glGetFloatv(GL_VIEWPORT, (GLfloat *)winsize);
   windX = winsize[0];
   windY = winsize[1];
   windW = winsize[2];
   windH = winsize[3];
   return (int)windH;
}

void X_Clear()
{
  glClear(GL_COLOR_BUFFER_BIT); 
}

void X_Point(int x, int y)
{
   glBegin(GL_POINTS);
     glVertex2i(x, y);
   glEnd();
}

void X_Poly(int n, POINT2 point[])
{
int   i;

   glBegin(GL_LINE_LOOP);
   for (i=0; i<n; i++) {
      glVertex2i(point[i].x, point[i].y);
   }
   glEnd();
}

void X_FillPoly(int n, POINT2 point[], int co, int cv)
{
int   i;

   glBegin(GL_TRIANGLES);
   glColor3f(mycolors[cv][0], mycolors[cv][1], mycolors[cv][2]);
   for (i=0; i<n; i++) {
      glVertex2i(point[i].x, point[i].y);
   }
   glEnd();

   if(co) {
      glBegin(GL_LINE_LOOP);
      glColor3f(mycolors[co][0], mycolors[co][1], mycolors[co][2]);
      for (i=0; i<n; i++) {
         glVertex2i(point[i].x, point[i].y);
      }
      glEnd();
   }
}
 
void drawfrac(int *terrainid, float *XMin, float *XMax, float *YMin, float *YMax)
{
PTYPE         *d1;               /* height field array */
unsigned int  xsize, ysize;      /* heightfield size */
int           xgsize, ygsize;    /* screen size */
char          c;
double        eyinc;
double        lmag;              /* magnitude of a vector */
int           shader;            /* 0 wireframe 1 height 2 slope 3 lightsource */
char          instr[120];        /* input string */
char          fname[120];
int           tile = 0;
int           draw = TRUE;
int           ncols, i;
int           listid;

   strcpy(fname, "output.tga");

   if (readn_hf(&d1, &xsize, &ysize, fname) != 0) exit(1);
   printf("%s is a (%d x %d) heightfield.\n",fname,xsize,ysize);

  /* nskip = (xsize / 50) + 1; */   nskip = 1; 
   xgsize = 600;
   ygsize = 400;

   ncols = maxcol+1;
   for(i=0; i<ncols; i++)
     {
        mycolors[i][0]   = 0.2;
        mycolors[i][1]   = (float)i/(float)ncols;
        mycolors[i][2]   = 0.2;
     }   
   mycolors[ncols][0] = 0.0;
   mycolors[ncols][1] = 1.0;
   mycolors[ncols][2] = 0.0;    /* used for XOR line mode */

   MinX = 0;
   MinY = 0;
   MaxX = (int)GetMaxX(); 
   MaxY = (int)GetMaxY();
   xgsize = MaxX;
   ygsize = MaxY;
   sxmid = xgsize / 2;           /* middle of screen in screen coords */
   symid = ygsize / 2;
   zsf = 0.3;                    /* z coordinate scale factor */
   hs = 0.8;                     /* vertical (y) coordinate scale factor */
   ycorr = 0.0;                  /* y offset correction */
   zeps = 0.001;
   ex = 0.0; ey = -2.8; ez = -3.8;   /* viewpoint 0,0,0 in +z direction */
   eyinc = 0.8;
   light.x = 3; light.y = 2; light.z = 0;   /* direction vector */
   lmag = sqrt(light.x*light.x + light.y*light.y + light.z*light.z);
   light.x /= lmag;
   light.y /= lmag;
   light.z /= lmag;

   bright = 0.8;                 /* lightsource intensity */
   zcenter = 1;
   shader = 1;

   x10 = 10 * xsize;
   y10 = 10 * ysize;
   draw = TRUE;
   maxcon = FALSE;
   cmin = 0; cmax = maxcol;

   MaxX = (int)GetMaxX();
   MaxY = (int)GetMaxY();
   xgsize = MaxX;
   ygsize = MaxY;
   sxmid = xgsize / 2;           /* middle of screen in screen coords */
   symid = ygsize / 2;
   yoff = (int) (symid*(ycorr + ey/zcenter)); 
   /*if (erasewin) X_Clear();*/

   listid = glGenLists(1);
   glNewList((GLuint)listid, GL_COMPILE);
     //glPushMatrix();
     //glPushAttrib(GL_ALL_ATTRIB_BITS);
     drawhf(d1,xsize,ysize,shader,tile);
     //glPopAttrib(); 
     //glPopMatrix();
   glEndList();

   *terrainid = listid;
   *XMin = MinX;
   *XMax = MaxX;
   *YMin = MinY;
   *YMax = MaxY;

   return;
}

/* =============================================================== */
/* project two triangles, given 3D coordinates, on a 2D viewscreen */
/* uses gobal vars:
 *   hs : vertical heightfield scaling factor
 *   ex,ey,ez : location of eye (viewpoint)
 *   symid : y coordinate midpoint
 *   ycorr : y correction factor
 *   yoff  : y offset in screen units 
 *   zcenter : center to 'rotate' appparent hf around
 *   maxcol  : maximum color value 
 *   bright  : lightsource intensity 
 * ===============================================================
 */

/*  shader: 0 = wireframe, 1 = height color, 2 = slope, 3 = lightsource */

void tri3d(double x0, double z0, double x1, double z1,
           double y0, double y1, double y2, double y3, int shader)
{
double zmod0, zmod1; 
double slope1;
double slope2; 
int    c1, c2;                   /* colors */
double nx,ny,nz,nmag;            /* normal vector */
double ctheta;                   /* cos(theta)  theta = angle between light and normal */

POINT2 p[3];                     /* three sets of points in 2D */

   c2 = c1 = 0;                  /* make compiler happy */ 
   
   if (shader == 1) {            /* height, before transform */
     c1 = maxcol*(y0 + y1 + y2 + y3)/4;
     c2 = c1;
   } 
   
   /* x0,y0,z0 should be in the range -1..1 */
   /* transform to eye coordinate (ex,ey,ez) */
   
   y0 *= hs; y1 *= hs; y2 *= hs; y3 *= hs;                          
   x0 -= ex; y0 -= ey; z0 -= ez;
   x1 -= ex; y1 -= ey; z1 -= ez;
   y2 -= ey; y3 -= ey; 
   if ((z1 <= zeps) || (z0 <= zeps)) return;  /* foreground clipping */

   if (shader == 2) {
     slope1 = atan2( (z0-z1)*(x1-x0),(z1-z0)*(y2-y0) );
     c1 = (int) ((maxcol>>1)*(0.5*slope1 + 1.8));
    
     slope2 = atan2( (z0-z1)*(x1-x0),(z1-z0)*(y1-y3) ); 
     c2 = (int) ((maxcol>>1)*(0.5*slope2 + 1.8));
     
   } else if (shader == 3) {     /* compute normal, dot product with lightsource */

     nx = (z0-z1)*(y2-y0);
     ny = (z1-z0)*(x1-x0);
     nz = (x1-x0)*(y2-y1);
     nmag = sqrt(nx*nx + ny*ny + nz*nz);
     ctheta = (nx*light.x + ny*light.y + nz*light.z) / nmag;
     c1 = (int) (maxcol>>1)*(bright*(ctheta + 1.0));
     
     nx = -(z0-z1)*(y3-y1);
     /* ny = (z1-z0)*(x1-x0); */ /* duplicates line above */
     nz = (x0-x1)*(y3-y0);
     nmag = sqrt(nx*nx + ny*ny + nz*nz);
     ctheta = (nx*light.x + ny*light.y + nz*light.z) / nmag;
     c2 = (int) (maxcol>>1)*(bright*(ctheta + 1.0));

   } else if (shader == 0) {     /* shader = 0: wireframe */
     c1 = maxcol;                /* white, we hope. brightest color, anyway */
   }

   if (maxcon) {                 /* if maximize_contrast is TRUE... */
     c1 = (int) (((double)maxcol/(cmax-cmin))*(c1-cmin));
     c2 = (int) (((double)maxcol/(cmax-cmin))*(c2-cmin));
   } 
   if (c1 < 0) c1 = 0;
   if (c1 > maxcol) c1 = maxcol;
   if (c2 < 0) c2 = 0;
   if (c2 > maxcol) c2 = maxcol;

   if (!maxcon) {
     if (c1 > cmax) cmax = c1;
     if (c1 < cmin) cmin = c1;
   }

   zmod0 = z0 * zsf;             /* z1 multiplied by z scale factor */
   zmod1 = z1 * zsf;

   /* sxmid, symid are midpoints of screen, eg sxmid=320 for 640x480 scn. */
   p[0].x = (int) (sxmid*x0/zmod0) + sxmid;  /* x,y,z 0 */
   p[0].y = (int) (symid*y0/zmod0) + symid + yoff;
 
   p[1].x = (int) (sxmid*x1/zmod1) + sxmid;  /* x,y,z 1 */
   p[1].y = (int) (symid*y1/zmod1) + symid + yoff;

   p[2].x = (int) (sxmid*x1/zmod0) + sxmid;  /* x,y,z 2 */
   p[2].y = (int) (symid*y2/zmod0) + symid + yoff;
   
   if( ((p[0].x==p[1].x) && (p[0].y==p[1].y)) ||
       ((p[0].x==p[2].x) && (p[0].y==p[2].y)) ||
       ((p[1].x==p[2].x) && (p[1].y==p[2].y))  )  {
         X_Point(p[0].x,p[0].y);
     } else {
       if (shader == 0) {
         X_FillPoly(3, p, maxcol, 0);
       } else if (shader == 1) {
           X_FillPoly(3, p, c1, maxcol/2+c1/2);
           } else 
             X_FillPoly(3, p, c1, c1);   
     }

   p[1].x = (int) (sxmid*x0/zmod0) + sxmid;  /* x,y,z 0 */
   p[1].y = (int) (symid*y0/zmod0) + symid + yoff;

   p[0].x = (int) (sxmid*x1/zmod1) + sxmid;  /* x,y,z 1 */
   p[0].y = (int) (symid*y1/zmod1) + symid + yoff;

   p[2].x = (int) (sxmid*x0/zmod1) + sxmid; /* x,y,z 3 */
   p[2].y = (int) (symid*y3/zmod1) + symid + yoff; 

   if( ((p[0].x==p[1].x) && (p[0].y==p[1].y)) ||
       ((p[0].x==p[2].x) && (p[0].y==p[2].y)) ||
       ((p[1].x==p[2].x) && (p[1].y==p[2].y))  )  {
         X_Point(p[0].x,p[0].y);
     } else {
       if (shader == 0)  {
         X_FillPoly(3, p, maxcol, 0);
       } else if (shader == 1) {
           X_FillPoly(3, p, c2, maxcol/2+c2/2);
           } else 
             X_FillPoly(3, p, c2, c2);
   }

} /* end tri3d() */

/* draw a heightfield, back to front, as a series of triangles */
/* uses global variable nskip as hf index increment. */

void drawhf(PTYPE *d1, int xsize, int ysize, int shader, int tile)
{
int    ix,iz,izi;
int    ymin, ymax, xmin, xmax;
double xd2, yd2;
double x0, y0, z0, x1, y1, z1;
double y2, y3;

   xd2 = xsize / 2.0;
   yd2 = ysize / 2.0;
   if (tile) {
     ymin = 0;
     ymax = (2*ysize) - 1;
     xmin = -xsize + 1;
     xmax = 2*xsize -1;
   } else {
     ymin = 0;
     ymax = ysize - 2;
     xmin = 0;
     xmax = xsize-1;
   }
   if (!maxcon) {
     cmax = 0; 
     cmin = maxcol;     /* initialize global lims to extrema */
   }

 fprintf(stderr, "Open output file\n");
 FILE *fp = fopen("fract.out", "w+");
    fprintf(fp, "%f %f %f\n", 0.0, 0.0, 0.0);
    fprintf(fp, "%f %f %f\n", 0.0, 0.0, 0.0);
    fprintf(fp, "%f %f %f %f\n", (float)xmin, (float)xmax, (float)ymin, (float)ymax);
    fprintf(fp, "%d %d %f %d\n", xmax, ymax, 0.0, 0);
   /* loop over the two array indices */        
   for (iz=ymin;iz<=ymax;iz += nskip) {
        izi = (ymax - iz) + ymin;
	for ( ix=xmin;ix<xmax;ix+= nskip) {
	  x0 = (ix-xd2)/xd2; z0 = (izi-yd2)/yd2; 
	  x1 = (ix+nskip-xd2)/xd2; z1 = (izi-nskip-yd2)/yd2; 
	  y0 = (double) Elmod(d1,ix,iz);
	  y1 = (double) Elmod(d1,ix+nskip,iz+nskip);
	  y2 = (double) Elmod(d1,ix+nskip,iz);
	  y3 = (double) Elmod(d1,ix,iz+nskip);
 fprintf(fp, "%5d%5d %10f\n", ix, iz, y0);
	  tri3d(x0,z0,x1,z1,y0,y1,y2,y3,shader); 
	} /* end for iz */
   } /* end for ix */

   /*Gflush();  */           /* flush events to X disp. */
 fclose(fp);
}

static void Redraw()
{
int    i;

   glEnable(GL_DEPTH_TEST);

   glEnable(GL_FOG);
   fogmode = GL_EXP;
   glFogi(GL_FOG_MODE, fogmode);
   glFogfv(GL_FOG_COLOR, fogColor);
   glFogf(GL_FOG_DENSITY, 0.35);
   glHint(GL_FOG_HINT, GL_DONT_CARE);
   glFogf(GL_FOG_START, 10.0);
   glFogf(GL_FOG_END, 100.0);
   glClearColor(0.3, 0.4, 0.5, 1.0);

   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   glOrtho(MinX, MaxX, MinY, MaxY, -1.0, 1.0);
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();

   glCallList(20);

   glDisable(GL_FOG);

   glFlush();
   //tkSwapBuffers();   
}

static void Reshape(int width, int height)
{
   glViewport(0, 0, (GLint)width, (GLint)height);
   /*
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   glOrtho(MinX, MaxX, MinY, MaxY, -1.0, 1.0);
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();
   */
   X_Clear();

   //drawfrac();
}

