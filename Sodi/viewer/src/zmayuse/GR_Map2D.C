/************************************************************
  GR_Map2D draws a textured flat map
  

************************************************************/

#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#include "GR_Shell.H"
#include "GR_Lines.H"			// Was GR_2Dlines.C
#include "GR_Map2D.H"
#include "GR_work.H"
#include "gltk.h"

#define RE      6378.145
#define DEGRAD  57.29577951308232
#define RADDEG  0.0174532925199432958 

TK_RGBImageRec  *mapimage;
IMAGE           *mapsized;
static int mapglid = 1647; 

GR_Map2D::GR_Map2D(char* mapfile, int type, int width, int height, float lat, float lon)
{
int             i, j;
int             gridnumber;
float           theta, phi;
float           rad = 1.000;
float           v[3];
float           degree = 10.0;
float           d2r = M_PI/180.0;
    
   if (getgdesc(GD_TEXTURE) == 0) {
      fprintf (stderr,
	       "Sorry: Texture mapping not available on this machine.\n");
      set_visible_flag (0);
      return;
   }

   p_filename = mapfile;

   p_xsize    = width;
   p_ysize    = height;
   p_lonleft  = lon*d2r;
   p_latupper = lat*d2r;
   p_type     = type;
   /*
   //work_progress (WP_CREATE, "Reading image file", (Widget)vfwindow);
   //fileName = "./RSD_Data/jpl_earth.rgb";
   mapimage = tkRGBImageLoad(mapfile);
   mapsized = (IMAGE *)malloc(sizeof(IMAGE));
   mapsized->xsize = p_xsize;
   mapsized->ysize = p_ysize;
   mapsized->tmp   = (unsigned char *)malloc(mapsized->xsize*mapsized->ysize*4);
   gluScaleImage(GL_RGB,
          mapimage->sizeX, mapimage->sizeY, GL_UNSIGNED_BYTE, mapimage->data,
          mapsized->xsize, mapsized->ysize, GL_UNSIGNED_BYTE, mapsized->tmp);
   free(mapimage->data);
   free(mapimage);
   //work_progress (WP_DONE, NULL, (Widget)vfwindow);
   //work_progress (WP_DESTROY, NULL, (Widget)vfwindow ); // remove dialog shell;
   */
   p_gr_objid = GR_genobj ();
   GR_makeobj (p_gr_objid);

   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   gluOrtho2D(-180.0*RADDEG, 180.0*RADDEG, -90.0*RADDEG, 90.0*RADDEG);
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();

   GR_color(255, 0, 0);
   glBegin(GL_LINES);
     glVertex3f(0.0, 0.0, 0.0);
     glVertex3f(2.0, 0.0, 0.0);
   glEnd();

   GR_color(0, 255, 0);
   glBegin(GL_LINES);
     glVertex3f(0.0, 0.0, 0.0);
     glVertex3f(0.0, 2.0, 0.0);
   glEnd();

   GR_color(0, 0, 255);
   glBegin(GL_LINES);
     glVertex3f(0.0, 0.0, 0.0);
     glVertex3f(0.0, 0.0, 2.0);
   glEnd();

   gridnumber = (int)(180.0/degree);
   theta = degree * M_PI / 180;

   glColor3f(1.0, 1.0, 1.0);
   //GR_pushmatrix ();
   for (i=0; i<(gridnumber/2); i++) {
     GR_bgnline();
     for (j=0, phi=0.0; j<=36; j++, phi+=M_PI/18) {
        v[0]=cos(theta*i)*cos(phi)*rad;
        v[1]=sin(theta*i)*rad;
        v[2]=cos(theta*i)*sin(phi)*rad;
        GR_v3f(v);
     }
     GR_endline();
     
     GR_bgnline();
     for (j=0, phi=0.0; j<=36; j++, phi+=M_PI/18) {
        v[0]=cos(theta*i)*cos(phi)*rad;
        v[1]=-sin(theta*i)*rad;
        v[2]=cos(theta*i)*sin(phi)*rad;
        GR_v3f(v);
     }
     GR_endline();      
   }
   //GR_popmatrix ();
 
   glEndList();
}

void GR_Map2D::objdraw ()
{
float           spoint[2], epoint[2];
int             xsize, ysize;
unsigned long   *lptr;
unsigned short  *base, *ibuf, *abuf;
int             i, j, y, x;
float           point[3];
float           londel, latdel;
float           d2r = M_PI/180.0;
int             gridnumber;
float           theta, phi;
float           rad = 1.000;
float           v[3];
float           degree = 10.0;
extern GR_Window	*mapwindow;
extern GR_DispList	*map_displist;

   GR_pushattributes ();
   GR_pushmatrix();

   glClearColor(1.0, 1.0, 1.0, 0.0);  
   glViewport(0, 0, p_xsize, p_ysize);
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
   /* 
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   gluOrtho2D(0.0, p_xsize, 0.0, p_ysize);
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();
  
   point[0] = (p_xsize / 2) - (mapsized->xsize / 2);
   point[1] = (p_ysize / 2) - (mapsized->ysize / 2);
   point[2] = 0;
   glRasterPos3fv(point);
 
   glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
   glPixelZoom(1.0, 1.0);
   glDrawPixels(mapsized->xsize, mapsized->ysize, GL_RGB, GL_UNSIGNED_BYTE,
                mapsized->tmp);
   */
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   gluOrtho2D(-180.0*RADDEG, 180.0*RADDEG, -90.0*RADDEG, 90.0*RADDEG);
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();

   GR_color(255, 0, 0);
   glBegin(GL_LINES);
     glVertex3f(0.0, 0.0, 0.0);
     glVertex3f(2.0, 0.0, 0.0);
   glEnd();

   GR_color(0, 255, 0);
   glBegin(GL_LINES);
     glVertex3f(0.0, 0.0, 0.0);
     glVertex3f(0.0, 2.0, 0.0);
   glEnd();

   GR_color(0, 0, 255);
   glBegin(GL_LINES);
     glVertex3f(0.0, 0.0, 0.0);
     glVertex3f(0.0, 0.0, 2.0);
   glEnd();

   gridnumber = (int)(180.0/degree);
   theta = degree * M_PI / 180;

   glColor3f(1.0, 1.0, 1.0);
   //GR_pushmatrix ();
   for (i=0; i<(gridnumber/2); i++) {
     GR_bgnline();
     for (j=0, phi=0.0; j<=36; j++, phi+=M_PI/18) {
        v[0]=cos(theta*i)*cos(phi)*rad;
        v[1]=sin(theta*i)*rad;
        v[2]=cos(theta*i)*sin(phi)*rad;
        GR_v3f(v);
     }
     GR_endline();
     
     GR_bgnline();
     for (j=0, phi=0.0; j<=36; j++, phi+=M_PI/18) {
        v[0]=cos(theta*i)*cos(phi)*rad;
        v[1]=-sin(theta*i)*rad;
        v[2]=cos(theta*i)*sin(phi)*rad;
        GR_v3f(v);
     }
     GR_endline();      
   }
   //GR_popmatrix ();
 
   GR_callobj (p_gr_objid);

   GR_popmatrix();
   GR_popattributes ();
}
