/************************************************************

  GR_Map2D draws a textured background flat map
  
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

GR_Map2D::GR_Map2D(char* mapfile, int type, int width, int height, float lat, float lon)
{   
   if (getgdesc(GD_TEXTURE) == 0) {
      fprintf (stderr,
	       "Sorry: Texture mapping not available on this machine.\n");
      set_visible_flag (0);
      return;
   }

   p_filename = mapfile;
   p_xsize    = width;
   p_ysize    = height;
   p_lonleft  = lon*RADDEG;
   p_latupper = lat*RADDEG;
   p_grid     = 0.0;
   p_type     = type;
//
//   Load image from file
//
   work_progress (WP_CREATE, "Reading image file");

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

   work_progress (WP_DONE, NULL);
   work_progress (WP_DESTROY, NULL); // remove dialog shell;
//
//   Build display list for later
//
   p_gr_objid = GR_genobj();
   GR_makeobj(p_gr_objid);

   GR_closeobj(); 
}

void GR_Map2D::set_grid(float degree)
{
   p_grid = degree;
}

void GR_Map2D::set_corners(float lonNW, float latNW, float lonSE, float latSE)
{
   p_lonleft  = lonNW;
   p_lonright = lonSE;
   p_latupper = latNW;
   p_latlower = latSE;
}

void GR_Map2D::objdraw()
{
int             i, j;
int             gridnumber;
float           theta, phi;
float           rad = 1.000;
float           v[3];
float           degree;
float           d2r = M_PI/180.0;
float           point[3];

   GR_pushattributes ();
   GR_pushmatrix();
//
//   Draw the map background
//
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
   //GR_callobj (p_gr_objid);
//
//   Draw the grid lines if wanted
//
   if (p_grid > 0.0) {
     degree = p_grid;
     gridnumber = (int)(180.0/degree);
     theta = degree * M_PI / 180;

     glColor3f(1.0, 1.0, 1.0);
     GR_pushmatrix ();
     glMatrixMode(GL_PROJECTION);
     glLoadIdentity();
     gluOrtho2D(-180.0*RADDEG, 180.0*RADDEG, -90.0*RADDEG, 90.0*RADDEG);
     glMatrixMode(GL_MODELVIEW);
     glLoadIdentity();

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
     GR_popmatrix ();
   }

   GR_popmatrix();
   GR_popattributes ();
}
