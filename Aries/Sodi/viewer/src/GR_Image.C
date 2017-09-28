/************************************************************

  GR_Map2D draws a textured background flat map
  
************************************************************/

#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#include "GR_Shell.H"
#include "GR_Lines.H"			// Was GR_2Dlines.C
#include "GR_Image.H"
#include "GR_work.H"
#include "gltk.h"

#define RE      6378.145
#define DEGRAD  57.29577951308232
#define RADDEG  0.0174532925199432958 

GR_Image::GR_Image(char* mapfile, int type, int width, int height, float lat, float lon)
{   
   if (getgdesc(GD_TEXTURE) == 0) {
      fprintf (stderr,
	       "Sorry: Texture mapping not available on this machine.\n");
      set_visible_flag (0);
      return;
   }

   p_filename = mapfile;                          // Image file
   p_xsize    = width;                            // Image desired width in pixels
   p_ysize    = height;                           // Image desired height in pixels
   p_lon      = lon*RADDEG;                       // Image X position in world coord
   p_lat      = lat*RADDEG;                       // Image Y position in world coord
   p_type     = type;
//
//   Load image from file
//
   //work_progress (WP_CREATE, "Reading image file");

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

   //work_progress (WP_DONE, NULL);
   //work_progress (WP_DESTROY, NULL); // remove dialog shell;
//
//   Build display list for later
//
   p_gr_objid = GR_genobj();
   GR_makeobj(p_gr_objid);

   GR_closeobj(); 
}

void GR_Image::set_lla(float lat, float lon, float alt)
{
   p_lon = lon*RADDEG;
   p_lat = lat*RADDEG;
   p_alt = alt;
}

void GR_Image::objdraw()
{
int             i, j;
float           d2r = M_PI/180.0;
float           point[3];
 extern GR_Window *mapwindow;

   GR_pushattributes ();
   GR_pushmatrix();
//
//   Draw the map background
//
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   //gluOrtho2D(124.5*RADDEG, 129.67*RADDEG, 36.33*RADDEG, 39.67*RADDEG);
   gluOrtho2D(mapwindow->left(), mapwindow->right(),
	   mapwindow->bottom(), mapwindow->top() );
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();

   point[0] = p_lon;
   point[1] = p_lat;
   point[2] = 0;
   glRasterPos3fv(point);
 
   glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
   glPixelZoom(1.0, 1.0);
   glDrawPixels(mapsized->xsize, mapsized->ysize, GL_RGB, GL_UNSIGNED_BYTE,
                mapsized->tmp);
 
   //GR_callobj (p_gr_objid);

   GR_popmatrix();
   GR_popattributes ();
}
