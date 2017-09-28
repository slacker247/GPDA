/*********************************************************************
 *
 * Texture mapped earth, use tiling for higher resolution version, 
 * with area texture capability;
 *
 *********************************************************************/

#ifndef GR_TEARTH
#define GR_TEARTH  "earthclass"

#include <math.h>
#include <sys/types.h>
#include <malloc.h>
#include <stdio.h>

#include "GL/gltk.h"

#define   TRUE        1
#define   FALSE       0
#define   RE          6378.145

typedef int Boolean;

class Tearth
{
 private:
   char*          p_filename;
   unsigned int   p_slid;
   unsigned int   p_clid;
   unsigned int   p_plid;
   unsigned int   p_glid;
   unsigned int   p_tlid;
   float          p_texps[14];
   float          p_tevps[7];
   Boolean        p_dotiling;
   Boolean        p_texturing;
   Boolean        p_terrain;
   int            p_component;
   int            p_type;
   int            p_xsize;
   int            p_ysize;
   float          p_fov;
   float          p_aspect;
   float          p_near;
   float          p_far;
   float          p_lat;
   float          p_lon;
   float          p_alt;
   float          p_azi;
   float          p_twist;
   float          p_lookx;
   float          p_looky;
   float          p_lookz;
   TK_RGBImageRec *image;
   
 protected:
   int  texture_init();
   void earthlines(int R, int G, int B, char* filename, int type);
  //unsigned long* take_rgbdata (char* infile);

 public:
   Tearth();
   void globe(char* filename=NULL, int component=4,
	      int xsize=2048, int ysize=1024, int lid=0);
   void gridlines(int R, int G, int B, int degree, int type);
   void coasts(char *fname, int id);
   void political(char *fname, int id);
   void terrain(char *terrafile, int type);
   void objdraw ();
   void set_vparams(int vmode, int lat, int lon, int alt, int fov, int azi);
   void set_twist(float twist)   { p_twist = twist; }
   void set_near(float near)     { p_near = near; }
   void set_far(float far)       { p_far = far; }
   void set_aspect(float aspect) { p_aspect = aspect; }
   void set_fov(float fov)       { p_fov = fov; }
   void set_lookat(float lx, float ly, float lz) 
                  { p_lookx = lx; p_looky = ly; p_lookz = lz; }
   void set_texture(Boolean tex) { p_texturing = tex; }

};
#endif
