/**********************************************************************
  This will make an area map onto the 3D globe. For now, this is only
  good for the Hawaii area. This will be replaced by GR_Tarea.[HC].

  -- create by Tung, 03/04/93;
  
  ********************************************************************/

#include <math.h>
#include <gl/image.h>
#include <sys/types.h>
#include <malloc.h>
#include <Xm/MessageB.h>
#include "GR_Interface.H"
#include "GR_DispObj.H"

extern "C" IMAGE* iopen (const char*, const char*); 
extern "C" void iclose (IMAGE*);
extern "C" void getrow (IMAGE*,unsigned short*, int, int);

void rgbatocpack (unsigned short* r, unsigned short* g, unsigned short* b,
                  unsigned short* a, unsigned long*  l, int n);
void rgbtocpack (unsigned short* r, unsigned short* g, unsigned short* b,
		 unsigned long* l, int n);


class Area: public GR_DispObj
{
 private:
   char* p_filename;
   float p_texps[9];
   float p_tevps[1];
   int p_xsize;
   int p_ysize;

 protected:
   void texture_init();
   void draw_area ();

 public:
   Area (char* filename=NULL, long type=201);
   unsigned long* take_rgbdata (char* infile);
   void objdraw ();
};


Area::Area (char* filename, long type)
{
   if (getgdesc(GD_TEXTURE) == 0)
   {
      fprintf (stderr,
	       "Sorry: Texuture mapping not available on this machine.\n");
      set_visible_flag (0);
      return;
   }
   set_register_flag (0);

   if (!filename)
     p_filename = "/home/simserv/tung/jk/hawaii.rgb";
   else
     p_filename = filename;

   p_type = type;
   p_gr_objid = genobj ();
   makeobj (p_gr_objid);
   GR_color (255,255,255);
   draw_area ();
   closeobj ();
   texture_init ();
}

      
void
Area::texture_init ()
{
   unsigned long *image;
   //int i,j,tiles;
   //float stx,sty,edx,edy,tilesizex, tilesizey;

   p_texps[0] = TX_MINFILTER;
   p_texps[1] = TX_BILINEAR;
   p_texps[2] = TX_MAGFILTER;
   p_texps[3] = TX_BILINEAR;
   p_texps[4] = TX_WRAP_S;
   p_texps[5] = TX_CLAMP;
   p_texps[6] = TX_WRAP_T;
   p_texps[7] = TX_CLAMP;
   p_texps[8] = TX_NULL;
  
   p_tevps[0] = TV_NULL;
   tevdef(5, 1, p_tevps);
 
   printf("\Reading image file.....\n");
   image = (unsigned long *) take_rgbdata (p_filename);
 
   p_texps[4] = TX_NULL; 
   texdef2d(102,4,p_xsize,p_ysize,image,5,p_texps);
}

   
void
Area::draw_area ()
{
   int j, k;
   float phi, theta;
   float vert[3];
   float factor = 1.0005;

   for (k=1, theta=-M_PI; k<=6; k++, theta+= M_PI/36)
   {
      bgnqstrip ();
      for (j=1, phi=M_PI/12; j<=4; j++, phi+= M_PI/36)
      {
	 vert[0] = (k - 1) / 6.0;
	 vert[1] = (j - 1) / 3.0;
            printf ("t=%f, s=%f\n", vert[0], vert[1]);
	 t2f (vert);
	 vert[0] = cos(phi) * sin (theta) * factor;
	 vert[1] = sin(phi) * factor;
	 vert[2] = cos(phi) * cos (theta) * factor;
	 //n3f (vert);
	 v3f (vert);
	 
	 vert[0] = k / 6.0;
	 vert[1] = (j - 1) / 3.0;
            printf ("t=%f, s=%f\n", vert[0], vert[1]);
	 t2f (vert);
	 vert[0] = cos(phi) * sin (theta+M_PI/36) * factor;
	 vert[1] = sin(phi) * factor;
	 vert[2] = cos(phi) * cos (theta+M_PI/36) * factor;
	 //n3f (vert);
	 v3f (vert);
      }
      endqstrip ();
   }
}


unsigned long*
Area::take_rgbdata (char* infile)
{
   IMAGE* image;
   int xsize, ysize;
   unsigned long *base, *lptr;
   unsigned short *rbuf, *gbuf, *bbuf, *abuf;
   int y;

   image = iopen (infile, "r");
   if (!image)
   {
      perror("iopen");
      exit(-1);
   }
 
   xsize = image->xsize;
   ysize = image->ysize;
   p_xsize = xsize; // here to determine the real sizes...
   p_ysize = ysize;

     printf ("xsize=%d, ysize=%d, zsize=%d\n", p_xsize, p_ysize, image->zsize);
 
 
   base = (unsigned long*) malloc (xsize*ysize*sizeof(unsigned long));
   rbuf = (unsigned short*) malloc (xsize*sizeof(unsigned short));
   gbuf = (unsigned short*) malloc (xsize*sizeof(unsigned short));
   bbuf = (unsigned short*) malloc (xsize*sizeof(unsigned short));
   abuf = (unsigned short*) malloc (xsize*sizeof(unsigned short));

   if (!base || !rbuf || !gbuf || !abuf)
   {
      perror("malloc");
      exit(-1);
   }

   lptr = base;
   for(y=0; y<ysize; y++)
   {
      if(image->zsize>=4)
      {
	 getrow(image,rbuf,y,0);
	 getrow(image,gbuf,y,1);
	 getrow(image,bbuf,y,2);
	 getrow(image,abuf,y,3);
	 rgbatocpack(rbuf,gbuf,bbuf,abuf,lptr,xsize);
	 lptr += xsize;
      }
      else if(image->zsize==3)
      {
	 getrow(image,rbuf,y,0);
	 getrow(image,gbuf,y,1);
	 getrow(image,bbuf,y,2);
	 rgbtocpack(rbuf,gbuf,bbuf,lptr,xsize);
	 lptr += xsize;
      }
      else
      {
	 perror("wrong zsize");
	 exit(-1);
      }
      
   }
   iclose(image);
   free(rbuf);
   free(gbuf);
   free(bbuf);
   free(abuf);
   return base;
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
Area::objdraw ()
{
  tevbind (TV_ENV0, 5);
  texbind (TX_TEXTURE_0, 102);
  callobj (p_gr_objid);
  tevbind (TV_ENV0, 0); // turn off texture;
}

