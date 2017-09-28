/**********************************************************************
  This test area loads an arbitrary size of texture, w. timing info;
  the purpose is to test for clarity as well as timing;
  
  -- Tung, 4/12/93
  
  ********************************************************************/

#include <stdio.h>
#include <unistd.h>
#include <math.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/param.h>
#include <gl/image.h>
#include <malloc.h>
#include <Xm/MessageB.h>
#include "GR_Interface.H"
#include "GR_DispObj.H"


extern "C" IMAGE* iopen (const char*, const char*); 
extern "C" void iclose (IMAGE*);
extern "C" void getrow (IMAGE*,unsigned short*, int, int);

extern void rgbtocpack (unsigned short* r, unsigned short* g, unsigned short* b,
		 unsigned long* l, int n);

double t0, dt;
//extern struct tms Tbuf;
struct tms Tbuf;


class Test: public GR_DispObj
{
 private:
   char* p_filename;
   float p_texps[7];
   float p_tevps[1];
   int p_xsize;
   int p_ysize;

 protected:
   void texture_init();
   void draw_Test (long lon, long lat);

 public:
   Test (long type=203, char* filename=NULL);
   unsigned long* take_rgbdata (char* infile);
   void objdraw ();
};

Test::Test (long type, char* filename)
{
   if (getgdesc(GD_TEXTURE) == 0)
   {
      fprintf (stderr,
	       "Sorry: Texuture mapping not available on this machine.\n");
      set_visible_flag (0);
      return;
   }
   set_register_flag (0);

   if (filename)
     p_filename = filename;
   else
     p_filename = strdup ("test.rgb");   

   finish ();
   t0 = times (&Tbuf);
   texture_init ();
   finish ();
   dt = times (&Tbuf) - t0;
   printf ("==> %d x %d image takes %lf ms to load.\n",
	   p_xsize, p_ysize, dt*10);

   p_type = type;
   p_gr_objid = genobj ();
   makeobj (p_gr_objid);
   GR_color (255,255,255);
   texbind (TX_TEXTURE_0, p_xsize+2000);
   draw_Test (0, 0);
   closeobj ();
}

      
void
Test::texture_init ()
{
   unsigned long *image;
   char msgstr[80];

   p_texps[0] = TX_MINFILTER;
   p_texps[1] = TX_POINT;
   p_texps[2] = TX_MAGFILTER;
   p_texps[3] = TX_BILINEAR;
   p_texps[4] = TX_WRAP;
   p_texps[5] = TX_REPEAT;
   p_texps[6] = TX_NULL;
  
   sprintf (msgstr, "Doing image file %s\n", p_filename);
   printf("%s", msgstr);
   image = (unsigned long *) take_rgbdata (p_filename);
   texdef2d(p_xsize+2000,4,p_xsize,p_ysize,image,7,p_texps);
   
   p_tevps[0] = TV_NULL;
   tevdef(203, 1, p_tevps);
}


/*
  draw_Test draws an area of polygons for the texture, starting at
  specified (lat,lon) location. Assume the texture, coming from the
  original 7926x3963 data, is used as a true size earth area. Since the
  texture size may vary (from 660x330, 990x495, to 1320x660,  etc.)
  and thus the covered area varis, the polygon area should change
  accordingly -- though the number of polygons is kept for 6x3 in all
  cases.
  */
void
Test::draw_Test (long lon, long lat)
{
   int j, k;
   float theta, phi;
   float theta_0, phi_0; // initial lon and lat in radians;
   float vert[3];
   float factor = 1.0010;
   float delta_angle = M_PI/ ((int)(3963/p_xsize)*6);
   // for a 660x330 image -> M_PI/36;
   // for a 990x495 image -> M_PI/24;
   // for a 1320x660 image -> M_PI/18;

   theta_0 = lon * M_PI/180.0;
   phi_0 = lat * M_PI/180.0;

   for (k=1, theta=theta_0; k<=6; k++, theta+= delta_angle)
   {
      bgnqstrip ();
      for (j=1, phi=phi_0; j<=4; j++, phi+= delta_angle)
      {
	 vert[0] = (k - 1) / 6.0;
	 vert[1] = (j - 1) / 3.0;
	 t2f (vert);
	 vert[0] = cos(phi) * sin (theta) * factor;
	 vert[1] = sin(phi) * factor;
	 vert[2] = cos(phi) * cos (theta) * factor;
	 n3f (vert);
	 v3f (vert);
	 
	 vert[0] = k / 6.0;
	 vert[1] = (j - 1) / 3.0;
	 t2f (vert);
	 vert[0] = cos(phi) * sin (theta+delta_angle) * factor;
	 vert[1] = sin(phi) * factor;
	 vert[2] = cos(phi) * cos (theta+delta_angle) * factor;
	 n3f (vert);
	 v3f (vert);
      }
      endqstrip ();
   }
}


unsigned long*
Test::take_rgbdata (char* infile)
{
   IMAGE* image;
   int xsize, ysize;
   unsigned long *base, *lptr;
   unsigned short *rbuf, *gbuf, *bbuf;
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

   base = (unsigned long*) malloc (xsize*ysize*sizeof(unsigned long));
   rbuf = (unsigned short*) malloc (xsize*sizeof(unsigned short));
   gbuf = (unsigned short*) malloc (xsize*sizeof(unsigned short));
   bbuf = (unsigned short*) malloc (xsize*sizeof(unsigned short));

   if (!base || !rbuf || !gbuf)
   {
      perror("malloc");
      exit(-1);
   }

   lptr = base;
   for(y=0; y<ysize; y++)
   {
      if(image->zsize==3)
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
   return base;
}



void
Test::objdraw ()
{
   int i;
   float t0;
   
   finish ();
   t0 = times (&Tbuf);

   for (i=0; i<10; i++)
   {
      tevbind (TV_ENV0, 203);
      backface (TRUE);
      callobj (p_gr_objid);
      backface (FALSE);
      tevbind (TV_ENV0, 0); // turn off texture;
   }
   
   finish ();
   dt = times (&Tbuf) - t0;
   printf ("--> %d x %d texture drawing time is %lf ms.\n",
	   p_xsize, p_ysize, dt);
}

/* ========= */

extern GR_Window* gwindow;
extern GR_DispList* displist;
Test* test1;
Test* test2;
Test* test3;

void
test1CB ()
{
   static Boolean first_test1 = TRUE;
   static Boolean on_flag = FALSE;

   if (first_test1)
   {
      printf ("test1 is called...\n");
      test1 = new Test (203, "test1.rgb");
      displist->add_object (test1);
      first_test1 = FALSE;
   }
   else
   {
      if (!on_flag)
	displist->delete_object_by_type (203);
      else
	displist->add_object (test1);
      on_flag = !on_flag;
   }
   gwindow->draw ();
}

void
test2CB ()
{
   static Boolean first_test2 = TRUE;
   static Boolean on_flag = FALSE;

   if (first_test2)
   {
      printf ("test2 is called...\n");
      test2 = new Test (204, "test2.rgb");
      displist->add_object (test2);
      first_test2 = FALSE;
   }
   else
   {
      if (!on_flag)
	displist->delete_object_by_type (204);
      else
	displist->add_object (test2);
      on_flag = !on_flag;
   }
   gwindow->draw ();
}

void
test3CB ()
{
   static Boolean first_test3 = TRUE;
   static Boolean on_flag = FALSE;

   if (first_test3)
   {
      printf ("test3 is called...\n");
      test3 = new Test (205, "test3.rgb");
      displist->add_object (test3);
      first_test3 = FALSE;
   }
   else
   {
      if (!on_flag)
	displist->delete_object_by_type (205);
      else
	displist->add_object (test3);
      on_flag = !on_flag;
   }
   gwindow->draw ();
}
