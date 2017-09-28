#include <math.h>
#include "GR_Tarea.H"
#include <sys/types.h>
#include <malloc.h>
#include <Xm/MessageB.h>
/*
void rgbatocpack (unsigned short* r, unsigned short* g, unsigned short* b,
                  unsigned short* a, unsigned long*  l, int n);
void rgbtocpack (unsigned short* r, unsigned short* g, unsigned short* b,
		 unsigned long* l, int n);
*/
/*

GR_Tarea::GR_Tarea (char* filename, long type)
{
   int i;
 
   if (getgdesc(GD_TEXTURE) == 0)
   {
      fprintf (stderr,
	       "Sorry: Texuture mapping not available on this machine.\n");
      set_visible_flag (0);
      return;
   }
   set_register_flag (0);
   
   if (!filename)
   {
      p_filename = "/usr/esd/tung/earth/data/jpl_2048x1024_gamma2p5.rgb";
   }
   else
     p_filename = filename;

   p_type = type;
   p_gr_objid = genobj ();
   makeobj (p_gr_objid);
   GR_color (255,255,255);
   draw_area (18, TRUE, TRUE);
   closeobj ();

   texture_init ();
}

      
void
GR_Tarea::texture_init ()
{
   unsigned long *image;
   int i,j,tiles;
   float stx,sty,edx,edy,tilesizex, tilesizey;

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
  
   if (type < 10)
      printf ("wrong area texture type specified -- has to be > 2000.\007\n"); 
   
   texdef2d(type,4,p_xsize,p_ysize,image,9,p_texps);
}

   
void
GR_Tarea::draw_area (int n, Boolean nflag, Boolean tflag)
{
   int i, j, k;
   float phi, theta, theta_save;
   float vert[3];

   for (k=1, theta=-M_PI; k<=4*n; k++, theta+= M_PI_2/n)
   {
      bgnqstrip ();
      for (j=1, phi=-M_PI_2; j<=2*n+1; j++, phi+= M_PI_2/n)
      {
         theta_save = theta;
	 for (i=1; i<=2; i++, theta+=M_PI_2/n)
	 {
	    if (tflag)
	    {
	       vert[0] = ((float)theta/(2*M_PI) + 0.5);
	       vert[1] = ((float)phi/M_PI + 0.5);
	       t2f (vert);
	    }
	    vert[0] = cos(phi) * sin (theta);
	    vert[1] = sin(phi);
	    vert[2] = cos(phi) * cos (theta);
	    
	    if (nflag)
	      n3f (vert);
	    v3f (vert);
	 }
	 theta = theta_save;
      }
      endqstrip ();
   }
}


// a partial sphere, or for now just a quadrant:   
void
GR_Tarea::draw_sphere (int n, Boolean nflag, Boolean tflag, int quadrant)
{
   int i, j, k;
   float phi, theta, theta_save;
   float vert[3];

   switch (quadrant)
   {
    case 1:
    case 2:
      theta = -M_PI;
      break;
    case 3:
    case 4:
      theta = 0;
      break;      
   }   
   for (k=1; k<=2*n; k++, theta+= M_PI_2/n)
   {
      bgnqstrip ();
      switch (quadrant)
      {
       case 1:
       case 3:
	 phi = -M_PI_2;
	 break;
       case 2:
       case 4:
	 phi = 0;
	 break;
      }	 
      for (j=1; j<=n+1; j++, phi+= M_PI_2/n)
      {
         theta_save = theta;
	 for (i=1; i<=2; i++, theta+=M_PI_2/n)
	 {
	    if (tflag)
	    {
	       vert[0] = ((float)theta/(2*M_PI) + 0.5);
	       vert[1] = ((float)phi/M_PI + 0.5);
	       t2f (vert);
	    }
	    vert[0] = cos(phi) * sin (theta);
	    vert[1] = sin(phi);
	    vert[2] = cos(phi) * cos (theta);
	    
	    if (nflag)
	      n3f (vert);
	    v3f (vert);
	 }
	 theta = theta_save;
      }
      endqstrip ();
   }
}


unsigned long*
GR_Tarea::take_rgbdata (char* infile)
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
GR_Tarea::objdraw ()
{
  tevbind (TV_ENV0, 4);
  callobj (p_gr_objid);
  tevbind (TV_ENV0, 0); // turn off texture;
}


void
GR_Tarea::v_process_pick (GR_MouseEvent& event, GR_Window* window)
{
   Widget dialog;
   Arg arg[10];
   char str [80]; XmString xstr;
   
   printf ("The Textured Earth was picked..");
   switch (event.button)
   {
    case GR_LEFTMOUSE:
      if (event.down)
      {
	 printf ("..by GR_LEFTMOUSE..");
      }
      break;
    case GR_MIDDLEMOUSE:
      if (event.down)
      {
	 printf (".. by GR_MIDDLEMOUSE..");
      }
      break;
    case GR_RIGHTMOUSE:
      if (event.down)
      {
	 printf (".. by GR_RIGHTMOUSE..");
         sprintf (str, "Textured Earth, Type = %d", p_type);
         xstr = XmStringCreateSimple (str);
         XtSetArg (arg[0], XmNmessageString, xstr);
         dialog = XmCreateMessageDialog (window->widget(), "message",
arg, 1);
         XmStringFree (xstr);
         XtManageChild (dialog);
    }
      break;
   }
   printf("\n");
}
*/
