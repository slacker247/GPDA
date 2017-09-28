/**********************************************************************
 See GR_Tearth3.C
 
  -- 06/29/93: created, Tung.
  -- 07/13/93: changed granularity;
     
********************************************************************/

#include "GR_Tearth3.H"
#include "GR_work.H"

extern void rgbtocpack
     (unsigned short* r, unsigned short* g, unsigned short* b,
      unsigned long* l, int n);

void three_comp_rgbtocpack 
     (unsigned short* r, unsigned short* g, unsigned short* b,
      unsigned long* l, int n);

GR_Tearth3::GR_Tearth3 (char *dirname, long type)
{
   int i, j, maxi;
   
   if (getgdesc(GD_TEXTURE) == 0)
   {
      fprintf (stderr,
	       "Sorry: Texuture mapping not available on this machine.\007\n");
      set_visible_flag (0);
      return;
   }
   set_register_flag (0);

   if (!dirname)
      dirname = "../../data";
 
   for (i=0; i<52; i++)
   {
      sprintf (p_filename[i], "%s/tearth3data/e.%d", dirname, i+1);
      printf ("%d). --> %s\n", i+1, p_filename[i]); 
   }

   maxi = texture_init ();

   p_type = type;
   p_gr_objid = GR_genobj ();
   GR_makeobj (p_gr_objid);
   GR_color (255,255,255);

   for (i=0; i<4 && i<maxi; i++)
   {
      texbind (TX_TEXTURE_0, 301+i);
      draw_area (-180+180*(i%2), -90+150*(i/2), 180*(i%2), -60+150*(i/2));
   }
   /*
   texbind (TX_TEXTURE_0, 11);
   draw_area (-180, -90, 0, -60);
   texbind (TX_TEXTURE_0, 12);
   draw_area (0, -90, 180, -60);
   texbind (TX_TEXTURE_0, 13);
   draw_area (-180, 60, 0, 90);
   texbind (TX_TEXTURE_0, 14);
   draw_area (0, 60, 180, 90);
   */

   for (j=0; j<=3 && (12*j+i)<maxi; j++)
   {
      for (i=0; i<=11 && (12*j+i)<maxi; i++)
      {
	 texbind (TX_TEXTURE_0, j*12+i+305);
	 draw_area (-180+i*30, -60+j*30, -150+i*30, -30+j*30);
      }
   }
   GR_closeobj ();
}

      
int
GR_Tearth3::texture_init ()
{
   unsigned long *image[52];
   int i, maxi;
   char msgstr[80];

   p_texps[0] = TX_MINFILTER;
   p_texps[1] = TX_POINT;
   p_texps[2] = TX_MAGFILTER;
   p_texps[3] = TX_BILINEAR;
   p_texps[4] = TX_WRAP;
   p_texps[5] = TX_CLAMP;
   p_texps[6] = TX_NULL;
  
   p_tevps[0] = TV_NULL;
   tevdef(2, 1, p_tevps);
 
   work_progress (0, "Earth Texture"); 
   
   for (i=0; i<52; i++)
   {
      sprintf (msgstr, "Doing image file %s\n",
               p_filename[i]);
      printf("%s\n", msgstr);
      work_progress (1, msgstr); 
      image[i] = (unsigned long *) take_rgbdata (p_filename[i]);
      if (!image[i])
      {
         fprintf (stderr, "Oops, I don't find the image file.\n");
         work_progress (3, NULL);
         return i;
      } 
      texdef2d(301+i,4,p_xsize,p_ysize,image[i],7,p_texps);
      maxi = i;
   }
   maxi++;
   work_progress (2, NULL);
   work_progress (3, NULL);
   return maxi;

}

   
void
GR_Tearth3::draw_area (long slon, long slat, long elon, long elat)
{
   int j, k;
   long jmax, kmax;
   float theta, phi;
   float theta_0, phi_0; // initial lon and lat in radians;
   float vert[3];
   float factor = 1.0010;
   float d2r = M_PI/180.0;
   long resolution = 5;
   float delta = M_PI * resolution / 180.0; 

   theta_0 = slon * d2r;
   phi_0 = slat * d2r;
   jmax = (elat - slat) / resolution;
   kmax = (elon - slon) / resolution;

   for (k=1, theta=theta_0; k<=kmax; k++, theta+= delta)
   {
      GR_bgnqstrip ();
      for (j=1, phi=phi_0; j<=jmax+1; j++, phi+= delta)
      {
	 vert[0] = (k - 1) / (kmax*1.0);
	 vert[1] = (j - 1) / (jmax*1.0);
	 // to solve the seam problem.....:
         if (vert[1]==1.0)
           vert[1]=0.992;
         t2f (vert);
	 vert[0] = cos(phi) * sin (theta) * factor;
	 vert[1] = sin(phi) * factor;
	 vert[2] = cos(phi) * cos (theta) * factor;
	 GR_n3f (vert);
	 GR_v3f (vert);
	 
	 vert[0] = k / (kmax*1.0);
	 vert[1] = (j - 1) / (jmax*1.0);
	 // to solve the seam problem.....: 
         if (vert[0]==1.0)
           vert[0]=0.992;
         if (vert[1]==1.0)
           vert[1]=0.992;
 	 t2f (vert);
	 vert[0] = cos(phi) * sin (theta+delta) * factor;
	 vert[1] = sin(phi) * factor;
	 vert[2] = cos(phi) * cos (theta+delta) * factor;
	 GR_n3f (vert);
	 GR_v3f (vert);
      }
      GR_endqstrip ();
   }
}


unsigned long*
GR_Tearth3::take_rgbdata (char* infile)
{
   IMAGE* image;
   int xsize, ysize;
   unsigned long *base, *lptr;
   unsigned short *rbuf, *gbuf, *bbuf;
   int y;

   image = iopen (infile, "r");
   if (!image)
   {
      perror("iopen\007");
      return NULL;
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

   if (!base || !rbuf || !gbuf || !bbuf)
   {
      perror("malloc\007");
      return NULL;
   }

   lptr = base;
   for(y=0; y<ysize; y++)
   {
      if(image->zsize==3)
      {
	 getrow(image,rbuf,y,0);
	 getrow(image,gbuf,y,1);
	 getrow(image,bbuf,y,2);
	 //three_comp_rgbtocpack(rbuf,gbuf,bbuf,lptr,xsize);
	 rgbtocpack(rbuf,gbuf,bbuf,lptr,xsize);
	 lptr += xsize;
      }
      else
      {
	 perror("wrong zsize\007");
	 return NULL;
      }
      
   }
   iclose(image);
   free(rbuf);
   free(gbuf);
   free(bbuf);
   return base;
}

// Note: still buggy:
void
three_comp_rgbtocpack (unsigned short* r, unsigned short* g, unsigned short* b,
            unsigned long* l, int n)
{
   while (n>=4)
   {
      l[0] = b[1] | (r[0]<<8) | (g[0]<<16) | (b[0]<<24);
      l[1] = g[2] | (b[2]<<8) | (r[1]<<16) | (g[1]<<24);
      l[2] = r[3] | (g[3]<<8) | (b[3]<<16) | (r[2]<<24);
      l += 3;
      r += 4;
      g += 4;
      b += 4;
      n -= 4;
   }
}


void
GR_Tearth3::objdraw ()
{
  tevbind (TV_ENV0, 2);
  GR_backface (TRUE);
  GR_callobj (p_gr_objid);
  GR_backface (FALSE);
  tevbind (TV_ENV0, 0); // turn off texture;
}

