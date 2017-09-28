/**********************************************************************
  A testing transparent poly w. texture of clouds.
  
  -- Tung, 3/29/93  
********************************************************************/
#include <stdio.h>
#include "GR_Cloud.H"

Cloud::Cloud (long type, char* filename)
{
   int ret_code;

   if (getgdesc (GD_TEXTURE) == 0)
   {
      fprintf (stderr,
	       "Sorry: Texuture mapping not available on this machine.\007\n");
      set_visible_flag (0);
      return;
   }

   if (getgdesc (GD_BLEND) == 0)
   {
      fprintf (stderr,
	       "Sorry: you cannot do blendfunction on this machine.\007\n");
      set_visible_flag (0);
      return;
   }
   set_register_flag (0);

   if (filename)
      p_filename = filename;
   else
     p_filename = strdup ("cloud.test");

   ret_code = texture_init ();

   p_type = type;

   p_gr_objid = GR_genobj ();
   GR_makeobj (p_gr_objid);
   if (ret_code > 0)  // i.e., texture_init() is successful;
   {
     blendfunction (BF_SA, BF_MSA);
     GR_cpack (0x80000000);
     texbind (TX_TEXTURE_0, 202);
     draw_Cloud (-105, 15);
     blendfunction (BF_ONE, BF_ZERO);
   }
   GR_closeobj ();
}

      
int
Cloud::texture_init ()
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
  
   p_tevps[0] = TV_BLEND;
   p_tevps[1] = TV_COLOR;
   p_tevps[2] = 1.0;
   p_tevps[3] = 1.0;
   p_tevps[4] = 1.0;
   p_tevps[5] = 1.0;
   p_tevps[6] = TV_NULL;
   tevdef(202, 7, p_tevps);

   sprintf (msgstr, "Doing image file %s\n", p_filename);
   printf("%s", msgstr);
   image = (unsigned long *) take_bwdata (p_filename);
   if (!image)
   {
     fprintf (stderr, "Oops, I con't find image file %s\n", p_filename);
     return 0;
   }  
   texdef2d(202, 2, p_xsize, p_ysize, image, 7, p_texps);
   return 1;
}

   
void
Cloud::draw_Cloud (long lon, long lat)
{
   int j, k;
   float theta, phi;
   float theta_0, phi_0; // initial lon and lat in radians;
   float vert[3];
   float factor = 1.05;

   theta_0 = lon * M_PI / 180.0;
   phi_0 = lat * M_PI / 180.0;

   for (k=1, theta=theta_0; k<=6; k++, theta+= M_PI/36)
   {
      GR_bgnqstrip ();
      for (j=1, phi=phi_0; j<=4; j++, phi+= M_PI/36)
      {
	 vert[0] = (k - 1) / 6.0;
	 vert[1] = (j - 1) / 3.0;
	 // printf ("t=%f, s=%f\n", vert[0], vert[1]);
	 t2f (vert);
	 vert[0] = cos(phi) * sin (theta) * factor;
	 vert[1] = sin(phi) * factor;
	 vert[2] = cos(phi) * cos (theta) * factor;
	 // n3f (vert);
	 GR_v3f (vert);
	 
	 vert[0] = k / 6.0;
	 vert[1] = (j - 1) / 3.0;
	 // printf ("t=%f, s=%f\n", vert[0], vert[1]);
	 t2f (vert);
	 vert[0] = cos(phi) * sin (theta+M_PI/36) * factor;
	 vert[1] = sin(phi) * factor;
	 vert[2] = cos(phi) * cos (theta+M_PI/36) * factor;
	 // n3f (vert);
	 GR_v3f (vert);
      }
      GR_endqstrip ();
   }
}


unsigned long*
Cloud::take_bwdata (char* infile)
{
   IMAGE* image;
   int xsize, ysize;
   unsigned long *lptr;
   unsigned short *base, *ibuf, *abuf;
   int y, x;

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
   if (image->zsize != 1)
   {
      perror ("wrong zsize\007");
      return NULL;
   }
   //printf ("xsize=%d, ysize=%d, zsize=%d\n", p_xsize, p_ysize, image->zsize);
 
 
   base = (unsigned short*) malloc (xsize*ysize*sizeof(unsigned short));
   ibuf = (unsigned short*) malloc (xsize*sizeof(unsigned short));
   abuf = (unsigned short*) malloc (xsize*sizeof(unsigned short));

   if (!base || !ibuf || !abuf)
   {
      perror("malloc\007");
      return NULL;
   }

   for (y=0; y<ysize; y++)
   {
      getrow(image,ibuf,y,0);
      for (x=0; x<xsize; x++)
      {
	 //base[x+y*xsize] = ibuf[x] | (ibuf[x]<<8);
         base[x+y*xsize] = (0xff) | (ibuf[x]<<8);
      }
   }
   iclose(image);
   free(ibuf);
   free(abuf);
   lptr = (unsigned long*)base;
   return lptr;
}


void
Cloud::objdraw ()
{
  tevbind (TV_ENV0, 202);
  GR_backface (TRUE);
  GR_callobj (p_gr_objid);
  GR_backface (FALSE);
  tevbind (TV_ENV0, 0); // turn off texture;
}

/* ========== */

