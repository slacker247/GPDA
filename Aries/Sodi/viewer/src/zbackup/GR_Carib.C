/**********************************************************************
  This loads the detailed Caribbean areas -- ugly but fast.
  -- Tung, 3/18/93
     
********************************************************************/

#include "GR_Carib.H"
#include "GR_work.H"
/*
void rgbatocpack (unsigned short* r, unsigned short* g, unsigned short* b,
                  unsigned short* a, unsigned long*  l, int n);
void rgbtocpack (unsigned short* r, unsigned short* g, unsigned short* b,
		 unsigned long* l, int n);
*/
Carib::Carib (char *dirname, long type)
{
   int i, maxi;
   char filename[20];
   
   if (getgdesc(GD_TEXTURE) == 0)
   {
      fprintf (stderr,
	       "Sorry: Texuture mapping not available on this machine.\n");
      set_visible_flag (0);
      return;
   }
   set_register_flag (0);

   if (!dirname)
      dirname = "../../data";
 
   for (i=0; i<9; i++)
   {
      sprintf (p_filename[i], "%s/carib.%d", dirname, i);
      printf ("%d). --> %s\n", i, p_filename[i]); 
   }

   maxi = texture_init ();

   p_type = type;
   p_gr_objid = GR_genobj ();
   GR_makeobj (p_gr_objid);
   GR_color (255,255,255);

   for (i=0; i<maxi; i++)
   {
     texbind (TX_TEXTURE_0, 102+i);
     draw_Carib (-135+30*(i%3), 30-15*(i/3));
   }
   /*
   texbind (TX_TEXTURE_0, 102);
   draw_Carib (-135, 30);
   texbind (TX_TEXTURE_0, 103);
   draw_Carib (-105, 30);
   texbind (TX_TEXTURE_0, 104);
   draw_Carib (-75, 30);
   texbind (TX_TEXTURE_0, 105);
   draw_Carib (-135, 15);
   texbind (TX_TEXTURE_0, 106);
   draw_Carib (-105, 15);
   texbind (TX_TEXTURE_0, 107);
   draw_Carib (-75, 15);
   texbind (TX_TEXTURE_0, 108);
   draw_Carib (-135, 0);
   texbind (TX_TEXTURE_0, 109);
   draw_Carib (-105, 0);
   texbind (TX_TEXTURE_0, 110);
   draw_Carib (-75, 0);
   */

   GR_closeobj ();
}

      
int
Carib::texture_init ()
{
   unsigned long *image[9];
   int i, maxi;
   char msgstr[80];

   p_texps[0] = TX_MINFILTER;
   p_texps[1] = TX_POINT;
   p_texps[2] = TX_MAGFILTER;
   p_texps[3] = TX_BILINEAR;
   //p_texps[4] = TX_NULL;
   p_texps[4] = TX_WRAP;
   p_texps[5] = TX_REPEAT;
   p_texps[6] = TX_NULL;
  
   p_tevps[0] = TV_NULL;
   tevdef(5, 1, p_tevps);
 
   work_progress (0, "Caribbean Texture"); 
   
   for (i=0; i<9; i++)
   {
      sprintf (msgstr, "Doing image file %s\n",
               p_filename[i]);
      printf("%s\n", msgstr);
      work_progress (1, msgstr); 
      image[i] = (unsigned long *) take_rgbdata (p_filename[i]);
      if (!image[i])
      {
        fprintf (stderr, "Oops, I don't find the image file %s.\n",
                 p_filename[i]);
        work_progress (3, NULL);
        return i;
      }
      texdef2d(102+i,4,p_xsize,p_ysize,image[i],7,p_texps);
      maxi = i;
   }
   maxi++;
   work_progress (2, NULL);
   work_progress (3, NULL);
   return maxi;
}

   
void
Carib::draw_Carib (long lon, long lat)
{
   int j, k;
   float theta, phi;
   float theta_0, phi_0; // initial lon and lat in radians;
   float vert[3];
   float factor = 1.0010;
   float d2r = M_PI/180.0;

   theta_0 = lon * d2r;
   phi_0 = lat * d2r;

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
	 GR_n3f (vert);
	 GR_v3f (vert);
	 
	 vert[0] = k / 6.0;
	 vert[1] = (j - 1) / 3.0;
	 // printf ("t=%f, s=%f\n", vert[0], vert[1]);
	 t2f (vert);
	 vert[0] = cos(phi) * sin (theta+M_PI/36) * factor;
	 vert[1] = sin(phi) * factor;
	 vert[2] = cos(phi) * cos (theta+M_PI/36) * factor;
	 GR_n3f (vert);
	 GR_v3f (vert);
      }
      GR_endqstrip ();
   }
}


unsigned long*
Carib::take_rgbdata (char* infile)
{
   IMAGE* image;
   int xsize, ysize;
   unsigned long *base, *lptr;
   unsigned char *rbuf, *gbuf, *bbuf, *abuf;
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
   rbuf = (unsigned char*) malloc (xsize*sizeof(unsigned short));
   gbuf = (unsigned char*) malloc (xsize*sizeof(unsigned short));
   bbuf = (unsigned char*) malloc (xsize*sizeof(unsigned short));
   abuf = (unsigned char*) malloc (xsize*sizeof(unsigned short));

   if (!base || !rbuf || !gbuf || !abuf)
   {
      perror("malloc\007");
      return NULL;
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
	 perror("wrong zsize \007");
	 return NULL;
      }
      
   }
   iclose(image);
   free(rbuf);
   free(gbuf);
   free(bbuf);
   free(abuf);
   return base;
}
/*
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
*/

void
Carib::objdraw ()
{
  tevbind (TV_ENV0, 5);
  GR_backface (TRUE);
  GR_callobj (p_gr_objid);
  GR_backface (FALSE);
  tevbind (TV_ENV0, 0); // turn off texture;
}

