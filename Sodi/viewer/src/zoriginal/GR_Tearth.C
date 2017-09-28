/************************************************************

  11/06/92, Tung: Added work dialog to report work progress.
  02/22/93: tried 6-tiles, etc.
  07/22/93: changed a few things: comment out the untiled texture
            loading; let "exit" be "return" on file not found. 
 
************************************************************/

#include "GR_Tearth.H"
#include "GR_work.H"
/*
void rgbatocpack (unsigned short* r, unsigned short* g, unsigned short* b,
                  unsigned short* a, unsigned long*  l, int n);
void rgbtocpack (unsigned short* r, unsigned short* g, unsigned short* b,
		 unsigned long* l, int n);
*/
GR_Tearth::GR_Tearth (char* filename, int component, int xsize, int ysize)
{
   static Boolean firstTearth_comp1 = TRUE;
   static Boolean firstTearth_comp4 = TRUE;
   int i, ret_code;
 
   if (component == 1)
     if (!firstTearth_comp1)
        return;
     else
  	firstTearth_comp1 = FALSE;

   if (component == 4)
     if (!firstTearth_comp4)
        return;
     else
        firstTearth_comp4 = FALSE;

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
      if (component == 1)
	p_filename = "/usr/esd/tung/earth/data/t5";
      else
	p_filename = "/usr/esd/tung/earth/jpl.rgb";
   }
   else
     p_filename = filename;

   p_dotiling = TRUE;
   p_type = 300;
   p_component = component;
   
   //p_workstate = 0;
 
   p_xsize = xsize;
   p_ysize = ysize;
   // for comp=3 or 4, real sizes may be determined by take_rgbdate 
   
   p_gr_objid = GR_genobj ();
   GR_makeobj (p_gr_objid);
   if (p_component == 1)
       GR_color (0, 10, 40);
   else
       GR_color (255,255,255);
   draw_sphere (18, TRUE, TRUE);
   GR_closeobj ();

   ret_code = texture_init ();

   p_tiled_gr_objid = GR_genobj ();
   GR_makeobj (p_tiled_gr_objid);
   if (ret_code > 0)   // i.e., texture_init() is successful;
   {
     for (i=1; i<=6; i++)   // change from 8 to 6
     {
       texbind (TX_TEXTURE_0, i);
       GR_callobj (p_gr_objid);
     }
   }   
   GR_closeobj ();
}

      
int
GR_Tearth::texture_init ()
{
   unsigned long *image;
   int i, tiles;
   float stx,sty,edx,edy,tilesizex, tilesizey;
   char msgstr[80];

   work_progress (0, "Texture Initialization");

   p_texps[0] = TX_MINFILTER;
   p_texps[1] = TX_BILINEAR;
   p_texps[2] = TX_MAGFILTER;
   p_texps[3] = TX_BILINEAR;
   p_texps[4] = TX_WRAP_S;
   p_texps[5] = TX_CLAMP;
   p_texps[6] = TX_WRAP_T;
   p_texps[7] = TX_CLAMP;
   p_texps[8] = TX_TILE;
   p_texps[9]  = 0.0;
   p_texps[10] = 0.0;
   p_texps[11] = .25;
   p_texps[12] = .25;
   p_texps[13] = TX_NULL;

   printf("\nReading image file.....\n");
   work_progress (1, "Reading image file");

   if (p_component == 1)
   {
      p_tevps[0] = TV_COLOR;
      p_tevps[1] = 0.5;
      p_tevps[2] = 0.11;
      p_tevps[3] = 0.0;
      p_tevps[4] = 1.0;
      p_tevps[5] = TV_BLEND;
      p_tevps[6] = TV_NULL;
      tevdef(1, 7, p_tevps);
      image = (unsigned long *) take_hdfdata (p_filename, p_xsize, p_ysize);
   }

   else if (p_component == 4)
   {
      p_tevps[0] = TV_NULL;
      tevdef(4, 1, p_tevps);
      image = (unsigned long *) take_rgbdata (p_filename);
   }      
  
   if (!image)
   {
      fprintf (stderr, "I cannot find the texture files.\n");
      work_progress (3, NULL); 
      return 0;
   }
 
   tilesizex = (p_xsize/2) - 1;
   tilesizey = (p_ysize/2) - 1;
   stx = 0.0;
   sty = 0.0;
   edx = tilesizex;
   edy = tilesizey;
   printf("\nDefining Earth Textures.....\n");
   work_progress (1, "Defining Earth Textures");

   tiles=1;
   for (i=1; i<=2; i++, tiles++)
   {
         sprintf (msgstr, "Doing tile #%d... out of 6", tiles);
         work_progress (1, msgstr);
         printf (" Doing tile #%d: stx, sty, edx, edy = %f %f %f %f ", tiles, stx, sty, edx, edy);
         p_texps[9]  = stx;
         p_texps[10] = sty;
         p_texps[11] = edx;
         p_texps[12] = edy;

         texdef2d (tiles,p_component,p_xsize,p_ysize,image,14,p_texps);
         stx = edx + 1.0;
         edx = stx + tilesizex;
         printf ("....done.\n");
   } 

   tilesizex = (int)(tilesizex/2) * 1.0;
   stx = 0.0;
   sty = tilesizey + 1.0;
   edx = tilesizex;
   edy = sty + tilesizey;
 
   for (i=1; i<=4; i++, tiles++)
   {
         sprintf (msgstr, "Doing tile #%d... out of 6", tiles);
         work_progress (1, msgstr);

	 printf (" Doing tile #%d: stx, sty, edx, edy = %f %f %f %f ", tiles, stx, sty, edx, edy);
	 p_texps[9]  = stx;
	 p_texps[10] = sty;
	 p_texps[11] = edx;
	 p_texps[12] = edy;
	 
	 texdef2d (tiles,p_component,p_xsize,p_ysize,image,14,p_texps);
	 stx = edx + 1.0;
	 edx = stx + tilesizex;
	 printf ("....done.\n");
   }
   
   //sprintf (msgstr, "Doing the untiled version");
   //work_progress (1, msgstr);
   //p_texps[6] = TX_NULL;
   //texdef2d(101,p_component,p_xsize,p_ysize,image,7,p_texps);

   work_progress (2, NULL);
   work_progress (3, NULL); // remove dialog shell;
 
   return 1;
}

   
void
GR_Tearth::draw_sphere (int n, Boolean nflag, Boolean tflag)
{
   int i, j, k;
   float phi, theta, theta_save;
   float vert[3];

   for (k=1, theta=-M_PI; k<=4*n; k++, theta+= M_PI_2/n)
   {
      GR_bgnqstrip ();
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
	      GR_n3f (vert);
	    GR_v3f (vert);
	 }
	 theta = theta_save;
      }
      GR_endqstrip ();
   }
}


// a partial sphere, or for now just a quadrant:   
void
GR_Tearth::draw_sphere (int n, Boolean nflag, Boolean tflag, int quadrant)
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
      GR_bgnqstrip ();
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
	      GR_n3f (vert);
	    GR_v3f (vert);
	 }
	 theta = theta_save;
      }
      GR_endqstrip ();
   }
}


unsigned long*
GR_Tearth::take_hdfdata (char* infile, int xsize, int ysize)
{
   FILE* file;
   int x, y;
   char *base;
   unsigned long* lptr;
   
   if ((file = fopen(infile,"r")) == NULL)
   {
      perror("fopen\007");
      return NULL;
   }
   
   base = (char*) malloc (xsize*ysize);
   if (!base)
   {
      perror("malloc\007");
      return NULL;
   }
   
   for (y=0; y<ysize; y++)
   {
      for (x=0; x<xsize; x++)
      {
	 base[x+(ysize-y-1)*xsize] = fgetc(file);
      }
   }
   
   lptr = (unsigned long*)base;
   return lptr;
}


unsigned long*
GR_Tearth::take_rgbdata (char* infile)
{
   IMAGE* image;
   int xsize, ysize;
   unsigned long *base, *lptr;
   unsigned short *rbuf, *gbuf, *bbuf, *abuf;
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

   base = (unsigned long*) malloc (xsize*ysize*sizeof(unsigned long));
   rbuf = (unsigned short*) malloc (xsize*sizeof(unsigned short));
   gbuf = (unsigned short*) malloc (xsize*sizeof(unsigned short));
   bbuf = (unsigned short*) malloc (xsize*sizeof(unsigned short));
   abuf = (unsigned short*) malloc (xsize*sizeof(unsigned short));

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
	 perror("wrong zsize");
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
GR_Tearth::objdraw ()
{
   if (p_component == 1)
      tevbind (TV_ENV0, 1);
   else if (p_component == 4)
      tevbind (TV_ENV0, 4);
   
   if (p_dotiling)
   {
      //printf ("...drawing tiled earth. \n");
      GR_callobj (p_tiled_gr_objid); 
   }
   else
   {
      //printf ("...drawing untiled earth. \n"); 
      //texbind (TX_TEXTURE_0, 101);
      //callobj (p_gr_objid);
   }

   tevbind (TV_ENV0, 0); // turn off texture;
}

/*
void
GR_Tearth::tiling_recover ()
{
   int i;

   texture_init ();
   p_tiled_gr_objid = GR_genobj ();
   GR_makeobj (p_tiled_gr_objid);
   for (i=1; i<=4; i++)
   {
       texbind (TX_TEXTURE_0, i);
       GR_callobj (p_gr_objid);
   }   
   GR_closeobj ();
}
*/

void
GR_Tearth::v_process_pick (GR_MouseEvent& event, GR_Window* window)
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
	/*
         sprintf (str, "Textured Earth, Type = %d", p_type);
	 xstr = XmStringCreateSimple (str);
	 XtSetArg (arg[0], XmNmessageString, xstr);
	 dialog = XmCreateMessageDialog (window->widget(), "message", arg, 1);
	 XmStringFree (xstr);
	 XtManageChild (dialog);
        */
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
