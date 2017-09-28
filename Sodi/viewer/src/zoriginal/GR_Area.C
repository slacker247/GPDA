#include <stdio.h>
#include <math.h>
#include <malloc.h>
#include "GR_Area.H"

GR_Area::GR_Area (char* rgbfile, char* elevdir, long type, int focus_lon, int focus_lat)
{
   IMAGE *rgbimage;
   int xsize, ysize, zsize;
   int rgb_xoffset, rgb_yoffset;
   int idre;
   char elevfstr[64], *elevfile;
   FILE *elevf;
   int elev_xoffset, elev_yoffset, elev_xblock, elev_yblock;
   
   unsigned short rbuf[4320], gbuf[4320], bbuf[4320];
   unsigned short rbufB[4320], gbufB[4320], bbufB[4320];
   int adj_lon, adj_lat;
   float lon, lat, alt, vec[3];
   float altfac = 0.00025; 
   float altbase = 0.00000; // maybe -0.00001;
   char elevbuf[120][120], tmpbuf[1080];  
   unsigned int i, j, row;
 
   // the rgbfile may be changed later, for now, use a test version
   //   izoomed and gammawarped (by 2.5) from World_2160x1080.rgb:
   if (rgbfile==NULL)
     rgbfile = "/usr/esd/tung/earth/data/TEST_4320x2160.rgb";
   if ((rgbimage = iopen (rgbfile,"r")) == NULL)
   {
      perror ("iopen_rgbfile");
      return;
   }
   xsize = rgbimage->xsize;
   ysize = rgbimage->ysize;
   zsize = rgbimage->zsize;
   if (zsize<3)
   {
      perror ("rgbfile_zsize");
      return;
   }
   if (xsize != 4320)
   {
      perror ("rgbfile_xsize");
      return;
   }
   if (ysize != 2160)
   {
      perror ("rgbfile_ysize");
      return;
   }
   adj_lon = (((focus_lon+180)/10)*10 - 180);          // -180,-170,..0,..,170
   adj_lat = ((((focus_lat+90)/10+1)%18)*10 - 90);     // -80,-70,..0,..,90
   
   rgb_xoffset = ((adj_lon+180)/10)%36*120;          // 0,120,...,4200
   rgb_yoffset = ((adj_lat+ 90)/10)%18*120;          // 0,120,...,2040
   printf ("rgb file is ready for processing.\n");
  
   elev_xoffset = ((adj_lon+180)/10)%9*120;          // 0,120,..,960
   elev_yoffset = (5 - ((adj_lat+ 90)/10)%6)*120;    // 600,..,120,0
   elev_xblock = ((adj_lon+180)/10)%36/9;            // 0, 1, 2 or 3
   elev_yblock = ((adj_lat+ 90)/10)%18/6;            // 0, 1 or 2
  
 
   if (elevdir==NULL)
     elevdir = "/usr/esd/tung/earth/data/";
   switch (elev_xblock)
   {
     case 0:
       if (elev_yblock == 0)      elevfile = "earth_120_180";
       else if (elev_yblock == 1) elevfile = "earth_060_180";
       else if (elev_yblock == 2) elevfile = "earth_000_180";
       break;
     case 1:
       if (elev_yblock == 0)      elevfile = "earth_120_270";
       else if (elev_yblock == 1) elevfile = "earth_060_270";
       else if (elev_yblock == 2) elevfile = "earth_000_270";
       break;
     case 2:
       if (elev_yblock == 0)      elevfile = "earth_120_000";
       else if (elev_yblock == 1) elevfile = "earth_060_000";
       else if (elev_yblock == 2) elevfile = "earth_000_000";
       break;
     case 3:
       if (elev_yblock == 0)      elevfile = "earth_120_090";
       else if (elev_yblock == 1) elevfile = "earth_060_090";
       else if (elev_yblock == 2) elevfile = "earth_000_090";
       break;
     default:
       break;
   }
   sprintf (elevfstr,  "%s%s", elevdir, elevfile);
   if ((elevf = fopen (elevfstr,"rb")) == NULL)
   {
      perror ("fopen_elevfile");
      return;
   }

   for (i=0; i<elev_yoffset; i++)
      fread (tmpbuf, 1080, 1, elevf);
   for (i=0; i<120; i++)
   {
      fread (tmpbuf, 1080, 1, elevf);
      memmove (elevbuf[i], &tmpbuf[elev_xoffset], 120); 
   }
   printf ("elevation file is ready for processing, elevbuf is filled.\n"); 

   printf ("Now process the 10 by 10 grid whose LU corner <LAT:%d,LON:%d>\n",
            adj_lat, adj_lon);

   p_type = type;
   p_gr_objid = GR_genobj ();
   GR_makeobj (p_gr_objid);
   {
      for (i=0, row=rgb_yoffset, lat=adj_lat*M_PI/180.0;
           i<120 - 1;
           i++, row--, lat-=M_PI/2160)
      {
          getrow (rgbimage, rbuf, row, 0);
          getrow (rgbimage, gbuf, row, 1);
          getrow (rgbimage, bbuf, row, 2);
          getrow (rgbimage, rbufB, row-1, 0);
          getrow (rgbimage, gbufB, row-1, 1);
          getrow (rgbimage, bbufB, row-1, 2);
          GR_bgnqstrip();
          for (j=0, lon=adj_lon*M_PI/180.0;
               j<120;
               j++, lon+=M_PI/2160) 
          {
             GR_color (rbuf[rgb_xoffset+j], gbuf[rgb_xoffset+j], bbuf[rgb_xoffset+j]);
	     idre = (int)elevbuf[i][j];
             if (idre > 145)
                 alt = (idre-145)*altfac + altbase;
             else
                 alt = altbase; 
             vec[0] = cos(lat)*sin(lon)*(1+alt);  
             vec[1] = sin(lat)*(1+alt);
             vec[2] = cos(lat)*cos(lon)*(1+alt);
             GR_v3f(vec);

             GR_color (rbufB[rgb_xoffset+j], gbufB[rgb_xoffset+j], bbufB[rgb_xoffset+j]);
             if (idre > 145)
                 alt = (idre-145)*altfac-0.0001;
             else
                 alt = -0.0001;
             vec[0] = cos(lat-M_PI/180.0)*sin(lon)*(1+alt);   
             vec[1] = sin(lat-M_PI/180.0)*(1+alt);
             vec[2] = cos(lat-M_PI/180.0)*cos(lon)*(1+alt);
             GR_v3f(vec);
           }
           GR_endqstrip();
       }
    }
    GR_closeobj();
    iclose (rgbimage);
    fclose (elevf);
}


void GR_Area::objdraw ()
{

   GR_callobj (p_gr_objid);
}
	
	     


