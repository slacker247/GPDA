#include <stdio.h>
#include <math.h>
#include <malloc.h>
#include "GR_Eearth.H"

void
buf2sbuf (char* inbuf, cpoint* outsbuf, float lat, float altfac)
{
   int i, elev;
   float lon, alt;

   for (i=0, lon=0.0; i<540; i++, lon+=M_PI/270.0)
   {
      elev = inbuf[i*8];
     
      if (elev > 145)
      { 
         if (elev > 148)
         {
            outsbuf[i].r =  25 + (short)floor ((255-elev)*0.7);
            outsbuf[i].g =   0 + (short)floor ((255-elev)*0.6); 
            outsbuf[i].b =   0;
         }
         else if (elev > 145)
         {
            outsbuf[i].r = 115 + (short)floor ((148-elev)*3.0);
            outsbuf[i].g =  85 + (short)floor ((148-elev)*2.0);
            outsbuf[i].b =   3;
         }
         alt = (elev - 145) * altfac;
      }
      else
      {
         outsbuf[i].r =  1; 
         outsbuf[i].g =  1; 
         outsbuf[i].b = 32 + elev; 
         alt = 0; 
      }
      outsbuf[i].vec[0] = cos(lat)*sin(lon)*(1+alt);
      outsbuf[i].vec[1] = sin(lat)*(1+alt);
      outsbuf[i].vec[2] = cos(lat)*cos(lon)*(1+alt);
    }
}



GR_Eearth::GR_Eearth (char* dirname, long type)
{
   char fstr1[64], fstr2[64], fstr3[64], fstr4[64];
   char fstr5[64], fstr6[64], fstr7[64], fstr8[64];
   char fstr9[64], fstr10[64], fstr11[64], fstr12[64];
   FILE *f1, *f2, *f3, *f4;
   float lat;
   float altfac = 0.00075; // to pass to buf2sbuf;
   char bufA[4320], bufB[4320], nullbuf[1080];  
   struct cpoint sbuf1[540], sbuf2[540];
   struct cpoint *sbufA, *sbufB, *sbuftmp;
   unsigned int i, j, k, jj;

   sbufA = sbuf1;
   sbufB = sbuf2;

   if (dirname == NULL)
     dirname = "/usr/esd/tung/earth/data/";

   sprintf (fstr1,  "%s%s", dirname, "earth_000_000");
   sprintf (fstr2,  "%s%s", dirname, "earth_000_090");
   sprintf (fstr3,  "%s%s", dirname, "earth_000_180");
   sprintf (fstr4,  "%s%s", dirname, "earth_000_270");
   sprintf (fstr5,  "%s%s", dirname, "earth_060_000");
   sprintf (fstr6,  "%s%s", dirname, "earth_060_090");
   sprintf (fstr7,  "%s%s", dirname, "earth_060_180");
   sprintf (fstr8,  "%s%s", dirname, "earth_060_270");
   sprintf (fstr9,  "%s%s", dirname, "earth_120_000");
   sprintf (fstr10, "%s%s", dirname, "earth_120_090");
   sprintf (fstr11, "%s%s", dirname, "earth_120_180");
   sprintf (fstr12, "%s%s", dirname, "earth_120_270");

   p_type = type;
 
   p_gr_objid = GR_genobj ();
   GR_makeobj (p_gr_objid);
   for (k=1; k<=3; k++) // meant to be 1 now but 3 later;
   {
     switch (k)
     {
       case 1:
          f1 = fopen (fstr1, "rb");
          f2 = fopen (fstr2, "rb");
          f3 = fopen (fstr3, "rb");
          f4 = fopen (fstr4, "rb");
          printf ("... start reading the first one_third...\n");
          fread (&bufA[0],    1080, 1, f1);
          fread (&bufA[1080], 1080, 1, f2);
          fread (&bufA[2160], 1080, 1, f3);
          fread (&bufA[3240], 1080, 1, f4);
          lat = M_PI_2;
          buf2sbuf (bufA, sbufA, lat, altfac);
          lat -= M_PI/270.0;
          break;

       case 2:
          f1 = fopen (fstr5, "rb");
          f2 = fopen (fstr6, "rb");
          f3 = fopen (fstr7, "rb");
          f4 = fopen (fstr8, "rb");
          printf ("... start reading the second one_third...\n");
          lat = M_PI/6;
          break;

       case 3:
          f1 = fopen ( fstr9, "rb");
          f2 = fopen (fstr10, "rb");
          f3 = fopen (fstr11, "rb");
          f4 = fopen (fstr12, "rb");
          printf ("... start reading the third one_third...\n");
          lat = - M_PI/6;
          break;

      }
    
      for (j=0; j<89; j++, lat -= M_PI/270.0) 
      {
          for (jj=0; jj<7; jj++)
          {
             fread (nullbuf, 1080, 1, f1);
             fread (nullbuf, 1080, 1, f2);
             fread (nullbuf, 1080, 1, f3);
             fread (nullbuf, 1080, 1, f4);
          }

          fread (&bufB[0],    1080, 1, f1);
          fread (&bufB[1080], 1080, 1, f2);
          fread (&bufB[2160], 1080, 1, f3);
          fread (&bufB[3240], 1080, 1, f4);
          buf2sbuf (bufB, sbufB, lat, altfac);
          GR_bgnqstrip ();
          {
             for (i=0; i<540; i++)
             {
                GR_color (sbufA[i].r, sbufA[i].g, sbufA[i].b);
                //n3f (sbufA[i].vec);
                GR_v3f (sbufA[i].vec);

                GR_color (sbufB[i].r, sbufB[i].g, sbufB[i].b);
                //n3f (sbufB[i].vec);
                GR_v3f (sbufB[i].vec);
              }
              GR_color (sbufA[0].r, sbufA[0].g, sbufA[0].b);
              //n3f (sbufA[i].vec);
              GR_v3f (sbufA[0].vec);

              GR_color (sbufB[0].r, sbufB[0].g, sbufB[0].b);
              //n3f (sbufB[0].vec);
              GR_v3f (sbufB[0].vec);
          }
          GR_endqstrip ();
          sbuftmp = sbufA;
          sbufA = sbufB;
          sbufB = sbuftmp;
          if ((j % 10) == 0)
            printf ("   finished line %d, lat=%f\n", j, lat);
       }
       fclose (f1);
       fclose (f2);
       fclose (f3);
       fclose (f4);
   }
   GR_closeobj ();
}


void GR_Eearth::objdraw ()
{

   GR_callobj (p_gr_objid);
}
	
	     


