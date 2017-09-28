#include <math.h>
#include "GR_Bearth.H"

GR_Blueearth::GR_Blueearth (long type, int n)
{
  p_type = type;
  p_gr_objid = GR_genobj ();
  GR_makeobj (p_gr_objid);
  GR_color (4, 0, 40);
  draw_sphere (n, TRUE, FALSE);
  GR_closeobj ();
}

void
GR_Blueearth::objdraw()
{
   GR_backface (TRUE);
   GR_callobj (p_gr_objid);
   GR_backface (FALSE);
}

void
GR_Blueearth::draw_sphere (int n, Boolean nflag, Boolean tflag)
{
   int i, j, k;
   float phi, theta, theta_save;
   float vert[3];
   float factor = 0.9999;

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
               //vert[0] = ((float)theta/(2*M_PI) + 0.5);
               //ivert[1] = ((float)phi/M_PI + 0.5);
               //t2f (vert);
            }
            vert[0] = factor * cos(phi) * sin (theta);
            vert[1] = factor * sin(phi);
            vert[2] = factor * cos(phi) * cos (theta);

            if (nflag)
              GR_n3f (vert);
            GR_v3f (vert);
         }
         theta = theta_save;
      }
      GR_endqstrip ();
   }
}

