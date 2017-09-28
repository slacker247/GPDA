#include <math.h>
#include "GR_Bearth.H"

GLUquadricObj *quadObj;

GR_Blueearth::GR_Blueearth (long type, int n)
{
static float mat_AMBIENT[]      = { 0.3, 0.3, 0.3, 1.0 };
static float mat_DIFFUSE[]      = { 0.8, 0.8, 0.8, 1.0 };
static float mat_EMISSION[]     = { 0.3, 0.3, 0.3, 1.0 };
static float mat_SPECULAR[]     = { 0.8, 0.8, 0.8, 1.0 };
static float mat_SHININESS[]    = { 10.0 };

   p_type = type;
   p_gr_objid = GR_genobj ();
   GR_makeobj(p_gr_objid);

   quadObj = gluNewQuadric();
   gluQuadricDrawStyle(quadObj, (GLenum) GLU_FILL);
   gluQuadricOrientation(quadObj, (GLenum) GLU_OUTSIDE);
   gluQuadricNormals(quadObj, (GLenum) GLU_SMOOTH);
   glEnable(GL_LIGHTING);
       glMaterialfv(GL_FRONT, GL_AMBIENT, mat_AMBIENT);
       glMaterialfv(GL_FRONT, GL_DIFFUSE, mat_DIFFUSE);
       glMaterialfv(GL_FRONT, GL_EMISSION, mat_EMISSION);
       glMaterialfv(GL_FRONT, GL_SPECULAR, mat_SPECULAR);
       glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, mat_SHININESS);
   GR_color (255, 0, 0);
   //gluSphere(quadObj, 1.0, 32, 32);
   draw_sphere (n, TRUE, FALSE);
   gluDeleteQuadric(quadObj);
   GR_closeobj ();

   glDisable(GL_LIGHTING);
}

void
GR_Blueearth::objdraw()
{
   glEnable(GL_LIGHTING);
   GR_backface (TRUE);
   glEnable(GL_POLYGON_OFFSET_FILL);
   glPolygonOffset(1.0, 1.0);

   GR_callobj (p_gr_objid);

   glDisable(GL_POLYGON_OFFSET_FILL);
   GR_backface (FALSE);
   glDisable(GL_LIGHTING);
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

