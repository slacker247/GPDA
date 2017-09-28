#include "GR_Interface.H"
#include "GR_Terrain.H"
#include "convert.H"
#include "def.H"
#include "GISP_Globals.H"
/*
#include <GL/gl.h>
#include <GL/glu.h>
*/
#include "dted.h"

float mat_AMBIENT[]       = { 0.3, 0.3, 0.3, 1.0 };
float mat_DIFFUSE[]       = { 0.8, 0.8, 0.8, 1.0 };
float mat_EMISSION[]      = { 0.3, 0.3, 0.3, 1.0 };
float mat_SPECULAR[]      = { 0.8, 0.8, 0.8, 1.0 };
float mat_SHININESS[]     = { 10.0 };
float dullmat_AMBIENT[]   = { 0.2, 0.2, 0.2, 1.0 };
float dullmat_DIFFUSE[]   = { 0.5, 0.5, 0.5, 1.0 };
float dullmat_EMISSION[]  = { 0.3, 0.3, 0.3, 1.0 };
float dullmat_SPECULAR[]  = { 0.4, 0.4, 0.4, 1.0 };
float dullmat_SHININESS[] = { 5.0 };
float sunlt_LCOLOR[]      = { 0.2, 0.8, 0.5, 0.0 };
float sunlt_POSITION[]    = { 1.0, -1.0, 1.0, 0.0 };
float sunlt_AMBIENT[]     = { 0.1, 0.1, 0.1, 1.0 };
float sunlt_DIFFUSE[]     = { 1.0, 1.0, 1.0, 1.0 };
float sunlt_SPECULAR[]    = { 1.0, 1.0, 1.0, 1.0 };
float light_POSITION[]    = {-1.0,-1.0, 2.0, 0.0 };
float moonlt_LCOLOR[]     = { 0.1, 0.1, 0.8, 0.0 };
float moonlt_POSITION[]   = {-1.0, 1.0, 2.0, 0.0 };
float nonelt_LCOLOR[]     = { 0.1, 0.1, 0.6, 0.0 };
float nonelt_POSITION[]   = { 0.0, 0.0, 1.0, 0.0 };
float lm_LMAMBIENT[]      = { 0.5, 0.5, 0.5, 1.0 };
float lm_LOCALVIEWER[]    = { 1.0 };
float dulllm_LMAMBIENT[]  = { 0.2, 0.2, 0.2, 1.0 };
float dulllm_LOCALVIEWER[] = { 1.0 };

GLUquadricObj *quadricTexObj;

void
GR_Terrain::TNorm(double v[3])
{
  double r;
  float  fr;

  fr = sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
  r = (double)fr;

  if(r < 0.00001) return;

  v[0] /= r;
  v[1] /= r;
  v[2] /= r;
}

void
GR_Terrain::TCross(double v1[3], double v2[3], double result[3])
{
  result[0] = v1[1] * v2[2] - v1[2] * v2[1];
  result[1] = v1[2] * v2[0] - v1[0] * v2[2];
  result[2] = v1[0] * v2[1] - v1[1] * v2[0];
}

GR_Terrain::GR_Terrain(long terid, float latitude, float longitude, char *terrafile)
{
char     chfile[64];
FILE     *fd;
int      idum, jdum, iflag = 1;
GLuint   i,j;

DTED     myDtedFile;
int      width, height;
float    swLat, swLon, neLat, neLon;

int      Ix, Iy, sep, numverts;
int      Zskip;

//double   x[500], y[500], z[500][500];
double   *x, *y, z[500][500];
double   v1[3], v2[3], v3[3], v4[3], v5[3], v6[3], v7[3];
double   Xmin,  Xmax,  Ymin,  Ymax, Zmin, Zmax, Zscale, Zbias;

float    lat, lon, alt;
float    x0, x1, y0, y1, z00, z01, z10, z11;
float    fw, fh;

C_CONVERT          conversion;

GLfloat light_ambient[]   = { 0.5, 0.5, 0.5, 1.0 };
GLfloat light_diffuse[]   = { 0.0, 1.0, 0.0, 1.0 };
GLfloat light_specular[]  = { 0.5, 0.5, 0.5, 1.0 };
GLfloat light_position[]  = { 1.0, -1.0, 1.0, 0.0 };

   p_terrain   = FALSE;
   p_type      = T_TERRAIN;
   p_xsize     = 0;
   p_ysize     = 0;
   p_lat       = latitude;
   p_lon       = longitude;
   p_orient    = 0.0;
   p_alt       = 6700.0;
   p_azi       = 0.0;
   p_tlid      = 0;

   p_ulen   = 200.0/RE;
   p_vlen   = 200.0/RE;

   p_base[0] = -p_ulen/2.0;
   p_base[1] = -p_ulen/2.0;

   p_lupper[0] = -p_ulen/2.0;
   p_lupper[1] = p_vlen/2.0;

   p_rupper[0] = p_ulen/2.0;
   p_rupper[1] = p_vlen/2.0;

   p_rlower[0] = p_ulen/2.0;
   p_rlower[1] = -p_vlen/2.0;
   
   myDtedFile  = dted_open(terrafile);

   dted_get_file_size(myDtedFile, &width, &height);
   dted_get_file_coverage(myDtedFile, &swLat, &swLon, &neLat, &neLon);

   Zmin   = (double)dted_get_min_elev(myDtedFile);
   Zmax   = (double)dted_get_max_elev(myDtedFile);
   Zbias  = Zmin;
   Zscale = 2.0;
   Zskip  = 3;
   Ix     = (width/Zskip)-1;
   Iy     = (height/Zskip)-1;
   fw     = (neLon-swLon) * ((float)Zskip/(float)width);
   fh     = (neLat-swLat) * ((float)Zskip/(float)height);

   x = new double[Ix];
   y = new double[Iy];

   for (i=0; i<Ix; i++) { 
      x[i] = ((double)swLon + (double)i*(double)fw)*M_PI/180.0;
      for (j=0; j<Iy; j++){
	 y[j] = ((double)swLat + (double)j*(double)fh)*M_PI/180.0;
         z[i][j] = (((double)dted_get_elev(myDtedFile, Zskip*j, Zskip*i)-Zbias)*Zscale)/1000.0;
	 if (i == j) printf("Tearth:terrain %f %f %f\n", x[i], y[j], z[i][j]);
      }
   }
   /*
   for (i=0; i<Ix; i++)
       for (j=0; j<Iy; j++) {
          x[i] = (float)(i+1)/RE;
          y[j] = (float)(j+1)/RE;
          z[i][j] = ((((float)dted_get_elev(myDtedFile, Zskip*j, Zskip*i)-Zbias)*Zscale))/RE;
       }
   */
   dted_close();

   printf("\n");
   printf("Plot type is .................... %s\n", "3D DTED");
   printf("Input data file is .............. %s\n", "n31.dte");
   printf("Number of columns is ............ %d (%d)\n", width, Ix);
   printf("Number of rows is ............... %d (%d)\n", height, Iy);
   printf("Minimum elevation is ............ %f\n", Zmin);
   printf("Maximum elevation is ............ %f\n", Zmax);
   printf("SW Corner Lat/Lon is ............ %f %f\n", swLat, swLon);
   printf("NE Corner Lat/Lon is ............ %f %f\n", neLat, neLon);

   if (p_tlid != 0) glDeleteLists(p_tlid, 1);
   p_tlid = glGenLists(1);
   p_terrain = TRUE;
/*
 *   Generate 3D digital terrain map display list
 */
   glLightfv(GL_LIGHT2, GL_AMBIENT,  light_ambient);
   glLightfv(GL_LIGHT2, GL_DIFFUSE,  light_diffuse);
   glLightfv(GL_LIGHT2, GL_SPECULAR, light_specular);
   glLightfv(GL_LIGHT2, GL_POSITION, light_position);

   glNewList(p_tlid, GL_COMPILE);
      GR_pushattributes();
      GR_pushmatrix();

      glRotatef(p_lon, 0.0, 1.0, 0.0);           // Rotate to correct longitude from Greenwich
      glRotatef(-p_lat, 1.0, 0.0, 0.0);          // Rotate to correct latitude from Equator
      glRotatef(180.0-p_orient, 0.0, 0.0, 1.0);  // Rotate to correct orientation from North
      glTranslatef(0.0, 0.0, 1.0);               // Move to correct location on surface
      //glScalef(0.1, 0.1, 0.1);
  
      GR_linewidth (2*GR_getlwidth());
      GR_color (255, 0, 0);
      draw_box();

      for (i=0; i<Ix-1; i++) {
         for (j=0; j<Iy-1; j++) {
            conversion.lla_to_xyz(y[j],   x[i],   z[i][j],     v1);
	    conversion.lla_to_xyz(y[j],   x[i+1], z[i+1][j],   v2);
	    conversion.lla_to_xyz(y[j+1], x[i+1], z[i+1][j+1], v3);
	    conversion.lla_to_xyz(y[j+1], x[i],   z[i][j+1],   v4);

            v5[1] = v1[1]-v2[1]; v5[2] = v1[2]-v2[2]; v5[0] = v1[0]-v2[0];
            v6[1] = v2[1]-v3[1]; v6[2] = v2[2]-v3[2]; v6[0] = v2[0]-v3[0];
            TCross(v5, v6, v7);
	    TNorm(v7);

	    //glColor3f(0.0, 1.0, 0.0);
	    //if (z[i][j] < 0.05*(Zmax-Zbias)) glColor3f(0.0, 0.0, 0.7);
	    //if (z[i][j] > 0.90*(Zmax-Zbias)) glColor3f(1.0, 1.0, 1.0);
            //if (z[i][j] > 0.90*(Zmax-Zbias)) glColor3f(1.0, 1.0, 1.0);

            glBegin(GL_POLYGON);
	      glNormal3d(v6[0], v6[1], v6[2]);
              glVertex3dv(v1);                         // SW
              glVertex3dv(v2);                         // SE
              glVertex3dv(v3);                         // NE
              glVertex3dv(v4);                         // NW
            glEnd();
         }
      }
    /*
    for (i=0; i<Ix-1; i++)
       for (j=0; j<Iy-1; j++) {
          v1[0] = x[i];        v1[1] = y[j];        v1[2] = z[i][j];
          v2[0] = x[i+1];      v2[1] = y[j];        v2[2] = z[i+1][j];
          v3[0] = x[i];        v3[1] = y[j+1];      v3[2] = z[i][j+1];
          v4[0] = v1[0]-v2[0]; v4[1] = v1[1]-v2[1]; v4[2] = v1[2]-v2[2];
          v5[0] = v2[0]-v3[0]; v5[1] = v2[1]-v3[1]; v5[2] = v2[2]-v3[2];
          TCross(v4, v5, v6);
          TNorm(v6);
          //glColor3f(0.0, 1.0, 0.0);
          //if (z[i][j] < 0.05*(Zmax-Zbias)) glColor3f(0.0, 0.0, 0.7);
          //if (z[i][j] > 0.90*(Zmax-Zbias)) glColor3f(1.0, 1.0, 1.0);
          //if (z[i][j] > 0.90*(Zmax-Zbias)) glColor3f(1.0, 1.0, 1.0);
          glBegin(GL_POLYGON);
            glNormal3f(v6[0],  v6[1],  v6[2]);
            glVertex3f(x[i],   y[j],   z[i][j]);         // SW
            glVertex3f(x[i+1], y[j],   z[i+1][j]);       // SE
            glVertex3f(x[i+1], y[j+1], z[i+1][j+1]);     // NE
            glVertex3f(x[i],   y[j+1], z[i][j+1]);       // NW
          glEnd();
       }
   */
   printf("Terrain display list generated\n");

   GR_color(255, 0, 0);
   glBegin(GL_LINES);
     glVertex3f(-2.0*RE, 0.0, 0.0);
     glVertex3f(2.0*RE, 0.0, 0.0);
   glEnd();

   GR_color(0, 255, 0);
   glBegin(GL_LINES);
     glVertex3f(0.0, -2.0*RE, 0.0);
     glVertex3f(0.0, 2.0*RE, 0.0);
   glEnd();

   GR_color(0, 0, 255);
   glBegin(GL_LINES);
     glVertex3f(0.0, 0.0, -2.0*RE);
     glVertex3f(0.0, 0.0, 2.0*RE);
   glEnd();

      GR_popmatrix();
      GR_popattributes();
   glEndList();  
}

void
GR_Terrain::draw_box ()
{  
   GR_bgnpolygon();
     GR_v2f(p_base);
     GR_v2f(p_lupper);
     GR_v2f(p_rupper);
     GR_v2f(p_rlower);
   GR_endline();
}

void
GR_Terrain::objdraw ()
{
    //glLightfv(GL_LIGHT2,  GL_POSITION, sunlt_POSITION);
    //glLightfv(GL_LIGHT2,  GL_DIFFUSE,  sunlt_LCOLOR);
    //glLightModelfv(GL_LIGHT_MODEL_AMBIENT, lm_LMAMBIENT);
    //glLightModelfv(GL_LIGHT_MODEL_LOCAL_VIEWER, lm_LOCALVIEWER);

    glShadeModel(GL_SMOOTH);
    glEnable(GL_LIGHT0);
    glEnable(GL_LIGHT2);
    glEnable(GL_LIGHTING);
    //glEnable(GL_COLOR_MATERIAL);
    glDepthFunc(GL_LESS);
    glEnable(GL_DEPTH_TEST);
    /*
     *   Render the terrain
     */
    if (p_terrain) {
      //printf("Tearth: drawing terrain\n");
       glColor3f(0.0, 1.0, 0.0);
       glPolygonOffset(0.1, 0.1);
       glEnable(GL_POLYGON_OFFSET_FILL);
       glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
       glPushMatrix();
         glCallList(p_tlid);
       glPopMatrix();
    }
}

