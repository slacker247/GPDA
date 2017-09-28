
#include "Tearth.H"
#include "dted.h"

#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>
#include "GL/gltk.h"
#include "GL/glsgi.h"

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

GLUquadricObj *quadTexObj;

void
TNorm(float v[3])
{
  float r;

  r = sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);

  v[0] /= r;
  v[1] /= r;
  v[2] /= r;
}

void
TCross(float v1[3], float v2[3], float result[3])
{
  result[0] = v1[1] * v2[2] - v1[2] * v2[1];
  result[1] = v1[2] * v2[0] - v1[0] * v2[2];
  result[2] = v1[0] * v2[1] - v1[1] * v2[0];
}

unsigned char *
AlphaPadImage(int bufSize, unsigned char *inData, int alpha)
{
    unsigned char *outData, *out_ptr, *in_ptr;
    int i;

    outData = (unsigned char *) malloc(bufSize * 4);
    out_ptr = outData;
    in_ptr = inData;

    for (i = 0; i < bufSize; i++) {
        *out_ptr++ = *in_ptr++;
        *out_ptr++ = *in_ptr++;
        *out_ptr++ = *in_ptr++;
        *out_ptr++ = alpha;
    }

    free (inData);
    return outData;
}

Tearth::Tearth()
{
   p_texturing = FALSE;
   p_terrain   = FALSE;
   p_component = 4;
   p_type      = 0;
   p_xsize     = 0;
   p_ysize     = 0;
   p_fov       = 62.0;
   p_aspect    = 1.0;
   p_near      = 0.1;
   p_far       = 100.0;
   p_lat       = 37.0;
   p_lon       = -102.0;
   p_alt       = 6700.0;
   p_azi       = 0.0;
   p_twist     = 0.0;
   p_lookx     = 0.0;
   p_looky     = 0.0;
   p_lookz     = 0.0;
   p_slid      = 0;
   p_glid      = 0;
   p_plid      = 0;
   p_clid      = 0;
   p_tlid      = 0;
   image       = (TK_RGBImageRec *)NULL;
}

void
Tearth::globe(char* filename, int component, int xsize, int ysize, int type)
{
static Boolean firstTearth_comp1 = TRUE;
static Boolean firstTearth_comp4 = TRUE;
int            i, ret_code;

GLUquadricObj *quadObj;
   
   if (!filename)
     p_filename = "/usr/esd/tung/earth/jpl.rgb";
   else
     p_filename = filename;

   p_dotiling = TRUE;
   p_type = type;
   p_component = component;
 
   p_xsize = xsize;
   p_ysize = ysize;

   glPushAttrib(GL_ALL_ATTRIB_BITS);

   glLightfv(GL_LIGHT0,  GL_POSITION, sunlt_POSITION);
   glLightfv(GL_LIGHT0,  GL_DIFFUSE,  sunlt_LCOLOR);
   glLightModelfv(GL_LIGHT_MODEL_AMBIENT, lm_LMAMBIENT);
   glLightModelfv(GL_LIGHT_MODEL_LOCAL_VIEWER, lm_LOCALVIEWER);
   glShadeModel(GL_SMOOTH);

   glEnable(GL_LIGHT0);
   glEnable(GL_LIGHTING);
   glEnable(GL_COLOR_MATERIAL);
   glDepthFunc(GL_LESS);
   glEnable(GL_DEPTH_TEST);

   if (p_slid != 0) glDeleteLists(p_slid,1);
   p_slid = glGenLists(1);
   glNewList(p_slid, GL_COMPILE);

   if (p_texturing) ret_code = texture_init ();

   glPushMatrix();
   quadObj = gluNewQuadric();
   gluQuadricDrawStyle(quadObj, (GLenum)GLU_FILL);
   gluQuadricOrientation(quadObj, (GLenum)GLU_OUTSIDE);
   gluQuadricNormals(quadObj, (GLenum)GLU_SMOOTH);
   if (p_texturing) {
      gluQuadricTexture(quadObj, GL_TRUE);
      glRotatef(-90.0, 1.0, 0.0, 0.0);
      glColor3f(1.0, 1.0, 1.0);
   } else {
      glEnable(GL_LIGHTING);
      glMaterialfv(GL_FRONT, GL_AMBIENT, mat_AMBIENT);
      glMaterialfv(GL_FRONT, GL_DIFFUSE, mat_DIFFUSE);
      glMaterialfv(GL_FRONT, GL_EMISSION, mat_EMISSION);
      glMaterialfv(GL_FRONT, GL_SPECULAR, mat_SPECULAR);
      glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, mat_SHININESS);
      glColor3f(0.0, 0.0, 0.95);
   }
   gluSphere(quadObj, 1.0, 32, 32);
   if (p_texturing) glDisable(GL_TEXTURE_2D);
   gluDeleteQuadric(quadObj);
   glEndList();
   glPopMatrix();

   glPopAttrib();
}

void 
Tearth::set_vparams(int vmode, int lat, int lon, int alt, int fov, int azi)           
{
float    vx, vy, vz;
float    lx, ly, lz;
float    Kt = 2.50;                              // tangential view constant;
   
   p_lat    = lat*M_PI/180.0;
   p_lon    = lon*M_PI/180.0;
   p_alt    = 1 + alt/RE;
   p_azi    = azi*M_PI/180.0;
   p_fov    = fov;
   p_aspect = 1.0;
   p_near   = 0.1;
   p_far    = 100.0;
   p_twist  = 0.0;
   p_lookx  = 0.0;
   p_looky  = 0.0;
   p_lookz  = 0.0;
   
   if (vmode == 0)                               // normal view:
   {
      vx = cos (p_lat) * sin (p_lon) * p_alt;
      vy = sin (p_lat) * p_alt;
      vz = cos (p_lat) * cos (p_lon) * p_alt;
      lx = p_lookx;
      ly = p_looky;
      lz = p_lookz;
   } else {                                      // tangential view:
      p_alt = 2 - exp (-alt/RE);

      // for now fix the alt:

      vx = -Kt * sin (p_azi) * cos (p_lon)
           + Kt * cos (p_azi) * sin (p_lat) * sin (p_lon)
           + cos (p_lat) * sin (p_lon);
      vy = -Kt * cos(p_azi) * cos(p_lat)
           + sin (p_lat);
      vz = Kt * sin (p_azi) * sin (p_lon)
           + Kt * cos (p_azi) * sin (p_lat) * cos (p_lon)
           + cos (p_lat) * cos (p_lon);
      
      lx = cos (p_lat) * sin (p_lon);
      ly = sin (p_lat);
      lz = cos (p_lat) * cos (p_lon);
      if (p_lat < 0)
        p_twist = 180.0;      
   }
   //fprintf(stderr, "Viewing parameters are %f %f %f %f %f %f %f %f %f %f %f\n",
   //     p_fov, p_aspect, p_near, p_far, p_twist, vx, vy, vz, lx, ly, lz);
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   gluPerspective((GLdouble)p_fov, (GLdouble)p_aspect, (GLdouble)p_near, (GLdouble)p_far);
   gluLookAt (
        (GLdouble)vx, (GLdouble)vy,  (GLdouble)vz,
        (GLdouble)lx, (GLdouble)ly,  (GLdouble)lz,
        (GLdouble)sin(p_twist*M_PI/180.0),
        (GLdouble)cos(p_twist*M_PI/180.0),
        (GLdouble)0.0);
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();
}

void
Tearth::coasts(char *shorefile, int type)
{
   if (p_clid != 0) glDeleteLists(p_clid,1);
   p_clid = glGenLists(1);
   glNewList(p_clid, GL_COMPILE);
   earthlines(255, 255, 255, shorefile, p_clid);
}

void
Tearth::political(char *polifile, int type)
{
   if (p_plid != 0) glDeleteLists(p_plid,1);
   p_plid = glGenLists(1);
   glNewList(p_plid, GL_COMPILE);
   earthlines(255, 255, 255, polifile,  p_plid);
}

void
Tearth::terrain(char *terrafile, int type)
{
char     chfile[64];
FILE     *fd;
int      idum, jdum, iflag = 1;
GLuint   i,j;
float    v1[3], v2[3], v3[3], v4[3], v5[3], v6[3], v7[3];
float    vert[3];
float    longitude, latitude;
DTED     myDtedFile;
int      width, height;
float    swLat, swLon, neLat, neLon;
float    x[500], y[500], z[500][500];
float    Xmin,  Xmax,  Ymin,  Ymax, Zmin, Zmax, Zscale, Zbias;
int      Ix, Iy, sep, numverts;
int      Zskip;
float    lat, lon, alt;
float    x0, x1, y0, y1, z00, z01, z10, z11;
float    fw, fh;
GLfloat light_ambient[]   = { 0.5, 0.5, 0.5, 1.0 };
GLfloat light_diffuse[]   = { 0.0, 1.0, 0.0, 1.0 };
GLfloat light_specular[]  = { 0.5, 0.5, 0.5, 1.0 };
GLfloat light_position[]  = { 1.0, -1.0, 1.0, 0.0 };

   if (p_tlid != 0) glDeleteLists(p_tlid,1);
   p_tlid = glGenLists(1);
   p_terrain = TRUE;
   
   myDtedFile = dted_open(terrafile);

   dted_get_file_size(myDtedFile, &width, &height);
   dted_get_file_coverage(myDtedFile, &swLat, &swLon, &neLat, &neLon);
   /*
   Zmin   = (float)dted_get_min_elev(myDtedFile);
   Zmax   = (float)dted_get_max_elev(myDtedFile);
   Zbias  = 95.0;
   Zscale = 0.2;
   Zskip  = 3;
   Ix     = (width/Zskip)-1;
   Iy     = (height/Zskip)-1;
   fw     = (float)Zskip/(float)width;
   fh     = (float)Zskip/(float)height;
 
   for (i=0; i<Ix; i++) {
      x[i] = (swLon + (float)i*fw)*M_PI/180.0;
      for (j=0; j<Iy; j++){
	 y[j] = (swLat + (float)j*fh)*M_PI/180.0;
         z[i][j] = 1.0 + (((float)dted_get_elev(myDtedFile, Zskip*j, Zskip*i)-Zbias)*Zscale)/RE;
	 //printf("Tearth:terrain %f %f %f\n", x[i], y[j], z[i][j]);
      }
   }
   */
   dted_close();

   latitude = swLat + (neLat - swLat)/2.0;
   longitude = swLon + (neLon - swLon)/2.0;
   lat = latitude*M_PI/180;
   lon = longitude*M_PI/180;
   vert[0] = cos(lat) * sin(lon);
   vert[1] = sin(lat);
   vert[2] = cos(lat) * cos(lon);
/*
 *   Generate 3D digital terrain map display list
 */
   glLightfv(GL_LIGHT0, GL_AMBIENT,  light_ambient);
   glLightfv(GL_LIGHT0, GL_DIFFUSE,  light_diffuse);
   glLightfv(GL_LIGHT0, GL_SPECULAR, light_specular);
   glLightfv(GL_LIGHT0, GL_POSITION, light_position);

   glNewList(p_tlid, GL_COMPILE);

   glTranslatef(vert[0], vert[1], vert[2]);
   glRotatef(-latitude,  0.0, 0.0, 1.0);
   glRotatef(longitude, 0.0, 1.0, 0.0);
   glScalef(0.01, 0.01, 0.01);

   /*
   for (i=0; i<Ix-1; i++) {
      for (j=0; j<Iy-1; j++) {
         //y[j] = sin(lat)*alt;
         //z[i][j] = cos(lat)*cos(lon)*alt;
         //x[i] = cos(lat)*sin(lon)*alt;
	 v1[1] = cos(y[j])*sin(x[i])*z[i][j];
         v1[2] = sin(y[j])*z[i][j];
         v1[0] = cos(x[i])*cos(y[j])*z[i][j];
	 //printf("Tearth: %f %f %f\n", v1[1], v1[2], v1[0]); 
         v2[1] = cos(y[j])*sin(x[i+1])*z[i+1][j];
         v2[2] = sin(y[j])*z[i+1][j];
         v2[0] = cos(x[i+1])*cos(y[j])*z[i+1][j];

         v3[1] = cos(y[j+1])*sin(x[i])*z[i][j+1];
         v3[2] = sin(y[j+1])*z[i][j+1];
         v3[0] = cos(x[i])*cos(y[j+1])*z[i][j+1];

         v4[1] = cos(y[j+1])*sin(x[i+1])*z[i+1][j+1];
         v4[2] = sin(y[j+1])*z[i+1][j+1];
         v4[0] = cos(x[i+1])*cos(y[j+1])*z[i+1][j+1];

         v5[1] = v1[1]-v2[1]; v5[2] = v1[2]-v2[2]; v5[0] = v1[0]-v2[0];
         v6[1] = v2[1]-v3[1]; v6[2] = v2[2]-v3[2]; v6[0] = v2[0]-v3[0];
         TCross(v5, v6, v7);
	 TNorm(v7);
	 //glColor3f(0.0, 1.0, 0.0);
	 //if (z[i][j] < 0.05*(Zmax-Zbias)) glColor3f(0.0, 0.0, 0.7);
	 //if (z[i][j] > 0.90*(Zmax-Zbias)) glColor3f(1.0, 1.0, 1.0);
         //if (z[i][j] > 0.90*(Zmax-Zbias)) glColor3f(1.0, 1.0, 1.0);
         glBegin(GL_POLYGON);
	   glNormal3f(v6[0], v6[1], v6[2]);
           glVertex3fv(v1);                         // SW
           glVertex3fv(v2);                         // SE
           glVertex3fv(v4);                         // NE
           glVertex3fv(v3);                         // NW
         glEnd();
      }
   }
   */
   glCallList(type);
   glEndList();   
}

void 
Tearth::gridlines(int R, int G, int B, int degree, int type)
{
int     i, j;
int     gridnumber;
float   theta, phi;
float   rad = 1.005;
float   v[3];
float   Red, Green, Blue;
GLUquadricObj* circleobj;

    if (degree < 5)
      degree = 5;
    else if (degree > 90)
      degree = 90;
    gridnumber = 180/degree;
    theta = degree * M_PI / 180;

    Red   = R/255.0;
    Green = G/255.0;
    Blue  = B/255.0;

   if (p_glid != 0) glDeleteLists(p_glid,1);
   p_glid = glGenLists(1);
   glNewList(p_glid, GL_COMPILE);

    glColor3f(Red, Green, Blue);
    glColor3f(0.5, 0.5, 0.0);

    for (i=0; i<gridnumber; i++)
    {
       glPushMatrix();
       glRotatef(float(i*degree), 0.0, 1.0, 0.0);
       circleobj = gluNewQuadric();
       //glTranslatef(x, y, 0.0);
       gluQuadricDrawStyle(circleobj, (GLenum)GLU_SILHOUETTE);
       gluDisk(circleobj, 0.0, (GLdouble)rad, (GLint)180, (GLint)1);
       glPopMatrix();
    }

    glPushMatrix();
    for (i=0; i<(gridnumber/2); i++)
    {
       glBegin(GL_LINE_STRIP);
       for (j=0, phi=0.0; j<=36; j++, phi+=M_PI/18)
       {
          v[0]=cos(theta*i)*cos(phi)*rad;
          v[1]=sin(theta*i)*rad;
          v[2]=cos(theta*i)*sin(phi)*rad;
          glVertex3f(v[0], v[1], v[2]);
       }
       glEnd();
       glBegin(GL_LINE_STRIP);
       for (j=0, phi=0.0; j<=36; j++, phi+=M_PI/18)
       {
          v[0]=cos(theta*i)*cos(phi)*rad;
          v[1]=-sin(theta*i)*rad;
          v[2]=cos(theta*i)*sin(phi)*rad;
          glVertex3f(v[0], v[1], v[2]);
       }
       glEnd();
    }
    glPopMatrix();
    glEndList();
}

void 
Tearth::earthlines(int R, int G, int B, char* filename, int type)
{
float   vec[3], lon, lat, old_lon, old_lat, dist_sqr, extra_lon, extra_lat;
float   Red, Green, Blue;
float   factor = 1.0015; // to compensate for normal lines to show
//float factor2 = 1.015;  // to compensate for long lines to show
float   threshold = 0.015;
int     num_extra_pts;
long    i,j,k;
FILE    *fileptr;
long    count, totalcount;
int     thiscount, lineno;
char    str[80];

    Red   = R/255.0;
    Green = G/255.0;
    Blue  = B/255.0;

    glNewList(type, GL_COMPILE);

    glColor3f(Red, Green, Blue);
    fileptr = fopen (filename, "r+");
    fscanf(fileptr, "%d", &count);
    for (i=0; i<count; i++)
    {
       fscanf(fileptr, "%d%d", &lineno, &thiscount);
       glBegin(GL_LINE_STRIP);
       for (j=0; j<thiscount; j++)
       {
          fscanf(fileptr, "%f%f", &lon, &lat);
         
        // a patch for the lone US border line:
        // between lon (-94.5,-123.5) and lat (48.5, 49.5),
        // Alaska between lon (-135,-145) and lat (60, 70), and
        // N. Afr. lines between lon (-15, 40) and lat (15,35)
        //
          if ( (j > 0) &&
             ( ((lon > -2.15547) && (lon < -1.64933)
               && (lat > 0.84648) && (lat < 0.86393)) ||
               ((lon < -2.3561944) && (lon > -2.5307371)
               && (lat > 1.047197) && (lat < 1.2217303)) ||
               ((lon > -0.2617992) && (lon < 0.6981316)
               && (lat > 0.2617992) && (lat < 0.610865))
             )
            ) 
          {
             dist_sqr = (lon - old_lon) * (lon - old_lon) +
                        (lat - old_lat) * (lat - old_lat);
             num_extra_pts = (int)(dist_sqr / threshold);
	   /*
           if (num_extra_pts > 0)
           {
             if (INFOfp != NULL)
                fprintf (INFOfp, 
                  "... making %d extra points from old_lon=%f, old_lat=%f, to lon=%f, lat=%f .
..\n",
                         num_extra_pts, old_lon*180/M_PI, old_lat*180/M_PI,
                         lon*180/M_PI, lat*180/M_PI);
           }
	   */
             for (k=1; k<=num_extra_pts;k++)
             {
                extra_lon = lon * k / (num_extra_pts + 1.0) +
                            old_lon * (num_extra_pts + 1.0 - k) / (num_extra_pts + 1.0);
                extra_lat = lat * k / (num_extra_pts + 1.0) +
                            old_lat * (num_extra_pts + 1.0 - k) / (num_extra_pts + 1.0);
                vec[0] = cos(extra_lat) * sin(extra_lon) * factor;
                vec[1] = sin(extra_lat) * factor;
                vec[2] = cos(extra_lat) * cos(extra_lon) * factor;
                glVertex3f(vec[0], vec[1], vec[2]);
             }
         }
         old_lon = lon;
         old_lat = lat;
 
         vec[0] = cos(lat) * sin(lon) * factor;
         vec[1] = sin(lat) * factor;
         vec[2] = cos(lat) * cos(lon) * factor;        
         glVertex3f(vec[0], vec[1], vec[2]);
      }
      glEnd();
    }
    glEndList();
}

void
Tearth::objdraw ()
{
/*
 *   Draw the globe (with coasts and boundaries, if wanted)
 */
    glPushAttrib(GL_ALL_ATTRIB_BITS);

    glLightfv(GL_LIGHT0,  GL_POSITION, sunlt_POSITION);
    glLightfv(GL_LIGHT0,  GL_DIFFUSE,  sunlt_LCOLOR);
    glLightModelfv(GL_LIGHT_MODEL_AMBIENT, lm_LMAMBIENT);
    glLightModelfv(GL_LIGHT_MODEL_LOCAL_VIEWER, lm_LOCALVIEWER);
    glShadeModel(GL_SMOOTH);

    glEnable(GL_LIGHT0);
    glEnable(GL_LIGHTING);
    glEnable(GL_COLOR_MATERIAL);
    glDepthFunc(GL_LESS);
    glEnable(GL_DEPTH_TEST);
    /*
     *   Render the globe
     */
    glPushMatrix();
    if (p_texturing) {
       glEnable(GL_TEXTURE_2D);
       glEnable(GL_CULL_FACE);
    }
    glCallList(p_slid);
    if (p_texturing) {
       glDisable(GL_CULL_FACE);
       glDisable(GL_TEXTURE_2D);
    } else {
       glDisable(GL_LIGHTING);
       glDisable(GL_COLOR_MATERIAL);
    }
    glPopMatrix();
    /*
     *   Render the coastlines, political boundaries, and gridlines
     */
    glPushMatrix();
    glCallList(p_clid);
    glCallList(p_plid);
    glCallList(p_glid);
    glPopMatrix();
    /*
     *   Render the terrain
     */
    if (p_terrain) {
       printf("Tearth: drawing terrain\n");
       glShadeModel(GL_SMOOTH);
       //glDisable(GL_LIGHT0);
       glDisable(GL_COLOR_MATERIAL);
       //glEnable(GL_LIGHT2);
       glEnable(GL_LIGHTING);
       glDepthFunc(GL_LESS);
       glEnable(GL_DEPTH_TEST);
       glColor3f(0.0, 1.0, 0.0);
       glPolygonOffset(0.1, 0.1);
       glEnable(GL_POLYGON_OFFSET_FILL);
       glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
       glPushMatrix();
         glCallList(p_tlid);
       glPopMatrix();
    }

    glPopAttrib();
}
      
int
Tearth::texture_init ()
{
int            i, tiles;
float          stx, sty, edx, edy, tilesizex, tilesizey;
char           msgstr[80];
GLint          sphereMap[] = {GL_SPHERE_MAP};

    glEnable(GL_TEXTURE_2D);

    p_texps[0] = TX_MINFILTER;
    p_texps[1] = TX_BILINEAR;
    glTexParameterf(GL_TEXTURE_2D, (GLenum) p_texps[0], p_texps[1]);

    p_texps[2] = TX_MAGFILTER;
    p_texps[3] = TX_BILINEAR;
    glTexParameterf(GL_TEXTURE_2D, (GLenum) p_texps[2], p_texps[3]);

    p_texps[4] = TX_WRAP_S;
    p_texps[5] = TX_CLAMP;
    glTexParameterf(GL_TEXTURE_2D, (GLenum) p_texps[4], p_texps[5]);

    p_texps[6] = TX_WRAP_T;
    p_texps[7] = TX_CLAMP;
    glTexParameterf(GL_TEXTURE_2D, (GLenum) p_texps[6], p_texps[7]);

    p_texps[8] = TX_TILE;
    p_texps[9]  = 0.0;
    p_texps[10] = 0.0;
    p_texps[11] = .25;
    p_texps[12] = .25;
    p_texps[13] = TX_NULL;

    if (p_component == 1)
    {
      //p_tevps[0] = TV_COLOR;
      p_tevps[0] = 0.5;
      p_tevps[1] = 0.11;
      p_tevps[2] = 0.0;
      p_tevps[3] = 1.0;
      glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, p_tevps);
      p_tevps[5] = TV_BLEND;
      glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_BLEND);
      p_tevps[6] = TV_NULL;
      //tevdef(1, 7, p_tevps);
      //image = (unsigned long *) take_hdfdata (p_filename, p_xsize, p_ysize);
    }
    else if (p_component == 4)
    {
      p_tevps[0] = TV_NULL;
      //tevdef(4, 1, p_tevps);
      glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
      //image = (unsigned long *) take_rgbdata (p_filename);
      image = tkRGBImageLoad(p_filename);
      if (!image)
      {
         fprintf (stderr, "I cannot find the texture files.\n");
         return 0;
      }
      image->data = AlphaPadImage(image->sizeX*image->sizeY,
                                  image->data, 128);
      glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
      gluBuild2DMipmaps(GL_TEXTURE_2D, p_component,
                        image->sizeX, image->sizeY,
                        GL_RGBA, GL_UNSIGNED_BYTE, image->data);
    }     

   return 1;
}
