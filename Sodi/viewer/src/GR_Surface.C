/* isosurf.c */

/* Mesa 2.0 version

   Display an isosurface of 3-D wind speed volume.  Use arrow keys to
   rotate, S toggles smooth shading, L toggles lighting
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include "GL/gl.h"

#include "def.H"
#include "GR_Surface.H"

#ifndef GL_VERSION_1_1
/* OpenGL 1.1 not supported so emulate OpenGL 1.1
   vertex arrays with the EXT_vertex_array extension. */
#define GL_VERTEX_ARRAY GL_VERTEX_ARRAY_EXT
#define GL_NORMAL_ARRAY GL_NORMAL_ARRAY_EXT
#define glDrawArrays(a,b,c) glDrawArraysEXT(a,b,c)
#define glVertexPointer(a,b,c,d) glVertexPointerEXT(a,b,c,numverts,d)
#define glNormalPointer(a,b,c) glNormalPointerEXT(a,b,numverts,c)
#endif

GLboolean use_vertex_arrays = GL_TRUE;

GLboolean smooth = GL_TRUE;
GLboolean lighting = GL_TRUE;

#define MAXVERTS 10000

static GLfloat verts[MAXVERTS][3];
static GLfloat norms[MAXVERTS][3];
//GLfloat *verts;
//GLfloat *norms;
static GLint numverts;

static GLfloat xrot;
static GLfloat yrot;

static float ambient[] =             {0.1, 0.1, 0.1, 1.0};
static float diffuse[] =             {0.5, 1.0, 1.0, 1.0};
static float position0[] =           {0.0, 0.0, 20.0, 0.0};
static float position1[] =           {0.0, 0.0, -20.0, 0.0};
static float front_mat_shininess[] = {60.0};
static float front_mat_specular[] =  {0.2, 0.2, 0.2, 1.0};
static float front_mat_diffuse[] =   {0.5, 0.28, 0.38, 1.0};
static float lmodel_ambient[] =      {1.0, 1.0, 1.0, 1.0};
static float lmodel_twoside[] =      {GL_FALSE};

void SurfaceRead(char *);
void SurfaceMake(int id, float latitude, float longitude);
void SurfaceDraw(int id);

void 
GR_Surface::SurfaceRead(char *filename)
{
FILE *f;

  f = fopen(filename, "r");
  if (!f) {
    printf("Couldn't read %s\n", filename);
    exit(1);
  }
  numverts = 0;
  while (!feof(f) && numverts < MAXVERTS) {
    fscanf(f, "%f %f %f  %f %f %f",
      &verts[numverts][0], &verts[numverts][1], &verts[numverts][2],
      &norms[numverts][0], &norms[numverts][1], &norms[numverts][2]);
    numverts++;
  }
  numverts--;

  printf("%d vertices, %d triangles\n", numverts, numverts - 2);
  fclose(f);
}
 
GR_Surface::GR_Surface(char *filename, int sid, float latitude, float longitude, float major)
{
  //int i;

int            i, j, ix, iy;
double         a, b, x, y, a2, b2, oldx, oldy, dx;
int            slices = 32;
int            fill = GL_FALSE;


//verts = new float[10000][3];
//norms = new float[10000][3];

   SurfaceRead(filename);

   glLightfv(GL_LIGHT4, GL_AMBIENT, ambient);
   glLightfv(GL_LIGHT4, GL_DIFFUSE, diffuse);
   glLightfv(GL_LIGHT4, GL_POSITION, position0);

   glLightfv(GL_LIGHT5, GL_AMBIENT, ambient);
   glLightfv(GL_LIGHT5, GL_DIFFUSE, diffuse);
   glLightfv(GL_LIGHT5, GL_POSITION, position1);

   p_ulen  = (double)major/RE;

#if defined(GL_EXT_vertex_array) || defined(GL_VERSION_1_1)
   if (use_vertex_arrays) {
    glVertexPointer(3, GL_FLOAT, 0, verts);
    glNormalPointer(GL_FLOAT, 0, norms);
    glEnable(GL_VERTEX_ARRAY);
    glEnable(GL_NORMAL_ARRAY);
   }
#endif

   p_gr_objid = GR_genobj ();
   GR_makeobj (p_gr_objid);

   glPushMatrix();

   glRotated(longitude, 0.0, 1.0, 0.0);     // Rotate to correct longitude from Greenwich
   glRotated(-latitude, 1.0, 0.0, 0.0);     // Rotate to correct latitude from Equator
   glTranslated(0.0, 0.0, 1.01);             // Move to correct location on surface
   glScalef(p_ulen, p_ulen, p_ulen);

#if defined(GL_EXT_vertex_array) || defined(GL_VERSION_1_1)
   if (use_vertex_arrays) {
    glDrawArrays(GL_TRIANGLE_STRIP, 0, numverts);
   } else {
#endif
    glBegin(GL_TRIANGLE_STRIP);
    for (i = 0; i < numverts; i++) {
      glNormal3fv(norms[i]);
      glVertex3fv(verts[i]);
    }
    glEnd();
#if defined(GL_EXT_vertex_array) || defined(GL_VERSION_1_1)
   }
#endif

   glPopMatrix();
   glEndList();
}

void 
GR_Surface::objdraw()
{
  //glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
   glPushAttrib(GL_ALL_ATTRIB_BITS);

   glLightModelfv(GL_LIGHT_MODEL_AMBIENT, lmodel_ambient);
   glLightModelfv(GL_LIGHT_MODEL_TWO_SIDE, lmodel_twoside);
   glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, front_mat_shininess);
   glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, front_mat_specular);
   glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, front_mat_diffuse);

   glShadeModel(GL_SMOOTH);
   glEnable(GL_DEPTH_TEST);

   glEnable(GL_LIGHT4);
   glEnable(GL_LIGHT5);
   glEnable(GL_LIGHTING);

   //GR_color(p_r, p_g, p_b);
   GR_callobj (p_gr_objid);

   glDisable(GL_LIGHT4);
   glDisable(GL_LIGHT5);

   glPopAttrib();
}
