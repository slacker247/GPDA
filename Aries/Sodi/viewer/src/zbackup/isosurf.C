/* isosurf.c */

/* Mesa 2.0 version

   Display an isosurface of 3-D wind speed volume.  Use arrow keys to
   rotate, S toggles smooth shading, L toggles lighting

   Brian Paul */

/* Conversion to GLUT and OpenGL 1.1 by Mark J. Kilgard */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <GL/glut.h>

#ifndef GL_VERSION_1_1
/* OpenGL 1.1 not supported so emulate OpenGL 1.1
   vertex arrays with the EXT_vertex_array extension. */
#define GL_VERTEX_ARRAY GL_VERTEX_ARRAY_EXT
#define GL_NORMAL_ARRAY GL_NORMAL_ARRAY_EXT
#define glDrawArrays(a,b,c) glDrawArraysEXT(a,b,c)
#define glVertexPointer(a,b,c,d) glVertexPointerEXT(a,b,c,numverts,d)
#define glNormalPointer(a,b,c) glNormalPointerEXT(a,b,numverts,c)
#endif

GLboolean speed_test = GL_FALSE;
GLboolean use_vertex_arrays = GL_FALSE;

GLboolean doubleBuffer = GL_TRUE;

GLboolean smooth = GL_TRUE;
GLboolean lighting = GL_TRUE;

#define MAXVERTS 10000

static GLfloat verts[MAXVERTS][3];
static GLfloat norms[MAXVERTS][3];
static GLint numverts;

static GLfloat xrot;
static GLfloat yrot;

static void 
read_surface(char *filename)
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

static void 
draw_surface(void)
{
  int i;

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
}

static void 
draw1(void)
{
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glPushMatrix();
  glRotatef(yrot, 0.0, 1.0, 0.0);
  glRotatef(xrot, 1.0, 0.0, 0.0);

  draw_surface();

  glPopMatrix();

  if (doubleBuffer) {
    glutSwapBuffers();
  } else {
    glFlush();
  }
}

static void 
draw(void)
{
  if (speed_test) {
    for (xrot = 0.0; xrot <= 360.0; xrot += 10.0) {
      draw1();
    }
    exit(0);
  } else {
    draw1();
  }
}

static void 
InitMaterials(void)
{
  static float ambient[] =
  {0.1, 0.1, 0.1, 1.0};
  static float diffuse[] =
  {0.5, 1.0, 1.0, 1.0};
  static float position0[] =
  {0.0, 0.0, 20.0, 0.0};
  static float position1[] =
  {0.0, 0.0, -20.0, 0.0};
  static float front_mat_shininess[] =
  {60.0};
  static float front_mat_specular[] =
  {0.2, 0.2, 0.2, 1.0};
  static float front_mat_diffuse[] =
  {0.5, 0.28, 0.38, 1.0};
  static float lmodel_ambient[] =
  {1.0, 1.0, 1.0, 1.0};
  static float lmodel_twoside[] =
  {GL_FALSE};

  glLightfv(GL_LIGHT0, GL_AMBIENT, ambient);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, diffuse);
  glLightfv(GL_LIGHT0, GL_POSITION, position0);
  glEnable(GL_LIGHT0);

  glLightfv(GL_LIGHT1, GL_AMBIENT, ambient);
  glLightfv(GL_LIGHT1, GL_DIFFUSE, diffuse);
  glLightfv(GL_LIGHT1, GL_POSITION, position1);
  glEnable(GL_LIGHT1);

  glLightModelfv(GL_LIGHT_MODEL_AMBIENT, lmodel_ambient);
  glLightModelfv(GL_LIGHT_MODEL_TWO_SIDE, lmodel_twoside);
  glEnable(GL_LIGHTING);

  glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, front_mat_shininess);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, front_mat_specular);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, front_mat_diffuse);
}

static void 
Init(void)
{
  glClearColor(0.0, 0.0, 0.0, 0.0);

  glShadeModel(GL_SMOOTH);
  glEnable(GL_DEPTH_TEST);

  InitMaterials();

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glFrustum(-1.0, 1.0, -1.0, 1.0, 5, 25);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glTranslatef(0.0, 0.0, -6.0);

#if defined(GL_EXT_vertex_array) || defined(GL_VERSION_1_1)
  if (use_vertex_arrays) {
    glVertexPointer(3, GL_FLOAT, 0, verts);
    glNormalPointer(GL_FLOAT, 0, norms);
    glEnable(GL_VERTEX_ARRAY);
    glEnable(GL_NORMAL_ARRAY);
  }
#endif
}

int
supportsOneDotOne(void)
{
  const GLubyte *version;
  int major, minor;

  version = glGetString(GL_VERSION);
  if (sscanf(version, "%d.%d", &major, &minor) == 2)
    return major > 1 || minor >= 1;
  return 0;  /* OpenGL version string malformed! */
}
