#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>
#include <math.h> 
#include <time.h>
#include <signal.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/sysinfo.h>
#include <sys/time.h>

#include "forms.h"
#include "H3Dforms.h"
#include "H3Dproc.H"

/* GL includes */
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>
#include "GL/glx.h" 
#include "GL/gltk.h"

/* --------------------------------------------------------------------- */
 
#ifndef FALSE
#define FALSE   0
#endif
#ifndef TRUE
#define TRUE    1
#endif 

#define DEGRAD  57.29577951308232
#define RADDEG  0.0174532925199432958

#define logf(x) (float)log((double)x)

/*
 * Macros
 */
#define GRID		0x22
#define ZGRID		0x23
#define XGRID		0x24
#define YGRID		0x25
#define HISTO           0x26
#define AXIS            0x27

/* --------------------------------------------------------------------- */

int             H3DwinX, H3DwinY;
int             H3DwinW, H3DwinH;
int             H3DglInit = FALSE;
char            *H3DitMaps;
char            h3dtemp[1280];                    // Global scratch space for string operations
char            H3Dlabel[32];                     // Time Line Enforcer window label
char            H3Dlevels[6][32];
Window          H3Dwinid;
H3DINFO         h3dinfo;

int             h3d_sizeX = 36;
int             h3d_sizeY = 36;
float           h3d_table[36][36];

int             nWidth, nHeight;
float           fAspect;
float           H3Drot = 30.0;
float           H3Delev = 50.0;
GLdouble        upX, upY, upZ;
GLdouble        eyeX, eyeY, eyeZ;
GLdouble        lookX, lookY, lookZ;

GLfloat         r[6] = { 1.0, 0.0, 0.0, 1.0, 0.0, 1.0 };
GLfloat         g[6] = { 0.0, 1.0, 0.0, 1.0, 1.0, 0.0 };
GLfloat         b[6] = { 0.0, 0.0, 1.0, 0.0, 1.0, 1.0 };

void            *font1 = GLUT_BITMAP_9_BY_15;     /* used fonts */
void            *font2 = GLUT_BITMAP_8_BY_13;
void            *font3 = GLUT_STROKE_ROMAN;

FD_h3dgraph     *fd_h3dgraph;                     // Histogram top level form

/* --------------------------------------------------------------------- */

//void timelapsedCB(int tid, void *stuff);
//callback events for opengl canvas
int h3dexposeCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud);
int h3dbuttonCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud);


void MakeGrid(void);
void MakeHistogram();
void MakeAxis();
void Legend();

extern char *strsub(char *istr, char och, char nch);
extern "C" GLXContext fl_get_glcanvas_context(FL_OBJECT * ob);

int             H3DidleCB(XEvent *ev, void *data);           // Draw idle loop
void            H3DdrawCB();                                 // Draw to opengl canvas
void            H3Dinit();
void            H3Dshow(int xpos, int ypos, int width, int height, Window mainwinID);
void            H3Ddata(int ix, int iy, float table[36][36], H3DINFO info);
extern int      EraseActiveEntry(char *text);
extern int      StoreActiveEntry(char *text);

/*                                                                       */
/* --------------------------------------------------------------------- */
/*              M A I N   P R O G R A M   S T A R T S   H E R E          */
/* --------------------------------------------------------------------- */
/*                                                                       */
void H3Dinit()
{
int             i, j;

   fd_h3dgraph  = create_form_h3dgraph();

   strcpy(H3Dlabel, "Histogram-3D");

   //fl_set_thumbwheel_return(fd_h3dgraph->interval, FL_RETURN_CHANGED);
   fl_set_slider_value(fd_h3dgraph->zoomer, (double)80.0);
   fl_set_slider_bounds(fd_h3dgraph->zoomer, (double)60.0, (double)200.0);
   fl_set_slider_step(fd_h3dgraph->zoomer, (double)1.0);

   fl_set_slider_value(fd_h3dgraph->viewpos, (double)H3Drot);
   fl_set_slider_bounds(fd_h3dgraph->viewpos, (double)-90.0, (double)90.0);
   fl_set_slider_step(fd_h3dgraph->viewpos, (double)1.0);

   fl_set_slider_value(fd_h3dgraph->elevation, (double)H3Delev);
   fl_set_slider_bounds(fd_h3dgraph->elevation, (double)0.0, (double)150.0);
   fl_set_slider_step(fd_h3dgraph->elevation, (double)1.0);

   fl_add_canvas_handler(fd_h3dgraph->canvas, Expose,      h3dexposeCB, 0);
   fl_add_canvas_handler(fd_h3dgraph->canvas, ButtonPress, h3dbuttonCB, 0);

   return;
}

int H3Dclose(FL_FORM *form, void *data)
{
   H3DexitCB(NULL, 0);

   return(0);
}

void H3Dshow(int xpos, int ypos, int width, int height, Window mainwinID)
{

   if(!fl_form_is_visible(fd_h3dgraph->h3dgraph) ) {
      fl_transient();
      fl_winposition(xpos, ypos);
      fl_initial_winsize(width, height);
      H3Dwinid = fl_prepare_form_window(fd_h3dgraph->h3dgraph,
                                     FL_PLACE_POSITION,FL_TRANSIENT, H3Dlabel);
      fl_winreshape(H3Dwinid, xpos, ypos, width, height);
      fl_get_wingeometry(H3Dwinid, &H3DwinX, &H3DwinY, &H3DwinW, &H3DwinH); 
      fl_show_form_window(fd_h3dgraph->h3dgraph);
      fl_set_form_atclose(fd_h3dgraph->h3dgraph, H3Dclose, 0);
      StoreActiveEntry(H3Dlabel);

      H3DglInit = FALSE;
   }

   H3DdrawCB();

   return;
}

void H3Ddata(int ix, int iy, float table[36][36], H3DINFO info)
{
int             i, j;
float           ratio;
typedef float   *array;
array           *a;

   h3d_sizeX = ix;
   h3d_sizeY = iy;

   h3dinfo   = info;

   fl_set_counter_value(fd_h3dgraph->h3d_scale, (double)h3dinfo.vscale);

   for (i=0; i<6; i++)
     strcpy(H3Dlevels[i], "Not Used");

   for (i=0; i<info.nlevels; i++)
     strcpy(H3Dlevels[i], info.label[i]);
   /*
   float (* h3d_table)[h3d_sizeY];
   h3d_table = new float[h3d_sizeX][h3d_sizeY];
                -- or --
   a = new array[36];
   for (i=0; i<36; i++) a[i] = new float[36];
   */
   for (i=0; i<h3d_sizeX; i++) {
     for (j=0; j<h3d_sizeY; j++) {
       if (i == j) {
	 ratio = 1.0;
       } else {
	 ratio = table[i][j];
       }
       h3d_table[i][j] = ratio;
     }
   }

   if (glIsList(HISTO)) glDeleteLists(HISTO, 1);

   return;
}

void H3Ddraw(int tid, void *stuff)
{
  //h3ddrawCB();

   return;
}
/*                                                                       */
/* --------------------------------------------------------------------- */
/*      C A L L B A C K    R O U T I N E S    S T A R T    H E R E       */
/* --------------------------------------------------------------------- */
/*                                                                       */
void H3DexitCB(FL_OBJECT *ob, long data)
{
   fl_set_idle_callback(NULL, 0);

   fl_hide_form(fd_h3dgraph->h3dgraph);
   EraseActiveEntry(H3Dlabel);

   return;
}

void H3DnoneCB(FL_OBJECT *ob, long data)
{
   return;
}

int H3DidleCB(XEvent *ev, void *data) /* idle callback */
{
  //H3DdrawCB();

   fl_check_forms();

   return(0);
}

/*
 * Draw a string (Bitmap)
 */
void glPutBitmap(GLint x, GLint y, char *string, void *font)
{
int             len, i;
float           colors[4];

   glGetFloatv(GL_CURRENT_COLOR, colors);
   glColor3f(0.0, 0.0, 0.0);
   glRasterPos2i(x, y);
   len = (int)strlen(string);
   for (i = 0; i < len; i++) {
     glutBitmapCharacter(font, string[i]);
   }
   glColor4f(colors[0], colors[1], colors[2], colors[3]);
}
/*
 * Draw a string (Stroke)
 */
void glPutStroke(GLint x, GLint y, char *string, void *font)
{
int             len, i, ch;
float           colors[4];

   glGetFloatv(GL_CURRENT_COLOR, colors);
   glColor3f(0.0, 0.0, 0.0);
   //glRasterPos2i(x, y);
   len = (int)strlen(string);
   for (i = 0; i < len; i++) {
     ch = (int)string[i];
     glutStrokeCharacter(font, ch);
   }
   glColor4f(colors[0], colors[1], colors[2], colors[3]);
}
/*
 * Draw the 3d grid
 */
void DrawGrid(void)
{
   glPushMatrix();
     glRotatef(180.0+H3Drot, 1.0, 0.0, 0.0);
     glRotatef( 90.0, 0.0, 0.0, 1.0);
     glTranslatef(-36.0, -36.0, 0.0);
     glCallList(GRID);
   glPopMatrix();
}
/*
 * Draw the scene
 */
void display(void)
{
int             i,j;
double          ratio;
GLfloat         fSize, nPeriod;
GLUquadricObj   *qobj;

//
//   Draw the grid
//
   glPushMatrix();
     glRotatef(270.0+H3Drot, 0.0, 0.0, 1.0);
     glTranslatef(-(h3d_sizeX*3.0/2.0), -(h3d_sizeY*3.0/2.0), 0.0);
     glCallList(GRID);
   glPopMatrix();
//
//   Draw axis
//
   /*
   glPushMatrix();
     glCallList(AXIS);
   glPopMatrix();
   */
//
//   Draw the histogram
//
   glPushMatrix();
     glRotatef(270.0+H3Drot, 0.0, 0.0, 1.0);
     glTranslatef(-(h3d_sizeX*3.0/2.0), -(h3d_sizeY*3.0/2.0), 0.0);
     glScaled(1.0, 1.0, fl_get_counter_value(fd_h3dgraph->h3d_scale));
     glCallList(HISTO);
   glPopMatrix();

   return;
}

void h3ddrawCB()
{
}

void H3DdrawCB()
{
int             i;
int             vpwindW, vpwindH;
int             viewport[4];
float           point[3], scale, xtic, fscale;
char            chlabel[16];
Window          thiswin;
//
//      Define the Lighting Model parameters
//
static float    mat_AMBIENT[]        = { 0.3, 0.3, 0.3, 1.0 };
static float    mat_DIFFUSE[]        = { 0.8, 0.8, 0.8, 1.0 };
static float    mat_EMISSION[]       = { 0.3, 0.3, 0.3, 1.0 };
static float    mat_SPECULAR[]       = { 0.8, 0.8, 0.8, 1.0 };
static float    mat_SHININESS[]      = { 10.0 };
static float    dullmat_AMBIENT[]    = { 0.2, 0.2, 0.2, 1.0 };
static float    dullmat_DIFFUSE[]    = { 0.5, 0.5, 0.5, 1.0 };
static float    dullmat_EMISSION[]   = { 0.3, 0.3, 0.3, 1.0 };
static float    dullmat_SPECULAR[]   = { 0.4, 0.4, 0.4, 1.0 };
static float    dullmat_SHININESS[]  = { 50.0 };
static float    sunlt_LCOLOR[]       = { 0.2, 0.8, 0.5, 0.0 };
static float    sunlt_POSITION[]     = {-200.0, -200.0, -200.0, 0.0 };
static float    sunlt_AMBIENT[]      = { 0.1, 0.1, 0.1, 1.0 };
static float    sunlt_DIFFUSE[]      = { 1.0, 1.0, 1.0, 1.0 };
static float    sunlt_SPECULAR[]     = { 1.0, 1.0, 1.0, 1.0 };
static float    light_POSITION[]     = {-1.0,-1.0, 2.0, 0.0 };
static float    moonlt_LCOLOR[]      = { 0.1, 0.1, 0.8, 0.0 };
static float    moonlt_POSITION[]    = {-1.0, 1.0, 2.0, 0.0 };
static float    nonelt_LCOLOR[]      = { 0.1, 0.1, 0.6, 0.0 };
static float    nonelt_POSITION[]    = { 0.0, 0.0, 1.0, 0.0 };
static float    lm_LMAMBIENT[]       = { 0.5, 0.5, 0.5, 1.0 };
static float    lm_LOCALVIEWER[]     = { 1.0 };
static float    dulllm_LMAMBIENT[]   = { 0.2, 0.2, 0.2, 1.0 };
static float    dulllm_LOCALVIEWER[] = { 1.0 };

   if(!fl_form_is_visible(fd_h3dgraph->h3dgraph) ) return;

   //thiswin = fl_winget();
   //fl_set_cursor(thiswin, XC_watch);

   glXMakeCurrent(fl_display, fl_get_canvas_id(fd_h3dgraph->canvas),
                  fl_get_glcanvas_context(fd_h3dgraph->canvas));
 
   if (!H3DglInit) {
     fprintf(stderr, "Entering GL init\n");
     //
     //   Set up the viewing window
     //
     fl_get_winsize(fl_get_canvas_id(fd_h3dgraph->canvas), &nWidth, &nHeight);
     fAspect = (float)nWidth/(float)nHeight;
     glViewport(0, 0, nWidth, nHeight);
     //
     //   Build the display lists
     //
     MakeAxis();
     MakeGrid();
     MakeHistogram();
     //
     //   Set up the default Model view point
     //
     eyeX  = 35.0;     eyeY  = 80.0;    eyeZ  = 50.0;
     lookX = 35.0;     lookY = 30.0;    lookZ = 0.0;
     upX   = 0.0;      upY   = 0.0;     upZ   = 1.0;
     //
     //   Set up the materials
     //
     glMaterialfv(GL_FRONT, GL_AMBIENT,   dullmat_AMBIENT);
     glMaterialfv(GL_FRONT, GL_DIFFUSE,   dullmat_DIFFUSE);
     glMaterialfv(GL_FRONT, GL_SPECULAR,  dullmat_SPECULAR);
     glMaterialfv(GL_FRONT, GL_SHININESS, dullmat_SHININESS);
     //
     //   Set up the lighting
     //
     glLightfv(GL_LIGHT0,  GL_POSITION, sunlt_POSITION);
     glLightfv(GL_LIGHT0,  GL_DIFFUSE,  sunlt_LCOLOR);
     glLightModelfv(GL_LIGHT_MODEL_AMBIENT, lm_LMAMBIENT);
     glLightModelfv(GL_LIGHT_MODEL_LOCAL_VIEWER, lm_LOCALVIEWER);
     //
     fl_set_idle_callback(H3DidleCB, 0);
     //
     H3DglInit = TRUE;
   }

   if (!glIsList(HISTO)) MakeHistogram();

   glClearColor(1.0, 1.0, 1.0, 0.0);
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
   glPushMatrix();                                // Save initial state of OpenGL
   //
   glShadeModel(GL_SMOOTH);
   glEnable(GL_COLOR_MATERIAL);
   glEnable(GL_LIGHTING);
   glEnable(GL_LIGHT0);
   //
   //   Set up the projection for the legend
   //
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   gluOrtho2D(0, nWidth, nHeight, 0);
   glMatrixMode(GL_MODELVIEW);
   //
   //   Do the legend display
   //
   glColor3f(1.0, 1.0, 0.0);
   //glPutBitmap(15, 15, "CPU Activity", font1);
   Legend();
   //
   //   Set up the projection for the histogram
   // 
   glMatrixMode (GL_PROJECTION);
   glLoadIdentity ();
   //gluOrtho2D(0.0, 0.0, -10.0, 100.0);
   //glFrustum (-50.0, 50.0, 50.0, 50.0, 200.0, -200.0);
   gluPerspective((GLdouble)45.0, fAspect, 45.0, 500.0);
   //
   eyeX  = 0.0;
   eyeY  = (float)fl_get_slider_value(fd_h3dgraph->zoomer);
   eyeZ  = (float)fl_get_slider_value(fd_h3dgraph->elevation);
   lookX = 0.0;
   lookY = 0.0;
   lookZ = 0.0;
   glMatrixMode (GL_MODELVIEW);
   glLoadIdentity ();             /* clear the matrix */
   gluLookAt(eyeX, eyeY, eyeZ, lookX, lookY, lookZ, upX, upY, upZ);
   //
   //   Do the histogram display
   //
   display();
   //
   glDisable(GL_LIGHT0);
   glDisable(GL_LIGHTING);
   glDisable(GL_COLOR_MATERIAL);

   glPopMatrix();                                 // Restore initial state of OpenGL

   glFlush();
   glXSwapBuffers(fl_display, fl_get_canvas_id(fd_h3dgraph->canvas));

   //fl_reset_cursor(thiswin);

   return;
}

void H3DrefreshCB(FL_OBJECT *ob, long data)
{
   fl_ringbell(0);
   H3DdrawCB();

   return;
}

void H3DsliderCB(FL_OBJECT *ob, long data)
{
   H3Drot = (float)fl_get_slider_value(fd_h3dgraph->viewpos);
   H3DdrawCB();

   return;
}

void H3DzoomCB(FL_OBJECT *ob, long data)
{
   eyeY = (float)fl_get_slider_value(fd_h3dgraph->zoomer);
   H3DdrawCB();

   return;
}

void H3DelevCB(FL_OBJECT *ob, long data)
{

   H3DdrawCB();

   return;
}

//<------------------------expose-------------------------------------->

//Run when the opengl screen needs to be refreshed 'cause it is "expose"

int h3dexposeCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud)
{
   H3DdrawCB();

   return(0);
}

//<---------------------button press-------------------------------->

//when the user clicks in the opengl canvas window...the program goes here

int h3dbuttonCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud)
{
FL_Coord        width, height;
float           xpos, ypos, closest, d;
float           slat, slon, salt, shead;
int             i, xval, yval;

   fl_ringbell(0);

   xval = xev->xbutton.x;
   yval = xev->xbutton.y;

   fl_get_winsize(win, &width, &height);
   //xpos = (-180.0*RADDEG) + ((float)xval/(float)width)*(360.0*RADDEG);
   //ypos = (90.0*RADDEG) - ((float)yval/(float)height)*(180.0*RADDEG);
   xpos = xval; ypos = yval;
   fprintf(stderr, "User clicked in canvas at %f %f\n", xpos, ypos);
   /*
   xpos = (xpos*DEGRAD) + 180.0;
   ypos = (ypos*DEGRAD) + 90.0;
   closest = 99999.9;
   for (i=0; i<n_assets; i++) {
     AssetGetLLAH(i, &slat, &slon, &salt, &shead);
     slon = slon + 180.0;
     slat = slat + 90.0;
     d = sqrt( (xpos-slon)*(xpos-slon) + (ypos-slat)*(ypos-slat) );
     if (d < closest) {
       closest = d;
       asset_index = i;
     }
   }
   fprintf(stderr, "We seem to have a match at %s\n", AssetGetName(asset_index));
   */

   return(0);
}
/*
 * Toggle line antialias 
 */
void ToggleAAlias(void)
{
    if(glIsEnabled(GL_LINE_SMOOTH))
	{
	glDisable(GL_LINE_SMOOTH);	    
	glDisable(GL_BLEND);	    
	}
    else
	{
	glEnable(GL_LINE_SMOOTH);
	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    	glHint(GL_LINE_SMOOTH_HINT, GL_DONT_CARE);
	}
}
/*
 * Display the legend
 */
void Legend(void)
{
    glPutBitmap(5, 40, H3Dlevels[0], font2);    
    glPushMatrix();
      glPushAttrib(GL_CURRENT_BIT);
      glTranslatef(10.0, 50.0, 0.0);
      glColor3f(r[0], g[0], b[0]);
      glRecti(0, 0, 20, 20);
      glPopAttrib();
    glPopMatrix();
    
    glPutBitmap(5, 90, H3Dlevels[1], font2);    
    glPushMatrix();
      glPushAttrib(GL_CURRENT_BIT);
      glTranslatef(10.0, 100.0, 0.0);
      glColor3f(r[1], g[1], b[1]);
      glRecti(0, 0, 20, 20);
      glPopAttrib();
    glPopMatrix();

    glPutBitmap(5, 140, H3Dlevels[2], font2);    
    glPushMatrix();
      glPushAttrib(GL_CURRENT_BIT);
      glTranslatef(10.0, 150.0, 0.0);
      glColor3f(r[2], g[2], b[2]);
      glRecti(0, 0, 20, 20);
      glPopAttrib();
    glPopMatrix();

    glPutBitmap(5, 190, H3Dlevels[3], font2);    
    glPushMatrix();
      glPushAttrib(GL_CURRENT_BIT);
      glTranslatef(10.0, 200.0, 0.0);
      glColor3f(r[3], g[3], b[3]);
      glRecti(0, 0, 20, 20);
      glPopAttrib();
    glPopMatrix();
    
    glPutBitmap(5, 240, H3Dlevels[4], font2);    
    glPushMatrix();
      glPushAttrib(GL_CURRENT_BIT);
      glTranslatef(10.0, 250.0, 0.0);
      glColor3f(r[4], g[4], b[4]);
      glRecti(0, 0, 20, 20);
      glPopAttrib();
    glPopMatrix();

    glPutBitmap(5, 290, H3Dlevels[5], font2);    
    glPushMatrix();
      glPushAttrib(GL_CURRENT_BIT);
      glTranslatef(10.0, 300.0, 0.0);
      glColor3f(r[5], g[5], b[5]);
      glRecti(0, 0, 20, 20);
      glPopAttrib();
    glPopMatrix();

    glPutBitmap(nWidth/2, nHeight-40, "Effect", font1);
    glPutBitmap(nWidth-150, (nHeight/2)-20, "Perturbation", font1);
    //glPutBitmap(nWidth-30, nHeight-30, "0", font1); 
    //glPutBitmap(nWidth-45, 45, "100", font1);
    glPushMatrix();
      glScalef(0.1, 0.1, 0.1);
      glRotatef(180.0, 0.0, 1.0, 0.0);
      glTranslatef(50.0, 50.0, 0.0);
      glPutStroke(0, 0, "Origin", font3);
    glPopMatrix();
}
/*
 * Construct the grid display list
 */
void MakeGrid(void)
{
    int i, j, k, n, nodeNo;
    
    glNewList(ZGRID, GL_COMPILE_AND_EXECUTE);
    glColor3f(0.0, 0.0, 0.0);
    glBegin(GL_LINES);
    for(i=0;i<=(h3d_sizeX+1)*3;i=i+=3) {
      k = 0;
      nodeNo = 0;
      for (n=0; n<h3dinfo.nlevels; n++) {
	nodeNo = nodeNo + h3dinfo.nnodes[n];
	if (i > (nodeNo)*3) k++;
      }
      glColor3f(r[k], g[k], b[k]);
      glVertex2d(i, 0);
      glVertex2d(i, (h3d_sizeX+1)*3);
      glVertex2d(0, i);
      glVertex2d((h3d_sizeX+1)*3, i);
    }
    glEnd();
    glEndList();
            
    glNewList(XGRID, GL_COMPILE_AND_EXECUTE);
    glColor3f(0.0, 0.0, 0.0);
    glPushMatrix();
      glRotatef(90.0, 1.0, 0.0, 0.0);    
      glBegin(GL_LINES);
      for(i=0;i<=6;i+=3) {
	glVertex2d(0, i);
	glVertex2d((h3d_sizeX+1)*3, i);
      }
      for(i=0;i<=(h3d_sizeX+1)*3;i+=3) {
        k = 0;
	nodeNo = 0;
	for (n=0; n<h3dinfo.nlevels; n++) {
	  nodeNo = nodeNo + h3dinfo.nnodes[n];
	  if (i > (nodeNo)*3) k++;
	}
	glColor3f(r[k], g[k], b[k]);
	glVertex2d(i, 0);
	glVertex2d(i, 6);
      } 
      glEnd();
    glPopMatrix();
    glEndList();

    glNewList(YGRID, GL_COMPILE_AND_EXECUTE);
    glColor3f(0.0, 0.0, 0.0);
    glPushMatrix();
      glRotatef(-90.0, 0.0, 1.0, 0.0);    
      glBegin(GL_LINES);
      for(i=0;i<=(h3d_sizeX+1)*3;i+=3) {
        k = 0;
	nodeNo = 0;
	for (n=0; n<h3dinfo.nlevels; n++) {
	  nodeNo = nodeNo + h3dinfo.nnodes[n];
	  if (i > (nodeNo)*3) k++;
	}
	glColor3f(r[k], g[k], b[k]);
	glVertex2d(0, i);
	glVertex2d(6, i);
      }
      for(i=0;i<=6;i+=3) {
	glVertex2d(i, 0);
	glVertex2d(i, (h3d_sizeX+1)*3);
      } 
      glEnd();
    glPopMatrix();
    glEndList();

    /* now call them all */
    glNewList(GRID, GL_COMPILE_AND_EXECUTE);
      //glLineWidth(2.0);
      glColor3f(0.0, 0.0, 0.0);
      glCallList(ZGRID);
      glCallList(XGRID);
      glCallList(YGRID);
      //glLineWidth(1.0);
    glEndList();
}
/*
 * Make the 3d histogram
 */
void MakeHistogram(void)
{
int             i, j, k, n, nodeNo;
double          ratio;
GLUquadricObj   *qobj;

 printf("Make the histogram\n");
 
   glNewList(HISTO, GL_COMPILE_AND_EXECUTE);

   for (i=0; i<h3d_sizeX; i++) {
     glPushMatrix();
       glTranslatef((i+1)*3.0, 0.0, 0.0);
       for (j=0; j<h3d_sizeY; j++) {
         glPushMatrix();
           glTranslatef(0.0, (j+1)*3.0, 0.0);
           qobj = gluNewQuadric();
	   if (i == j) {
	     ratio = 1.0;
	     glColor3f(0.0, 0.0, 0.0);
	   } else {
	     ratio = h3d_table[i][j];
	     //glColor3f(r[i/6], g[i/6], b[i/6]);
	     k = 0;
	     nodeNo = 0;
	     for (n=0; n<h3dinfo.nlevels; n++) {
	       nodeNo = nodeNo + h3dinfo.nnodes[n];
	       if (j > nodeNo-1) k++;
	     }
	     glColor3f(r[k], g[k], b[k]);
	   }
           gluCylinder(qobj, 0.5, 0.5, ratio, 36, 36);
	   gluDeleteQuadric(qobj);
         glPopMatrix();
       }
     glPopMatrix();
   }

   glEndList();
}
/*
 * Make the 3d axis
 */
void MakeAxis(void)
{
int             i,j;

   glNewList(AXIS, GL_COMPILE_AND_EXECUTE);

   glColor3f(1.0, 0.0, 0.0);
   glBegin(GL_LINES);
     glVertex3f(-200.0, 0.0, 0.0);
     glVertex3f(2.0*100.0, 0.0, 0.0);
   glEnd();

   glColor3f(0.0, 1.0, 0.0);
   glBegin(GL_LINES);
     glVertex3f(0.0, -200.0, 0.0);
     glVertex3f(0.0, 2.0*100.0, 0.0);
   glEnd();

   glColor3f(0.0, 0.0, 1.0);
   glBegin(GL_LINES);
     glVertex3f(0.0, 0.0, -200.0);
     glVertex3f(0.0, 0.0, 2.0*100.0);
   glEnd();

   glEndList();
}
