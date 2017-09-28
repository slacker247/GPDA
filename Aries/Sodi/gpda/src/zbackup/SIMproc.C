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

#include "Globals.h"
#include "forms.h"
#include "SIMforms.h"

/* GL includes */
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>
#include "GL/glx.h" 
#include "GL/gltk.h"
#include "glfont.h"

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

FILE            *SIMfd;
int             SIMwinX, SIMwinY;
int             SIMwinW, SIMwinH;
int             SIMglInit = FALSE;
int             SIMeof = FALSE;
int             SIMstep, SIMgrid = 4;
int             SIMx, SIMy, SIMn, SIMsteps;
int             sim_rec   = 0;
int             sim_sizeX = 36;
int             sim_sizeY = 36;
int             sim_nWidth, sim_nHeight;
float           sim_fAspect;
float           SIMrot = 30.0;
float           SIMelev = 50.0;
char            *SIMitMaps;
char            simtemp[1280];                    // Global scratch space for string operations
char            SIMlabel[32];                     // Time Line Enforcer window label
char            SIMlevels[6][32];
Window          SIMwinid;

//GLfloat         r[6] = { 1.0, 0.0, 0.0, 1.0, 0.0, 1.0 };
//GLfloat         g[6] = { 0.0, 1.0, 0.0, 1.0, 1.0, 0.0 };
//GLfloat         b[6] = { 0.0, 0.0, 1.0, 0.0, 1.0, 1.0 };

//void            *font1 = GLUT_BITMAP_9_BY_15;     /* used fonts */
//void            *font2 = GLUT_BITMAP_8_BY_13;
//void            *font3 = GLUT_STROKE_ROMAN;

FD_simgraph     *fd_simgraph;                     // Histogram top level form

/* --------------------------------------------------------------------- */

void            SIMinit();
void            SIMshow(int xpos, int ypos, int width, int height, Window mainwinID);
void            SIMexitCB(FL_OBJECT *ob, long data);
int             SIMidleCB(XEvent *ev, void *data);           // Draw idle loop
void            SIMdrawCB();                                 // Draw to opengl canvas

//void timelapsedCB(int tid, void *stuff);
//callback events for opengl canvas
int simexposeCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud);
int simbuttonCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud);

extern "C" GLXContext fl_get_glcanvas_context(FL_OBJECT * ob);

extern int      EraseActiveEntry(char *text);
extern int      StoreActiveEntry(char *text);
extern int      FinishUp();

/*                                                                       */
/* --------------------------------------------------------------------- */
/*              M A I N   P R O G R A M   S T A R T S   H E R E          */
/* --------------------------------------------------------------------- */
/*                                                                       */
void SIMinit()
{
int             i, j;

   fd_simgraph  = create_form_simgraph();

   strcpy(SIMlabel, "Swarm-Simulation");

   //fl_set_thumbwheel_return(fd_simgraph->interval, FL_RETURN_CHANGED);
   fl_set_slider_value(fd_simgraph->zoomer, (double)80.0);
   fl_set_slider_bounds(fd_simgraph->zoomer, (double)60.0, (double)200.0);
   fl_set_slider_step(fd_simgraph->zoomer, (double)1.0);

   fl_set_slider_value(fd_simgraph->viewpos, (double)SIMrot);
   fl_set_slider_bounds(fd_simgraph->viewpos, (double)-90.0, (double)90.0);
   fl_set_slider_step(fd_simgraph->viewpos, (double)1.0);

   fl_set_slider_value(fd_simgraph->elevation, (double)SIMelev);
   fl_set_slider_bounds(fd_simgraph->elevation, (double)0.0, (double)150.0);
   fl_set_slider_step(fd_simgraph->elevation, (double)1.0);

   fl_add_canvas_handler(fd_simgraph->canvas, Expose,      simexposeCB, 0);
   fl_add_canvas_handler(fd_simgraph->canvas, ButtonPress, simbuttonCB, 0);

   return;
}

int SIMclose(FL_FORM *form, void *data)
{
   SIMexitCB(NULL, 0);

   return(0);
}

void SIMshow(int xpos, int ypos, int width, int height, Window mainwinID)
{
char            cmdline[64];

   if(!fl_form_is_visible(fd_simgraph->simgraph) ) {
      fl_transient();
      fl_winposition(xpos, ypos);
      fl_initial_winsize(width, height);
      SIMwinid = fl_prepare_form_window(fd_simgraph->simgraph,
                                     FL_PLACE_POSITION,FL_TRANSIENT, SIMlabel);
      fl_winreshape(SIMwinid, xpos, ypos, width, height);
      fl_get_wingeometry(SIMwinid, &SIMwinX, &SIMwinY, &SIMwinW, &SIMwinH); 
      fl_show_form_window(fd_simgraph->simgraph);
      fl_set_form_atclose(fd_simgraph->simgraph, SIMclose, 0);
      StoreActiveEntry(SIMlabel);

      SIMglInit = FALSE;
   }

   fl_set_button(fd_simgraph->sim_playback, 1);

   if (fl_get_button(fd_simgraph->sim_playback) == 1) {
     SIMfd = fopen("../../swarm/iowsim/output.out", "r");
   } else {
     strcpy(cmdline, "heatbugs -b");
     //if ((SIMfd = popen(cmdline, "r")) == NULL)   // Start the simulator if we can
        SIMfd = fopen("../../swarm/iowsim/output.out", "r");
   }
   fgets(simtemp, 1024, SIMfd);
   sscanf(simtemp, "%*s %*s %*s %*s %*s %d %*s %d %*s %d.", &SIMx, &SIMy, &SIMn);
   fgets(simtemp, 1024, SIMfd);
   fgets(simtemp, 1024, SIMfd);
   sscanf(simtemp, "%*s %*s %*s %*s %d", &SIMsteps);
   fgets(simtemp, 1024, SIMfd);

   printf("  %d  %d  %d  %d\n", SIMx, SIMy, SIMn, SIMsteps);

   SIMstep = 0;

   SIMdrawCB();

   return;
}

void SIMdraw(int tid, void *stuff)
{
   SIMdrawCB();

   return;
}
/*                                                                       */
/* --------------------------------------------------------------------- */
/*      C A L L B A C K    R O U T I N E S    S T A R T    H E R E       */
/* --------------------------------------------------------------------- */
/*                                                                       */
void SIMexitCB(FL_OBJECT *ob, long data)
{
   fl_set_idle_callback(NULL, 0);

   fl_hide_form(fd_simgraph->simgraph);
   EraseActiveEntry(SIMlabel);

   FinishUp();

   return;
}

void SIMnoneCB(FL_OBJECT *ob, long data)
{
   return;
}

int SIMidleCB(XEvent *ev, void *data) /* idle callback */
{
   SIMdrawCB();

   fl_check_forms();

   return(0);
}

/*
 * Draw a string (Bitmap)
 *
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
 *
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
*/
/*
 * Draw the scene
 */
void simdisplay(void)
{
int             i,j;
double          ratio;
GLfloat         fSize, nPeriod;
GLUquadricObj   *qobj;
int             x, y, heat, den;
float           point[3], scale, xtic, fscale, fcolor;
char            *c;
char            chequ;
char            chlabel[16];
char            chtmp1[16], chtmp2[16];

//
//   Draw the grid
/*
   glPushMatrix();
     glRotatef(270.0+SIMrot, 0.0, 0.0, 1.0);
     glTranslatef(-(sim_sizeX*3.0/2.0), -(sim_sizeY*3.0/2.0), 0.0);
     glCallList(GRID);
   glPopMatrix();
*/
//   Draw axis
/*
   glPushMatrix();
     glCallList(AXIS);
   glPopMatrix();
*/
//   Draw the simulation results for a time step
//
   fgets(simtemp, 1024, SIMfd);
   if (feof(SIMfd)) {
     SIMeof = TRUE;
     fl_set_idle_callback(NULL, 0);
     return;
   }
   //
   glPushMatrix();
   //
   SIMstep++;
   sprintf(chlabel, "%d", SIMstep);
   fl_set_input(fd_simgraph->sim_step, chlabel);
   //
   glClearColor(1.0, 1.0, 1.0, 0.0);
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
   //
   for (i=0; i<SIMn; i++) {
     c = strchr(simtemp, 'B');
     sscanf(c, "%s %s (%d,%d) %c %d %d", chtmp1, chtmp2, &x, &y, &chequ, &heat, &den);
     sprintf(chlabel, "%d", heat);
     fl_set_input(fd_simgraph->sim_results[0], chlabel);
     sprintf(chlabel, "%d", den);
     fl_set_input(fd_simgraph->sim_results[1], chlabel);
     //
     fcolor = (float)heat/100.0;
     glColor3f(fcolor/256.0, 0.0, 0.0);
     glPointSize((GLfloat)SIMgrid);
     glBegin(GL_POINTS);
       glVertex2f((float)x*SIMgrid, (float)y*SIMgrid);
     glEnd();
     //
     fgets(simtemp, 1024, SIMfd);
   }
   //
   glPopMatrix();

   return;
}

void SIMdrawCB()
{
int             i;
int             vpwindW, vpwindH;
int             viewport[4];
int             x, y, den;
float           point[3], scale, xtic, fscale, fcolor;
char            *c;
char            chequ;
char            chlabel[16];
char            chtmp1[16], chtmp2[16];
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

   if(!fl_form_is_visible(fd_simgraph->simgraph) ) return;
   if (SIMeof) return;

   //thiswin = fl_winget();
   //fl_set_cursor(thiswin, XC_watch);

   glXMakeCurrent(fl_display, fl_get_canvas_id(fd_simgraph->canvas),
                  fl_get_glcanvas_context(fd_simgraph->canvas));
 
   if (!SIMglInit) {
     //
     //   Set up the viewing window
     //
     fl_get_winsize(fl_get_canvas_id(fd_simgraph->canvas), &sim_nWidth, &sim_nHeight);
     sim_fAspect = (float)sim_nWidth/(float)sim_nHeight;
     glViewport(0, 0, sim_nWidth, sim_nHeight);
     SIMgrid = (int)MIN((float)sim_nWidth/(float)SIMx, (float)sim_nHeight/(float)SIMy);
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
     fl_set_idle_callback(SIMidleCB, 0);
     //
     glClearColor(1.0, 1.0, 1.0, 0.0);
     glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
     //
     SIMglInit = TRUE;
   }

   glPushMatrix();                                // Save initial state of OpenGL
   //
   //glShadeModel(GL_SMOOTH);
   //glEnable(GL_COLOR_MATERIAL);
   //glEnable(GL_LIGHTING);
   //glEnable(GL_LIGHT0);
   //
   //   Set up the projection for the display
   //
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   gluOrtho2D(0, sim_nWidth, sim_nHeight, 0);
   glMatrixMode(GL_MODELVIEW);
   //
   //   Do the display
   //
   simdisplay();
   //
   glDisable(GL_LIGHT0);
   glDisable(GL_LIGHTING);
   glDisable(GL_COLOR_MATERIAL);

   glPopMatrix();                                 // Restore initial state of OpenGL

   glFlush();
   glXSwapBuffers(fl_display, fl_get_canvas_id(fd_simgraph->canvas));

   //fl_reset_cursor(thiswin);

   return;
}

void SIMstartCB(FL_OBJECT *ob, long data)
{
char            cmdline[64];

   fl_set_button(fd_simgraph->sim_playback, 1);

   if (fl_get_button(fd_simgraph->sim_playback) == 1) {
     SIMfd = fopen("../../swarm/iowsim/output.out", "r");
   } else {
     strcpy(cmdline, "heatbugs -b");
     if ((SIMfd = popen(cmdline, "r")) == NULL)   // Start the simulator if we can
        SIMfd = fopen("../../swarm/iowsim/output.out", "r");
   }
   fgets(simtemp, 1024, SIMfd);
   sscanf(simtemp, "%*s %*s %*s %*s %*s %d %*s %d %*s %d.", &SIMx, &SIMy, &SIMn);
   fgets(simtemp, 1024, SIMfd);
   fgets(simtemp, 1024, SIMfd);
   sscanf(simtemp, "%*s %*s %*s %*s %d", &SIMsteps);
   fgets(simtemp, 1024, SIMfd);

   printf("  %d  %d  %d  %d\n", SIMx, SIMy, SIMn, SIMsteps);

   SIMstep = 0;

   SIMdrawCB();

   return;
}

void SIMrefreshCB(FL_OBJECT *ob, long data)
{
   fl_ringbell(0);
   SIMdrawCB();

   return;
}

void SIMsliderCB(FL_OBJECT *ob, long data)
{
   SIMrot = (float)fl_get_slider_value(fd_simgraph->viewpos);
   SIMdrawCB();

   return;
}

void SIMzoomCB(FL_OBJECT *ob, long data)
{
  //eyeY = (float)fl_get_slider_value(fd_simgraph->zoomer);
   SIMdrawCB();

   return;
}

void SIMelevCB(FL_OBJECT *ob, long data)
{

   SIMdrawCB();

   return;
}

void SIMcontrolCB(FL_OBJECT *ob, long data)
{

   switch (data) {
   case 0: // Pause
     fl_set_idle_callback(NULL, 0);
     break;

   case 1: // Resume
     fl_set_idle_callback(SIMidleCB, 0);
     break;

   case 2: // Step
     SIMdrawCB();
     break;

   case 3: // Reset
     rewind(SIMfd);
     fgets(simtemp, 1024, SIMfd);
     fgets(simtemp, 1024, SIMfd);
     fgets(simtemp, 1024, SIMfd);
     fgets(simtemp, 1024, SIMfd);

     glClearColor(1.0, 1.0, 1.0, 0.0);
     glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

     SIMstep = 0;

     fl_set_idle_callback(SIMidleCB, 0);
     break;
   }

   return;
}

//<------------------------expose-------------------------------------->

//Run when the opengl screen needs to be refreshed 'cause it is "expose"

int simexposeCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud)
{
   SIMdrawCB();

   return(0);
}

//<---------------------button press-------------------------------->

//when the user clicks in the opengl canvas window...the program goes here

int simbuttonCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud)
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
