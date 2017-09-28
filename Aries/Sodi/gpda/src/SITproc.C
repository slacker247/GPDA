#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h> 
#include <time.h>
#include <signal.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <stream.h>
#include <sys/ioctl.h>
#ifdef SC_THREADS
#include "pthread.h"
#endif

#include <GL/glut.h>
#include "GL/glx.h" 
#include "GL/gltk.h"

#include "Globals.h"

#include "forms.h"
#include "BMCforms.h"

/* --------------------------------------------------------------------- */

#define FOG     0
#define AXIS    0

#define PAN	1
#define ROT	2

#define MAX(a,b) ((a) > (b) ? (a) : (b))
#define MIN(a,b) ((a) < (b) ? (a) : (b))

#define DEGRAD  57.29577951308232
#define RADDEG  0.0174532925199432958

typedef struct {
    unsigned short imagic;
    unsigned short type;
    unsigned short dim;
    unsigned short xsize;
    unsigned short ysize;
    unsigned short zsize;
    unsigned int   min;
    unsigned int   max;
    unsigned int   wastebytes;
    char           name[80];
    unsigned int   colormap;
    int            file;
    unsigned short flags;
    short          dorev;
    short          x;
    short          y;
    short          z;
    short          cnt;
    unsigned char  *tmp;
    unsigned char  *tmpR;
    unsigned char  *tmpG;
    unsigned char  *tmpB;
    unsigned long  rleend;              // for RLE images
    unsigned int   *rowstart;           // for RLE images
    int            *rowsize;            // for RLE images
} IMAGE;

/* --------------------------------------------------------------------- */

int             SITwinX, SITwinY;
int             SITwinW, SITwinH;
char            SITlabel[32];
char            *SITdatadir;
Window          SITwinid;
IMAGE           *vfsized;

char            sittemp[128];

static float    ttrans[2];
static float    transx, transy, rotx, roty;
static int      ox = -1, oy = -1;
static int      mot;

void            *Sitfont1 = GLUT_BITMAP_9_BY_15;  /* used fonts */
void            *Sitfont2 = GLUT_BITMAP_8_BY_13;
void            *Sitfont3 = GLUT_STROKE_ROMAN;

extern int      UpdateSit;

FD_sitaware     *fd_sitaware;

/* --------------------------------------------------------------------- */

void SITinit();
void SITshow(int xpos, int ypos, int width, int height, Window mainwinID);
void SITdraw();
void SITexitCB(FL_OBJECT *object, long item_no);
int  SITclose(FL_FORM *form, void *data);
void SITnoneCB(FL_OBJECT *ob, long data);
void SitAwareInit(char *fname);
void SitAwareDisplay(int width, int height);

int SITexposeCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud);

extern void glPutBitmap(GLint x, GLint y, char *string, void *font);
extern void glPutStroke(GLint x, GLint y, char *string, void *font);

/* --------------------------------------------------------------------- */

extern int  EraseActiveEntry(char *text);
extern int  StoreActiveEntry(char *text);
extern int  FinishUp();

/*                                                                       */
/* --------------------------------------------------------------------- */
/*              M A I N   P R O G R A M   S T A R T S   H E R E          */
/* --------------------------------------------------------------------- */
/*                                                                       */
void SITinit()
{
int             i;

   fd_sitaware = create_form_sitaware();

   strcpy(SITlabel, "Situation-Aware");

   fl_set_browser_fontsize(fd_sitaware->sit_threats, 10);
   fl_set_browser_fontstyle(fd_sitaware->sit_threats, FL_FIXED_STYLE|FL_BOLD_STYLE);

   fl_add_canvas_handler(fd_sitaware->sit_canvas, Expose, SITexposeCB, 0);

   return;
}

void SITshow(int xpos, int ypos, int width, int height, Window mainwinID)
{
Window          winid;
char            mapfile[128];
float           point[3], scale;
int             vpwindW, vpwindH;
int             viewport[4];
int             i;
TK_RGBImageRec  *vfimage;
char            filename[80];

   if(!fl_form_is_visible(fd_sitaware->sitaware) ) {
      fl_transient();
      fl_winposition(xpos, ypos);
      fl_initial_winsize(width, height);
      winid = fl_prepare_form_window(fd_sitaware->sitaware,
                                     FL_PLACE_POSITION,FL_TRANSIENT, SITlabel);
      fl_winreshape(winid, xpos, ypos, width, height);
      fl_get_wingeometry(winid, &SITwinX, &SITwinY, &SITwinW, &SITwinH);
      fl_show_form_window(fd_sitaware->sitaware);
      fl_set_form_atclose(fd_sitaware->sitaware, SITclose, 0);
      StoreActiveEntry(SITlabel);
   }

   fl_clear_browser(fd_sitaware->sit_threats);
   if (strcmp(getenv("GPDADOMAIN"), "iow") == 0)
     sprintf(sittemp, "%-8s   %-16s  %4d %4d", "Russia", "www.housecall.ro", 0, 8);
   else if (strcmp(getenv("GPDADOMAIN"), "aoc") == 0)
     sprintf(sittemp, "%-8s   %-16s  %4d   %4d", "DPRK", "  SAM-18", 0, 2);
   fl_addto_browser(fd_sitaware->sit_threats, sittemp);
//
//   Display TopLevel Situational Awareness map, if necessary
//
   if (UpdateSit == 0) {
     //
     //   Hide drawable and just display pixmap
     //
     fl_hide_object(fd_sitaware->sit_canvas);
   } else {
     //
     //   Show drawable and prepare it for OpenGL
     //
     fl_show_object(fd_sitaware->sit_canvas);
     glXMakeCurrent(fl_display, fl_get_canvas_id(fd_sitaware->sit_canvas),
		    fl_get_glcanvas_context(fd_sitaware->sit_canvas)); 
     fl_get_winsize(fl_get_canvas_id(fd_sitaware->sit_canvas), &SITwinW, &SITwinH);
     glViewport(0, 0, SITwinW, SITwinH);

     if (UpdateSit == 1) {
       //
       //   Draw graphics
       //
       SitAwareInit("filename");
       SITdraw();
     } else {
       //
       //   Load and draw bitmap
       //
       if ((SITdatadir = getenv("DATADIR")) == NULL) {
	 SITdatadir = "../RSD_Data";
       }
       sprintf (mapfile, "%s%s", SITdatadir, "/earth-hires.rgb");
       printf("VF: Reading image file %s\n", mapfile);
       vfimage = tkRGBImageLoad(mapfile);
       vfsized = (IMAGE *)malloc(sizeof(IMAGE));
       vfsized->xsize = SITwinW;
       vfsized->ysize = SITwinH;
       vfsized->tmp   = (unsigned char *)malloc(vfsized->xsize*vfsized->ysize*4);
       gluScaleImage(GL_RGB,
		   vfimage->sizeX, vfimage->sizeY, GL_UNSIGNED_BYTE, vfimage->data,
		   vfsized->xsize, vfsized->ysize, GL_UNSIGNED_BYTE, vfsized->tmp);
       free(vfimage->data);
       free(vfimage);

       glClearColor(1.0, 0.0, 0.0, 0.0);
       glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

       glMatrixMode(GL_PROJECTION);
       glLoadIdentity();
       gluOrtho2D(0.0, SITwinW, 0.0, SITwinH);
       glMatrixMode(GL_MODELVIEW);
       glLoadIdentity();
       /*
       point[0] = (vpwindW / 2) - (vfsized->xsize / 2);
       point[1] = (vpwindH / 2) - (vfsized->ysize / 2);
       point[2] = 0;
       glRasterPos3fv(point);
       */
       glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
       glPixelZoom(1.0, 1.0);
       glDrawPixels(vfsized->xsize, vfsized->ysize, GL_RGB, GL_UNSIGNED_BYTE,
		    vfsized->tmp);
     }
   }

   return;
}
void SITexitCB(FL_OBJECT *object, long item_no)
{
int             item = item_no;

   UpdateSit = 0;

   fl_hide_form(fd_sitaware->sitaware);
   EraseActiveEntry(SITlabel);

   FinishUp();

   return;
}

int SITclose(FL_FORM *form, void *data)
{
long            item;

   item = (long)data;
   SITexitCB(NULL, item);

   return(0);
}

void SITnoneCB(FL_OBJECT *ob, long data)
{
   return;
}

void SITdraw()
{
  /*
   sprintf(sittemp, "Intrusions: %s", 
	  fl_get_object_label(fd_bmc3->summ_value[6]));
   fl_set_object_label(fd_sitaware->sit_leakers, sittemp);
//
//   Update the 'CURRENT STATUS' area
//
   fl_set_input(fd_sitaware->sit_defcon, fl_get_input(fd_bmc3->defcon_val));
   fl_set_input(fd_sitaware->sit_dea,    "Free");
   fl_set_input(fd_sitaware->sit_roe,    "CONUS");
  */
//
//   Update the map area
//
   if (fl_form_is_visible(fd_sitaware->sitaware)) {
     glXMakeCurrent(fl_display, fl_get_canvas_id(fd_sitaware->sit_canvas),
		    fl_get_glcanvas_context(fd_sitaware->sit_canvas)); 
     fl_get_winsize(fl_get_canvas_id(fd_sitaware->sit_canvas), &SITwinW, &SITwinH); 
     glViewport(0, 0, SITwinW, SITwinH);

     if (UpdateSit == 1) {
       //
       //   Re-draw graphics
       //
       SitAwareDisplay(SITwinW, SITwinH);
     } else {
       //
       //   Re-draw bitmap
       //
       glMatrixMode(GL_PROJECTION);
       glLoadIdentity();
       gluOrtho2D(0.0, SITwinW, 0.0, SITwinH);
       glMatrixMode(GL_MODELVIEW);
       glLoadIdentity();

       glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
       glPixelZoom(1.0, 1.0);
       glDrawPixels(vfsized->xsize, vfsized->ysize, GL_RGB, GL_UNSIGNED_BYTE,
		   vfsized->tmp);
     }
     glXSwapBuffers(fl_display, fl_get_canvas_id(fd_sitaware->sit_canvas));
   }

   return;
}

void draw_base(void)
{
GLUquadricObj   *qobj;

    glColor3f(0.75, 0.75, 0.75);

    glPushMatrix();
      glScalef(5.5, 6.0, 15);
      glTranslatef(-0.535f, 0.0f, 0.0f);
      glTranslatef(0.0f, -0.4f, 0.0f);
      //
      //   Draw CONUS, Alaska, and Hawaii
      //   
      glCallList(47);
      //
      //   Draw AFIWC Intrusion Attempts
      //
      glPushMatrix();
        glColor3f(0.5, 0.5, 0.0);
        glTranslatef(-98.5/-180.0, 29.5/90.0, 0.0f);
        glScalef(0.01, 0.01, 0.1);
        qobj = gluNewQuadric();
        gluCylinder(qobj, 0.075, 0.075, -0.3, 36, 36);
        gluDeleteQuadric(qobj);
	//
        glScalef(0.005, 0.005, 1.0);
        glRotatef(180.0, 0.0, 1.0, 0.0);
        glPutStroke(0, 0, "AFIWC", Sitfont3);
      glPopMatrix();
      //
      //   Draw SAFB Intrusion Attempts
      //      
      glPushMatrix();
        glColor3f(0.0, 0.5, 0.5);
        glTranslatef(-104.9/-180.0, 38.9/90.0, 0.0f);
        glScalef(0.01, 0.01, 0.1);
        qobj = gluNewQuadric();
        gluCylinder(qobj, 0.075, 0.075, -0.5, 36, 36);
        gluDeleteQuadric(qobj);
        //
        glScalef(0.005, 0.005, 1.0);
        glRotatef(180.0, 0.0, 1.0, 0.0);
        glPutStroke(0, 0, "SAFB", Sitfont3);
      glPopMatrix();

    glPopMatrix();

    return;
}

void SitAwareDisplay(int width, int height)
{
GLfloat         far_cull;

    glClear(GL_COLOR_BUFFER_BIT);
    //
    //   Set up the projection for the legend
    //
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluOrtho2D(0, 400, 400, 0);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    //
    //   Do the legend display
    //
    glColor3f(0.0, 0.0, 0.0);
    glPutBitmap(130, 20, "Situation Awareness", Sitfont1);
    glPutBitmap(135, 35, "(Intrusion Attempts)", Sitfont2);
    //
    //   Set up the projection for the map and overlays
    //
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(17.5, 1.0, 0.1, far_cull = 8.0);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    glTranslatef(0.0, 0.0, -5.5);
    glRotatef(-60.0, 1.0, 0.0, 0.0);
    //
    //   Do the map and overlays
    //
    glPushMatrix();
    glRotatef(180.0, 0.0, 0.0, 1.0);
    glRotatef(180.0, 1.0, 0.0, 0.0);
    draw_base();
    //
    //   Draw an axis if wanted
    //
    if (AXIS) {
      glColor3f(1.0, 0.0, 0.0);
      glBegin(GL_LINES);
        glVertex3f(0.0, 0.0, 0.0);
        glVertex3f(2.0, 0.0, 0.0);
      glEnd();

      glColor3f(0.0, 1.0, 0.0);
      glBegin(GL_LINES);
        glVertex3f(0.0, 0.0, 0.0);
        glVertex3f(0.0, 2.0, 0.0);
      glEnd();

      glColor3f(0.0, 0.0, 1.0);
      glBegin(GL_LINES);
        glVertex3f(0.0, 0.0, 0.0);
        glVertex3f(0.0, 0.0, 2.0);
      glEnd();
    }

    glPopMatrix();

    //glutSwapBuffers();
}

void SitAwareInit(char *fname) {
GLfloat         fog_color[4], fog_density = 0.05, density, far_cull;
unsigned        *image;
int             width, height, components;
const char      *filename = "data/clouds.bw";
char            temp1[24], temp2[24], temp3[24], temp4[24], temp5[24];
char            chmsg[64];
int             rtn, color, stn;
float           f1, f2, f3, f4, f5, lat_shift, lon_shift;
float           time, X, Y, Z, Vx, Vy, Vz, alt, vel;
float           latmax, latmin, lonmax, lonmin;
FILE            *infile, *outfile;
  
    if (strlen(filename) == 0) {
      //image = read_texture(filename, width, height, components);
	if (image == NULL) {
	    fprintf(stderr, "Error: Can't load image file \"%s\".\n",
		    filename);
	    exit(EXIT_FAILURE);
	} else {
	    printf("%d x %d image loaded\n", width, height);
	}
	if (components != 1 && components != 2) {
	    printf("must be a l or la image\n");
	    exit(EXIT_FAILURE);
	}
	if (components == 1) {
	    /* hack for RE */
	    int i;
	    GLubyte *p = (GLubyte *)image;
	    for(i = 0; i < width*height; i++) {
		p[i*4+3] = p[i*4+0];
	    }
	    components = 2;
	}
    } else {
	int i, j;
	unsigned char *img;
	components = 4; width = height = 512;
	image = (unsigned *) malloc(width*height*sizeof(unsigned));
	img = (unsigned char *)image;
	for (j = 0; j < height; j++)
	    for (i = 0; i < width; i++) {
		int w2 = width/2, h2 = height/2;
		if (i & 32)
		    img[4*(i+j*width)+0] = 0xff;
		else
		    img[4*(i+j*width)+1] = 0xff;
		if (j&32)
		    img[4*(i+j*width)+2] = 0xff;
		if ((i-w2)*(i-w2) + (j-h2)*(j-h2) > 64*64 &&
		    (i-w2)*(i-w2) + (j-h2)*(j-h2) < 300*300) img[4*(i+j*width)+3] = 0xff;
	    }

	infile = fopen("statebound.dat", "r");

	glNewList(47, GL_COMPILE_AND_EXECUTE);

	while (!feof(infile)) {
	  glPushMatrix();
	  int iprint = 0;
	  lat_shift = 0.0;
	  lon_shift = 0.0;
	  fscanf(infile, "%d %s\n", &rtn, chmsg);
	  //printf("State %s has %d points\n", chmsg, rtn);
	  if (strstr(chmsg, "Alaska") != NULL) {
	    lat_shift = -20.0;
	    lon_shift = -70.0;
	    glScalef(0.5, 0.5, 1.0);
	  }
	  if (strstr(chmsg, "Hawaii") != NULL) {
	    lat_shift = -2.0;
	    lon_shift = 62.5;
	    iprint = 1;
	    //glScalef(0.5, 0.5, 1.0);
	  }
	  glBegin(GL_LINE_STRIP);
	  for (i=0; i<rtn; i++) {
	    fscanf(infile, "%f %f", &f1, &f2);
	    glVertex2f((f2+lon_shift)/-180.0, (f1+lat_shift)/90.0);
	    //if (iprint) printf(" %f  %f\n", (f2+lon_shift), (f1+lat_shift));
	  }
	  glEnd();

	  glPopMatrix();
	}
	fclose(infile);

	glColor3f(0.0, 0.0, 0.0);
	glLineWidth(2.0f);
	glBegin(GL_LINE_STRIP);
	  glVertex2f(-150.0/-180.0, 27.0/90.0);
	  glVertex2f(-105.0/-180.0, 27.0/90.0);
	  glVertex2f(-100.0/-180.0, 22.0/90.0);
	  glVertex2f(-100.0/-180.0, 10.0/90.0);
	glEnd();
	glBegin(GL_LINE_STRIP);
	  glVertex2f(-100.0/-180.0, 22.0/90.0);
	  glVertex2f(-90.0/-180.0,  22.0/90.0);
	  glVertex2f(-90.0/-180.0,  10.0/90.0);
	glEnd();
	glLineWidth(1.0f);

	glEndList();
    }

    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexImage2D(GL_TEXTURE_2D, 0, components, width,
                 height, 0, GL_RGBA, GL_UNSIGNED_BYTE,
                 image);
    /*glEnable(GL_TEXTURE_2D);*/
    /*
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(17.5, 1.0, 0.1, far_cull = 8.0);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    glTranslatef(0.0, 0.0, -5.5);
    glRotatef(-60.0, 1.0, 0.0, 0.0);
    */
    glClearColor(1.0, 1.0, 1.0, 0.0);
    
    if (FOG) {
      //
      //   Set up the fog stuff
      //
      density = 1.- expf(-5.5 * fog_density * fog_density *
			 far_cull * far_cull);

      density = MAX(MIN(density, 1.), 0.);

      fog_color[0] = .23*.2 + density *.57*.2;
      fog_color[1] = .35*.2 + density *.45*.2;
      fog_color[2] = .78*.5 + density *.22*.2;

      glClearColor(fog_color[0], fog_color[1], fog_color[2], 1.f);
    
      glFogi(GL_FOG_MODE, GL_EXP2);
      glFogf(GL_FOG_DENSITY, fog_density);
      glFogfv(GL_FOG_COLOR, fog_color);
      if (fog_density > 0)
	glEnable(GL_FOG);
      //glLineWidth(2.0f);
      glEnable(GL_LINE_SMOOTH);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glPointSize(10.f);
      glEnable(GL_POINT_SMOOTH);
      glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);
    }   
}
/*                                                                       */
/* --------------------------------------------------------------------- */
/*      C A L L B A C K    R O U T I N E S    S T A R T    H E R E       */
/* --------------------------------------------------------------------- */
/*                                                                       */
void sitoverlayCB(FL_OBJECT *ob, long data)
{
  /* fill-in code for callback */
}

//<------------------------expose-------------------------------------->
//Run when the opengl screen needs to be refreshed 'cause it is "expose"
int SITexposeCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud)
{

  //SITdraw();

  return(0);
}
