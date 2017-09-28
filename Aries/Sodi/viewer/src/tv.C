
#include <math.h>
#include <sys/types.h>
#include <malloc.h>
#include <stdio.h>
#include <stdlib.h> 
#include <string.h>

#include "GR_Shell.H"
#include "GR_Model.H"
#include "texture.H"

#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/MessageB.h>
#include <Xm/List.h>
#include <Xm/Label.h>
#include <Xm/Separator.h>

#define RE 6378.145

#define MAX(a,b) ((a) > (b) ? (a) : (b))
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define expf(x)  ((float)exp((x)))

Widget		tv_shell;
GR_Window	*tvwindow;
Boolean		tvfirst = TRUE;
extern int	VMODE, v_LAT, v_LON, v_FOV, v_ALT, v_AZI;
int		tvwindW=200, tvwindH=260;
unsigned char   *tvdigits[15];
int		tvdigitX, tvdigitY;

static float ttrans[2] = { 0.0, 0.0 };
static float scale = 1.0;
static float transx = 0.0, transy = 0.0, rotx = 0.0, roty = 0.0;

void test_viewerCB (Widget);
void tv_doneCB ();
void tvdraw ();
//unsigned *read_texture(const char *name, int *width, int *height, int *components);
//unsigned *read_texture(char *name, int *width, int *height, int *components);

void
tv_init ()
{
   Widget tv_form;
   Widget tv_list; 
   Widget tv_frame;
   Widget tv_control, done_button, test_button, Lcd1;
   Widget septitle;
   Widget nodeidstr00, nodeidstr01;
   XmString *xstr, title;
   XmString     nodenoid;
   int i;


   tv_shell = XtCreatePopupShell("TestViewer", topLevelShellWidgetClass,
                                GR_toplevel, 0, 0);
   tv_form = XmCreateForm (tv_shell, "TVForm", NULL, 0);
   XtVaSetValues(tv_form,
                 XmNwidth,            300,
                 XmNheight,           320,
                 NULL);
 
   done_button = XtVaCreateManagedWidget ("Quit",
                 xmPushButtonWidgetClass, tv_form,
                 XmNwidth,            60,
                 XmNheight,           40,
                 XmNshadowThickness,  4,
                 NULL);
   XtVaSetValues (done_button,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_FORM,
                 NULL);
   XtAddCallback (done_button, XmNactivateCallback,
                    (XtCallbackProc)tv_doneCB, NULL);

   septitle = XtVaCreateManagedWidget("VertSep", xmSeparatorWidgetClass, tv_form,
                 XmNwidth,            10,
                 XmNheight,           300,
                 XmNorientation,      XmVERTICAL,
                 XmNseparatorType,    XmSHADOW_ETCHED_IN,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       done_button,
                 XmNtopAttachment,    XmATTACH_FORM,
                 NULL);

   tv_frame = XtVaCreateManagedWidget ("TVFrame",
               xmFrameWidgetClass, tv_form,
               XmNwidth,              tvwindW,
               XmNheight,             tvwindH,
               XmNshadowType,         XmSHADOW_IN,
               XmNtopAttachment,      XmATTACH_FORM,
               XmNleftAttachment,     XmATTACH_WIDGET,
               XmNleftWidget,         done_button,
               NULL);

   tvwindow = new GR_Window ();
   tvwindow->doublebuffer ();
   tvwindow->rgbmode ();
   tvwindow->GR_Widget::createWidget ("TVWindow", tv_frame);

   tvwindow->set_viewmode (GR_ORTHO2);
   tvwindow->left (-1.0);
   tvwindow->right (+1.0);
   tvwindow->bottom (-1.0);
   tvwindow->top (+1.0);

   tvwindow->set_drawfunction(tvdraw);

   XtManageChild (tv_form);
   XtPopup(tv_shell, XtGrabNone);
}

void
test_viewerCB (Widget toplevel)
{
   if (tvfirst) {
      GR_toplevel = toplevel;
      tvfirst = FALSE;
      tv_init ();
   } else {
      XtPopup(tv_shell, XtGrabNone);
   }
}

void
tv_doneCB ()
{
  XtPopdown (tv_shell);
//  tvwindow->set_awake (FALSE);
}

void display(void) {
    glClear(GL_COLOR_BUFFER_BIT);
    /*
    glPushMatrix();
       glTranslatef(transx, transy, 0.0f);
       glRotatef(rotx, 0.0, 1.0, 0.0);
       glRotatef(roty, 1.0, 0.0, 0.0);
       glScalef(scale, scale, 1.0);
       glScalef(10, 1, 10);
       glColor3f(0.19, 0.25, 0.70);
       glMatrixMode(GL_TEXTURE);
       glPushMatrix();
          glTranslatef(ttrans[0], ttrans[1], 0.);
          glBegin(GL_QUADS);
             glTexCoord2f(0, 0); glVertex3f(-1.0, 1.0, -1.0);
             glTexCoord2f(0, 5); glVertex3f(-1.0, 1.0,  1.0);
             glTexCoord2f(5, 5); glVertex3f( 1.0, 1.0,  1.0);
             glTexCoord2f(5, 0); glVertex3f( 1.0, 1.0, -1.0);
          glEnd();
       glPopMatrix();
       glMatrixMode(GL_MODELVIEW);
    glPopMatrix();
    //glutSwapBuffers();
    */
    glEnable(GL_BLEND);
    glEnable(GL_CULL_FACE);
    glCullFace(GL_FRONT);
          glBegin(GL_QUADS);
             glTexCoord2f(0, 0); glVertex3f(-1.0, 1.0, -1.0);
             glTexCoord2f(0, 5); glVertex3f(-1.0, 1.0,  1.0);
             glTexCoord2f(5, 5); glVertex3f( 1.0, 1.0,  1.0);
             glTexCoord2f(5, 0); glVertex3f( 1.0, 1.0, -1.0);
          glEnd();
    glDisable(GL_CULL_FACE);
    glDisable(GL_BLEND);
}

void tvdraw ()
{
GLfloat   cloud_color[4] = { 1.0, 1.0, 1.0, 0.0 };
GLfloat   fog_color[4], fog_density = 0.05, density, far_cull;
unsigned  *image;
int       width, height, components;
char      fname[20];
char      *filename;

    filename = strcpy(fname, "clouds.bw");
    if (filename) {
        image = read_texture(filename, &width, &height, &components);
        if (image == NULL) {
            fprintf(stderr, "Error: Can't load image file \"%s\".\n",
                    filename);
            return;
        } else {
            printf("%d x %d image loaded\n", width, height);
        }
        if (components != 1) {
            printf("must be a bw image\n");
            return;
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
    }


   GR_pushattributes ();

    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_BLEND);
    glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, cloud_color);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexImage2D(GL_TEXTURE_2D, 0, components, width,
                 height, 0, GL_RGBA, GL_UNSIGNED_BYTE,
                 image);
    glEnable(GL_TEXTURE_2D);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(50.0, 1.0, 0.1, far_cull = 10.0);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    glTranslatef(0.0, 0.0, -5.5);
    /*
    density = 1.0- expf(-5.5 * fog_density * fog_density *
                              far_cull * far_cull);

    density = MAX(MIN(density, 1.0), 0.0);

    fog_color[0] = 0.23 + density *0.57;
    fog_color[1] = 0.35 + density *0.45;
    fog_color[2] = 0.78 + density *0.22;

    glClearColor(fog_color[0], fog_color[1], fog_color[2], 1.0f);

    glFogi(GL_FOG_MODE, GL_EXP2);
    glFogf(GL_FOG_DENSITY, fog_density);
    glFogfv(GL_FOG_COLOR, fog_color);
    if (fog_density > 0)
        glEnable(GL_FOG);
    */
    display();

   GR_popattributes ();
}
