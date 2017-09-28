#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include <GL/glut.h>
#include "glm.h"

#include "skyfly.h"

#define SKY_R 0.23f
#define SKY_G 0.35f
#define SKY_B 0.78f

#define TERR_DARK_R 0.27f
#define TERR_DARK_G 0.18f
#define TERR_DARK_B 0.00f

#define TERR_LITE_R 0.24f
#define TERR_LITE_G 0.53f
#define TERR_LITE_B 0.05f

typedef struct {
  unsigned char red;
  unsigned char green;
  unsigned char blue;
} palette_t;

/****************************************************************************/
/********************************* GLOBALS **********************************/
/****************************************************************************/

palette_t       pal[256];
static int      buttons[BUTCOUNT] = { 0 };
static int      mouse_x, mouse_y;
int             TerrainType = TERR_DTED;
int             Keyboard_mode;
float           Speed = 0.01, DeltaSpeed = .001, BaseSpeed = 0.005;
char            chmode[2][8] = { "Running", "Paused" };

int             numvertices, numtriangles, numnormals, numtexcoords;
int             numgroups, nummaterials;
char*           pathname;

struct Air      models[1000];
int             airlist;
int             n_aircraft = 0;
int             AirFriend  = 1;
int             AirHostile = 1;
int             AirUnknown = 1;

GLuint          model_list = 0;                   /* display list for object */
char            model_file[128];                  /* name of the object file */
GLboolean       facet_normal = GL_FALSE;          /* draw with facet normal? */
GLMmodel*       model;
GLfloat         smoothing_angle = 90.0;           /* smoothing angle */
GLfloat         scale;                            /* scaling factor */
GLuint          material_mode = 0;
GLUquadricObj*  circleobj;

GLboolean       show_timer   = GL_TRUE;
GLboolean       show_stats   = GL_FALSE;
GLboolean       fullscreen   = GL_TRUE;
GLboolean       usetexture   = GL_TRUE;
GLboolean       bounding_box = GL_FALSE;
GLboolean       RUNMODE      = GL_FALSE;

/****************************************************************************/
/****************************** PROTOTYPES **********************************/
/****************************************************************************/

void init_misc(void);
void init_skyfly(int);
void sim_singlechannel(void);
void cull_proc(void);
void draw_proc(void);
void ModelLists(void);
extern float get_elevation(int i, int j);

/****************************************************************************/
/************************** INITIALIZATION **********************************/
/****************************************************************************/
void
ModelLoad(void)
{
int             i, ii, jj;
FILE            *airfp;
int             mtype, fixed, n_air;
float           xpos, ypos, zpos, speed, heading, scale;
char            *MODELDIR;
char            chtype[4];
char            chowner[16];
char            chmodel[64];
char            filename[128];
/*
 *   Initialize the aircraft and ground models
*/
    if ((MODELDIR = getenv("MODELDIR")) == NULL) MODELDIR = "./Models";
    airlist = glGenLists(1000);
    airfp = fopen("initmodels.dat", "r");
    fscanf(airfp, "%d\n", &n_air);
    for (i=0; i<n_air; i++) {
      fscanf(airfp, "%s %s %d %d %f %f %f %f %f %f %s\n", chmodel, chtype, &mtype, &fixed,
              &xpos, &ypos, &zpos, &speed, &heading, &scale, chowner);
       models[n_aircraft].Xpos    = xpos;
       models[n_aircraft].Ypos    = ypos;
       models[n_aircraft].Zpos    = zpos;
       models[n_aircraft].Speed   = speed;
       models[n_aircraft].Heading = heading;
       models[n_aircraft].Fixed   = fixed;
       models[n_aircraft].Scale   = scale;
       if (strcmp(chowner, "FRIEND") == 0)  models[n_aircraft].Owner  = FRIEND;
       else if (strcmp(chowner, "HOSTILE") == 0) models[n_aircraft].Owner  = HOSTILE;
       else models[n_aircraft].Owner  = UNKNOWN;
       printf("%s %s %d %d %f %f %f %f %f %f %s\n", chmodel, chtype, mtype, fixed,
              xpos, ypos, zpos, speed, heading, scale, chowner);

       if (strcmp(chtype, "ogl") == 0) {
         sprintf(filename, "%s/%s.model", MODELDIR, chmodel);
         load_model(filename, mtype, airlist+n_aircraft);
         models[n_aircraft].listid  = airlist+n_aircraft;
         n_aircraft = n_aircraft+1;
         pathname     = filename;                 /* Save the statistics */
         numvertices  = 9999;
         numtriangles = 9999;
         numnormals   = 9999;
         numtexcoords = 0;
         numgroups    = 1;
         nummaterials = 0;
       } else if (strcmp(chtype, "obj") == 0) {
	 sprintf(filename, "%s.%s", chmodel, chtype);
         model = glmReadOBJ(filename);            /* Read in the model */
         scale = glmUnitize(model);
         glmFacetNormals(model);
         glmVertexNormals(model, smoothing_angle);
         if (model->nummaterials > 0) material_mode = 2;
         ModelLists();                            /* Create new display lists */
         models[n_aircraft].listid  = model_list;
         n_aircraft = n_aircraft+1;
         pathname     = model->pathname;          /* Save the statistics */
         numvertices  = model->numvertices;
         numtriangles = model->numtriangles;
         numnormals   = model->numnormals;
         numtexcoords = model->numtexcoords;
         numgroups    = model->numgroups;
         nummaterials = model->nummaterials;
       }

    }
    fclose(airfp);
}

void
ModelLists(void)
{
GLfloat         ambient[] = { 0.2, 0.2, 0.2, 1.0 };
GLfloat         diffuse[] = { 0.8, 0.8, 0.8, 1.0 };
GLfloat         specular[] = { 0.0, 0.0, 0.0, 1.0 };
GLfloat         shininess = 65.0;

   if (model_list)
     glDeleteLists(model_list, 1);

   glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, ambient);
   glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, diffuse);
   glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, specular);
   glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, shininess);
  
   /* generate a list */
   if (material_mode == 0) {
     if (facet_normal)
       model_list = glmList(model, GLM_FLAT);
     else
       model_list = glmList(model, GLM_SMOOTH);
   } else if (material_mode == 1) {
     if (facet_normal)
       model_list = glmList(model, GLM_FLAT | GLM_COLOR);
      else
      model_list = glmList(model, GLM_SMOOTH | GLM_COLOR);
   } else if (material_mode == 2) {
      if (facet_normal)
      model_list = glmList(model, GLM_FLAT | GLM_MATERIAL);
     else
       model_list = glmList(model, GLM_SMOOTH | GLM_MATERIAL);
   }
}

/****************************************************************************/
/******************************* ROUTINES ***********************************/
/****************************************************************************/

/* text: general purpose text routine.  draws a string according to
 * format in a stroke font at x, y after scaling it by the scale
 * specified (scale is in window-space (lower-left origin) pixels).
 *
 * x      - position in x (in window-space)
 * y      - position in y (in window-space)
 * scale  - scale in pixels
 * format - as in printf()
 */
void
text(GLuint x, GLuint y, GLfloat scale, char* format, ...)
{
va_list         args;
char            buffer[255], *p;
GLfloat         font_scale = 119.05 + 33.33;

   va_start(args, format);
   vsprintf(buffer, format, args);
   va_end(args);

   glMatrixMode(GL_PROJECTION);
   glPushMatrix();
   glLoadIdentity();
   gluOrtho2D(0, glutGet(GLUT_WINDOW_WIDTH), 0, glutGet(GLUT_WINDOW_HEIGHT));

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();
   glLoadIdentity();

   glPushAttrib(GL_ENABLE_BIT);
   glDisable(GL_LIGHTING);
   glDisable(GL_TEXTURE_2D);
   glDisable(GL_DEPTH_TEST);
   glTranslatef(x, y, 0.0);

   glScalef(scale/font_scale, scale/font_scale, scale/font_scale);

   for(p = buffer; *p; p++)
     glutStrokeCharacter(GLUT_STROKE_ROMAN, *p);

   glPopAttrib();

   glPopMatrix();
   glMatrixMode(GL_PROJECTION);
   glPopMatrix();
   glMatrixMode(GL_MODELVIEW);
}

int Xgetbutton(int button)
{
int             b;

   if (button < 0 || button >= BUTCOUNT) return -1;

   b = buttons[button];
   if (button < LEFTMOUSE) buttons[button] = 0;
   return b;
}

int Xgetvaluator(int val)
{
   switch (val) {
     case MOUSEX:
       return mouse_x;

     case MOUSEY:
       return mouse_y;

     default:
       return -1;
   }
}

void setPaletteIndex(int i, GLfloat r, GLfloat g, GLfloat b)
{
   pal[i].red   = (unsigned char)(255.0F * r);
   pal[i].green = (unsigned char)(255.0F * g);
   pal[i].blue  = (unsigned char)(255.0F * b);      
}                                                           

void init_cmap(void)
{
int 		ii, jj, color;
GLfloat 	r0, g0, b0, r1, g1, b1;

   /* Set up color map */
   color = 10;
   memset(pal,0,sizeof(pal));

   /* Sky colors */
   sky_base = color;
   r0 = SKY_R; r1 = 1.0f;
   g0 = SKY_G; g1 = 1.0f;
   b0 = SKY_B; b1 = 1.0f;
   for (ii = 0; ii < SKY_COLORS; ii++) {
        GLfloat p, r, g, b;
        p = (GLfloat) ii / (SKY_COLORS-1);
        r = r0 + p * (r1 - r0);
        g = g0 + p * (g1 - g0);
        b = b0 + p * (b1 - b0);
        for (jj = 0; jj < FOG_LEVELS; jj++) {
            GLfloat fp, fr, fg, fb;
            fp = (FOG_LEVELS > 1) ? (GLfloat) jj / (FOG_LEVELS-1) : 0.0f;
            fr = r + fp * (fog_params[0] - r);
            fg = g + fp * (fog_params[1] - g);
            fb = b + fp * (fog_params[2] - b);
            setPaletteIndex(sky_base + (ii*FOG_LEVELS) + jj, fr, fg, fb);
        }
   }
   color += (SKY_COLORS * FOG_LEVELS);

   /* Terrain colors */
   terr_base = color;
   r0 = TERR_DARK_R;   r1 = TERR_LITE_R;
   g0 = TERR_DARK_G;   g1 = TERR_LITE_G;
   b0 = TERR_DARK_B;   b1 = TERR_LITE_B;
   for (ii = 0; ii < TERR_COLORS; ii++) {
        GLfloat p, r, g, b;
        p = (GLfloat) ii / (TERR_COLORS-1);
        r = r0 + p * (r1 - r0);
        g = g0 + p * (g1 - g0);
        b = b0 + p * (b1 - b0);
        for (jj = 0; jj < FOG_LEVELS; jj++) {
            GLfloat fp, fr, fg, fb;
            fp = (FOG_LEVELS > 1) ? (GLfloat) jj / (FOG_LEVELS-1) : 0.0f;
            fr = r + fp * (fog_params[0] - r);
            fg = g + fp * (fog_params[1] - g);
            fb = b + fp * (fog_params[2] - b);
            setPaletteIndex(terr_base + (ii*FOG_LEVELS) + jj, fr, fg, fb);
        }
   }
   color += (TERR_COLORS * FOG_LEVELS);

   /* Plane colors */
   plane_colors[0] = color;
   plane_colors[1] = color + (PLANE_COLORS/2);
   plane_colors[2] = color + (PLANE_COLORS-1);
   r0 = 0.4; r1 = 0.8;
   g0 = 0.4; g1 = 0.8;
   b0 = 0.1; b1 = 0.1;
   for (ii = 0; ii < PLANE_COLORS; ii++) {
        GLfloat p, r, g, b;
        p = (GLfloat) ii / (PLANE_COLORS);
        r = r0 + p * (r1 - r0);
        g = g0 + p * (g1 - g0);
        b = b0 + p * (b1 - b0);
        setPaletteIndex(plane_colors[0] + ii, r, g, b);
   }
   color += PLANE_COLORS;
#if 0
   GM_setPalette(pal,256,0);
   GM_realizePalette(256,0,true);
#endif
}

void draw(void);

void gameLogic(void)
{
   sim_singlechannel();
   if (RUNMODE) draw();
}

/* When not visible, stop animating.  Restart when visible again. */
static void 
visible(int vis)
{
   if (vis == GLUT_VISIBLE) {
     glutIdleFunc(gameLogic);
   } else {
     glutIdleFunc(NULL);
   }
}

static void
DisplayModels(void)
{
int             i, j, ii, jj;
int             showair = 1;
float           scale;
extern float    XYScale;

   glPushAttrib(GL_ALL_ATTRIB_BITS);
/*
 *   Setup GL environment
 */
   glEnable (GL_LIGHTING);
   glEnable (GL_LIGHT0);
   glEnable(GL_COLOR_MATERIAL);
   glColor3f(0.5, 0.5, 0.5);
/*  
 *   Draw the models if any available
 */
   if (showair && n_aircraft>0) {
     for (i=0; i<n_aircraft; i++) { 
       if (AirFriend   &&  (models[i].Owner==FRIEND) ||
           AirHostile  &&  (models[i].Owner==HOSTILE) ||
           AirUnknown  &&  (models[i].Owner==UNKNOWN)) {
         glPushMatrix();
         ii = models[i].Xpos; jj = models[i].Ypos;
         models[i].Zpos = get_elevation(ii, jj);
	 models[i].Zpos = 3.2;
         glTranslatef(models[i].Xpos, models[i].Ypos, models[i].Zpos);
         scale = models[i].Scale;
         glScalef(scale, scale, scale);
	 glRotatef(90.0, 1.0, 0.0, 0.0);
         glCallList(model_list /*models[i].listid*/);
         glPopMatrix();
       } 
     }
   }

   if (bounding_box) {
     glEnable(GL_BLEND);
     glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
     glEnable(GL_CULL_FACE);
     glColor4f(1.0, 0.0, 0.0, 0.25);
     glutSolidCube(2.0);
   }

   glPopAttrib();
}

int             lastCount;      /* Timer count for last fps update */
int             frameCount;     /* Number of frames for timing */
int             fpsRate;        /* Current frames per second rate */

void draw(void)
{
int             newCount;
char            buf[200];
int             i, len;
 
   /* Draw the frame */
   cull_proc();
   draw_proc();

   DisplayModels();

   /* Update the frames per second count if we have gone past at least
      a quarter of a second since the last update. */
   newCount = glutGet(GLUT_ELAPSED_TIME);
   frameCount++;
   if ((newCount - lastCount) > 1000) {
     fpsRate = (int) ((10000.0f / (newCount - lastCount)) * frameCount);
     lastCount = newCount;
     frameCount = 0;
   }
   if (show_timer) {
     sprintf(buf,"%3d.%d fps   [%-6s]   ESC->Exit   G->Start   RtBtn->Menu",
	    fpsRate / 10, fpsRate % 10, chmode[Keyboard_mode]);
     glPushAttrib(GL_ENABLE_BIT | GL_CURRENT_BIT);
     glDisable(GL_LIGHTING);
     glDisable(GL_TEXTURE_2D);
     glDisable(GL_DEPTH_TEST);
     glDisable(GL_FOG);
     glDisable(GL_BLEND);
     glMatrixMode(GL_PROJECTION);
     glPushMatrix();
     glLoadIdentity();
     glOrtho(0, Wxsize, 0, Wysize, -1, 1);
     glMatrixMode(GL_MODELVIEW);
     glPushMatrix();
     glLoadIdentity();

     glColor3f(1.0f, 1.0f, 0.0f);
     glRasterPos2i(10, 10);
     len = strlen(buf);
     for (i = 0; i < len; i++)
       glutBitmapCharacter(GLUT_BITMAP_TIMES_ROMAN_24, buf[i]);

     glLoadIdentity();
     glTranslatef(Wxsize*0.95, Wysize*0.05, 0.0);
     gluDisk(circleobj, 0.0, (GLdouble)Wysize/25.0, (GLint)180, (GLint)1);
     //X = cosf(-Azimuth + M_PI/2.0) * Wysize/25.0;
     glColor3f(0.0, 0.0, 0.0);
     glLineWidth(2);
     glBegin(GL_LINES);
     glVertex3f(0.0, 0.0, 0.0);
     glVertex3f(0.0, Wysize/25.0, 0.0);
     glEnd();
     glLineWidth(1);

     glMatrixMode(GL_PROJECTION);
     glPopMatrix();
     glMatrixMode(GL_MODELVIEW);
     glPopMatrix();
     glPopAttrib();
   }

   if (show_stats) {
     glColor3f(1.0, 1.0, 1.0);
     text(5, glutGet(GLUT_WINDOW_HEIGHT) - (5+20*1), 20, "Model: %s",
          pathname);
     text(5, glutGet(GLUT_WINDOW_HEIGHT) - (5+20*2), 20, "Vertices: %d",
          numvertices);
     text(5, glutGet(GLUT_WINDOW_HEIGHT) - (5+20*3), 20, "Triangles: %d",
          numtriangles);
     text(5, glutGet(GLUT_WINDOW_HEIGHT) - (5+20*4), 20, "Normals: %d",
          numnormals);
     text(5, glutGet(GLUT_WINDOW_HEIGHT) - (5+20*5), 20, "Texcoords: %d",
          numtexcoords);
     text(5, glutGet(GLUT_WINDOW_HEIGHT) - (5+20*6), 20, "Groups: %d",
          numgroups);
     text(5, glutGet(GLUT_WINDOW_HEIGHT) - (5+20*7), 20, "Materials: %d",
          nummaterials);
   }

   glutSwapBuffers();
}

void
reshape(int width, int height)
{
   Wxsize = width;
   Wysize = height;

   mouse_x = Wxsize/2;
   mouse_y = Wysize/2;

   glViewport(0, 0, width, height);
}

/* ARGSUSED1 */
void
keyboard(unsigned char c, int x, int y)
{

    if (c >= '0' && c <= '9') {
      Speed = BaseSpeed * (float)((c-'0')*2);
    } else {
      switch (c) {
      case 0x1B:                                            // Exit
      case 'q':
      case 'Q':
        exit(0);
        break;

      case 'd':                                             // Dither toggle
        set_dither(!dither);
        break;
      case 'f':                                             // Fog toggle
        set_fog(!fog);
        break;
      case 'g':                                             // Start animation
      case 'G':
        RUNMODE = GL_TRUE;
        break;
      case 'r':                                             // Reset
        buttons[RKEY] = 1;
        break;
      case 's':                                             // Statistics toggle
        show_stats = !show_stats;
        break;
      case 't':                                             // FPS display toggle
        show_timer = !show_timer;
        break;
      case 'x':                                             // Texture toggle
        usetexture = !usetexture;
        break;

      case '.':                                             // Not Used
        buttons[PERIODKEY] = 1;
        break;
      case ' ':                                             // Pause
        buttons[SPACEKEY] = 1;
        break;
      case '+':                                             // Increase speed by delta
        if (Speed < 0.1) Speed += DeltaSpeed;                    // Max speed is 0.1
        break;
      case '-':                                             // Decrease speed by delta
        if (Speed > 0.0) Speed -= DeltaSpeed;                    // Min speed is 0.0
        break;
      case '/':                                             // Decrease speed by 1/2
        Speed = Speed*0.5;
        break;
      case '*':                                             // Increase speed by 2
        Speed = Speed*2.0;
        break;
      }
    }
}

void mouse(int button, int state, int x, int y)
{
   mouse_x = x;
   mouse_y = y;
   switch (button) {
     case GLUT_LEFT_BUTTON:
        buttons[LEFTMOUSE] = (state == GLUT_DOWN);
        break;
     case GLUT_MIDDLE_BUTTON:
        buttons[MIDDLEMOUSE] = (state == GLUT_DOWN);
        break;
     case GLUT_RIGHT_BUTTON:
        buttons[RIGHTMOUSE] = (state == GLUT_DOWN);
        break;
   }
}

void motion(int x, int y)
{
   mouse_x = x;
   mouse_y = y;
}


/* ARGSUSED1 */
void special(int key, int x, int y)
{
   switch (key) {
    case GLUT_KEY_LEFT:
        buttons[LEFTARROWKEY] = 1;
        break;
    case GLUT_KEY_UP:
        buttons[UPARROWKEY] = 1;
        break;
    case GLUT_KEY_RIGHT:
        buttons[RIGHTARROWKEY] = 1;
        break;
    case GLUT_KEY_DOWN:
        buttons[DOWNARROWKEY] = 1;
        break;
    case GLUT_KEY_PAGE_UP:
        buttons[PAGEUPKEY] = 1;
        break;
    case GLUT_KEY_PAGE_DOWN:
        buttons[PAGEDOWNKEY] = 1;
        break;
   }
}

/****************************************************************************/
/**************************    MENU  HANDLING   *****************************/
/****************************************************************************/

void
terraintypeCB(int value)
{
   TerrainType = value;
   init_skyfly(TerrainType);
}

void
setlevelCB(int value)
{

}


void
MenuCB(int item)
{
   switch (item) {

     case 'd':
       keyboard('d', 0, 0);
       break;

     case 'f':
       keyboard('f', 0, 0);
       break;

     case 'g':
       keyboard('g', 0, 0);
       break;

     case 'q':
       exit(0);
       break;

     case 'r':
       keyboard('r', 0, 0);
       break;

     case 't':
       keyboard('t', 0, 0);
       break;

     case 'x':
       keyboard('x', 0, 0);
       break;

     default:
       break;
   }
}

/****************************************************************************/
/**************************    MAIN GLUT LOOP   *****************************/
/****************************************************************************/

int
main(int argc, char **argv)
{
int             mainmenu, submenu1, submenu2;

   glutInit(&argc, argv);

   if (argc > 1 && !strcmp(argv[1], "-w"))
      fullscreen = GL_FALSE;

   glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE | GLUT_DEPTH | GLUT_MULTISAMPLE);
   if (fullscreen) {
      glutGameModeString("640x480:16@60");
      glutEnterGameMode();
   } else {
      glutInitWindowSize(400, 400);
      glutCreateWindow("GLUT-based OpenGL skyfly");
   }

   submenu1 = glutCreateMenu(setlevelCB);
   glutAddMenuEntry("0", 0);  glutAddMenuEntry("1", 1);
   glutAddMenuEntry("2", 2);  glutAddMenuEntry("3", 3);
   glutAddMenuEntry("4", 4);  glutAddMenuEntry("5", 5);
   glutAddMenuEntry("6", 6);  glutAddMenuEntry("7", 7);
   glutAddMenuEntry("8", 8);

   submenu2 = glutCreateMenu(terraintypeCB);
   glutAddMenuEntry("Sinusoidal",                  TERR_SINE);
   glutAddMenuEntry("Fractal",                     TERR_FRAC);
   glutAddMenuEntry("3D DTED",                     TERR_DTED);
   glutAddMenuEntry("USGS DEM",                    TERR_USGS);

   mainmenu = glutCreateMenu(MenuCB);
   //glutAddSubMenu("Terrain Type...",               submenu2);
   glutAddSubMenu("Fractal Level...",              submenu1);
   glutAddMenuEntry("Toggle Dithering On/Off",     'd');
   glutAddMenuEntry("Toggle Fog On/Off",           'f');
   glutAddMenuEntry("Toggle Texture On/Off",       'x');
   glutAddMenuEntry("Toggle Frame Rate On/Off",    't');
   glutAddMenuEntry("Start Animation",             'g');
   glutAddMenuEntry("Reset Animation",             'r');
   glutAddMenuEntry("Quit",                        'q');
   glutAttachMenu(GLUT_RIGHT_BUTTON);

   glutDisplayFunc(draw);
   glutReshapeFunc(reshape);
   glutVisibilityFunc(visible);
   glutKeyboardFunc(keyboard);
   glutMouseFunc(mouse);
   glutMotionFunc(motion);
   glutPassiveMotionFunc(motion);
   glutSpecialFunc(special);

   init_misc();
   if (!rgbmode) init_cmap();

   if (argc > 1) {
     if (strcmp(argv[1], "dted") == 0) TerrainType = TERR_DTED;
     if (strcmp(argv[1], "usgs") == 0) TerrainType = TERR_USGS;
     if (strcmp(argv[1], "frac") == 0) TerrainType = TERR_FRAC;
     if (strcmp(argv[1], "sine") == 0) TerrainType = TERR_SINE;
   }
   init_skyfly(TerrainType);

   ModelLoad();

   circleobj = gluNewQuadric();
   gluQuadricDrawStyle(circleobj, (GLenum)GLU_FILL);

   glutMainLoop();
   return 0;             /* ANSI C requires main to return int. */
}
