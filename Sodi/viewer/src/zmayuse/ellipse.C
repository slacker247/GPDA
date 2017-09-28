
#include <GL/glut.h>
#include <stdlib.h>
#include <math.h>

#define drawOneLine(x1,y1,x2,y2)  glBegin(GL_LINES);  \
   glVertex2f ((x1),(y1)); glVertex2f ((x2),(y2)); glEnd();

void init(void) 
{
   glClearColor (0.0, 0.0, 0.0, 0.0);
   glShadeModel (GL_FLAT);
}

void display(void)
{
   int i,j;
   double a,b, x,y, a2,b2, oldx,oldy, dx;
   int slices = 32;
   int fill = GL_TRUE;

   glClear (GL_COLOR_BUFFER_BIT);

/* select red for all lines  */
   glColor3f (0.75, 0.0, 0.0);

/* in 1st row, 3 lines, each with a different stipple
   glEnable (GL_LINE_STIPPLE);
   
   glLineStipple (1, 0x0101);  //  dotted  
   drawOneLine (50.0, 125.0, 150.0, 125.0);
   glLineStipple (1, 0x00FF);  //  dashed
   drawOneLine (150.0, 125.0, 250.0, 125.0);
   glLineStipple (1, 0x1C47);  //  dash/dot/dash
   drawOneLine (250.0, 125.0, 350.0, 125.0);
*/

   a = 150.0;   a2 = a*a;
   b = 50.0;    b2 = b*b;

   dx = a/(double)slices;

   glTranslatef(a, 25.0, 0.0);
   
   if (fill) {
      glBegin(GL_TRIANGLE_FAN);
        oldx = -a;
        oldy = b;
        for (i=1; i<=2*slices; i++) {
          x = oldx + dx;
          y = b + (double)sqrt(b2 - ((x*x*b2)/a2));
          glVertex3d(a,    b,    0.0);
          glVertex3d(oldx, oldy, 0.0);
          glVertex3d(x,    y,    0.0);
          oldx = x;
          oldy = y;
        }
        for (i=1; i<=2*slices; i++) {
          x = oldx - dx;
          y = b - (double)sqrt(b2 - ((x*x*b2)/a2));
          glVertex3d(a,    b,    0.0);
          glVertex3d(oldx, oldy, 0.0);
          glVertex3d(x,    y,    0.0);
          oldx = x;
          oldy = y;
        }
      glEnd();
   } else {
      glBegin(GL_LINE_LOOP);
        oldx = -a;
        oldy = b;
        glVertex3d(oldx, oldy, 0.0);
        for (i=1; i<=2*slices; i++) {
          x = oldx + dx;
          y = b + (double)sqrt(b2 - ((x*x*b2)/a2));
          glVertex3d(x, y, 0.0);
          oldx = x;	
        }
        for (i=1; i<=2*slices; i++) {
          x = oldx - dx;
          y = b - (double)sqrt(b2 - ((x*x*b2)/a2));
          glVertex3d(x, y, 0.0);
          oldx = x;
        }
      glEnd();
   }
   glFlush ();
}

void reshape (int w, int h)
{
   glViewport (0, 0, (GLsizei) w, (GLsizei) h);
   glMatrixMode (GL_PROJECTION);
   glLoadIdentity ();
   gluOrtho2D (0.0, (GLdouble) w, 0.0, (GLdouble) h);
}

/* ARGSUSED1 */
void keyboard(unsigned char key, int x, int y)
{
   switch (key) {
      case 27:
         exit(0);
         break;
   }
}

int main(int argc, char** argv)
{
   glutInit(&argc, argv);
   glutInitDisplayMode (GLUT_SINGLE | GLUT_RGB);
   glutInitWindowSize (400, 150); 
   glutInitWindowPosition (100, 100);
   glutCreateWindow (argv[0]);
   init ();
   glutDisplayFunc(display); 
   glutReshapeFunc(reshape);
   glutKeyboardFunc(keyboard);
   glutMainLoop();
   return 0;
}
