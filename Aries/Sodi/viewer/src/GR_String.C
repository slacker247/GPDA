
#include <stdio.h>
#include <math.h>
#include <malloc.h>

#include "def.H"
#include "glfont.H"
#include "GR_String.H"

GR_String::GR_String (float x, float y, float z, char* s)
{
long i,j;

   p_x = x;
   p_y = y;
   p_z = z;

   p_r = 1.0;
   p_g = 1.0;
   p_b = 1.0;

   strncpy(p_text, s, 23);
}

GR_String::GR_String (float latitude, float longitude, char* s)
{
long           i,j;
float          vert[3];
float          lon, lat;
float          factor = 1.00;

   p_lat    = latitude;
   p_lon    = longitude;

   lat = latitude*M_PI/180.0;
   lon = longitude*M_PI/180.0;
   p_x = cos(lat) * sin(lon) * factor;
   p_y = sin(lat) * factor;
   p_z = cos(lat) * cos(lon) * factor;

   p_r = 1.0;
   p_g = 1.0;
   p_b = 1.0;

   strncpy(p_text, s, 23);
}

void
GR_String::set_color(int r, int g, int b)
{
   p_r = r/255.0;
   p_g = g/255.0;
   p_b = b/255.0;
}

void
GR_String::set_text(char *s)
{
   strncpy(p_text, s, 23);
}

void
GR_String::objdraw ()
{
GLdouble    winX, winY, winZ;

   glColor3f(p_r, p_g, p_b);

   winX = p_x; winY = p_y; winZ = p_z;
   /*
   glPushMatrix();

     glRotatef(p_lon, 0.0, 1.0, 0.0);           // Rotate to correct longitude from Greenwich
     glRotatef(-p_lat, 1.0, 0.0, 0.0);          // Rotate to correct latitude from Equator
     //glRotatef(180.0-p_orient, 0.0, 0.0, 1.0);  // Rotate to correct orientation from North
     glTranslatef(0.0, 0.0, 1.0);               // Move to correct location on surface

     //glTranslatef(p_x, p_y, p_z);
     glScalef(0.1, 0.1, 0.1);
     glfontPrint(p_text);
   glPopMatrix();
   */
   glRasterPos3f(winX, winY, winZ); 
   printString(p_text);
}
