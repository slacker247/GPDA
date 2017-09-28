
#include	<math.h>
#include	<string.h>
#include        <sys/types.h>
#include        <malloc.h>
#include        <stdlib.h>
#include        <stdio.h>
#include        "GL/gl.h"

#define         FALSE      0
#define         TRUE       1

#define         POLAR
#define         DO_POLAR
#define         CARDINAL   2

#define		FLAT	   GL_FLAT
#define		GOURAUD    GL_SMOOTH

#define         MAXPATS    120

static unsigned long    patterns[MAXPATS][32];  /* Polygon fill patterns */
static unsigned long    currpatt[32];           /* Current fill pattern */
static int              currentpattern = 0;     /* Current line pattern */

void def_pats ();

/* -------------------------------------------------------------- */
void
GR_defpattern (short n, short size, unsigned long pattern[])
{
int     i;

        if (n >= MAXPATS) {
            fprintf(stderr, "defpattern: Pattern index too big. %d\n", n);
            return;
          }
        if (size != 32) {
            fprintf(stderr, "defpattern: Only size 32 patterns supported\n");
            return;
          }
        for (i=0; i<size; i++)
            patterns[n][i] = pattern[i];
}

void
GR_setpattern (short n)
{
int index, i;

        index = (int)n;
        currentpattern = index;
        for (i=0; i<32; i++)
            currpatt[i] = patterns[index][i];
        glPolygonStipple((GLubyte *)currpatt);
        glEnable(GL_POLYGON_STIPPLE);
}

long
GR_getpattern ()
{
        return (long)currentpattern;
}

void
def_pats ()
{
   int i;
   unsigned long patmask[32];

   for (i=0; i<32; i++)
   {
      patmask[i]   = 0xffffffff;
   }
   GR_defpattern (0, 32, patmask);

   for (i=0; i<32; i+=2)
   {
      patmask[i]   = 0xaaaaaaaa;
      patmask[i+1] = 0x55555555;
   }
   GR_defpattern (1, 32, patmask);

   for (i=0; i<32; i+=2)
   {
      patmask[i]   = 0x88888888;
      patmask[i+1] = 0x22222222;
   }
   GR_defpattern (2, 32, patmask);

   for (i=0; i<32; i+=2)
   {
      patmask[i]   = 0x03030303;
      patmask[i+1] = 0x30303030;
   }
   GR_defpattern (3, 32, patmask);

   for (i=0; i<32; i+=4)
   {
      patmask[i]   = 0x11111111;
      patmask[i+1] = 0x11111111;
      patmask[i+2] = 0xffffffff;
      patmask[i+3] = 0x11111111;
   }
   GR_defpattern (12, 32, patmask);
}

/* ----------------------------------------------------------- */

void 
load_model (char* file_name, long type, int list) 
{
long      angle, num_sides;
int       r, g, b;
float     x, y, z;
char      geom_file[64];
FILE      *fp;
char	  line [81];
char	  cmnd [81];
float     varray[3];
short     carray[3];
int       i, j;
int       num_areas, num_verts;
float     vert[3], vertlast[3];
char      *MODELDIR;
char      model_path[80], model_file [80];

/*
   if ((MODELDIR = getenv("MODELDIR")) == NULL)
        MODELDIR = "/home/dre/speedes0.45/demos/prox/Models";
   sprintf (model_path, "%s/%s", MODELDIR, file_name);
*/
   fp = fopen(file_name,"r");
   if ( fp == NULL )
   {
      printf("Model: Error on file open for file %s\007\n",file_name);
      return;
   }

   def_pats();
   //printf("Object id is %d from file %s\n", type, file_name);
   glNewList((GLuint)list, GL_COMPILE);

   glPushMatrix();
   glPushAttrib(GL_ALL_ATTRIB_BITS);

   while ( fgets(line,80,fp) != NULL )
   {
      memset( (void *)cmnd, '\0', (size_t)(sizeof(cmnd)) );
      sscanf(line,"%s",cmnd);
      /*printf("Model: processing command %s\n", cmnd);*/
 
      if ( strcmp(cmnd,"scale") == 0 )
      {
         if (sscanf(line,"%*s%f%f%f",&x,&y,&z) == 3)
         {
            glScalef(x,y,z);
         }
      }
   
      if ( strcmp(cmnd,"bgnpoint") == 0 )
      {
	 glBegin(GL_POINTS);
      }

      if ( strcmp(cmnd,"endpoint") == 0 )
      {
	 glEnd();
      }

      if ( strcmp(cmnd,"line") == 0 )
      {
	 fgets(line,80,fp);
	 sscanf(line,"%*s%f%f%f",&x,&y,&z);
	 glBegin(GL_LINES);
         glVertex3f(x, y, z);
	 fgets(line,80,fp);
	 sscanf(line,"%*s%f%f%f",&x,&y,&z);
	 glVertex3f(x, y, z);
         glEnd();
      }

      if ( strcmp (cmnd, "color") == 0 )
      {
	 sscanf (line, "%*s%d%d%d", &r, &g, &b);
      }

      if ( strcmp (cmnd, "pattern") == 0 )
      {
         sscanf(line, "%*s%d", &i);
	 GR_setpattern ((short) i);
      }

      if ( strcmp(cmnd,"crv") == 0 )
      {
	 fprintf(stderr, "Command crv not implemented\n");
	 /*
	 float p[4][3];
	 short c[3];
	 int num_pts = 0;
	 GR_matrix basmat; 
	    
	 basmat[0][0] = -.5 ;
	 basmat[0][1] = 1.5 ;
	 basmat[0][2] = -1.5 ; 
	 basmat[0][3] = .5 ; 
	 basmat[1][0] = 1 ;
	 basmat[1][1] = -2.5 ;
	 basmat[1][2] = 2.0 ; 
	 basmat[1][3] = -.5 ; 
	 basmat[2][0] = -.5 ;
	 basmat[2][1] = 0 ;
	 basmat[2][2] = .5 ; 
	 basmat[2][3] = .0 ; 
	 basmat[3][0] = 0 ;
	 basmat[3][1] = 1 ;
	 basmat[3][2] = 0 ; 
	 basmat[3][3] = 0 ; 
	    	    
	 GR_defbasis(CARDINAL,basmat);
	 GR_curvebasis(CARDINAL);
	 GR_curveprecision(20);
	 for ( i = 0; i < 4 ; i++ )
	 {
	    fgets (line, 80, fp); // get next line for vtx's
	    sscanf (line, "%*s%f%f%f", &x, &y, &z);
	    p[i][0] = y;
	    p[i][1] = z;
	    p[i][2] = x;
	    num_pts++;
	 }
	 c[0] = (short) r;
	 c[1] = (short) g;
	 c[2] = (short) b;
	 GR_RGBcolor((short)r, (short)g, (short)b);
	 GR_crv(p);
	 */
      }

      if ( strcmp(cmnd,"pnt") == 0 )
      {
         glBegin(GL_POINTS);
         glVertex3f(x, y, z);
         glEnd();
      }

      if ( strcmp(cmnd,"poly") == 0 )
      {
	 sscanf (line, "%*s%d", &num_sides);
	 glColor3f((GLfloat)r, (GLfloat)g, (GLfloat)b);  

	 if (type!=33 && num_sides==4)
	 {
	    glBegin(GL_QUADS);
            for (i=0; i<2; i++)
            {
               fgets (line, 80, fp);
               sscanf (line, "%*s%f%f%f", &x, &y, &z);
               glVertex3f(x, y, z);
	    }
	    fgets (line, 80, fp);
	    sscanf (line, "%*s%f%f%f", &x, &y, &z);
	    vertlast[0] = y;
	    vertlast[1] = z;
	    vertlast[2] = x;	       
	    fgets (line, 80, fp);
	    sscanf (line, "%*s%f%f%f", &x, &y, &z);
	    glVertex3f(x, y, z);
            glVertex3f(vertlast[0], vertlast[1], vertlast[2]);
	    glEnd();
	 } 
         else if (type!=33 && num_sides==3)
	 {
	    glBegin(GL_TRIANGLES);
	    for (i=0; i<3; i++)
	    {
	       fgets (line, 80, fp);
	       sscanf (line, "%*s%f%f%f", &x, &y, &z);
	       glVertex3f(x, y, z);
	    }
	    glEnd();
	 }
	 else
	 {
	    fgets (line, 80, fp);
	    sscanf (line, "%*s%f%f%f", &x, &y, &z);
	    glBegin(GL_POLYGON);
            glVertex3f(x, y, z);	
	    for (i = 1; i < num_sides; i++ )
	    { 
	       fgets(line,80,fp);
	       sscanf(line,"%*s%f%f%f",&x,&y,&z);
	       glVertex3f(x, y, z);	
	    }
	    glEnd();
	 }
      }
         
      if ( strcmp(cmnd,"tmesh") == 0 )
      {
         sscanf (line, "%*s%d", &num_areas);
         glColor3i(r, g, b);
         for (j = 0; j<num_areas; j++)
         {
            fgets(line,80,fp); /* read off endtmesh line */
            glBegin(GL_TRIANGLE_STRIP);
            for (i = 0; i<3; i++)
            {
               fgets(line,80,fp);
               sscanf(line,"%*s%f%f%f",&x,&y,&z);
               glVertex3f(x, y, z);
            }
            fgets(line,80,fp); /* read off endtmesh line */
            glEnd();
         }
      }
         
      if (strcmp(cmnd,"qstrip")==0)
      {
	 sscanf (line, "%*s%d%d", &num_areas, &num_verts);
         glColor3i(r, g, b);
         for (j = 0; j<num_areas; j++)
	 {
	    fgets(line,80,fp); /* read off endtmesh line */
	    glBegin(GL_QUAD_STRIP);
	    for (i = 0; i<num_verts; i++)
	    {
	       fgets(line,80,fp);
	       sscanf(line,"%*s%f%f%f",&x,&y,&z);
               glVertex3f(x, y, z);
	    }
	    fgets(line,80,fp); /* read off endtmesh line */
	    glEnd();
	 }
      }

      if ( strcmp(cmnd,"rotatex") == 0 )
      {
	 sscanf(line,"%*s%ld",&angle);
	 glRotatef(0.1*(GLfloat)angle, 1.0, 0.0, 0.0);
      }

      if ( strcmp(cmnd,"rotatey") == 0 )
      {
	 sscanf(line,"%*s%ld",&angle);
	 glRotatef(0.1*(GLfloat)angle, 0.0, 1.0, 0.0);
      }

      if ( strcmp(cmnd,"rotatez") == 0 )
      {
	 sscanf(line,"%*s%ld",&angle);
	 glRotatef(0.1*(GLfloat)angle, 0.0, 0.0, 1.0);
      }

      if ( strcmp(cmnd,"translate") == 0 )
      {
	 sscanf(line,"%*s%f%f%f",&x,&y,&z);
	 glTranslatef(x,y,z);
      }

      if ( strcmp(cmnd,"move") == 0 )
      {
	 sscanf(line,"%*s%f%f%f",&x,&y,&z);
	 glBegin(GL_LINES);
         glVertex3f(x, y, z);
      }

      if ( strcmp(cmnd,"pushmatrix") == 0 )
      {
	 glPushMatrix();
      }

      if ( strcmp(cmnd,"popmatrix") == 0 )
      {
	 glPopMatrix();
      }

   }  /* end of while */

   glPopAttrib(); 
   glPopMatrix();
   glEndList();

   fclose(fp);
}

