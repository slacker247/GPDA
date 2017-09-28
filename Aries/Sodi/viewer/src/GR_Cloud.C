
#include <stdio.h>
#include "GR_Cloud.H"
#include "texture.H"

#define MAX(a,b) ((a) > (b) ? (a) : (b))
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define expf(x) ((float)exp((x)))  

GR_Cloud::GR_Cloud (long type, char* filename)
{
int ret_code;

   if (getgdesc (GD_TEXTURE) == 0) {
      fprintf (stderr,
	       "Sorry: Texture mapping not available on this machine.\007\n");
      set_visible_flag (0);
      return;
   }
   if (getgdesc (GD_BLEND) == 0) {
      fprintf (stderr,
	       "Sorry: Blending not available on this machine.\007\n");
      set_visible_flag (0);
      return;
   }

   p_type = type;

   if (filename)
      p_filename = filename;
   else
      p_filename = strdup ("clouds.bw");

   set_register_flag (0);
   ret_code = texture_init(filename);

   p_gr_objid = GR_genobj ();
   GR_makeobj (p_gr_objid);

   GR_blendfunction (BF_SA, BF_MSA);
   //GR_cpack (0x80000000);
   //texbind (TX_TEXTURE_0, 202);
   glBindTextureEXT(GL_TEXTURE_2D, 202);
   draw_cloud (-105.0, 15.0, -100.0, 10.0);
   GR_blendfunction (BF_ONE, BF_ZERO);
  
   GR_closeobj();
}

void
GR_Cloud::objdraw ()
{
   GR_pushattributes();
   GR_pushmatrix();

   glEnable(GL_BLEND);
   glEnable(GL_TEXTURE_2D);

   //glColor3f(0.19, 0.25, 0.70);
   //glMatrixMode(GL_TEXTURE);
   //glPushMatrix();
   //tevbind (TV_ENV0, 202);

   GR_backface (TRUE);
   glEnable(GL_POLYGON_OFFSET_FILL);
   glPolygonOffset(1.0, 1.0);

   GR_callobj (p_gr_objid);

   //GR_backface (FALSE);
   //tevbind (TV_ENV0, 0); // turn off texture;

   //glPopMatrix();
   //glMatrixMode(GL_MODELVIEW);

   GR_backface (FALSE);
   glDisable(GL_TEXTURE_2D);
   glDisable(GL_BLEND);

   GR_popmatrix();
   GR_popattributes();
}

int
GR_Cloud::texture_init(char *filename)
{
GLfloat   cloud_color[4] = { 1.0, 1.0, 1.0, 0.0 };
GLfloat   fog_color[4], fog_density = 0.05, density, far_cull;
unsigned  *image;
int       width, height, components;
char      fname[20];

   fprintf (stderr, "Doing image file %s\n", filename);

   glShadeModel(GL_FLAT);

   if (filename) {
        image = read_texture(filename, &width, &height, &components);
        if (image == NULL) {
            fprintf(stderr, "Error: Can't load image file \"%s\".\n", filename);
            return(0);
        } else {
            printf("%d x %d image loaded\n", width, height);
        }
        if (components != 1) {
            printf("must be a bw image\n");
            return(0);
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

   glBindTexture(GL_TEXTURE_2D, 202);
   glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
   glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_BLEND);   // was GL_BLEND
   //glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, cloud_color);
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);

   glTexImage2D(GL_TEXTURE_2D, 0, components, width,
                 height, 0, GL_RGBA, GL_UNSIGNED_BYTE, image);
   /*
      gluBuild2DMipmaps(GL_TEXTURE_2D, components, image->sizeX, image->sizeY,
                        GL_RGBA, GL_UNSIGNED_BYTE, image->data);
   */

   return(1);
}

void
GR_Cloud::draw_cloud (float lonNW, float latNW, float lonSE, float latSE)
{
int             j, k;
float           theta, phi;
float           theta0, phi0, theta1, phi1; // initial lon and lat in radians;
float           vert[3], vertSW[3], vertNW[3], vertNE[3], vertSE[3];
float           factor = 1.05;

   theta0 = lonNW * M_PI / 180.0;
   phi0   = latNW * M_PI / 180.0;
   theta1 = lonSE * M_PI / 180.0;
   phi1   = latSE * M_PI / 180.0;

   for (k=1, theta=theta0; k<=6; k++, theta+= M_PI/36)
   {
      GR_bgnqstrip ();
      for (j=1, phi=phi0; j<=4; j++, phi+= M_PI/36)
      {
	 vert[0] = (k - 1) / 6.0;
	 vert[1] = (j - 1) / 3.0;
	 // printf ("t=%f, s=%f\n", vert[0], vert[1]);
	 t2f (vert);
	 vert[0] = cos(phi) * sin (theta) * factor;
	 vert[1] = sin(phi) * factor;
	 vert[2] = cos(phi) * cos (theta) * factor;
	 GR_n3f (vert);
	 GR_v3f (vert);
	 
	 vert[0] = k / 6.0;
	 vert[1] = (j - 1) / 3.0;
	 // printf ("t=%f, s=%f\n", vert[0], vert[1]);
	 t2f (vert);
	 vert[0] = cos(phi) * sin (theta+M_PI/36) * factor;
	 vert[1] = sin(phi) * factor;
	 vert[2] = cos(phi) * cos (theta+M_PI/36) * factor;
	 GR_n3f (vert);
	 GR_v3f (vert);
      }
      GR_endqstrip ();
   }
}
