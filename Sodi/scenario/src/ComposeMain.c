#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <math.h>

#include "GR_Interface.H"

#include "forms.h"
#include "Composer.h"
#include "asset_vars.h"
#include "gl.c"
#include "GL/gltk.h"
#include "glfont.h"

#define DEGRAD  57.29577951308232
#define RADDEG  0.0174532925199432958

int             n_assets = 0;
int             asset_index;
int             airlist;

//forms used
   FD_Map *fd_Map;
   FD_select_change *fd_select_change;
   FD_location *fd_location;
   FD_weapons *fd_weapons;
   FD_defense *fd_defense;
   FD_sensors *fd_sensors;
   FD_color *fd_color;
   FD_selectsensor *fd_selectsensor;
   FD_selectdefense *fd_selectdefense;
   FD_units *fd_units;


//callback events for opengl canvas
int exposeCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud);
int buttonpressCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud);

//draw to opengl canvas
void draw();

extern int   AssetLoad(char *filename);
extern int   AssetGetLLAH(int index, float *lat, float *lon, float *alt, float *head);
extern int   AssetGetIcon(int index);
extern float AssetGetScale(int index);
extern char  *AssetGetName(int index);
extern void  load_model (char* file_name, long type, int list);


int main(int argc, char *argv[])
{
   void Initialize();

   fl_initialize(&argc, argv, 0, 0, 0);
   fd_Map = create_form_Map();
   fd_select_change = create_form_select_change();
   fd_location = create_form_location();
   fd_weapons = create_form_weapons();
   fd_sensors = create_form_sensors();
   fd_defense = create_form_defense();
   fd_color = create_form_color();
   fd_selectsensor = create_form_selectsensor();
   fd_selectdefense = create_form_selectdefense();
   fd_units = create_form_units();

   fl_add_canvas_handler(fd_Map->canvas, Expose,      exposeCB, 0);
   fl_add_canvas_handler(fd_Map->canvas, ButtonPress, buttonpressCB, 0);

   /* fill-in form initialization code */

   Initialize();

   /* show the first form */
   fl_show_form(fd_Map->Map,FL_PLACE_CENTER,FL_FULLBORDER,"Scenario Composer");
   fl_do_forms();
   return 0;
}

void draw()
{
char            *fileName = 0;
char            *DATADIR;
char            mapfile[128];
float           point[3], scale;
int             vpwindW, vpwindH;
int             viewport[4];
int             i;
TK_RGBImageRec  *vfimage;
IMAGE           *vfsized;
char            filename[80];
float           assetlat, assetlon, assetalt, assethead;
static Bool displayListInitiated = False;

   if (!displayListInitiated) {
      glXMakeCurrent(fl_display, fl_get_canvas_id(fd_Map->canvas),
                     fl_get_glcanvas_context(fd_Map->canvas)); 

      fl_get_winsize(fl_get_canvas_id(fd_Map->canvas), &vpwindW, &vpwindH);
      //printf("Viewport is %d x %d\n", vpwindW, vpwindH); 

      if ((DATADIR = getenv("DATADIR")) == NULL) {
         DATADIR = "../RSD_Data";
      }
      //sprintf (mapfile, "%s%s", DATADIR, "/jpl_earth.rgb");
      sprintf (mapfile, "%s%s", DATADIR, "/earth-hires.rgb");
      //printf("VF: Reading image file %s\n", mapfile);
      vfimage = tkRGBImageLoad(mapfile);
      vfsized = (IMAGE *)malloc(sizeof(IMAGE));
      vfsized->xsize = vpwindW;
      vfsized->ysize = vpwindH;
      vfsized->tmp   = (unsigned char *)malloc(vfsized->xsize*vfsized->ysize*4);
      gluScaleImage(GL_RGB,
          vfimage->sizeX, vfimage->sizeY, GL_UNSIGNED_BYTE, vfimage->data,
          vfsized->xsize, vfsized->ysize, GL_UNSIGNED_BYTE, vfsized->tmp);
      free(vfimage->data);
      free(vfimage);

      glClearColor(1.0, 0.0, 0.0, 0.0);
      glViewport(0, 0, vpwindW, vpwindH);
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

      glMatrixMode(GL_PROJECTION);
      glLoadIdentity();
      gluOrtho2D(0.0, vpwindW, 0.0, vpwindH);
      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity();

      point[0] = (vpwindW / 2) - (vfsized->xsize / 2);
      point[1] = (vpwindH / 2) - (vfsized->ysize / 2);
      point[2] = 0;
      glRasterPos3fv(point);

      glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
      glPixelZoom(1.0, 1.0);
      glDrawPixels(vfsized->xsize, vfsized->ysize, GL_RGB, GL_UNSIGNED_BYTE,
                vfsized->tmp);

      airlist = glGenLists(400);      // Save 400 Display Lists for the models

      glfontMake(GL_ALLFONTS);        // Build the fonts
      glfontSet(GL_STROKE);

      displayListInitiated = TRUE;    // We've been through this code once already
   }

   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   gluOrtho2D(-180.0*RADDEG, 180.0*RADDEG, -90.0*RADDEG, 90.0*RADDEG);
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();

   for (i=1; i<n_assets; i++) {
      scale = AssetGetScale(i);
      AssetGetLLAH(i, &assetlat, &assetlon, &assetalt, &assethead);
      //fprintf(stderr, "Asset %d at %f %f %s\n", AssetGetIcon(i),
      //assetlon, assetlat, AssetGetName(i));
      glPushMatrix();
        glTranslatef(assetlon*RADDEG, assetlat*RADDEG, 0.0);
        glScalef(scale, scale, scale);
        glCallList(airlist+AssetGetIcon(i));
        glRasterPos2f(assetlon*RADDEG, assetlat*RADDEG);
	glScalef(0.1, 0.1, 0.1);
        glfontPrint(AssetGetName(i));
      glPopMatrix();
   }

   glFlush();
   glXSwapBuffers(fl_display, fl_get_canvas_id(fd_Map->canvas));
}

//<---------------Initialize----------------------------->

/** Sets up choice menus on select_change form and
sets returns for input fields
**/

void Initialize()
{
   color_call = 0;
   object = fd_Map->crc;

   fl_clear_choice(fd_selectsensor->selectsensortype);
   fl_clear_choice(fd_selectdefense->selectdefensetype);

   fl_set_choice_fontsize(fd_selectsensor->selectsensortype, 8);
   fl_set_choice_fontsize(fd_selectdefense->selectdefensetype, 8);

   fl_addto_choice(fd_selectdefense->selectdefensetype, "CIRCLE");
   fl_addto_choice(fd_selectdefense->selectdefensetype, "RECTANGLE");

   fl_addto_choice(fd_selectsensor->selectsensortype, "GBR SENSOR");
   fl_addto_choice(fd_selectsensor->selectsensortype, "GBI SENSOR");
   fl_addto_choice(fd_selectsensor->selectsensortype, "AEGIS SENSOR");
   fl_addto_choice(fd_selectsensor->selectsensortype, "PATRIOT SENSOR");
   fl_addto_choice(fd_selectsensor->selectsensortype, "THAAD SENSOR");
   fl_addto_choice(fd_selectsensor->selectsensortype, "HAWK SENSOR");
   fl_addto_choice(fd_selectsensor->selectsensortype, "DSP SENSOR");
   fl_addto_choice(fd_selectsensor->selectsensortype, "SBIRS SENSOR");

   fl_set_input_return(fd_select_change->assetname, FL_RETURN_END_CHANGED);
   fl_set_input_return(fd_select_change->assetid, FL_RETURN_END_CHANGED);
   fl_set_input_return(fd_select_change->status, FL_RETURN_END_CHANGED);
   fl_set_input_return(fd_select_change->altitude, FL_RETURN_END_CHANGED);
   fl_set_input_return(fd_select_change->icon, FL_RETURN_END_CHANGED);
   fl_set_input_return(fd_select_change->scale, FL_RETURN_END_CHANGED);


}

//<-----------------Read File Values------------------------->

/***
Searches file "readfile" for default asset valuse
searches under CRC, GBR, AEGIS, PATRIOT, THAAD,
JTAGS, GBI, SBIRS, DSP, AWACS, Tactical, Cruise, and ArmyNavy
as defined in the "asset" variable defined in 
the Map form callbacks (Map_CB.c)

-make file to be read from user entered at command line

***/

void Read_File_Asset(char *name)
{
   int read_weapons = 0;
   int read_defense = 0;

   void Read_File_Sensor(char *name);
   void Read_File_Defense(char *name);

   FILE *filetoread;
   char chtemp[128];
   char file_name[50];

   char type[128] = "";

   filetoread = fopen("assetdata.dat", "r");

   fscanf(filetoread, "%s", type);
   fscanf(filetoread, "%s", chtemp);
   
   while ((strcmp(type, name) != 0) || (strcmp(chtemp, "{") != 0) || !EOF)
   { 
      strcpy(type, chtemp);
      fscanf(filetoread, "%s", chtemp);
   }

   //strcpy(Current.asset_name, type);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   Current.asset_id = atoi(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
  
   if (strcmp(chtemp, "T") == 0)
      Current.north = 1;
   else Current.north = 0;

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
    
   if (strcmp(chtemp, "T") == 0) 
      Current.east = 1;
   else Current.east = 0;

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   Current.lat_deg = atof(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   Current.lat_min = atof(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   Current.lat_sec = atof(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   Current.lon_deg = atof(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   Current.lon_min = atof(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   Current.lon_sec = atof(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   Current.altitude = atof(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   strcpy(Current.status, chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   Current.icon = atoi(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   Current.scale = atof(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   Current.r = atoi(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   Current.g = atoi(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   Current.b = atoi(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);

   if (strcmp(chtemp, "F") == 0)
      Current.sensor = 0;
   else 
   {
      Current.sensor = 0;

      fscanf(filetoread, "%s", chtemp);
      fscanf(filetoread, "%s", chtemp);
      fscanf(filetoread, "%s", chtemp);
      strcpy(Current.sensor_type, chtemp);
 
      fscanf(filetoread, "%s", chtemp);
      fscanf(filetoread, "%s", chtemp);
      fscanf(filetoread, "%s", chtemp);
      Current.elevation = atof(chtemp);

      fscanf(filetoread, "%s", chtemp);
      fscanf(filetoread, "%s", chtemp);
      fscanf(filetoread, "%s", chtemp);
      Current.azimuth = atof(chtemp);
   }

//end sensor if area

//begin coverage if area

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
 
   if (strcmp(chtemp, "T") == 0)
      Current.coverage = 1;
   else Current.coverage = 0;

//begin weapons if area
 
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);

   if (strcmp(chtemp, "F") == 0)
   {
      Current.weapons = 0;
   
      Current.gbi_id = 2;
      Current.n_gbi = 35;
      Current.n_hold = 5;
      Current.pkill = 1.0;

      strcpy(Current.gbi_type, "PATRIOT");

   }
   else if (strcmp(chtemp, "T") == 0) 
   {
      Current.weapons = 0;
      read_weapons = 1;

      fscanf(filetoread, "%s", chtemp);
      fscanf(filetoread, "%s", chtemp);
      fscanf(filetoread, "%s", chtemp);
      Current.gbi_id = atoi(chtemp);

      fscanf(filetoread, "%s", chtemp);
      fscanf(filetoread, "%s", chtemp);
      fscanf(filetoread, "%s", chtemp);
      Current.n_gbi = atoi(chtemp);
   
      fscanf(filetoread, "%s", chtemp);
      fscanf(filetoread, "%s", chtemp);
      fscanf(filetoread, "%s", chtemp);
      Current.n_hold = atoi(chtemp);

      fscanf(filetoread, "%s", chtemp);
      fscanf(filetoread, "%s", chtemp);
      fscanf(filetoread, "%s", chtemp);
      Current.pkill = atoi(chtemp);

      fscanf(filetoread, "%s", chtemp);
      fscanf(filetoread, "%s", chtemp);
      fscanf(filetoread, "%s", chtemp);
      strcpy(Current.gbi_type, chtemp);
   }

//end weapons if area

//being defense if area

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);


   if (strcmp(chtemp, "F") == 0)
      Current.area_defense = 0;
   else  if (strcmp(chtemp, "T") == 0)
   {
      Current.area_defense = 0;
      read_defense = 1;

      fscanf(filetoread, "%s", chtemp);
      fscanf(filetoread, "%s", chtemp);
      strcpy(Current.type, chtemp);

   }
//end defense

//begin unit if area

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);

   if (strcmp(chtemp, "T") == 0)
      Current.unit = 1;
   else Current.unit = 0;
   if (Current.unit == 1)
   { 
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   strcpy(Current.unit_type, chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   strcpy(Current.unit_name, chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   Current.unit_int_type = atoi(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   Current.size = atof(chtemp);
   }

   fclose(filetoread);

   Changing = Current;

   if (read_weapons == 1)
      Read_File_Sensor(Current.sensor_type);

   if (read_defense == 1)
      Read_File_Defense(Current.type);

}

//<-------------Read File Sensors--------------------------->

/***
Searches the file "readfile" for the appropriate
sensor name to load default values
***/


void Read_File_Sensor(char *name)
{

   FILE *filetoread;
   char chtemp[128];
   char file_name[50];

   char type[128];

   filetoread = fopen("assetdata.dat", "r");

   fscanf(filetoread, "%s", type);
   fscanf(filetoread, "%s", chtemp);

   while ((strcmp(type, name) != 0) || (strcmp(chtemp, "{") != 0))
   {
     strcpy(type, chtemp);
     fscanf(filetoread, "%s", chtemp);
   }

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   
   if (strcmp(chtemp, "F") == 0)
      CurrentSensors.sensor_type = 0;
  else CurrentSensors.sensor_type = 1;

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   CurrentSensors.scan_time = atof(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   CurrentSensors.rmin = atof(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   CurrentSensors.rmax = atof(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   CurrentSensors.rdotmin = atof(chtemp);


   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   CurrentSensors.signal = atof(chtemp);


   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   CurrentSensors.luminosity = atof(chtemp);


   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   CurrentSensors.rmax_low = atof(chtemp);


   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   CurrentSensors.fov_high = atof(chtemp);


   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   CurrentSensors.fov_low = atof(chtemp);


   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   
   if (strcmp(chtemp, "F") == 0)
      CurrentSensors.fixed = 0;
   else CurrentSensors.fixed = 1;

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   
   if (strcmp(chtemp, "F") == 0)
      CurrentSensors.los = 0;
   else CurrentSensors.los = 1;

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   CurrentSensors.error = atof(chtemp);


   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   CurrentSensors.icon = atoi(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   CurrentSensors.scale = atof(chtemp);


//colors removed from sensor data
//but I haven't removed them from file

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
//   CurrentSensors.r = atoi(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
//  CurrentSensors.g = atoi(chtemp);


   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
//   CurrentSensors.b = atoi(chtemp);

   fclose(filetoread);

   ChangingSensors = CurrentSensors;
}

//<---------------Read File Defense-------------------->

/*** 
Searches the file "readfile" for defense
defaults of the RECTANGLE and CIRCLE kinds
***/

void Read_File_Defense(char *name)
{
   FILE *filetoread;
   char chtemp[128];
   char file_name[50];

   char type[128];

   filetoread = fopen("assetdata.dat", "r");

   fscanf(filetoread, "%s", type);
   fscanf(filetoread, "%s", chtemp);

   while ((strcmp(type, name) != 0) || (strcmp(chtemp, "{") != 0))
   {
     strcpy(type, chtemp); 
     fscanf(filetoread, "%s", chtemp);

   }

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);

   if (strcmp(chtemp, "F") == 0)
      CurrentDefense.area_label = 0;
   else CurrentDefense.area_label = 1;

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);

   if (strcmp(chtemp, "F") == 0)
      CurrentDefense.area_north = 0;
   else CurrentDefense.area_north = 1;

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);

   if (strcmp(chtemp, "F") == 0)
      CurrentDefense.area_east = 0;
   else CurrentDefense.area_east = 1;

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   CurrentDefense.area_lat_deg = atof(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   CurrentDefense.area_lat_min = atof(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   CurrentDefense.area_lat_sec = atof(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   CurrentDefense.area_lon_deg = atof(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   CurrentDefense.area_lon_min = atof(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   CurrentDefense.area_lon_sec = atof(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   strcpy(CurrentDefense.area_type, chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   CurrentDefense.area_major = atof(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   CurrentDefense.area_minor = atof(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   CurrentDefense.area_orient = atof(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   strcpy(CurrentDefense.area_file, chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   CurrentDefense.r = atoi(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   CurrentDefense.g = atoi(chtemp);

   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   fscanf(filetoread, "%s", chtemp);
   CurrentDefense.b = atoi(chtemp);

   fclose(filetoread);
   
   ChangingDefense = CurrentDefense;
}


//<---------------------Load From File--------------------->

/***
Reads assets, sensors, and area defenses defined
by user from the assets.par file
***/

void Load_Data_From_File(char *file_name)
{
FILE            *filetoload;
FILE            *MODELfp;
char            chtemp[128];
char            filename[50];
char            *MODELDIR;
int             i = 0, listid, tempid;
float           assetlat, assetlon, assetalt, assethead, scale;

   n_assets = AssetLoad(file_name);

   MODELfp = fopen("Models.desc", "r");

   if ((MODELDIR = getenv("MODELDIR")) == NULL) {
        MODELDIR = "../Models";
   }

   for (i=0; i<n_assets; i++) {
      rewind(MODELfp);
      while (!feof(MODELfp)) {
        fgets(chtemp, 120, MODELfp);
        if (chtemp[0] != '#') {
	   sscanf(chtemp, "%d %s", &tempid, filename);
           if (tempid == AssetGetIcon(i)) {
	      listid = AssetGetIcon(i);
              sprintf(chtemp, "%s/%s", MODELDIR, filename);
              load_model(chtemp, listid, airlist+listid);
	      break;
	   }
	}
      }
   }

   fclose(MODELfp);

   draw();
}

//<----------------------Save To File--------------------->

/***
Writes assets, sensors, and area defenses defined
by user to the assets.par file
***/

void Save_Data_To_File(char *file_name)
{
   FILE *filetosave;
   char chtemp[128];
   //char file_name[50];
   int i = 0;

   filetosave = fopen(file_name, "w");

/*** Header of assets.par file ***/

   
   sprintf(chtemp, "%s","parameters { \n");
   fprintf(filetosave, chtemp);

   sprintf(chtemp, "%s","  logical on_off T\n");
   fprintf(filetosave, chtemp);

   sprintf(chtemp, "%s","}\n");
   fprintf(filetosave, chtemp);

   sprintf(chtemp, "%s","\n");
   fprintf(filetosave, chtemp);

   sprintf(chtemp, "%s","asset_names {\n");
   fprintf(filetosave, chtemp);

   sprintf(chtemp, "%s","  logical label F\n");
   fprintf(filetosave, chtemp);

   sprintf(chtemp, "%s","\n");
   fprintf(filetosave, chtemp);

   sprintf(chtemp, "%s%d%s","  int max_id ", Highest_id, "\n" );
   fprintf(filetosave, chtemp);

   sprintf(chtemp, "%s","\n");
   fprintf(filetosave, chtemp);

   while (i <= array_place)
   {
      sprintf(chtemp, "%s%s%s","  type ", Save_Data[i].asset_name, "\n");
      fprintf(filetosave, chtemp);

      i = i + 1;
   }

   sprintf(chtemp, "%s","}\n");
   fprintf(filetosave, chtemp);

   sprintf(chtemp, "%s","\n");
   fprintf(filetosave, chtemp);

/*** Individual asset data***/

/*first part of data */

   i = 0;

   while (i <= array_place)
   {
   sprintf(chtemp, "%s%s",Save_Data[i].asset_name, " { \n");
   fprintf(filetosave, chtemp);

   sprintf(chtemp, "%s%d%s", "   int asset_id ", Save_Data[i].asset_id, "\n");
   fprintf(filetosave, chtemp);

   if (Save_Data[i].north == 0)
   sprintf(chtemp, "%s", "   logical north F \n");
   else if (Save_Data[i].north == 1)
   sprintf(chtemp, "%s", "   logical north T \n");
   fprintf(filetosave, "%s", chtemp);

   if (Save_Data[i].east == 0)
   sprintf(chtemp, "%s", "   logical east F \n");
   else if (Save_Data[i].east == 1)
   sprintf(chtemp, "%s", "   logical east T \n");
   fprintf(filetosave, "%s", chtemp);

    sprintf(chtemp, "%s%.1f%s","   float lat_deg ", Save_Data[i].lat_deg, "\n");
   fprintf(filetosave, "%s", chtemp);

   sprintf(chtemp, "%s%.1f%s","   float lat_min ",  Save_Data[i].lat_min, "\n");
   fprintf(filetosave, "%s", chtemp);

   sprintf(chtemp, "%s%.1f%s","   float lat_sec ", Save_Data[i].lat_sec, "\n");
   fprintf(filetosave, "%s", chtemp);

   sprintf(chtemp, "%s%.1f%s","   float lon_deg ", Save_Data[i].lon_deg, "\n");
   fprintf(filetosave, "%s", chtemp);

   sprintf(chtemp, "%s%.1f%s","   float lon_min ", Save_Data[i].lon_min, "\n");
   fprintf(filetosave, "%s", chtemp);

   sprintf(chtemp, "%s%.1f%s","   float lon_sec ", Save_Data[i].lon_sec, "\n");
   fprintf(filetosave, "%s", chtemp);

   sprintf(chtemp, "%s%.1f%s","   float altitude ", Save_Data[i].altitude, "\n");
   fprintf(filetosave, "%s", chtemp);

   sprintf(chtemp, "%s%s%s","   string status ", Save_Data[i].status, "\n");
   fprintf(filetosave, "%s", chtemp);

   sprintf(chtemp, "%s%d%s","   int icon ", Save_Data[i].icon, "\n");
   fprintf(filetosave, "%s", chtemp);

   sprintf(chtemp, "%s%.1f%s","   float scale ", Save_Data[i].scale, "\n");
   fprintf(filetosave, "%s", chtemp);

   sprintf(chtemp, "%s%d%s","   int r ", 
              Save_Data[i].r, "\n");
   fprintf(filetosave, "%s", chtemp);

   sprintf(chtemp, "%s%d%s","   int g ", 
              Save_Data[i].g, "\n");
   fprintf(filetosave, "%s", chtemp);

   sprintf(chtemp, "%s%d%s","   int b ", 
              Save_Data[i].b, "\n");
   fprintf(filetosave, "%s", chtemp);

   sprintf(chtemp, "%s","\n");
   fprintf(filetosave, "%s", chtemp);

/* sensor data */

   if (Save_Data[i].sensor == 0)
   {
      sprintf(chtemp, "%s","   logical sensors F \n");
      fprintf(filetosave, "%s", chtemp);
   }

   else if (Save_Data[i].sensor == 1)
   {

   sprintf(chtemp, "%s","   logical sensors T \n");
   fprintf(filetosave, "%s", chtemp);

   sprintf(chtemp, "%s%s%s", "   string sensor_type ", Save_Data[i].sensor_type, "\n");
   fprintf(filetosave, "%s", chtemp);

   sprintf(chtemp, "%s%.1f%s","   float elevation ", Save_Data[i].elevation, "\n");
   fprintf(filetosave, "%s", chtemp);

   sprintf(chtemp, "%s%.1f%s","   float azimuth ", Save_Data[i].azimuth, "\n");
   fprintf(filetosave, "%s", chtemp);

   if (Save_Data[i].coverage == 0)
   sprintf(chtemp, "%s","   logical coverage F\n");
   else sprintf(chtemp, "%s","   logical coverage T\n");
   fprintf(filetosave, "%s", chtemp);

   }

   sprintf(chtemp, "%s","\n");
   fprintf(filetosave, "%s", chtemp);

/* weapons data */

   if (Save_Data[i].weapons == 0)
   {
      sprintf(chtemp, "%s","   logical weapons F \n");
      fprintf(filetosave, "%s", chtemp);
   }
   else if (Save_Data[i].weapons == 1)
   {

   sprintf(chtemp, "%s","   logical weapons T \n");
   fprintf(filetosave, "%s", chtemp);

   sprintf(chtemp, "%s%d%s","   int gbi_id ", Save_Data[i].gbi_id, "\n");
   fprintf(filetosave, "%s", chtemp);

   sprintf(chtemp, "%s%d%s","   int n_gbi ", Save_Data[i].n_gbi, "\n");
   fprintf(filetosave, "%s", chtemp);

   sprintf(chtemp, "%s%d%s","   int n_hold ", Save_Data[i].n_hold, "\n");
   fprintf(filetosave, "%s", chtemp);

   sprintf(chtemp, "%s%.f%s","   float pkill ",  Save_Data[i].pkill, "\n");
   fprintf(filetosave, "%s", chtemp);

   sprintf(chtemp, "%s%s%s","   string gbi_type ",  Save_Data[i].gbi_type, "\n");
   fprintf(filetosave, "%s", chtemp);

   }

   sprintf(chtemp, "%s","\n");
   fprintf(filetosave, "%s", chtemp);

   if (Save_Data[i].area_defense == 0)
   {
      sprintf(chtemp, "%s","   logical area_defense F \n");
      fprintf(filetosave, "%s", chtemp);
   }
   else if (Save_Data[i].area_defense == 1)
   {

   sprintf(chtemp, "%s","   logical area_defense T \n");
   fprintf(filetosave, "%s", chtemp);

   sprintf(chtemp, "%s%s%s","   type ", Save_Data[i].type, "\n");
   fprintf(filetosave, "%s", chtemp);
   
   }

   sprintf(chtemp, "%s","\n");
   fprintf(filetosave, "%s", chtemp);

   if (Save_Data[i].unit == 0)
   {
       sprintf(chtemp, "%s","   logical unit F \n");
      fprintf(filetosave, "%s", chtemp);
   }

   else if (Save_Data[i].unit == 1)
   {
     sprintf(chtemp, "%s","   logical unit T \n");
     fprintf(filetosave, "%s", chtemp);

     sprintf(chtemp,"%s%s%s","   string unit_type ",Save_Data[i].unit_type,"\n");
     fprintf(filetosave, "%s", chtemp);

     sprintf(chtemp,"%s%s%s","   string unit_name ",Save_Data[i].unit_name,"\n");
     fprintf(filetosave, "%s", chtemp);

     sprintf(chtemp, "%s%d%s","   int type ", Save_Data[i].unit_int_type,"\n");
     fprintf(filetosave, "%s", chtemp);

     sprintf(chtemp, "%s%.1f%s","   float size ",Save_Data[i].size,"\n");
     fprintf(filetosave, "%s", chtemp);

   }

   sprintf(chtemp, "%s","}\n");
   fprintf(filetosave, "%s", chtemp);

   sprintf(chtemp, "%s","\n");
   fprintf(filetosave, "%s", chtemp);

   i = i + 1;
   }  // end data while loop

/*** End of Asset data save ***/
/* Start Sensor Data Save */

   i = 0;

//uses < not <= for repeated sensor checking reasons

   while (i < sensor_array)
   {
      sprintf(chtemp, "%s%s",Save_Sensors[i].name, " {\n");
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s%d%s","   int sensor_type ", 
              Save_Sensors[i].sensor_type, "\n");
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s%.1f%s","   float scan_time ", 
              Save_Sensors[i].scan_time, "\n");
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s%.1f%s","   float rmin ", 
              Save_Sensors[i].rmin, "\n");
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s%.1f%s","   float rmax ", 
              Save_Sensors[i].rmax, "\n");
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s%.1f%s","   float rmax_low ", 
              Save_Sensors[i].rmax_low, "\n");
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s%.1f%s","   float rdotmin ", 
              Save_Sensors[i].rdotmin, "\n");
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s%.1f%s","   float signal ", 
              Save_Sensors[i].signal, "\n");
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s%.1f%s","   float luminosity ", 
              Save_Sensors[i].luminosity, "\n");
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s%.1f%s","   float fov_high ", 
              Save_Sensors[i].fov_high, "\n");
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s%.1f%s","   float fov_low ", 
              Save_Sensors[i].fov_low, "\n");
      fprintf(filetosave, "%s", chtemp);

      if(Save_Sensors[i].fixed == 0)
      sprintf(chtemp, "%s","   logical fixed  F \n");
      else sprintf(chtemp, "%s","   logical fixed  T \n");
      fprintf(filetosave, "%s", chtemp);

      if(Save_Sensors[i].los == 0)
      sprintf(chtemp, "%s","   logical los F \n");
      else sprintf(chtemp, "%s","   logical los T \n"); 
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s%.1f%s","   float error ", 
              Save_Sensors[i].error, "\n");
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s%d%s","   int icon ", 
              Save_Sensors[i].icon, "\n");
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s%.1f%s","   float scale ", 
              Save_Sensors[i].scale, "\n");
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s","}\n");
      fprintf(filetosave, "%s", chtemp);

   sprintf(chtemp, "%s","\n");
   fprintf(filetosave, "%s", chtemp);
  
   i = i + 1;
 
   }  //end for loop for sensors

/** start area defense save to file **/

   i = 0;
  
   while (i < defense_array)
   {
      sprintf(chtemp, "%s%s", Save_Defense[i].name, " {\n");
      fprintf(filetosave, "%s", chtemp);

      if(Save_Defense[i].area_label == 0)
         sprintf(chtemp, "%s", "   logical area_label F \n");
      else sprintf(chtemp, "%s", "   logical area_label T \n");
      fprintf(filetosave, "%s", chtemp);

      if(Save_Defense[i].area_north == 0)
         sprintf(chtemp, "%s", "   logical area_north F \n");
      else sprintf(chtemp, "%s", "   logical area_north T \n");
      fprintf(filetosave, "%s", chtemp);

      if(Save_Defense[i].area_east == 0)
         sprintf(chtemp, "%s", "   logical area_east F \n");
      else sprintf(chtemp, "%s", "   logical area_east T \n");
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s%.1f%s", "   float area_lat_deg ", 
              Save_Defense[i].area_lat_deg, " \n");
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s%.1f%s", "   float area_lat_min ", 
              Save_Defense[i].area_lat_min, " \n");
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s%.1f%s", "   float area_lat_sec ",
              Save_Defense[i].area_lat_sec, " \n");
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s%.1f%s", "   float area_lon_deg ",
              Save_Defense[i].area_lon_deg, " \n");
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s%.1f%s", "   float area_lon_min ",
              Save_Defense[i].area_lon_min, " \n");
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s%.1f%s", "   float area_lon_sec ",
              Save_Defense[i].area_lon_sec, " \n");
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s%s%s", "   char area_type ",
              Save_Defense[i].area_type, " \n");
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s%.1f%s", "   float area_major ",
              Save_Defense[i].area_major, " \n");
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s%.1f%s", "   float area_minor ",
              Save_Defense[i].area_minor, " \n");
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s%.1f%s", "   float area_orient ",
              Save_Defense[i].area_orient, " \n");
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s%s%s", "   char area_file ",
              Save_Defense[i].area_file, " \n");
      fprintf(filetosave, "%s", chtemp);
      
      sprintf(chtemp, "%s%d%s", "   int r ",
              Save_Defense[i].r, " \n");
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s%d%s", "   int g ",
              Save_Defense[i].g, " \n");
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s%d%s", "   int b ",
              Save_Defense[i].b, " \n");
      fprintf(filetosave, "%s", chtemp);

      sprintf(chtemp, "%s", "}\n");
      fprintf(filetosave, "%s", chtemp);

   sprintf(chtemp, "%s", "\n");
   fprintf(filetosave, "%s", chtemp);

   i = i + 1;

   }  // end for loop for defense

   fclose(filetosave);
}

/***** Includes that require above information added here *****/
#include "ComposeCB.c"
#include "CanvasCB.c"


