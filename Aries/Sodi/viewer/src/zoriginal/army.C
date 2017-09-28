/************************************************************

  To place the army icons to the Middle Est area.
  3/4/93, Tung

 ************************************************************/
#include <math.h>
#include "GR_Army.H"
#include "GR_Model.H"
#define D2R 0.0174532


GR_DispList *army_displist;
extern GR_Window *gwindow;

void armyCB ();
void army_init ();
void add_army_icon (long, float, float);
void scud_init ();
void add_scud (float, float);
extern float get_scale (long type);


void
armyCB ()
{
   static Boolean first_army = TRUE;
   static Boolean on_flag = FALSE;

   if (first_army)
   {
      printf ("armyCB is called...\n");
      army_displist = new GR_DispList;
      gwindow->addDispList (army_displist, "army_displist");
      army_init ();
      scud_init ();
      first_army = FALSE;
   }
   else
   {
      if (!on_flag)
      {
	 //army_displist->delete_objects ();
	 gwindow->remDispList (army_displist, "army_displist");
      }
      else
      {
	 gwindow->addDispList (army_displist, "army_displist");
	 //army_init ();
	 //scud_init ();
      }
      on_flag = !on_flag;
   }
   gwindow->draw();
}

void
army_init ()
{
   add_army_icon (1, 28.2, 48.0);
   add_army_icon (1, 28.4, 47.6);
   add_army_icon (1, 28.5, 47.4);
   add_army_icon (1, 28.7, 46.6);
   add_army_icon (2, 28.3, 47.8);
   add_army_icon (3, 28.5, 47.5);
   add_army_icon (3, 28.8, 47.4);
   add_army_icon (4, 28.4, 46.8);
   add_army_icon (4, 28.2, 46.3);
   add_army_icon (4, 28.5, 46.1);
   add_army_icon (4, 28.4, 45.5);
   add_army_icon (4, 28.7, 45.3);
   add_army_icon (4, 29.2, 45.1);
   add_army_icon (4, 29.3, 44.8);
   add_army_icon (4, 30.4, 43.8);
   add_army_icon (5, 28.8, 46.0);
   add_army_icon (5, 28.9, 44.7);
   add_army_icon (5, 29.8, 44.2);
   add_army_icon (6, 29.0, 45.2);
   add_army_icon (7, 29.7, 44.7);
   add_army_icon (8, 28.6, 44.9);
   add_army_icon (8, 29.3, 43.8);
   add_army_icon (8, 29.6, 43.5);
   add_army_icon (9, 30.0, 43.8);
   add_army_icon (10, 29.8, 43.6);
   add_army_icon (11, 30.3, 44.5);
}


void
add_army_icon (long armytype, float lat, float lon)
{
   float x,y,z, alt;
   GR_Army *army;

   alt = 0.0001;
   x = cos(lat*D2R)*sin(lon*D2R)*(1+alt);
   y = sin(lat*D2R)*(1+alt);
   z = cos(lat*D2R)*cos(lon*D2R)*(1+alt);
   
   army = new GR_Army (armytype);
   army->rotate_x(-lat);
   army->rotate_y(lon);
   army->translate(x,y,z);
   army_displist->add_object(army);
}


void
scud_init ()
{
   add_scud (30.1, 47.6);
   add_scud (30.3, 46.5);
   add_scud (30.5, 47.25);
}

void
add_scud (float lat, float lon)
{
   GR_Model *scud;
   float x,y,z, alt;
   float scale_factor = get_scale (49);

   alt = 0.0001;
   x = cos(lat*D2R)*sin(lon*D2R)*(1+alt);
   y = sin(lat*D2R)*(1+alt);
   z = cos(lat*D2R)*cos(lon*D2R)*(1+alt);
   
   scud = new GR_Model (2001, 49);
   scale_factor *= 0.5;
   
   scud->scale (scale_factor, scale_factor, scale_factor);
   scud->rotate_y (90);
      scud->rotate_x(90);
   
   scud->rotate_x(-lat);
   scud->rotate_y(lon);
   scud->translate(x,y,z);
   army_displist->add_object(scud);
   
}
