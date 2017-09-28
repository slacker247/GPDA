#include "GR_DispObj.H"
#include <stdio.h>

GR_DispObj::GR_DispObj ()
{
   p_register_pick = 1;
   p_visible = 1;
   p_highlight = 0;
}

void 
GR_DispObj::set_xyz (float x, float y, float z)
{
   p_x = x;
   p_y = y;
   p_z = z;
}

void 
GR_DispObj::set_llah (float lat, float lon, float alt, float heading)
{
   p_lat = lat;
   p_lon = lon; 
   p_alt = alt;
   p_heading = heading;
}


void
GR_DispObj::draw ()
{
   if (!p_visible)
     return;

   p_useXForm = 1;
   GR_pushmatrix ();
   GR_multmatrix (p_modelmatrix);
   if (p_register_pick)
     GR_register_object (this);
   this->objdraw ();
   if (p_useXForm)
   {
      p_useXForm = 0;
      GR_popmatrix ();
   }
}

void
GR_DispObj::objdraw ()
{
}

void
GR_DispObj::set_highlight (long tf)
{
   if (tf)
     p_highlight++;
   else if (p_highlight > 0)
     p_highlight--;
   
   this->v_set_highlight (tf);
}

void
GR_DispObj::pickEvent (GR_MouseEvent&, GR_Window*)
{
   printf ("GR_DispObj::pickEvent (GR_MouseEvent& event, GR_Window *window\
)\n");
}

void
GR_DispObj::v_set_highlight (long)
{
}

  void
GR_DispObj::pickEvent (GR_MouseEvent&, GR_DispObj*, GR_Window*)
{
 //  printf ("GR_DispObj::pickEvent (GR_MouseEvent&, GR_DispObj*, GR_Window*)\n");
}

void
GR_DispObj::rectEvent (short&, short&, short&, short&)
{
}

void
GR_DispObj::dragEvent (GR_MouseEvent&, GR_Window*)
{
}

void
GR_DispObj::keyEvent (KeySym, GR_Window*)
{
}

GR_Point
GR_DispObj::position (GR_Point origin)
{
   return origin * p_modelmatrix;
}

GR_Point
GR_DispObj::position ()
{
   GR_Point origin (0, 0, 0);

   return origin * p_modelmatrix;
}

void
GR_DispObj::set_noXForm ()
{
   if (p_useXForm)
   {
      GR_popmatrix ();
      p_useXForm = 0;
   }
}

/*

void
GR_DispObj::process_pick (GR_MouseEvent& event, GR_Window *window)
{
  this->v_process_pick (event, window);
}

void
GR_DispObj::v_process_pick (GR_MouseEvent&, GR_Window *)
{
}

void
GR_DispObj::pick_event (GR_MouseEvent&, GR_DispObj*, GR_Window*)
{
}

void
GR_DispObj::drag_event (GR_MouseEvent&, GR_Window*)
{
}

void
GR_DispObj::key_event (KeySym, GR_Window*)
{
}
*/








