#include <stdio.h>
#include "GR_DispObj.H"
#include "GISP_Pers_Obj.H"
#include "GR_Idlist.H"

extern IDlist* GISP_pick_idlist;


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
   GR_matrix gmat;
   int i,j;

   if (!p_visible)
     return;

   p_useXForm = 1;

   GR_pushmatrix ();
   GR_multmatrix (p_modelmatrix);
/*
   glGetFloatv(GL_MODELVIEW_MATRIX, (GLfloat *)gmat);
   printf("After Matrix for object type %d\n", this->get_type());
   for (i=0; i<16; i++)
          printf("  %f", gmat[i]);
   printf("\n");
*/
   if (p_register_pick)
     GR_register_object (this);
   this->objdraw ();
   if (p_useXForm)
   {
      p_useXForm = 0;
   }
   GR_popmatrix ();
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
GR_DispObj::pickEvent (GR_MouseEvent& event, GR_Window* window)
{
long id, type;

   id = get_id ();
   type = get_type ();
   printf ("GR_DispObj::pickEvent (GR_MouseEvent& event, GR_Window *window)\n");
   printf ("  Object type is %d\n", this->get_type() );
   switch (event.button)
   {
    case GR_LEFTMOUSE:
      if (event.down)
      {
         printf ("..by GR_LEFTMOUSE..");
	 if (!GISP_pick_idlist) {
           printf (" Creating a new GISP_pick_idlist\n");
	   GISP_pick_idlist = new IDlist (100);
	 }
	 if (GISP_pick_idlist->in_list (id))
	   printf (" --> already picked...\007\n");
	 else {
           if (type != 0) {
	      GISP_Pers_Obj* gispobjptr = new GISP_Pers_Obj (id, type);
              gispobjptr->set_object((GISP_Obj *)this);
	      GISP_pick_idlist->put_list (id);
	   }
	 }
         GISP_pick_idlist->print_list ();   // for debugging only....
       }
      break;
    case GR_MIDDLEMOUSE:
      if (!event.down)
      {
	  printf (".. by GR_MIDDLEMOUSE..");
      }
      break;
    case GR_RIGHTMOUSE:
      if (event.down)
      {
         printf (".. by GR_RIGHTMOUSE..");
      }
      break;
   }
   printf("\n");
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

GR_point
GR_DispObj::position (GR_point origin)
{
	GR_point  result;

printf("Do a position calculation\n");
        result.x =
                  origin.x * p_modelmatrix[0]
                + origin.y * p_modelmatrix[1]
                + origin.z * p_modelmatrix[2]
                + p_modelmatrix[3];

        result.y =
                  origin.x * p_modelmatrix[4]
                + origin.y * p_modelmatrix[5]
                + origin.z * p_modelmatrix[6]
                + p_modelmatrix[7];

        result.z =
                  origin.x * p_modelmatrix[8]
                + origin.y * p_modelmatrix[9]
                + origin.z * p_modelmatrix[10]
                + p_modelmatrix[11];

        return result;

//   return origin * p_modelmatrix;
}

GR_point
GR_DispObj::position ()
{
	GR_point  result;

        result.x = p_modelmatrix[12];
        result.y = p_modelmatrix[13];
        result.z = p_modelmatrix[14];

        return result;
//   return origin * p_modelmatrix;
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

