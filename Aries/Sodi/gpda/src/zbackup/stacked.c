#include <stdlib.h>

#include "forms.h"
#include "stacked.h"

typedef struct {
  double        value;
  int           color;
} STACKDATA;

static int handle_stacked(FL_OBJECT *obj, int event, FL_Coord mx, FL_Coord my, 
			  int key, void *ev)
{
int             i, j;
int             ix, iy, iw, ih;
double          total;
char            chtext[256];
STACKDATA       *obspec;

   obspec = (STACKDATA *)obj->spec;

   switch (event)
   {
   case FL_DRAW:
     total = 0.0;
     for (i=1; i<=(int)obspec[0].value; i++)
       total = total + obspec[i].value;
     //
     if (obj->type == FL_VERT_STACKED) {
       ix = obj->x;
       iw = obj->w;
       iy = obj->y + obj->h;
       //
       for (i=1; i<=(int)obspec[0].value; i++) {
	 ih = (int)((double)obj->h * (obspec[i].value/total));
	 iy = iy - ih;
	 fl_drw_box(obj->boxtype, ix, iy, iw, ih, obspec[i].color, obj->bw);
       }
     } else {  
       iy = obj->y;
       ih = obj->h;
       ix = obj->x;
       //
       for (i=1; i<=(int)obspec[0].value; i++) {
	 iw = (int)((double)obj->w * (obspec[i].value/total));
	 fl_drw_box(obj->boxtype, ix, iy, iw, ih, obspec[i].color, obj->bw);
	 ix = ix + iw;
       }
     }
   case FL_DRAWLABEL:
     fl_draw_object_label(obj);
     break;

   case FL_FREEMEM:
     fl_free(obj->spec);
     break;
   }
   /*
   sprintf(chtext, "%s\nBelief = %f\nIgnorance = %f\nConflict = %f",
	   "CM-01 Neutralized", 60.0, 40.0, 0.0);
   fl_set_object_helper(obj, chtext);
   */
   return 0;
}

FL_OBJECT *fl_create_stacked(int type, FL_Coord x, FL_Coord y, FL_Coord w, FL_Coord h,
			     const char *label)
{
FL_OBJECT       *ob;
STACKDATA       *obspec;

   //
   //   Create a generic object class with an appropriate ID
   //
   ob = fl_make_object(FL_STACKED, type, x, y, w, h, label, handle_stacked);
   //
   //   Initialize some members
   //
   ob->boxtype = FL_UP_BOX;
   ob->lcol    = FL_BLACK;
   ob->bw      = -1;
   ob->align   = FL_ALIGN_CENTER | FL_ALIGN_BOTTOM;
   //
   //   Create class specific structures and initialize
   //
   //   'obspec' points to the user supplied data stored in the STACKDATA structure.
   //   'obspec[0].value' holds the number of stacks in the bar graph (in double).
   //   The max number of stacks/graph is (arbitrarily) 20.
   //
   ob->spec    = fl_calloc(20, sizeof(STACKDATA));
   //
   obspec = (STACKDATA *)ob->spec;
   obspec[0].value = 0.0;
   obspec[0].color = FL_WHITE;

   return ob;
}

FL_OBJECT *fl_add_stacked(int type, FL_Coord x, FL_Coord y, FL_Coord w, FL_Coord h,
			  const char *label)
{
FL_OBJECT       *ob;

   //   Create the stacked object class of the appropriate type
   ob = fl_create_stacked(type, x, y, w, h, label);

   fl_add_object(fl_current_form, ob);

   return ob;
}

void fl_clear_stacked( FL_OBJECT *obj)
{
STACKDATA       *obspec;

   obspec = (STACKDATA *)obj->spec;
   obspec[0].value = 0.0;

   fl_redraw_object(obj);

   return;
}

void fl_add_stacked_value(FL_OBJECT *obj, double val, const char *text, int col)
{
int             index;
STACKDATA       *obspec;

   obspec = (STACKDATA *)obj->spec;
   obspec[0].value = obspec[0].value + 1.0;

   index = (int)obspec[0].value;
   obspec[index].value = val;
   obspec[index].color = col;

   fl_redraw_object(obj);

   return;
}

void fl_replace_stacked_value(FL_OBJECT *obj, int index,
			      double val, const char *text, int col)
{
STACKDATA       *obspec;

   obspec = (STACKDATA *)obj->spec;

   if ((index > 0)  && index <= ((int)obspec[0].value)) {
     obspec[index].value = val;
     obspec[index].color = col;
   }

   fl_redraw_object(obj);

   return;
}
#ifdef STACKED_DRIVER
void done(FL_OBJECT *ob, long data) { exit(0);}

int
main(int argc, char *argv[])
{
  FL_FORM *form;
  FL_OBJECT *vobj;
  FL_OBJECT *hobj;
  int i, j, depth, col;

  fl_initialize(&argc, argv, "FormDemo", 0, 0);

  form = fl_bgn_form(FL_UP_BOX,400.0,400.0);
  vobj = fl_add_stacked(FL_VERT_STACKED,320.0,20.0,40.0,200.0,"Exit");
  fl_set_object_callback(vobj, done, 0);
  hobj = fl_add_stacked(FL_HOR_STACKED, 20.0, 200.0, 200.0,10.0,"Exit");
  fl_set_object_callback(hobj, done, 0);
  fl_end_form();

  fl_add_stacked_value(vobj, 25.0, "B", FL_BLUE);
  fl_add_stacked_value(vobj, 15.0, "B", FL_YELLOW);
  fl_add_stacked_value(vobj, 45.0, "B", FL_RED);
  fl_add_stacked_value(vobj, 35.0, "B", FL_CYAN);
  fl_add_stacked_value(vobj, 20.0, "B", FL_GREEN);

  fl_add_stacked_value(hobj, 25.0, "B", FL_BLUE);
  fl_add_stacked_value(hobj, 15.0, "B", FL_YELLOW);
  fl_add_stacked_value(hobj, 45.0, "B", FL_RED);
  fl_add_stacked_value(hobj, 35.0, "B", FL_CYAN);
  fl_add_stacked_value(hobj, 20.0, "B", FL_GREEN);

  fl_show_form(form,FL_PLACE_CENTER,FL_NOBORDER,"Free Object");
  fl_do_forms();
  return 0;
}
#endif
