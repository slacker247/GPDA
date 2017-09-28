/* Form definition file generated with fdesign. */

#include "forms.h"
#include <stdlib.h>
#include "H3Dforms.h"

FD_h3dgraph *create_form_h3dgraph(void)
{
  FL_OBJECT *obj;
  FD_h3dgraph *fdui = (FD_h3dgraph *) fl_calloc(1, sizeof(*fdui));

  fdui->h3dgraph = fl_bgn_form(FL_NO_BOX, 850, 460);
  obj = fl_add_box(FL_UP_BOX,0,0,850,460,"");
    fl_set_object_lalign(obj,FL_ALIGN_BOTTOM);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_button(FL_NORMAL_BUTTON,760,10,80,30,"Dismiss");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,H3DexitCB,0);
  fdui->canvas = obj = fl_add_glcanvas(FL_NORMAL_CANVAS,35,60,785,370,"");
    fl_set_object_color(obj,FL_WHITE,FL_BLACK);
    fl_set_object_callback(obj,H3DnoneCB,0);
  fdui->viewpos = obj = fl_add_slider(FL_HOR_NICE_SLIDER,35,435,780,20,"");
    fl_set_object_color(obj,FL_COL1,FL_RED);
    fl_set_object_callback(obj,H3DsliderCB,0);
    fl_set_slider_bounds(obj, 0, 200);
    fl_set_slider_value(obj, 30);
    fl_set_slider_size(obj, 0.15);
    fl_set_slider_step(obj, 1);
    fl_set_slider_increment(obj, 10, 1);
     fl_set_slider_return(obj, FL_RETURN_END_CHANGED);
  fdui->zoomer = obj = fl_add_slider(FL_VERT_NICE_SLIDER,820,60,20,375,"Zoom");
    fl_set_object_color(obj,FL_COL1,FL_CYAN);
    fl_set_object_lalign(obj,FL_ALIGN_TOP);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,H3DzoomCB,0);
    fl_set_slider_bounds(obj, -90, 90);
    fl_set_slider_value(obj, 80);
    fl_set_slider_size(obj, 0.15);
    fl_set_slider_step(obj, 1);
    fl_set_slider_increment(obj, 5, 1);
     fl_set_slider_return(obj, FL_RETURN_END_CHANGED);
  fdui->elevation = obj = fl_add_slider(FL_VERT_NICE_SLIDER,10,60,20,375,"Tilt");
    fl_set_object_color(obj,FL_COL1,FL_YELLOW);
    fl_set_object_lalign(obj,FL_ALIGN_TOP);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,H3DelevCB,0);
    fl_set_slider_bounds(obj, 0, 200);
    fl_set_slider_value(obj, 50);
    fl_set_slider_size(obj, 0.15);
    fl_set_slider_step(obj, 1);
    fl_set_slider_increment(obj, 5, 1);
     fl_set_slider_return(obj, FL_RETURN_END_CHANGED);
  fdui->h3d_scale = obj = fl_add_counter(FL_NORMAL_COUNTER,85,20,95,20,"V. Scale");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,H3DrefreshCB,0);
    fl_set_counter_precision(obj, 0);
    fl_set_counter_bounds(obj, 1, 100);
    fl_set_counter_value(obj, 10);
    fl_set_counter_step(obj, 1, 2);
  obj = fl_add_button(FL_NORMAL_BUTTON,410,25,85,25,"Refresh");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,H3DrefreshCB,0);
  fl_end_form();

  fdui->h3dgraph->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

