/* Form definition file generated with fdesign. */

#include "forms.h"
#include <stdlib.h>
#include "xps_fdui.h"

FD_xps *create_form_xps(void)
{
  FL_OBJECT *obj;
  FD_xps *fdui = (FD_xps *) fl_calloc(1, sizeof(*fdui));

  fdui->xps = fl_bgn_form(FL_NO_BOX, 790, 360);
  obj = fl_add_box(FL_FRAME_BOX,0,0,790,360,"");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
  fdui->xps_browser = obj = fl_add_browser(FL_HOLD_BROWSER,20,55,750,275,"");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_callback(obj,xps_browser_cb,0);
  fdui->xps_timer = obj = fl_add_timer(FL_HIDDEN_TIMER,640,20,70,20,"Timer");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_resize(obj, FL_RESIZE_NONE);
    fl_set_object_callback(obj,xps_timer_cb,0);
  fl_end_form();

  fdui->xps->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

