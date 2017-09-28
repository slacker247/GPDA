/* Form definition file generated with fdesign. */

#include "forms.h"
#include <stdlib.h>
#include "XPSforms.h"

FD_xps *create_form_xps(void)
{
  FL_OBJECT *obj;
  FD_xps *fdui = (FD_xps *) fl_calloc(1, sizeof(*fdui));

  fdui->xps = fl_bgn_form(FL_NO_BOX, 851, 461);
  obj = fl_add_box(FL_FRAME_BOX,0,0,851,461,"");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
  fdui->xps_browser = obj = fl_add_browser(FL_HOLD_BROWSER,20,80,810,360,"");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_callback(obj,xps_browser_cb,0);
  fdui->xps_timer = obj = fl_add_timer(FL_HIDDEN_TIMER,640,20,70,20,"Timer");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_resize(obj, FL_RESIZE_NONE);
    fl_set_object_callback(obj,xps_timer_cb,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,750,10,90,30,"Exit");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,XPSexitCB,0);
  fl_end_form();

  fdui->xps->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

