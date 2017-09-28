/* Form definition file generated with fdesign. */

#include "forms.h"
#include <stdlib.h>
#include "EXPLforms.h"

FD_explain *create_form_explain(void)
{
  FL_OBJECT *obj;
  FD_explain *fdui = (FD_explain *) fl_calloc(1, sizeof(*fdui));

  fdui->explain = fl_bgn_form(FL_NO_BOX, 850, 460);
  obj = fl_add_box(FL_UP_BOX,0,0,850,460,"");
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,620,300,220,110,"Frame Info");
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,EXPLnoneCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,770,10,70,30,"Dismiss");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,EXPLexitCB,3);
  fdui->ds_explain = obj = fl_add_browser(FL_NORMAL_BROWSER,620,60,220,230,"Text Explaination");
    fl_set_object_color(obj,FL_LEFT_BCOL,FL_YELLOW);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,EXPLnoneCB,0);
  fdui->dsex_stop = obj = fl_add_button(FL_NORMAL_BUTTON,715,420,30,30,"@square");
    fl_set_object_callback(obj,dscontrolCB,3);
  fdui->dsex_step = obj = fl_add_button(FL_NORMAL_BUTTON,745,420,30,30,"@>");
    fl_set_object_callback(obj,dscontrolCB,4);
  fdui->dsex_play = obj = fl_add_button(FL_NORMAL_BUTTON,775,420,30,30,"@>>");
    fl_set_object_callback(obj,dscontrolCB,5);
  fdui->dsex_back = obj = fl_add_button(FL_NORMAL_BUTTON,685,420,30,30,"@4>");
    fl_set_object_callback(obj,dscontrolCB,2);
  fdui->dsex_rev = obj = fl_add_button(FL_NORMAL_BUTTON,655,420,30,30,"@4>>");
    fl_set_object_callback(obj,dscontrolCB,1);
  fdui->dsex_rew = obj = fl_add_button(FL_NORMAL_BUTTON,625,420,30,30,"@4>|");
    fl_set_object_callback(obj,dscontrolCB,0);
  fdui->dsex_end = obj = fl_add_button(FL_NORMAL_BUTTON,805,420,30,30,"@>|");
    fl_set_object_callback(obj,dscontrolCB,6);
  fdui->ds_frameno[2] = obj = fl_add_pixmap(FL_NORMAL_PIXMAP,680,345,20,20,"");
    fl_set_object_callback(obj,EXPLnoneCB,0);
  fdui->ds_frameno[1] = obj = fl_add_pixmap(FL_NORMAL_PIXMAP,700,345,20,20,"");
    fl_set_object_callback(obj,EXPLnoneCB,0);
  fdui->ds_frametime[0] = obj = fl_add_pixmap(FL_NORMAL_PIXMAP,680,380,20,20,"");
    fl_set_object_callback(obj,EXPLnoneCB,0);
  fdui->ds_frametime[1] = obj = fl_add_pixmap(FL_NORMAL_PIXMAP,700,380,20,20,"");
    fl_set_object_callback(obj,EXPLnoneCB,0);
  fdui->ds_frametime[2] = obj = fl_add_pixmap(FL_NORMAL_PIXMAP,730,380,20,20,"");
    fl_set_object_callback(obj,EXPLnoneCB,0);
  fdui->ds_frametime[3] = obj = fl_add_pixmap(FL_NORMAL_PIXMAP,750,380,20,20,"");
    fl_set_object_callback(obj,EXPLnoneCB,0);
  fdui->ds_frametime[4] = obj = fl_add_pixmap(FL_NORMAL_PIXMAP,780,380,20,20,"");
    fl_set_object_callback(obj,EXPLnoneCB,0);
  fdui->ds_frametime[5] = obj = fl_add_pixmap(FL_NORMAL_PIXMAP,800,380,20,20,"");
    fl_set_object_callback(obj,EXPLnoneCB,0);
  fdui->ds_frameno[0] = obj = fl_add_pixmap(FL_NORMAL_PIXMAP,720,345,20,20,"");
    fl_set_object_callback(obj,EXPLnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,625,340,60,30,"Frame\nNumber:");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,EXPLnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,625,370,50,30,"Frame\nTime:");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,EXPLnoneCB,0);
  fdui->ds_image = obj = fl_add_canvas(FL_NORMAL_CANVAS,5,5,580,430,"");
    fl_set_object_callback(obj,EXPLnoneCB,0);
  fdui->dsim_vscroll = obj = fl_add_scrollbar(FL_VERT_SCROLLBAR,590,0,20,440,"");
    fl_set_object_callback(obj,dsimscrollCB,0);
  fdui->dsim_hscroll = obj = fl_add_scrollbar(FL_HOR_SCROLLBAR,0,440,590,20,"");
    fl_set_object_callback(obj,dsimscrollCB,1);
  fdui->dsim_percent = obj = fl_add_slider(FL_HOR_FILL_SLIDER,680,315,150,20,"% Loaded:");
    fl_set_object_color(obj,FL_WHITE,FL_BLUE);
    fl_set_object_lsize(obj,FL_DEFAULT_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,EXPLnoneCB,0);
    fl_set_slider_bounds(obj, 0, 100);
    fl_set_slider_value(obj, 0);
    fl_set_slider_size(obj, 0.15);
  fdui->dsim_delay = obj = fl_add_dial(FL_LINE_DIAL,800,340,30,30,"Delay is\n1000 ms");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,dsimdelayCB,0);
    fl_set_dial_bounds(obj, 500, 2000);
    fl_set_dial_value(obj, 1000);
    fl_set_dial_step(obj, 100);
    fl_set_dial_return(obj, FL_RETURN_CHANGED);
  fl_end_form();

  fdui->explain->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_expl_complete *create_form_expl_complete(void)
{
  FL_OBJECT *obj;
  FD_expl_complete *fdui = (FD_expl_complete *) fl_calloc(1, sizeof(*fdui));

  fdui->expl_complete = fl_bgn_form(FL_NO_BOX, 471, 146);
  obj = fl_add_box(FL_UP_BOX,0,0,471,146,"");
  fdui->percent = obj = fl_add_slider(FL_HOR_FILL_SLIDER,60,85,350,25,"Percent complete:");
    fl_set_object_color(obj,FL_RED,FL_WHITE);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,EXPLnoneCB,0);
    fl_set_slider_bounds(obj, 0, 100);
    fl_set_slider_value(obj, 0);
    fl_set_slider_step(obj, 1);
  fdui->message = obj = fl_add_text(FL_NORMAL_TEXT,20,25,430,25,"");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,EXPLnoneCB,0);
  fl_end_form();

  fdui->expl_complete->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

