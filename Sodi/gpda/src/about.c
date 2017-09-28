/* Form definition file generated with fdesign. */

#include "forms.h"
#include <stdlib.h>
#include "about.h"

FD_About_Form *create_form_About_Form(void)
{
  FL_OBJECT *obj;
  FD_About_Form *fdui = (FD_About_Form *) fl_calloc(1, sizeof(*fdui));

  fdui->About_Form = fl_bgn_form(FL_NO_BOX, 386, 411);
  obj = fl_add_box(FL_UP_BOX,0,0,386,411,"");
    fl_set_object_color(obj,FL_MCOL,FL_COL1);
    fl_set_object_gravity(obj, FL_NorthWest, FL_SouthEast);
  obj = fl_add_text(FL_NORMAL_TEXT,15,10,355,30,"General Purpose Decision Aids");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lcolor(obj,FL_INDIANRED);
    fl_set_object_lsize(obj,FL_HUGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_TIMESBOLD_STYLE+FL_EMBOSSED_STYLE);
    fl_set_object_gravity(obj, FL_NorthWest, FL_NorthWest);
  fdui->About_Go_Back = obj = fl_add_button(FL_RETURN_BUTTON,150,365,70,30,"Ok");
    fl_set_object_gravity(obj, FL_NorthWest, FL_NorthWest);
    fl_set_object_callback(obj,aboutexitCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,140,135,235,35,"Copyright (c) 2000-2002 by TRW\n(Contains TRW Proprietary Information)");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_TIMESITALIC_STYLE);
    fl_set_object_gravity(obj, FL_NorthWest, FL_NorthWest);
  fdui->Gena = obj = fl_add_pixmap(FL_NORMAL_PIXMAP,15,140,120,120,"");
    fl_set_object_color(obj,FL_BLACK,FL_TOP_BCOL);
    fl_set_object_gravity(obj, FL_NorthWest, FL_NorthWest);
    fl_set_pixmap_file(obj, "../BitMaps/DecisionAid.xpm");
  obj = fl_add_text(FL_NORMAL_TEXT,10,110,360,20,"See http://129.193.164.57 for more details");
    fl_set_object_color(obj,FL_MCOL,FL_MCOL);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_gravity(obj, FL_NorthWest, FL_NorthWest);
  obj = fl_add_text(FL_NORMAL_TEXT,140,170,235,35,"XFORMS Copyright(c) 1996-1998 by\nT.C. Zhao and Mark Overmars");
    fl_set_object_color(obj,FL_MCOL,FL_MCOL);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_TIMESITALIC_STYLE);
  fdui->about_version = obj = fl_add_text(FL_NORMAL_TEXT,25,60,150,20,"Version: 2.2");
    fl_set_object_color(obj,FL_TOP_BCOL,FL_LIGHTER_COL1);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->about_os = obj = fl_add_text(FL_NORMAL_TEXT,200,60,150,20,"OS: Linux");
    fl_set_object_color(obj,FL_TOP_BCOL,FL_LIGHTER_COL1);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->about_date = obj = fl_add_text(FL_NORMAL_TEXT,25,85,150,20,"Compiled: 2002");
    fl_set_object_color(obj,FL_TOP_BCOL,FL_LIGHTER_COL1);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->about_poc = obj = fl_add_text(FL_NORMAL_TEXT,200,85,150,20,"POC: Dennis Ellis");
    fl_set_object_color(obj,FL_TOP_BCOL,FL_LIGHTER_COL1);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,10,45,360,5,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_callback(obj,BMCnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,135,205,235,35,"GAlib Copyright(c) 1996 by\nMatthew Wall (MIT)");
    fl_set_object_color(obj,FL_MCOL,FL_MCOL);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_TIMESITALIC_STYLE);
  obj = fl_add_text(FL_NORMAL_TEXT,140,240,235,35,"FuzzyCLIPS Copyright(c) 1998 by\nNational Research Council Canada");
    fl_set_object_color(obj,FL_MCOL,FL_MCOL);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_TIMESITALIC_STYLE);
  fl_end_form();

  fdui->About_Form->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

