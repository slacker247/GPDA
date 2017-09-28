/* Form definition file generated with fdesign. */

#include "forms.h"
#include <stdlib.h>
#include "CCPforms.h"

FD_ccpmanage *create_form_ccpmanage(void)
{
  FL_OBJECT *obj;
  FD_ccpmanage *fdui = (FD_ccpmanage *) fl_calloc(1, sizeof(*fdui));

  fdui->ccpmanage = fl_bgn_form(FL_NO_BOX, 850, 460);
  obj = fl_add_box(FL_UP_BOX,0,0,850,460,"");
  fdui->pix_manage = obj = fl_add_pixmap(FL_NORMAL_PIXMAP,0,5,660,450,"");
    fl_set_object_callback(obj,CCPnoneCB,0);
    fl_set_pixmap_file(obj, "../BitMaps/force-manage.xpm");
  obj = fl_add_button(FL_NORMAL_BUTTON,760,10,80,30,"Exit");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,CCPexitCB,1);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,660,50,180,10,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_callback(obj,CCPnoneCB,0);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,660,180,180,10,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_callback(obj,CCPnoneCB,0);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,660,320,180,10,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_callback(obj,CCPnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,670,100,170,20,"THREAT PLATFORM");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,CCPnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,670,250,170,20,"AIRCRAFT BASE AT RISK");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,CCPnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,670,380,170,20,"BASE VULNERABILITY");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,CCPnoneCB,0);
  fl_end_form();

  fdui->ccpmanage->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_ccpsurvival *create_form_ccpsurvival(void)
{
  FL_OBJECT *obj;
  FD_ccpsurvival *fdui = (FD_ccpsurvival *) fl_calloc(1, sizeof(*fdui));

  fdui->ccpsurvival = fl_bgn_form(FL_NO_BOX, 850, 460);
  obj = fl_add_box(FL_UP_BOX,0,0,850,460,"");
  obj = fl_add_button(FL_NORMAL_BUTTON,760,10,80,30,"Exit");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,CCPexitCB,2);
  fdui->pix_survival = obj = fl_add_pixmap(FL_NORMAL_PIXMAP,0,5,660,450,"");
    fl_set_object_callback(obj,CCPnoneCB,0);
    fl_set_pixmap_file(obj, "../BitMaps/force-survival.xpm");
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,660,50,180,10,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_callback(obj,CCPnoneCB,0);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,660,180,180,10,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_callback(obj,CCPnoneCB,0);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,660,330,180,10,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_callback(obj,CCPnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,670,100,170,20,"LAUNCH ORIGIN");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,CCPnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,660,240,180,20,"DECISION TIME REMAINING");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,CCPnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,660,380,180,20,"INTEREST POINTS AT RISK");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,CCPnoneCB,0);
  fl_end_form();

  fdui->ccpsurvival->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_ccpattack *create_form_ccpattack(void)
{
  FL_OBJECT *obj;
  FD_ccpattack *fdui = (FD_ccpattack *) fl_calloc(1, sizeof(*fdui));

  fdui->ccpattack = fl_bgn_form(FL_NO_BOX, 850, 460);
  obj = fl_add_box(FL_UP_BOX,0,0,850,460,"");
  obj = fl_add_button(FL_NORMAL_BUTTON,760,10,80,30,"Exit");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,CCPexitCB,3);
  fdui->pix_attack = obj = fl_add_pixmap(FL_NORMAL_PIXMAP,0,5,660,450,"");
    fl_set_object_callback(obj,CCPnoneCB,0);
    fl_set_pixmap_file(obj, "../BitMaps/attack-assess.xpm");
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,660,50,180,10,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_callback(obj,CCPnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,660,90,180,20,"INTEREST POINTS AT RISK");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,CCPnoneCB,0);
  fl_end_form();

  fdui->ccpattack->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

