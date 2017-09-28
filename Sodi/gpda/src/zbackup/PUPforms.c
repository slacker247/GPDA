/* Form definition file generated with fdesign. */

#include "forms.h"
#include <stdlib.h>
#include "PUPforms.h"

FD_pup *create_form_pup(void)
{
  FL_OBJECT *obj;
  FD_pup *fdui = (FD_pup *) fl_calloc(1, sizeof(*fdui));

  fdui->pup = fl_bgn_form(FL_NO_BOX, 851, 461);
  obj = fl_add_box(FL_FRAME_BOX,0,0,851,461,"");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
  fdui->pup_log = obj = fl_add_browser(FL_HOLD_BROWSER,280,315,560,135,"History Log");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_color(obj,FL_LEFT_BCOL,FL_YELLOW);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,PUPnoneCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,750,10,90,30,"Dismiss");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,PUPexitCB,0);
  fdui->pup_shell = obj = fl_add_input(FL_NORMAL_INPUT,365,65,385,25,"Plan Update Fix");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,PUPnoneCB,0);
  fdui->pup_exec = obj = fl_add_button(FL_NORMAL_BUTTON,755,65,80,25,"<- Execute");
    fl_set_object_lcolor(obj,FL_RED);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,pupexecCB,0);
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,20,355,240,95,"Score");
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,PUPnoneCB,0);
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,20,80,240,230,"Current Status");
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,PUPnoneCB,0);
  fdui->pup_belief = obj = fl_add_input(FL_NORMAL_INPUT,150,90,100,20,"Belief");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,PUPnoneCB,0);
  fdui->pup_disbelief = obj = fl_add_input(FL_NORMAL_INPUT,150,150,100,20,"Disbelief");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,PUPnoneCB,0);
  fdui->pup_bthresh = obj = fl_add_input(FL_NORMAL_INPUT,150,120,100,20,"Belief Threshold");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,PUPnoneCB,0);
  fdui->pup_dthresh = obj = fl_add_input(FL_NORMAL_INPUT,150,180,100,20,"Disbelief Threshold");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,PUPnoneCB,0);
  fdui->pup_weight = obj = fl_add_input(FL_NORMAL_INPUT,150,210,100,20,"Weight");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,PUPnoneCB,0);
  fdui->pup_update = obj = fl_add_choice(FL_NORMAL_CHOICE2,30,285,220,20,"");
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_callback(obj,pupupdateCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,30,265,220,20,"Plan Update");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,PUPnoneCB,0);
  fdui->pup_time = obj = fl_add_input(FL_NORMAL_INPUT,150,240,100,20,"Time");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,PUPnoneCB,0);
  fdui->pup_estimated = obj = fl_add_input(FL_NORMAL_INPUT,150,370,100,20,"Estimated");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,PUPnoneCB,0);
  fdui->pup_actual = obj = fl_add_input(FL_NORMAL_INPUT,150,395,100,20,"Actual");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,PUPnoneCB,0);
  fdui->pup_worth = obj = fl_add_input(FL_NORMAL_INPUT,150,420,100,20,"Worth");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,PUPnoneCB,0);
  fdui->pup_strategy = obj = fl_add_browser(FL_HOLD_BROWSER,280,115,555,170,"Plan Update Strategies");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_color(obj,FL_WHITE,FL_YELLOW);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,pupstratCB,0);
  fdui->pup_action = obj = fl_add_button(FL_NORMAL_BUTTON,280,65,80,25,"Operator");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_callback(obj,PUPnoneCB,0);
  fdui->pup_hypothesis = obj = fl_add_choice(FL_NORMAL_CHOICE2,20,30,240,25,"Hypothesis");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_callback(obj,PUPnoneCB,0);
  fl_end_form();

  fdui->pup->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

