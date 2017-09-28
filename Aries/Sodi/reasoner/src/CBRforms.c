/* Form definition file generated with fdesign. */

#include "forms.h"
#include <stdlib.h>
#include "CBRforms.h"

FD_add *create_form_add(void)
{
  FL_OBJECT *obj;
  FD_add *fdui = (FD_add *) fl_calloc(1, sizeof(*fdui));

  fdui->add = fl_bgn_form(FL_NO_BOX, 310, 230);
  obj = fl_add_box(FL_UP_BOX,0,0,310,230,"");
    fl_set_object_color(obj,FL_BLACK,FL_COL1);
  fdui->add_now = obj = fl_add_button(FL_NORMAL_BUTTON,20,180,110,30,"Add");
    fl_set_object_color(obj,FL_BLUE,FL_COL1);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,add_nowCB,0);
  fdui->add_cancel = obj = fl_add_button(FL_NORMAL_BUTTON,180,180,110,30,"Cancel");
    fl_set_object_color(obj,FL_BLUE,FL_COL1);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,add_cancelCB,0);
  fdui->add_loc = obj = fl_add_input(FL_NORMAL_INPUT,230,120,60,30,"Input name/character of location");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_callback(obj,add_locCB,0);
  fdui->add_output = obj = fl_add_text(FL_NORMAL_TEXT,20,20,270,80,"Output");
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_color(obj,FL_MCOL,FL_MCOL);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fl_end_form();

  fdui->add->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_changer *create_form_changer(void)
{
  FL_OBJECT *obj;
  FD_changer *fdui = (FD_changer *) fl_calloc(1, sizeof(*fdui));

  fdui->changer = fl_bgn_form(FL_NO_BOX, 420, 210);
  obj = fl_add_box(FL_UP_BOX,0,0,420,210,"");
    fl_set_object_color(obj,FL_DODGERBLUE,FL_COL1);
  fdui->output = obj = fl_add_text(FL_NORMAL_TEXT,10,20,400,120,"Information here");
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_color(obj,FL_LEFT_BCOL,FL_WHITE);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->done = obj = fl_add_button(FL_NORMAL_BUTTON,300,170,110,30,"Done");
    fl_set_object_color(obj,FL_BLACK,FL_COL1);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,doneCB,0);
  fl_end_form();

  fdui->changer->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_char_changer *create_form_char_changer(void)
{
  FL_OBJECT *obj;
  FD_char_changer *fdui = (FD_char_changer *) fl_calloc(1, sizeof(*fdui));

  fdui->char_changer = fl_bgn_form(FL_NO_BOX, 460, 250);
  obj = fl_add_box(FL_UP_BOX,0,0,460,250,"");
    fl_set_object_color(obj,FL_BLACK,FL_COL1);
  fdui->char_output = obj = fl_add_text(FL_NORMAL_TEXT,20,20,420,100,"Output");
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->char_remove = obj = fl_add_button(FL_NORMAL_BUTTON,20,200,170,30,"Remove Locations");
    fl_set_object_color(obj,FL_BLUE,FL_COL1);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,char_remove,0);
  fdui->char_add = obj = fl_add_button(FL_NORMAL_BUTTON,20,150,170,30,"Add Locations");
    fl_set_object_color(obj,FL_BLUE,FL_COL1);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,char_add,0);
  fdui->char_never_mind = obj = fl_add_button(FL_NORMAL_BUTTON,320,180,120,30,"Done");
    fl_set_object_color(obj,FL_BLUE,FL_COL1);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,char_never_mind,0);
  fl_end_form();

  fdui->char_changer->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_criteria *create_form_criteria(void)
{
  FL_OBJECT *obj;
  FD_criteria *fdui = (FD_criteria *) fl_calloc(1, sizeof(*fdui));

  fdui->criteria = fl_bgn_form(FL_NO_BOX, 970, 600);
  obj = fl_add_box(FL_UP_BOX,0,0,970,600,"");
  obj = fl_add_box(FL_UP_BOX,310,220,150,50,"");
    fl_set_object_color(obj,FL_RED,FL_RED);
    fl_set_object_lcolor(obj,FL_RED);
  fdui->max_threats = obj = fl_add_choice(FL_DROPLIST_CHOICE,320,230,130,30,"Maximum Number of Threats in Route  ");
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,max_threatsCB,0);
  obj = fl_add_box(FL_UP_BOX,310,340,150,50,"");
    fl_set_object_color(obj,FL_RED,FL_RED);
    fl_set_object_lcolor(obj,FL_RED);
  obj = fl_add_box(FL_UP_BOX,310,280,150,50,"");
    fl_set_object_color(obj,FL_RED,FL_RED);
    fl_set_object_lcolor(obj,FL_RED);
  obj = fl_add_box(FL_UP_BOX,770,540,160,50,"");
    fl_set_object_color(obj,FL_YELLOW,FL_YELLOW);
    fl_set_object_lcolor(obj,FL_YELLOW);
  obj = fl_add_box(FL_UP_BOX,770,490,160,50,"");
    fl_set_object_color(obj,FL_YELLOW,FL_YELLOW);
    fl_set_object_lcolor(obj,FL_YELLOW);
  obj = fl_add_box(FL_UP_BOX,770,440,160,50,"");
    fl_set_object_color(obj,FL_YELLOW,FL_YELLOW);
    fl_set_object_lcolor(obj,FL_YELLOW);
  obj = fl_add_box(FL_UP_BOX,770,390,160,50,"");
    fl_set_object_color(obj,FL_YELLOW,FL_YELLOW);
    fl_set_object_lcolor(obj,FL_YELLOW);
  obj = fl_add_box(FL_UP_BOX,770,210,160,50,"");
    fl_set_object_color(obj,FL_GREEN,FL_COL1);
  obj = fl_add_box(FL_UP_BOX,770,270,160,50,"");
    fl_set_object_color(obj,FL_GREEN,FL_COL1);
  obj = fl_add_text(FL_NORMAL_TEXT,510,170,90,30,"Terrain");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_HUGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_text(FL_NORMAL_TEXT,10,10,140,30,"Operations");
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lsize(obj,FL_HUGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_text(FL_NORMAL_TEXT,510,330,160,30,"Vulnerability");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_HUGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->terrain = obj = fl_add_choice(FL_DROPLIST_CHOICE,780,220,140,30,"Terrain  ");
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,terrainCB,0);
  fdui->slope = obj = fl_add_choice(FL_DROPLIST_CHOICE,780,280,140,30,"Slope  (in percent)  ");
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,slopeCB,0);
  fdui->timing = obj = fl_add_choice(FL_DROPLIST_CHOICE,320,290,130,30,"Prefered Timing in Instance of Threat  ");
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,threat_timingCB,0);
  fdui->strength = obj = fl_add_choice(FL_DROPLIST_CHOICE,320,350,130,30,"Maximum Threat Streangth  ");
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,strengthCB,0);
  obj = fl_add_box(FL_UP_BOX,260,50,200,50,"");
    fl_set_object_color(obj,FL_BLUE,FL_BLUE);
    fl_set_object_lcolor(obj,FL_BLUE);
  fdui->time = obj = fl_add_choice(FL_DROPLIST_CHOICE,270,60,180,30,"Choose Time Criticality  ");
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,timeCB,0);
  obj = fl_add_box(FL_UP_BOX,260,110,200,50,"");
    fl_set_object_color(obj,FL_BLUE,FL_BLUE);
  obj = fl_add_box(FL_UP_BOX,320,520,140,50,"");
    fl_set_object_color(obj,FL_BLUE,FL_DARKVIOLET);
    fl_set_object_lcolor(obj,FL_DARKVIOLET);
  obj = fl_add_box(FL_UP_BOX,100,520,150,50,"");
    fl_set_object_color(obj,FL_BLUE,FL_DARKVIOLET);
    fl_set_object_lcolor(obj,FL_DARKVIOLET);
  obj = fl_add_box(FL_UP_BOX,100,440,150,50,"");
    fl_set_object_color(obj,FL_BLUE,FL_DARKVIOLET);
    fl_set_object_lcolor(obj,FL_DARKVIOLET);
  obj = fl_add_box(FL_UP_BOX,290,440,170,50,"");
    fl_set_object_color(obj,FL_BLUE,FL_DARKVIOLET);
    fl_set_object_lcolor(obj,FL_DARKVIOLET);
  fdui->commodities = obj = fl_add_choice(FL_DROPLIST_CHOICE,110,450,130,30,"Commoditites  ");
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,commoditiesCB,0);
  fdui->equipage = obj = fl_add_choice(FL_DROPLIST_CHOICE,330,530,120,30,"Equipage  ");
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,equipageCB,0);
  fdui->fuel = obj = fl_add_choice(FL_DROPLIST_CHOICE,300,450,150,30,"Fuel  ");
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,fuelCB,0);
  fdui->subsidiaries = obj = fl_add_choice(FL_DROPLIST_CHOICE,110,530,130,30,"Subsidiaries  ");
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,subsidiariesCB,0);
  fdui->expected_down_comm = obj = fl_add_choice(FL_DROPLIST_CHOICE,782,400,136,30,"Expected Downed Communications  ");
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,expected_down_commCB,0);
  fdui->unexpected_down_comm = obj = fl_add_choice(FL_DROPLIST_CHOICE,782,450,136,30,"Unexpected Downed Communications  ");
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,unexpected_down_comm,0);
  fdui->natural_elements = obj = fl_add_choice(FL_DROPLIST_CHOICE,781,500,138,30,"Natural Elements  ");
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,natural_elemenstCB,0);
  fdui->mechanical = obj = fl_add_choice(FL_DROPLIST_CHOICE,780,550,140,30,"Mechanical Failure  ");
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,mechanicalCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,10,570,230,20,"(troop reinforcements, medical support) ");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
  obj = fl_add_text(FL_NORMAL_TEXT,10,490,240,20,"(i.e. food, water, frilities of civilization)");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
  obj = fl_add_text(FL_NORMAL_TEXT,250,570,200,20,"(ease of access to machinery items)");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
  obj = fl_add_text(FL_NORMAL_TEXT,130,390,260,30,"Criticality of following criteria to mission ");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE);
  obj = fl_add_text(FL_NORMAL_TEXT,10,380,120,40,"Logistics");
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lsize(obj,FL_HUGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_text(FL_NORMAL_TEXT,10,170,90,50,"Threat");
    fl_set_object_lcolor(obj,FL_RED);
    fl_set_object_lsize(obj,FL_HUGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_text(FL_NORMAL_TEXT,100,190,230,20,"Allowable criteria in threat interaction");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE);
  obj = fl_add_text(FL_NORMAL_TEXT,560,10,330,50,"COA Selection Criteria");
    fl_set_object_lsize(obj,FL_HUGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE);
  obj = fl_add_text(FL_NORMAL_TEXT,620,50,210,30,"Input Display");
    fl_set_object_lsize(obj,FL_HUGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE);
  fdui->start = obj = fl_add_button(FL_NORMAL_BUTTON,610,100,80,30,"START");
    fl_set_object_color(obj,FL_BLACK,FL_COL1);
    fl_set_object_lcolor(obj,FL_WHITE);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,startCB,0);
  fdui->exit = obj = fl_add_button(FL_NORMAL_BUTTON,740,100,80,30,"EXIT");
    fl_set_object_color(obj,FL_BLACK,FL_COL1);
    fl_set_object_lcolor(obj,FL_WHITE);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,exitCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,680,340,260,20,"Possibilty of following criteria contributing");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE);
  obj = fl_add_text(FL_NORMAL_TEXT,610,190,10,10,"Text");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
  obj = fl_add_text(FL_NORMAL_TEXT,680,360,180,20,"to failure of mission");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE);
  obj = fl_add_text(FL_NORMAL_TEXT,150,20,340,20,"Importance of below criteria for mission effectiveness");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE);
  obj = fl_add_text(FL_NORMAL_TEXT,610,180,310,20,"Minimum preferred/required type of land of travel ");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE);
  fdui->distance = obj = fl_add_choice(FL_DROPLIST_CHOICE,270,120,180,30,"Route Distance Criticality  ");
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,distanceCB,0);
  fl_end_form();

  fdui->criteria->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_final *create_form_final(void)
{
  FL_OBJECT *obj;
  FD_final *fdui = (FD_final *) fl_calloc(1, sizeof(*fdui));

  fdui->final = fl_bgn_form(FL_NO_BOX, 810, 490);
  obj = fl_add_box(FL_UP_BOX,0,0,810,490,"");
    fl_set_object_color(obj,FL_BLACK,FL_COL1);
  fdui->cancel_final = obj = fl_add_button(FL_NORMAL_BUTTON,540,430,140,40,"Cancel");
    fl_set_object_color(obj,FL_BLUE,FL_COL1);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,cancel_finalCB,0);
  fdui->accept = obj = fl_add_button(FL_NORMAL_BUTTON,380,430,140,40,"Accept");
    fl_set_object_color(obj,FL_BLUE,FL_COL1);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,acceptCB,0);
  fdui->start_over = obj = fl_add_button(FL_NORMAL_BUTTON,220,430,140,40,"Start Over");
    fl_set_object_color(obj,FL_BLUE,FL_COL1);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,start_overCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,290,10,260,50,"Course of Action Selected");
    fl_set_object_boxtype(obj,FL_UP_BOX);
    fl_set_object_color(obj,FL_RIGHT_BCOL,FL_MCOL);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->text_box1 = obj = fl_add_browser(FL_NORMAL_BROWSER,20,100,490,290,"");
    fl_set_object_color(obj,FL_TOP_BCOL,FL_BLACK);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_box(FL_DOWN_BOX,530,130,260,220,"");
  fl_end_form();

  fdui->final->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_float_changer *create_form_float_changer(void)
{
  FL_OBJECT *obj;
  FD_float_changer *fdui = (FD_float_changer *) fl_calloc(1, sizeof(*fdui));

  fdui->float_changer = fl_bgn_form(FL_NO_BOX, 420, 260);
  obj = fl_add_box(FL_UP_BOX,0,0,420,260,"");
    fl_set_object_color(obj,FL_BLACK,FL_COL1);
  fdui->float_output = obj = fl_add_text(FL_NORMAL_TEXT,20,20,380,90,"float_ouput");
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_color(obj,FL_TOP_BCOL,FL_MCOL);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->float_input = obj = fl_add_input(FL_FLOAT_INPUT,200,140,160,30,"Enter real number value");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,float_inputCB,0);
  fdui->done4 = obj = fl_add_button(FL_NORMAL_BUTTON,50,210,110,30,"Done");
    fl_set_object_color(obj,FL_BLUE,FL_COL1);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,done4CB,0);
  fdui->nevermind = obj = fl_add_button(FL_NORMAL_BUTTON,250,210,110,30,"Never Mind");
    fl_set_object_color(obj,FL_BLUE,FL_COL1);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,nevermind3CB,0);
  fl_end_form();

  fdui->float_changer->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_graphs *create_form_graphs(void)
{
  FL_OBJECT *obj;
  FD_graphs *fdui = (FD_graphs *) fl_calloc(1, sizeof(*fdui));

  fdui->graphs = fl_bgn_form(FL_NO_BOX, 960, 610);
  obj = fl_add_box(FL_UP_BOX,0,0,960,610,"");
  fdui->graph1 = obj = fl_add_chart(FL_BAR_CHART,130,70,820,160,"Total Case Points Comparison");
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,graph1CB,0);
  fdui->graph2 = obj = fl_add_chart(FL_BAR_CHART,130,250,820,160,"Total Match Case Points (index values in a case match that of given criteria)");
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,graph2CB,0);
  fdui->graph3 = obj = fl_add_chart(FL_BAR_CHART,130,430,820,160,"Case Comparisons After Modification");
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,graph3CB,0);
  fdui->graph_done = obj = fl_add_button(FL_NORMAL_BUTTON,10,10,120,50,"Done");
    fl_set_object_color(obj,FL_BLUE,FL_COL1);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,graph_doneCB,0);
  fdui->text1 = obj = fl_add_text(FL_NORMAL_TEXT,10,70,110,160,"");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->text2 = obj = fl_add_text(FL_NORMAL_TEXT,10,250,110,160,"");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->text3 = obj = fl_add_text(FL_NORMAL_TEXT,10,430,110,160,"");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fl_end_form();

  fdui->graphs->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_one *create_form_one(void)
{
  FL_OBJECT *obj;
  FD_one *fdui = (FD_one *) fl_calloc(1, sizeof(*fdui));

  fdui->one = fl_bgn_form(FL_NO_BOX, 320, 250);
  obj = fl_add_box(FL_UP_BOX,0,0,320,250,"");
  fdui->button = obj = fl_add_button(FL_NORMAL_BUTTON,210,90,90,60,"Button");
    fl_set_object_color(obj,FL_ORCHID,FL_COL1);
    fl_set_object_callback(obj,buttonCB,0);
  fl_end_form();

  fdui->one->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_remove *create_form_remove(void)
{
  FL_OBJECT *obj;
  FD_remove *fdui = (FD_remove *) fl_calloc(1, sizeof(*fdui));

  fdui->remove = fl_bgn_form(FL_NO_BOX, 320, 230);
  obj = fl_add_box(FL_UP_BOX,0,0,320,230,"");
    fl_set_object_color(obj,FL_BLACK,FL_COL1);
  fdui->remove_remove = obj = fl_add_button(FL_NORMAL_BUTTON,30,180,110,30,"Remove");
    fl_set_object_color(obj,FL_BLUE,FL_COL1);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,remove_removeCB,0);
  fdui->remove_cancel = obj = fl_add_button(FL_NORMAL_BUTTON,180,180,110,30,"Cancel");
    fl_set_object_color(obj,FL_BLUE,FL_COL1);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,remove_cancelCB,0);
  fdui->remove_input = obj = fl_add_input(FL_NORMAL_INPUT,240,120,50,30,"Input Node/Letter to Remove");
    fl_set_object_color(obj,FL_YELLOW,FL_MCOL);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,remove_inputCB,0);
  fdui->remove_output = obj = fl_add_text(FL_NORMAL_TEXT,20,20,270,80,"Output");
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fl_end_form();

  fdui->remove->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_terrain_changer *create_form_terrain_changer(void)
{
  FL_OBJECT *obj;
  FD_terrain_changer *fdui = (FD_terrain_changer *) fl_calloc(1, sizeof(*fdui));

  fdui->terrain_changer = fl_bgn_form(FL_NO_BOX, 430, 300);
  obj = fl_add_box(FL_UP_BOX,0,0,430,300,"");
    fl_set_object_color(obj,FL_BLACK,FL_COL1);
  fdui->terrain_output = obj = fl_add_text(FL_NORMAL_TEXT,20,20,390,110,"Terrain");
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_color(obj,FL_TOP_BCOL,FL_MCOL);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->new_terrain = obj = fl_add_choice(FL_DROPLIST_CHOICE,140,160,250,30,"Choose new terrain");
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,new_terrainCB,0);
  fdui->nevermind2 = obj = fl_add_button(FL_NORMAL_BUTTON,240,240,100,30,"Never Mind");
    fl_set_object_color(obj,FL_BLUE,FL_BLUE);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,nevermind2,0);
  fdui->done3 = obj = fl_add_button(FL_NORMAL_BUTTON,70,240,100,30,"Done");
    fl_set_object_color(obj,FL_BLUE,FL_COL1);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,done3CB,0);
  fl_end_form();

  fdui->terrain_changer->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_test *create_form_test(void)
{
  FL_OBJECT *obj;
  FD_test *fdui = (FD_test *) fl_calloc(1, sizeof(*fdui));

  fdui->test = fl_bgn_form(FL_NO_BOX, 970, 640);
  obj = fl_add_box(FL_UP_BOX,0,0,970,640,"");
  fdui->coa1 = obj = fl_add_browser(FL_HOLD_BROWSER,190,300,120,290,"");
    fl_set_object_color(obj,FL_TOP_BCOL,FL_YELLOW);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP);
    fl_set_object_callback(obj,coa1CB,0);
  fdui->coa2 = obj = fl_add_browser(FL_HOLD_BROWSER,510,300,120,290,"");
    fl_set_object_color(obj,FL_TOP_BCOL,FL_YELLOW);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP);
    fl_set_object_callback(obj,coa2CB,0);
  fdui->coa3 = obj = fl_add_browser(FL_HOLD_BROWSER,830,300,130,290,"");
    fl_set_object_color(obj,FL_TOP_BCOL,FL_YELLOW);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP);
    fl_set_object_callback(obj,coa3CB,0);
  fdui->compute = obj = fl_add_button(FL_NORMAL_BUTTON,240,600,120,30,"Decide Case");
    fl_set_object_color(obj,FL_BLACK,FL_COL1);
    fl_set_object_lcolor(obj,FL_WHITE);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,computeCB,0);
  fdui->exit2 = obj = fl_add_button(FL_NORMAL_BUTTON,580,600,120,30,"exit");
    fl_set_object_color(obj,FL_BLACK,FL_COL1);
    fl_set_object_lcolor(obj,FL_WHITE);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,exit2CB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,730,270,150,30,"COA #3");
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_text(FL_NORMAL_TEXT,80,270,150,30,"COA #1");
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->coa1info = obj = fl_add_browser(FL_NORMAL_BROWSER,20,300,170,290,"");
    fl_set_object_color(obj,FL_LEFT_BCOL,FL_TOP_BCOL);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,coa1infoCB,0);
  fdui->coa2info = obj = fl_add_browser(FL_NORMAL_BROWSER,340,300,170,290,"");
    fl_set_object_color(obj,FL_LEFT_BCOL,FL_YELLOW);
    fl_set_object_lcolor(obj,43);
    fl_set_object_callback(obj,coa2infoCB,0);
  fdui->coa3info = obj = fl_add_browser(FL_NORMAL_BROWSER,660,300,170,290,"");
    fl_set_object_color(obj,FL_LEFT_BCOL,FL_YELLOW);
    fl_set_object_callback(obj,coa3infoCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,410,270,140,30,"COA #2");
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->route1 = obj = fl_add_chart(FL_LINE_CHART,10,70,310,180,"");
    fl_set_object_color(obj,FL_MCOL,FL_COL1);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,route1CB,0);
  fdui->route2 = obj = fl_add_chart(FL_LINE_CHART,330,70,310,180,"");
    fl_set_object_color(obj,FL_MCOL,FL_COL1);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,route2CB,0);
  fdui->route3 = obj = fl_add_chart(FL_LINE_CHART,650,70,310,180,"");
    fl_set_object_color(obj,FL_MCOL,FL_COL1);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,route3CB,0);
  fl_end_form();

  fdui->test->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_time_changer *create_form_time_changer(void)
{
  FL_OBJECT *obj;
  FD_time_changer *fdui = (FD_time_changer *) fl_calloc(1, sizeof(*fdui));

  fdui->time_changer = fl_bgn_form(FL_NO_BOX, 480, 250);
  obj = fl_add_box(FL_UP_BOX,0,0,480,250,"");
    fl_set_object_color(obj,FL_BLACK,FL_COL1);
  fdui->output1 = obj = fl_add_text(FL_NORMAL_TEXT,20,20,440,90,"output");
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_color(obj,FL_TOP_BCOL,FL_LEFT_BCOL);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->input = obj = fl_add_input(FL_INT_INPUT,230,130,160,30,"Enter new integer value");
    fl_set_object_color(obj,FL_TOP_BCOL,FL_TOP_BCOL);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,inputCB,0);
  fdui->done2 = obj = fl_add_button(FL_NORMAL_BUTTON,80,200,110,30,"Done");
    fl_set_object_color(obj,FL_BLUE,FL_COL1);
    fl_set_object_lcolor(obj,FL_WHITE);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,done2CB,0);
  fdui->nevermind = obj = fl_add_button(FL_NORMAL_BUTTON,280,200,110,30,"Never Mind");
    fl_set_object_color(obj,FL_BLUE,FL_COL1);
    fl_set_object_lcolor(obj,FL_WHITE);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,nevermindCB,0);
  fl_end_form();

  fdui->time_changer->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

