/* Form definition file generated with fdesign. */

#include "forms.h"
#include <stdlib.h>
#include "JTAMCforms.h"

FD_ODI *create_form_ODI(void)
{
  FL_OBJECT *obj;
  FD_ODI *fdui = (FD_ODI *) fl_calloc(1, sizeof(*fdui));

  fdui->ODI = fl_bgn_form(FL_NO_BOX, 940, 660);
  obj = fl_add_box(FL_UP_BOX,0,0,940,660,"");
  fdui->background = obj = fl_add_pixmap(FL_NORMAL_PIXMAP,10,50,920,580,"Offense/Defense Demonstration System");
    fl_set_object_lsize(obj,FL_HUGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_SHADOW_STYLE);
    fl_set_object_callback(obj,nothingCB,0);
    fl_set_pixmap_file(obj, "/home/dre/Jtamv/BitMaps/NMDback.xpm");
  fdui->battle_button = obj = fl_add_button(FL_NORMAL_BUTTON,10,160,150,100,"Offense/\nDefense\nIntegrated\nView");
    fl_set_object_boxtype(obj,FL_EMBOSSED_BOX);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,battleCB,0);
  fdui->visual_button = obj = fl_add_button(FL_NORMAL_BUTTON,10,280,150,90,"Mission\nPreview");
    fl_set_object_boxtype(obj,FL_EMBOSSED_BOX);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,visualCB,0);
  fdui->config_button = obj = fl_add_button(FL_NORMAL_BUTTON,160,390,150,60,"Configuration\nEditor");
    fl_set_object_boxtype(obj,FL_EMBOSSED_BOX);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,configCB,0);
  fdui->scenario_button = obj = fl_add_button(FL_NORMAL_BUTTON,160,450,150,60,"Scenario\nEditor");
    fl_set_object_boxtype(obj,FL_EMBOSSED_BOX);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,scenarioCB,0);
  fdui->planner_button = obj = fl_add_button(FL_NORMAL_BUTTON,160,510,150,60,"Simulator");
    fl_set_object_boxtype(obj,FL_EMBOSSED_BOX);
    fl_set_object_lcolor(obj,FL_WHEAT);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,plannerCB,0);
  fdui->quit_button = obj = fl_add_button(FL_NORMAL_BUTTON,10,580,150,60,"Quit");
    fl_set_object_boxtype(obj,FL_EMBOSSED_BOX);
    fl_set_object_lcolor(obj,FL_RED);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,quitCB,0);
  fdui->utility_button = obj = fl_add_button(FL_NORMAL_BUTTON,10,390,150,90,"Utilities\n& Editors");
    fl_set_object_boxtype(obj,FL_EMBOSSED_BOX);
    fl_set_object_lcolor(obj,FL_CHARTREUSE);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,utilitiesCB,0);
  fl_end_form();

  fdui->ODI->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_config *create_form_config(void)
{
  FL_OBJECT *obj;
  FD_config *fdui = (FD_config *) fl_calloc(1, sizeof(*fdui));

  fdui->config = fl_bgn_form(FL_NO_BOX, 940, 580);
  obj = fl_add_box(FL_UP_BOX,0,0,940,580,"");
  obj = fl_add_text(FL_NORMAL_TEXT,30,10,880,40,"Configuration Editor");
    fl_set_object_lcolor(obj,FL_RED);
    fl_set_object_lsize(obj,FL_HUGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_SHADOW_STYLE);
  obj = fl_add_frame(FL_ENGRAVED_FRAME,20,90,280,480,"");
  fdui->save_jtamv = obj = fl_add_button(FL_NORMAL_BUTTON,60,530,80,30,"Save");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,saveCB,1);
  obj = fl_add_frame(FL_ENGRAVED_FRAME,320,90,280,480,"");
  fdui->save_input = obj = fl_add_button(FL_NORMAL_BUTTON,360,530,80,30,"Save");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,saveCB,2);
  obj = fl_add_frame(FL_ENGRAVED_FRAME,620,90,280,480,"");
  fdui->save_graph = obj = fl_add_button(FL_NORMAL_BUTTON,660,530,80,30,"Save");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,saveCB,3);
  fdui->classification = obj = fl_add_input(FL_NORMAL_INPUT,40,120,230,30,"Classification");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,classCB,0);
  fdui->texfile = obj = fl_add_input(FL_NORMAL_INPUT,40,360,160,30,"Texture");
    fl_set_object_color(obj,FL_COL1,FL_TOP_BCOL);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,jtamvCB,21);
  fdui->hostid = obj = fl_add_input(FL_NORMAL_INPUT,40,460,160,30,"Host ID");
    fl_set_object_color(obj,FL_COL1,FL_TOP_BCOL);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,jtamvCB,22);
  fdui->portid = obj = fl_add_input(FL_INT_INPUT,40,490,160,30,"Port ID");
    fl_set_object_color(obj,FL_COL1,FL_TOP_BCOL);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,jtamvCB,23);
  fdui->source = obj = fl_add_choice(FL_DROPLIST_CHOICE,40,420,160,30,"Input Source");
    fl_set_object_boxtype(obj,FL_UP_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,sourceCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,20,60,280,30,"JTAMV Parameters");
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
  obj = fl_add_text(FL_NORMAL_TEXT,320,60,280,30,"Data Parameters");
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
  obj = fl_add_text(FL_NORMAL_TEXT,620,60,280,30,"Sim Parameters");
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
  fdui->vinfile = obj = fl_add_input(FL_NORMAL_INPUT,340,120,230,30,"Data File Name");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,infileCB,0);
  fdui->delay = obj = fl_add_input(FL_INT_INPUT,340,420,160,30,"Delay (sec)");
    fl_set_object_color(obj,FL_COL1,FL_TOP_BCOL);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,inputCB,20);
  obj = fl_add_button(FL_NORMAL_BUTTON,820,10,80,40,"Done");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,doneCB,0);
  fdui->autorun = obj = fl_add_checkbutton(FL_PUSH_BUTTON,40,160,110,20,"Auto Run");
    fl_set_object_callback(obj,jtamvCB,0);
  fdui->autoexit = obj = fl_add_checkbutton(FL_PUSH_BUTTON,160,160,110,20,"Auto exit");
    fl_set_object_callback(obj,jtamvCB,1);
  fdui->textured = obj = fl_add_checkbutton(FL_PUSH_BUTTON,40,180,110,20,"Textured Earth");
    fl_set_object_callback(obj,jtamvCB,2);
  fdui->coverage = obj = fl_add_checkbutton(FL_PUSH_BUTTON,160,180,110,20,"Sensor Coverage");
    fl_set_object_callback(obj,jtamvCB,3);
  fdui->gridlines = obj = fl_add_checkbutton(FL_PUSH_BUTTON,40,200,110,20,"Grid Lines");
    fl_set_object_callback(obj,jtamvCB,4);
  fdui->boundary = obj = fl_add_checkbutton(FL_PUSH_BUTTON,160,200,110,20,"Coastlines/Political");
    fl_set_object_callback(obj,jtamvCB,5);
  fdui->timestats = obj = fl_add_checkbutton(FL_PUSH_BUTTON,40,260,110,20,"Timing Statistics");
    fl_set_object_callback(obj,jtamvCB,6);
  fdui->simstats = obj = fl_add_checkbutton(FL_PUSH_BUTTON,160,260,110,20,"Sim Statistics");
    fl_set_object_callback(obj,jtamvCB,7);
  fdui->tracks = obj = fl_add_checkbutton(FL_PUSH_BUTTON,40,220,110,20,"Tracks");
    fl_set_object_callback(obj,jtamvCB,8);
  fdui->trails = obj = fl_add_checkbutton(FL_PUSH_BUTTON,160,220,110,20,"Trails");
    fl_set_object_callback(obj,jtamvCB,9);
  fdui->tracklabel = obj = fl_add_checkbutton(FL_PUSH_BUTTON,340,160,130,20,"Label Track");
    fl_set_object_callback(obj,inputCB,0);
  fdui->trackdrop = obj = fl_add_checkbutton(FL_PUSH_BUTTON,460,160,130,20,"Drop Track");
    fl_set_object_callback(obj,inputCB,1);
  fdui->r2links = obj = fl_add_checkbutton(FL_PUSH_BUTTON,340,180,130,20,"Show R2");
    fl_set_object_callback(obj,inputCB,2);
  fdui->keeplinks = obj = fl_add_checkbutton(FL_PUSH_BUTTON,460,180,130,20,"Keep Links");
    fl_set_object_callback(obj,inputCB,3);
  fdui->starttime = obj = fl_add_input(FL_FLOAT_INPUT,340,460,160,30,"Start Time");
    fl_set_object_color(obj,FL_COL1,FL_TOP_BCOL);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,timeCB,0);
  fdui->endtime = obj = fl_add_input(FL_FLOAT_INPUT,340,490,160,30,"End Time");
    fl_set_object_color(obj,FL_COL1,FL_TOP_BCOL);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,timeCB,1);
  fdui->saveas1 = obj = fl_add_button(FL_NORMAL_BUTTON,170,530,80,30,"Save As ...");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,saveasCB,1);
  fdui->saveas2 = obj = fl_add_button(FL_NORMAL_BUTTON,470,530,80,30,"Save As ...");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,saveasCB,2);
  fdui->saveas3 = obj = fl_add_button(FL_NORMAL_BUTTON,770,530,80,30,"Save As ...");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,saveasCB,3);
  fdui->p_endtime = obj = fl_add_input(FL_FLOAT_INPUT,640,490,160,30,"End Time");
    fl_set_object_color(obj,FL_COL1,FL_TOP_BCOL);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,simtimeCB,1);
  fdui->p_starttime = obj = fl_add_input(FL_FLOAT_INPUT,640,460,160,30,"Start Time");
    fl_set_object_color(obj,FL_COL1,FL_TOP_BCOL);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,simtimeCB,0);
  fdui->pinfile = obj = fl_add_input(FL_NORMAL_INPUT,640,120,230,30,"Data File Name");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,infileCB,1);
  fdui->siminfile = obj = fl_add_checkbutton(FL_PUSH_BUTTON,640,160,130,20,"Input File");
    fl_set_object_callback(obj,siminputCB,0);
  fdui->p_cycletime = obj = fl_add_input(FL_FLOAT_INPUT,640,410,160,30,"Cycle Time");
    fl_set_object_color(obj,FL_COL1,FL_TOP_BCOL);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,simtimeCB,3);
  fdui->p_synctime = obj = fl_add_input(FL_FLOAT_INPUT,640,380,160,30,"Sync Time");
    fl_set_object_color(obj,FL_COL1,FL_TOP_BCOL);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,simtimeCB,2);
  fdui->region_menu = obj = fl_add_menu(FL_PULLDOWN_MENU,40,320,160,30,"Region");
    fl_set_object_boxtype(obj,FL_UP_BOX);
    fl_set_object_callback(obj,regionCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,200,320,30,30,"Edit");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,regeditCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,220,325,20,20,"@->");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_callback(obj,regeditCB,0);
  fdui->impacts = obj = fl_add_checkbutton(FL_PUSH_BUTTON,40,240,110,20,"Impact Ellipses");
    fl_set_object_callback(obj,jtamvCB,10);
  obj = fl_add_checkbutton(FL_PUSH_BUTTON,160,240,110,20,"Enable Log");
    fl_set_object_callback(obj,jtamvCB,11);
  fl_end_form();

  fdui->config->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_regionedit *create_form_regionedit(void)
{
  FL_OBJECT *obj;
  FD_regionedit *fdui = (FD_regionedit *) fl_calloc(1, sizeof(*fdui));

  fdui->regionedit = fl_bgn_form(FL_NO_BOX, 380, 420);
  obj = fl_add_box(FL_UP_BOX,0,0,380,420,"");
  fdui->region_browser = obj = fl_add_browser(FL_HOLD_BROWSER,30,70,160,160,"Region Names");
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,regbrowserCB,0);
  fdui->region_name = obj = fl_add_input(FL_NORMAL_INPUT,30,260,160,30,"Selected Region");
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,regioneditCB,0);
  fdui->region_lat = obj = fl_add_input(FL_INT_INPUT,260,60,100,30,"Latitude");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,regioneditCB,1);
  fdui->region_lon = obj = fl_add_input(FL_INT_INPUT,260,110,100,30,"Longitude");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,regioneditCB,2);
  fdui->region_alt = obj = fl_add_input(FL_INT_INPUT,260,160,100,30,"Altitude");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,regioneditCB,3);
  fdui->region_fov = obj = fl_add_input(FL_INT_INPUT,260,210,100,30,"FOV");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,regioneditCB,4);
  fdui->region_azi = obj = fl_add_input(FL_INT_INPUT,260,260,100,30,"Azimuth");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,regioneditCB,5);
  obj = fl_add_text(FL_NORMAL_TEXT,40,20,300,30,"Region Editor");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE+FL_SHADOW_STYLE);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,80,370,90,30,"Save");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,regioneditCB,10);
  obj = fl_add_button(FL_NORMAL_BUTTON,220,370,90,30,"Cancel");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,regioneditCB,11);

  fdui->regedgrp = fl_bgn_group();
  fdui->region_norm = obj = fl_add_checkbutton(FL_RADIO_BUTTON,60,310,110,20,"Normal View");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,regioneditCB,6);
    fl_set_button(obj, 1);
  fdui->region_tang = obj = fl_add_checkbutton(FL_RADIO_BUTTON,210,310,110,20,"Tangential View");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,regioneditCB,7);
  fl_end_group();

  fl_end_form();

  fdui->regionedit->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

