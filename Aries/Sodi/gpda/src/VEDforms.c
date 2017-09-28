/* Form definition file generated with fdesign. */

#include "forms.h"
#include <stdlib.h>
#include "VEDforms.h"

static FL_PUP_ENTRY fdchoice_source_0[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "File",	0,	"",	 FL_PUP_NONE},
    { "Socket",	0,	"",	 FL_PUP_NONE},
    { "Speedes",	0,	"",	 FL_PUP_NONE},
    {0}
};

static FL_PUP_ENTRY fdchoice_region_choice_1[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "choice 1",	0,	"",	 FL_PUP_NONE},
    { "choice 2",	0,	"",	 FL_PUP_NONE},
    { "choice 3",	0,	"",	 FL_PUP_NONE},
    { "choice 4",	0,	"",	 FL_PUP_NONE},
    {0}
};

FD_config *create_form_config(void)
{
  FL_OBJECT *obj;
  FD_config *fdui = (FD_config *) fl_calloc(1, sizeof(*fdui));

  fdui->config = fl_bgn_form(FL_NO_BOX, 720, 630);
  obj = fl_add_box(FL_UP_BOX,0,0,720,630,"");
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,20,90,290,530,"Display Parameters");
    fl_set_object_lcolor(obj,FL_WHEAT);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,ODInoneCB,0);
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,320,380,290,240,"Simulation Parameters");
    fl_set_object_lcolor(obj,FL_WHEAT);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,ODInoneCB,0);
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,320,90,290,270,"File/Socket Parameters");
    fl_set_object_lcolor(obj,FL_WHEAT);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,ODInoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,30,10,660,40,"Parameter File Editor");
    fl_set_object_lcolor(obj,FL_RED);
    fl_set_object_lsize(obj,FL_HUGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_SHADOW_STYLE);
  fdui->save_jtamv = obj = fl_add_button(FL_NORMAL_BUTTON,620,530,80,40,"Save");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,saveCB,1);
  fdui->classification = obj = fl_add_input(FL_NORMAL_INPUT,40,120,250,30,"Classification");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,classCB,0);
  fdui->texfile = obj = fl_add_input(FL_NORMAL_INPUT,40,580,250,30,"Texture File Name");
    fl_set_object_color(obj,FL_COL1,FL_TOP_BCOL);
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,jtamvCB,21);
  fdui->hostid = obj = fl_add_input(FL_NORMAL_INPUT,340,280,160,30,"Host ID");
    fl_set_object_color(obj,FL_COL1,FL_TOP_BCOL);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,jtamvCB,22);
  fdui->portid = obj = fl_add_input(FL_INT_INPUT,340,310,160,30,"Port ID");
    fl_set_object_color(obj,FL_COL1,FL_TOP_BCOL);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,jtamvCB,23);
  fdui->source = obj = fl_add_choice(FL_NORMAL_CHOICE2,40,420,160,30,"Input Source");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,sourceCB,0);
    fl_set_choice_entries(obj, fdchoice_source_0);
    fl_set_choice(obj,1);
  fdui->vinfile = obj = fl_add_input(FL_NORMAL_INPUT,340,120,220,30,"Input File Name");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,infileCB,0);
  fdui->delay = obj = fl_add_input(FL_INT_INPUT,340,230,160,30,"Delay (sec)");
    fl_set_object_color(obj,FL_COL1,FL_TOP_BCOL);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,inputCB,20);
  obj = fl_add_button(FL_NORMAL_BUTTON,620,580,80,40,"Done");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
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
  fdui->timestats = obj = fl_add_checkbutton(FL_PUSH_BUTTON,40,350,110,20,"Timing Statistics");
    fl_set_object_callback(obj,jtamvCB,6);
  fdui->simstats = obj = fl_add_checkbutton(FL_PUSH_BUTTON,150,350,110,20,"Sim Statistics");
    fl_set_object_callback(obj,jtamvCB,7);
  fdui->tracks = obj = fl_add_checkbutton(FL_PUSH_BUTTON,40,240,110,20,"Tracks");
    fl_set_object_callback(obj,jtamvCB,8);
  fdui->trails = obj = fl_add_checkbutton(FL_PUSH_BUTTON,160,240,110,20,"Trails");
    fl_set_object_callback(obj,jtamvCB,9);
  fdui->tracklabel = obj = fl_add_checkbutton(FL_PUSH_BUTTON,40,280,130,20,"Label Track");
    fl_set_object_callback(obj,inputCB,0);
  fdui->trackdrop = obj = fl_add_checkbutton(FL_PUSH_BUTTON,160,280,130,20,"Drop Track");
    fl_set_object_callback(obj,inputCB,1);
  fdui->r2links = obj = fl_add_checkbutton(FL_PUSH_BUTTON,40,300,130,20,"Show R2");
    fl_set_object_callback(obj,inputCB,2);
  fdui->keeplinks = obj = fl_add_checkbutton(FL_PUSH_BUTTON,160,300,130,20,"Keep Links");
    fl_set_object_callback(obj,inputCB,3);
  fdui->saveas1 = obj = fl_add_button(FL_NORMAL_BUTTON,620,480,80,40,"Save As ...");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,saveasCB,1);
  fdui->starttime = obj = fl_add_input(FL_FLOAT_INPUT,340,550,160,30,"Start Time");
    fl_set_object_color(obj,FL_COL1,FL_TOP_BCOL);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,simtimeCB,0);
  fdui->pinfile = obj = fl_add_input(FL_NORMAL_INPUT,340,410,220,30,"Playback File Name");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,infileCB,1);
  fdui->siminfile = obj = fl_add_checkbutton(FL_PUSH_BUTTON,340,450,130,20,"Input File");
    fl_set_object_callback(obj,siminputCB,0);
  fdui->cycletime = obj = fl_add_input(FL_FLOAT_INPUT,340,520,160,30,"Cycle Time");
    fl_set_object_color(obj,FL_COL1,FL_TOP_BCOL);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,simtimeCB,3);
  fdui->synctime = obj = fl_add_input(FL_FLOAT_INPUT,340,490,160,30,"Sync Time");
    fl_set_object_color(obj,FL_COL1,FL_TOP_BCOL);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,simtimeCB,2);
  obj = fl_add_button(FL_NORMAL_BUTTON,200,470,30,30,"Edit");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,regeditCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,220,475,20,20,"@->");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_callback(obj,regeditCB,0);
  fdui->impacts = obj = fl_add_checkbutton(FL_PUSH_BUTTON,40,260,110,20,"Impact Ellipses");
    fl_set_object_callback(obj,jtamvCB,10);
  obj = fl_add_checkbutton(FL_PUSH_BUTTON,40,330,110,20,"Enable Log");
    fl_set_object_callback(obj,jtamvCB,11);
  fdui->region_choice = obj = fl_add_choice(FL_NORMAL_CHOICE2,40,470,160,30,"Viewing Region");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,regionCB,0);
    fl_set_choice_entries(obj, fdchoice_region_choice_1);
    fl_set_choice(obj,2);
  fdui->endtime = obj = fl_add_input(FL_FLOAT_INPUT,340,580,160,30,"End Time");
    fl_set_object_color(obj,FL_COL1,FL_TOP_BCOL);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,simtimeCB,1);
  fdui->folder_icon = obj = fl_add_pixmapbutton(FL_NORMAL_BUTTON,570,120,30,30,"");
    fl_set_object_callback(obj,odifilesCB,0);
    fl_set_pixmapbutton_file(obj, "../BitMaps/folder.xpm");
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

