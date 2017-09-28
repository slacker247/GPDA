/* Form definition file generated with fdesign. */

#include "forms.h"
#include <stdlib.h>
#include "TLEforms.h"

static FL_PUP_ENTRY fdmenu_filter_menu_0[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "Critical",	0,	"",	 FL_PUP_RADIO},
    { "High",	0,	"",	 FL_PUP_RADIO},
    { "Medium",	0,	"",	 FL_PUP_RADIO},
    { "Low",	0,	"",	 FL_PUP_RADIO},
    {0}
};

static FL_PUP_ENTRY fdmenu_unit_menu_1[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "Seconds",	0,	"",	 FL_PUP_RADIO},
    { "Minutes",	0,	"",	 FL_PUP_RADIO},
    { "Hours",	0,	"",	 FL_PUP_RADIO},
    { "Days",	0,	"",	 FL_PUP_RADIO},
    { "Months",	0,	"",	 FL_PUP_NONE},
    {0}
};

static FL_PUP_ENTRY fdmenu_scale_menu_2[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "Linear",	0,	"",	 FL_PUP_RADIO},
    { "Log",	0,	"",	 FL_PUP_RADIO},
    {0}
};

static FL_PUP_ENTRY fdmenu_source_menu_3[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "Real Time",	0,	"",	 FL_PUP_RADIO},
    { "Sim. Time",	0,	"",	 FL_PUP_RADIO},
    {0}
};

FD_tlegraph *create_form_tlegraph(void)
{
  FL_OBJECT *obj;
  FD_tlegraph *fdui = (FD_tlegraph *) fl_calloc(1, sizeof(*fdui));

  fdui->tlegraph = fl_bgn_form(FL_NO_BOX, 850, 460);
  obj = fl_add_box(FL_UP_BOX,0,0,850,460,"");
    fl_set_object_lalign(obj,FL_ALIGN_BOTTOM);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,570,50,100,110,"Graph Options");
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_button(FL_NORMAL_BUTTON,760,10,80,30,"Exit");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TLEexitCB,0);
  fdui->canvas = obj = fl_add_glcanvas(FL_NORMAL_CANVAS,10,170,800,260,"");
    fl_set_object_color(obj,FL_WHITE,FL_BLACK);
    fl_set_object_callback(obj,TLEnoneCB,0);
  fdui->time_scroll = obj = fl_add_scrollbar(FL_HOR_NICE_SCROLLBAR,10,435,800,20,"");
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_callback(obj,timescrollCB,0);
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,680,70,160,90,"Next Event");
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TLEnoneCB,0);
  fdui->next_event = obj = fl_add_text(FL_NORMAL_TEXT,685,80,150,20,"event");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TLEnoneCB,0);
  fdui->event_timer = obj = fl_add_timer(FL_VALUE_TIMER,715,105,90,20,"");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_callback(obj,TLEnoneCB,0);
  fdui->real_time = obj = fl_add_clock(FL_DIGITAL_CLOCK,570,20,80,20,"Real Time");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_color(obj,FL_WHITE,FL_BOTTOM_BCOL);
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lalign(obj,FL_ALIGN_TOP);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TLEnoneCB,0);
  fdui->event_list = obj = fl_add_browser(FL_SELECT_BROWSER,10,10,330,150,"");
    fl_set_object_color(obj,FL_LEFT_BCOL,FL_YELLOW);
    fl_set_object_callback(obj,eventlistCB,0);
  fdui->tle_accept = obj = fl_add_button(FL_NORMAL_BUTTON,730,130,20,20,"@-21+");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_callback(obj,acceptCB,1);
  fdui->tle_auto = obj = fl_add_button(FL_NORMAL_BUTTON,790,130,20,20,"@-2circle");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_callback(obj,acceptCB,0);
  fdui->interval = obj = fl_add_thumbwheel(FL_VERT_THUMBWHEEL,820,165,20,270,"30");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,intervalCB,0);
  fdui->filter_menu = obj = fl_add_menu(FL_PULLDOWN_MENU,580,60,80,20,"Alarm Filter");
    fl_set_object_boxtype(obj,FL_UP_BOX);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_callback(obj,filterCB,0);
    fl_set_menu_entries(obj, fdmenu_filter_menu_0);
  fdui->unit_menu = obj = fl_add_menu(FL_PULLDOWN_MENU,580,85,80,20,"Time Units");
    fl_set_object_boxtype(obj,FL_UP_BOX);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_callback(obj,timeunitCB,0);
    fl_set_menu_entries(obj, fdmenu_unit_menu_1);
  fdui->scale_menu = obj = fl_add_menu(FL_PULLDOWN_MENU,580,110,80,20,"Time Scale");
    fl_set_object_boxtype(obj,FL_UP_BOX);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_callback(obj,timescaleCB,0);
    fl_set_menu_entries(obj, fdmenu_scale_menu_2);
  fdui->source_menu = obj = fl_add_menu(FL_PULLDOWN_MENU,580,135,80,20,"Time Source");
    fl_set_object_boxtype(obj,FL_UP_BOX);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_callback(obj,timesourceCB,0);
    fl_set_menu_entries(obj, fdmenu_source_menu_3);
  obj = fl_add_text(FL_NORMAL_TEXT,750,130,40,20,"Auto");
    fl_set_object_lsize(obj,FL_TINY_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TLEnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,690,130,40,20,"Alarms");
    fl_set_object_lsize(obj,FL_TINY_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TLEnoneCB,0);
  fdui->sim_time = obj = fl_add_clock(FL_DIGITAL_CLOCK,660,20,80,20,"Sim Time");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_color(obj,FL_WHITE,FL_DEEPPINK);
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lalign(obj,FL_ALIGN_TOP);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TLEnoneCB,0);
  fdui->tle_explain = obj = fl_add_pixmap(FL_NORMAL_PIXMAP,345,10,220,150,"");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_callback(obj,TLEnoneCB,0);
  fl_end_form();

  fdui->tlegraph->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

static FL_PUP_ENTRY fdchoice_tle_type_4[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "Alarm",	0,	"",	 FL_PUP_NONE},
    { "Update",	0,	"",	 FL_PUP_NONE},
    { "Comm",	0,	"",	 FL_PUP_NONE},
    { "Sim",	0,	"",	 FL_PUP_NONE},
    { "Replan",	0,	"",	 FL_PUP_NONE},
    {0}
};

static FL_PUP_ENTRY fdchoice_evt_tunit_5[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "Seconds",	0,	"",	 FL_PUP_NONE},
    { "Minutes",	0,	"",	 FL_PUP_NONE},
    { "Hours",	0,	"",	 FL_PUP_NONE},
    { "Days",	0,	"",	 FL_PUP_NONE},
    { "Months",	0,	"",	 FL_PUP_NONE},
    {0}
};

static FL_PUP_ENTRY fdchoice_tle_priority_6[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "Low",	0,	"",	 FL_PUP_NONE},
    { "Medium",	0,	"",	 FL_PUP_NONE},
    { "High",	0,	"",	 FL_PUP_NONE},
    { "Critical",	0,	"",	 FL_PUP_NONE},
    {0}
};

static FL_PUP_ENTRY fdchoice_tle_repeat_7[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "One-time",	0,	"",	 FL_PUP_NONE},
    { "Periodic",	0,	"",	 FL_PUP_NONE},
    {0}
};

FD_tleevent *create_form_tleevent(void)
{
  FL_OBJECT *obj;
  FD_tleevent *fdui = (FD_tleevent *) fl_calloc(1, sizeof(*fdui));

  fdui->tleevent = fl_bgn_form(FL_NO_BOX, 340, 410);
  obj = fl_add_box(FL_UP_BOX,0,0,340,410,"");
  obj = fl_add_text(FL_NORMAL_TEXT,10,10,320,30,"Event Information");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TLEnoneCB,0);
  fdui->evt_desc = obj = fl_add_input(FL_NORMAL_INPUT,80,50,250,20,"Description");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TLEnoneCB,0);
  fdui->evt_time = obj = fl_add_input(FL_FLOAT_INPUT,80,80,140,20,"Time");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TLEnoneCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,150,370,50,30,"Done");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,evtexitCB,0);
  fdui->evt_remain = obj = fl_add_input(FL_NORMAL_INPUT,80,310,140,20,"T. Remaining");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TLEnoneCB,0);
  fdui->evt_runit = obj = fl_add_input(FL_NORMAL_INPUT,230,310,100,20,"");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TLEnoneCB,0);
  fdui->tle_type = obj = fl_add_choice(FL_NORMAL_CHOICE2,80,110,110,20,"Type");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TLEnoneCB,0);
    fl_set_choice_entries(obj, fdchoice_tle_type_4);
    fl_set_choice(obj,1);
  fdui->evt_tunit = obj = fl_add_choice(FL_NORMAL_CHOICE2,230,80,100,20,"");
    fl_set_object_callback(obj,TLEnoneCB,0);
    fl_set_choice_entries(obj, fdchoice_evt_tunit_5);
    fl_set_choice(obj,1);
  fdui->tle_priority = obj = fl_add_choice(FL_NORMAL_CHOICE2,80,200,100,20,"Priority");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TLEnoneCB,0);
    fl_set_choice_entries(obj, fdchoice_tle_priority_6);
    fl_set_choice(obj,1);
  fdui->tle_value = obj = fl_add_input(FL_NORMAL_INPUT,80,140,250,20,"Value");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TLEnoneCB,0);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,10,295,320,10,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_callback(obj,TLEnoneCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,90,260,60,30,"Add");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,modlistCB,0);
  fdui->tle_repeat = obj = fl_add_choice(FL_NORMAL_CHOICE2,80,170,100,20,"Repeat");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TLEnoneCB,0);
    fl_set_choice_entries(obj, fdchoice_tle_repeat_7);
    fl_set_choice(obj,1);
  fdui->tle_period = obj = fl_add_input(FL_INT_INPUT,230,170,100,20,"Period");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TLEnoneCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,200,260,60,30,"Remove");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,modlistCB,1);
  fdui->evt_file = obj = fl_add_input(FL_NORMAL_INPUT,80,230,250,20,"Data File");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TLEnoneCB,0);
  fdui->evt_id = obj = fl_add_input(FL_NORMAL_INPUT,80,340,140,20,"Event ID");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TLEnoneCB,0);
  fdui->evt_subtype = obj = fl_add_input(FL_NORMAL_INPUT,200,110,130,20,"");
    fl_set_object_callback(obj,TLEnoneCB,0);
  fl_end_form();

  fdui->tleevent->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_tlereplan *create_form_tlereplan(void)
{
  FL_OBJECT *obj;
  FD_tlereplan *fdui = (FD_tlereplan *) fl_calloc(1, sizeof(*fdui));

  fdui->tlereplan = fl_bgn_form(FL_NO_BOX, 850, 460);
  obj = fl_add_box(FL_UP_BOX,0,0,850,460,"");
  obj = fl_add_button(FL_NORMAL_BUTTON,760,10,80,30,"Dismiss");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,tle_planexitCB,0);
  fdui->replan_graph = obj = fl_add_xyplot(FL_NORMAL_XYPLOT,20,180,710,260,"");
    fl_set_object_lsize(obj,FL_DEFAULT_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TLEnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,20,10,710,40,"Replan Trigger Explanation");
    fl_set_object_color(obj,FL_WHEAT,FL_MCOL);
    fl_set_object_lsize(obj,FL_HUGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_TIMESBOLDITALIC_STYLE);
    fl_set_object_callback(obj,TLEnoneCB,0);
  fdui->explain_text = obj = fl_add_text(FL_NORMAL_TEXT,20,60,440,120,"");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_FIXEDBOLD_STYLE);
    fl_set_object_callback(obj,TLEnoneCB,0);
  fdui->parms_text = obj = fl_add_text(FL_NORMAL_TEXT,470,60,260,120,"");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_FIXEDBOLD_STYLE);
    fl_set_object_callback(obj,TLEnoneCB,0);
  fl_end_form();

  fdui->tlereplan->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

