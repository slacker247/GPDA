/* Form definition file generated with fdesign. */

#include "forms.h"
#include <stdlib.h>
#include "TGTforms.h"

static FL_PUP_ENTRY fdchoice_country_menu_0[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "Russia",	0,	"",	 FL_PUP_NONE},
    { "China",	0,	"",	 FL_PUP_NONE},
    { "DPRK",	0,	"",	 FL_PUP_NONE},
    { "Iran",	0,	"",	 FL_PUP_NONE},
    { "Iraq",	0,	"",	 FL_PUP_NONE},
    { "Lybia",	0,	"",	 FL_PUP_NONE},
    { "India",	0,	"",	 FL_PUP_NONE},
    { "Pakistan",	0,	"",	 FL_PUP_NONE},
    {0}
};

static FL_PUP_ENTRY fdchoice_tgt_mobility_1[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "Fixed",	0,	"",	 FL_PUP_NONE},
    { "Mobile",	0,	"",	 FL_PUP_NONE},
    {0}
};

static FL_PUP_ENTRY fdchoice_tgt_value_2[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "Low",	0,	"",	 FL_PUP_NONE},
    { "Medium",	0,	"",	 FL_PUP_NONE},
    { "High",	0,	"",	 FL_PUP_NONE},
    { "Critical",	0,	"",	 FL_PUP_NONE},
    {0}
};

static FL_PUP_ENTRY fdchoice_class_menu_3[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "Mobile Target",	0,	"",	 FL_PUP_NONE},
    { "Missile Base",	0,	"",	 FL_PUP_NONE},
    { "Air Base",	0,	"",	 FL_PUP_NONE},
    { "Sub Base",	0,	"",	 FL_PUP_NONE},
    { "Nuclear Facility",	0,	"",	 FL_PUP_NONE},
    { "Chemical Facility",	0,	"",	 FL_PUP_NONE},
    { "Biological Facility",	0,	"",	 FL_PUP_NONE},
    { "C&C Facility",	0,	"",	 FL_PUP_NONE},
    { "Population Center",	0,	"",	 FL_PUP_NONE},
    { "Time Critical Target",	0,	"",	 FL_PUP_NONE},
    { "Other",	0,	"",	 FL_PUP_NONE},
    { "All",	0,	"",	 FL_PUP_NONE},
    {0}
};

static FL_PUP_ENTRY fdchoice_tgt_filter_4[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "By Locale",	0,	"",	 FL_PUP_NONE},
    { "By Class",	0,	"",	 FL_PUP_NONE},
    { "By Locale & Class",	0,	"",	 FL_PUP_NONE},
    { "By Ready Status",	0,	"",	 FL_PUP_NONE},
    {0}
};

static FL_PUP_ENTRY fdmenu_File_5[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "Exit",	0,	"",	 FL_PUP_NONE},
    {0}
};

FD_targetstatus *create_form_targetstatus(void)
{
  FL_OBJECT *obj;
  FD_targetstatus *fdui = (FD_targetstatus *) fl_calloc(1, sizeof(*fdui));

  fdui->targetstatus = fl_bgn_form(FL_NO_BOX, 850, 460);
  fdui->tgt_hardthumb = obj = fl_add_box(FL_UP_BOX,0,0,850,460,"");
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,580,60,120,205,"Arrival DE Factors");
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,715,60,120,205,"Damage DE Factors");
    fl_set_object_lcolor(obj,FL_RED);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,495,50,10,400,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_callback(obj,TGTnoneCB,0);
  obj = fl_add_lightbutton(FL_PUSH_BUTTON,510,80,60,20,"> 90");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_GREEN,FL_COL1);
    fl_set_object_lsize(obj,FL_TINY_SIZE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  obj = fl_add_lightbutton(FL_PUSH_BUTTON,510,105,60,15,"90 - 70");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_YELLOW,FL_COL1);
    fl_set_object_lsize(obj,FL_TINY_SIZE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  obj = fl_add_lightbutton(FL_PUSH_BUTTON,510,125,60,15,"< 70");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_RED,FL_COL1);
    fl_set_object_lsize(obj,FL_TINY_SIZE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  fdui->site_browser = obj = fl_add_browser(FL_HOLD_BROWSER,325,50,160,200,"Locales");
    fl_set_object_color(obj,FL_COL1,FL_COL1);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,localebrowserCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,565,30,280,20,"Targeted Site Status");
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  fdui->damage[0] = obj = fl_add_lightbutton(FL_PUSH_BUTTON,720,70,100,20,"Vunerability");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_RED,FL_COL1);
    fl_set_object_callback(obj,damageCB,0);
  fdui->damage[1] = obj = fl_add_lightbutton(FL_PUSH_BUTTON,720,90,100,20,"Accuracy");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_GREEN,FL_COL1);
    fl_set_object_callback(obj,damageCB,1);
  fdui->arrival[12] = obj = fl_add_lightbutton(FL_PUSH_BUTTON,600,190,90,20,"Civilian");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_GREEN,FL_COL1);
    fl_set_object_callback(obj,arrivalCB,12);
  fdui->arrival[11] = obj = fl_add_lightbutton(FL_PUSH_BUTTON,600,170,90,20,"C2");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_GREEN,FL_COL1);
    fl_set_object_callback(obj,arrivalCB,11);
  fdui->arrival[10] = obj = fl_add_lightbutton(FL_PUSH_BUTTON,600,150,90,20,"Importance");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_RED,FL_COL1);
    fl_set_object_callback(obj,arrivalCB,10);
  fdui->country_menu = obj = fl_add_choice(FL_NORMAL_CHOICE2,60,55,110,25,"Targeted Country");
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,targetselectCB,0);
    fl_set_choice_entries(obj, fdchoice_country_menu_0);
    fl_set_choice(obj,4);
  fdui->damage[2] = obj = fl_add_lightbutton(FL_PUSH_BUTTON,720,110,100,20,"Yield");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_GREEN,FL_COL1);
    fl_set_object_callback(obj,damageCB,2);
  fdui->damage[4] = obj = fl_add_lightbutton(FL_PUSH_BUTTON,720,130,100,20,"Constraints");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_YELLOW,FL_COL1);
    fl_set_object_callback(obj,damageCB,4);
  fdui->damage[10] = obj = fl_add_lightbutton(FL_PUSH_BUTTON,745,150,80,20,"Collateral");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_GREEN,FL_COL1);
    fl_set_object_callback(obj,damageCB,10);
  fdui->damage[11] = obj = fl_add_lightbutton(FL_PUSH_BUTTON,745,170,80,20,"Fraticide");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_YELLOW,FL_COL1);
    fl_set_object_callback(obj,damageCB,11);
  fdui->arrival[4] = obj = fl_add_lightbutton(FL_PUSH_BUTTON,585,130,100,20,"Constraints");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_GREEN,FL_COL1);
    fl_set_object_callback(obj,arrivalCB,4);
  fdui->arrival[0] = obj = fl_add_lightbutton(FL_PUSH_BUTTON,585,70,100,20,"Pre-launch");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_GREEN,FL_COL1);
    fl_set_object_callback(obj,arrivalCB,0);
  fdui->arrival[1] = obj = fl_add_lightbutton(FL_PUSH_BUTTON,585,90,100,20,"In-Flight");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_GREEN,FL_COL1);
    fl_set_object_callback(obj,arrivalCB,1);
  fdui->arrival[2] = obj = fl_add_lightbutton(FL_PUSH_BUTTON,585,110,100,20,"Defense");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_YELLOW,FL_COL1);
    fl_set_object_callback(obj,arrivalCB,2);
  fdui->arrival[13] = obj = fl_add_lightbutton(FL_PUSH_BUTTON,600,210,80,20,"Weather");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_YELLOW,FL_COL1);
    fl_set_object_callback(obj,arrivalCB,13);
  obj = fl_add_text(FL_NORMAL_TEXT,720,235,110,20,"P           =");
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,735,240,50,20,"damage");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,585,235,70,20,"P           =");
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,600,240,50,20,"arrival");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  fdui->damage_p = obj = fl_add_text(FL_NORMAL_TEXT,790,230,40,30,"0.94");
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  fdui->arrival_p = obj = fl_add_text(FL_NORMAL_TEXT,655,230,40,30,"0.97");
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  fdui->tgt_locale = obj = fl_add_text(FL_NORMAL_TEXT,170,105,130,20,"");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,60,105,110,20,"Locale of Target");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,60,270,110,20,"Target Position");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  fdui->tgt_country = obj = fl_add_text(FL_NORMAL_TEXT,170,125,130,20,"");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  fdui->tgt_hardness = obj = fl_add_text(FL_NORMAL_TEXT,170,325,70,20,"");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,60,345,110,20,"Target Mobility");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,60,365,110,20,"Target Value");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,60,325,110,20,"Target Hardness");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  fdui->tgt_sethard = obj = fl_add_thumbwheel(FL_HOR_THUMBWHEEL,240,325,60,20,"");
    fl_set_object_callback(obj,tgthardCB,0);
  fdui->tgt_active = obj = fl_add_browser(FL_HOLD_BROWSER,580,290,255,155,"Currently Active Targets");
    fl_set_object_lcolor(obj,FL_RED);
    fl_set_object_lalign(obj,FL_ALIGN_TOP);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  fdui->tgt_delete = obj = fl_add_button(FL_NORMAL_BUTTON,510,365,60,20,"Delete");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,tgtactiveCB,0);
  fdui->tgt_add = obj = fl_add_button(FL_NORMAL_BUTTON,510,340,60,20,"Add");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,tgtactiveCB,1);
  fdui->tgt_save = obj = fl_add_button(FL_NORMAL_BUTTON,510,315,60,20,"Save");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,tgtactiveCB,2);
  fdui->tgt_mobility = obj = fl_add_choice(FL_NORMAL_CHOICE2,170,345,130,20,"");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_callback(obj,TGTnoneCB,0);
    fl_set_choice_entries(obj, fdchoice_tgt_mobility_1);
    fl_set_choice(obj,1);
  obj = fl_add_text(FL_NORMAL_TEXT,60,195,110,20,"Target Type");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  fdui->tgt_type = obj = fl_add_text(FL_NORMAL_TEXT,170,195,130,20,"");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  fdui->tgt_value = obj = fl_add_choice(FL_NORMAL_CHOICE2,170,365,130,20,"");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_callback(obj,TGTnoneCB,0);
    fl_set_choice_entries(obj, fdchoice_tgt_value_2);
    fl_set_choice(obj,1);
  obj = fl_add_text(FL_NORMAL_TEXT,60,175,110,20,"Target Name");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  fdui->tgt_pixmap[1] = obj = fl_add_pixmapbutton(FL_NORMAL_BUTTON,10,85,40,40,"");
    fl_set_object_callback(obj,tgttypeCB,1);
  fdui->tgt_pixmap[2] = obj = fl_add_pixmapbutton(FL_NORMAL_BUTTON,10,125,40,40,"");
    fl_set_object_callback(obj,tgttypeCB,2);
  fdui->tgt_pixmap[3] = obj = fl_add_pixmapbutton(FL_NORMAL_BUTTON,10,165,40,40,"");
    fl_set_object_callback(obj,tgttypeCB,3);
  fdui->tgt_pixmap[4] = obj = fl_add_pixmapbutton(FL_NORMAL_BUTTON,10,205,40,40,"");
    fl_set_object_callback(obj,tgttypeCB,4);
  fdui->tgt_pixmap[5] = obj = fl_add_pixmapbutton(FL_NORMAL_BUTTON,10,245,40,40,"");
    fl_set_object_callback(obj,tgttypeCB,5);
  fdui->tgt_name = obj = fl_add_input(FL_NORMAL_INPUT,170,175,130,20,"");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_callback(obj,TGTnoneCB,0);
  fdui->tgt_load = obj = fl_add_button(FL_NORMAL_BUTTON,510,290,60,20,"Load");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,tgtactiveCB,3);
  fdui->class_menu = obj = fl_add_choice(FL_NORMAL_CHOICE2,190,55,110,25,"Target Class");
    fl_set_object_color(obj,FL_COL1,FL_RED);
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,targetselectCB,1);
    fl_set_choice_entries(obj, fdchoice_class_menu_3);
    fl_set_choice(obj,2);
  fdui->target_browser = obj = fl_add_browser(FL_HOLD_BROWSER,325,310,160,140,"Targets");
    fl_set_object_color(obj,FL_COL1,FL_COL1);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,targetbrowserCB,1);
  fdui->tgt_pixmap[0] = obj = fl_add_pixmapbutton(FL_NORMAL_BUTTON,10,45,40,40,"");
    fl_set_object_callback(obj,tgttypeCB,0);
  fdui->tgt_pixmap[6] = obj = fl_add_pixmapbutton(FL_NORMAL_BUTTON,10,285,40,40,"");
    fl_set_object_callback(obj,tgttypeCB,6);
  fdui->tgt_pixmap[7] = obj = fl_add_pixmapbutton(FL_NORMAL_BUTTON,10,325,40,40,"");
    fl_set_object_callback(obj,tgttypeCB,7);
  fdui->tgt_pixmap[8] = obj = fl_add_pixmapbutton(FL_NORMAL_BUTTON,10,365,40,40,"");
    fl_set_object_callback(obj,tgttypeCB,8);
  fdui->tgt_pixmap[9] = obj = fl_add_pixmapbutton(FL_NORMAL_BUTTON,10,405,40,40,"");
    fl_set_object_callback(obj,tgttypeCB,9);
  fdui->tgt_filter = obj = fl_add_choice(FL_NORMAL_CHOICE2,325,270,165,20,"Filter Targets");
    fl_set_object_lcolor(obj,FL_TOP_BCOL);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TGTnoneCB,0);
    fl_set_choice_entries(obj, fdchoice_tgt_filter_4);
    fl_set_choice(obj,3);
  fdui->tgt_latitude = obj = fl_add_input(FL_NORMAL_INPUT,170,250,110,20,"Lat");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT);
    fl_set_object_callback(obj,TGTnoneCB,0);
  fdui->tgt_longitude = obj = fl_add_input(FL_NORMAL_INPUT,170,270,110,20,"Lon");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT);
    fl_set_object_callback(obj,TGTnoneCB,0);
  obj = fl_add_menu(FL_PULLDOWN_MENU,5,5,50,20,"File");
    fl_set_object_callback(obj,TGTexitCB,0);
    fl_set_menu_entries(obj, fdmenu_File_5);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,5,25,835,10,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
  fdui->tgt_altitude = obj = fl_add_input(FL_NORMAL_INPUT,170,290,110,20,"Alt");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT);
    fl_set_object_callback(obj,TGTnoneCB,0);
  fdui->tgt_count = obj = fl_add_input(FL_NORMAL_INPUT,510,425,60,20,"# Targets");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_lalign(obj,FL_ALIGN_TOP);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  fdui->tgt_map = obj = fl_add_button(FL_NORMAL_BUTTON,170,145,130,20,"Not Available");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,targetimageCB,1);
  obj = fl_add_text(FL_NORMAL_TEXT,60,125,110,20,"Locale Country");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,60,145,110,20,"Locale Map");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,60,215,110,20,"Target Imagery");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TGTnoneCB,0);
  fdui->tgt_imagery = obj = fl_add_button(FL_NORMAL_BUTTON,170,215,130,20,"Not Available");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,targetimageCB,2);
  fl_end_form();

  fdui->targetstatus->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_tgtimage *create_form_tgtimage(void)
{
  FL_OBJECT *obj;
  FD_tgtimage *fdui = (FD_tgtimage *) fl_calloc(1, sizeof(*fdui));

  fdui->tgtimage = fl_bgn_form(FL_NO_BOX, 706, 631);
  obj = fl_add_box(FL_UP_BOX,0,0,706,631,"");
  fdui->tgt_image = obj = fl_add_canvas(FL_SCROLLED_CANVAS,10,10,685,575,"");
    fl_set_object_callback(obj,TGTnoneCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,320,595,85,25,"Done");
    fl_set_object_callback(obj,targetimageCB,0);
  fl_end_form();

  fdui->tgtimage->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

