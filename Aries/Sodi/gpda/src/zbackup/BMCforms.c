/* Form definition file generated with fdesign. */

#include "forms.h"
#include <stdlib.h>
#include "BMCforms.h"

static FL_PUP_ENTRY fdchoice_bmc_choice_0[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "NMD System",	0,	"",	 FL_PUP_NONE},
    { "Firing Unit",	0,	"",	 FL_PUP_NONE},
    { "ITW/AA",	0,	"",	 FL_PUP_NONE},
    { "Combined NMD",	0,	"",	 FL_PUP_NONE},
    { "STRATCOM",	0,	"",	 FL_PUP_NONE},
    { "Air Operations",	0,	"",	 FL_PUP_NONE},
    { "IO Warfare",	0,	"",	 FL_PUP_NONE},
    { "Homeland Defense",	0,	"",	 FL_PUP_NONE},
    {0}
};

static FL_PUP_ENTRY fdchoice_node_center_1[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "NORAD CCC",	0,	"",	 FL_PUP_NONE},
    { "BMDC",	0,	"",	 FL_PUP_NONE},
    { "SCCC",	0,	"",	 FL_PUP_NONE},
    { "Firing Unit",	0,	"",	 FL_PUP_NONE},
    { "Air Ops Center",	0,	"",	 FL_PUP_NONE},
    { "STRATCOM",	0,	"",	 FL_PUP_NONE},
    { "MIDB Data Center",	0,	"",	 FL_PUP_NONE},
    {0}
};

static FL_PUP_ENTRY fdmenu_bmc_inject_2[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "Evidence Report",	0,	"",	 FL_PUP_NONE},
    { "JCS Order",	0,	"",	 FL_PUP_NONE},
    { "Potential Event",	0,	"",	 FL_PUP_NONE},
    { "Track Report",	0,	"",	 FL_PUP_NONE},
    { "TCT Detect",	0,	"",	 FL_PUP_NONE},
    {0}
};

FD_bmc3 *create_form_bmc3(void)
{
  FL_OBJECT *obj;
  FD_bmc3 *fdui = (FD_bmc3 *) fl_calloc(1, sizeof(*fdui));

  fdui->bmc3 = fl_bgn_form(FL_NO_BOX, 1001, 711);
  obj = fl_add_box(FL_UP_BOX,0,0,1001,711,"");
  obj = fl_add_box(FL_BORDER_BOX,140,200,820,470,"");
    fl_set_object_color(obj,FL_LEFT_BCOL,FL_COL1);
  obj = fl_add_button(FL_NORMAL_BUTTON,20,470,90,30,"Signoff");
    fl_set_object_lcolor(obj,FL_RED);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,signoffCB,0);
  fdui->clocktime = obj = fl_add_clock(FL_DIGITAL_CLOCK,890,10,100,30,"");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_color(obj,FL_COL1,FL_BOTTOM_BCOL);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_NORMAL_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,10,40,980,10,"");
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
  fdui->classification = obj = fl_add_text(FL_NORMAL_TEXT,330,10,350,30,"Unclassified");
    fl_set_object_boxtype(obj,FL_BORDER_BOX);
    fl_set_object_color(obj,FL_DEEPPINK,FL_MCOL);
    fl_set_object_lcolor(obj,FL_WHITE);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,125,50,5,655,"");
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
  obj = fl_add_button(FL_NORMAL_BUTTON,20,435,90,30,"Lockscreen");
    fl_set_object_lcolor(obj,FL_SPRINGGREEN);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,lockCB,0);
  fdui->node_position = obj = fl_add_text(FL_NORMAL_TEXT,10,205,110,20,"CCO");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,140,45,210,20,"STATUS");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,360,50,10,110,"");
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,570,50,10,110,"");
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
  fdui->sum_label = obj = fl_add_text(FL_NORMAL_TEXT,580,50,220,20,"ATO SUMMARY");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,800,50,10,110,"");
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,130,160,860,10,"");
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
  fdui->plan_val = obj = fl_add_text(FL_NORMAL_TEXT,265,140,95,20,"MAAP-1411.8");
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->defcon_label = obj = fl_add_checkbutton(FL_NORMAL_BUTTON,130,65,80,20,"DEFCON:");
    fl_set_object_color(obj,FL_GREEN,FL_GREEN);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,dirstatusCB,1);
  fdui->rp_label = obj = fl_add_checkbutton(FL_NORMAL_BUTTON,130,85,80,20,"JAOP");
    fl_set_object_color(obj,FL_GREEN,FL_GREEN);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,dirstatusCB,2);
  fdui->dea_label = obj = fl_add_checkbutton(FL_NORMAL_BUTTON,130,105,60,20,"GAT");
    fl_set_object_color(obj,FL_GREEN,FL_GREEN);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,dirstatusCB,3);
  fdui->roe_label = obj = fl_add_checkbutton(FL_NORMAL_BUTTON,130,125,60,20,"Area:");
    fl_set_object_color(obj,FL_GREEN,FL_GREEN);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,dirstatusCB,4);
  fdui->msncon_label = obj = fl_add_checkbutton(FL_NORMAL_BUTTON,250,65,110,20,"Msn Constraints");
    fl_set_object_color(obj,FL_GREEN,FL_GREEN);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,dirstatusCB,5);
  fdui->msnobj_label = obj = fl_add_checkbutton(FL_NORMAL_BUTTON,250,95,110,20,"");
    fl_set_object_color(obj,FL_GREEN,FL_GREEN);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,dirstatusCB,6);
  fdui->plan_label = obj = fl_add_checkbutton(FL_NORMAL_BUTTON,250,125,110,20,"");
    fl_set_object_color(obj,FL_GREEN,FL_GREEN);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,dirstatusCB,7);
  fdui->alarm_button = obj = fl_add_button(FL_NORMAL_BUTTON,10,50,95,20,"");
    fl_set_object_color(obj,FL_TOP_BCOL,FL_COL1);
    fl_set_object_callback(obj,alarmCB,0);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,10,90,110,10,"");
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
  fdui->sdf_chart = obj = fl_add_chart(FL_PIE_CHART,470,90,100,70,"");
    fl_set_object_lalign(obj,FL_ALIGN_TOP);
    fl_set_object_callback(obj,wpnchartCB,1);
  fdui->monitor_menu = obj = fl_add_menu(FL_PULLDOWN_MENU,630,170,110,20,"Execution Monitor");
    fl_set_object_boxtype(obj,FL_UP_BOX);
    fl_set_object_color(obj,FL_WHITE,FL_MCOL);
    fl_set_object_callback(obj,monitormenuCB,0);
  fdui->determine_menu = obj = fl_add_menu(FL_PULLDOWN_MENU,270,170,110,20,"Determine Strategy");
    fl_set_object_boxtype(obj,FL_UP_BOX);
    fl_set_object_color(obj,FL_WHITE,FL_MCOL);
    fl_set_object_callback(obj,determineCB,0);
  fdui->coadev_menu = obj = fl_add_menu(FL_PULLDOWN_MENU,390,170,110,20,"COA Development");
    fl_set_object_boxtype(obj,FL_UP_BOX);
    fl_set_object_color(obj,FL_WHITE,FL_MCOL);
    fl_set_object_callback(obj,coadevmenuCB,0);
  fdui->assess_menu = obj = fl_add_menu(FL_PULLDOWN_MENU,150,170,110,20,"Assessments");
    fl_set_object_boxtype(obj,FL_UP_BOX);
    fl_set_object_color(obj,FL_WHITE,FL_MCOL);
    fl_set_object_callback(obj,assessmenuCB,0);
  fdui->util_menu = obj = fl_add_menu(FL_PULLDOWN_MENU,870,170,105,20,"Control");
    fl_set_object_boxtype(obj,FL_UP_BOX);
    fl_set_object_color(obj,FL_WHITE,FL_MCOL);
    fl_set_object_callback(obj,utilmenuCB,0);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,10,505,110,10,"");
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
  fdui->summ_label[4] = obj = fl_add_text(FL_NORMAL_TEXT,690,80,80,20,"No. of Leakers:");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->summ_label[5] = obj = fl_add_text(FL_NORMAL_TEXT,690,100,90,20,"Targets in Track:");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->summ_label[6] = obj = fl_add_text(FL_NORMAL_TEXT,690,120,90,20,"Targets Engaged:");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->summ_label[7] = obj = fl_add_text(FL_NORMAL_TEXT,690,140,90,20,"Not Engaged:");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->summ_label[0] = obj = fl_add_text(FL_NORMAL_TEXT,575,80,90,20,"Tot Exp Threats:");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->summ_label[1] = obj = fl_add_text(FL_NORMAL_TEXT,575,100,90,20,"Tot Exp Targets:");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->summ_label[2] = obj = fl_add_text(FL_NORMAL_TEXT,575,120,90,20,"Tot Targets Killed:");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->summ_label[3] = obj = fl_add_text(FL_NORMAL_TEXT,575,140,90,20,"Cur Exp Targets:");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->summ_value[0] = obj = fl_add_text(FL_NORMAL_TEXT,660,80,30,20,"25");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,missummaryCB,0);
  fdui->summ_value[1] = obj = fl_add_text(FL_NORMAL_TEXT,660,100,30,20,"25");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_GREEN,FL_GREEN);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,missummaryCB,1);
  fdui->summ_value[2] = obj = fl_add_text(FL_NORMAL_TEXT,660,120,30,20,"12");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_GREEN,FL_GREEN);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,missummaryCB,2);
  fdui->summ_value[3] = obj = fl_add_text(FL_NORMAL_TEXT,660,140,30,20,"13");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_GREEN,FL_GREEN);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,missummaryCB,3);
  fdui->summ_value[4] = obj = fl_add_text(FL_NORMAL_TEXT,770,80,30,20,"0");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_GREEN,FL_GREEN);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,missummaryCB,4);
  fdui->summ_value[5] = obj = fl_add_text(FL_NORMAL_TEXT,770,100,30,20,"5");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_GREEN,FL_GREEN);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,missummaryCB,5);
  fdui->summ_value[6] = obj = fl_add_text(FL_NORMAL_TEXT,770,120,30,20,"4");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_GREEN,FL_GREEN);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,missummaryCB,6);
  fdui->summ_value[7] = obj = fl_add_text(FL_NORMAL_TEXT,770,140,30,20,"1");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_GREEN,FL_GREEN);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,missummaryCB,7);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,10,315,110,10,"");
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
  fdui->act_windows = obj = fl_add_browser(FL_SELECT_BROWSER,10,530,110,170,"Active Windows");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,actwindowsCB,0);
  fdui->map2d = obj = fl_add_pixmapbutton(FL_NORMAL_BUTTON,810,50,180,90,"");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_callback(obj,map2dCB,0);
    fl_set_object_helper(obj, "Show 3D Globe");
    fl_set_pixmapbutton_file(obj, "../BitMaps/navmap.xpm");
    fl_set_pixmapbutton_focus_file(obj, "../BitMaps/navmap.xpm");
    fl_set_pixmapbutton_focus_outline(obj,0);
  fdui->alarm_text = obj = fl_add_text(FL_NORMAL_TEXT,10,70,110,20,"Alarm Text");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,nothingCB,0);
  fdui->nmd_chart = obj = fl_add_chart(FL_PIE_CHART,370,90,100,70,"");
    fl_set_object_lalign(obj,FL_ALIGN_TOP);
    fl_set_object_callback(obj,wpnchartCB,0);
  fdui->runmode = obj = fl_add_text(FL_NORMAL_TEXT,130,15,180,20,"DEVELOPMENT");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,nothingCB,0);
  fdui->projectname = obj = fl_add_text(FL_NORMAL_TEXT,690,15,180,20,"Offense/Defense Integration");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,130,190,860,10,"");
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->detailed_menu = obj = fl_add_menu(FL_PULLDOWN_MENU,510,170,110,20,"Detailed Planning");
    fl_set_object_boxtype(obj,FL_UP_BOX);
    fl_set_object_color(obj,FL_WHITE,FL_MCOL);
    fl_set_object_callback(obj,detailmenuCB,0);
  fdui->bmc_choice = obj = fl_add_choice(FL_NORMAL_CHOICE2,10,145,110,20,"");
    fl_set_object_color(obj,FL_WHITE,FL_BLACK);
    fl_set_object_lcolor(obj,FL_COL1);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,opsmodeCB,0);
    fl_set_choice_entries(obj, fdchoice_bmc_choice_0);
    fl_set_choice(obj,6);
  obj = fl_add_button(FL_NORMAL_BUTTON,10,105,110,20,"System Capability");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_color(obj,FL_SPRINGGREEN,FL_SPRINGGREEN);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,showstatsCB,5);
  fdui->nmd_weap_menu = obj = fl_add_choice(FL_NORMAL_CHOICE2,370,65,100,20,"Def. Summary");
    fl_set_object_color(obj,FL_WHITE,FL_BLACK);
    fl_set_object_lalign(obj,FL_ALIGN_TOP);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,nmdweapsumCB,0);
  fdui->sdf_weap_menu = obj = fl_add_choice(FL_NORMAL_CHOICE2,470,65,100,20,"Off. Summary");
    fl_set_object_color(obj,FL_WHITE,FL_BLACK);
    fl_set_object_lalign(obj,FL_ALIGN_TOP);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,sdfweapsumCB,0);
  fdui->bmctop_browser = obj = fl_add_browser(FL_NORMAL_BROWSER,635,510,315,155,"");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_color(obj,FL_LEFT_BCOL,FL_YELLOW);
    fl_set_object_callback(obj,BMCnoneCB,0);
    fl_set_browser_hscrollbar(obj, FL_OFF);
    fl_set_browser_vscrollbar(obj, FL_OFF);
  fdui->node_center = obj = fl_add_choice(FL_NORMAL_CHOICE,10,175,110,20,"");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_callback(obj,BMCnoneCB,0);
    fl_set_choice_entries(obj, fdchoice_node_center_1);
    fl_set_choice(obj,5);
  fdui->msnobj_val = obj = fl_add_text(FL_NORMAL_TEXT,265,110,90,20,"ATO-16Jul01");
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->bmc_inject = obj = fl_add_menu(FL_PULLDOWN_MENU,810,140,180,20,"Event Injector");
    fl_set_object_boxtype(obj,FL_UP_BOX);
    fl_set_object_callback(obj,NMDinjectCB,0);
    fl_set_menu_entries(obj, fdmenu_bmc_inject_2);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,10,130,110,10,"");
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,10,230,110,10,"");
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
  obj = fl_add_button(FL_NORMAL_BUTTON,20,400,90,30,"Screen Snap");
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,snapCB,0);
  fdui->bmc_mission = obj = fl_add_pixmap(FL_NORMAL_PIXMAP,635,230,315,240,"Mission Area");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_color(obj,FL_LEFT_BCOL,FL_LEFT_BCOL);
    fl_set_object_lsize(obj,FL_HUGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,20,365,90,30,"Command Log");
    fl_set_object_lcolor(obj,FL_DARKTOMATO);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,cmdlogCB,0);
  fdui->bmctop_map = obj = fl_add_pixmap(FL_NORMAL_PIXMAP,145,205,485,460,"");
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->tool_menu = obj = fl_add_menu(FL_PULLDOWN_MENU,750,170,110,20,"Knowledge Manage");
    fl_set_object_boxtype(obj,FL_UP_BOX);
    fl_set_object_color(obj,FL_WHITE,FL_MCOL);
    fl_set_object_callback(obj,toolmenuCB,0);
  fdui->bmc_message = obj = fl_add_input(FL_NORMAL_INPUT,190,685,665,20,"Message");
    fl_set_object_boxtype(obj,FL_UP_BOX);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->bmc_canvas = obj = fl_add_glcanvas(FL_NORMAL_CANVAS,145,205,485,460,"");
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->simtime = obj = fl_add_button(FL_NORMAL_BUTTON,15,10,100,30,"simtime");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_NORMAL_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,simtimeCB,0);
  fdui->defcon_val = obj = fl_add_input(FL_NORMAL_INPUT,205,65,40,20,"");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->rp_val = obj = fl_add_input(FL_NORMAL_INPUT,205,85,40,20,"");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->dea_val = obj = fl_add_input(FL_NORMAL_INPUT,205,105,40,20,"");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_callback(obj,BMCnoneCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,20,330,90,30,"File Manager");
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,filemanCB,0);
  fdui->roe_val = obj = fl_add_text(FL_NORMAL_TEXT,155,140,90,20,"");
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->msncon_val = obj = fl_add_text(FL_NORMAL_TEXT,265,80,95,20,"None");
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->bmc_feed = obj = fl_add_pixmap(FL_NORMAL_PIXMAP,935,690,15,15,"Data Feeds");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->bmc_incount = obj = fl_add_input(FL_NORMAL_INPUT,955,685,40,20,"");
    fl_set_object_boxtype(obj,FL_BORDER_BOX);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->bmc_alarmcount = obj = fl_add_input(FL_INT_INPUT,105,50,20,20,"");
    fl_set_object_boxtype(obj,FL_BORDER_BOX);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,635,205,320,20,"General Purpose Decision Aids Testbed - 3.3");
    fl_set_object_color(obj,FL_LEFT_BCOL,FL_LEFT_BCOL);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fl_end_form();

  fdui->bmc3->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_login *create_form_login(void)
{
  FL_OBJECT *obj;
  FD_login *fdui = (FD_login *) fl_calloc(1, sizeof(*fdui));

  fdui->login = fl_bgn_form(FL_NO_BOX, 420, 380);
  obj = fl_add_box(FL_UP_BOX,0,0,420,380,"");
  fdui->username = obj = fl_add_input(FL_NORMAL_INPUT,150,190,180,30,"User ID:");
    fl_set_object_lcolor(obj,FL_RED);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE);
    fl_set_object_callback(obj,loginCB,1);
  fdui->password = obj = fl_add_input(FL_NORMAL_INPUT,150,230,180,30,"Password:");
    fl_set_object_lcolor(obj,FL_RED);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE);
    fl_set_object_callback(obj,loginCB,2);
  obj = fl_add_button(FL_NORMAL_BUTTON,90,290,70,30,"OK");
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,loginCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,250,290,70,30,"Cancel");
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,signoffCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,20,30,380,60,"TRW\nOffense/Defense Integration");
    fl_set_object_lsize(obj,FL_HUGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,90,130,220,30,"BMC3 System");
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_SHADOW_STYLE);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,20,340,380,30,"(The information presented here is TRW Proprietary)");
    fl_set_object_lcolor(obj,FL_RED);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fl_end_form();

  fdui->login->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_cmocnode *create_form_cmocnode(void)
{
  FL_OBJECT *obj;
  FD_cmocnode *fdui = (FD_cmocnode *) fl_calloc(1, sizeof(*fdui));

  fdui->cmocnode = fl_bgn_form(FL_NO_BOX, 470, 611);
  obj = fl_add_box(FL_UP_BOX,0,0,470,611,"");
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,220,80,210,200,"Position");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE);
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,30,80,180,200,"Command Center");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE);
  obj = fl_add_text(FL_NORMAL_TEXT,10,20,450,20,"Select desired Command Center and position from list below");
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_button(FL_NORMAL_BUTTON,120,570,70,30,"OK");
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,loginCB,10);
  obj = fl_add_button(FL_NORMAL_BUTTON,280,570,70,30,"Cancel");
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,signoffCB,0);
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,30,330,180,230,"Command Center");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE);
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,220,330,210,230,"Position");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE);
  obj = fl_add_text(FL_NORMAL_TEXT,10,50,450,20,"NMD/SDF");
    fl_set_object_color(obj,FL_WHITE,FL_MCOL);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_text(FL_NORMAL_TEXT,10,300,450,20,"AOC/TMD");
    fl_set_object_color(obj,FL_WHITE,FL_MCOL);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);

  fdui->cc_grp = fl_bgn_group();
  fdui->cmdcenter[4] = obj = fl_add_checkbutton(FL_RADIO_BUTTON,50,250,140,20,"STRATCOM");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,cmdcenterCB,4);
  fdui->cmdcenter[0] = obj = fl_add_checkbutton(FL_RADIO_BUTTON,50,100,140,20,"NORAD CCC");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,cmdcenterCB,0);
  fdui->cmdcenter[1] = obj = fl_add_checkbutton(FL_RADIO_BUTTON,50,120,140,20,"BMDC");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,cmdcenterCB,1);
  fdui->cmdcenter[2] = obj = fl_add_checkbutton(FL_RADIO_BUTTON,50,140,140,20,"SCCC");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,cmdcenterCB,2);
  fdui->cmdcenter[3] = obj = fl_add_checkbutton(FL_RADIO_BUTTON,50,160,140,20,"Firing Unit");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,cmdcenterCB,3);
  obj = fl_add_checkbutton(FL_RADIO_BUTTON,50,350,140,20,"Air Operations");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,cmdcenterCB,5);
  obj = fl_add_checkbutton(FL_RADIO_BUTTON,50,490,140,20,"PAC3");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,cmdcenterCB,6);
  fl_end_group();


  fdui->pos_grp = fl_bgn_group();
  fdui->position[10] = obj = fl_add_checkbutton(FL_RADIO_BUTTON,230,250,180,20,"SIOP Analyst");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,positionCB,11);
  fdui->position[1] = obj = fl_add_checkbutton(FL_RADIO_BUTTON,230,100,180,20,"CINC/CD");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,positionCB,1);
  fdui->position[2] = obj = fl_add_checkbutton(FL_RADIO_BUTTON,230,120,180,20,"Msl Defense 1");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,positionCB,2);
  fdui->position[3] = obj = fl_add_checkbutton(FL_RADIO_BUTTON,230,140,180,20,"Msl Defense 2");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,positionCB,3);
  fdui->position[5] = obj = fl_add_checkbutton(FL_RADIO_BUTTON,230,160,180,20,"Cmdr/Director");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,positionCB,4);
  fdui->position[6] = obj = fl_add_checkbutton(FL_RADIO_BUTTON,230,180,180,20,"Battle Intel Analyst");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,positionCB,5);
  fdui->position[7] = obj = fl_add_checkbutton(FL_RADIO_BUTTON,230,200,180,20,"Weapons Analyst");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,positionCB,6);
  fdui->position[8] = obj = fl_add_checkbutton(FL_RADIO_BUTTON,230,220,180,20,"Sensor Analyst");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,positionCB,7);
  obj = fl_add_checkbutton(FL_RADIO_BUTTON,230,350,180,20,"CCO");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,positionCB,21);
  obj = fl_add_checkbutton(FL_RADIO_BUTTON,230,370,180,20,"ODO");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,positionCB,22);
  obj = fl_add_checkbutton(FL_RADIO_BUTTON,230,390,180,20,"Weather Officer");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,positionCB,23);
  obj = fl_add_checkbutton(FL_RADIO_BUTTON,230,410,180,20,"Intel Officer");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,positionCB,24);
  obj = fl_add_checkbutton(FL_RADIO_BUTTON,230,490,180,20,"Firing Unit");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,positionCB,31);
  fl_end_group();

  obj = fl_add_checkbutton(FL_RADIO_BUTTON,230,430,180,20,"UAW Officer");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,positionCB,25);
  obj = fl_add_checkbutton(FL_RADIO_BUTTON,230,510,180,20,"Commander");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,positionCB,32);
  fl_end_form();

  fdui->cmocnode->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

static FL_PUP_ENTRY fdchoice_trk_menu_3[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "Track Engagement Summary",	0,	"",	 FL_PUP_NONE},
    { "Boost Tracks",	0,	"",	 FL_PUP_GRAY},
    { "Sensor Tracks",	0,	"",	 FL_PUP_GRAY},
    { "Engagement Tracks",	0,	"",	 FL_PUP_GRAY},
    { "Predictive Tracks",	0,	"",	 FL_PUP_GRAY},
    {0}
};

FD_bmctrack *create_form_bmctrack(void)
{
  FL_OBJECT *obj;
  FD_bmctrack *fdui = (FD_bmctrack *) fl_calloc(1, sizeof(*fdui));

  fdui->bmctrack = fl_bgn_form(FL_NO_BOX, 850, 460);
  obj = fl_add_box(FL_UP_BOX,0,0,850,460,"");
  fdui->trk_browser = obj = fl_add_browser(FL_HOLD_BROWSER,10,150,740,260,"");
    fl_set_object_lcolor(obj,FL_DEEPPINK);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_FIXEDBOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->trk_header = obj = fl_add_text(FL_NORMAL_TEXT,5,110,740,40,"");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,760,10,80,30,"Exit");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,TRKexitCB,0);
  fdui->trk_menu = obj = fl_add_choice(FL_NORMAL_CHOICE2,410,90,330,20,"");
    fl_set_object_callback(obj,TRKnoneCB,7);
    fl_set_choice_entries(obj, fdchoice_trk_menu_3);
    fl_set_choice(obj,1);
  obj = fl_add_button(FL_NORMAL_BUTTON,30,420,150,30,"Highlight Globe Track ...");
    fl_set_object_callback(obj,TRKnoneCB,10);
  obj = fl_add_button(FL_NORMAL_BUTTON,210,420,140,30,"Time To Go ...");
    fl_set_object_callback(obj,TRKnoneCB,11);
  obj = fl_add_button(FL_NORMAL_BUTTON,380,420,150,30,"Threatened Assets ...");
    fl_set_object_callback(obj,TRKnoneCB,12);
  obj = fl_add_button(FL_NORMAL_BUTTON,560,420,150,30,"Management By Exception ...");
    fl_set_object_callback(obj,TRKnoneCB,13);
  obj = fl_add_text(FL_NORMAL_TEXT,10,10,750,20,"TRACKS");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,nothingCB,0);
  fl_end_form();

  fdui->bmctrack->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_infodialog *create_form_infodialog(void)
{
  FL_OBJECT *obj;
  FD_infodialog *fdui = (FD_infodialog *) fl_calloc(1, sizeof(*fdui));

  fdui->infodialog = fl_bgn_form(FL_NO_BOX, 322, 381);
  obj = fl_add_box(FL_UP_BOX,0,0,322,381,"");
  obj = fl_add_button(FL_NORMAL_BUTTON,130,330,60,30,"OK");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,infoexitCB,0);
  fdui->info_text = obj = fl_add_text(FL_NORMAL_TEXT,30,60,260,250,"text");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_FIXEDBOLD_STYLE);
    fl_set_object_callback(obj,nothingCB,0);
  fdui->info_title = obj = fl_add_text(FL_NORMAL_TEXT,30,20,260,30,"title");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,nothingCB,0);
  fl_end_form();

  fdui->infodialog->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_drawdown *create_form_drawdown(void)
{
  FL_OBJECT *obj;
  FD_drawdown *fdui = (FD_drawdown *) fl_calloc(1, sizeof(*fdui));

  fdui->drawdown = fl_bgn_form(FL_NO_BOX, 850, 460);
  obj = fl_add_box(FL_UP_BOX,0,0,850,460,"");
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,10,70,400,380,"Drawdown Curves");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,420,70,420,190,"Red 1st Strike Diminishing Returns");
    fl_set_object_color(obj,FL_RED,FL_COL1);
    fl_set_object_lcolor(obj,FL_RED);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,760,10,80,30,"Exit");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,drawdownCB,0);
  fdui->red_diminish_plot = obj = fl_add_xyplot(FL_NORMAL_XYPLOT,430,170,400,90,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_COL1,FL_RED);
    fl_set_object_callback(obj,drawdownCB,2);
  obj = fl_add_text(FL_NORMAL_TEXT,430,90,60,20,"# Strikes");
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,430,120,50,20,"No ODI");
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,430,150,60,20,"With ODI");
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,430,110,410,10,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_COL1,FL_BLACK);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,430,140,410,10,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_COL1,FL_BLACK);
    fl_set_object_callback(obj,nothingCB,0);
  fdui->no_odi[0] = obj = fl_add_input(FL_FLOAT_INPUT,510,120,40,20,"");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER);
    fl_set_object_callback(obj,drawdownCB,3);
  fdui->no_odi[2] = obj = fl_add_input(FL_FLOAT_INPUT,650,120,40,20,"");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER);
    fl_set_object_callback(obj,drawdownCB,3);
  fdui->no_odi[4] = obj = fl_add_input(FL_FLOAT_INPUT,790,120,40,20,"");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER);
    fl_set_object_callback(obj,drawdownCB,3);
  fdui->no_odi[3] = obj = fl_add_input(FL_FLOAT_INPUT,720,120,40,20,"");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER);
    fl_set_object_callback(obj,drawdownCB,3);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,490,90,10,90,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_COL1,FL_BLACK);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,560,90,10,90,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_COL1,FL_BLACK);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,630,90,10,90,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_COL1,FL_BLACK);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,700,90,10,90,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_COL1,FL_BLACK);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,770,90,10,90,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_COL1,FL_BLACK);
    fl_set_object_callback(obj,nothingCB,0);
  fdui->with_odi[0] = obj = fl_add_input(FL_FLOAT_INPUT,510,150,40,20,"");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER);
    fl_set_object_callback(obj,drawdownCB,4);
  fdui->with_odi[1] = obj = fl_add_input(FL_FLOAT_INPUT,580,150,40,20,"");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER);
    fl_set_object_callback(obj,drawdownCB,4);
  fdui->with_odi[2] = obj = fl_add_input(FL_FLOAT_INPUT,650,150,40,20,"");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER);
    fl_set_object_callback(obj,drawdownCB,4);
  fdui->with_odi[3] = obj = fl_add_input(FL_FLOAT_INPUT,720,150,40,20,"");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER);
    fl_set_object_callback(obj,drawdownCB,4);
  fdui->with_odi[4] = obj = fl_add_input(FL_FLOAT_INPUT,790,150,40,20,"");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER);
    fl_set_object_callback(obj,drawdownCB,4);
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,420,280,420,170,"Blue 1st Strike Diminishing Returns");
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,nothingCB,0);
  fdui->blue_diminish_plot = obj = fl_add_xyplot(FL_NORMAL_XYPLOT,430,360,400,90,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_COL1,FL_BLUE);
    fl_set_object_callback(obj,drawdownCB,4);
  fdui->drawdown_plot = obj = fl_add_xyplot(FL_ACTIVE_XYPLOT,20,120,380,330,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_callback(obj,drawdownCB,1);
  obj = fl_add_text(FL_NORMAL_TEXT,715,285,120,20,"Kill Ratio Table");
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,430,300,60,20,"# Strikes");
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,430,330,50,20,"Triad");
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,430,320,410,10,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_COL1,FL_BLACK);
    fl_set_object_callback(obj,nothingCB,0);
  fdui->triad[0] = obj = fl_add_input(FL_FLOAT_INPUT,510,330,40,20,"");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER);
    fl_set_object_callback(obj,drawdownCB,6);
  fdui->triad[1] = obj = fl_add_input(FL_FLOAT_INPUT,580,330,40,20,"");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER);
    fl_set_object_callback(obj,drawdownCB,6);
  fdui->triad[2] = obj = fl_add_input(FL_FLOAT_INPUT,650,330,40,20,"");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER);
    fl_set_object_callback(obj,drawdownCB,6);
  fdui->triad[4] = obj = fl_add_input(FL_FLOAT_INPUT,790,330,40,20,"");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER);
    fl_set_object_callback(obj,drawdownCB,6);
  fdui->triad[3] = obj = fl_add_input(FL_FLOAT_INPUT,720,330,40,20,"");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER);
    fl_set_object_callback(obj,drawdownCB,6);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,490,300,10,60,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_COL1,FL_BLACK);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,560,300,10,60,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_COL1,FL_BLACK);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,630,300,10,60,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_COL1,FL_BLACK);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,700,300,10,60,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_COL1,FL_BLACK);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,770,300,10,60,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_COL1,FL_BLACK);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,715,75,120,20,"Kill Ratio Table");
    fl_set_object_lcolor(obj,FL_RED);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,510,90,40,20,"1 - 3");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,580,90,40,20,"3 - 10");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,650,90,40,20,"10 - 30");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,710,90,60,20,"30 - 100");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,510,300,40,20,"1 - 3");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,775,90,60,20,"100 - 300");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,775,300,60,20,"100 - 300");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,710,300,60,20,"30 - 100");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,650,300,40,20,"10 - 30");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,580,300,40,20,"3 - 10");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,10,50,830,10,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_callback(obj,nothingCB,0);
  fdui->no_odi[1] = obj = fl_add_input(FL_FLOAT_INPUT,580,120,40,20,"");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER);
    fl_set_object_callback(obj,drawdownCB,3);
  fdui->drawdown_alpha = obj = fl_add_input(FL_FLOAT_INPUT,40,100,100,20,"Alpha Angle");
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,drawdownCB,10);
  fdui->drawdown_parity = obj = fl_add_input(FL_NORMAL_INPUT,165,100,100,20,"Parity Point");
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,drawdownCB,11);
  fdui->drawdown_offset = obj = fl_add_input(FL_NORMAL_INPUT,290,100,100,20,"Offset Point");
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,drawdownCB,12);
  fl_end_form();

  fdui->drawdown->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_sitmonitor *create_form_sitmonitor(void)
{
  FL_OBJECT *obj;
  FD_sitmonitor *fdui = (FD_sitmonitor *) fl_calloc(1, sizeof(*fdui));

  fdui->sitmonitor = fl_bgn_form(FL_NO_BOX, 1001, 711);
  obj = fl_add_box(FL_UP_BOX,0,0,1001,711,"");
  obj = fl_add_text(FL_NORMAL_TEXT,20,20,380,30,"Situation Monitor");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE);
  obj = fl_add_text(FL_NORMAL_TEXT,30,80,350,20,"The Situation Monitor has determined that the");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,nothingCB,0);
  fdui->sit_what = obj = fl_add_text(FL_NORMAL_TEXT,30,100,360,30,"DEFCON Level");
    fl_set_object_color(obj,FL_BLUE,FL_MCOL);
    fl_set_object_lcolor(obj,FL_WHITE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,30,150,350,20,"should be changed to");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->sit_value = obj = fl_add_text(FL_NORMAL_TEXT,30,170,360,30,"4");
    fl_set_object_color(obj,FL_CYAN,FL_MCOL);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_text(FL_NORMAL_TEXT,30,240,350,20,"Do You Concur?");
    fl_set_object_color(obj,FL_WHITE,FL_MCOL);
    fl_set_object_lcolor(obj,FL_RED);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_button(FL_NORMAL_BUTTON,40,280,90,30,"Yes");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_button(FL_NORMAL_BUTTON,160,280,90,30,"No");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_button(FL_NORMAL_BUTTON,280,280,90,30,"Why");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fl_end_form();

  fdui->sitmonitor->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_bmcengage *create_form_bmcengage(void)
{
  FL_OBJECT *obj;
  FD_bmcengage *fdui = (FD_bmcengage *) fl_calloc(1, sizeof(*fdui));

  fdui->bmcengage = fl_bgn_form(FL_NO_BOX, 850, 460);
  obj = fl_add_box(FL_UP_BOX,0,0,850,460,"");
  obj = fl_add_button(FL_NORMAL_BUTTON,760,10,80,30,"Exit");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,ENGexitCB,0);
  fdui->engage_browser = obj = fl_add_browser(FL_NORMAL_BROWSER,10,120,740,280,"");
    fl_set_object_color(obj,FL_LEFT_BCOL,FL_YELLOW);
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_callback(obj,BMCnoneCB,0);
    fl_set_browser_vscrollbar(obj, FL_ON);
  fdui->engage_header = obj = fl_add_text(FL_NORMAL_TEXT,10,80,740,40,"");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fl_end_form();

  fdui->bmcengage->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_bmcalarms *create_form_bmcalarms(void)
{
  FL_OBJECT *obj;
  FD_bmcalarms *fdui = (FD_bmcalarms *) fl_calloc(1, sizeof(*fdui));

  fdui->bmcalarms = fl_bgn_form(FL_NO_BOX, 620, 530);
  obj = fl_add_box(FL_UP_BOX,0,0,620,530,"");
  fdui->folder = obj = fl_add_tabfolder(FL_TOP_TABFOLDER,10,70,600,450,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->accept_btn = obj = fl_add_button(FL_NORMAL_BUTTON,120,20,110,30,"Accept");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_TIMESBOLD_STYLE);
    fl_set_object_callback(obj,alrmactionCB,0);
  fdui->defer_btn = obj = fl_add_button(FL_NORMAL_BUTTON,260,20,110,30,"Defer");
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_TIMESBOLD_STYLE);
    fl_set_object_callback(obj,alrmactionCB,1);
  fdui->cancel_btn = obj = fl_add_button(FL_NORMAL_BUTTON,390,20,110,30,"Cancel");
    fl_set_object_lcolor(obj,FL_RED);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_TIMESBOLD_STYLE);
    fl_set_object_callback(obj,alrmactionCB,2);
  fl_end_form();

  fdui->bmcalarms->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_alrmaccept *create_form_alrmaccept(void)
{
  FL_OBJECT *obj;
  FD_alrmaccept *fdui = (FD_alrmaccept *) fl_calloc(1, sizeof(*fdui));

  fdui->alrmaccept = fl_bgn_form(FL_NO_BOX, 620, 530);
  obj = fl_add_box(FL_UP_BOX,0,0,620,530,"");
  fdui->alrm_accepted = obj = fl_add_browser(FL_SELECT_BROWSER,10,10,570,400,"");
    fl_set_object_callback(obj,BMCnoneCB,0);
  fl_end_form();

  fdui->alrmaccept->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_alrmdefered *create_form_alrmdefered(void)
{
  FL_OBJECT *obj;
  FD_alrmdefered *fdui = (FD_alrmdefered *) fl_calloc(1, sizeof(*fdui));

  fdui->alrmdefered = fl_bgn_form(FL_NO_BOX, 590, 420);
  obj = fl_add_box(FL_UP_BOX,0,0,590,420,"");
  fdui->alrm_defered = obj = fl_add_browser(FL_SELECT_BROWSER,10,10,570,400,"");
    fl_set_object_callback(obj,BMCnoneCB,0);
  fl_end_form();

  fdui->alrmdefered->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_alrmcancel *create_form_alrmcancel(void)
{
  FL_OBJECT *obj;
  FD_alrmcancel *fdui = (FD_alrmcancel *) fl_calloc(1, sizeof(*fdui));

  fdui->alrmcancel = fl_bgn_form(FL_NO_BOX, 590, 420);
  obj = fl_add_box(FL_UP_BOX,0,0,590,420,"");
  fdui->alrm_canceled = obj = fl_add_browser(FL_SELECT_BROWSER,10,10,570,400,"");
    fl_set_object_callback(obj,BMCnoneCB,0);
  fl_end_form();

  fdui->alrmcancel->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_sitaware *create_form_sitaware(void)
{
  FL_OBJECT *obj;
  FD_sitaware *fdui = (FD_sitaware *) fl_calloc(1, sizeof(*fdui));

  fdui->sitaware = fl_bgn_form(FL_NO_BOX, 850, 460);
  obj = fl_add_box(FL_UP_BOX,0,0,850,460,"");
  fdui->sit_canvas = obj = fl_add_glcanvas(FL_NORMAL_CANVAS,300,10,350,250,"");
    fl_set_object_callback(obj,BMCnoneCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,760,10,80,30,"Exit");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,SITexitCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,10,10,280,20,"CURRENT STATUS");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,SITnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,10,130,280,20,"THREAT INFORMATION");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,SITnoneCB,0);
  fdui->sit_threats = obj = fl_add_browser(FL_NORMAL_BROWSER,10,170,280,90,"");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->sit_threat_label = obj = fl_add_text(FL_NORMAL_TEXT,10,150,280,20,"Country                      Site                Actual     Attempts");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_text(FL_NORMAL_TEXT,10,270,280,20,"DEFENSE DURABILITY");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,SITnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,300,270,280,20,"RISK APPRECIATION");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,SITnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,590,270,250,20,"TIMELINE");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,SITnoneCB,0);
  fdui->sit_defcon = obj = fl_add_input(FL_NORMAL_INPUT,100,40,40,20,"DEFCON:");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_callback(obj,SITnoneCB,0);
  fdui->sit_roe = obj = fl_add_input(FL_NORMAL_INPUT,110,80,130,20,"ROE:");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_callback(obj,SITnoneCB,0);
  fdui->sit_dea = obj = fl_add_input(FL_NORMAL_INPUT,180,40,60,20,"DEA:");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_callback(obj,SITnoneCB,0);
  fdui->sit_strategy = obj = fl_add_input(FL_NORMAL_INPUT,110,100,130,20,"STRATEGY:");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_callback(obj,SITnoneCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,245,85,20,10,"");
    fl_set_object_boxtype(obj,FL_OVAL_BOX);
    fl_set_object_color(obj,FL_GREEN,FL_TOP_BCOL);
  obj = fl_add_button(FL_NORMAL_BUTTON,245,45,20,10,"");
    fl_set_object_boxtype(obj,FL_OVAL_BOX);
    fl_set_object_color(obj,FL_GREEN,FL_TOP_BCOL);
  fdui->sit_riskmap = obj = fl_add_pixmap(FL_NORMAL_PIXMAP,300,290,280,160,"");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_callback(obj,SITnoneCB,0);
  fdui->sit_namap = obj = fl_add_pixmap(FL_NORMAL_PIXMAP,300,10,350,250,"");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_callback(obj,SITnoneCB,0);
  fdui->sit_leakers = obj = fl_add_lightbutton(FL_PUSH_BUTTON,655,10,90,20,"LEAKER: 0");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_color(obj,FL_COL1,FL_GREEN);
    fl_set_object_callback(obj,SITnoneCB,0);
    fl_set_button(obj, 1);
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,660,65,180,195,"MAP OVERLAYS");
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,SITnoneCB,0);
  fdui->sit_overlay[0] = obj = fl_add_lightbutton(FL_PUSH_BUTTON,670,74,150,20,"Web Defacements");
    fl_set_object_callback(obj,sitoverlayCB,0);
  fdui->sit_overlay[1] = obj = fl_add_lightbutton(FL_PUSH_BUTTON,670,97,150,20,"DoS/DDoS Attacks");
    fl_set_object_callback(obj,sitoverlayCB,1);
  fdui->sit_overlay[2] = obj = fl_add_lightbutton(FL_PUSH_BUTTON,670,120,150,20,"System Pentrations");
    fl_set_object_callback(obj,sitoverlayCB,2);
  obj = fl_add_input(FL_NORMAL_INPUT,110,60,130,20,"SYSCAP:");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_callback(obj,SITnoneCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,245,65,20,10,"");
    fl_set_object_boxtype(obj,FL_OVAL_BOX);
    fl_set_object_color(obj,FL_GREEN,FL_TOP_BCOL);
  fdui->sit_overlay[3] = obj = fl_add_lightbutton(FL_PUSH_BUTTON,670,143,150,20,"Trojan Horses");
    fl_set_object_callback(obj,sitoverlayCB,3);
  fdui->sit_atrisk = obj = fl_add_text(FL_NORMAL_TEXT,300,430,280,20,"");
    fl_set_object_color(obj,FL_WHITE,FL_MCOL);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  obj = fl_add_slider(FL_HOR_FILL_SLIDER,620,315,125,10,"Plan");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_color(obj,FL_TOMATO,FL_COL1);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT);
    fl_set_object_callback(obj,BMCnoneCB,0);
    fl_set_slider_value(obj, 0);
    fl_set_slider_size(obj, 0.15);
  obj = fl_add_slider(FL_HOR_FILL_SLIDER,620,330,160,10,"Mission");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_color(obj,FL_TOMATO,FL_COL1);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT);
    fl_set_object_callback(obj,BMCnoneCB,0);
    fl_set_slider_value(obj, 0);
    fl_set_slider_size(obj, 0.15);
  obj = fl_add_slider(FL_HOR_FILL_SLIDER,700,375,100,10,"Plan");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_color(obj,FL_DODGERBLUE,FL_COL1);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT);
    fl_set_object_callback(obj,BMCnoneCB,0);
    fl_set_slider_value(obj, 0);
    fl_set_slider_size(obj, 0.15);
  obj = fl_add_slider(FL_HOR_FILL_SLIDER,790,390,40,10,"Mission");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_color(obj,FL_DODGERBLUE,FL_COL1);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT);
    fl_set_object_callback(obj,BMCnoneCB,0);
    fl_set_slider_value(obj, 0);
    fl_set_slider_size(obj, 0.15);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,830,300,10,140,"1:32");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_callback(obj,BMCnoneCB,0);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,610,300,10,140,"0:00");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->sit_overlay[4] = obj = fl_add_lightbutton(FL_PUSH_BUTTON,670,166,150,20,"Ping Saturations");
    fl_set_object_callback(obj,sitoverlayCB,4);
  fdui->sit_overlay[5] = obj = fl_add_lightbutton(FL_PUSH_BUTTON,670,189,150,20,"Detected Viruses");
    fl_set_object_callback(obj,sitoverlayCB,5);
  fdui->sit_overlay[6] = obj = fl_add_lightbutton(FL_PUSH_BUTTON,670,212,150,20,"Intrusion Attempts");
    fl_set_object_callback(obj,sitoverlayCB,6);
    fl_set_button(obj, 1);
  fdui->sit_overlay[7] = obj = fl_add_lightbutton(FL_PUSH_BUTTON,670,235,150,20,"DNS Attacks");
    fl_set_object_callback(obj,sitoverlayCB,7);
  fdui->sit_durable[3] = obj = fl_add_chart(FL_HORBAR_CHART,10,375,85,65,"Firewall Status");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,SITnoneCB,0);
  fdui->sit_durable[4] = obj = fl_add_chart(FL_HORBAR_CHART,106,375,85,65,"Port Status");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,SITnoneCB,0);
  fdui->sit_durable[5] = obj = fl_add_chart(FL_HORBAR_CHART,200,375,85,65,"IDS Rules");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,SITnoneCB,0);
  fdui->sit_durable[2] = obj = fl_add_chart(FL_HORBAR_CHART,200,293,85,65,"Firewall Status");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,SITnoneCB,0);
  fdui->sit_durable[1] = obj = fl_add_chart(FL_HORBAR_CHART,105,293,85,65,"Firewall Status");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,SITnoneCB,0);
  fdui->sit_durable[0] = obj = fl_add_chart(FL_HORBAR_CHART,10,293,85,65,"Firewall Status");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,SITnoneCB,0);
  fl_end_form();

  fdui->sitaware->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_statcomm *create_form_statcomm(void)
{
  FL_OBJECT *obj;
  FD_statcomm *fdui = (FD_statcomm *) fl_calloc(1, sizeof(*fdui));

  fdui->statcomm = fl_bgn_form(FL_NO_BOX, 486, 321);
  obj = fl_add_box(FL_FRAME_BOX,0,0,486,321,"");
  obj = fl_add_text(FL_NORMAL_TEXT,15,70,450,40,"Interface Name                     Msgs In      Bytes In              Msgs Out      Bytes Out");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->ifstats_list = obj = fl_add_browser(FL_NORMAL_BROWSER,15,110,450,175,"");
    fl_set_object_callback(obj,BMCnoneCB,0);
    fl_set_browser_vscrollbar(obj, FL_ON);
  obj = fl_add_text(FL_NORMAL_TEXT,55,15,380,30,"Communications Statistics");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE);
  fl_end_form();

  fdui->statcomm->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_statsubs *create_form_statsubs(void)
{
  FL_OBJECT *obj;
  FD_statsubs *fdui = (FD_statsubs *) fl_calloc(1, sizeof(*fdui));

  fdui->statsubs = fl_bgn_form(FL_NO_BOX, 486, 321);
  obj = fl_add_box(FL_FRAME_BOX,0,0,486,321,"");
  obj = fl_add_text(FL_NORMAL_TEXT,55,15,380,30,"Sub-System Status");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE);
  fdui->subs_bm = obj = fl_add_lightbutton(FL_PUSH_BUTTON,30,55,190,30,"Battle Manager");
    fl_set_object_color(obj,FL_RED,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->subs_mon = obj = fl_add_lightbutton(FL_PUSH_BUTTON,30,100,190,30,"Execution Monitor");
    fl_set_object_color(obj,FL_RED,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->subs_sim = obj = fl_add_lightbutton(FL_PUSH_BUTTON,265,55,190,30,"Simulator");
    fl_set_object_color(obj,FL_RED,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->subs_cop = obj = fl_add_lightbutton(FL_PUSH_BUTTON,265,100,190,30,"Fused Operating Picture");
    fl_set_object_color(obj,FL_RED,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->subs_ext = obj = fl_add_lightbutton(FL_PUSH_BUTTON,30,215,190,30,"External Interface");
    fl_set_object_color(obj,FL_RED,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->subs_multi = obj = fl_add_lightbutton(FL_PUSH_BUTTON,265,215,190,30,"Multi-Threaded Tools");
    fl_set_object_color(obj,FL_RED,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->subs_link = obj = fl_add_lightbutton(FL_PUSH_BUTTON,265,145,190,30,"Link Analyzer");
    fl_set_object_color(obj,FL_RED,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->subs_kb = obj = fl_add_lightbutton(FL_PUSH_BUTTON,30,145,190,30,"Knowledge Base");
    fl_set_object_color(obj,FL_RED,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fl_end_form();

  fdui->statsubs->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_statuser *create_form_statuser(void)
{
  FL_OBJECT *obj;
  FD_statuser *fdui = (FD_statuser *) fl_calloc(1, sizeof(*fdui));

  fdui->statuser = fl_bgn_form(FL_NO_BOX, 486, 321);
  obj = fl_add_box(FL_FRAME_BOX,0,0,486,321,"");
  obj = fl_add_text(FL_NORMAL_TEXT,55,15,380,30,"User Information");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE);
  fdui->user_info = obj = fl_add_browser(FL_NORMAL_BROWSER,20,70,445,230,"");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_callback(obj,BMCnoneCB,0);
    fl_set_browser_hscrollbar(obj, FL_OFF);
    fl_set_browser_vscrollbar(obj, FL_OFF);
  fl_end_form();

  fdui->statuser->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_stathard *create_form_stathard(void)
{
  FL_OBJECT *obj;
  FD_stathard *fdui = (FD_stathard *) fl_calloc(1, sizeof(*fdui));

  fdui->stathard = fl_bgn_form(FL_NO_BOX, 486, 321);
  obj = fl_add_box(FL_FRAME_BOX,0,0,486,321,"");
  obj = fl_add_text(FL_NORMAL_TEXT,55,15,380,30,"Hardware Information");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE);
  fdui->hard_info = obj = fl_add_browser(FL_NORMAL_BROWSER,20,70,445,230,"");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_callback(obj,BMCnoneCB,0);
    fl_set_browser_hscrollbar(obj, FL_OFF);
    fl_set_browser_vscrollbar(obj, FL_OFF);
  fl_end_form();

  fdui->stathard->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_statsyst *create_form_statsyst(void)
{
  FL_OBJECT *obj;
  FD_statsyst *fdui = (FD_statsyst *) fl_calloc(1, sizeof(*fdui));

  fdui->statsyst = fl_bgn_form(FL_NO_BOX, 486, 321);
  obj = fl_add_box(FL_FRAME_BOX,0,0,486,321,"");
  obj = fl_add_text(FL_NORMAL_TEXT,55,15,380,30,"System Information");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE);
  fdui->syst_info = obj = fl_add_browser(FL_NORMAL_BROWSER,20,70,445,230,"");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_callback(obj,BMCnoneCB,0);
    fl_set_browser_hscrollbar(obj, FL_OFF);
    fl_set_browser_vscrollbar(obj, FL_OFF);
  fl_end_form();

  fdui->statsyst->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_bmcstatus *create_form_bmcstatus(void)
{
  FL_OBJECT *obj;
  FD_bmcstatus *fdui = (FD_bmcstatus *) fl_calloc(1, sizeof(*fdui));

  fdui->bmcstatus = fl_bgn_form(FL_NO_BOX, 521, 441);
  obj = fl_add_box(FL_UP_BOX,0,0,521,441,"");
  obj = fl_add_button(FL_NORMAL_BUTTON,210,400,95,25,"Dismiss");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,statexitCB,0);
  fdui->folder = obj = fl_add_tabfolder(FL_TOP_TABFOLDER,15,25,490,355,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fl_end_form();

  fdui->bmcstatus->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_statenvs *create_form_statenvs(void)
{
  FL_OBJECT *obj;
  FD_statenvs *fdui = (FD_statenvs *) fl_calloc(1, sizeof(*fdui));

  fdui->statenvs = fl_bgn_form(FL_NO_BOX, 486, 321);
  obj = fl_add_box(FL_UP_BOX,0,0,486,321,"");
  obj = fl_add_text(FL_NORMAL_TEXT,55,15,380,30,"Environment Variables");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->bmc_env_setting = obj = fl_add_browser(FL_NORMAL_BROWSER,105,55,360,195,"");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_callback(obj,bmcenvironCB,0);
    fl_set_browser_hscrollbar(obj, FL_OFF);
    fl_set_browser_vscrollbar(obj, FL_OFF);
  fdui->bmc_env_vars = obj = fl_add_browser(FL_HOLD_BROWSER,25,55,80,195,"");
    fl_set_object_callback(obj,bmcenvironCB,1);
    fl_set_browser_hscrollbar(obj, FL_OFF);
    fl_set_browser_vscrollbar(obj, FL_OFF);
  fdui->bmc_env_input = obj = fl_add_input(FL_NORMAL_INPUT,105,260,360,25,"");
    fl_set_object_callback(obj,bmcenvironCB,2);
  obj = fl_add_button(FL_NORMAL_BUTTON,25,260,80,25,"Set");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,bmcenvironCB,3);
  obj = fl_add_button(FL_NORMAL_BUTTON,25,285,80,25,"Unset");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,bmcenvironCB,4);
  fl_end_form();

  fdui->statenvs->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_statfile *create_form_statfile(void)
{
  FL_OBJECT *obj;
  FD_statfile *fdui = (FD_statfile *) fl_calloc(1, sizeof(*fdui));

  fdui->statfile = fl_bgn_form(FL_NO_BOX, 486, 321);
  obj = fl_add_box(FL_UP_BOX,0,0,486,321,"");
  obj = fl_add_text(FL_NORMAL_TEXT,55,15,380,30,"File Information");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE);
  fdui->file_info = obj = fl_add_browser(FL_NORMAL_BROWSER,20,95,445,205,"");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_callback(obj,BMCnoneCB,0);
    fl_set_browser_hscrollbar(obj, FL_OFF);
    fl_set_browser_vscrollbar(obj, FL_OFF);
  fdui->bmc_file_folder = obj = fl_add_pixmapbutton(FL_NORMAL_BUTTON,365,60,30,30,"");
    fl_set_object_callback(obj,filestatCB,0);
    fl_set_pixmapbutton_file(obj, "../BitMaps/folder.xpm");
  fdui->bmc_file_name = obj = fl_add_input(FL_NORMAL_INPUT,50,65,310,20,"File:");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,bmcenvironCB,5);
  obj = fl_add_button(FL_NORMAL_BUTTON,405,65,55,20,"Apply");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,bmcenvironCB,5);
  fl_end_form();

  fdui->statfile->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_bmcbrowse *create_form_bmcbrowse(void)
{
  FL_OBJECT *obj;
  FD_bmcbrowse *fdui = (FD_bmcbrowse *) fl_calloc(1, sizeof(*fdui));

  fdui->bmcbrowse = fl_bgn_form(FL_NO_BOX, 850, 460);
  obj = fl_add_box(FL_UP_BOX,0,0,850,460,"");
  fdui->bmc_browse = obj = fl_add_browser(FL_NORMAL_BROWSER,10,50,830,405,"");
    fl_set_object_color(obj,FL_LEFT_BCOL,FL_YELLOW);
    fl_set_object_callback(obj,BMCnoneCB,0);
    fl_set_browser_vscrollbar(obj, FL_ON);
  obj = fl_add_button(FL_NORMAL_BUTTON,755,10,85,25,"Dismiss");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,browseexitCB,0);
  fl_end_form();

  fdui->bmcbrowse->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_About *create_form_About(void)
{
  FL_OBJECT *obj;
  FD_About *fdui = (FD_About *) fl_calloc(1, sizeof(*fdui));

  fdui->About = fl_bgn_form(FL_NO_BOX, 386, 456);
  obj = fl_add_box(FL_UP_BOX,0,0,386,456,"");
    fl_set_object_color(obj,FL_MCOL,FL_COL1);
    fl_set_object_gravity(obj, FL_NorthWest, FL_SouthEast);
  obj = fl_add_text(FL_NORMAL_TEXT,15,10,355,30,"General Purpose Decision Aids");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lcolor(obj,FL_INDIANRED);
    fl_set_object_lsize(obj,FL_HUGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_TIMESBOLD_STYLE+FL_EMBOSSED_STYLE);
    fl_set_object_gravity(obj, FL_NorthWest, FL_NorthWest);
  fdui->About_Go_Back = obj = fl_add_button(FL_RETURN_BUTTON,150,415,70,30,"Ok");
    fl_set_object_gravity(obj, FL_NorthWest, FL_NorthWest);
    fl_set_object_callback(obj,aboutexitCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,10,125,365,40,"Copyright (c) 2000-2003 by Northrop Grumman\n(Contains Northrop Grumman Proprietary Information)");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_TIMESITALIC_STYLE);
    fl_set_object_gravity(obj, FL_NorthWest, FL_NorthWest);
  fdui->Gena = obj = fl_add_pixmap(FL_NORMAL_PIXMAP,15,170,120,120,"");
    fl_set_object_color(obj,FL_BLACK,FL_TOP_BCOL);
    fl_set_object_gravity(obj, FL_NorthWest, FL_NorthWest);
    fl_set_pixmap_file(obj, "../BitMaps/DecisionAid.xpm");
  obj = fl_add_text(FL_NORMAL_TEXT,10,110,360,20,"See http://158.114.52.140 for more details");
    fl_set_object_color(obj,FL_MCOL,FL_MCOL);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_gravity(obj, FL_NorthWest, FL_NorthWest);
  obj = fl_add_text(FL_NORMAL_TEXT,140,165,235,35,"XFORMS Copyright(c) 1996-1998 by\nT.C. Zhao and Mark Overmars");
    fl_set_object_color(obj,FL_MCOL,FL_MCOL);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
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
  obj = fl_add_text(FL_NORMAL_TEXT,140,200,235,35,"GAlib Copyright(c) 1996 by\nMatthew Wall (MIT)");
    fl_set_object_color(obj,FL_MCOL,FL_MCOL);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_TIMESITALIC_STYLE);
  obj = fl_add_text(FL_NORMAL_TEXT,140,235,235,35,"FuzzyCLIPS Copyright(c) 1998 by\nNational Research Council Canada");
    fl_set_object_color(obj,FL_MCOL,FL_MCOL);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_TIMESITALIC_STYLE);
  fdui->about_copyright = obj = fl_add_browser(FL_NORMAL_BROWSER,10,295,365,110,"");
    fl_set_object_callback(obj,BMCnoneCB,0);
  fl_end_form();

  fdui->About->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

static FL_PUP_ENTRY fdchoice_prt_name_4[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "lp",	0,	"",	 FL_PUP_NONE},
    {0}
};

FD_printform *create_form_printform(void)
{
  FL_OBJECT *obj;
  FD_printform *fdui = (FD_printform *) fl_calloc(1, sizeof(*fdui));

  fdui->printform = fl_bgn_form(FL_NO_BOX, 586, 566);
  obj = fl_add_box(FL_UP_BOX,0,0,586,566,"");
  fdui->prt_list = obj = fl_add_browser(FL_NORMAL_BROWSER,15,30,560,155,"Select from Active Windows:");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,15,205,560,65,"Print to:");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,15,290,560,50,"Paper Orientation:");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,15,360,560,55,"Options:");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,15,440,560,55,"Classification Options:");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,180,520,90,25,"Print");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,bmcprintCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,305,520,90,25,"Cancel");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,bmcprintCB,1);
  obj = fl_add_checkbutton(FL_PUSH_BUTTON,60,220,80,20,"Printer");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,bmcprintCB,10);
    fl_set_button(obj, 1);
  obj = fl_add_checkbutton(FL_PUSH_BUTTON,60,245,80,20,"File");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,bmcprintCB,11);
  fdui->prt_name = obj = fl_add_choice(FL_NORMAL_CHOICE2,220,220,130,20,"Printer Name:");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,bmcprintCB,12);
    fl_set_choice_entries(obj, fdchoice_prt_name_4);
    fl_set_choice(obj,1);
  fdui->prt_fname = obj = fl_add_input(FL_NORMAL_INPUT,220,245,205,20,"File Name:");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,bmcprintCB,13);
  fdui->prt_autogen = obj = fl_add_checkbutton(FL_PUSH_BUTTON,440,245,100,20,"Autogenerate");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,bmcprintCB,14);
    fl_set_button(obj, 1);
  fdui->prt_land = obj = fl_add_checkbutton(FL_PUSH_BUTTON,160,310,110,20,"Landscape");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,bmcprintCB,15);
    fl_set_button(obj, 1);
  fdui->prt_port = obj = fl_add_checkbutton(FL_PUSH_BUTTON,310,310,110,20,"Portrait");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,bmcprintCB,16);
  fdui->prt_optfname = obj = fl_add_checkbutton(FL_PUSH_BUTTON,40,380,110,20,"Filename");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,bmcprintCB,17);
    fl_set_button(obj, 1);
  fdui->prt_pagenum = obj = fl_add_checkbutton(FL_PUSH_BUTTON,160,380,130,20,"Page Numbering");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,bmcprintCB,18);
    fl_set_button(obj, 1);
  fdui->prt_border = obj = fl_add_checkbutton(FL_PUSH_BUTTON,310,380,90,20,"Borders");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,bmcprintCB,19);
  fdui->prt_margin = obj = fl_add_checkbutton(FL_PUSH_BUTTON,440,380,110,20,"Binding Margin");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,bmcprintCB,20);

  fdui->classgroup = fl_bgn_group();
  fdui->prt_unclass = obj = fl_add_checkbutton(FL_RADIO_BUTTON,160,460,110,20,"Unclassified");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,bmcprintCB,21);
  fdui->prt_confid = obj = fl_add_checkbutton(FL_RADIO_BUTTON,310,460,110,20,"Confidential");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,bmcprintCB,22);
  fdui->prt_secret = obj = fl_add_checkbutton(FL_RADIO_BUTTON,440,460,110,20,"Secret");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,bmcprintCB,23);
  fl_end_group();

  fdui->prt_propriet = obj = fl_add_checkbutton(FL_RADIO_BUTTON,40,460,110,20,"Proprietary");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,bmcprintCB,24);
    fl_set_button(obj, 1);
  fl_end_form();

  fdui->printform->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

