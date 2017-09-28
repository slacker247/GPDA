/* Form definition file generated with fdesign. */

#include "forms.h"
#include <stdlib.h>
#include "BOXforms.h"

static FL_PUP_ENTRY fdmenu_bmc_inject_0[] =
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

  fdui->bmc3 = fl_bgn_form(FL_NO_BOX, 1000, 175);
  obj = fl_add_box(FL_UP_BOX,0,0,1000,175,"");
  obj = fl_add_button(FL_NORMAL_BUTTON,20,55,90,30,"Signoff");
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
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,120,50,10,120,"");
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
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
  fdui->sdf_chart = obj = fl_add_chart(FL_PIE_CHART,470,90,100,70,"");
    fl_set_object_lalign(obj,FL_ALIGN_TOP);
    fl_set_object_callback(obj,wpnchartCB,1);
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
  fdui->map2d = obj = fl_add_pixmapbutton(FL_NORMAL_BUTTON,810,50,180,90,"");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_callback(obj,map2dCB,0);
    fl_set_object_helper(obj, "Show 3D Globe");
    fl_set_pixmapbutton_file(obj, "../BitMaps/navmap.nmd.xpm");
    fl_set_pixmapbutton_focus_file(obj, "/home/dre/Sodi/BitMaps/navmap.xpm");
    fl_set_pixmapbutton_focus_outline(obj,0);
  fdui->nmd_chart = obj = fl_add_chart(FL_PIE_CHART,370,90,100,70,"");
    fl_set_object_lalign(obj,FL_ALIGN_TOP);
    fl_set_object_callback(obj,wpnchartCB,0);
  fdui->projectname = obj = fl_add_text(FL_NORMAL_TEXT,15,15,180,20,"Offense/Defense Integration");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,nothingCB,0);
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
  fdui->msnobj_val = obj = fl_add_text(FL_NORMAL_TEXT,265,110,90,20,"ATO-16Jul01");
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->bmc_inject = obj = fl_add_menu(FL_PULLDOWN_MENU,810,140,180,20,"Event Injector");
    fl_set_object_boxtype(obj,FL_UP_BOX);
    fl_set_object_callback(obj,NMDinjectCB,0);
    fl_set_menu_entries(obj, fdmenu_bmc_inject_0);
  fdui->defcon_val = obj = fl_add_input(FL_NORMAL_INPUT,205,65,40,20,"");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->rp_val = obj = fl_add_input(FL_NORMAL_INPUT,205,85,40,20,"");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_callback(obj,BMCnoneCB,0);
  fdui->dea_val = obj = fl_add_input(FL_NORMAL_INPUT,205,105,40,20,"");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_callback(obj,BMCnoneCB,0);
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
  fl_end_form();

  fdui->bmc3->fdui = fdui;

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

