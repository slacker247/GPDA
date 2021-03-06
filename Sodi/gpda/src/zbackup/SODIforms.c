/* Form definition file generated with fdesign. */

#include "forms.h"
#include <stdlib.h>
#include "SODIforms.h"

FD_bmc3 *create_form_bmc3(void)
{
  FL_OBJECT *obj;
  FD_bmc3 *fdui = (FD_bmc3 *) fl_calloc(1, sizeof(*fdui));

  fdui->bmc3 = fl_bgn_form(FL_NO_BOX, 992, 671);
  obj = fl_add_box(FL_UP_BOX,0,0,992,671,"");
  obj = fl_add_button(FL_NORMAL_BUTTON,20,440,90,30,"Signoff");
    fl_set_object_callback(obj,signoffCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,10,200,110,20,"CMOC Node");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->defcon_val = obj = fl_add_text(FL_NORMAL_TEXT,210,70,20,20,"5");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_color(obj,FL_TOP_BCOL,FL_MCOL);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,nothingCB,0);
  fdui->sim_history = obj = fl_add_browser(FL_NORMAL_BROWSER,130,200,850,460,"");
  obj = fl_add_clock(FL_DIGITAL_CLOCK,910,10,70,30,"");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_color(obj,FL_COL1,FL_BOTTOM_BCOL);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_NORMAL_STYLE+FL_ENGRAVED_STYLE);
  fdui->intel_browser = obj = fl_add_browser(FL_NORMAL_BROWSER,820,50,160,110,"");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,intel_browserCB,0);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,10,40,950,10,"");
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
  obj = fl_add_text(FL_NORMAL_TEXT,280,10,350,20,"Classification");
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,120,50,10,610,"");
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
  obj = fl_add_button(FL_NORMAL_BUTTON,20,410,90,30,"Lockscreen");
    fl_set_object_callback(obj,lockCB,0);
  fdui->node_val = obj = fl_add_text(FL_NORMAL_TEXT,10,220,110,20,"Command Director");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,140,50,210,20,"DIRECTIVE STATUS");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_text(FL_NORMAL_TEXT,370,50,150,20,"WPN SUMMARY");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,360,50,10,110,"");
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,520,50,10,110,"");
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
  obj = fl_add_text(FL_NORMAL_TEXT,530,50,280,20,"MISSION SUMMARY");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,810,50,10,110,"");
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,10,190,970,10,"");
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,130,160,850,10,"");
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
  obj = fl_add_text(FL_NORMAL_TEXT,10,10,160,20,"DEVELOPMENT");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->plan_val = obj = fl_add_text(FL_NORMAL_TEXT,260,130,100,20,"Assured");
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,nothingCB,0);
  fdui->rp_val = obj = fl_add_text(FL_NORMAL_TEXT,210,90,20,20,"1");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,nothingCB,0);
  fdui->dea_val = obj = fl_add_text(FL_NORMAL_TEXT,190,110,50,20,"Granted");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,nothingCB,0);
  fdui->roe_val = obj = fl_add_text(FL_NORMAL_TEXT,190,130,70,20,"Defend US");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_checkbutton(FL_PUSH_BUTTON,130,70,80,20,"DEFCON:");
    fl_set_object_color(obj,FL_RED,FL_YELLOW);
  obj = fl_add_checkbutton(FL_PUSH_BUTTON,130,90,80,20,"RP:");
    fl_set_object_color(obj,FL_RED,FL_YELLOW);
  obj = fl_add_checkbutton(FL_PUSH_BUTTON,130,110,60,20,"DEA:");
    fl_set_object_color(obj,FL_RED,FL_YELLOW);
  obj = fl_add_checkbutton(FL_PUSH_BUTTON,130,130,60,20,"ROE:");
    fl_set_object_color(obj,FL_RED,FL_YELLOW);
  obj = fl_add_checkbutton(FL_PUSH_BUTTON,250,70,110,20,"Msn Constraints");
    fl_set_object_color(obj,FL_RED,FL_YELLOW);
  obj = fl_add_checkbutton(FL_PUSH_BUTTON,250,90,110,20,"Msn Obj: Primary");
    fl_set_object_color(obj,FL_RED,FL_YELLOW);
  obj = fl_add_checkbutton(FL_PUSH_BUTTON,250,110,110,20,"Execution Plan:");
    fl_set_object_color(obj,FL_RED,FL_YELLOW);
  obj = fl_add_text(FL_NORMAL_TEXT,10,140,110,20,"Combined NMD");
    fl_set_object_boxtype(obj,FL_UP_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_text(FL_NORMAL_TEXT,10,160,110,20,"Combined OPSCAP");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,nothingCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,10,100,110,20,"SYSCAP");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_checkbutton(FL_PUSH_BUTTON,10,70,110,20,"Alarm Text");
    fl_set_object_color(obj,FL_RED,FL_YELLOW);
  obj = fl_add_button(FL_NORMAL_BUTTON,10,50,110,20,"");
    fl_set_object_color(obj,FL_TOP_BCOL,FL_COL1);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,10,90,110,10,"");
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,10,120,110,10,"");
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,10,240,110,10,"");
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
  fdui->wpn_chart = obj = fl_add_chart(FL_PIE_CHART,370,70,150,90,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_callback(obj,wpnchartCB,0);
  fdui->track_menu = obj = fl_add_menu(FL_PULLDOWN_MENU,140,170,140,20,"TRACK/ENGAGEMENT");
    fl_set_object_callback(obj,trackmenuCB,0);
  fdui->option_menu = obj = fl_add_menu(FL_PULLDOWN_MENU,300,170,140,20,"DEFENSE OPTIONS");
    fl_set_object_callback(obj,optionmenuCB,0);
  obj = fl_add_menu(FL_PULLDOWN_MENU,440,170,110,20,"RESOURCES");
  obj = fl_add_menu(FL_PULLDOWN_MENU,560,170,120,20,"ESI");
  obj = fl_add_menu(FL_PULLDOWN_MENU,710,170,110,20,"UTILITIES");
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,10,470,110,10,"");
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
  obj = fl_add_text(FL_NORMAL_TEXT,690,80,100,20,"# of Leakers:");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,cbpnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,690,100,100,20,"Tgts in Trk:");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,cbpnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,690,120,100,20,"Tgts Eng:");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,cbpnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,690,140,100,20,"Tgts Not Eng:");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,cbpnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,530,80,100,20,"Tot Exp RVs:");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,cbpnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,530,100,100,20,"Tot Tgts Exp:");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,cbpnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,530,120,100,20,"Tgts Killed:");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,cbpnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,530,140,100,20,"Cur Tgts Exp:");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,cbpnoneCB,0);
  fdui->sum_tot_rvs = obj = fl_add_text(FL_NORMAL_TEXT,630,80,40,20,"194");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,missummaryCB,0);
  fdui->sum_tot_tgt = obj = fl_add_text(FL_NORMAL_TEXT,630,100,40,20,"194");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,missummaryCB,1);
  fdui->sum_tgt_kill = obj = fl_add_text(FL_NORMAL_TEXT,630,120,40,20,"194");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,missummaryCB,2);
  fdui->sum_cur_tgt = obj = fl_add_text(FL_NORMAL_TEXT,630,140,40,20,"194");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,missummaryCB,3);
  fdui->sum_leakers = obj = fl_add_text(FL_NORMAL_TEXT,780,80,40,20,"194");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,missummaryCB,4);
  fdui->sum_tgt_trk = obj = fl_add_text(FL_NORMAL_TEXT,780,100,40,20,"194");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,missummaryCB,5);
  fdui->sum_tgt_eng = obj = fl_add_text(FL_NORMAL_TEXT,780,120,40,20,"194");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,missummaryCB,6);
  fdui->sum_tgt_not = obj = fl_add_text(FL_NORMAL_TEXT,780,140,40,20,"194");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,missummaryCB,7);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,10,400,110,10,"");
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
  fdui->act_windows = obj = fl_add_browser(FL_NORMAL_BROWSER,10,500,110,160,"Active Windows");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,actwindowsCB,0);
    fl_add_browser_line(obj, "Browser line 1");
    fl_add_browser_line(obj, "Browser line 2");
    fl_add_browser_line(obj, "Browser line 3");
    fl_add_browser_line(obj, "Browser line 4");
    fl_add_browser_line(obj, "Browser line 5");
    fl_add_browser_line(obj, "Browser line 6");
    fl_add_browser_line(obj, "Browser line 7");
    fl_add_browser_line(obj, "Browser line 8");
    fl_add_browser_line(obj, "Browser line 9");
    fl_add_browser_line(obj, "Browser line 10");
    fl_add_browser_line(obj, "Browser line 11");
    fl_add_browser_line(obj, "Browser line 12");
    fl_add_browser_line(obj, "Browser line 13");
    fl_add_browser_line(obj, "Browser line 14");
    fl_add_browser_line(obj, "Browser line 15");
    fl_add_browser_line(obj, "Browser line 16");
    fl_add_browser_line(obj, "Browser line 17");
    fl_add_browser_line(obj, "Browser line 18");
    fl_add_browser_line(obj, "Browser line 19");
    fl_add_browser_line(obj, "Browser line 20");
    fl_add_browser_line(obj, "Browser line 21");
    fl_add_browser_line(obj, "Browser line 22");
    fl_add_browser_line(obj, "Browser line 23");
    fl_add_browser_line(obj, "Browser line 24");
    fl_add_browser_line(obj, "Browser line 25");
    fl_add_browser_line(obj, "Browser line 26");
    fl_add_browser_line(obj, "Browser line 27");
    fl_add_browser_line(obj, "Browser line 28");
    fl_add_browser_line(obj, "Browser line 29");
    fl_add_browser_line(obj, "Browser line 30");
  fdui->fogofwar = obj = fl_add_button(FL_NORMAL_BUTTON,20,380,90,20,"Fog-of-War");
    fl_set_object_callback(obj,warfogCB,0);
  fl_end_form();

  fdui->bmc3->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_login *create_form_login(void)
{
  FL_OBJECT *obj;
  FD_login *fdui = (FD_login *) fl_calloc(1, sizeof(*fdui));

  fdui->login = fl_bgn_form(FL_NO_BOX, 422, 331);
  obj = fl_add_box(FL_UP_BOX,0,0,422,331,"");
  fdui->username = obj = fl_add_input(FL_NORMAL_INPUT,150,200,180,20,"User ID:");
    fl_set_object_lcolor(obj,FL_RED);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE);
    fl_set_object_callback(obj,loginCB,1);
  fdui->password = obj = fl_add_input(FL_NORMAL_INPUT,150,230,180,20,"Password:");
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
    fl_set_object_callback(obj,exitCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,20,30,380,30,"Strategic Offense/Defense Integration");
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLDITALIC_STYLE);
  obj = fl_add_text(FL_NORMAL_TEXT,90,130,220,30,"BMC3 System");
    fl_set_object_lsize(obj,FL_HUGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_SHADOW_STYLE);
  fl_end_form();

  fdui->login->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_cmocnode *create_form_cmocnode(void)
{
  FL_OBJECT *obj;
  FD_cmocnode *fdui = (FD_cmocnode *) fl_calloc(1, sizeof(*fdui));

  fdui->cmocnode = fl_bgn_form(FL_NO_BOX, 482, 351);
  obj = fl_add_box(FL_UP_BOX,0,0,482,351,"");
  obj = fl_add_text(FL_NORMAL_TEXT,10,30,460,20,"Select desired Command Center and position from list below");
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj = fl_add_button(FL_NORMAL_BUTTON,130,310,70,30,"OK");
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,positionCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,300,310,70,30,"Cancel");
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,exitCB,0);

  fdui->positions = fl_bgn_group();
  fdui->position[1] = obj = fl_add_checkbutton(FL_RADIO_BUTTON,230,70,180,20,"CINC/CD");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,positionCB,1);
  fdui->position[2] = obj = fl_add_checkbutton(FL_RADIO_BUTTON,230,90,180,20,"Msl Defense 1");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,positionCB,2);
  fdui->position[3] = obj = fl_add_checkbutton(FL_RADIO_BUTTON,230,110,180,20,"Msl Defense 2");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,positionCB,3);
  fdui->position[5] = obj = fl_add_checkbutton(FL_RADIO_BUTTON,230,180,180,20,"Cmdr/Director");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,positionCB,5);
  fdui->position[6] = obj = fl_add_checkbutton(FL_RADIO_BUTTON,230,200,180,20,"Battle Intel Analyst");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,positionCB,6);
  fdui->position[7] = obj = fl_add_checkbutton(FL_RADIO_BUTTON,230,220,180,20,"Weapons Analyst");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,positionCB,7);
  fdui->position[8] = obj = fl_add_checkbutton(FL_RADIO_BUTTON,230,240,180,20,"Sensor Analyst");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,positionCB,8);
  fl_end_group();


  fdui->centers = fl_bgn_group();
  fdui->position[10] = obj = fl_add_checkbutton(FL_RADIO_BUTTON,50,90,140,20,"NORAD CCC");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,positionCB,10);
  fdui->position[11] = obj = fl_add_checkbutton(FL_RADIO_BUTTON,50,180,140,20,"BMDC");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,positionCB,11);
  fdui->position[12] = obj = fl_add_checkbutton(FL_RADIO_BUTTON,50,210,140,20,"SCCC");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,positionCB,12);
  fdui->position[13] = obj = fl_add_checkbutton(FL_RADIO_BUTTON,50,240,140,20,"Firing Unit");
    fl_set_object_lcolor(obj,FL_GREEN);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,positionCB,13);
  fl_end_group();

  fl_end_form();

  fdui->cmocnode->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

