/* Form definition file generated with fdesign. */

#include "forms.h"
#include <stdlib.h>
#include "ODIforms.h"

FD_ODI *create_form_ODI(void)
{
  FL_OBJECT *obj;
  FD_ODI *fdui = (FD_ODI *) fl_calloc(1, sizeof(*fdui));

  fdui->ODI = fl_bgn_form(FL_NO_BOX, 940, 660);
  obj = fl_add_box(FL_UP_BOX,0,0,940,660,"");
  obj = fl_add_button(FL_NORMAL_BUTTON,790,500,140,60,"Data Mining");
    fl_set_object_boxtype(obj,FL_EMBOSSED_BOX);
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,datamineCB,0);
  fdui->background = obj = fl_add_pixmap(FL_NORMAL_PIXMAP,160,55,310,240,"Strategic & Missile Defense");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,ODInoneCB,0);
    fl_set_pixmap_file(obj, "/home/dre/Sodi/BitMaps/NMDback.xpm");
  fdui->battle_button = obj = fl_add_button(FL_NORMAL_BUTTON,10,260,140,60,"NMD/SDF\nBMC3");
    fl_set_object_boxtype(obj,FL_EMBOSSED_BOX);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,battleCB,0);
  fdui->quit_button = obj = fl_add_button(FL_NORMAL_BUTTON,395,610,150,40,"Quit");
    fl_set_object_boxtype(obj,FL_EMBOSSED_BOX);
    fl_set_object_color(obj,FL_DARKTOMATO,FL_COL1);
    fl_set_object_lcolor(obj,FL_WHEAT);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,ODIexitCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,10,320,140,60,"Theater\nFiring Unit");
    fl_set_object_boxtype(obj,FL_EMBOSSED_BOX);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,patriotCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,790,560,140,60,"Terrain-based\nPlanner");
    fl_set_object_boxtype(obj,FL_EMBOSSED_BOX);
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,reasonerCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,10,200,140,60,"Aerospace\nOperations");
    fl_set_object_boxtype(obj,FL_EMBOSSED_BOX);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,battleCB,1);
  obj = fl_add_button(FL_NORMAL_BUTTON,790,440,140,60,"Engagement\nPlanner");
    fl_set_object_boxtype(obj,FL_EMBOSSED_BOX);
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,engageCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,790,140,140,60,"UAV\nFlythru");
    fl_set_object_boxtype(obj,FL_EMBOSSED_BOX);
    fl_set_object_lcolor(obj,FL_RED);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,predatorCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,790,390,140,50,"Mission\nPlanning");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_color(obj,FL_CYAN,FL_MCOL);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_SHADOW_STYLE);
  obj = fl_add_text(FL_NORMAL_TEXT,10,30,140,50,"Mission\nAreas");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_color(obj,FL_YELLOW,FL_MCOL);
    fl_set_object_lcolor(obj,FL_DODGERBLUE);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_SHADOW_STYLE);
  obj = fl_add_text(FL_NORMAL_TEXT,790,30,140,50,"Mission\nPreview");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_color(obj,FL_RED,FL_MCOL);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_SHADOW_STYLE);
  obj = fl_add_text(FL_NORMAL_TEXT,10,450,140,50,"Decision\nAids");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_color(obj,FL_BLUE,FL_MCOL);
    fl_set_object_lcolor(obj,FL_WHITE);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_SHADOW_STYLE);
    fl_set_object_callback(obj,ODInoneCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,790,200,140,60,"Configuration\nEditor");
    fl_set_object_boxtype(obj,FL_EMBOSSED_BOX);
    fl_set_object_lcolor(obj,FL_RED);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,configedCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,790,260,140,60,"Simulator");
    fl_set_object_boxtype(obj,FL_EMBOSSED_BOX);
    fl_set_object_lcolor(obj,FL_RED);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,plannerCB,0);
  fdui->visual_button = obj = fl_add_button(FL_NORMAL_BUTTON,790,80,140,60,"Fused Ops\nPicture");
    fl_set_object_boxtype(obj,FL_EMBOSSED_BOX);
    fl_set_object_lcolor(obj,FL_RED);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,visualCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,10,500,140,60,"Fusion Engine");
    fl_set_object_boxtype(obj,FL_EMBOSSED_BOX);
    fl_set_object_lcolor(obj,FL_DARKVIOLET);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,fusionCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,10,560,140,60,"Plan Optimization");
    fl_set_object_boxtype(obj,FL_EMBOSSED_BOX);
    fl_set_object_lcolor(obj,FL_DARKVIOLET);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,fusionCB,1);
  obj = fl_add_button(FL_NORMAL_BUTTON,10,140,140,60,"DoH & Anti-\nTerrorism");
    fl_set_object_boxtype(obj,FL_EMBOSSED_BOX);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,battleCB,3);
  obj = fl_add_button(FL_NORMAL_BUTTON,915,545,10,10,"?");
    fl_set_object_callback(obj,splashhelpCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,150,10,640,40,"General Purpose Decision Aids (GPDA) Demo System");
    fl_set_object_lsize(obj,FL_HUGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_SHADOW_STYLE);
    fl_set_object_callback(obj,ODInoneCB,0);
  obj = fl_add_pixmap(FL_NORMAL_PIXMAP,470,55,310,240,"DoH/Anti-Terrorism");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,ODInoneCB,0);
    fl_set_pixmap_file(obj, "/home/dre/Sodi/BitMaps/TheaterBack.xpm");
  obj = fl_add_pixmap(FL_NORMAL_PIXMAP,160,330,310,240,"Air Operations");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,ODInoneCB,0);
    fl_set_pixmap_file(obj, "/home/dre/Sodi/BitMaps/AOCback.xpm");
  obj = fl_add_pixmap(FL_NORMAL_PIXMAP,470,330,310,240,"Information Warfare");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,ODInoneCB,0);
    fl_set_pixmap_file(obj, "/home/dre/Sodi/BitMaps/IOWback.xpm");
  obj = fl_add_clock(FL_DIGITAL_CLOCK,210,622,120,30,"");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_color(obj,FL_LEFT_BCOL,FL_BLUE);
    fl_set_object_lcolor(obj,FL_WHITE);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lstyle(obj,FL_NORMAL_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,ODInoneCB,0);
  obj = fl_add_box(FL_UP_BOX,550,610,240,10,"");
  obj = fl_add_box(FL_UP_BOX,150,610,240,10,"");
  obj = fl_add_button(FL_NORMAL_BUTTON,10,80,140,60,"Information\nWarfare");
    fl_set_object_boxtype(obj,FL_EMBOSSED_BOX);
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_ENGRAVED_STYLE);
    fl_set_object_callback(obj,battleCB,2);
  fl_end_form();

  fdui->ODI->fdui = fdui;

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

FD_help *create_form_help(void)
{
  FL_OBJECT *obj;
  FD_help *fdui = (FD_help *) fl_calloc(1, sizeof(*fdui));

  fdui->help = fl_bgn_form(FL_NO_BOX, 420, 380);
  obj = fl_add_box(FL_UP_BOX,0,0,420,380,"");
  obj = fl_add_button(FL_NORMAL_BUTTON,160,330,100,30,"Done");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,helpdoneCB,0);
  fdui->helptext = obj = fl_add_browser(FL_NORMAL_BROWSER,10,20,400,290,"");
    fl_set_object_callback(obj,ODInoneCB,0);
  fl_end_form();

  fdui->help->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

