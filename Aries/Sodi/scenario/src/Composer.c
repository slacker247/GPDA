/* Form definition file generated with fdesign. */

#include "forms.h"
#include <stdlib.h>
#include "Composer.h"

FD_Map *create_form_Map(void)
{
  FL_OBJECT *obj;
  FD_Map *fdui = (FD_Map *) fl_calloc(1, sizeof(*fdui));

  fdui->Map = fl_bgn_form(FL_NO_BOX, 820, 690);
  obj = fl_add_box(FL_UP_BOX,0,0,820,690,"");
  fdui->canvas = obj = fl_add_glcanvas(FL_NORMAL_CANVAS,10,10,800,480,"");
    fl_set_object_color(obj,FL_COL1,FL_COL1);
    fl_set_object_callback(obj,canvasCB,0);
  fdui->threats = obj = fl_add_box(FL_DOWN_BOX,420,540,310,140,"Threats");
    fl_set_object_color(obj,FL_TOMATO,FL_COL1);
    fl_set_object_lcolor(obj,FL_RED);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->Assets = obj = fl_add_box(FL_DOWN_BOX,10,540,400,140,"Assets");
    fl_set_object_color(obj,FL_DODGERBLUE,FL_COL1);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->hawk = obj = fl_add_lightbutton(FL_PUSH_BUTTON,20,610,80,20,"HAWK");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,hawkCB,0);
  fdui->crc = obj = fl_add_lightbutton(FL_PUSH_BUTTON,120,550,80,20,"CRC");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,crcCB,0);
  fdui->aegis = obj = fl_add_lightbutton(FL_PUSH_BUTTON,20,550,80,20,"AEGIS");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,aegisCB,0);
  fdui->patriot = obj = fl_add_lightbutton(FL_PUSH_BUTTON,20,570,80,20,"PATRIOT");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,patriotCB,0);
  fdui->gbi = obj = fl_add_lightbutton(FL_PUSH_BUTTON,220,550,80,20,"GBI");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gbiCB,0);
  fdui->dsp = obj = fl_add_lightbutton(FL_PUSH_BUTTON,120,650,80,20,"DSP");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,dspCB,0);
  fdui->sbirs = obj = fl_add_lightbutton(FL_PUSH_BUTTON,120,630,80,20,"SBIRS");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,sbirsCB,0);
  fdui->gbr = obj = fl_add_lightbutton(FL_PUSH_BUTTON,120,590,80,20,"GBR");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gbrCB,0);
  fdui->thaad = obj = fl_add_lightbutton(FL_PUSH_BUTTON,20,590,80,20,"THAAD");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,thaadCB,0);
  fdui->awacs = obj = fl_add_lightbutton(FL_PUSH_BUTTON,120,610,80,20,"AWACS");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,awacsCB,0);
  fdui->tactical = obj = fl_add_lightbutton(FL_PUSH_BUTTON,20,630,80,20,"Tactical");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,tacticalCB,0);
  fdui->cruise = obj = fl_add_lightbutton(FL_PUSH_BUTTON,430,650,80,20,"Cruise");
    fl_set_object_color(obj,FL_COL1,FL_GREEN);
    fl_set_object_lcolor(obj,FL_WHITE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,cruiseCB,0);
  fdui->armynavy = obj = fl_add_lightbutton(FL_PUSH_BUTTON,20,650,80,20,"Forces");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,armynavyCB,0);
  fdui->foreignair = obj = fl_add_lightbutton(FL_PUSH_BUTTON,430,630,80,20,"Tactical");
    fl_set_object_color(obj,FL_COL1,FL_GREEN);
    fl_set_object_lcolor(obj,FL_WHITE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,tacticalCB,0);
  fdui->scud = obj = fl_add_lightbutton(FL_PUSH_BUTTON,430,550,80,20,"Scud");
    fl_set_object_color(obj,FL_COL1,FL_GREEN);
    fl_set_object_lcolor(obj,FL_WHITE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,tacticalCB,0);
  fdui->nodong = obj = fl_add_lightbutton(FL_PUSH_BUTTON,430,570,80,20,"NoDong");
    fl_set_object_color(obj,FL_COL1,FL_GREEN);
    fl_set_object_lcolor(obj,FL_WHITE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,tacticalCB,0);
  fdui->m9 = obj = fl_add_lightbutton(FL_PUSH_BUTTON,430,590,80,20,"M-9");
    fl_set_object_color(obj,FL_COL1,FL_GREEN);
    fl_set_object_lcolor(obj,FL_WHITE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,tacticalCB,0);
  fdui->ss18 = obj = fl_add_lightbutton(FL_PUSH_BUTTON,530,550,80,20,"SS-18");
    fl_set_object_color(obj,FL_COL1,FL_GREEN);
    fl_set_object_lcolor(obj,FL_WHITE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,tacticalCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,340,520,70,20,"Save");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,maindoneCB,1);
  obj = fl_add_button(FL_NORMAL_BUTTON,420,520,70,20,"Load");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,maindoneCB,2);
  obj = fl_add_button(FL_NORMAL_BUTTON,660,520,70,20,"Save");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,maindoneCB,1);
  obj = fl_add_lightbutton(FL_PUSH_BUTTON,220,610,80,20,"ICBM");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gbiCB,0);
  obj = fl_add_lightbutton(FL_PUSH_BUTTON,220,630,80,20,"SLBM");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gbiCB,0);
  obj = fl_add_lightbutton(FL_PUSH_BUTTON,220,650,80,20,"Bomber");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gbiCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,10,520,70,20,"Load");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,maindoneCB,2);
  fdui->jtags = obj = fl_add_lightbutton(FL_PUSH_BUTTON,120,570,80,20,"JTAGS");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,jtagsCB,0);
  fdui->maindone = obj = fl_add_button(FL_NORMAL_BUTTON,740,540,70,30,"Done");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,maindoneCB,0);
  fl_end_form();

  fdui->Map->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_select_change *create_form_select_change(void)
{
  FL_OBJECT *obj;
  FD_select_change *fdui = (FD_select_change *) fl_calloc(1, sizeof(*fdui));

  fdui->select_change = fl_bgn_form(FL_NO_BOX, 530, 440);
  obj = fl_add_box(FL_UP_BOX,0,0,530,440,"");
  obj = fl_add_text(FL_NORMAL_TEXT,50,110,50,20,"0 - 100");
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT|FL_ALIGN_INSIDE);
  obj = fl_add_text(FL_NORMAL_TEXT,330,140,50,20,"G, Y, R");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
  obj = fl_add_text(FL_NORMAL_TEXT,330,110,50,20,"0.1 - 1.0");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
  fdui->location = obj = fl_add_button(FL_NORMAL_BUTTON,260,190,130,30,"Location");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,locationCB,0);
  fdui->north = obj = fl_add_text(FL_NORMAL_TEXT,240,260,40,30,"North");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->northtext = obj = fl_add_text(FL_NORMAL_TEXT,290,260,20,30,"T");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
  fdui->east = obj = fl_add_text(FL_NORMAL_TEXT,340,260,40,30,"East");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->easttext = obj = fl_add_text(FL_NORMAL_TEXT,390,260,20,30,"T");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
  fdui->latdeg = obj = fl_add_text(FL_NORMAL_TEXT,170,290,100,20,"Degrees Latitude");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->latdegtext = obj = fl_add_text(FL_NORMAL_TEXT,280,290,40,20,"Text");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
  fdui->latmin = obj = fl_add_text(FL_NORMAL_TEXT,170,310,100,20,"Minutes Latitude");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->latmintext = obj = fl_add_text(FL_NORMAL_TEXT,280,310,40,20,"Text");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
  fdui->latsec = obj = fl_add_text(FL_NORMAL_TEXT,170,330,100,20,"Seconds Latitude");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->latsectext = obj = fl_add_text(FL_NORMAL_TEXT,280,330,40,20,"Text");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
  fdui->londeg = obj = fl_add_text(FL_NORMAL_TEXT,350,290,100,20,"Degrees Longitude");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->lonmin = obj = fl_add_text(FL_NORMAL_TEXT,350,310,100,20,"Minutes Longitude");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->lonsec = obj = fl_add_text(FL_NORMAL_TEXT,350,330,100,20,"Seconds Longitude");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->londegtext = obj = fl_add_text(FL_NORMAL_TEXT,470,290,40,20,"Text");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
  fdui->lonmintext = obj = fl_add_text(FL_NORMAL_TEXT,470,310,40,20,"Text");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
  fdui->lonsectext = obj = fl_add_text(FL_NORMAL_TEXT,470,330,40,20,"Text");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
  fdui->savedata = obj = fl_add_button(FL_NORMAL_BUTTON,120,390,110,30,"Done");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,saveCB,0);
  fdui->cancel = obj = fl_add_button(FL_NORMAL_BUTTON,270,390,110,30,"Cancel");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,cancelCB,0);
  fdui->assetname = obj = fl_add_input(FL_NORMAL_INPUT,130,70,190,20,"Enter New Asset Name:");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,assetnameCB,0);
  fdui->assetid = obj = fl_add_input(FL_INT_INPUT,130,100,100,20,"Enter New Asset ID:");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,assetidCB,0);
  fdui->unit = obj = fl_add_lightbutton(FL_PUSH_BUTTON,30,300,110,30,"Unit T/F");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,unitCB,0);
  fdui->defense = obj = fl_add_lightbutton(FL_PUSH_BUTTON,30,260,110,30,"Defense T/F");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,defenseCB,0);
  fdui->weapons = obj = fl_add_lightbutton(FL_PUSH_BUTTON,30,220,110,30,"Weapons T/F");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,weaponsCB,0);
  fdui->coverage = obj = fl_add_lightbutton(FL_PUSH_BUTTON,30,180,110,30,"Coverage T/F");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,coverageCB,0);
  fdui->sensor = obj = fl_add_lightbutton(FL_PUSH_BUTTON,30,140,110,30,"Sensor T/F");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,sensorCB,0);
  fdui->colorasset = obj = fl_add_button(FL_NORMAL_BUTTON,30,340,110,30,"Color");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,colorassetCB,0);
  fdui->scale = obj = fl_add_input(FL_FLOAT_INPUT,380,100,70,20,"Scale");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,scaleCB,0);
  fdui->status = obj = fl_add_input(FL_NORMAL_INPUT,380,130,70,20,"Status");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,statusCB,0);
  fdui->altitude = obj = fl_add_input(FL_FLOAT_INPUT,310,240,80,20,"Altitude");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,altitudeCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,330,80,50,20,"0 - 400");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
  fdui->icon = obj = fl_add_input(FL_INT_INPUT,380,70,70,20,"Icon");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,iconCB,0);
  fdui->instructions = obj = fl_add_text(FL_NORMAL_TEXT,40,10,440,40,"Text");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
  fl_end_form();

  fdui->select_change->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_location *create_form_location(void)
{
  FL_OBJECT *obj;
  FD_location *fdui = (FD_location *) fl_calloc(1, sizeof(*fdui));

  fdui->location = fl_bgn_form(FL_NO_BOX, 400, 280);
  obj = fl_add_box(FL_UP_BOX,0,0,400,280,"");
  obj = fl_add_text(FL_NORMAL_TEXT,240,70,40,20,"T or F");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
  obj = fl_add_text(FL_NORMAL_TEXT,130,70,40,20,"T or F");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
  fdui->deglatchange = obj = fl_add_input(FL_FLOAT_INPUT,100,100,70,20,"Degrees Latitude");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,deglatchangeCB,0);
  fdui->minlatchange = obj = fl_add_input(FL_FLOAT_INPUT,100,130,70,20,"Minutes Latitude");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,minlatchangeCB,0);
  fdui->seclatchange = obj = fl_add_input(FL_NORMAL_INPUT,100,160,70,20,"Seconds Latitude");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,seclatchangeCB,0);
  fdui->seclonchange = obj = fl_add_input(FL_NORMAL_INPUT,300,160,70,20,"Seconds Longitude");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,seclonchangeCB,0);
  fdui->deglonchange = obj = fl_add_input(FL_FLOAT_INPUT,300,100,70,20,"Degrees Longitude");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,deglonchangeCB,0);
  fdui->minlonchange = obj = fl_add_input(FL_NORMAL_INPUT,300,130,70,20,"Minutes Longitude");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,minlonchangeCB,0);
  fdui->donelocation = obj = fl_add_button(FL_NORMAL_BUTTON,280,200,90,30,"Done");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,donelocationCB,0);
  fdui->locationtext = obj = fl_add_text(FL_NORMAL_TEXT,40,10,320,20,"Change the values as appropriate below");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->northdirection = obj = fl_add_input(FL_NORMAL_INPUT,120,50,70,20,"North");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,northdirectionCB,0);
  fdui->eastdirection = obj = fl_add_input(FL_NORMAL_INPUT,230,50,70,20,"East");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,eastdirectionCB,0);
  fl_end_form();

  fdui->location->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_weapons *create_form_weapons(void)
{
  FL_OBJECT *obj;
  FD_weapons *fdui = (FD_weapons *) fl_calloc(1, sizeof(*fdui));

  fdui->weapons = fl_bgn_form(FL_NO_BOX, 330, 230);
  obj = fl_add_box(FL_UP_BOX,0,0,330,230,"");
  obj = fl_add_text(FL_NORMAL_TEXT,170,100,50,20,"0.0 - 1.0");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
  obj = fl_add_text(FL_NORMAL_TEXT,20,70,50,20,"0 - 100");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
  fdui->nholdchange = obj = fl_add_input(FL_INT_INPUT,70,140,80,20,"N-HOLD");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,nholdchangeCB,0);
  fdui->pkillchange = obj = fl_add_input(FL_FLOAT_INPUT,220,90,80,20,"P-KILL");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,pkillchangeCB,0);
  fdui->ngbichange = obj = fl_add_input(FL_INT_INPUT,70,100,80,20,"N-GBI");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,ngbichangeCB,0);
  fdui->gbiidchange = obj = fl_add_input(FL_INT_INPUT,70,60,80,20,"GBI-ID");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gbiidchangeCB,0);
  fdui->gbitypechange = obj = fl_add_input(FL_NORMAL_INPUT,220,140,80,20,"GBI-TYPE");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gbitypechangeCB,0);
  fdui->weaponschangetext = obj = fl_add_text(FL_NORMAL_TEXT,30,20,280,20,"Enter new weapons values");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fdui->weaponsdone = obj = fl_add_button(FL_NORMAL_BUTTON,200,180,100,30,"Done");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,weaponsdoneCB,0);
  fl_end_form();

  fdui->weapons->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_defense *create_form_defense(void)
{
  FL_OBJECT *obj;
  FD_defense *fdui = (FD_defense *) fl_calloc(1, sizeof(*fdui));

  fdui->defense = fl_bgn_form(FL_NO_BOX, 590, 300);
  obj = fl_add_box(FL_UP_BOX,0,0,590,300,"");
  obj = fl_add_text(FL_NORMAL_TEXT,80,160,40,20,"T or F");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
  obj = fl_add_text(FL_NORMAL_TEXT,80,130,40,20,"T or F");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
  obj = fl_add_text(FL_NORMAL_TEXT,80,100,40,20,"T or F");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
  fdui->donedefense = obj = fl_add_button(FL_NORMAL_BUTTON,460,260,110,30,"Done");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,donedefenseCB,0);
  fdui->arealabel = obj = fl_add_input(FL_NORMAL_INPUT,130,90,80,20,"Area Label");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,arealabelCB,0);
  fdui->areanorth = obj = fl_add_input(FL_NORMAL_INPUT,130,120,80,20,"Area North");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,areanorthCB,0);
  fdui->areaeast = obj = fl_add_input(FL_NORMAL_INPUT,130,150,80,20,"Area East");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,areaeastCB,0);
  fdui->arealatdeg = obj = fl_add_input(FL_FLOAT_INPUT,130,180,80,20,"Area Latitude Degree");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,arealatdegCB,0);
  fdui->arealatmin = obj = fl_add_input(FL_FLOAT_INPUT,130,210,80,20,"Area Latitude Minutes");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,arealatminCB,0);
  fdui->arealatsec = obj = fl_add_input(FL_FLOAT_INPUT,340,90,80,20,"Area Latitude Seconds");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,arealatsecCB,0);
  fdui->arealondeg = obj = fl_add_input(FL_FLOAT_INPUT,340,120,80,20,"Area Longitude Degree");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,arealongdegCB,0);
  fdui->arealonmin = obj = fl_add_input(FL_FLOAT_INPUT,340,150,80,20,"Area Longitude Minutes");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,arealonminCB,0);
  fdui->arealonsec = obj = fl_add_input(FL_FLOAT_INPUT,340,180,80,20,"Area Longitude Seconds");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,arealonsecCB,0);
  fdui->areamajor = obj = fl_add_input(FL_FLOAT_INPUT,490,90,80,20,"Area Major");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,areamajorCB,0);
  fdui->areaminor = obj = fl_add_input(FL_FLOAT_INPUT,490,120,80,20,"Area Minor");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,areaminorCB,0);
  fdui->areaorient = obj = fl_add_input(FL_FLOAT_INPUT,490,150,80,20,"Area Orient");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,areaorientCB,0);
  fdui->color = obj = fl_add_button(FL_NORMAL_BUTTON,490,210,80,20,"Color");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,colorCB,0);
  fdui->areafile = obj = fl_add_input(FL_NORMAL_INPUT,490,180,80,20,"Area File");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,areafileCB,0);
  fdui->areaname = obj = fl_add_input(FL_NORMAL_INPUT,150,20,120,20,"Enter Defense Area Name");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,areanameCB,0);
  fdui->defensetextchange = obj = fl_add_text(FL_NORMAL_TEXT,340,20,190,20,"Make changes below as necessary");
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
  fl_end_form();

  fdui->defense->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_sensors *create_form_sensors(void)
{
  FL_OBJECT *obj;
  FD_sensors *fdui = (FD_sensors *) fl_calloc(1, sizeof(*fdui));

  fdui->sensors = fl_bgn_form(FL_NO_BOX, 480, 280);
  obj = fl_add_box(FL_UP_BOX,0,0,480,280,"");
  obj = fl_add_text(FL_NORMAL_TEXT,330,130,40,20,"T or F");
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT|FL_ALIGN_INSIDE);
  obj = fl_add_text(FL_NORMAL_TEXT,330,100,40,20,"T or F");
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT|FL_ALIGN_INSIDE);
  fdui->sensortypechange = obj = fl_add_input(FL_INT_INPUT,80,120,90,20,"sensor type");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,sensortypechangeCB,0);
  fdui->scantimechange = obj = fl_add_input(FL_FLOAT_INPUT,80,150,90,20,"scan time");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,scantimechangeCB,0);
  fdui->rminchange = obj = fl_add_input(FL_FLOAT_INPUT,80,180,90,20,"rmin");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,rminchangeCB,0);
  fdui->rmaxchange = obj = fl_add_input(FL_FLOAT_INPUT,80,210,90,20,"rmax");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,rmaxchangeCB,0);
  fdui->rmaxlowchange = obj = fl_add_input(FL_FLOAT_INPUT,240,60,90,20,"rmax low");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,rmaxlowchangeCB,0);
  fdui->rdotminchange = obj = fl_add_input(FL_FLOAT_INPUT,240,90,90,20,"rdot min");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,rdotminchangeCB,0);
  fdui->signalchange = obj = fl_add_input(FL_FLOAT_INPUT,240,120,90,20,"signal");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,signalchangeCB,0);
  fdui->luminositychange = obj = fl_add_input(FL_FLOAT_INPUT,240,150,90,20,"luminosity");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,luminositychangeCB,0);
  fdui->elevationchange = obj = fl_add_input(FL_FLOAT_INPUT,80,50,90,20,"elevation");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,elevationchangeCB,0);
  fdui->azimuthchange = obj = fl_add_input(FL_FLOAT_INPUT,80,80,90,20,"azimuth");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,azimuthchangeCB,0);
  fdui->fovhighchange = obj = fl_add_input(FL_FLOAT_INPUT,240,180,90,20,"fov high");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,fovhighchange,0);
  fdui->fovlowchange = obj = fl_add_input(FL_FLOAT_INPUT,240,210,90,20,"fov low");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,fovlowchangeCB,0);
  fdui->fixedchange = obj = fl_add_input(FL_NORMAL_INPUT,370,90,90,20,"fixed");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,fixedchangeCB,0);
  fdui->loschange = obj = fl_add_input(FL_NORMAL_INPUT,370,120,90,20,"los");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,loschangeCB,0);
  fdui->errorchange = obj = fl_add_input(FL_FLOAT_INPUT,370,150,90,20,"error");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,errorchangeCB,0);
  fdui->iconchange = obj = fl_add_input(FL_INT_INPUT,370,180,90,20,"icon");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,iconchangeCB,0);
  fdui->scalechange = obj = fl_add_input(FL_FLOAT_INPUT,370,210,90,20,"scale");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,scalechangeCB,0);
  fdui->donesensor = obj = fl_add_button(FL_NORMAL_BUTTON,370,240,90,30,"Done");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,donesensorCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,80,70,10,10,"Text");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
  fdui->sensorchangetext = obj = fl_add_text(FL_NORMAL_TEXT,80,10,340,20,"Make Sensor Changes if Necessary");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fl_end_form();

  fdui->sensors->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_color *create_form_color(void)
{
  FL_OBJECT *obj;
  FD_color *fdui = (FD_color *) fl_calloc(1, sizeof(*fdui));

  fdui->color = fl_bgn_form(FL_NO_BOX, 290, 170);
  obj = fl_add_box(FL_UP_BOX,0,0,290,170,"");
  fdui->black = obj = fl_add_button(FL_NORMAL_BUTTON,220,130,50,20,"");
    fl_set_object_color(obj,FL_BLACK,FL_COL1);
    fl_set_object_lstyle(obj,FL_NORMAL_STYLE+FL_SHADOW_STYLE);
    fl_set_object_resize(obj, FL_RESIZE_NONE);
    fl_set_object_callback(obj,blackCB,0);
  fdui->red = obj = fl_add_button(FL_NORMAL_BUTTON,20,70,50,20,"");
    fl_set_object_color(obj,FL_RED,FL_COL1);
    fl_set_object_callback(obj,redCB,0);
  fdui->green = obj = fl_add_button(FL_NORMAL_BUTTON,80,70,50,20,"");
    fl_set_object_color(obj,FL_GREEN,FL_COL1);
    fl_set_object_callback(obj,greenCB,0);
  fdui->yellow = obj = fl_add_button(FL_NORMAL_BUTTON,20,130,50,20,"");
    fl_set_object_color(obj,FL_YELLOW,FL_COL1);
    fl_set_object_callback(obj,yellowCB,0);
  fdui->blue = obj = fl_add_button(FL_NORMAL_BUTTON,80,100,50,20,"");
    fl_set_object_color(obj,FL_BLUE,FL_COL1);
    fl_set_object_callback(obj,blueCB,0);
  fdui->pink = obj = fl_add_button(FL_NORMAL_BUTTON,150,100,50,20,"");
    fl_set_object_color(obj,FL_MAGENTA,FL_COL1);
    fl_set_object_callback(obj,pinkCB,0);
  fdui->lightblue = obj = fl_add_button(FL_NORMAL_BUTTON,80,130,50,20,"");
    fl_set_object_color(obj,FL_CYAN,FL_COL1);
    fl_set_object_callback(obj,lightblueCB,0);
  fdui->white = obj = fl_add_button(FL_NORMAL_BUTTON,150,130,50,20,"");
    fl_set_object_color(obj,FL_WHITE,FL_COL1);
    fl_set_object_callback(obj,whiteCB,0);
  fdui->orange = obj = fl_add_button(FL_NORMAL_BUTTON,20,100,50,20,"");
    fl_set_object_color(obj,FL_DARKORANGE,FL_COL1);
    fl_set_object_callback(obj,orangeCB,0);
  fdui->purple = obj = fl_add_button(FL_NORMAL_BUTTON,150,70,50,20,"");
    fl_set_object_color(obj,FL_DARKVIOLET,FL_COL1);
    fl_set_object_callback(obj,purpleCB,0);
  fdui->grey = obj = fl_add_button(FL_NORMAL_BUTTON,220,100,50,20,"");
    fl_set_object_color(obj,FL_BOTTOM_BCOL,FL_COL1);
    fl_set_object_callback(obj,greyCB,0);
  fdui->brown = obj = fl_add_button(FL_NORMAL_BUTTON,220,70,50,20,"");
    fl_set_object_color(obj,FL_DARKTOMATO,FL_COL1);
    fl_set_object_callback(obj,brownCB,0);
  fdui->selectcolor = obj = fl_add_text(FL_NORMAL_TEXT,40,20,210,20,"Select Color for Viewing Pleasure");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fl_end_form();

  fdui->color->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_selectsensor *create_form_selectsensor(void)
{
  FL_OBJECT *obj;
  FD_selectsensor *fdui = (FD_selectsensor *) fl_calloc(1, sizeof(*fdui));

  fdui->selectsensor = fl_bgn_form(FL_NO_BOX, 300, 90);
  obj = fl_add_box(FL_UP_BOX,0,0,300,90,"");
  fdui->selectsensortype = obj = fl_add_choice(FL_NORMAL_CHOICE2,130,30,140,30,"Select Sensor Type");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,selectsensortypeCB,0);
  fl_end_form();

  fdui->selectsensor->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_selectdefense *create_form_selectdefense(void)
{
  FL_OBJECT *obj;
  FD_selectdefense *fdui = (FD_selectdefense *) fl_calloc(1, sizeof(*fdui));

  fdui->selectdefense = fl_bgn_form(FL_NO_BOX, 300, 90);
  obj = fl_add_box(FL_UP_BOX,0,0,300,90,"");
  fdui->selectdefensetype = obj = fl_add_choice(FL_NORMAL_CHOICE2,130,30,140,30,"Select Defense Type");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,selectdefensetypeCB,0);
  fl_end_form();

  fdui->selectdefense->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_units *create_form_units(void)
{
  FL_OBJECT *obj;
  FD_units *fdui = (FD_units *) fl_calloc(1, sizeof(*fdui));

  fdui->units = fl_bgn_form(FL_NO_BOX, 370, 190);
  obj = fl_add_box(FL_UP_BOX,0,0,370,190,"");
  fdui->unitname = obj = fl_add_input(FL_NORMAL_INPUT,70,50,150,20,"Unit Name");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,unitnameCB,0);
  fdui->type = obj = fl_add_input(FL_INT_INPUT,250,80,100,20,"Type");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,typeCB,0);
  fdui->size = obj = fl_add_input(FL_FLOAT_INPUT,250,110,100,20,"Size");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,sizeCB,0);
  fdui->unittype = obj = fl_add_choice(FL_NORMAL_CHOICE2,70,100,90,30,"Unit Type");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,unittypeCB,0);
  fdui->unitdone = obj = fl_add_button(FL_NORMAL_BUTTON,250,150,100,30,"Done");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,unitdoneCB,0);
  fdui->unittext = obj = fl_add_text(FL_NORMAL_TEXT,30,10,300,20,"Enter unit information below");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fl_end_form();

  fdui->units->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

