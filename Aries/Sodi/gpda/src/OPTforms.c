/* Form definition file generated with fdesign. */

#include "forms.h"
#include <stdlib.h>
#include "OPTforms.h"

static FL_PUP_ENTRY fdmenu_File_0[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "Exit",	0,	"",	 FL_PUP_NONE},
    {0}
};

FD_gagraph *create_form_gagraph(void)
{
  FL_OBJECT *obj;
  FD_gagraph *fdui = (FD_gagraph *) fl_calloc(1, sizeof(*fdui));

  fdui->gagraph = fl_bgn_form(FL_NO_BOX, 850, 460);
  obj = fl_add_box(FL_UP_BOX,0,0,850,460,"");
  fdui->ga_canvas = obj = fl_add_glcanvas(FL_NORMAL_CANVAS,190,50,650,360,"");
    fl_set_object_boxtype(obj,FL_BORDER_BOX);
    fl_set_object_callback(obj,OPTnoneCB,0);
  fdui->bitmap_stop = obj = fl_add_bitmapbutton(FL_NORMAL_BUTTON,650,420,40,30,"");
    fl_set_object_callback(obj,StopCB,0);
    fl_set_object_helper(obj, "Stop");
    fl_set_bitmapbutton_file(obj, "bitmaps/stop.xbm");
  fdui->bitmap_rew = obj = fl_add_bitmapbutton(FL_NORMAL_BUTTON,600,420,40,30,"");
    fl_set_object_callback(obj,ResetCB,0);
    fl_set_object_helper(obj, "Reset");
    fl_set_bitmapbutton_file(obj, "bitmaps/rew.xbm");
  fdui->bitmap_run = obj = fl_add_bitmapbutton(FL_NORMAL_BUTTON,800,420,40,30,"");
    fl_set_object_callback(obj,EvolveCB,0);
    fl_set_object_helper(obj, "Evolve");
    fl_set_bitmapbutton_file(obj, "bitmaps/ffwd.xbm");
  fdui->bitmap_step = obj = fl_add_bitmapbutton(FL_NORMAL_BUTTON,700,420,40,30,"");
    fl_set_object_callback(obj,StepCB,0);
    fl_set_object_helper(obj, "Single Step");
    fl_set_bitmapbutton_file(obj, "bitmaps/fwds.xbm");
  fdui->bitmap_some = obj = fl_add_bitmapbutton(FL_NORMAL_BUTTON,750,420,40,30,"");
    fl_set_object_callback(obj,EvolveSomeCB,0);
    fl_set_object_helper(obj, "'n' Generations");
    fl_set_bitmapbutton_file(obj, "bitmaps/ffst.xbm");
  fdui->ga_start = obj = fl_add_button(FL_NORMAL_BUTTON,510,420,80,30,"GO!");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,OPTsetupCB,0);
  fdui->geneview = obj = fl_add_browser(FL_MULTI_BROWSER,10,50,170,400,"Actions");
    fl_set_object_boxtype(obj,FL_FRAME_BOX);
    fl_set_object_lalign(obj,FL_ALIGN_TOP);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,OPTnoneCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,190,420,90,30,"Options");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gainbuttonCB,2);
  obj = fl_add_button(FL_NORMAL_BUTTON,290,420,90,30,"Advanced");
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gainbuttonCB,3);
  obj = fl_add_positioner(FL_NORMAL_POSITIONER,5,25,835,10,"");
    fl_set_object_boxtype(obj,FL_NO_BOX);
    fl_set_object_callback(obj,optactionCB,0);
  obj = fl_add_menu(FL_PULLDOWN_MENU,5,5,50,20,"File");
    fl_set_object_callback(obj,OPTexitCB,0);
    fl_set_menu_entries(obj, fdmenu_File_0);
  fl_end_form();

  fdui->gagraph->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

static FL_PUP_ENTRY fdchoice_gamenu_genome_1[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "Real Number",	0,	"",	 FL_PUP_NONE},
    { "Binary-to-Decimal",	0,	"",	 FL_PUP_NONE},
    { "Response Option",	0,	"",	 FL_PUP_NONE},
    {0}
};

static FL_PUP_ENTRY fdchoice_gamenu_algorithm_2[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "Incremental",	0,	"",	 FL_PUP_NONE},
    { "Simple",	0,	"",	 FL_PUP_NONE},
    { "Steady-State",	0,	"",	 FL_PUP_NONE},
    { "Deme",	0,	"",	 FL_PUP_NONE},
    {0}
};

static FL_PUP_ENTRY fdchoice_gamenu_mutation_3[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "Random Flip",	0,	"",	 FL_PUP_NONE},
    { "Random Swap",	0,	"",	 FL_PUP_NONE},
    { "Gaussian",	0,	"",	 FL_PUP_NONE},
    {0}
};

static FL_PUP_ENTRY fdchoice_gamenu_xover_4[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "Partial Match",	0,	"",	 FL_PUP_NONE},
    { "Ordered",	0,	"",	 FL_PUP_NONE},
    { "Cycle",	0,	"",	 FL_PUP_NONE},
    { "One Point",	0,	"",	 FL_PUP_NONE},
    { "Two Point",	0,	"",	 FL_PUP_NONE},
    { "Even/Odd",	0,	"",	 FL_PUP_NONE},
    { "Uniform",	0,	"",	 FL_PUP_NONE},
    {0}
};

static FL_PUP_ENTRY fdchoice_gamenu_init_5[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "Uniform Random",	0,	"",	 FL_PUP_NONE},
    { "Order-based Random",	0,	"",	 FL_PUP_NONE},
    { "Initialize-to-Zero",	0,	"",	 FL_PUP_NONE},
    {0}
};

static FL_PUP_ENTRY fdchoice_gamenu_replace_6[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "Replace Parent",	0,	"",	 FL_PUP_NONE},
    { "Replace Random",	0,	"",	 FL_PUP_NONE},
    { "Replace Worst",	0,	"",	 FL_PUP_NONE},
    { "Replace Best",	0,	"",	 FL_PUP_NONE},
    {0}
};

static FL_PUP_ENTRY fdchoice_gamenu_select_7[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "Rank",	0,	"",	 FL_PUP_NONE},
    { "Roulette Wheel",	0,	"",	 FL_PUP_NONE},
    { "Tournament",	0,	"",	 FL_PUP_NONE},
    { "Stochastic Remainder Sampling",	0,	"",	 FL_PUP_NONE},
    { "Stochastic Uniform Sampling",	0,	"",	 FL_PUP_NONE},
    { "Deterministic Sampling",	0,	"",	 FL_PUP_NONE},
    {0}
};

static FL_PUP_ENTRY fdchoice_gamenu_terminate_8[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "No Scaling",	0,	"",	 FL_PUP_NONE},
    { "Linear",	0,	"",	 FL_PUP_NONE},
    {0}
};

static FL_PUP_ENTRY fdchoice_gamenu_scaling_9[] =
{ 
    /*  itemtext   callback  shortcut   mode */
    { "No Scaling",	0,	"",	 FL_PUP_NONE},
    { "Linear",	0,	"",	 FL_PUP_NONE},
    { "Sigma Truncation",	0,	"",	 FL_PUP_NONE},
    { "Power Law",	0,	"",	 FL_PUP_NONE},
    { "Sharing",	0,	"",	 FL_PUP_NONE},
    {0}
};

FD_gaparms *create_form_gaparms(void)
{
  FL_OBJECT *obj;
  FD_gaparms *fdui = (FD_gaparms *) fl_calloc(1, sizeof(*fdui));

  fdui->gaparms = fl_bgn_form(FL_NO_BOX, 850, 460);
  obj = fl_add_box(FL_UP_BOX,0,0,850,460,"");
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,510,20,180,130,"Selection Criteria");
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gaselectCB,0);
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,300,230,180,220,"Algorithms");
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,OPTnoneCB,0);
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,510,230,180,220,"Genome Operators");
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,OPTnoneCB,0);
  fdui->gamenu_genome = obj = fl_add_choice(FL_NORMAL_CHOICE2,530,50,140,30,"Genome");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gachoiceCB,0);
    fl_set_choice_entries(obj, fdchoice_gamenu_genome_1);
    fl_set_choice(obj,3);
  fdui->gamenu_algorithm = obj = fl_add_choice(FL_NORMAL_CHOICE2,530,270,140,20,"Algorithm");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gachoiceCB,1);
    fl_set_choice_entries(obj, fdchoice_gamenu_algorithm_2);
    fl_set_choice(obj,3);
  fdui->ga_geninc = obj = fl_add_input(FL_NORMAL_INPUT,630,120,40,20,"Generation\nIncrement");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,OPTinputCB,0);
  fdui->gamenu_mutation = obj = fl_add_choice(FL_NORMAL_CHOICE2,530,320,140,20,"Mutation Method");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gachoiceCB,2);
    fl_set_choice_entries(obj, fdchoice_gamenu_mutation_3);
    fl_set_choice(obj,3);
  fdui->gamenu_xover = obj = fl_add_choice(FL_NORMAL_CHOICE2,530,370,140,20,"Crossover Method");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gachoiceCB,3);
    fl_set_choice_entries(obj, fdchoice_gamenu_xover_4);
    fl_set_choice(obj,7);
  fdui->gamenu_init = obj = fl_add_choice(FL_NORMAL_CHOICE2,530,420,140,20,"Initialization Method");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gachoiceCB,4);
    fl_set_choice_entries(obj, fdchoice_gamenu_init_5);
    fl_set_choice(obj,1);
  fdui->gamenu_replace = obj = fl_add_choice(FL_NORMAL_CHOICE2,310,270,140,20,"Replacement Method");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gachoiceCB,5);
    fl_set_choice_entries(obj, fdchoice_gamenu_replace_6);
    fl_set_choice(obj,3);
  fdui->gamenu_select = obj = fl_add_choice(FL_NORMAL_CHOICE2,310,320,140,20,"Selection Method");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gachoiceCB,6);
    fl_set_choice_entries(obj, fdchoice_gamenu_select_7);
    fl_set_choice(obj,2);
  fdui->gamenu_terminate = obj = fl_add_choice(FL_NORMAL_CHOICE2,310,370,140,20,"Termination Method");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gachoiceCB,7);
    fl_set_choice_entries(obj, fdchoice_gamenu_terminate_8);
    fl_set_choice(obj,1);
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,10,20,260,430,"Parameter Override");
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,OPTnoneCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,750,10,80,30,"Dismiss");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gaexitCB,2);
  fdui->gaparm[0] = obj = fl_add_input(FL_NORMAL_INPUT,160,30,90,20,"Minimum/Maximum");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gaparmsCB,0);
  fdui->gaparm[1] = obj = fl_add_input(FL_NORMAL_INPUT,160,60,90,20,"Number of Generations");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gaparmsCB,1);
  fdui->gaparm[2] = obj = fl_add_input(FL_NORMAL_INPUT,160,90,90,20,"Convergence Percentage");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gaparmsCB,2);
  fdui->gaparm[3] = obj = fl_add_input(FL_NORMAL_INPUT,160,120,90,20,"Convergence Generations");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gaparmsCB,3);
  fdui->gaparm[4] = obj = fl_add_input(FL_NORMAL_INPUT,160,150,90,20,"Crossover Probability");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gaparmsCB,4);
  fdui->gaparm[5] = obj = fl_add_input(FL_NORMAL_INPUT,160,180,90,20,"Mutation Probability");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gaparmsCB,5);
  fdui->gaparm[6] = obj = fl_add_input(FL_NORMAL_INPUT,160,210,90,20,"Population Size");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gaparmsCB,6);
  fdui->gaparm[7] = obj = fl_add_input(FL_NORMAL_INPUT,160,240,90,20,"Number of Populations");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gaparmsCB,7);
  fdui->gaparm[8] = obj = fl_add_input(FL_NORMAL_INPUT,160,270,90,20,"Replace Percentage");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gaparmsCB,8);
  fdui->gaparm[9] = obj = fl_add_input(FL_NORMAL_INPUT,160,300,90,20,"Replace Number");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gaparmsCB,9);
  fdui->gaparm[10] = obj = fl_add_input(FL_NORMAL_INPUT,160,330,90,20,"No. of Best Indivduals");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gaparmsCB,10);
  fdui->gaparm[11] = obj = fl_add_input(FL_NORMAL_INPUT,160,360,90,20,"Number of Offspring");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gaparmsCB,11);
  fdui->gaparm[12] = obj = fl_add_input(FL_NORMAL_INPUT,160,390,90,20,"Migration Percentage");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gaparmsCB,12);
  fdui->gaparm[13] = obj = fl_add_input(FL_NORMAL_INPUT,160,420,90,20,"Migration Number");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gaparmsCB,13);
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,300,20,180,90,"Record Diversity");
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,OPTnoneCB,0);
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,300,120,180,90,"Elitism");
    fl_set_object_lcolor(obj,FL_CYAN);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,OPTnoneCB,0);

  fdui->diversegrp = fl_bgn_group();
  obj = fl_add_checkbutton(FL_RADIO_BUTTON,320,40,110,20,"Diversity On");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,OPTnoneCB,1);
    fl_set_button(obj, 1);
  obj = fl_add_checkbutton(FL_RADIO_BUTTON,320,70,110,20,"Diversity Off");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,OPTnoneCB,0);
  fl_end_group();


  fdui->elitismgrp = fl_bgn_group();
  obj = fl_add_checkbutton(FL_RADIO_BUTTON,320,140,100,20,"Elitism On");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,OPTnoneCB,1);
    fl_set_button(obj, 1);
  obj = fl_add_checkbutton(FL_RADIO_BUTTON,320,170,100,20,"Elitism Off");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,OPTnoneCB,0);
  fl_end_group();

  fdui->gamenu_scaling = obj = fl_add_choice(FL_NORMAL_CHOICE2,310,420,140,20,"Scaling Scheme");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gachoiceCB,8);
    fl_set_choice_entries(obj, fdchoice_gamenu_scaling_9);
    fl_set_choice(obj,1);
  fl_end_form();

  fdui->gaparms->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

FD_gacommit *create_form_gacommit(void)
{
  FL_OBJECT *obj;
  FD_gacommit *fdui = (FD_gacommit *) fl_calloc(1, sizeof(*fdui));

  fdui->gacommit = fl_bgn_form(FL_NO_BOX, 850, 460);
  obj = fl_add_box(FL_UP_BOX,0,0,850,460,"");
  obj = fl_add_labelframe(FL_ENGRAVED_FRAME,250,20,350,110,"Active Constraints");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,OPTnoneCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,750,20,80,30,"Dismiss");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,gaexitCB,3);
  fdui->opt_reslist = obj = fl_add_browser(FL_HOLD_BROWSER,30,190,390,220,"Weapon Type                        # Available          # Committed");
    fl_set_object_color(obj,FL_WHITE,FL_YELLOW);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,optcommitCB,0);
  fdui->opt_tgtlist = obj = fl_add_browser(FL_HOLD_BROWSER,430,190,390,220,"Target Class                         # Available          # Committed");
    fl_set_object_color(obj,FL_WHITE,FL_YELLOW);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,optcommitCB,5);
  obj = fl_add_text(FL_NORMAL_TEXT,70,150,150,20,"Resource Commitment:");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,OPTnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,490,150,130,20,"Target Commitment:");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,OPTnoneCB,0);
  obj = fl_add_text(FL_NORMAL_TEXT,20,20,100,30,"Mission\nCommitment:");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,OPTnoneCB,0);
  fdui->opt_nactions = obj = fl_add_counter(FL_SIMPLE_COUNTER,20,60,100,20,"# of Actions");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,optactionCB,0);
    fl_set_counter_precision(obj, 0);
    fl_set_counter_bounds(obj, 0, 1000);
    fl_set_counter_value(obj, 3);
    fl_set_counter_step(obj, 1, 1);
  fdui->opt_constrain[0] = obj = fl_add_checkbutton(FL_NORMAL_BUTTON,260,30,150,20,"Nuclear Threshold");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,optconstrainCB,0);
  fdui->opt_constrain[1] = obj = fl_add_checkbutton(FL_NORMAL_BUTTON,260,50,150,20,"Fratricide");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,optconstrainCB,1);
  fdui->opt_constrain[2] = obj = fl_add_checkbutton(FL_NORMAL_BUTTON,260,70,150,20,"Leakage < 5%");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,optconstrainCB,2);
  fdui->opt_cobstrain[3] = obj = fl_add_checkbutton(FL_NORMAL_BUTTON,260,90,150,20,"Unassigned");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,optconstrainCB,3);
  fdui->opt_constrain[4] = obj = fl_add_checkbutton(FL_NORMAL_BUTTON,260,110,150,20,"Unassigned");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,optconstrainCB,4);
  fdui->opt_constrain[5] = obj = fl_add_checkbutton(FL_NORMAL_BUTTON,440,30,150,20,"Unassigned");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,optconstrainCB,5);
  fdui->opt_contrain[6] = obj = fl_add_checkbutton(FL_NORMAL_BUTTON,440,50,150,20,"Unassigned");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,optconstrainCB,6);
  fdui->opt_constrain[7] = obj = fl_add_checkbutton(FL_NORMAL_BUTTON,440,70,150,20,"Unassigned");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,optconstrainCB,7);
  fdui->opt_constrain[8] = obj = fl_add_checkbutton(FL_NORMAL_BUTTON,440,90,150,20,"Unassigned");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,optconstrainCB,8);
  fdui->opt_constrain[9] = obj = fl_add_checkbutton(FL_NORMAL_BUTTON,440,110,150,20,"Unassigned");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,optconstrainCB,9);
  fdui->opt_restext = obj = fl_add_text(FL_NORMAL_TEXT,220,150,170,20,"U.S. Only");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lcolor(obj,FL_BLUE);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,OPTnoneCB,0);
  fdui->opt_tgttext = obj = fl_add_text(FL_NORMAL_TEXT,620,150,170,20,"DPRK");
    fl_set_object_boxtype(obj,FL_SHADOW_BOX);
    fl_set_object_lcolor(obj,FL_RED);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,OPTnoneCB,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,290,420,80,20,"Commit");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,optcommitCB,2);
  obj = fl_add_button(FL_NORMAL_BUTTON,690,420,80,20,"Commit");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,optcommitCB,7);
  fdui->opt_rescommit = obj = fl_add_valslider(FL_HOR_BROWSER_SLIDER,180,420,100,20,"");
    fl_set_object_callback(obj,optcommitCB,1);
    fl_set_slider_precision(obj, 0);
    fl_set_slider_bounds(obj, 0, 100);
    fl_set_slider_value(obj, 1);
    fl_set_slider_size(obj, 0.19);
    fl_set_slider_step(obj, 1);
    fl_set_slider_increment(obj, 1, 1);
  fdui->opt_tgtcommit = obj = fl_add_valslider(FL_HOR_BROWSER_SLIDER,580,420,100,20,"");
    fl_set_object_callback(obj,optcommitCB,6);
    fl_set_slider_precision(obj, 0);
    fl_set_slider_bounds(obj, 0, 100);
    fl_set_slider_value(obj, 1);
    fl_set_slider_size(obj, 0.19);
    fl_set_slider_step(obj, 1);
    fl_set_slider_increment(obj, 1, 1);
  obj = fl_add_text(FL_NORMAL_TEXT,25,415,150,40,"Select Weapon Type above\nSet # to commit at right\nPush 'Commit' button");
    fl_set_object_lcolor(obj,FL_WHEAT);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,optcommitCB,6);
  obj = fl_add_text(FL_NORMAL_TEXT,425,415,150,40,"Select Target Class above\nSet # to commit at right\nPush 'Commit' button");
    fl_set_object_lcolor(obj,FL_WHEAT);
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,optcommitCB,6);
  obj = fl_add_text(FL_NORMAL_TEXT,130,20,100,30,"% Goodness\nof match");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,OPTnoneCB,0);
  fdui->opt_goodness = obj = fl_add_dial(FL_LINE_DIAL,150,55,50,40,"0 %");
    fl_set_object_callback(obj,optactionCB,1);
    fl_set_dial_bounds(obj, 0, 100);
    fl_set_dial_value(obj, 0);
    fl_set_dial_step(obj, 1);
  fdui->ga_goal = obj = fl_add_counter(FL_SIMPLE_COUNTER,20,110,100,20,"rDE Goal");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,optactionCB,0);
    fl_set_counter_precision(obj, 0);
    fl_set_counter_bounds(obj, 0, 1000);
    fl_set_counter_value(obj, 3);
    fl_set_counter_step(obj, 1, 1);
  fl_end_form();

  fdui->gacommit->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

