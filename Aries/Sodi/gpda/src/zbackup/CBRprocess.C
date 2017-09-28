#include<iostream.h>
#include<fstream.h>
#include<stdlib.h>


/***
    Include all neccessary files for X-Forms to run coded forms
    Code created by X-Forms when form was designed through
    their program
***/

#include "forms.h"
#include "criteria.h"
#include "test.h"
#include "changer.h"
#include "time_changer.h"
#include "terrain_changer.h"
#include "float_changer.h"
#include "char_changer.h"
#include "add.h"
#include "remove.h"
#include "final.h"
#include "graphs.h"
#include "cases.h"

/*holds total value of added user entered weights*/
float case_comparison_score;

/*struct from cases.h for user entered data*/
WeightCriteria Weights;

/***
    definition of forms
***/
FD_criteria *fd_criteria;
FD_test *fd_test;
FD_changer *fd_changer;
FD_time_changer *fd_time_changer;
FD_terrain_changer *fd_terrain_changer;
FD_float_changer *fd_float_changer;
FD_char_changer *fd_char_changer;
FD_add *fd_add;
FD_remove *fd_remove;
FD_final *fd_final;
FD_graphs *fd_graphs;

/*Instantiate the new Case class*/
CASECLASS COAcases;


/***
    The start, where it all begins.  This is a call X-Forms produced
    that initializes all the forms placed in here and runs the first
    one specified...in this case "criteria".
***/

void
CBRprocess()
{

   /*initializes all the forms so they can run*/
   fd_criteria = create_form_criteria();
   fd_test = create_form_test();
   fd_changer = create_form_changer();
   fd_time_changer = create_form_time_changer();
   fd_terrain_changer = create_form_terrain_changer();
   fd_float_changer = create_form_float_changer();
   fd_char_changer = create_form_char_changer();
   fd_add = create_form_add();
   fd_remove = create_form_remove();
   fd_final = create_form_final();
   fd_graphs = create_form_graphs();

   /*my function to place appropriate data in "criteria" form before it is run*/
   void initialize();

   //runs initialize
   initialize();

   /*tells X-Forms where on screen to put top left corner of form*/
   fl_set_form_position(fd_criteria->criteria, 20, 20);

   /*Displays the form "criteria"*/
   fl_show_form(fd_criteria->criteria,FL_PLACE_POSITION,FL_FULLBORDER,"criteria");
  
   return;
}

/*** 
   Include all form code written after initialization of 
   other elements
***/

#include "test_cb.h"
#include "changer_cb.h"
#include "time_changer_cb.h"
#include "terrain_changer_cb.h"
#include "float_changer_cb.h"
#include "char_changer_cb.h"
#include "add_cb.h"
#include "remove_cb.h"
#include "final_cb.h"
#include "graphs_cb.h"

//-------------------initialize-----------------------------

// fill-in form initialization code */
//creating option lists in pull-down lists on form criteria */

void initialize()
{

   /***
       sets defaults for the user entered weights as the best
       because the form shows all criteria selections as best.
       If the user does not touch a selection, leaving it as
       the best, the form will not register the value so
       they must be set here
   ***/

   Weights.max_threatsW = 1;
   Weights.terrainW = 1;
   Weights.slopeW = 1;
   Weights.timingW = 1;
   Weights.strengthW = 1;
   Weights.timeW = 1;
   Weights.commoditiesW = 1;
   Weights.equipageW = 1;
   Weights.fuelW = 1;
   Weights.subsidiariesW = 1;
   Weights.expected_down_commW = 1;
   Weights.unexpected_down_commW = 1;
   Weights.natural_elementsW = 1;
   Weights.mechanicalW = 1;
   Weights.distanceW = 1;


/*sets the fontsize of menu selections shown on the form*/
   fl_set_choice_fontsize(fd_criteria->time, 12);
   fl_set_choice_fontsize(fd_criteria->distance, 12);
   fl_set_choice_fontsize(fd_criteria->timing, 12);
   fl_set_choice_fontsize(fd_criteria->slope, 12);
   fl_set_choice_fontsize(fd_criteria->terrain, 12);
   fl_set_choice_fontsize(fd_criteria->equipage, 12);
   fl_set_choice_fontsize(fd_criteria->commodities, 12);
   fl_set_choice_fontsize(fd_criteria->subsidiaries, 12);
   fl_set_choice_fontsize(fd_criteria->fuel, 12);
   fl_set_choice_fontsize(fd_criteria->natural_elements, 12);
   fl_set_choice_fontsize(fd_criteria->mechanical, 12);
   fl_set_choice_fontsize(fd_criteria->expected_down_comm, 12);
   fl_set_choice_fontsize(fd_criteria->unexpected_down_comm, 12);
   fl_set_choice_fontsize(fd_criteria->max_threats, 12);
   fl_set_choice_fontsize(fd_criteria->strength, 12);

/*sets the menu titles as fontsize 14 and syle Times New Roman*/
   fl_setpup_fontsize(14);
   fl_setpup_fontstyle(8);

//operations init
/***
    Adds the words in quotes as menu options in the menu declared
    after fd_criteria->

    fd_criteria specifies the form being used...it could be 
    fd_graphs if the graphs form was wanted

    the name after the arrow fd_criteria->time specifies
    the field in the form named to be manipulated
***/

   fl_addto_choice(fd_criteria->time, "Fastest");
   fl_addto_choice(fd_criteria->time, "Fast");
   fl_addto_choice(fd_criteria->time, "Doesn't matter");

   fl_addto_choice(fd_criteria->distance, "Shortest");
   fl_addto_choice(fd_criteria->distance, "Short");
   fl_addto_choice(fd_criteria->distance, "Doesn't matter");

//terrain init

   fl_addto_choice(fd_criteria->terrain, "Urban");
   fl_addto_choice(fd_criteria->terrain, "Range Land");
   fl_addto_choice(fd_criteria->terrain, "Barren Land");
   fl_addto_choice(fd_criteria->terrain, "Tundra");
   fl_addto_choice(fd_criteria->terrain, "Perennial Ice/Snow");
   fl_addto_choice(fd_criteria->terrain, "Wetland");
   fl_addto_choice(fd_criteria->terrain, "Forest");

   fl_addto_choice(fd_criteria->slope, "0 <= 2");
   fl_addto_choice(fd_criteria->slope, "3 <= 5 ");
   fl_addto_choice(fd_criteria->slope, "6 <= 8");
   fl_addto_choice(fd_criteria->slope, "9 <= 11");
   fl_addto_choice(fd_criteria->slope, "12 <= 14");
   fl_addto_choice(fd_criteria->slope, "< 15");

//logistics init

   fl_addto_choice(fd_criteria->commodities, "Convenient");
   fl_addto_choice(fd_criteria->commodities, "Attainable");
   fl_addto_choice(fd_criteria->commodities, "Remote");

   fl_addto_choice(fd_criteria->equipage, "Convenient");
   fl_addto_choice(fd_criteria->equipage, "Attainable");
   fl_addto_choice(fd_criteria->equipage, "Remote");

   fl_addto_choice(fd_criteria->fuel, "Unconstrained");
   fl_addto_choice(fd_criteria->fuel, "Partially Constrained");
   fl_addto_choice(fd_criteria->fuel, "Constrained");

   fl_addto_choice(fd_criteria->subsidiaries, "Convenient");
   fl_addto_choice(fd_criteria->subsidiaries, "Attainable");
   fl_addto_choice(fd_criteria->subsidiaries, "Remote");
  
//vulnerability init
 
   fl_addto_choice(fd_criteria->natural_elements, "Never");
   fl_addto_choice(fd_criteria->natural_elements, "Very Poor");
   fl_addto_choice(fd_criteria->natural_elements, "Poor");
   fl_addto_choice(fd_criteria->natural_elements, "Fair");
   fl_addto_choice(fd_criteria->natural_elements, "High");
   fl_addto_choice(fd_criteria->natural_elements, "Very High");
   fl_addto_choice(fd_criteria->natural_elements, "Guaranteed");

   fl_addto_choice(fd_criteria->unexpected_down_comm, "Never");
   fl_addto_choice(fd_criteria->unexpected_down_comm, "Very Poor");
   fl_addto_choice(fd_criteria->unexpected_down_comm, "Poor");
   fl_addto_choice(fd_criteria->unexpected_down_comm, "Fair");
   fl_addto_choice(fd_criteria->unexpected_down_comm, "High");
   fl_addto_choice(fd_criteria->unexpected_down_comm, "Very High");
   fl_addto_choice(fd_criteria->unexpected_down_comm, "Guaranteed");

   fl_addto_choice(fd_criteria->expected_down_comm, "Never");
   fl_addto_choice(fd_criteria->expected_down_comm, "Very Rarely");
   fl_addto_choice(fd_criteria->expected_down_comm, "Rarely");
   fl_addto_choice(fd_criteria->expected_down_comm, "Often");
   fl_addto_choice(fd_criteria->expected_down_comm, "Very Often");
   fl_addto_choice(fd_criteria->expected_down_comm, "Frequently");
   fl_addto_choice(fd_criteria->expected_down_comm, "Always");

   fl_addto_choice(fd_criteria->mechanical, "Never");
   fl_addto_choice(fd_criteria->mechanical, "Very Poor");
   fl_addto_choice(fd_criteria->mechanical, "Poor");
   fl_addto_choice(fd_criteria->mechanical, "Fair");
   fl_addto_choice(fd_criteria->mechanical, "High");
   fl_addto_choice(fd_criteria->mechanical, "Very High");
   fl_addto_choice(fd_criteria->mechanical, "Guaranteed");

//threat init

   fl_addto_choice(fd_criteria->max_threats, "0");
   fl_addto_choice(fd_criteria->max_threats, "1 - 2");
   fl_addto_choice(fd_criteria->max_threats, "3 - 4");
   fl_addto_choice(fd_criteria->max_threats, "5 - 6");
   fl_addto_choice(fd_criteria->max_threats, "< 6");

   fl_addto_choice(fd_criteria->timing, "Start");
   fl_addto_choice(fd_criteria->timing, "Middle");
   fl_addto_choice(fd_criteria->timing, "End");
   fl_addto_choice(fd_criteria->timing, "Doesn't Matter");

   fl_addto_choice(fd_criteria->strength, "Pathetic");
   fl_addto_choice(fd_criteria->strength, "Weak");
   fl_addto_choice(fd_criteria->strength, "Equal");
   fl_addto_choice(fd_criteria->strength, "Overbearing");
   fl_addto_choice(fd_criteria->strength, "Don't Care");

}

/*--------------------Call backs for form criteria------------------*/

//when user chooses criteria, the weight of that value
//is set to appropriate variable

/***
    When a user clicks on a menu and selects an option from the list
    it produces, the "call back" is executed.  

    For example, when the max threats menu on the criteria form
    is selected and an option is chosen, the program will immediately
    call max_threatsCB below.  The call back function then knows what
    part of the form was manipulated and can perform various operations.
    In the cases below, the call backs call a function to get the
    line number that the user selected.  Once this number is known,
    it is determined what value for that criteria was selected and
    the appropriate weight variable is modified.

    This is how call backs work for most functions.  A field on a 
    form is clicked or highlighted so a function is called written
    by the programmer to tell the form what to do.

***/





void max_threatsCB(FL_OBJECT *ob, long data)
{
  int choice;

/*
  get the line number of the user selected choice
*/

  choice  = fl_get_choice(fd_criteria->max_threats);

/*
  assign the weights variable the appropriate value
  Follow this method for all 15 indexes available to
  choose from on the "criteria" form
*/

  switch (choice)
  {
     case 1: Weights.max_threatsW = 1; break;
     case 2: Weights.max_threatsW = .75; break;
     case 3: Weights.max_threatsW = .50; break;
     case 4: Weights.max_threatsW = .25; break;
     case 5: Weights.max_threatsW = 0; break;
  };
}

void terrainCB(FL_OBJECT *ob, long data)
{
   int choice;
 
   choice = fl_get_choice(fd_criteria->terrain);

   switch (choice)
   {
      case 1: Weights.terrainW = 1; break;
      case 2: Weights.terrainW = .86; break;
      case 3: Weights.terrainW = .72; break;
      case 4: Weights.terrainW = .58; break;
      case 5: Weights.terrainW = .44; break;
      case 6: Weights.terrainW = .30; break;
      case 7: Weights.terrainW = .16; break;
   };
}

void slopeCB(FL_OBJECT *ob, long data)
{
   int choice;
 
   choice = fl_get_choice(fd_criteria->slope);

   switch (choice)
   {
      case 1: Weights.slopeW = 1; break;
      case 2: Weights.slopeW = .8; break;
      case 3: Weights.slopeW = .6; break;
      case 4: Weights.slopeW = .4; break;
      case 5: Weights.slopeW = .2; break;
      case 6: Weights.slopeW = 0; break;
   };
}

void threat_timingCB(FL_OBJECT *ob, long data)
{
   int choice;

   choice = fl_get_choice(fd_criteria->timing);

//timing isn't a "weighted" value.  Number stands for what is is,
//like an enum type I didn't feel like writing.  These numbers
//are used for comparison later in scoring

   switch (choice)
   {
      case 1: Weights.timingW = 1; break;
      case 2: Weights.timingW = 2; break;
      case 3: Weights.timingW = 3; break;
      case 4: Weights.timingW = 4; break;
   };
}

void strengthCB(FL_OBJECT *ob, long data)
{
   int choice;
 
   choice = fl_get_choice(fd_criteria->strength);

   switch (choice)
   {
      case 1: Weights.strengthW = 1; break;
      case 2: Weights.strengthW = .75; break;
      case 3: Weights.strengthW = .50; break;
      case 4: Weights.strengthW = .25; break;
      case 5: Weights.strengthW = 0; break;
   };
}

void timeCB(FL_OBJECT *ob, long data)
{
   int choice;

   choice = fl_get_choice(fd_criteria->time);

   switch (choice)
   {
      case 1: Weights.timeW = 1; break;
      case 2: Weights.timeW = .5; break;
      case 3: Weights.timeW = 0; break;
   };
}

void commoditiesCB(FL_OBJECT *ob, long data)
{
   int choice;

   choice = fl_get_choice(fd_criteria->commodities);

   switch (choice)
   {
      case 1: Weights.commoditiesW = 1; break;
      case 2: Weights.commoditiesW = .5; break;
      case 3: Weights.commoditiesW = 0; break;
   };
}

void equipageCB(FL_OBJECT *ob, long data)
{

   int choice;

   choice = fl_get_choice(fd_criteria->equipage);

   switch (choice)
   {
      case 1: Weights.equipageW = 1; break;
      case 2: Weights.equipageW = .5; break;
      case 3: Weights.equipageW = 0; break;
   };
}

void fuelCB(FL_OBJECT *ob, long data)
{
   int choice;

   choice = fl_get_choice(fd_criteria->fuel);

   switch (choice)
   {
      case 1: Weights.fuelW = 1; break;
      case 2: Weights.fuelW = .5; break;
      case 3: Weights.fuelW = 0; break;
   };
}

void subsidiariesCB(FL_OBJECT *ob, long data)
{
   int choice;

   choice = fl_get_choice(fd_criteria->subsidiaries);

   switch (choice)
   {
      case 1: Weights.subsidiariesW = 1; break;
      case 2: Weights.subsidiariesW = .5; break;
      case 3: Weights.subsidiariesW = 0; break;
   };
}

void expected_down_commCB(FL_OBJECT *ob, long data)
{
   int choice;

   choice = fl_get_choice(fd_criteria->expected_down_comm);

   switch (choice)
   {
      case 1: Weights.expected_down_commW = 1; break;
      case 2: Weights.expected_down_commW = .7; break;
      case 3: Weights.expected_down_commW = .4; break;
      case 4: Weights.expected_down_commW = 0; break;
   }; 
}

void unexpected_down_comm(FL_OBJECT *ob, long data)
{

   int choice;

   choice = fl_get_choice(fd_criteria->unexpected_down_comm);

   switch (choice)
   {
      case 1: Weights.unexpected_down_commW = 1; break;
      case 2: Weights.unexpected_down_commW = .7; break;
      case 3: Weights.unexpected_down_commW = .4; break;
      case 4: Weights.unexpected_down_commW = 0; break;
   }; 

}

void natural_elemenstCB(FL_OBJECT *ob, long data)
{
   int choice;

   choice = fl_get_choice(fd_criteria->natural_elements);

   switch (choice)
   {
      case 1: Weights.natural_elementsW = 1; break;
      case 2: Weights.natural_elementsW = .7; break;
      case 3: Weights.natural_elementsW = .4; break;
      case 4: Weights.natural_elementsW = 0; break;
   }; 
}

void mechanicalCB(FL_OBJECT *ob, long data)
{
   int choice;

   choice = fl_get_choice(fd_criteria->mechanical);

   switch (choice)
   {
      case 1: Weights.mechanicalW = 1; break;
      case 2: Weights.mechanicalW = .7; break;
      case 3: Weights.mechanicalW = .4; break;
      case 4: Weights.mechanicalW = 0; break;
   }; 
}

/***
    The start call back is called when the start button of the "criteria"
    form is clicked.

    The user has finished entering criteria and is ready to see the best
    cases.

    So, the "criteria" form can be hidden, no longer needed, and 
    the next form to use is set up and shown.  The program is then
    running that form and will call call backs from the appropriate
    file..which are technically part of this one since they are
    all included
***/


void startCB(FL_OBJECT *ob, long data)
{
   float score;
   float Case_Selection();
   Case_Real_data real_data[3];
   char chtemp[128];

//hide the criteria form
   fl_hide_form(fd_criteria->criteria);

//get the added total of the user entered values
   score = Case_Selection();

//score each case in the case base based on the user entered
//values in Weights.  The one mean the first linked list...
//the cases in the case base.  A 2 would mean the linked list
//of the three best cases
   COAcases.set_case_values(Weights, 1);

//Find the three best cases
   COAcases.rate_cases(score);

//Determine the number of best cases (3 max) and set up the "test" form
//showing the case real data or NONE if less than three best
//cases were found.  Less than three would only occur if
//there were less than three cases in the case base
   COAcases.repair_cases();

//set the font size of the labels for the imaginary route graphs
//about to be created
   fl_set_chart_lsize(fd_test->route1, 12);
   fl_set_chart_lsize(fd_test->route2, 12);
   fl_set_chart_lsize(fd_test->route3, 12);

//Below places three imaginary routes with nodes meaning stoping
//places and edges in the respective field.  
//These I threw in just for show because I 
//couldn't get the graphs to graph the nodes and edges stored
//in the case real data to graph
   fl_add_chart_value(fd_test->route1, 1, "A", 1); 
   fl_add_chart_value(fd_test->route1, 2, "B", 1);
   fl_add_chart_value(fd_test->route1, 10, "C", 1);
   fl_add_chart_value(fd_test->route1, 3, "D", 1);
   fl_add_chart_value(fd_test->route1, 1, "E", 1);

   fl_add_chart_value(fd_test->route2, 10, "A", 1); 
   fl_add_chart_value(fd_test->route2, 2, "B", 1);
   fl_add_chart_value(fd_test->route2, 5, "C", 1);
   fl_add_chart_value(fd_test->route2, 2, "D", 1);
   fl_add_chart_value(fd_test->route2, 1, "E", 1);

   fl_add_chart_value(fd_test->route3, 6, "A", 1);
   fl_add_chart_value(fd_test->route3, 2, "B", 1);
   fl_add_chart_value(fd_test->route3, 7, "C", 1);
   fl_add_chart_value(fd_test->route3, 9, "D", 1);
   fl_add_chart_value(fd_test->route3, 6, "E", 1);

//show the form "test", centered on the screen, with a full border and 
//the title COAs
   fl_show_form(fd_test->test, FL_PLACE_CENTER, FL_FULLBORDER, "COAs");
 
//Sets the text field in a form called changer to the string in "" 
   fl_set_object_label(fd_changer->output, "If you click on a blue option shown
      in each COA browser below, a box like this will appear
      describing the value and allowing change.");

//Sets the postion of the form changer on the screen
   fl_set_form_position(fd_changer->changer, 50, 100);

//shows the form changer overlaying the test form
//a click on the done button on the changer form will remove
//it revealing all of the test form again
   fl_show_form(fd_changer->changer, FL_PLACE_POSITION, FL_FULLBORDER, "Info");

}

/***
    If the exit button is pushed on the "criteria" form
    the program removes the criteria form and returns
***/
void exitCB(FL_OBJECT *ob, long data)
{
  //exits form and returns to command line/caller
 
  fl_hide_form(fd_criteria->criteria);
}

void distanceCB(FL_OBJECT *ob, long data)
{
   int choice;

   choice = fl_get_choice(fd_criteria->distance);

   switch (choice)
   {
      case 1: Weights.distanceW = 1; break;
      case 2: Weights.distanceW = .5; break;
      case 3: Weights.distanceW = 0; break;
   };
}

//------------------------Case Selection----------------

//adds user-input weighted criteria and
//begins COA comparison and selection

float Case_Selection()
{
  void score_cases();

  case_comparison_score = 
      
      Weights.max_threatsW+
      Weights.terrainW+
      Weights.slopeW+
      Weights.strengthW+
      Weights.timeW+
      Weights.commoditiesW+
      Weights.equipageW+
      Weights.fuelW+
      Weights.subsidiariesW+
      Weights.expected_down_commW+
      Weights.unexpected_down_commW+
      Weights.natural_elementsW+
      Weights.mechanicalW+
      Weights.distanceW;

   return case_comparison_score;
}

//---------------------Initialize Test Form-------------------------

/***
    Called from Repair Cases in the case class code
    Places real data from the three best cases selected
    into three separate fields in test form to allow 
    modification of the data
***/ 

void initialize_test_form(Case_Real_data mission, int i)
{ 

   char chtemp[128];
   FL_OBJECT *useme;
   FL_OBJECT *andme;
   
   if (i == 0) 
   {
      useme = fd_test->coa1;
      andme = fd_test->coa1info;
   } 
   if (i == 1)
   {
      useme = fd_test->coa2;
      andme = fd_test->coa2info;
   } 
   if( i == 2)
   {
      useme = fd_test->coa3; 
      andme = fd_test->coa3info;
   }

   fl_set_browser_fontsize(useme, 12);
   fl_set_browser_fontstyle(useme, 8);

   fl_set_browser_fontsize(andme, 12);
   fl_set_browser_fontstyle(andme, 8);
   
   Case_Real_data add_data;
  
   add_data = mission;
   
   sprintf(chtemp, "%d", add_data.departure_time, '\n');
   fl_addto_browser(useme, chtemp);
   fl_addto_browser(andme, "Departure Time is: ");
 
   sprintf(chtemp, "%d", add_data.ETA, '\n');
   fl_addto_browser(useme, chtemp);
   fl_addto_browser(andme, "Estimate Time of Arrival: ");

   sprintf(chtemp, "%f", add_data.travel_time, '\n');
   fl_addto_browser(useme, chtemp);
   fl_addto_browser(andme, "Estimated Travel Time (in hours) is: ");

//terrain type
 
   switch (add_data.terrain_type)
   {
      case 0: sprintf(chtemp, "Urban");
      break;
      case 1: sprintf(chtemp, "Range Land");
      break;
      case 2: sprintf(chtemp, "Barren Land");
      break;
      case 3: sprintf(chtemp, "Tundra");
      break;
      case 4: sprintf(chtemp, "Ice/Snow");
      break;
      case 5: sprintf(chtemp, "Wetland");
      break;
      case 6: sprintf(chtemp, "Forest");
   };
   fl_addto_browser(useme, chtemp);
   fl_addto_browser(andme, "Terrain Type: ");

   sprintf(chtemp, "%f", add_data.slope, '\n');
   fl_addto_browser(useme, chtemp);
   fl_addto_browser(andme, "Average Slope of Route: ");

   sprintf(chtemp, "%s",
           add_data.threats.threat_locations, '\n');
   fl_addto_browser(useme, chtemp);
   fl_addto_browser(andme, "Threat Locations: ");

   sprintf(chtemp, "%d", add_data.num_of_threats, '\n');
   fl_addto_browser(useme, chtemp);
   fl_addto_browser(andme, "Number of Threats in Route: ");

   sprintf(chtemp, "%f", add_data.strength_info.manpower, '\n');
   fl_addto_browser(useme, chtemp);
   fl_addto_browser(andme, "Threat Manpower (a %): ");
   
   sprintf(chtemp, "%f", add_data.strength_info.firepower, '\n');
   fl_addto_browser(useme, chtemp);
   fl_addto_browser(andme, "Threat Firepower (a %): ");

   sprintf(chtemp, "%f", add_data.distance, '\n');
   fl_addto_browser(useme, chtemp);
   fl_addto_browser(andme, "The Distance of the Route: ");

   sprintf(chtemp, "%s", 
          add_data.commodity_locations, '\n');
   fl_addto_browser(useme, chtemp);
   fl_addto_browser(andme, "Commodity Locations: ");

   sprintf(chtemp, "%s", add_data.fuel_locations, '\n');
   fl_addto_browser(useme, chtemp);
   fl_addto_browser(andme, "Fuel Locations: ");

   sprintf(chtemp, "%s", add_data.equipage_locations, '\n');
   fl_addto_browser(useme, chtemp);
   fl_addto_browser(andme, "Equipage Location(s): ");

   sprintf(chtemp, "%s", add_data.subsidiary_locations, '\n');
   fl_addto_browser(useme, chtemp);
   fl_addto_browser(andme, "Subsidiary Location(s): ");

   sprintf(chtemp, "%f", 
          add_data.chance_expec_down_comm, '\n');
   fl_addto_browser(useme, chtemp);
   fl_addto_browser(andme, "Chance of Expected down comm (a %): ");

   sprintf(chtemp, "%f", 
          add_data.chance_unexpec_down_comm, '\n');
   fl_addto_browser(useme, chtemp);
   fl_addto_browser(andme, "Chance of Unexpected down comm (a %): ");

   sprintf(chtemp, "%f", 
          add_data.chance_natural_elements, '\n');
   fl_addto_browser(useme, chtemp);
   fl_addto_browser(andme, "Chance of Failure due to Natural Elements (a %): ");

   sprintf(chtemp, "%f", 
          add_data.chance_mechanical, '\n');
   fl_addto_browser(useme, chtemp);
   fl_addto_browser(andme, "Chance of Mechanical Failure (a %): ");
}

/***
   If three cases aren't available to put in test form,
   Enter NONE instead of a case.
***/

void empty_coa (int i)
{
   fl_addto_browser(fd_test->coa3, "NONE");

   if (i == 1)
      fl_addto_browser(fd_test->coa2, "NONE");
}

