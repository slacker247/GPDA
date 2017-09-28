#include "forms.h"
#include "test.h"

FL_OBJECT *useme;

void modify_depart_time(FL_OBJECT *useme);
void modify_ETA(FL_OBJECT *useme);
void modify_travel_time(FL_OBJECT *useme);
void modify_terrain(FL_OBJECT *useme);
void modify_slope(FL_OBJECT *useme);
void modify_threat_locations(FL_OBJECT *useme);
void modify_num_threats(FL_OBJECT *useme);
void modify_manpower(FL_OBJECT *useme);
void modify_firepower(FL_OBJECT *useme);
void modify_distance(FL_OBJECT *useme);
void modify_commod_locat(FL_OBJECT *useme);
void modify_fuel_locat(FL_OBJECT *useme);
void modify_equipage_locat(FL_OBJECT *useme);
void modify_subsidiary_locat(FL_OBJECT *useme);
void modify_expec_down_comm(FL_OBJECT *useme);
void modify_unexpec_down_comm(FL_OBJECT *useme);
void modify_natural_elements(FL_OBJECT *useme);
void modify_mechanical(FL_OBJECT *useme);

int line;

void coa1CB(FL_OBJECT *ob, long data)
{
   useme = fd_test->coa1;

   line = fl_get_browser(fd_test->coa1);

   if (fl_form_is_visible(fd_changer->changer))
   fl_hide_form(fd_changer->changer);

   if (fl_form_is_visible(fd_time_changer->time_changer))
   fl_hide_form(fd_time_changer->time_changer);
   
   if (fl_form_is_visible(fd_terrain_changer->terrain_changer))
   fl_hide_form(fd_terrain_changer->terrain_changer);
   
   if (fl_form_is_visible(fd_float_changer->float_changer))
   fl_hide_form(fd_float_changer->float_changer);
   
   if (fl_form_is_visible(fd_char_changer->char_changer))
   fl_hide_form(fd_char_changer->char_changer);
   
   if (fl_form_is_visible(fd_add->add))
   fl_hide_form(fd_add->add);
   
   if (fl_form_is_visible(fd_remove->remove))
   fl_hide_form(fd_remove->remove);

   switch (line)
   {
      case 1:  modify_depart_time(useme); break;
      case 2:  modify_ETA(useme); break;
      case 3:  modify_travel_time(useme); break;
      case 4:  modify_terrain(useme); break;
      case 5:  modify_slope(useme); break;
      case 6:  modify_threat_locations(useme); break;
      case 7:  modify_num_threats(useme); break;
      case 8:  modify_manpower(useme); break;
      case 9:  modify_firepower(useme); break;
      case 10: modify_distance(useme); break;
      case 11: modify_commod_locat(useme); break;
      case 12: modify_fuel_locat(useme); break;
      case 13: modify_equipage_locat(useme); break;
      case 14: modify_subsidiary_locat(useme); break;
      case 15: modify_expec_down_comm(useme); break;
      case 16: modify_unexpec_down_comm(useme); break;
      case 17: modify_natural_elements(useme); break;
      case 18: modify_mechanical(useme); break;
   };

}

void coa2CB(FL_OBJECT *ob, long data)
{
   //FL_OBJECT *useme;

   //int line;
   useme = fd_test->coa2;

   line = fl_get_browser(fd_test->coa2);

   if (fl_form_is_visible(fd_changer->changer))
   fl_hide_form(fd_changer->changer);

   if (fl_form_is_visible(fd_time_changer->time_changer))
   fl_hide_form(fd_time_changer->time_changer);
   
   if (fl_form_is_visible(fd_terrain_changer->terrain_changer))
   fl_hide_form(fd_terrain_changer->terrain_changer);
   
   if (fl_form_is_visible(fd_float_changer->float_changer))
   fl_hide_form(fd_float_changer->float_changer);
   
   if (fl_form_is_visible(fd_char_changer->char_changer))
   fl_hide_form(fd_char_changer->char_changer);
   
   if (fl_form_is_visible(fd_add->add))
   fl_hide_form(fd_add->add);
   
   if (fl_form_is_visible(fd_remove->remove))
   fl_hide_form(fd_remove->remove);
   
   switch (line)
   {
      case 1:  modify_depart_time(useme); break;
      case 2:  modify_ETA(useme); break;
      case 3:  modify_travel_time(useme); break;
      case 4:  modify_terrain(useme); break;
      case 5:  modify_slope(useme); break;
      case 6:  modify_threat_locations(useme); break;
      case 7:  modify_num_threats(useme); break;
      case 8:  modify_manpower(useme); break;
      case 9:  modify_firepower(useme); break;
      case 10:  modify_distance(useme); break;
      case 11: modify_commod_locat(useme); break;
      case 12: modify_fuel_locat(useme); break;
      case 13: modify_equipage_locat(useme); break;
      case 14: modify_subsidiary_locat(useme); break;
      case 15: modify_expec_down_comm(useme); break;
      case 16: modify_unexpec_down_comm(useme); break;
      case 17: modify_natural_elements(useme); break;
      case 18: modify_mechanical(useme); break;
   };
}

void coa3CB(FL_OBJECT *ob, long data)
{
   useme = fd_test->coa3;

   line = fl_get_browser(fd_test->coa3);

   if (fl_form_is_visible(fd_changer->changer))
   fl_hide_form(fd_changer->changer);

   if (fl_form_is_visible(fd_time_changer->time_changer))
   fl_hide_form(fd_time_changer->time_changer);

   if (fl_form_is_visible(fd_terrain_changer->terrain_changer))
   fl_hide_form(fd_terrain_changer->terrain_changer);

   if (fl_form_is_visible(fd_float_changer->float_changer))
   fl_hide_form(fd_float_changer->float_changer);
   
   if (fl_form_is_visible(fd_char_changer->char_changer))
   fl_hide_form(fd_char_changer->char_changer);
   
   if (fl_form_is_visible(fd_add->add))
   fl_hide_form(fd_add->add);
   
   if (fl_form_is_visible(fd_remove->remove))
   fl_hide_form(fd_remove->remove);
   
   switch (line)
   {
      case 1:  modify_depart_time(useme); break;
      case 2:  modify_ETA(useme); break;
      case 3:  modify_travel_time(useme); break;
      case 4:  modify_terrain(useme); break;
      case 5:  modify_slope(useme); break;
      case 6:  modify_threat_locations(useme); break;
      case 7:  modify_num_threats(useme); break;
      case 8:  modify_manpower(useme); break;
      case 9:  modify_firepower(useme); break;
      case 10:  modify_distance(useme); break;
      case 11: modify_commod_locat(useme); break;
      case 12: modify_fuel_locat(useme); break;
      case 13: modify_equipage_locat(useme); break;
      case 14: modify_subsidiary_locat(useme); break;
      case 15: modify_expec_down_comm(useme); break;
      case 16: modify_unexpec_down_comm(useme); break;
      case 17: modify_natural_elements(useme); break;
      case 18: modify_mechanical(useme); break;
   };
}

void computeCB(FL_OBJECT *ob, long data)
{ 
   float Case_Selection();
   float score;
   Case_Real_data final_case;
   char chtemp[128];

   COAcases.fix_index(2);

   COAcases.set_case_values(Weights, 2);
   score = Case_Selection();
   final_case = COAcases.score_final(score);

   fl_clear_browser(fd_final->text_box1);

   fl_set_browser_fontsize(fd_final->text_box1, 14);
   fl_set_browser_fontstyle(fd_final->text_box1, 8);
 
   if (fl_form_is_visible(fd_test->test))
   fl_hide_form(fd_test->test);

   if (fl_form_is_visible(fd_changer->changer))
   fl_hide_form(fd_changer->changer);

   if (fl_form_is_visible(fd_time_changer->time_changer))
   fl_hide_form(fd_time_changer->time_changer);

   if (fl_form_is_visible(fd_terrain_changer->terrain_changer))
   fl_hide_form(fd_terrain_changer->terrain_changer);

   if (fl_form_is_visible(fd_float_changer->float_changer))
   fl_hide_form(fd_float_changer->float_changer);
   
   if (fl_form_is_visible(fd_char_changer->char_changer))
   fl_hide_form(fd_char_changer->char_changer);
   
   if (fl_form_is_visible(fd_add->add))
   fl_hide_form(fd_add->add);

   sprintf(chtemp, "Departure time is %d%s", final_case.departure_time,
           " hours.", '\n');
   fl_addto_browser(fd_final->text_box1, chtemp);

   sprintf(chtemp, "Estimated time of travel: %f%s", final_case.travel_time,
           "hours.", '\n');
   fl_addto_browser(fd_final->text_box1, chtemp);

   sprintf(chtemp, "Estimate time of arrival: %d%s", final_case.ETA, 
           "hours.", '\n');
   fl_addto_browser(fd_final->text_box1, chtemp);

   switch(final_case.terrain_type)
   {
      case 0: sprintf(chtemp, "Terrain to be traveled: Urban", '\n');
              break; 
      case 1: sprintf(chtemp, "Terrain to be traveled: Range Land", '\n');
              break;
      case 2: sprintf(chtemp, "Terrain to be traveled: Barren Land", '\n');
              break;
      case 3: sprintf(chtemp, "Terrain to be traveled: Tundra", '\n');
              break;
      case 4: sprintf(chtemp, "Terrain to be traveled: Ice/Snow", '\n');
              break;
      case 5: sprintf(chtemp, "Terrain to be traveled: Wetlands", '\n');
              break;
      case 6: sprintf(chtemp, "Terrain to be traveled: Forest", '\n');
   };
   fl_addto_browser(fd_final->text_box1, chtemp);

   sprintf(chtemp, "The average slope of the route: %f%s",
           final_case.slope, " percent.", '\n');
   fl_addto_browser(fd_final->text_box1, chtemp);

   sprintf(chtemp, "Trip distance: %f%s", final_case.distance,
           " miles.", '\n');
   fl_addto_browser(fd_final->text_box1, chtemp);
  
    sprintf(chtemp, "The number of threats expected: %d", 
           final_case.num_of_threats, '\n');   
   fl_addto_browser(fd_final->text_box1, chtemp);

   sprintf(chtemp, "The locations are: %s", final_case.
           threats.threat_locations, '\n');
   fl_addto_browser(fd_final->text_box1, chtemp);

   sprintf(chtemp, "Strength of threat manpower is: %f", final_case.
           strength_info.manpower, '\n');
   fl_addto_browser(fd_final->text_box1, chtemp);
   
   sprintf(chtemp, "Strength of threat firepower is: %f", final_case.
            strength_info.firepower, '\n');
   fl_addto_browser(fd_final->text_box1, chtemp);

   sprintf(chtemp, "Commodity (food, beverage, entertainment) locations: %s",final_case.commodity_locations, '\n');
   fl_addto_browser(fd_final->text_box1, chtemp);
  
   sprintf(chtemp, "Fuel locations: %s", final_case.fuel_locations, '\n'); 
   fl_addto_browser(fd_final->text_box1, chtemp);

   sprintf(chtemp, "Equipage (parts) locations: %s", final_case.
           equipage_locations, '\n'); 
   fl_addto_browser(fd_final->text_box1, chtemp);

   sprintf(chtemp, 
     "Subsidiary (reinforcements, medical personnel) locations: %s", final_case.
      subsidiary_locations, '\n');
   fl_addto_browser(fd_final->text_box1, chtemp);

   sprintf(chtemp, 
      "Chance of failure due to expected downned communications: %f%s", 
       final_case.chance_expec_down_comm, " percent.", '\n');
   fl_addto_browser(fd_final->text_box1, chtemp);

   sprintf(chtemp, 
       "Chance of failure due to unexpected downned communications: %f%s",
           final_case.chance_unexpec_down_comm, " percent.", '\n');
   fl_addto_browser(fd_final->text_box1, chtemp);

   sprintf(chtemp, "Chance of mission failure due to natural elements: %f%s",
           final_case.chance_natural_elements, " percent.", '\n');
   fl_addto_browser(fd_final->text_box1, chtemp);

   sprintf(chtemp, "Chance of mission failure due to mechanical failure: %f%s",
           final_case.chance_mechanical, " percent.", '\n');
   fl_addto_browser(fd_final->text_box1, chtemp);



   fl_show_form(fd_final->final, FL_PLACE_CENTER, FL_FULLBORDER, "");

}

void exit2CB(FL_OBJECT *ob, long data)
{
   fl_finish();
   exit(0);
}

void route1CB(FL_OBJECT *ob, long data)
{
  /* fill-in code for callback */
}

void route2CB(FL_OBJECT *ob, long data)
{
  /* fill-in code for callback */
}

void route3CB(FL_OBJECT *ob, long data)
{
  /* fill-in code for callback */
}

void coa1infoCB(FL_OBJECT *ob, long data)
{
  /* fill-in code for callback */
}

void coa2infoCB(FL_OBJECT *ob, long data)
{
  /* fill-in code for callback */
}

void coa3infoCB(FL_OBJECT *ob, long data)
{
  /* fill-in code for callback */
}


//------------------------modify functions-------------------------

void modify_depart_time(FL_OBJECT *useme)
{
   fl_set_form_position(fd_time_changer->time_changer, 50, 100);
   fl_set_input_maxchars(fd_time_changer->input, 4);

   fl_set_object_label(fd_time_changer->output1, "Departure time is when the Patriot
   fire unit will be leaving its current location for its destination.
   To record a change in departure time, please enter the new
   departure time, in military, integer format below.");

   fl_show_form(fd_time_changer->time_changer, FL_PLACE_POSITION, FL_FULLBORDER, "Departure Time");

}

void modify_ETA(FL_OBJECT *useme)
{

   fl_set_form_position(fd_time_changer->time_changer, 50, 100);
   fl_set_input_maxchars(fd_time_changer->input, 4);
   
   fl_set_object_label(fd_time_changer->output1, "ETA is the Estimated Time of Arrival
      for the Patriot unit at its destination following the current route.
      If ETA is altered by anything but departure time,
      please record change below in an integer value.");  
 
   fl_show_form(fd_time_changer->time_changer, FL_PLACE_POSITION, FL_FULLBORDER, "ETA");

}

void modify_travel_time(FL_OBJECT *useme)
{
   fl_set_form_position(fd_float_changer->float_changer, 50, 100);
   fl_set_input_maxchars(fd_float_changer->float_input, 5);
   
   fl_set_object_label(fd_float_changer->float_output, "The estimated travel
      time of the route.  To change, enter below.
      Based on new entry and departure time, ETA will
      be calculated.");
   
   fl_show_form(fd_float_changer->float_changer, FL_PLACE_POSITION, FL_FULLBORDER, "Estimated Travel Time");

}
void modify_terrain(FL_OBJECT *useme)
{
   fl_clear_choice(fd_terrain_changer->new_terrain);
   
   fl_set_form_position(fd_terrain_changer->terrain_changer, 50, 100);

   fl_set_object_label(fd_terrain_changer->terrain_output, "Terrain is the type of land the unit
    will be traveling over.  If by some act of a god it has changed, 
    please enter the new terrain type below");

   fl_addto_choice(fd_terrain_changer->new_terrain, "Urban");
   fl_addto_choice(fd_terrain_changer->new_terrain, "Range Land");
   fl_addto_choice(fd_terrain_changer->new_terrain, "Barren Land");
   fl_addto_choice(fd_terrain_changer->new_terrain, "Tundra");
   fl_addto_choice(fd_terrain_changer->new_terrain, "Perennial Ice/Snow");
   fl_addto_choice(fd_terrain_changer->new_terrain, "Wetland");
   fl_addto_choice(fd_terrain_changer->new_terrain, "Forest");

   fl_show_form(fd_terrain_changer->terrain_changer, FL_PLACE_POSITION, FL_FULLBORDER, "Terrain");
}

void modify_slope(FL_OBJECT *useme)
{

   fl_set_form_position(fd_float_changer->float_changer, 50, 100);
   fl_set_input_maxchars(fd_float_changer->float_input, 5);
   
   fl_set_object_label(fd_float_changer->float_output, "The slope is the average slope 
      traveled during the planned route to the destination.  If it has
      been modified somehow, please enter the new average slope  ");
   
   fl_show_form(fd_float_changer->float_changer, FL_PLACE_POSITION, FL_FULLBORDER, "Slope");
}

void modify_threat_locations(FL_OBJECT *useme)
{
   fl_set_form_position(fd_char_changer->char_changer, 50, 100);
   
   fl_set_object_label(fd_char_changer->char_output, "Threat Locations are enemy
      locations along the route.  To modify, choose add or remove
      a location below.");
   
   fl_show_form(fd_char_changer->char_changer, FL_PLACE_POSITION, FL_FULLBORDER, "Threats");
}

void modify_num_threats(FL_OBJECT *useme)
{
   fl_set_form_position(fd_changer->changer, 50, 100);

   fl_set_object_label(fd_changer->output, "To modify the total number of
      threats, add a new location to Threat Locations above. 
      This addition will update the number of threats."); 
   
   fl_show_form(fd_changer->changer, FL_PLACE_POSITION, FL_FULLBORDER, "Number");
}

void modify_manpower(FL_OBJECT *useme)
{
   fl_set_form_position(fd_float_changer->float_changer, 50, 100);
   fl_set_input_maxchars(fd_float_changer->float_input, 5);

   fl_set_object_label(fd_float_changer->float_output, "Manpower is a percentage of the
      enemy's troops compared to the number in the Patriot Battery.  
      If this ratio has changed please enter new value below.");
   
   fl_show_form(fd_float_changer->float_changer, FL_PLACE_POSITION, FL_FULLBORDER, "Manpower");
}

void modify_firepower(FL_OBJECT *useme)
{
   fl_set_form_position(fd_float_changer->float_changer, 50, 100);
   fl_set_input_maxchars(fd_float_changer->float_input, 5);

   fl_set_object_label(fd_float_changer->float_output, "Firepower is the percentage of the 
      enemy's weapons compared to the Fire Unit's.  If this too has 
      changed, please change below.");
   
   fl_show_form(fd_float_changer->float_changer, FL_PLACE_POSITION, FL_FULLBORDER, "Firepower");
}

void modify_distance(FL_OBJECT *useme)
{
   fl_set_form_position(fd_float_changer->float_changer, 50, 100);
   fl_set_input_maxchars(fd_float_changer->float_input, 5);

   fl_set_object_label(fd_float_changer->float_output, "Distance is the total miles covered
      by the Fire Unit.  If the route has changed, please enter the 
      new calculated distance here.");

   fl_show_form(fd_float_changer->float_changer, FL_PLACE_POSITION, FL_FULLBORDER, "Distance");
}

void modify_commod_locat(FL_OBJECT *useme)
{
   fl_set_form_position(fd_char_changer->char_changer, 50, 100);

   fl_set_object_label(fd_char_changer->char_output, "Commodity locations are areas 
      troops can find entertainment, food, and water.  To change,
      please chose add or remove locations below.");
   
   fl_show_form(fd_char_changer->char_changer, FL_PLACE_POSITION, FL_FULLBORDER, "Commodoties");
}

void modify_fuel_locat(FL_OBJECT *useme)
{
   fl_set_form_position(fd_char_changer->char_changer, 50, 100);

   fl_set_object_label(fd_char_changer->char_output, "Fuel locations are where the 
      Fire Unit can re-fuel along the route.  If locations are 
      altered please pick add or remove locations below");
   
   fl_show_form(fd_char_changer->char_changer, FL_PLACE_POSITION, FL_FULLBORDER, "Fuel");
}

void modify_equipage_locat(FL_OBJECT *useme)
{
   fl_set_form_position(fd_char_changer->char_changer, 50, 100);

   fl_set_object_label(fd_char_changer->char_output, "Equipage locations are where
      the Fire Unit can find necessary parts to fix machinary 
      items.  If these locations are modified, please change below. ");
   
   fl_show_form(fd_char_changer->char_changer, FL_PLACE_POSITION, FL_FULLBORDER, "Equipage");
}

void modify_subsidiary_locat(FL_OBJECT *useme)
{
   fl_set_form_position(fd_char_changer->char_changer, 50, 100);

   fl_set_object_label(fd_char_changer->char_output, "Subsidiary locations are where
      the Fire Unit can obtain reinforcments, medical help, 
      and other needed people.  To change, select appropriate
      button below.");
 
   fl_show_form(fd_char_changer->char_changer, FL_PLACE_POSITION, FL_FULLBORDER, "Subsidiaries");
}

void modify_expec_down_comm(FL_OBJECT *useme)
{
   fl_set_form_position(fd_float_changer->float_changer, 50, 100);
   fl_set_input_maxchars(fd_float_changer->float_input, 5);

   fl_set_object_label(fd_float_changer->float_output, "Expected downned communications
      is the known percent chance comm will be down.  Please 
      change below if necessary ");
   
   fl_show_form(fd_float_changer->float_changer, FL_PLACE_POSITION, FL_FULLBORDER, "Expected down communications");
}

void modify_unexpec_down_comm(FL_OBJECT *useme)
{
   fl_set_form_position(fd_float_changer->float_changer, 50, 100);
   fl_set_input_maxchars(fd_float_changer->float_input, 5);
   
   fl_set_object_label(fd_float_changer->float_output, "Unexpected downned communications
      is the percent chance comm could be knocked out.  If this is
      believed to have changed, input below. ");
   
   fl_show_form(fd_float_changer->float_changer, FL_PLACE_POSITION, FL_FULLBORDER, "Unexpected down communications");
}

void modify_natural_elements(FL_OBJECT *useme)
{
   fl_set_form_position(fd_float_changer->float_changer, 50, 100);
   fl_set_input_maxchars(fd_float_changer->float_input, 5);
   

   fl_set_object_label(fd_float_changer->float_output, "Natural Elements is the percent
      chance the mission will be effected by weather or other natural
      occurances.  Please enter changes below. ");
   
   fl_show_form(fd_float_changer->float_changer, FL_PLACE_POSITION, FL_FULLBORDER, "Natural Elements");
}

void modify_mechanical(FL_OBJECT *useme)
{
   fl_set_form_position(fd_float_changer->float_changer, 50, 100);
   fl_set_input_maxchars(fd_float_changer->float_input, 5);
   

   fl_set_object_label(fd_float_changer->float_output, "Mechanical is the percent chance
      equipment will fail.  If this number is thought to have changed,
      please modify below. ");
   
   fl_show_form(fd_float_changer->float_changer, FL_PLACE_POSITION, FL_FULLBORDER, "Mechanical");
}

