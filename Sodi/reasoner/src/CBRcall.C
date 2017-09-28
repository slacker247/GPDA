#include <iostream.h>
#include <ctype.h>

#include "forms.h"
#include "CBRforms.h"

/* callbacks for form add */

/***
    When a user enters data into the field to add a location,
    the form is told not to call the call back until the user 
    specifies by clicking the done button
***/
   
void add_locCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(useme, FL_RETURN_END_CHANGED);
}

/***
   The button is pushed so the information displyed in the 
   appropriate field in the test form needs to be updated
   as well as the information in the case real data stored
   in nodes in the linked list
***/

void add_nowCB(FL_OBJECT *ob, long data)
{
   ifstream in;
   ofstream out;
   int i;
   const char *get_char;
   char char_me; 
   char chtemp[MAX2];
   char_return char_got;
   int num = 0;

//tells what field has information being changed
   if (useme == fd_test->coa1)
      i = 1;
   if (useme == fd_test->coa2)
      i = 2;
   if (useme == fd_test->coa3)
      i = 3;

//get the character from the field to be removed
   get_char = fl_get_input(fd_add->add_loc);

//save it to a file, then get it back out to convert the
//character from "const char" to just "char" for
//error checking purposes

   out.open("save_char");
   out << get_char;
   out.close();

   in.open("save_char");
   in >> char_me;
   in.close();

//make all chars uppercase
   char_me = toupper(char_me);

//make sure it is a legal character to add
   if ((strlen(get_char) > 1) || (char_me < 'A') || (char_me > 'Z'))
   {
      fl_set_object_label(fd_add->add_output, "That is not a legal character,
         please re-enter.");
   
      fl_set_input(fd_add->add_loc, "");
   }

//if it is, add it to real data in case storage and change
//the viewed data on the "test" form

   else
   {

//Adds the character from the string (char array) in the appropriate case
//and returns the string with the added location for updating the
//test form

      char_got =  COAcases.modify_case_add_char(i, get_char, char_me, line);
 
      for (int j = 0; j < MAX2; j++)
         char_got.char_temp[j] = toupper(char_got.char_temp[j]);

//updates the test form with the string with the added locations   
      fl_replace_browser_line(useme, line, char_got.char_temp);

      fl_set_input(fd_add->add_loc, "");
      fl_hide_form(fd_add->add);

//If adding a location to threat locations
//update the number of threats

      if (line == 6)
      {
         num = 0;
         for (int y = 0; y < MAX2; y++)
         { 
            if ((char_got.char_temp[y] >= 'A') && 
                (char_got.char_temp[y] <= 'Z'))
            num = num + 1;
         } 

         sprintf(chtemp, "%d", num);
         fl_replace_browser_line(useme, 7, chtemp); 

         COAcases.modify_case_int(i, num, 7);
     

//Notify user the number of threats has been updated since the location
//of threats was changed
 
         fl_set_object_label(fd_changer->output, "The number of threats in 
            the route has been corrected for the 
            type of input.");
    
         fl_show_form(fd_changer->changer, FL_PLACE_CENTER, FL_FULLBORDER, "");
      } 

   }
}

/***
    If the user cancels the add, hide the form and go back to "test"
***/

void add_cancelCB(FL_OBJECT *ob, long data)
{
   fl_hide_form(fd_add->add);
}


/* callbacks for form changer */
void doneCB(FL_OBJECT *ob, long data)
{
  fl_hide_form(fd_changer->changer);
}


/* callbacks for form char_changer */
void char_remove(FL_OBJECT *ob, long data)
{
   fl_set_form_position(fd_remove->remove, 200, 100);

   fl_set_object_label(fd_remove->remove_output, "Enter a location to
      remove.");

   fl_show_form(fd_remove->remove, FL_PLACE_POSITION, FL_FULLBORDER,
 "Remove Location");
}

void char_add(FL_OBJECT *ob, long data)
{
   fl_set_form_position(fd_add->add, 200, 100);
   
   fl_set_object_label(fd_add->add_output, "Enter a location to
      add.");
  
 fl_show_form(fd_add->add, FL_PLACE_POSITION, FL_FULLBORDER,
 "Add Location");

}

void char_never_mind(FL_OBJECT *ob, long data)
{
  fl_hide_form(fd_char_changer->char_changer);
}


/* callbacks for form final */

//-----------------cancel call back---------------------------

//ends the program

void cancel_finalCB(FL_OBJECT *ob, long data)
{
   fl_hide_form(fd_final->final);
   fl_finish();
   exit(0);
}

//-----------------accept call back----------------------------

//User accepts the case so the graph form is set up
//to show how the cases compare to each other and
//which one was chosen

void acceptCB(FL_OBJECT *ob, long data)
{
   int color1, color2, color3;
   float highest = 0;
   float second = 100;
   float third = 100;
   int i = 0;          //loop control variable
   char chtemp[128];   //temp array to output characters to X-Form form

/***clears all graphs on graphs form*/
   fl_clear_chart(fd_graphs->graph1);
   fl_clear_chart(fd_graphs->graph2);
   fl_clear_chart(fd_graphs->graph3);

 
/***sets labels to what I want them to say*/

   sprintf(chtemp, "%s", "Total case 
points ordered
as read in from 
case base.  
Higher means
a better route.");

   fl_set_object_label(fd_graphs->text1, chtemp);

   sprintf(chtemp, "%s", "Total points 
that match
user criteria.  
Higher is a 
better match."); 

   fl_set_object_label(fd_graphs->text2, chtemp);
   
   sprintf(chtemp, "%s", "Of the three 
best matching
cases, total 
matching points 
to given
criteria.  
Highest is 
best case.");

   fl_set_object_label(fd_graphs->text3, chtemp);

/*****
      Saves cases:  Writes all original cases to a file
                    and best case too if it is not an exact
                    copy of a previous case
*****/

   COAcases.save_cases();


/***gets points to plot on graphs*/

   point_return plotme;

   plotme = COAcases.plotme();

/*plot points on graph on X-Forms form graphs*/

   while ((plotme.total_points[i] > 0) && (plotme.total_points[i] < 20))
   {       
   
   fl_add_chart_value(fd_graphs->graph1, plotme.total_points[i], " ", 1+i);
 
   if (plotme.total_points[i] > highest)
   {
       highest = plotme.total_points[i];
       color1 = i+1;  
   }

   else if ((plotme.total_points[i] > second) && (plotme.total_points[i] < highest))
   {
      second = plotme.total_points[i];
      color2 = i+1;
   }

   else if((plotme.total_points[i] > third) && (plotme.total_points[i] < second)
       && (plotme.total_points[i] < highest))
   {
      third = plotme.total_points[i];
      color3 = i+1;
   }

 
   fl_add_chart_value(fd_graphs->graph2, plotme.match_points[i], " " , 1+i);


   i++;
   }

   plotme = COAcases.plotmetoo();


         fl_add_chart_value(fd_graphs->graph3, plotme.match_points[0], " ", 
                            color1);

         fl_add_chart_value(fd_graphs->graph3, plotme.match_points[1], " ", 
                            color1);

         fl_add_chart_value(fd_graphs->graph3, plotme.match_points[2], " ", 
                            color2);

   fl_show_form(fd_graphs->graphs, FL_PLACE_CENTER, FL_FULLBORDER, " ");
}

//----------------------start over call back-----------------------

//If the user wants to start over this will clear all forms from the
//screen, sets data to original, and goes back to the first form

void start_overCB(FL_OBJECT *ob, long data)
{
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
   
   if (fl_form_is_visible(fd_final->final))
   fl_hide_form(fd_final->final);


   fl_clear_browser(fd_test->coa1);
   fl_clear_browser(fd_test->coa2);
   fl_clear_browser(fd_test->coa3);
   
   fl_clear_browser(fd_test->coa1info);
   fl_clear_browser(fd_test->coa2info);
   fl_clear_browser(fd_test->coa3info);
   
   fl_show_form(fd_criteria->criteria, FL_PLACE_CENTER, FL_FULLBORDER, "");

}


/* callbacks for form float_changer */

//--------------------float_input call back------------------------

/***
    When a user enters data into the field to add a location,
    the form is told not to call the call back until the user
    specifies by clicking the done button
***/

void float_inputCB(FL_OBJECT *ob, long data)
{
   fl_set_input_return(useme, FL_RETURN_END_CHANGED);
}

//-------------------nevermind call back-------------------------

/***
    If the user cancels the add, hide the form and go back to "test"
***/

void nevermind3CB(FL_OBJECT *ob, long data)
{
   fl_set_input(fd_float_changer->float_input, "");
   fl_hide_form(fd_float_changer->float_changer);
} 



void done4CB(FL_OBJECT *ob, long data)
{

   int i;
   const char *get_str;
   char chtemp[128];
   float input_data;

   if (useme == fd_test->coa1)
      i = 1;
   if (useme == fd_test->coa2)
      i = 2;
   if (useme == fd_test->coa3)
      i = 3;

   get_str = fl_get_input(fd_float_changer->float_input);
   input_data = atof(get_str);

//if line is a distance or slope, make sure it is acceptable data ( >0)

   if ((line == 9) || (line == 3))
   {
      if (input_data < 0)
      {
 
         fl_set_object_label(fd_float_changer->float_output, "That is an
                invalid distance or time, plese 
                enter a positive value");

         fl_set_input(fd_float_changer->float_input, "");
      }

//if it is ok, update form and appropriate case real data
      else
      {
         COAcases.modify_case_float(i, input_data, get_str, line);

         sprintf(chtemp, "%f", input_data, '\n');
         fl_replace_browser_line(useme, line, chtemp);

         fl_set_input(fd_float_changer->float_input, "");
         fl_hide_form(fd_float_changer->float_changer);
      }
   }
  
//otherwise if it is a percent value from the form, make sure
//it is within percent limits
 
   else
   { 
   if ((input_data < 0) || (input_data > 100))
   {
     fl_set_object_label(fd_float_changer->float_output, "That is an invalid
          departure time, please enter a number 0 to 100
          to represent a percent value moron.");

      fl_set_input(fd_float_changer->float_input, "");
   }

//Then, when ok, change appropriate case and test form
   else
   {
      COAcases.modify_case_float(i, input_data, get_str, line);

      sprintf(chtemp, "%f", input_data, '\n');
      fl_replace_browser_line(useme, line, chtemp);

      fl_set_input(fd_float_changer->float_input, "");
      fl_hide_form(fd_float_changer->float_changer);
   }
   }
}


/***
    The graphs form shows the bar graphs of case comparisons
    and although there is no user interaction, call backs
    for each graph field need to be shown for the program to
    compile.  When the done button is pushed, the program
    will remove the forms and end the program
***/


void graph1CB(FL_OBJECT *ob, long data)
{
  /* fill-in code for callback */
}

void graph2CB(FL_OBJECT *ob, long data)
{
  /* fill-in code for callback */
}

void graph3CB(FL_OBJECT *ob, long data)
{
  /* fill-in code for callback */
}

void graph_doneCB(FL_OBJECT *ob, long data)
{
   fl_hide_form(fd_graphs->graphs);

   fl_finish();
   exit(0);


}


/* callbacks for form remove */

/***
   The button is pushed so the information displyed in the
   appropriate field in the test form needs to be updated
   as well as the information in the case real data stored
   in nodes in the linked list
***/


void remove_removeCB(FL_OBJECT *ob, long data)
{
   ifstream in;
   ofstream out;
   char_return char_got;
   int i;
   const char *get_char;
   char char_no_pointer;
   char chtemp[MAX2];
   int num = 0;

   if (useme == fd_test->coa1)
      i = 1;
   if (useme == fd_test->coa2)
      i = 2;
   if (useme == fd_test->coa3)
      i = 3;

//get the character from the field to be removed 
   get_char = fl_get_input(fd_remove->remove_input);


//save it to a file, then get it back out to convert the
//character from "const char" to just "char" for
//error checking purposes

   out.open("save_char");
   out << get_char;
   out.close();

   in.open("save_char");
   in >> char_no_pointer;
   in.close();

//make all chars uppercase
   char_no_pointer = toupper(char_no_pointer);

//make sure it is a legal character to remove

   if ((strlen(get_char) > 1) || (char_no_pointer < 'A') || 
       (char_no_pointer > 'Z'))
   {
      fl_set_object_label(fd_remove->remove_output, 
             "That is not a legal character,
              please re-enter.");

      fl_set_input(fd_remove->remove_input, "");
   }

//if it is, remove it from real data in case storage and change
//the viewed data on the "test" form
   else
   {

//Removes the character from the string (char array) in the appropriate case
//and returns the string without the removed location for updating the
//test form

      char_got =  COAcases.modify_case_rem_char(i, get_char, char_no_pointer, line);

      for (int j = 0; j < MAX2; j++)
         char_got.char_temp[j] = toupper(char_got.char_temp[j]);

//updates test form with string without location removed

      fl_replace_browser_line(useme, line, char_got.char_temp);

      fl_set_input(fd_remove->remove_input, "");
      fl_hide_form(fd_remove->remove);

//If removing a location from threat locations
//update the number of theats

      if (line == 6)
      {

         num = 0;
         for (int y = 0; y < MAX2; y++)
         {
            if ((char_got.char_temp[y] >= 'A') &&
                (char_got.char_temp[y] <= 'Z'))
            num = num + 1;
         }

         sprintf(chtemp, "%d", num);
         fl_replace_browser_line(useme, 7, chtemp);

         COAcases.modify_case_int(i, num, 7);

//Notify user the number of threats has been updated since the location
//of threats was changed

         fl_set_object_label(fd_changer->output, "The number of threats in
            the route has been updated.");

         fl_show_form(fd_changer->changer, FL_PLACE_CENTER, FL_FULLBORDER, "");
      }
   }


}

/***
    If the user cancels the add, hide the form and go back to "test"
***/

void remove_cancelCB(FL_OBJECT *ob, long data)
{
   fl_hide_form(fd_remove->remove);
}


/***
    When a user enters data into the field to remove a location,
    the form is told not to call the call back until the user
    specifies by clicking the done button
***/


void remove_inputCB(FL_OBJECT *ob, long data)
{
 fl_set_input_return(useme, FL_RETURN_END_CHANGED);
}


/*needed to make compiler happy*/
void new_terrainCB(FL_OBJECT *ob, long data)
{

}

//--------------------nevermind2 call back---------------------

/***
    If the user cancels the add, hide the form and go back to "test"
***/

void nevermind2(FL_OBJECT *ob, long data)
{
   fl_hide_form(fd_terrain_changer->terrain_changer);
}

//-----------------------done3 call back-------------------------

/***
   The button is pushed so the information displyed in the
   appropriate field in the test form needs to be updated
   as well as the information in the case real data stored
   in nodes in the linked list
***/

void done3CB(FL_OBJECT *ob, long data)
{
   int i;
   int choice;

//tells what field has information being changed
   if (useme == fd_test->coa1)
      i = 1;
   if (useme == fd_test->coa2)
      i = 2;
   if (useme == fd_test->coa3)
      i = 3;

//get the menu option selected by the user
   choice = fl_get_choice(fd_terrain_changer->new_terrain);


//change it to the enumerated value
   switch (choice)
   {
      case 1: fl_replace_browser_line(useme, line,  "Urban");
      break;
      case 2: fl_replace_browser_line(useme, line,  "Range Land");
      break;
      case 3: fl_replace_browser_line(useme, line, "Barren Land");
      break;
      case 4: fl_replace_browser_line(useme, line,"Tundra");
      break;
      case 5: fl_replace_browser_line(useme, line, "Ice/Snow");
      break;
      case 6: fl_replace_browser_line(useme, line, "Wetland");
      break;
      case 7: fl_replace_browser_line(useme, line, "Forest");
   };

//modify the case in the linked list
  COAcases.modify_case_terrain(i, choice);

//done, hide the form
  fl_hide_form(fd_terrain_changer->terrain_changer);
}


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

/*
Code to handle changes to test form in the areas:  Departure time, 
ETA, and Number of Threats

If the callback is called, the new value is taken from the Time Changer
form, the test form is updated with the new value, and then the
correct modification function is called in the case class code to 
change the value in the Case data structure

*/


//--------------------------input call back-----------------------

/***When input is received, form is notified***/

void inputCB(FL_OBJECT *ob, long data)
{
fl_set_input_return(useme, FL_RETURN_END_CHANGED);
}

//-----------------------done2 call back------------------------------

/***The user has notified the form he is done inserting new data
and the forms can be updated***/

void done2CB(FL_OBJECT *ob, long data)
{
   int i;
   const char *get_str;
   char chtemp[128];
   int input_data;

//specifies field on test form to be modified with new data

   if (useme == fd_test->coa1)
      i = 1;
   if (useme == fd_test->coa2) 
      i = 2;
   if (useme == fd_test->coa3) 
      i = 3;


//get string of user entered data and change it to an integer

   get_str = fl_get_input(fd_time_changer->input);
   input_data = atoi(get_str);


//error checking -- is input data a valid time

   if ((input_data < 0) || (input_data) > 2400)
   {
      fl_set_object_label(fd_time_changer->output1, "That is an invalid
          departure time, please re-enter moron.");
   
      fl_set_input(fd_time_changer->input, "");
   }


//if it is valid, update case real data in appropriate case and
//update field on test form

   else
   {
      COAcases.modify_case_int(i, input_data, line);
      
      sprintf(chtemp, "%d", input_data, '\n');
      fl_replace_browser_line(useme, line, chtemp);

//sets the time_changer form input area back to blank
//so if it is called again no data is already present

      fl_set_input(fd_time_changer->input, "");
      fl_hide_form(fd_time_changer->time_changer);
   }
}


//-------------------------nevermind call back-----------------------

/*cancels the opening of the form*/

void nevermindCB(FL_OBJECT *ob, long data)
{
  
  fl_set_input(fd_time_changer->input, "");
  fl_hide_form(fd_time_changer->time_changer);
}



