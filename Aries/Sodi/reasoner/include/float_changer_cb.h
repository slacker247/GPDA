#include "forms.h"
#include "float_changer.h"

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



