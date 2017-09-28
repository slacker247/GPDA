/*
Code to handle changes to test form in the areas:  Departure time, 
ETA, and Number of Threats

If the callback is called, the new value is taken from the Time Changer
form, the test form is updated with the new value, and then the
correct modification function is called in the case class code to 
change the value in the Case data structure

*/


#include "forms.h"
#include "time_changer.h"

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



