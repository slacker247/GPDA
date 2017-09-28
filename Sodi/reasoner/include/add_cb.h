#include <iostream.h>
#include <ctype.h>

#include "forms.h"
#include "add.h"

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



