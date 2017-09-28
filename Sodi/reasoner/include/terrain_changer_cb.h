#include "forms.h"
#include "terrain_changer.h"

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



