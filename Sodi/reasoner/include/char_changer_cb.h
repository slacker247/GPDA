#include "forms.h"
#include "char_changer.h"

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



