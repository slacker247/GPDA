/* Form definition file generated with fdesign. */

#include "forms.h"
#include <stdlib.h>
#include "textform.h"

FD_textform *create_form_textform(void)
{
  FL_OBJECT *obj;
  FD_textform *fdui = (FD_textform *) fl_calloc(1, sizeof(*fdui));

  fdui->textform = fl_bgn_form(FL_NO_BOX, 801, 441);
  obj = fl_add_box(FL_UP_BOX,0,0,801,441,"");
  fdui->text = obj = fl_add_textedit(FL_UP_BOX,10,10,780,420,"");
    fl_set_object_color(obj,FL_WHITE,FL_YELLOW);
  fl_end_form();

  fdui->textform->fdui = fdui;

  return fdui;
}
/*---------------------------------------*/

