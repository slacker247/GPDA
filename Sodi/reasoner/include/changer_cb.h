#include "forms.h"
#include "changer.h"

/* callbacks for form changer */
void doneCB(FL_OBJECT *ob, long data)
{
  fl_hide_form(fd_changer->changer);
}

