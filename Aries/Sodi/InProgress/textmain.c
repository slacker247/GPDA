#include <forms.h>
#include "textform.h"

main(int argc, char *argv[])
{
FD_textform   *fd_textform;

   fl_initialize(&argc, argv, 0, 0, 0);

fd_textform = create_form_textform();

fl_load_textedit(fd_textform->text, "fl_edit.c");

fl_show_form(fd_textform->textform, FL_PLACE_CENTER,FL_FULLBORDER, "Test");

   fl_do_forms();
}
