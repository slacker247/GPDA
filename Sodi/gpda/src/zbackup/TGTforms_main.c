#include "forms.h"
#include "TGTforms.h"

int main(int argc, char *argv[])
{
   FD_targetstatus *fd_targetstatus;

   fl_initialize(&argc, argv, 0, 0, 0);
   fd_targetstatus = create_form_targetstatus();

   /* fill-in form initialization code */

   /* show the first form */
   fl_show_form(fd_targetstatus->targetstatus,FL_PLACE_CENTERFREE,FL_FULLBORDER,"targetstatus");
   fl_do_forms();
   return 0;
}
