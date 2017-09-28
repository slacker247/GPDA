#include "forms.h"
#include "graphs.h"

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



