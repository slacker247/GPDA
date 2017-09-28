#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h> 
#include <time.h>
#include <signal.h>
#include <sys/ioctl.h>

#include "forms.h"
#include "CBPforms.h"

extern void CBPinit();

extern FD_cbpinput *fd_cbpinput;

int main(int argc, char *argv[])
{
int        i;

   fl_initialize(&argc, argv, 0, 0, 0);

   CBPinit();

   /* show the first form */
   fl_show_form(fd_cbpinput->cbpinput,FL_PLACE_CENTERFREE,FL_FULLBORDER,"cbpinput");
   fl_do_forms();
   return 0;
}

