// link.C method file

#include <stdio.h>
#include "link.H"

/************************************************************************
* set_color : set up the color of the link				*
************************************************************************/
void C_LINK::set_color(int color) {

  switch(color) {

  case(SP_BLACK):
    set_black();
    break;

  case(SP_RED):
    set_red();
    break;

  case(SP_GREEN):
    set_green();
    break;

  case(SP_BLUE):
    set_blue();
    break;

  case(SP_YELLOW):
    set_yellow();
    break;

  case(SP_CYAN):
    set_cyan();
    break;

  case(SP_PURPLE):
    set_purple();
    break;

  case(SP_MAGENTA):
    set_magenta();
    break;

  case(SP_WHITE):
    set_white();
    break;

  }

}
