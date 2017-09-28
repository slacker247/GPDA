// graphobj.C method file

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "graphobj.H"
#include "def.h"

/************************************************************************
* C_GRAPHOBJ : construct a graphobj object				*
************************************************************************/
C_GRAPHOBJ::C_GRAPHOBJ() {
  char s[120];

  reset_blocking();
  socket = -1;

  sprintf(s,"GRAPHICS%3.3d",NODE);
  NAME = strdup(s);
  fprintf(stderr,"Graphics object greated with name %s (%d)\n",
	NAME, strlen(NAME));

}

