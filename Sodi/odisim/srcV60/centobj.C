// centobj.C method file

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "def.h"

#include "centobj.H"

/************************************************************************
* C_CENTOBJ : construct a centobj object				*
************************************************************************/
C_CENTOBJ::C_CENTOBJ() {

  stale_track_time = 500.0;
  remove_tracks_on = 0;
  time = -INFINITY;
  blocking = 0;

  gbis_used = NULL;
  gbi_index = 0;

}

