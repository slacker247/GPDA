// grid.C method file

#include <stdio.h>
#include <math.h>
#include "grid.H"

#ifndef INFINITY
#define INFINITY 1.0e20
#endif

/************************************************************************
* C_GRID : construct a grid object					*
************************************************************************/
C_GRID::C_GRID() {

  tmovers = -INFINITY;
  tsensors = -INFINITY;
  mover_counter = 0;
  sensor_counter = 0;

  glob_gid = -1;

}

/************************************************************************
* print : print the grid information					*
************************************************************************/
void C_GRID::print() {

  fprintf(stderr,"GRID %d, has %d units and %d sensors\n",
	glob_gid,
	movers.get_length(),
	sensors.get_length());

}

