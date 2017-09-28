//...... obman object

#include <stdio.h>
#include "object_types.H"

//...... global objects

#include "freeobjs.H"
C_FREEOBJS freeobjs;


//...... enumerate object types

enum {
  GRID,
  SENSOR,
  DSP,
  EYE,
  MISSILE,
  GBI,
  RANDOM_AIR,
  OAG,
  EOMAN,
  CENTER,
  COM,
  GRAPHICS,
  NOSTATE,
  N_OBJECT_TYPES
};


//...... object manager header files

#include "gridman.H"
#include "sensman.H"
#include "dspman.H"
#include "eyeman.H"
#include "missman.H"
#include "gbiman.H"
#include "ranman.H"
#include "oagman.H"
#include "eomanman.H"
#include "comman.H"
#include "centerman.H"
#include "graphman.H"
#include "nostateman.H"


//...... define creation functions for all of the simulation object managers

void *new_GRIDMAN() 			{return new C_GRIDMAN();}
void *new_SENSMAN() 			{return new C_SENSMAN();}
void *new_DSPMAN() 			{return new C_DSPMAN();}
void *new_EYEMAN() 			{return new C_EYEMAN();}
void *new_MISSMAN() 			{return new C_MISSMAN();}
void *new_GBIMAN() 			{return new C_GBIMAN();}
void *new_RANMAN() 			{return new C_RANMAN();}
void *new_OAGMAN()			{return new C_OAGMAN();}
void *new_EOMANMAN() 			{return new C_EOMANMAN();}
void *new_COMMAN() 			{return new C_COMMAN();}
void *new_CENTERMAN() 			{return new C_CENTERMAN();}
void *new_GRAPHMAN() 			{return new C_GRAPHMAN();}
void *new_NOSTATEMAN() 			{return new C_NOSTATEMAN();}


/************************************************************************
* C_OBJECT_TYPES : define the object managers for SPEEDES               *
************************************************************************/
C_OBJECT_TYPES::C_OBJECT_TYPES() {

//...... define the number of object managers

  set_ntypes(N_OBJECT_TYPES);

//...... define the object managers

  define_manager("GRID",	GRID,		&new_GRIDMAN);
  define_manager("SENSOR",	SENSOR,		&new_SENSMAN);
  define_manager("DSP",		DSP,		&new_DSPMAN);
  define_manager("EYE",		EYE,		&new_EYEMAN);
  define_manager("MISSILE",	MISSILE,	&new_MISSMAN);
  define_manager("GBI",		GBI,		&new_GBIMAN);
  define_manager("RANDOM_AIR",	RANDOM_AIR,	&new_RANMAN);
  define_manager("OAG",		OAG,		&new_OAGMAN);
  define_manager("EOMAN",	EOMAN,		&new_EOMANMAN);
  define_manager("COM",		COM,		&new_COMMAN);
  define_manager("CENTER",	CENTER,		&new_CENTERMAN);
  define_manager("GRAPHICS",	GRAPHICS,	&new_GRAPHMAN);
  define_manager("NOSTATE",	NOSTATE,	&new_NOSTATEMAN);

}

