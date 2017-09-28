// oagman.C method file

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h>
#include "oagman.H"
#include "def.h"
#include "defunc.H"
//#include "speedes_com.H"

#include "sensor_model.H"

extern int NEXT_SCRIPT;
extern int OAG;
extern int UPDATE_GRID;
extern int EXT_GRAPHICS_SCRIPT;
   
/************************************************************************
* C_OAGMAN : construct a oagman object					*
************************************************************************/
C_OAGMAN::C_OAGMAN() {
#ifdef COMMENTOUT
  int i, j, index;
  int offset;
  int sequence = 0;
  C_BASETYPE *basetype;

  int n_flights;
  char *oag_file_name;
  FILE *oag_fp;

  char keyw[80];
  int plat_id;
  char plat_name[80], *cp;
  int n_plats, n_recs, obj_type;
  float t_offset;

  float start_t, start_lat, start_lon;
  int start_alt;
  float start_altitude;

  float end_t, end_lat, end_lon;
  int end_alt;
  float end_altitude;


  OAG = object_type("OAG");

  basetype = oag_parser->get_basetype("parameters");
  oag_file_name = basetype->get_string("file_name");
  N_TOT = n_flights = basetype->get_int("n_flights");

//...... construct the oag simulation objects

  N_LOC =  my_flights = deal_me( n_flights, offset );
  oagobj = new C_OAGOBJ[ my_flights ];
  printf("OAGMAN got file name %s and %d(%d) flights\n",
	 oag_file_name, n_flights, my_flights );

//...... initialize the oagobj objects

  oag_fp = fopen( oag_file_name, "r" );
  if  ( oag_fp == NULL )  {
    fprintf( stderr, "Failed to open OAG file\n" );
    exit(0);
  }

  index = 0;
  for  ( i=0; i<n_flights; i++ )  {
    fscanf( oag_fp, "%s %d\n", keyw,  &plat_id );
  /*  fscanf( oag_fp, "     %26c\n", plat_name );*/
    while  ( getc(oag_fp) != '\'' )  /*no-op*/;
    cp = plat_name;
    while  ( (*cp++ = getc(oag_fp)) != '\'' )  /*no-op*/;
    *(--cp) = '\0';
    fscanf( oag_fp, "%d %f %d %d\n", &n_plats, &t_offset, &n_recs, &obj_type );
#ifdef DEBUG
    fprintf( stderr, "%s %d\n", keyw,  plat_id );
    fprintf( stderr, "%s\n", plat_name );
    fprintf( stderr, "%d %f %d %d\n", n_plats, t_offset, n_recs, obj_type );
#endif
    if  ( n_recs <= 1 )  {
      fprintf( stderr, "Not enough flights records\n" );
      exit(0);
    }
  
    fscanf( oag_fp, "%f %f %f %d\n", &start_t, &start_lat, &start_lon, &start_alt );
#ifdef DEBUG
    fprintf( stderr, "%f %f %f %d\n", start_t, start_lat, start_lon, start_alt );
#endif
    start_t *= 3600.0;
    start_lat *= RADDEG;
    start_lon *= RADDEG;
    start_altitude = start_alt * 0.03048;  //  conversion from hundreds of feet to km
  
    n_recs--;
    sequence = 0;
    for  ( j=0; j<n_recs; j++ )  {
      fscanf( oag_fp, "%f %f %f %d\n", &end_t, &end_lat, &end_lon, &end_alt );
#ifdef DEBUG
      fprintf( stderr, "%f %f %f %d\n", end_t, end_lat, end_lon, end_alt );
#endif
      end_t *= 3600.0;
      end_lat *= RADDEG;
      end_lon *= RADDEG;
      end_altitude = end_alt * 0.03048;
  
      if ((i-offset+N_NODES)%N_NODES == 0) {
        oagobj[index].init_oagobj( sequence,
			start_lat, start_lon, start_altitude,
			end_lat, end_lon, end_altitude,
			start_t, end_t );
      }
      sequence++;
      start_lat = end_lat;
      start_lon = end_lon;
      start_altitude = end_altitude;
      start_t = end_t;
    }
    if ((i-offset+N_NODES)%N_NODES == 0) {
      index++;
    }
  }

//...... tell SPEEDES about the oagobj

  TOTAL_OBJECTS += N_TOT;
  reserve_n_objects( my_flights );
  for  ( i=0; i<my_flights; i++ )  define_object( &oagobj[i] );
  set_interact();

  printf("OAGMAN created %4d flights\n", my_flights );

#endif
}

/************************************************************************
* init_events : initialize events for the grid element objects		*
************************************************************************/
void C_OAGMAN::init_events() {
  int i;
  C_EOM *e;
  int NEXT_SCRIPT;
  int EXT_GRAPHICS_SCRIPT;
  int UPDATE_GRID;

  printf("OAGMAN initializing ...\n");

  NEXT_SCRIPT	  = event_type("NEXT_SCRIPT");
  EXT_GRAPHICS_SCRIPT  = event_type("EXT_GRAPHICS_SCRIPT");
  UPDATE_GRID	  = event_type("UPDATE_GRID");

  for (i=0; i<my_flights; i++) {
    e = oagobj[i].get_current_segment();
    if (e != NULL) {
      schedule( e->get_endtime(), NEXT_SCRIPT, OAG, i,
		NODE);
      schedule( e->get_start_time()-0.0, UPDATE_GRID,
		OAG, i, NODE);
      schedule( e->get_start_time(), EXT_GRAPHICS_SCRIPT,
		OAG, i, NODE );
    }
  }
}
