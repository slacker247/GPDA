#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <memory.h>

#include "xqueue.H"
#include "eom.H"
#include "random.H"
#include "headers.H"
#include "speedes_comm.H"
#include "stage.H"
#include "int_stage.H"
#include "bus.H"
#include "int_bus.H"
#include "greatcirc.H"
#include "stop.H"
#include "loiter.H"
#include "stereo_track.H"

#include "ext_tracker_mess.H"
#include "ext_lanl_bp_mess.H"
#include "ext_graphics_kill_output.H"
#include "ext_graphics_link_output.H"
#include "ext_graphics_script_mess.H"
#include "ext_graphics_script_output.H"
#include "ext_graphics_gvt_mess.H"
#include "ext_graphics_define_output.H"
#include "ext_graphics_define_mess.H"

#define NITS 10000000

#ifdef CPP20
#define EXTERN_C extern "C"
#else
#define EXTERN_C extern
#endif

EXTERN_C double walltime();

int main() {
  int i;
  C_RANDOM random;
  double temp;
  double wallclock0;
  double wallclock;

//...... print out basic variable type sizes

  fprintf(stderr,"Variable type sizes\n");
  fprintf(stderr,"sizeof(%s) = %d\n","short",sizeof(short));
  fprintf(stderr,"sizeof(%s) = %d\n","long",sizeof(long));
  fprintf(stderr,"sizeof(%s) = %d\n","int",sizeof(int));
  fprintf(stderr,"sizeof(%s) = %d\n","float",sizeof(float));
  fprintf(stderr,"sizeof(%s) = %d\n","double",sizeof(double));
  fprintf(stderr,"sizeof(%s) = %d\n","char",sizeof(char));
  fprintf(stderr,"sizeof(%s) = %d\n","char *",sizeof(char *));
  fprintf(stderr,"sizeof(%s) = %d\n","int *",sizeof(int *));
  fprintf(stderr,"\n");

//...... print out basic SPEEDES type sizes

  fprintf(stderr,"SPEEDES base class message types\n");
  fprintf(stderr,"sizeof(%s) = %d\n","MSG_STRUCT",sizeof(MSG_STRUCT));
  fprintf(stderr,"sizeof(%s) = %d\n","C_HEADER",sizeof(C_HEADER));
  fprintf(stderr,"sizeof(%s) = %d\n","C_EM_MODULE",sizeof(C_EM_MODULE));
  fprintf(stderr,"sizeof(%s) = %d\n","C_EM_COMMAND",sizeof(C_EM_COMMAND));
  fprintf(stderr,"\n");

//...... print out linked list item types

  fprintf(stderr,"Linked list item types\n");
  fprintf(stderr,"sizeof(%s) = %d\n","C_DUMMY_V",sizeof(C_DUMMY_V));
  fprintf(stderr,"sizeof(%s) = %d\n","C_ITEM",sizeof(C_ITEM));
  fprintf(stderr,"sizeof(%s) = %d\n","C_SQ_ITEM",sizeof(C_SQ_ITEM));
  fprintf(stderr,"sizeof(%s) = %d\n","C_QUEUE",sizeof(C_QUEUE));
  fprintf(stderr,"sizeof(%s) = %d\n","C_XQUEUE",sizeof(C_XQUEUE));
  fprintf(stderr,"\n");

//...... print out EOM types

  fprintf(stderr,"EOM types\n");
  fprintf(stderr,"sizeof(%s) = %d\n","C_EOM",sizeof(C_EOM));
  fprintf(stderr,"sizeof(%s) = %d\n","C_BASE_SPACE",sizeof(C_BASE_SPACE));
  fprintf(stderr,"sizeof(%s) = %d\n","C_BASE_STAGE",sizeof(C_BASE_STAGE));
  fprintf(stderr,"sizeof(%s) = %d\n","C_STAGE",sizeof(C_STAGE));
  fprintf(stderr,"sizeof(%s) = %d\n","C_KEPLER",sizeof(C_KEPLER));
  fprintf(stderr,"sizeof(%s) = %d\n","C_BUS",sizeof(C_BUS));
  fprintf(stderr,"sizeof(%s) = %d\n","C_INT_STAGE",sizeof(C_INT_STAGE));
  fprintf(stderr,"sizeof(%s) = %d\n","C_INT_BUS",sizeof(C_INT_BUS));
  fprintf(stderr,"sizeof(%s) = %d\n","C_STOP",sizeof(C_STOP));
  fprintf(stderr,"sizeof(%s) = %d\n","C_GREATCIRC",sizeof(C_GREATCIRC));
  fprintf(stderr,"sizeof(%s) = %d\n","C_LOITER",sizeof(C_LOITER));
  fprintf(stderr,"\n");

//...... print out stereo_track types

  fprintf(stderr,"stereo_track types\n");
  fprintf(stderr,"sizeof(%s) = %d\n","C_STEREO_TRACK",sizeof(C_STEREO_TRACK));
  fprintf(stderr,"sizeof(%s) = %d\n","C_DETECTION",sizeof(C_DETECTION));
  fprintf(stderr,"\n");

//...... print out stereo_track types

  fprintf(stderr,"external message types\n");
  fprintf(stderr,"sizeof(%s) = %d\n","EXT_TRACKER_MESS",
	sizeof(EXT_TRACKER_MESS));
  fprintf(stderr,"sizeof(%s) = %d\n","EXT_LANL_BP_MESS",
	sizeof(EXT_LANL_BP_MESS));
  fprintf(stderr,"sizeof(%s) = %d\n","EXT_GRAPHICS_KILL_OUTPUT",
	sizeof(EXT_GRAPHICS_KILL_OUTPUT));
  fprintf(stderr,"sizeof(%s) = %d\n","EXT_GRAPHICS_LINK_OUTPUT",
	sizeof(EXT_GRAPHICS_LINK_OUTPUT));
  fprintf(stderr,"sizeof(%s) = %d\n","GLINK",
	sizeof(GLINK));
  fprintf(stderr,"sizeof(%s) = %d\n","EXT_GRAPHICS_SCRIPT_MESS",
	sizeof(EXT_GRAPHICS_SCRIPT_MESS));
  fprintf(stderr,"sizeof(%s) = %d\n","EXT_GRAPHICS_SCRIPT_OUTPUT",
	sizeof(EXT_GRAPHICS_SCRIPT_OUTPUT));
  fprintf(stderr,"sizeof(%s) = %d\n","EXT_GRAPHICS_GVT_MESS",
	sizeof(EXT_GRAPHICS_GVT_MESS));
  fprintf(stderr,"sizeof(%s) = %d\n","EXT_GRAPHICS_DEFINE_OUTPUT",
	sizeof(EXT_GRAPHICS_DEFINE_OUTPUT));
  fprintf(stderr,"sizeof(%s) = %d\n","EXT_GRAPHICS_DEFINE_MESS",
	sizeof(EXT_GRAPHICS_DEFINE_MESS));
  fprintf(stderr,"\n");

//...... timing loop test

  temp = 1.0;
  wallclock0 = walltime();
  for (i=0; i<NITS; i++) {
    temp /= 1.0000000012345;
  }
  wallclock = walltime() - wallclock0;
  fprintf(stderr,"Wall time for %d iterations is %f sec. with value %f\n",
	NITS,wallclock, temp);

  return 1;

}
