// ext_graphics_kill_output.H message file

#ifndef egko
#define egko

#include <stdio.h>
#include <string.h>
#include <memory.h>
#include "headers.H"

/************************************************************************
*	external graphics kill output message	 			*
************************************************************************/
class EXT_GRAPHICS_KILL_OUTPUT : public C_EM_MODULE {

  public:

    int kill_object_id;

    EXT_GRAPHICS_KILL_OUTPUT() {strcpy(EM_command,"EXT_GRAPHICS_KILL");
	data_bytes = sizeof(EXT_GRAPHICS_KILL_OUTPUT) - sizeof(C_EM_HEADER);}

};

#endif
