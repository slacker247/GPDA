// ext_graphics_script_output.H message file

#ifndef egso
#define egso

#include <stdio.h>
#include <string.h>
#include <memory.h>
#include "headers.H"

/************************************************************************
*	external graphics script output message	 			*
************************************************************************/
class EXT_GRAPHICS_SCRIPT_OUTPUT : public C_EM_MODULE {

  public:

    EXT_GRAPHICS_SCRIPT_OUTPUT() {strcpy(EM_command,"EXT_GRAPHICS_SCRIPT");
	data_bytes = sizeof(EXT_GRAPHICS_SCRIPT_OUTPUT) - sizeof(C_EM_HEADER);}

    void init(int size) {
	strcpy(EM_command,"EXT_GRAPHICS_SCRIPT");
	data_bytes = sizeof(EXT_GRAPHICS_SCRIPT_OUTPUT)
		- sizeof(C_EM_HEADER)
		+ size;
    }

    void init(char *buff, int size) {
        char *bf;
	strcpy(EM_command,"EXT_GRAPHICS_SCRIPT");
	data_bytes = sizeof(EXT_GRAPHICS_SCRIPT_OUTPUT)
		- sizeof(C_EM_HEADER)
		+ size;
        bf = (char *)this;
        bf += sizeof(EXT_GRAPHICS_SCRIPT_OUTPUT);
        memcpy(bf, buff, size);
    }

};

#endif
