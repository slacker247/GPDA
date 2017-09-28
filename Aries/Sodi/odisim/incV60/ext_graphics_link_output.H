// ext_graphics_link_output.H message file

#ifndef eglo
#define eglo

#include <stdio.h>
#include <string.h>
#include <memory.h>
#include "headers.H"

enum {
  SP_BLACK,
  SP_RED,
  SP_GREEN,
  SP_BLUE,
  SP_YELLOW,
  SP_CYAN,
  SP_PURPLE,
  SP_MAGENTA,
  SP_WHITE,
  N_COLORS
};

class GLINK {
  public:
    int sensor_unique_id;
    int track_unique_id;
    int link_flag;
    int color;
    double intensity;
};

/************************************************************************
*	external graphics link output message	 			*
************************************************************************/
class EXT_GRAPHICS_LINK_OUTPUT : public C_EM_MODULE {

  public:

    EXT_GRAPHICS_LINK_OUTPUT() {strcpy(EM_command,"EXT_GRAPHICS_LINK");
	data_bytes = sizeof(EXT_GRAPHICS_LINK_OUTPUT) - sizeof(C_EM_HEADER);}

    void init(int size) {
	strcpy(EM_command,"EXT_GRAPHICS_LINK");
	data_bytes = sizeof(EXT_GRAPHICS_LINK_OUTPUT)
		- sizeof(C_EM_HEADER)
		+ size;
    }

    void init(char *buff, int size) {
        char *bf;
	strcpy(EM_command,"EXT_GRAPHICS_LINK");
	data_bytes = sizeof(EXT_GRAPHICS_LINK_OUTPUT)
		- sizeof(C_EM_HEADER)
		+ size;
        bf = (char *)this;
        bf += sizeof(EXT_GRAPHICS_LINK_OUTPUT);
        memcpy(bf, buff, size);
    }

};

#endif
