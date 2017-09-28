// ext_graphics_define_output.H message file

#ifndef egdo
#define egdo

#include <stdio.h>
#include <string.h>
#include <memory.h>
#include "headers.H"

/************************************************************************
*	external graphics define output message	 			*
************************************************************************/
class EXT_GRAPHICS_DEFINE_OUTPUT : public C_EM_MODULE {

  public:

    double X[3];
    int icon;
    int unique_id;
    double Rmin;
    double Rmax;

    EXT_GRAPHICS_DEFINE_OUTPUT() {init();}

    void init() {
	strcpy(EM_command,"EXT_GRAPHICS_DEFINE");
	data_bytes = sizeof(EXT_GRAPHICS_DEFINE_OUTPUT) - sizeof(C_EM_HEADER);
    }

};

#endif
