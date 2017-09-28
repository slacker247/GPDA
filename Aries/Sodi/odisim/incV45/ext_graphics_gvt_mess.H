// ext_graphics_gvt_mess.H message file

#ifndef extgraphgvt_mess
#define extgraphgvt_mess

#include <stdio.h>
#include <string.h>
#include <memory.h>
#include "headers.H"

/************************************************************************
*	external graphics_gvt message	 				*
************************************************************************/
class EXT_GRAPHICS_GVT_MESS : public C_EM_MODULE {

  public:

    EXT_GRAPHICS_GVT_MESS() {strcpy(EM_command,"EXT_GRAPHICS_GVT");
			data_bytes = sizeof(EXT_GRAPHICS_GVT_MESS)
				   - sizeof(C_EM_HEADER);}

    void init(int size) {
	strcpy(EM_command,"EXT_GRAPHICS_GVT");
	data_bytes = sizeof(EXT_GRAPHICS_GVT_MESS)
		- sizeof(C_EM_HEADER)
		+ size;}

    void init(char *buff, int size) {
        char *bf;
	strcpy(EM_command,"EXT_GRAPHICS_GVT");
	data_bytes = sizeof(EXT_GRAPHICS_GVT_MESS) - sizeof(C_EM_HEADER) +size;
        bf = (char *)this;
        bf += sizeof(EXT_GRAPHICS_GVT_MESS);
        memcpy(bf, buff, size);}

};

#endif
