// ext_lanl_bp_mess.H message file

#ifndef extlanlbp_mess
#define extlanlbp_mess

#include <stdio.h>
#include <string.h>
#include <memory.h>
#include "headers.H"

/************************************************************************
*	external lanl_bp message	 				*
************************************************************************/
class EXT_LANL_BP_MESS : public C_EM_MODULE {

  public:

    EXT_LANL_BP_MESS() {strcpy(EM_command,"EXT_LANL_BP");
			data_bytes = sizeof(EXT_LANL_BP_MESS)
				   - sizeof(C_EM_HEADER);}

    void init(int size) {
	strcpy(EM_command,"EXT_LANL_BP");
	data_bytes = sizeof(EXT_LANL_BP_MESS) - sizeof(C_EM_HEADER) + size;}

    void init(char *buff, int size) {
        char *bf;
	strcpy(EM_command,"EXT_LANL_BP");
	data_bytes = sizeof(EXT_LANL_BP_MESS) - sizeof(C_EM_HEADER) + size;
        bf = (char *)this;
        bf += sizeof(EXT_LANL_BP_MESS);
        memcpy(bf, buff, size);}

};

#endif
