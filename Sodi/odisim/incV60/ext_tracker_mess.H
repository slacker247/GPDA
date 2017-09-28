// ext_tracker_mess.H message file

#ifndef exttrackermess
#define exttrackermess

#include <stdio.h>
#include <string.h>
#include <memory.h>
#include "headers.H"

/************************************************************************
*	external lanl_bp message	 				*
************************************************************************/
class EXT_TRACKER_MESS : public C_EM_MODULE {

  public:

    int n_tracks;
    int n_tracks_updated;

    EXT_TRACKER_MESS() {strcpy(EM_command,"EXT_TRACKER");
			data_bytes = sizeof(EXT_TRACKER_MESS)
				   - sizeof(C_EM_HEADER);}

    void init(int size) {
	strcpy(EM_command,"EXT_TRACKER");
	data_bytes = sizeof(EXT_TRACKER_MESS) - sizeof(C_EM_HEADER) + size;}

    void init(char *buff, int size) {
        char *bf;
	strcpy(EM_command,"EXT_TRACKER");
	data_bytes = sizeof(EXT_TRACKER_MESS) - sizeof(C_EM_HEADER) + size;
        bf = (char *)this;
        bf += sizeof(EXT_TRACKER_MESS);
        memcpy(bf, buff, size);}

};

#endif
