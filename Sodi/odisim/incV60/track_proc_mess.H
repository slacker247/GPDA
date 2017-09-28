// track_proc_mess.H message file

#ifndef trkproc_mess
#define trkproc_mess

#include <stdio.h>
#include "headers.H"

/************************************************************************
*	process the track on stateless object message			*
************************************************************************/
class TRACK_PROC_MESS : public C_HEADER {

  public:

    int object_type;
    int object_id;
    int object_node;
    int n_tracks;

};

#endif
