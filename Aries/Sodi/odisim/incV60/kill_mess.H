// kill_mess.H message file

#ifndef killmess
#define killmess

#include "headers.H"

/************************************************************************
*	kill message 							*
************************************************************************/
class KILL_MESS : public C_HEADER {

  public:

    double kill_position[3];
    double bomb_size;

};

#endif
