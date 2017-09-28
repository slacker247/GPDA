// send_message_mess.H message file

#ifndef sendmess_mess
#define sendmess_mess

#include "headers.H"

/************************************************************************
*	send message message 						*
************************************************************************/
class SEND_MESSAGE_MESS : public C_HEADER {

  public:

    char source[40];
    char destination[40];
    char message_type[40];
    double time;
    int priority;
    int protocol;
    int nbytes;

};

#endif
