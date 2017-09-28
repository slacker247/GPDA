// receive_message.H header file

#ifndef receive_message_object
#define receive_message_object

#include "event.H"
#include "receive_message_mess.H"

/************************************************************************
*	receive_message object 						*
************************************************************************/
class C_RECEIVE_MESSAGE : public C_EVENT {

  private:

    RECEIVE_MESSAGE_MESS *receive_message_mess;

  public:

    C_RECEIVE_MESSAGE();
    virtual void init(C_HEADER *);
    virtual void exchange();
    virtual void temp_process();
    virtual void perm_process();
    virtual void cleanup();
    virtual int lazy() {return 1;}

};


#endif
