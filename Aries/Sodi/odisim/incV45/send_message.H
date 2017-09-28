// send_message.H header file

#ifndef send_message_object
#define send_message_object

#include "event.H"
#include "send_message_mess.H"

/************************************************************************
*	send_message object 						*
************************************************************************/
class C_SEND_MESSAGE : public C_EVENT {

  private:

    SEND_MESSAGE_MESS *send_message_mess;

    static int done;
    static int RECEIVE_MESSAGE;
    static int SEND_MESSAGE;
    static int COM;

  protected:

  public:

    C_SEND_MESSAGE();
    virtual void init(C_HEADER *);
    virtual void exchange();
    virtual void temp_process();
    virtual void perm_process();
    virtual void cleanup();
    virtual int lazy() {return 1;}

};


#endif
