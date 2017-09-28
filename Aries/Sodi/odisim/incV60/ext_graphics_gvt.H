// ext_graphics_gvt.H header file

#ifndef ext_graphics_gvt_object
#define ext_graphics_gvt_object

#include "event.H"
#include "ext_graphics_gvt_mess.H"

/************************************************************************
*	ext_graphics_gvt object 					*
************************************************************************/
class C_EXT_GRAPHICS_GVT : public C_EVENT {

  private:
    int blocking;
    int new_blocking;
    double EM_done_time;
    int EM_socket;
    int socket;
    EXT_GRAPHICS_GVT_MESS *ext_graphics_gvt_mess;

    int toggle;
    char *buff;
    C_ITEM *item;

  protected:

  public:

    C_EXT_GRAPHICS_GVT();
    ~C_EXT_GRAPHICS_GVT();
    virtual void init(C_HEADER *);
    virtual void temp_process();
    virtual void exchange();
    virtual void perm_process();
    virtual void cleanup();

};


#endif
