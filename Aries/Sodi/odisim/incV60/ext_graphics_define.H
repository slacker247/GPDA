// ext_graphics_define.H header file

#ifndef exg_define_object
#define exg_define_object

#include "event.H"
#include "ext_graphics_define_output.H"
#include "ext_graphics_link_output.H"

/************************************************************************
*	ext_graphics_define object 					*
************************************************************************/
class C_EXT_GRAPHICS_DEFINE : public C_EVENT {

  private:

    EXT_GRAPHICS_DEFINE_OUTPUT ext_graphics_define_output;
    EXT_GRAPHICS_LINK_OUTPUT *ext_graphics_link_output;

  protected:

  public:

    C_EXT_GRAPHICS_DEFINE();
    virtual void init(C_HEADER *);
    virtual void temp_process();
    virtual void exchange();
    virtual void perm_process();
    virtual void cleanup();

};


#endif
