// ext_graphics_script.H header file

#ifndef exg_script_object
#define exg_script_object

#include "event.H"
#include "ext_graphics_script_output.H"
#include "ext_graphics_link_output.H"

/************************************************************************
*	ext_graphics_script object 					*
************************************************************************/
class C_EXT_GRAPHICS_SCRIPT : public C_EVENT {

  private:
    EXT_GRAPHICS_SCRIPT_OUTPUT *ext_graphics_script_output;
    EXT_GRAPHICS_LINK_OUTPUT *ext_graphics_link_output;

  protected:

  public:

    C_EXT_GRAPHICS_SCRIPT();
    virtual void init(C_HEADER *);
    virtual void temp_process();
    virtual void exchange();
    virtual void perm_process();
    virtual void cleanup();

};


#endif
