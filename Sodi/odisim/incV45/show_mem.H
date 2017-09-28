// show_mem.H header file

#ifndef show_mem_object
#define show_mem_object

#include "event.H"

/************************************************************************
*	show_mem object 							*
************************************************************************/
class C_SHOW_MEM : public C_EVENT {

  private:

    static int done;
    static int SHOW_MEM;
    static int GRID;

  protected:

  public:

    C_SHOW_MEM();
    virtual void init(C_HEADER *);
    virtual void exchange();
    virtual void temp_process();
    virtual void perm_process();
    virtual void cleanup();
    virtual int lazy() {return 1;}

};


#endif
