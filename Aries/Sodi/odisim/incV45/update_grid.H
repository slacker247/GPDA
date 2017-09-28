// update_grid.H header file

#ifndef update_grid_object
#define update_grid_object

#include "event.H"

/************************************************************************
*	update_grid object 						*
************************************************************************/
class C_UPDATE_GRID : public C_EVENT {

  private:

    int nd;
    int gid;
    double delay_g2m;

//...... statics

    static int done;
    static int UPDATE_GRID;
    static int ADD_M2G;
    static int DEL_MFG;
    static int GRID;

  protected:

  public:

    C_UPDATE_GRID();
    virtual void init(C_HEADER *);
    virtual void exchange();
    virtual void temp_process();
    virtual void perm_process();
    virtual void cleanup();

};


#endif
