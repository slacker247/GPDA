// gridid.H header file

#ifndef gridid_object
#define gridid_object

#include "sq_item.H"

/************************************************************************
*	gridid object 							*
************************************************************************/
class C_GRIDID : public C_SQ_ITEM {

  private:

    int gid;
    int node;
    double delay_g2m;

  protected:

  public:

    C_GRIDID() {gid=-1, node=-1; delay_g2m = -1.0;}
    void init(int g, int n) {gid = g; node = n;}

    int get_gid() {return gid;}
    int get_node() {return node;}

    double get_delay_g2m() {return delay_g2m;}

    void exchange(int &g, int &n, double &d) {
	int temp; double temp1;
	temp = gid; gid = g; g = temp;
	temp = node; node = n; n = temp;
	temp1 = delay_g2m; delay_g2m = d; d = temp1;
    }

};


#endif
