// gbi_item.H header file

#ifndef gbi_item_object
#define gbi_item_object

#include "qitem.H"

/************************************************************************
*	gbi object 							*
************************************************************************/
class C_GBI_ITEM : public C_ITEM {

  private:

    double range;
    double alt;

  protected:

  public:

//  C_GBI();

    void set_theta(int theta) {set_id(theta);}
    void set_time(double t) {set_time_tag(t);}
    void set_alt(double a) {alt = a;}
    void set_range(double r) {range = r;}

    int get_theta() {return get_id();}
    double get_time() {return get_time_tag();}
    double get_alt() {return alt;}
    double get_range() {return range;}

};


#endif
