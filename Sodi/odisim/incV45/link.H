// link.H header file

#ifndef link_object
#define link_object

#include "ext_graphics_link_output.H"
#include "qitem.H"

/************************************************************************
*	link object 							*
************************************************************************/
class C_LINK : public C_ITEM {

  private:

    double intensity;		// this is a number between 0 and 1
    int rgb[3];			// red blue and green

  protected:

  public:

    C_LINK() {intensity = 1.0; rgb[0]=255; rgb[1]=255; rgb[2]=255;}

    int get_sensor_id() {return id;}
    int get_track_id() {return freeid;}

    void set_sensor_id(int sid) {id = sid;}
    void set_track_id(int tid) {freeid = tid;}

    void set_intensity(double i) {intensity = i;}
    double get_intensity() {return intensity;}

    int get_red() {return (int)(rgb[0]*intensity);}
    int get_green() {return (int)(rgb[1]*intensity);}
    int get_blue() {return (int)(rgb[2]*intensity);}

    void set_color(int color);

    void set_black() {rgb[0]=0; rgb[1]=0; rgb[2]=0;}
    void set_red() {rgb[0]=255; rgb[1]=0; rgb[2]=0;}
    void set_green() {rgb[0]=0; rgb[1]=255; rgb[2]=0;}
    void set_blue() {rgb[0]=0; rgb[1]=0; rgb[2]=255;}
    void set_yellow() {rgb[0]=255; rgb[1]=255; rgb[2]=0;}
    void set_cyan() {rgb[0]=0; rgb[1]=255; rgb[2]=255;}
    void set_purple() {rgb[0]=255; rgb[1]=0; rgb[2]=255;}
    void set_magenta() {rgb[0]=255; rgb[1]=0; rgb[2]=200;}
    void set_white() {rgb[0]=255; rgb[1]=255; rgb[2]=255;}

};


#endif
