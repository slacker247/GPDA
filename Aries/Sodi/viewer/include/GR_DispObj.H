/*		%W%		%G%		*/
/**************************************************************************

 modifications made: in get and set visible flag functions added validation
   test (if (this)....) on 7/29/93 by Tung;

***************************************************************************/


#ifndef GR_DSOBJ
#define GR_DSOBJ

#define INLINE inline

#include 	"GR_Interface.H"
#include	"GR_Window.H"

extern void printString(char *);

class GR_Window;


class GR_DispObj
{
 private:
   GR_matrix		p_modelmatrix;
   unsigned 		p_register_pick : 1;
   unsigned		p_visible : 1;
   unsigned             p_useXForm : 1;
   
 protected:
   float                p_x;
   float                p_y;
   float                p_z;
   float                p_lat;
   float                p_lon;
   float                p_alt;
   float		p_heading;
   double               p_simtime;
   long                 p_type;
   long                 p_gr_objid;
   long			p_id;
   long                 p_highlight;
   long			p_trackid;

   virtual void v_set_highlight (long tf);
   virtual void objdraw ();
   
 public:
   GR_DispObj ();
   long get_type () { return p_type;}
   void set_type (long type) { p_type = type;}

   double get_time () { return p_simtime; }
   void set_time (double time) { p_simtime = time; }

   long get_id () { return p_id;}
   void set_id (long id) { p_id = id;}

   float get_x () {return p_x;}
   float get_y () {return p_y;}
   float get_z () {return p_z ;}
   void set_xyz (float x, float y, float z);

   float get_lat () {return p_lat;}
   float get_lon () {return p_lon;}
   float get_alt () {return p_alt;}
   float get_heading () {return p_heading;}
   void set_llah (float lat, float lon, float alt, float heading);

   long get_objid () { return p_gr_objid;}
   long  get_trackid () { return p_trackid; }
   void  set_trackid (long trackid) { p_trackid = trackid; }
   
   void translate (float x, float y, float z = 0.0);
   GR_point position (GR_point origin);
   GR_point position ();
   void scale (float x, float y, float z = 1.0);
   void rotate_x (float value);
   void rotate_y (float value);
   void rotate_z (float value);
   void label (float x, float y, float z, char *text);
   void reset ();
   void finish ();
   void draw ();
   void set_register_flag (long tf);
   Boolean get_register_flag ();  // extra?...
   
   void set_visible_flag (long tf);
   long get_visible_flag ();
   void set_noXForm ();
   virtual void pickEvent (GR_MouseEvent&, GR_Window*);
   virtual void pickEvent (GR_MouseEvent&, GR_DispObj*, GR_Window*);
   virtual void dragEvent (GR_MouseEvent&, GR_Window*);
   virtual void rectEvent (short &x1, short &y1, short &x2, short &y2);
   virtual void keyEvent (KeySym, GR_Window*);
   virtual void set_highlight (long tf);
};


INLINE Boolean
GR_DispObj::get_register_flag ()
{
   if (p_register_pick == 1)
     return TRUE;
   else
     return FALSE;
}

INLINE void
GR_DispObj::set_register_flag (long tf)
{
   if (tf)
     p_register_pick = 1;
   else
     p_register_pick = 0;
}

INLINE void
GR_DispObj::set_visible_flag (long tf)
{
  if (this)  // extra protection added 7/29/93 by Tung;
  {
    if (tf)
      p_visible = 1;
    else
      p_visible = 0;
  }
}

INLINE long
GR_DispObj::get_visible_flag ()
{
  if (this)  // extra protection added 7/29/93 by Tung;
    return p_visible;
  else
    return 0;
}

INLINE void 
GR_DispObj::translate (float x, float y, float z)
{
   GR_translate(x, y, z);
}

INLINE void 
GR_DispObj::scale (float x, float y, float z)
{
   GR_scale(x, y, z);
}

INLINE void 
GR_DispObj::rotate_x (float value)
{
   GR_rot(value, 'x');
}

INLINE void 
GR_DispObj::rotate_y (float value)
{
   GR_rot(value, 'y');
}

INLINE void 
GR_DispObj::rotate_z (float value)
{
   GR_rot(value, 'z');
}

INLINE void
GR_DispObj::label(float x, float y, float z, char *text)
{
    glPushAttrib(GL_LIGHTING_BIT);
    glShadeModel(GL_FLAT);
    glRasterPos3f(x, y, z);
    printString(text);
    glPopAttrib();
}

INLINE void
GR_DispObj::reset ()
{
   glMatrixMode(GL_MODELVIEW_MATRIX);
   glLoadIdentity();
}

INLINE void
GR_DispObj::finish()
{
   glGetFloatv(GL_MODELVIEW_MATRIX, (GLfloat *)p_modelmatrix);
}
#endif
