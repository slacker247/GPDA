/* **************************************************************************
  A new RSD Model program build on top of GR_DispObj.

  For backward compatibility, a large portion of the original code was kept,
  though things like model_name() may not be needed at all -- they should be
  cleaned up later.

  v_process_pick, pick_event and drag_event are new.

  Y. Tung, 8/25/92

**************************************************************************   */
   
#ifndef		GR_MODEL
#define		GR_MODEL		"GR_Modelclass"

#include  <stdio.h>
#include  <gl.h>
#include  "upd_defs.H"
#include  "rsd_patterns.H"
#include  "rsd_geom.H"
#include  "GR_Interface.H"
#include  "GR_DispObj.H"

#define   MAXTYPES   499
#define   MAX_POLYS  36500
#define   NUM_VTX    16
#define   EDIT_MODE  2


class GR_Model: public GR_DispObj
{
 private:
   static long	p_types[MAXTYPES];
   static char 	p_filenames[99][80];
   static long	p_filetypes[99];
   static long  link_flag;
   static long  polar_flag;
   static long  label_flag;
   static long  ring_flag;
   static long  radar_flag;
   static long  restart_flag;
   static long  missile_trajectory_flag;
   static long  aircraft_trajectory_flag;
   long	  p_files_read;
   long	  p_objtype;
   long	  p_objid;
   long   p_curtype;
   long   p_model_type;
   short  p_first_type;

   void load_model (char* filename, long type);
   void load_geom_file (char* filename);
   void load_gouraud_file (char* filename);
   void load_lightmodel_file (char* filename);

 protected:
   typedef struct
   {
      short  color[NUM_VTX][3];
      short  num_vtx;
      float  vtx_array[NUM_VTX][3];
   } POLYGONS;

   typedef struct
   {
      float  data_point[3];
      short  track_color[3];
   } TRACK_ELEMENT;

   static int gouraud_flag;
   static int lightmodel_flag;
   static int earthmodel_flag;
   static int land_polys;
   static int sea_polys;
   static POLYGONS sea_polygons[MAX_POLYS];
   static POLYGONS land_polygons[MAX_POLYS];

   int num_polys;
   int boundry_flag;
   int boundry_type;
   int num_global_points;
   int num_polar_points;
   long label_id;
   char name_buf[64];
   char menu_title[64];
   
 public:
   GR_Model (char* filename=NULL);
   ~GR_Model () {};
   void  set_type (long type);
   void  model_name();
   void  objdraw ();
   void  v_process_pick (GR_MouseEvent&, GR_Window*);
   void  pick_event (GR_MouseEvent&, GR_DispObj*, GR_Window*);
   void  drag_event (GR_MouseEvent&, GR_Window*);

   
   int   gouraud() {return gouraud_flag;}
   int   radar_toggle() {return radar_flag;}   
   void  radar_toggle(int flag) {radar_flag = flag;}
   int   ring_toggle() {return ring_flag;}   
   void  ring_toggle(int x) {ring_flag = x;}
   int   link_toggle() {return link_flag;}
   void  link_toggle(int x) {link_flag = x;}
   int   labels() {return label_flag;}
   void  labels(int x) {label_flag = x;}
   int   model_type() {return p_objtype;}
   int   model_id(int type) {return p_types[type];}
   void  model_type(int type) {p_objtype = type;}
   int   first_model_type() {return p_first_type;}
   int   polar_map() {return polar_flag;}
   void  polar_map(int x) {polar_flag = x;}
   int   restart() {return restart_flag;}
   void  restart(int x) {restart_flag = x;}
   int   missile_trajectories() {return missile_trajectory_flag;}
   void  missile_trajectories(int x) {missile_trajectory_flag = x;}
   int   aircraft_trajectories() {return aircraft_trajectory_flag;}
   void  aircraft_trajectories(int x) {aircraft_trajectory_flag = x;}
   void  reset_trajectories() {num_global_points = 0;}
};

void GR_Model::v_process_pick (GR_MouseEvent& event, GR_Window* window)
{
   Widget dialog;
   Arg arg[1];
   XmString t;
   char string [32];
   
   sprintf (string, "An object with type=%d and ID=%d has been picked by ",
	    p_objtype, p_objid);
   
   switch (event.button)
   {
    case GR_LEFTMOUSE:
      if (event.down)
      {
	 printf ("GR_LEFTMOUSE...\n");
	 printf ("%s\n", string);
	 window->set_drag_event (this);
      }
      break;
    case GR_MIDDLEMOUSE:
      if (!event.down)
      {
	 printf ("GR_MIDDLEMOUSE...Pick another object\n");
	 window->set_pick_event (this);
      }
      break;
    case GR_RIGHTMOUSE:
      if (event.down)
      {
	 printf ("GR_RIGHTMOUSE...Menu\n");
	 /*
	 t = XmStringCreateSimple (string);
	 XtSetArg (arg[0], XmNmessageString, t);
	 dialog = XmCreateMessageDialog (window->widget(), "message", arg, 1);
	 XmStringFree (t);
	 XtManageChild (dialog);
         */
       }
      break;
   };
}

void
pick_event (GR_MouseEvent& event, GR_DispObj* object, GR_Window* window)
{
   printf("...doing window->draw();\n");
   window->draw();
}


void
drag_event (GR_MouseEvent&, GR_Window*)
{
   printf("...doing nothing for drag_event\n");
}



#endif



