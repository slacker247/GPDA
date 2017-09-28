/***************************************************************************
  A new RSD Model program build on top of GR_DispObj.

  For backward compatibility, a large portion of the original code was kept,
  though things like model_name() may not be needed at all -- they should be
  cleaned up later.

  v_process_pick, pick_event and drag_event are new.

  -- created from old RSD,  Y. Tung, 8/25/92
  -- added fields p_x, p_y and p_z plus set and get these vlues;, 10/27/92;
     which were moved to GR_DispObj on 11/5/92;
  -- 11/12/92: cleaned up a lot of old stuff;
     also, added list of sensor pointers, and included emh.H;
  -- 12/29/92: addied another constructor with no argument, made the loading
     file-loading constructor take one argument for filename 
     instead of zero or one argument: GR_Model(char*filename==NULL);
     also removed the destructor which seemed to cause segmentation
     fault when do "delete" a pointer to an array of GR_Model's;
  -- 01/18/93: added new member pickEvent();
  -- 01/25/93: added new member parse_model();

**************************************************************************   */
   
#ifndef		GR_MODEL
#define		GR_MODEL		"GR_Modelclass"

#include  <stdio.h>
#include  "upd_defs.H"
#include  "rsd_patterns.H"
#include  "rsd_geom.H"
#include  "GR_Interface.H"
#include  "GR_DispObj.H"
#include  "GR_Sensor.H"
 
#define   MAXTYPES   499
#define   MAX_POLYS  36500
#define   NUM_VTX    16

class GR_Model: public GR_DispObj
{
 private:
   static long	p_idtable[MAXTYPES];
   static char 	p_filenames[99][80];
   static long	p_filetypes[99];
   long	  p_files_read;
   void load_model (char* filename, long type);
   void load_geom_file (char* filename);
   void load_gouraud_file (char* filename);
   void load_lightmodel_file (char* filename);
   
   GR_Sensor  **p_sobj_list;
   int        p_total_sensors;

 protected:
   typedef struct
   {
      short  color[NUM_VTX][3];
      short  num_vtx;
      float  vtx_array[NUM_VTX][3];
   } POLYGONS;
   static int land_polys;
   static int sea_polys;
   static POLYGONS sea_polygons[MAX_POLYS];
   static POLYGONS land_polygons[MAX_POLYS];
   
 public:
   GR_Model (char* filename);
   GR_Model (long id, long type);
   GR_Model ();
   void  parse_model (char* filename);
   long  get_p_files_read () { return p_files_read; }
   char* get_p_filenames (int i) { return p_filenames[i]; }
   long  get_p_filetypes (int i) { return p_filetypes[i]; } 

   void  init (long id, long type);
   void  set_type (long type);
   void  get_modelname(int type, int maxchars, char* name);
   void  objdraw ();
   void  v_process_pick (GR_MouseEvent&, GR_Window*) {};
   void  pick_event (GR_MouseEvent&, GR_DispObj*, GR_Window*) {};
   void  drag_event (GR_MouseEvent&, GR_Window*) {};

   GR_Sensor* get_sobj (int index);
   void  add_sobj (GR_Sensor* sobj);
   void  delete_sobj (GR_Sensor* sobj);
   int   get_total_sensors () { return p_total_sensors; }

   void pickEvent (GR_MouseEvent&, GR_Window*);
   //void pickEvent (GR_MouseEvent&, GR_DispObj*, GR_Window*);
   
};

float get_scale (long type);  

#endif



