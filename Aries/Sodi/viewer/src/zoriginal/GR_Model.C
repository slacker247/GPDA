/****************************************************************************
   Implementing GR_Model functions.

  -- Y. Tung, 8/25/92
  -- 11/12/92: clean up;
  -- 12/21/92: make use of env var MODELDIR for both the model
     description file and the model files -- if MODELDIR is defined;
  -- 01/25/93: add member function parse_model();
  -- 02/16/93: add def_pats ();
  -- 03/15/93: modify "poly 4" parsing;
  -- 03/15/93: modify "poly 3" parsing;
  -- 04/27/93: add mask 0x000fffff for the picked object id;
  -- 05/13/93: make qstrip and tmesh NA to type==33 due to earth-drawing
               problem (not all old earth polygons were desgined correctly).
  -- 06/10/93: make default model files directory path
               "../../data/IRIS4D/models".
  -- 06/14/93: added qstrip parsing so that it understands qstrip also;  
  -- 07/22/93: will ignore rather than exit when a model file is not found;
	       
 **************************************************************************/

#include	<math.h>
#include	<string.h>
#include        <sys/types.h>
#include        <malloc.h>
#include        <stdlib.h>
#include        <Xm/MessageB.h>
#include	"GR_Model.H"

#define         POLAR
#define         DO_POLAR
#define         CARDINAL   2

#define		FLAT	   GL_FLAT
#define		GOURAUD    GL_SMOOTH

void def_pats ();

/* -------------------------------------------------------------- */
void
def_pats ()
{
   int i;
   unsigned short patmask[16];

   for (i=0; i<16; i+=2)
   {
      patmask[i] = 0xaaaa;
      patmask[i+1] =0x5555;
   }
   GR_defpattern (1, 16, patmask);

   for (i=0; i<16; i+=2)
   {
      patmask[i] = 0x8888;
      patmask[i+1] =0x2222;
   }
   GR_defpattern (2, 16, patmask);

   for (i=0; i<16; i+=2)
   {
      patmask[i] = 0x0303;
      patmask[i+1] =0x3030;;
   }
   GR_defpattern (3, 16, patmask);

   for (i=0; i<16; i+=4)
   {
      patmask[i] = 0x1111;
      patmask[i+1] = 0x1111;
      patmask[i+2] = 0xffff;
      patmask[i+3] = 0x1111;
   }
   GR_defpattern (12, 16, patmask);
}



/* -------------------------------------------------------------- */
GR_Sensor*
GR_Model::get_sobj (int index)
{
  if (index >= 0 && index < p_total_sensors)
     return p_sobj_list[index];
  else
     return NULL;
}

void
GR_Model::add_sobj (GR_Sensor* sobj)
{
  if (p_total_sensors == 0)
     p_sobj_list = (GR_Sensor**)malloc(MAXSENSORS*sizeof(GR_Sensor*));
  p_total_sensors++;
  p_sobj_list[p_total_sensors-1] = sobj;
}

void
GR_Model::delete_sobj (GR_Sensor* sobj)
{
  Boolean DONE = FALSE;
  int i;
  long id = sobj->GR_DispObj::get_id();
 
  for (i=0; i<p_total_sensors; i++)
  {
     if (p_sobj_list[i]->GR_DispObj::get_id() == id)
     {
        p_sobj_list[i] = p_sobj_list[p_total_sensors-1];
        p_total_sensors--;
        DONE = TRUE;
        break;
     }
  }
  if (!DONE)
     printf ("Warning: deleting an inexist sensor %d from platform %d.\007\n",
             id, this->GR_DispObj::get_id()); 
}


/* -------------------------------------------------------------- */

GR_Model::GR_Model ()
{
   p_total_sensors = 0;
}

void
GR_Model::init (long id, long type)
{
   set_id (id);
   set_type (type);
}

GR_Model::GR_Model (long id, long type)
{
   p_total_sensors = 0;
   set_id (id);
   set_type (type);
}

/* -------------------------------------------------------------- */

GR_Model::GR_Model (char* desc_file)
{
   FILE*        fileptr;
   char		line [81];
   long		i, type;
   static Boolean  first_model = TRUE;
   int          found;
   char         *MODELDIR;
   char         *FWDIR;
   char		model_file [80], model_path[80];
   
   if (first_model)
      first_model = FALSE;
   else
      return;

   if (desc_file == NULL)
      desc_file = "Models.desc";

   def_pats ();                        // load pattern definitions;

   p_files_read = 0;
   sea_polys = 0;
   land_polys = 0;
   
   for (i=0; i<MAXTYPES; i++)
     p_idtable [i] = -1;
   fileptr = fopen (desc_file, "r");

   if (fileptr == NULL)
   {
      fprintf (stderr, "Cannot access file %s\007\n", desc_file);
      return;
   }

   if ((MODELDIR = getenv("MODELDIR")) == NULL)
   {
      if ((FWDIR = getenv("FRAMEWORKHOME")) == NULL)
	MODELDIR = "../../data/models";
      else
      {
	 MODELDIR = (char*)malloc(80);
	 sprintf (MODELDIR, "%s/data/models", FWDIR);
      }	
   }

   while ( fgets (line, 80, fileptr) != NULL )
   {
      //this will parse comments in the model description file
      if ( strncmp(line,"#",1) ==  0 )
      {
	 continue;
      }
      sscanf (line, "%d", &type);
      sscanf (line, "%*s%s", model_file);
      if (MODELDIR != NULL)
         sprintf (model_path, "%s/%s", MODELDIR, model_file);
      else
         sprintf (model_path, "../../data/models/%s", model_file);
 
      printf("Loading model file: %s ", model_path);

      found = 0;
      for (i=0; i<p_files_read; i++)
      {
	 if (strcmp(model_file,(char*)(p_filenames[i])) == 0)
	 {
	    printf("... duplicated file %s -> skip.\n",model_file);
	    found = 1;
	    p_gr_objid		= p_idtable[p_filetypes[i]];
            p_idtable[type]	= p_gr_objid;
	 }
      }

      if ( !found ) 
      {
	 load_model (model_path, type);
	 strcpy( p_filenames[p_files_read],model_file);
	 p_filetypes[p_files_read++] = type;
	 printf("... loaded.\n");
      }

   }
   fclose (fileptr);
}


/* ----------------------------------------------------------- */
void
GR_Model::parse_model (char* filename)
{
   FILE*        fileptr;
   char		line [81];
   long		i, type;
   char		model_path[80], model_file [80];
   int          found;
   char         *MODELDIR;
   char         *FWDIR;
   
   if (filename == NULL)
      filename = "modvModels.desc";
   
   p_files_read = 0;
   sea_polys = 0;
   land_polys = 0;
   
   for (i=0; i<MAXTYPES; i++)
     p_idtable [i] = -1;
   fileptr = fopen (filename, "r");


   if (fileptr == NULL)
   {
      fprintf (stderr, "Cannot access file %s\007\n", filename);
      return;
   }


   if ((MODELDIR = getenv("MODELDIR")) == NULL)
   {
      if ((FWDIR = getenv("FRAMEWORKHOME")) == NULL)
	MODELDIR = "../../data/models";
      else
      {
	 MODELDIR = (char*)malloc(80);
	 sprintf (MODELDIR, "%s/data/models", FWDIR);
      }
   }

   while ( fgets (line, 80, fileptr) != NULL )
   {
      //this will parse comments in the model description file
      if ( strncmp(line,"#",1) ==  0 )
      {
	 continue;
      }
      sscanf (line, "%d", &type);
      sscanf (line, "%*s%s", model_file);
      if (MODELDIR != NULL)
         sprintf (model_path, "%s/%s", MODELDIR, model_file);
      else
         sprintf (model_path, "../../data/models/%s", model_file);
 
      printf("About to load model file: %s ", model_path);

      found = 0;
      for (i=0; i<p_files_read; i++)
      {
	 if (strcmp(model_file,(char*)(p_filenames[i])) == 0)
	 {
	    printf("... duplicated file %s -> skip.\n",model_file);
	    found = 1;
	    p_gr_objid		= p_idtable[p_filetypes[i]];
            p_idtable[type]	= p_gr_objid;
	 }
      }

      if ( !found ) 
      {
	 load_model (model_path, type);
	 strcpy( p_filenames[p_files_read],model_file);
	 p_filetypes[p_files_read++] = type;
	 printf("... loaded.\n");
      }

   }
   fclose (fileptr);
}


/* ----------------------------------------------------------- */

void 
GR_Model::load_model (char* file_name, long type) 
{
   long      angle, num_sides;
   long      r, g, b;
   float     x, y, z;
   char      geom_file[64];
   FILE      *fp;
   char	     line [81];
   char	     cmnd [81];
   float     varray[3];
   short     carray[3];
   int       i, j;
   int       num_areas, num_verts;

   float vert[3], vertlast[3];

   if (type >= MAXTYPES)
     return;

   fp = fopen(file_name,"r");
   if ( fp == NULL )
   {
      printf("C_Model: Error on file open for file %s\007\n",file_name);
      return;
   }

   p_gr_objid = GR_genobj ();
   p_idtable[type] = p_gr_objid;
   GR_makeobj (p_gr_objid);
   GR_pushmatrix();
   GR_pushattributes ();

   while ( fgets(line,80,fp) != NULL )
   {
      sscanf(line,"%s",cmnd);
 
      if ( strcmp(cmnd,"scale") == 0 )
      {
         // modify old code to prevent wrong scale due to failed sscanf:
         if (sscanf(line,"%*s%f%f%f",&x,&y,&z) == 3)
         {
            scale(x,y,z);
            // printf ("\n   doing scale(%f,%f,%f)  ",x, y, z);
         }
      }
   
      if ( strcmp(cmnd,"GR_bgnpoint") == 0 )
      {
	 GR_bgnpoint();
      }
      if ( strcmp(cmnd,"GR_endpoint") == 0 )
      {
	 GR_endpoint();
      }
      if ( strcmp(cmnd,"geomfile") == 0 )
      {
	 sscanf(line, "%s%s",cmnd, geom_file);
	 load_geom_file (geom_file);
	 strcpy(cmnd,"");
      }
      else if ( strcmp(cmnd,"earthmodel") == 0 )
      {
	 printf("EARTHMODEL\n");
	 strcpy(cmnd,"");
      }
      else if ( strcmp(cmnd,"lightmodelfile") == 0 )
      {
	 sscanf(line, "%s%s",cmnd, geom_file);
	 load_lightmodel_file (geom_file);
	 strcpy(cmnd,"");
      }
      else if ( strcmp(cmnd,"editfile") == 0 )
      {
	 sscanf(line, "%s%s",cmnd, geom_file);
	 load_lightmodel_file (geom_file);
	 strcpy(cmnd,"");
      }
      else if ( strcmp(cmnd,"flatfile") == 0 )
      {
	 sscanf(line, "%s%s",cmnd, geom_file);
	 GR_shademodel(FLAT);
	 load_gouraud_file (geom_file);
	 strcpy(cmnd,"");
      }
      else if ( strcmp(cmnd,"gouraudfile") == 0 )
      {
	 sscanf(line, "%s%s",cmnd, geom_file);
	 GR_shademodel(GOURAUD);
	 load_gouraud_file (geom_file);
	 strcpy(cmnd,"");
      }
      else if ( strcmp(cmnd,"boundryfile") == 0 )
      {
      }
      else if ( strcmp(cmnd,"line") == 0 )
      {
	 fgets(line,80,fp);
	 sscanf(line,"%*s%f%f%f",&x,&y,&z);
	 GR_move(x,y,z);
	 fgets(line,80,fp);
	 sscanf(line,"%*s%f%f%f",&x,&y,&z);
	 GR_draw(x,y,z);
      }
      else if ( strcmp (cmnd, "color") == 0 )
      {
	 sscanf (line, "%*s%d%d%d", &r, &g, &b);
	 fgets (line, 80, fp);
	 sscanf (line, "%s%d", cmnd, &i);
	 if ( strcmp (cmnd, "pattern") == 0 )
	 {
	    GR_setpattern ((short) i);
	    fgets (line, 80, fp); // get next line for vtx's
	    sscanf (line, "%s%d", cmnd,&num_sides);
	 }
	 if ( strcmp(cmnd,"crv") == 0 )
	 {
	    float p[4][3];
	    short c[3];
	    int num_pts = 0;
	    GR_matrix basmat; 
	    
	    basmat[0][0] = -.5 ;
	    basmat[0][1] = 1.5 ;
	    basmat[0][2] = -1.5 ; 
	    basmat[0][3] = .5 ; 
	    basmat[1][0] = 1 ;
	    basmat[1][1] = -2.5 ;
	    basmat[1][2] = 2.0 ; 
	    basmat[1][3] = -.5 ; 
	    basmat[2][0] = -.5 ;
	    basmat[2][1] = 0 ;
	    basmat[2][2] = .5 ; 
	    basmat[2][3] = .0 ; 
	    basmat[3][0] = 0 ;
	    basmat[3][1] = 1 ;
	    basmat[3][2] = 0 ; 
	    basmat[3][3] = 0 ; 
	    	    
	    GR_defbasis(CARDINAL,basmat);
	    GR_curvebasis(CARDINAL);
	    GR_curveprecision(20);
	    for ( i = 0; i < 4 ; i++ )
	    {
	       fgets (line, 80, fp); // get next line for vtx's
	       sscanf (line, "%*s%f%f%f", &x, &y, &z);
	       p[i][0] = y;
	       p[i][1] = z;
	       p[i][2] = x;
	       num_pts++;
	    }
	    c[0] = (short) r;
	    c[1] = (short) g;
	    c[2] = (short) b;
	    GR_RGBcolor((short)r, (short)g, (short)b);
	    GR_crv(p);
	 }
	 if ( strcmp(cmnd,"pnt") == 0 )
	 {
	    float p[3];
	    short c[3];
	    sscanf (line, "%*s%f%f%f", &p[0], &p[1], &p[2]);
	    c[0] = (short)r;
	    c[1] = (short)g;
	    c[2] = (short)b;
	    GR_c3s(c);
	    GR_v3f(p);
	 }
	 if ( strcmp(cmnd,"line") == 0 )
	 {
	    GR_RGBcolor ((short)r, (short)g, (short)b);     
	    fgets(line,80,fp);
	    sscanf(line,"%*s%f%f%f",&x,&y,&z);
	    GR_move(x,y,z);
	    fgets(line,80,fp);
	    sscanf(line,"%*s%f%f%f",&x,&y,&z);
	    GR_draw(x,y,z);
	 }
	 else if ( strcmp(cmnd,"poly") == 0 )
	 {
	    sscanf (line, "%*s%d", &num_sides);
	    GR_RGBcolor ((short)r, (short)g, (short)b);   
	    if (type!=33 && num_sides==4)
	    {
               GR_bgnqstrip ();
               for (i=0; i<2; i++)
               {
                  fgets (line, 80, fp);
                  sscanf (line, "%*s%f%f%f", &x, &y, &z);
                  vert[0] = y;
                  vert[1] = z;
                  vert[2] = x;
                  GR_v3f(vert);
	       }
	       fgets (line, 80, fp);
	       sscanf (line, "%*s%f%f%f", &x, &y, &z);
	       vertlast[0] = y;
	       vertlast[1] = z;
	       vertlast[2] = x;
	       
	       fgets (line, 80, fp);
	       sscanf (line, "%*s%f%f%f", &x, &y, &z);
	       vert[0] = y;
	       vert[1] = z;
	       vert[2] = x;
	       GR_v3f(vert);
	       GR_v3f(vertlast);
	       GR_endqstrip ();
	    }
	    else if (type!=33 && num_sides==3)
	    {
	       GR_bgntmesh ();
	       for (i=0; i<3; i++)
	       {
		  fgets (line, 80, fp);
		  sscanf (line, "%*s%f%f%f", &x, &y, &z);
		  vert[0] = y;
		  vert[1] = z;
		  vert[2] = x;
		  GR_v3f(vert);
	       }
	       GR_endtmesh ();
	    }
	    else
	    {
	       fgets (line, 80, fp);
	       sscanf (line, "%*s%f%f%f", &x, &y, &z);
	       GR_pmv (y, z, x);	
	       for (i = 1; i < num_sides; i++ )
	       { 
	          fgets(line,80,fp);
	          sscanf(line,"%*s%f%f%f",&x,&y,&z);
	          GR_pdr( y,z,x );	
	       }
	       GR_pclos();
	    }
	 }
         
         /* the following is the old tmesh routine, never tested/used:
         else if ( strcmp(cmnd,"tmesh") == 0 ) 
	 { 
	    sscanf (line, "%*s%d", &num_sides);
	    GR_bgntmesh();
	    for (i = 1; i < num_sides; i++ )
	    { 
	       fgets (line, 80, fp);
	       sscanf (line, "%*s%hd%hd%hd", &r, &g, &b);
	       fgets(line,80,fp);
	       sscanf(line,"%*s%f%f%f",&x,&y,&z);
	       carray[0] = (short) r;
	       carray[1] = (short) g;
	       carray[2] = (short) b;
	       varray[0] = y;
	       varray[1] = z;
	       varray[2] = x;
	       if ( i == 2 )
		 GR_swaptmesh();
	       GR_c3s(carray);
	       GR_v3f(varray);
	    }
	    GR_endtmesh();
	 }
         */

         else if ( strcmp(cmnd,"tmesh") == 0 )  // new part added 7/13/93:
         {
            sscanf (line, "%*s%d", &num_areas);
            GR_RGBcolor((short)r, (short)g, (short)b);
            for (j = 0; j < num_areas; j++)
            {
               fgets(line,80,fp); // read off bgntmesh line
               GR_bgntmesh();
               for (i = 0; i < 3; i++)
               {
                  fgets(line,80,fp);
                  sscanf(line,"%*s%f%f%f",&x,&y,&z);
                  varray[0] = y;
                  varray[1] = z;
                  varray[2] = x;
                  GR_v3f(varray);
               }
               fgets(line,80,fp); // read off endtmesh line 
               GR_endtmesh();
            }
         }
         
	 else if (strcmp(cmnd,"qstrip")==0) // new part added 06/14/93:
	 {
	    sscanf (line, "%*s%d%d", &num_areas, &num_verts);
            GR_RGBcolor((short)r, (short)g, (short)b);
            for (j = 0; j < num_areas; j++)
	    {
	       fgets(line,80,fp); // read off GR_bgnqstrip line
	       GR_bgnqstrip ();
	       for (i = 0; i < num_verts; i++)
	       {
		  fgets(line,80,fp);
		  sscanf(line,"%*s%f%f%f",&x,&y,&z);
		  varray[0] = y;
		  varray[1] = z;
		  varray[2] = x;
		  GR_v3f(varray);
	       }
	       fgets(line,80,fp); // read off GR_endqstrip line
	       GR_endqstrip ();
	    }
	 }
      } 
      else if ( strcmp(cmnd,"pattern") == 0 )
      {
	 sscanf(line,"%*s%d",&i);
	 GR_setpattern((short) i);
      }
     /* else if ( strcmp(cmnd,"scale") == 0 )
      {
	 x=1.0, y=1.0, z=1.0; // to prevent wrong scale due to failed sscanf:
	 sscanf(line,"%*s%f%f%f",&x,&y,&z);
	 scale(x,y,z);
      }
     */
      else if ( strcmp(cmnd,"rotatex") == 0 )
      {
	 sscanf(line,"%*s%ld",&angle);
	 GR_rotate((short) angle * 10,'x');
      }
      else if ( strcmp(cmnd,"rotatey") == 0 )
      {
	 sscanf(line,"%*s%ld",&angle);
	 GR_rotate((short) angle * 10,'y');
      }
      else if ( strcmp(cmnd,"rotatez") == 0 )
      {
	 sscanf(line,"%*s%ld",&angle);
	 GR_rotate((short) angle * 10,'z');
      }
      else if ( strcmp(cmnd,"translate") == 0 )
      {
	 sscanf(line,"%*s%f%f%f",&x,&y,&z);
	 GR_translate(x,y,z);
      }
      else if ( strcmp(cmnd,"move") == 0 )
      {
	 sscanf(line,"%*s%f%f%f",&x,&y,&z);
	 GR_move(x,y,z);
      }
      else if ( strcmp(cmnd,"pushmatrix") == 0 )
      {
	 GR_pushmatrix();
      }
      else if ( strcmp(cmnd,"popmatrix") == 0 )
      {
	 GR_popmatrix();
      }
   }  // end of while

   GR_popattributes (); 
   GR_popmatrix();
   GR_closeobj();
   fclose(fp);
}


/* ------------------------------------------------------------------- */
void
GR_Model::load_gouraud_file(char* filename)
{
   char line[80];
   char cmnd[80];
   short r,g,b;
   float x,y,z;
   int   num_vertices;
   int land=0,sea=0,first_time=1;
   int i;
   
   short carray[3];
   float varray[3];
   FILE* fp;

   fp = fopen(filename,"r");
   if ( fp == NULL )
   {
      printf ("gouraud file %s does not exist\007\n", filename);
      return;
   }

   while ( fgets(line,80,fp) != NULL )
   {
      sea = 0;
      land = 0;
      first_time = 1;
      sscanf(line,"%s",cmnd);
      
      if ( strcmp(cmnd,"poly") == 0 || strcmp(cmnd,"tmesh") == 0)
      {
	 GR_bgnpolygon();
	 sscanf(line,"%*s%d",&num_vertices);
	 for (i = 0; i < num_vertices; i++ )
	 {
	    fgets(line,80,fp);
	    sscanf(line,"%*s%hd%hd%hd",&r,&g,&b);
	    fgets(line,80,fp);
	    sscanf(line,"%*s%f%f%f",&x,&y,&z);
	    carray[0] = r;
	    carray[1] = g;
	    carray[2] = b;
	    varray[0] = y;
	    varray[1] = z;
	    varray[2] = x;
	    GR_c3s(carray);
	    GR_v3f(varray);
	 }
	 GR_endpolygon();
      }
      else if ( strcmp(cmnd,"tmesh") == 0 )
      {
	 GR_bgntmesh();
	 sscanf(line,"%*s%d",&num_vertices);
	 for (i = 0; i < num_vertices; i++ )
	 {
	    fgets(line,80,fp);
	    sscanf(line,"%*s%hd%hd%hd",&r,&g,&b);
	    fgets(line,80,fp);
	    sscanf(line,"%*s%f%f%f",&x,&y,&z);
	    carray[0] = r;
	    carray[1] = g;
	    carray[2] = b;
	    varray[0] = y;
	    varray[1] = z;
	    varray[2] = x;
	    if ( i == 2 )
	      GR_swaptmesh();
	    GR_c3s(carray);
	    GR_v3f(varray);
	 }
	 GR_endtmesh();
      }
   }
}

/* ------------------------------------------------------------------- */
void
GR_Model::load_lightmodel_file(char* filename)
{
   int i;
   char line[80];
   char cmnd[80];
   short r,g,b;
   float x,y,z;
   int   num_vertices;
   int   land=0,sea=0,first_time=1;

   FILE* fp;
   printf("LOADING LIGHT MODEL  %s\n",filename);
   fp = fopen(filename,"r");
   if ( fp == NULL )
   {
      printf ("lightmodel file %s does not exist\007\n", filename);
      return;
   }
   
   while ( fgets(line,80,fp) != NULL )
   {
      sea = 0;
      land = 0;
      first_time = 1;
      sscanf(line,"%s",cmnd);
      if ( strcmp(cmnd,"poly") == 0 )
      {  
	 sscanf(line,"%*s%d",&num_vertices);
	 for (i = 0; i < num_vertices; i++ )
	 {
	    fgets(line,80,fp);
	    sscanf(line,"%*s%hd%hd%hd",&r,&g,&b);
	    fgets(line,80,fp);
	    sscanf(line,"%*s%f%f%f",&x,&y,&z);
	    if ( first_time == 1)
	    {
	       if ( b >= r && b >= g )
		 sea = 1;
	       else
		 land = 1;
	       first_time = 0;
	    }
	    if ( sea == 1 )  //sea poly
	    {
	       sea_polygons[sea_polys].num_vtx = num_vertices;	
	       sea_polygons[sea_polys].vtx_array[i][0] = y;
	       sea_polygons[sea_polys].vtx_array[i][1] = z;
	       sea_polygons[sea_polys].vtx_array[i][2] = x;
	       sea_polygons[sea_polys].color[i][0] = r;	
	       sea_polygons[sea_polys].color[i][1] = g;	
	       sea_polygons[sea_polys].color[i][2] = b;	
	    }
	    else
	    {
	       land_polygons[land_polys].num_vtx = num_vertices;	
	       land_polygons[land_polys].vtx_array[i][0] = y;
	       land_polygons[land_polys].vtx_array[i][1] = z;
	       land_polygons[land_polys].vtx_array[i][2] = x;
	       land_polygons[land_polys].color[i][0] = r;	
	       land_polygons[land_polys].color[i][1] = g;	
	       land_polygons[land_polys].color[i][2] = b;	
	    }
	 }
	 if ( sea == 1)
	   sea_polys++;
	 else
	   land_polys++;
      }
   }
   printf("SEA %d   LAND %d\n",sea_polys,land_polys);
}



/* ----------------------------------------------------------------------------
* Note: this "load_geom_file" was authored by Steve Gray in June 1987.
*       also, in original notes, it is said:
*				see file "bldgeom.c" for file format
*				see file "rsd_geom.h" for command codes
* --------------------------------------------------------------------------- */

void 
GR_Model::load_geom_file(char* filename)
{
   FILE		*fp;
   int		num_cmds, cmd;
   int		num_verts, len;
   float	vert[3];
   float	first[3];
   int		cval[3];
   int		mapval[4];
   float	bndy[4];
   char	str_buf[256];

   fp = fopen(filename,"r");
   if ( fp == NULL )
   {
      printf ("file %s does not exist\007\n", filename);
      return;
   }

   num_cmds = 0;

   /* read next command code */
   while (	fread( (char*)&cmd, sizeof(int), 1, fp) != 0 )
   {
      /* switch on command code */
      switch ( cmd ) {
       case RSD_POLY:
	 /* read vertex count for next poly */
	 fread( (char*)&num_verts, sizeof(int), 1, fp);
	 /* read and move to first vertex */
	 fread( (char*)vert, sizeof(float), 3, fp);
	 GR_pmv( vert[1], vert[2], vert[0]);
	 /* read remaining verts */
	 for ( num_verts--; num_verts > 0; num_verts-- )
	 {
	    fread( (char*)vert, sizeof(float), 3, fp);
	    GR_pdr( vert[1], vert[2], vert[0]);
	 }
	 /* close the poly */
	 GR_pclos();
	 break;
	 
       case RSD_COLOR:
	 fread( (char*)cval, sizeof(int), 3, fp);
	 GR_RGBcolor( (short) cval[0], (short) cval[1], (short) cval[2]);
	 break;

       case RSD_RECT:
	 fread( (char*)bndy, sizeof(float), 4, fp);
	 GR_rect( bndy[0], bndy[1], bndy[2], bndy[3]);
	 break;

       case RSD_RECTF:
	 fread( (char*)bndy, sizeof(float), 4, fp);
	 GR_rectf( bndy[0], bndy[1], bndy[2], bndy[3]);
	 break;
	 
       case RSD_MOVE:
	 fread( (char*)vert, sizeof(float), 3, fp);
	 GR_move( vert[0], vert[1], vert[2]);
	 break;

       case RSD_DRAW:
	 fread( (char*)vert, sizeof(float), 3, fp);
	 GR_draw( vert[0], vert[1], vert[2]);
	 break;
	 
       case RSD_CMOV:
	 fread( (char*)vert, sizeof(float), 3, fp);
	 GR_cmov( vert[0], vert[1], vert[2]);
	 break;
	 
       case RSD_CHARSTR:
	 fread( (char*)&len, sizeof(int), 1, fp);
	 fread( (char*)str_buf, sizeof(char), len, fp);
	 str_buf[len] = '\0';
	 GR_charstr( str_buf);
	 break;
	 
       case RSD_CLEAR:
	 GR_clear();
	 break;
	 
       case RSD_PATTERN:
	 fread( (char*)&len, sizeof(int), 1, fp);
	 GR_setpattern((short) len);
	 break;
	 
       case RSD_MAPCOLOR:
	 fread((char*) mapval, sizeof(int), 4, fp);
	 break;
	 
       case RSD_PNT:
	 fread( (char*)vert, sizeof(float), 3, fp);
	 GR_pnt( vert[0], vert[1], vert[2]);
	 break;

       default:
	 fprintf( stderr, "Unrecognized command in binary geom file\n");
	 fprintf( stderr, "(command %d, value %d\n", num_cmds, cmd);
      }
      num_cmds++;
   }
   fclose (fp);
}

/* --------------------------------------------------------------------- */
void
GR_Model::set_type (long type)
{
   if (type >= MAXTYPES || type < 0)
   {
      printf("ERROR in GR::set_type: type %d out of range [0..%d]\n",
	     type, MAXTYPES);
      return;
   }
   
   if (p_idtable [type] != -1)
   {
      p_type = type;
      p_gr_objid = p_idtable [type];
   }
   else
   {
      printf("type %d not found in Model table\n",type);
   }
}


/* ---------------------------------------------------------------------- */

void
GR_Model::objdraw ()
{
   // printf ("doing GR_Model::objdraw()\n");
   GR_callobj (p_gr_objid);
}

/* ----------------- set static variables -------------------------- */

long GR_Model::p_idtable [MAXTYPES];
char GR_Model::p_filenames[99][80];
long GR_Model::p_filetypes[99];
int GR_Model::land_polys;
int GR_Model::sea_polys;
GR_Model::POLYGONS GR_Model::sea_polygons[MAX_POLYS];
GR_Model::POLYGONS GR_Model::land_polygons[MAX_POLYS];

/* ----- add new event routines, 1/18/93 ------  */
void
GR_Model::pickEvent(GR_MouseEvent& event, GR_Window* window)
{
   Widget dialog;
   Arg arg[10];
   char str [80];
   XmString xstr;

   printf ("Model object #%d, type %d, was picked..",
           (get_id() & 0x000fffff),
	   get_type()
	   );
   switch (event.button)
   {
    case GR_LEFTMOUSE:
      if (event.down)
      {
         printf ("..by GR_LEFTMOUSE..");
         sprintf (str, "Object #%d, type %d",
		  (p_id & 0x000fffff),
		  get_type()
		  );
         xstr = XmStringCreateSimple (str);
         XtSetArg (arg[0], XmNmessageString, xstr);
         dialog = XmCreateMessageDialog (window->widget(), "message", arg, 1);
       XmStringFree (xstr);
         XtManageChild (dialog);
      }
      break;
    case GR_MIDDLEMOUSE:
      if (!event.down)
      {
         printf (".. by GR_MIDDLEMOUSE..");
      }
      break;
    case GR_RIGHTMOUSE:
      if (event.down)
      {
         printf (".. by GR_RIGHTMOUSE..");
      }
      break;
   }
   printf("\n");
}

/* ------ get_scale(): give a type, returns a scale_factor ----- */

float
get_scale (long type)
{
   float sf;

   switch (type)
   {
    case 1:
      sf = 0.01; // site
      break;
    case 2:
      sf = 0.01; // stage1
      break;
    case 3:
      sf = 0.04; // bsts
      break;
    case 6:
      sf = 0.01; // rv
      break;
    case 9:
      sf = 0.04; // bsts_sim
      break;
    case 17:
      sf = 0.01; // stage2
      break;
    case 18:
      sf = 0.01; // bus
      break;
    case 19:
      sf = 0.01; // wrhd, scale changed from 0.01 to 0.05, on 3/5/93.
      // and then back to 0.01 on 5/14/93.
      break;
    case 20:
      sf = 0.15; // full_sensor
      break;
    case 24:
      sf = 0.20; // ssts_sim
      break;
    case 29:
      sf = 0.0175; // tank
      break;
    case 30:
      sf = 0.01; // shell
      break;
    case 31:
      sf = 0.13; // impact
      break;
    case 32:
      sf = 0.3; //cv_impact
      break;
    case 34:
      sf = 0.01; // decoy
      break;
    case 37:
      sf = 0.011; // new_impact
      break;
    case 38:
      sf = 0.01; //new_impact2
      break;
    case 40:     // blue, orange, purple and brown balls
    case 41:
    case 42:
    case 43:
      sf = 0.05;
      break;
    case 44:     // BE -- smaller purple-pink ball
    case 45:     // DSP -- bigger dark orange ball
      sf = 1.0;
      break; 
    case 49:
      sf = 0.045; // scud_launcher
      break;
    case 50:
      sf = 1;  // ?; newcirc2
      break;
    case 51:
      sf = 0.0003; // f15
      break;
    case 55:
      sf = 0.01; // back2
      break;
    case 56:
      sf = 0.031; // bear2
      break;
    case 57:
      sf = 0.0012; // blue_fighter_highlight.model
      break;
    case 58:
      //sf = 0.091; // alcm_red_slow.model
      sf = 0.012; // alcm_red_slow.model -- new scale defined 6/14/93
      break;
    case 61:
      sf = 0.041; // blue_tanker_detected
      break;
    case 63:
      sf = 0.01; // backfire_highlight
      break;
    case 64:
      sf = 0.041; // bear_highlight
      break;
    case 68:
      sf = 0.031; // awac
      break;
    case 69:
      sf = 0.00013; // 747.model2
      break;      
    case 70:
      sf = 0.0003; // mig
      break;
    case 72:
      sf = 0.03; // navalbase
      break;
    case 73:
      sf = 0.03; // rednaval
      break;
    case 74:
      sf = 0.02; // blueairfield
      break;
    case 75:
      sf = 0.02; // redairfield2
      break;
    case 76:
      sf = 0.01; // ROCC_facility
      break;
    case 77:
      sf = 1; // ? redHQ
      break;
    case 78:
      sf = 0.02; // blue_sub_site
      break;
    case 79:
      sf = 0.02; // red_sub_site
      break;
    case 81:
      sf = 0.071; // red_alcm_highlight
      break;
    case 90:
      sf = 0.01; // nudet0
      break;
    case 91:
      sf = 0.02; // nudet1
      break;
    case 92:
      sf = 0.03; // nudet2
      break;
    case 93:
      sf = 0.034; // nudet3
      break;
    case 95:
      sf = 0.03; // carrier2
      break;
    case 96:
      sf = 0.03; // carrier
      break;
    case 97:
      sf = 0.03; // prop_plane
      break;
    case 98:
      sf = 0.03; // jet_plane
      break;
    case 101:
      sf = 0.0003; // f15_detected
      break;
    case 102:
      sf = 0.041; // awac_detected
      break;
    case 104:
      sf = 0.00013; // 747_highlight
      break;
    case 106:
      sf = 0.003; // blue_air_mission
      break;
    case 107:
      sf = 0.003; // blue_air_mission_detected
      break;
    case 108:
      sf = 0.003; // red_air_mission
      break;
    case 109:
      sf = 0.003; // red_air_mission_detected
      break;
    case 128:
      sf = 0.0002; // 747_detected
      break;
    case 150:
      sf = 0.01; // gbi
      break;
    case 151:
      sf = 0.01; // gbi2
      break;
    case 152:
      sf = 0.01; // gbi_site
      break;
    case 160:
      sf = 0.041; // jstars,.... tmp...
      break;
    case 199:
      sf = 0.08; // crayon
      break;
    case 222:
      sf = 1;  //?; adiz
      break;
    default:
      sf = 0.01;
      break;
   }

   return sf;
}
