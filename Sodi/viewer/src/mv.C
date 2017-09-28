/************************************************************
  mv.C is a model viewer attached to the rsd.C.
  
  -- 01/25/93: created by Tung; 
  -- 06/14/93: changed "Done" to "Quit";
  
************************************************************/

#include <math.h>
#include <sys/types.h>
#include <malloc.h>
#include <stdlib.h>
#include <string.h>

#include "GR_Shell.H"
#include "GR_Model.H"

#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/MessageB.h>
#include <Xm/List.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>

//GR_Shell *mv_shell;
Widget     mv_shell;
GR_DispList *mv_displist;
GR_Window *mvwindow;
 
void model_viewerCB (Widget);
void mv_doneCB ();
void zoominCB ();
void zoomoutCB ();
void scale_textCB ();
void mv_listCB (Widget, XtPointer, XmListCallbackStruct* call_data);
void mv_sliderCB (Widget, char client_data, 
                  XmScaleCallbackStruct* call_data);


Boolean first_modelviewer = TRUE;
   
GR_Model *allmodels;
int model_type = 1;   // some default type;
extern float get_scale (long type);
extern Widget createOneScale (Widget parent, char code, XmString, 
                       XtCallbackProc CB);

void parse_model (char* filename, int type_wanted);
void load_model (char* file_name, long type);
void mv_draw ();
int mv_LAT=20;
int mv_LON=50;
int mv_FOV=30;
float ViewDist=2.0;
float scale_factor = 1.0;
char scale_text[10];

Widget mv_lat_scale, mv_lon_scale, mv_fov_scale, scale_box;

void
mv_init ()
{
   Widget mv_form;
   Widget mv_list; 
   Widget mv_frame;
   Widget mv_control, done_button, zoom_in, zoom_out;
   XmString *xstr, title;
   int i;
   int count; 
   char line[80]; 
   float px, py, pz;


   if (allmodels == NULL)
   {
      allmodels = new GR_Model;
      allmodels->parse_model ("Models.desc");
   }
   count = (int)(allmodels->get_p_files_read ());
   xstr = (XmString*)malloc(count*sizeof(XmString));
      
   for (i=0; i<count; i++)
   {
      sprintf (line, "%d  %s", 
               allmodels->get_p_filetypes(i),
               allmodels->get_p_filenames(i));
      xstr[i] = XmStringCreate (line,
                XmSTRING_DEFAULT_CHARSET);        
   }
   
//   mv_shell = new GR_Shell;
//   mv_shell->createWidget ("ModelViewer");
   mv_shell = XtCreatePopupShell("ModelViewer", topLevelShellWidgetClass,
                                GR_toplevel, 0, 0);
   mv_form = XmCreateForm (mv_shell, "MVForm", NULL, 0);

   mv_control = XtVaCreateManagedWidget ("MVControl",
                 xmRowColumnWidgetClass, mv_form,
                 XmNleftAttachment, XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_FORM,
                 XmNrightAttachment, XmATTACH_FORM,
                 XmNheight, 40,
                 XmNtopOffset, 10,
                 XmNleftOffset, 10,
                 XmNspacing, 10,
                 XmNorientation, XmHORIZONTAL,
                 XmNpacking, XmPACK_COLUMN,
                 XmNnumColumns, 1,
                 NULL);

   mv_list = XmCreateScrolledList (mv_form, "MVList", NULL, 0);
   XtVaSetValues (mv_list,
                  XmNitems, xstr,
                  XmNitemCount, count,
                  XmNvisibleItemCount, 20,
                  NULL);
   XtAddCallback (mv_list, XmNbrowseSelectionCallback,
                  (XtCallbackProc)mv_listCB, NULL);
   XtVaSetValues (XtParent(mv_list),
                  XmNtopAttachment, XmATTACH_FORM,
                  XmNrightAttachment, XmATTACH_FORM,
                  XmNbottomAttachment, XmATTACH_WIDGET,
                  XmNbottomWidget, mv_control, 
                  NULL);

   mv_frame = XtVaCreateManagedWidget ("MVFrame",
               xmFrameWidgetClass, mv_form,
               XmNshadowType, XmSHADOW_IN,
               XmNtopAttachment, XmATTACH_FORM,
               XmNleftAttachment, XmATTACH_FORM,
               XmNbottomAttachment, XmATTACH_WIDGET,
               XmNbottomWidget, mv_control,
               XmNrightAttachment, XmATTACH_WIDGET,
               XmNrightWidget, XtParent(mv_list),
               NULL);

   mvwindow = new GR_Window ();
   mvwindow->doublebuffer ();
   mvwindow->rgbmode ();
   mvwindow->GR_Widget::createWidget ("MVWindow", mv_frame);

   mvwindow->set_viewmode (GR_PERSPECTIVE);
   mvwindow->aspect (1);
   mvwindow->field_of_view ((float)mv_FOV);
 
   px = ViewDist * cos(mv_LAT*M_PI/180.0)*sin(mv_LON*M_PI/180.0);
   py = ViewDist * sin(mv_LAT*M_PI/180.0);
   pz = ViewDist * cos(mv_LAT*M_PI/180.0)*cos(mv_LON*M_PI/180.0); 
   mvwindow->view_position (px, py, pz);
   mvwindow->look_vector (0.0, 0.0, 0.0);
   
   //mv_displist = new GR_DispList;
   //mvwindow->addDispList (mv_displist);
   //mv_displist->add_object ();

   mvwindow->set_drawfunction (mv_draw);

   done_button = XtVaCreateManagedWidget ("Quit",
                 xmPushButtonWidgetClass, mv_control,
                 XmNwidth, 20,
                 XmNheight, 30,
                 XmNshadowThickness, 4,
                 NULL);
   XtAddCallback (done_button, XmNactivateCallback,
                    (XtCallbackProc)mv_doneCB, NULL);
 
   zoom_in = XtVaCreateManagedWidget ("ZoomIn",
                 xmPushButtonWidgetClass, mv_control,
                 XmNwidth, 20,
                 XmNheight, 30,
                 XmNshadowThickness, 4,
                 NULL);
   XtAddCallback (zoom_in, XmNactivateCallback,
                    (XtCallbackProc)zoominCB, NULL);
 
   zoom_out = XtVaCreateManagedWidget ("ZoomOut",
                 xmPushButtonWidgetClass, mv_control,
                 XmNwidth, 20,
                 XmNheight, 30,
                 XmNshadowThickness, 4,
                 NULL);
   XtAddCallback (zoom_out, XmNactivateCallback,
                    (XtCallbackProc)zoomoutCB, NULL);
 
   title = XmStringCreateSimple ("Latitude"); 
   mv_lat_scale = createOneScale (mv_control, 'A', title,
                                  (XtCallbackProc)mv_sliderCB);
   XmScaleSetValue (mv_lat_scale, mv_LAT);

   title = XmStringCreateSimple ("Longitude");
   mv_lon_scale = createOneScale (mv_control, 'O', title,
                                  (XtCallbackProc)mv_sliderCB);
   XmScaleSetValue (mv_lon_scale, mv_LON);

   title = XmStringCreateSimple ("Field of View");
   mv_fov_scale = createOneScale (mv_control, 'F', title,
                                  (XtCallbackProc)mv_sliderCB);
   XmScaleSetValue (mv_fov_scale, mv_FOV);

   scale_box = XtVaCreateManagedWidget("Scaling",
      xmTextFieldWidgetClass, mv_control,
      XmNwidth,           30,
      XmNheight,          30,
      XmNshadowThickness, 3,
      NULL);
   XtAddCallback (scale_box, XmNactivateCallback,
                  (XtCallbackProc)scale_textCB, NULL);
   sprintf (scale_text, "%.1f", scale_factor);
   XtVaSetValues (scale_box, XmNvalue, scale_text, NULL);

   XmStringFree (title);

   XtManageChild (mv_form);
   XtManageChild (mv_list);
//   mv_shell->realize ();
   XtPopup(mv_shell, XtGrabNone);
//   mvwindow->draw ();
}

void 
model_viewerCB (Widget toplevel)
{
   if (first_modelviewer)
   {
      GR_toplevel = toplevel;
      first_modelviewer = FALSE;
      mv_init ();
   }
   else
   {
       XtPopup(mv_shell, XtGrabNone);
   }
}


void
mv_doneCB ()
{
  XtPopdown (mv_shell);
//  mvwindow->set_awake (FALSE);
}

void
zoominCB()
{
  scale_factor = scale_factor*2.0;
  sprintf (scale_text, "%.1f", scale_factor);
  XtVaSetValues (scale_box, XmNvalue, scale_text, NULL);
  mvwindow->draw ();
}

void
zoomoutCB()
{
  scale_factor = scale_factor*0.5;
  sprintf (scale_text, "%.1f", scale_factor);
  XtVaSetValues (scale_box, XmNvalue, scale_text, NULL);
  mvwindow->draw ();
}

void
scale_textCB ()
{
}

void
mv_listCB (Widget, XtPointer, XmListCallbackStruct* call_data)
{
char *text;

  XmStringGetLtoR (call_data->item,
                   XmSTRING_DEFAULT_CHARSET,
                   &text); 
  sscanf (text, "%d%*s", &model_type);
  parse_model("Models.desc", model_type);
  scale_factor = 1.0;
  sprintf (scale_text, "%.1f", scale_factor);
  XtVaSetValues (scale_box, XmNvalue, scale_text, NULL);
  mvwindow->draw ();
}

void drawxyz ()
{
  register short i;

  GR_pushattributes ();
  GR_color (255, 255, 255);
  GR_move (0.0, 0.0, 0.0);
  GR_draw (2.0, 0.0, 0.0);
  GR_move (0.0, 0.0, 0.0);
  GR_draw (0.0, 2.0, 0.0);
  GR_move (0.0, 0.0, 0.0);
  GR_draw (0.0, 0.0, 2.0);
  GR_color (0, 0, 0);
  for (i=1; i<=20; i++)
  {
     GR_move (i*0.1, 0.0, 0.0);
     GR_draw (i*0.1, 0.02, 0.0);
  }
  GR_cmov (2.0, 0.0, 0.0);
  GR_charstr("X");
  GR_color (255, 0, 0);
  for (i=1; i<=20; i++)
  {
     GR_move (0.0, i*0.1, 0.0);
     GR_draw (0.02, i*0.1, 0.0);
  }
  GR_cmov (0.0, 2.0, 0.0);
  GR_charstr("Y");
  GR_color (0, 0, 255);
  for (i=1; i<=20; i++)
  {
     GR_move (0.0, 0.0, i*0.1);
     GR_draw (0.0, 0.02, i*0.1);
  }
  GR_cmov (0.0, 0.0, 2.0);
  GR_charstr("Z");
  GR_popattributes ();
}

void
mv_draw ()
{

  drawxyz ();
/*
  scale_factor = get_scale (model_type);
            if (model_type == 24)
               scale_factor *= 0.5; // debugging: use small scale;
            else if (model_type == 2 || model_type == 17 ||
                     model_type == 150 || model_type == 151)
               scale_factor *= 5;   // debugging: use larger scale;
            else if (model_type == 6 || model_type == 18) // rv
               scale_factor *= 5; // debugging: use larger scale;
*/
  parse_model("Models.desc", model_type);
/*
  if (allmodels)
  {
     allmodels->reset();
     allmodels->set_type (model_type);
     scale_factor = get_scale (model_type);
        scale_factor *= 2;  // for better single-model view 
     allmodels->scale (scale_factor, scale_factor, scale_factor);
     //allmodels->GR_DispObj::draw(); 
     allmodels->draw(); 
  }  
  else
     printf (" Oops, allmodels == NULL?\007\n");
*/
}

void 
mv_sliderCB (Widget, char client_data, XmScaleCallbackStruct* call_data)
{
   char code = client_data;
   float px, py, pz;

   if (code == 'A' || code == 'O')
   {
     if (code == 'A')
         mv_LAT = call_data->value;
     else
         mv_LON = call_data->value;

     px = ViewDist * cos(mv_LAT*M_PI/180.0)*sin(mv_LON*M_PI/180.0);
     py = ViewDist * sin(mv_LAT*M_PI/180.0);
     pz = ViewDist * cos(mv_LAT*M_PI/180.0)*cos(mv_LON*M_PI/180.0);
     mvwindow->view_position (px, py, pz);
     mvwindow->look_vector (0.0, 0.0, 0.0);
   }
   else if (code == 'F')
   {
     mv_FOV = call_data->value;
     mvwindow->field_of_view((float)mv_FOV);
   }
   else
   {
     XtWarning ("Unknown code in mv_sliderCB function.");
     return;
   }
   
   mvwindow->request_draw (250);

}

void
parse_model (char* filename, int type_wanted)
{
   FILE*        fileptr;
   char		line [81];
   long		i, type;
   char		model_path[80], model_file [80];
   int          found, done;
   char         *MODELDIR;
   int		p_files_read, sea_polys, land_polys;
   
   if (filename == NULL)
      filename = "Models.desc";
   
   p_files_read = 0;
   sea_polys = 0;
   land_polys = 0;
   
   fileptr = fopen (filename, "r");
   if (fileptr == NULL)
   {
      fprintf (stderr, "Cannot access file %s\007\n", filename);
      return;
   }

   if ((MODELDIR = getenv("MODELDIR")) == NULL)
   {
	MODELDIR = "./Models";
   }

   done = FALSE;
   while ( (fgets (line, 80, fileptr) != NULL) & (done != TRUE) )
   {
      //this will parse comments in the model description file
      if ( strncmp(line,"#",1) ==  0 )
      {
	 continue;
      }
      sscanf (line, "%d", &type);
      sscanf (line, "%*s%s", model_file);

      if ( type == type_wanted ) 
      {
         if (MODELDIR != NULL)
            sprintf (model_path, "%s/%s", MODELDIR, model_file);
         else
            sprintf (model_path, "./Models/%s", model_file);
	 load_model (model_path, type);
         done = TRUE;
      }

   }
   fclose (fileptr);
}

void 
load_model (char* file_name, long type) 
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

   fp = fopen(file_name,"r");
   if ( fp == NULL )
   {
      printf("C_Model: Error on file open for file %s\007\n",file_name);
      return;
   }

   //printf("Object type is %d scaled by %f\n", type, scale_factor);
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   glFrustum(-1.0, 1.0, -1.0, 1.0, 1.5, 20.0);
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();
   gluLookAt(0.0, 0.0, 5.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);
   glScalef(scale_factor, scale_factor, scale_factor);
   GR_pushattributes ();

   while ( fgets(line,80,fp) != NULL )
   {
      memset( (void *)cmnd, '\0', (size_t)(sizeof(cmnd)) );
      sscanf(line,"%s",cmnd);
//      printf("Model: processing command %s\n", cmnd);
 
      if ( strcmp(cmnd,"scale") == 0 )
      {
         if (sscanf(line,"%*s%f%f%f",&x,&y,&z) == 3)
         {
            GR_scale(x,y,z);
         }
      }
   
      if ( strcmp(cmnd,"bgnpoint") == 0 )
      {
	 GR_bgnpoint();
      }

      if ( strcmp(cmnd,"endpoint") == 0 )
      {
	 GR_endpoint();
      }

      if ( strcmp(cmnd,"geomfile") == 0 )
      {
	 sscanf(line, "%s%s",cmnd, geom_file);
	 strcpy(cmnd,"");
      }

      if ( strcmp(cmnd,"earthmodel") == 0 )
      {
	 printf("EARTHMODEL\n");
	 strcpy(cmnd,"");
      }

      if ( strcmp(cmnd,"lightmodelfile") == 0 )
      {
	 sscanf(line, "%s%s",cmnd, geom_file);
	 strcpy(cmnd,"");
      }

      if ( strcmp(cmnd,"editfile") == 0 )
      {
	 sscanf(line, "%s%s",cmnd, geom_file);
	 strcpy(cmnd,"");
      }

      if ( strcmp(cmnd,"flatfile") == 0 )
      {
	 sscanf(line, "%s%s",cmnd, geom_file);
	 strcpy(cmnd,"");
      }

      if ( strcmp(cmnd,"gouraudfile") == 0 )
      {
	 sscanf(line, "%s%s",cmnd, geom_file);
	 strcpy(cmnd,"");
      }

      if ( strcmp(cmnd,"boundryfile") == 0 )
      {
      }

      if ( strcmp(cmnd,"line") == 0 )
      {
	 fgets(line,80,fp);
	 sscanf(line,"%*s%f%f%f",&x,&y,&z);
	 GR_move(x,y,z);
	 fgets(line,80,fp);
	 sscanf(line,"%*s%f%f%f",&x,&y,&z);
	 GR_draw(x,y,z);
      }

      if ( strcmp (cmnd, "color") == 0 )
      {
	 sscanf (line, "%*s%d%d%d", &r, &g, &b);
      }

      if ( strcmp (cmnd, "pattern") == 0 )
      {
         sscanf(line, "%*s%d", &i);
	 GR_setpattern ((short) i);
      }

	 if ( strcmp(cmnd,"crv") == 0 )
	 {
	    float p[4][3];
	    short c[3];
	    int num_pts = 0;
	    GR_matrix basmat; 
	   /* 
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
	    */	    
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

	 if ( strcmp(cmnd,"poly") == 0 )
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
         
         if ( strcmp(cmnd,"tmesh") == 0 )  // new part added 7/13/93:
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
         
	 if (strcmp(cmnd,"qstrip")==0) // new part added 06/14/93:
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

     /* else if ( strcmp(cmnd,"scale") == 0 )
      {
	 x=1.0, y=1.0, z=1.0; // to prevent wrong scale due to failed sscanf:
	 sscanf(line,"%*s%f%f%f",&x,&y,&z);
	 GR_scale(x,y,z);
      }
     */

      if ( strcmp(cmnd,"rotatex") == 0 )
      {
	 sscanf(line,"%*s%ld",&angle);
	 GR_rotate((short) angle * 10,'x');
      }

      if ( strcmp(cmnd,"rotatey") == 0 )
      {
	 sscanf(line,"%*s%ld",&angle);
	 GR_rotate((short) angle * 10,'y');
      }

      if ( strcmp(cmnd,"rotatez") == 0 )
      {
	 sscanf(line,"%*s%ld",&angle);
	 GR_rotate((short) angle * 10,'z');
      }

      if ( strcmp(cmnd,"translate") == 0 )
      {
	 sscanf(line,"%*s%f%f%f",&x,&y,&z);
	 GR_translate(x,y,z);
      }

      if ( strcmp(cmnd,"move") == 0 )
      {
	 sscanf(line,"%*s%f%f%f",&x,&y,&z);
	 GR_move(x,y,z);
      }

      if ( strcmp(cmnd,"pushmatrix") == 0 )
      {
	 GR_pushmatrix();
      }

      if ( strcmp(cmnd,"popmatrix") == 0 )
      {
	 GR_popmatrix();
      }

   }  // end of while

   GR_popattributes (); 
   glFlush();
   fclose(fp);
}

