/******************************************************************

  Graphics -- Interactive Speedes interface
  -- 06/04/93: created from sps.C made 11/24/92, by Tung;
  -- 08/24/93: 
    
*******************************************************************/
#include "newsps.H"
#include "speedes_graphics.H"
#include "GR_Spslinks.H"
#include "GISP_Obj.H"

float T0, DT;
struct tms Tbuf;

GR_Shell *sps_panel;
GR_DispList *sps_displist;
GR_DispList *sps_links_displist;
extern GR_Window *gwindow;

extern float get_scale (long type);

void sps_progressCB (Widget, XtPointer, XmScaleCallbackStruct*);
void sps_timestepCB (Widget, int);
void sps_textCB (Widget, int);
void sps_fplayCB ();
void sps_rplayCB ();
void sps_pauseCB ();
void sps_stepCB ();
void sps_resetCB ();

XtWorkProcId spsId = NULL;
Boolean spsWP (XtPointer);
 
C_SPEEDES_GRAPHICS *speedes_graphics;
C_SPEEDES_STATE    *speedes_state;
double speedes_time;
double speedes_gvt;
double speedes_slider_time;
double speedes_start_time = 0.0;
double speedes_end_time = 99999.0;
double speedes_delta_t = 10.0;
int speedes_direction = 1;
Boolean sps_singlestep_mode = FALSE;

Widget sps_form;
Widget sps_progress_slider, sps_progress_text_w;
Widget sps_timestep_text_w;

// next two are called within main program:
Widget makeSpanel (char* name, Widget parent);
void speedes_init ();

GR_Spslinks *spslinks=NULL;
short LMASK = 0xffff;
void sps_linksCB (Widget, XtPointer client_data, XtPointer);
void sps_com_linksCB (Widget, XtPointer client_data, XtPointer);
void sps_sen_linksCB (Widget, XtPointer client_data, XtPointer);


/* ============================================= */

Widget
makeSpanel (char* name, Widget parent)
{
   Widget spanel;
   Widget progress_area;
   Widget progress_label;  //Widget progress_slider, progress_text_w;
   Widget timestep_area;
   Widget timestep_label, timestep_buttons; //Widget timestep_text_w;
   Widget timestep_b;
   Widget button_area;
   Widget fplay_b, rplay_b, pause_b, step_b, reset_b;
   XmString title;


   Pixel fg, bg;
   Pixmap pixmap;
   char *BITMAPDIR;
   char rplayfile[80], fplayfile[80], pausefile[80];
 
   if ((BITMAPDIR=getenv("BITMAPDIR"))==NULL)
     BITMAPDIR = "../../data";
   sprintf (rplayfile, "%s%s", BITMAPDIR, "/bitmaps/rplay.bit");
   sprintf (pausefile, "%s%s", BITMAPDIR, "/bitmaps/pause.bit");
   sprintf (fplayfile, "%s%s", BITMAPDIR, "/bitmaps/fplay.bit");
   
   spanel = XmCreateForm (parent, name, NULL, 0);
   XtManageChild (spanel);
   XtVaSetValues (spanel,
		  XmNfractionBase, 100,
                  NULL);

   progress_area = XtVaCreateManagedWidget
     ("ProgressArea",
      xmFormWidgetClass, spanel,
      XmNleftAttachment, XmATTACH_POSITION,
      XmNleftPosition, 0,
      XmNrightAttachment, XmATTACH_POSITION,
      XmNrightPosition, 30,
      XmNtopAttachment, XmATTACH_FORM,
      XmNbottomAttachment, XmATTACH_FORM,
      XmNorientation, XmHORIZONTAL,
      XmNfractionBase, 100,
      NULL);
   
   timestep_area = XtVaCreateManagedWidget
     ("TimestepArea",
      xmFormWidgetClass, spanel,
      XmNleftAttachment, XmATTACH_POSITION,
      XmNleftPosition, 31,
      XmNrightAttachment, XmATTACH_POSITION,
      XmNrightPosition, 70,
      XmNtopAttachment, XmATTACH_FORM,
      XmNbottomAttachment, XmATTACH_FORM,
      XmNorientation, XmHORIZONTAL,
      XmNspacing, 10,
      NULL);

   button_area = XtVaCreateManagedWidget
     ("ButtonArea",
      xmRowColumnWidgetClass, spanel,
      XmNleftAttachment, XmATTACH_POSITION,
      XmNleftPosition, 71,
      XmNrightAttachment, XmATTACH_POSITION,
      XmNrightPosition, 99,
      XmNtopAttachment, XmATTACH_FORM,
      XmNbottomAttachment, XmATTACH_FORM,
      XmNorientation, XmHORIZONTAL,
      XmNspacing, 4,
      XmNpacking, XmPACK_COLUMN,
      XmNnumColumns, 1,
      NULL);
   
   title = XmStringCreateSimple ("Sim Time: ");
   progress_label = XtVaCreateManagedWidget
     ("ProgressLabel",
      xmLabelGadgetClass, progress_area,
      XmNlabelString, title,
      XmNheight, 40,
      XmNtopAttachment, XmATTACH_FORM,
      XmNleftAttachment, XmATTACH_POSITION,
      XmNleftPosition, 3,
      XmNrightAttachment, XmATTACH_POSITION,
      XmNrightPosition, 25,
      NULL);
   XmStringFree (title);
   
   sps_progress_slider = XtVaCreateManagedWidget
     ("ProgressSlider",
      xmScaleWidgetClass, progress_area,
      XmNorientation, XmHORIZONTAL,
      XmNshowValue, True,
      XmNdecimalPoints, 1,
      XmNscaleMultiple, 100,
      XmNheight, 40,
      XmNtopAttachment, XmATTACH_FORM,
      XmNleftAttachment, XmATTACH_POSITION,
      XmNleftPosition, 27,
      XmNrightAttachment, XmATTACH_POSITION,
      XmNrightPosition, 78,
      NULL);
   XtAddCallback (sps_progress_slider, XmNvalueChangedCallback,
		     (XtCallbackProc)sps_progressCB, NULL);
   XtAddCallback (sps_progress_slider, XmNdragCallback,
		  (XtCallbackProc)sps_progressCB, NULL);
   XtVaSetValues (sps_progress_slider,
		  XmNminimum, (int)(speedes_start_time*10),
		  XmNmaximum, (int)(speedes_end_time*10),
		  NULL);

   sps_progress_text_w = XtVaCreateManagedWidget
     ("ProgressText",
      xmTextFieldWidgetClass, progress_area,
      XmNtraversalOn, True,
      XmNheight, 40,
      XmNtopAttachment, XmATTACH_FORM,
      XmNleftAttachment, XmATTACH_POSITION,
      XmNleftPosition, 80,
      XmNrightAttachment, XmATTACH_POSITION,
      XmNrightPosition, 97,
      XmNshadowThickness, 3,
      NULL);
   XtAddCallback (sps_progress_text_w, XmNactivateCallback,
		  (XtCallbackProc)sps_textCB,
		  (XtPointer)0);   // 0 for setting sim time;

   char time_string[10];
   sprintf (time_string, "%.1f", speedes_start_time);
   XtVaSetValues (sps_progress_text_w,
		  XmNvalue, time_string,
		  NULL);
   
   title = XmStringCreateSimple ("Time Step: ");
   timestep_label = XtVaCreateManagedWidget
     ("TimestepLabel",
      xmLabelGadgetClass, timestep_area,
      XmNlabelString, title,
      XmNheight, 40,
      XmNtopAttachment, XmATTACH_FORM,
      XmNleftAttachment, XmATTACH_POSITION,
      XmNleftPosition, 3,
      XmNrightAttachment, XmATTACH_POSITION,
      XmNrightPosition, 23,
      NULL);
   XmStringFree (title);
   timestep_buttons = XtVaCreateManagedWidget
     ("TimestepButtons",
      xmRowColumnWidgetClass, timestep_area,
      XmNheight, 40,
      XmNtopAttachment, XmATTACH_FORM,
      XmNleftAttachment, XmATTACH_POSITION,
      XmNleftPosition, 25,
      XmNrightAttachment, XmATTACH_POSITION,
      XmNrightPosition, 80,
      XmNorientation, XmHORIZONTAL,
      XmNpacking, XmPACK_COLUMN,
      XmNnumColumns, 1,
      XmNspacing, 1,
            NULL);

   timestep_b = XtVaCreateManagedWidget
     (" 1",
      xmPushButtonWidgetClass, timestep_buttons,
      XmNwidth, 1,
      XmNheight, 28,
      XmNshadowThickness, 2,
      NULL);
   XtAddCallback (timestep_b, XmNactivateCallback,
		  (XtCallbackProc)sps_timestepCB, (XtPointer)1);

   timestep_b = XtVaCreateManagedWidget
     (" 2",
      xmPushButtonWidgetClass, timestep_buttons,
      XmNwidth, 1,
      XmNheight, 28,
      XmNshadowThickness, 2,
      NULL);
   XtAddCallback (timestep_b, XmNactivateCallback,
		  (XtCallbackProc)sps_timestepCB, (XtPointer)2);

   timestep_b = XtVaCreateManagedWidget
     (" 5",
      xmPushButtonWidgetClass, timestep_buttons,
      XmNwidth, 1,
      XmNheight, 28,
      XmNshadowThickness, 2,
      NULL);
   XtAddCallback (timestep_b, XmNactivateCallback,
		  (XtCallbackProc)sps_timestepCB, (XtPointer)5);
   
   timestep_b = XtVaCreateManagedWidget
     ("10",
      xmPushButtonWidgetClass, timestep_buttons,
      XmNwidth, 1,
      XmNheight, 28,
      XmNshadowThickness, 2,
      NULL);
   XtAddCallback (timestep_b, XmNactivateCallback,
		  (XtCallbackProc)sps_timestepCB, (XtPointer)10);
   
   timestep_b = XtVaCreateManagedWidget
     ("20",
      xmPushButtonWidgetClass, timestep_buttons,
      XmNwidth, 1,
      XmNheight, 28,
      XmNshadowThickness, 2,
      NULL);

   XtAddCallback (timestep_b, XmNactivateCallback,
		  (XtCallbackProc)sps_timestepCB, (XtPointer)20);

   timestep_b = XtVaCreateManagedWidget
     ("50",
      xmPushButtonWidgetClass, timestep_buttons,
      XmNwidth, 1,
      XmNheight, 28,
      XmNshadowThickness, 2,
      NULL);
   XtAddCallback (timestep_b, XmNactivateCallback,
		  (XtCallbackProc)sps_timestepCB, (XtPointer)50);
   
   timestep_b = XtVaCreateManagedWidget
     ("100",
      xmPushButtonWidgetClass, timestep_buttons,
      XmNwidth, 1,
      XmNheight, 28,
      XmNshadowThickness, 2,
      NULL);
   XtAddCallback (timestep_b, XmNactivateCallback,
		  (XtCallbackProc)sps_timestepCB, (XtPointer)100);

   sps_timestep_text_w = XtVaCreateManagedWidget
     ("TimestepText",
      xmTextFieldWidgetClass, timestep_area,
      XmNtraversalOn, True,
      XmNheight, 40,
      XmNtopAttachment, XmATTACH_FORM,
      XmNleftAttachment, XmATTACH_POSITION,
      XmNleftPosition, 82,
      XmNrightAttachment, XmATTACH_POSITION,
      XmNrightPosition, 97,
      XmNshadowThickness, 3,
      NULL);
   XtAddCallback (sps_timestep_text_w, XmNactivateCallback,
		  (XtCallbackProc)sps_textCB,
		  (XtPointer)1);   // 1 for setting time step;

   char delta_t_string[10];
   sprintf (delta_t_string, "%.1f", speedes_delta_t);
   XtVaSetValues (sps_timestep_text_w,
		  XmNvalue, delta_t_string,
		  NULL);
   
   rplay_b = XtVaCreateManagedWidget
     ("<",
      xmPushButtonWidgetClass, button_area,
      XmNwidth, 30,
      XmNheight, 24,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (rplay_b, XmNactivateCallback,
		  (XtCallbackProc)sps_rplayCB, (XtPointer)0);

   XtVaGetValues (rplay_b, XmNforeground, &fg,
		  XmNbackground, &bg, NULL);
   pixmap = XmGetPixmap(XtScreen(rplay_b), rplayfile, fg, bg);
   if (pixmap == XmUNSPECIFIED_PIXMAP)
     printf("   Couldn't load file %s\n", rplayfile);
   else
     XtVaSetValues (rplay_b,
		    XmNlabelType, XmPIXMAP,
		    XmNlabelPixmap, pixmap,
		    NULL);
   
   pause_b = XtVaCreateManagedWidget
     ("||",
      xmPushButtonWidgetClass, button_area,
      XmNwidth, 30,
      XmNheight, 24,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (pause_b, XmNactivateCallback,
		  (XtCallbackProc)sps_pauseCB, (XtPointer)0);

   XtVaGetValues (pause_b, XmNforeground, &fg,
		  XmNbackground, &bg, NULL);
   pixmap = XmGetPixmap(XtScreen(pause_b), pausefile, fg, bg);
   if (pixmap == XmUNSPECIFIED_PIXMAP)
     printf("   Couldn't load file %s\n", pausefile);
   else
     XtVaSetValues (pause_b,
		    XmNlabelType, XmPIXMAP,
		    XmNlabelPixmap, pixmap,
		    NULL);

   fplay_b = XtVaCreateManagedWidget
     (">",
      xmPushButtonWidgetClass, button_area,
      XmNwidth, 30,
      XmNheight, 24,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (fplay_b, XmNactivateCallback,
		  (XtCallbackProc)sps_fplayCB, (XtPointer)0);

   XtVaGetValues (fplay_b, XmNforeground, &fg,
		  XmNbackground, &bg, NULL);
   pixmap = XmGetPixmap(XtScreen(fplay_b), fplayfile, fg, bg);
   if (pixmap == XmUNSPECIFIED_PIXMAP)
     printf("   Couldn't load file %s\n", fplayfile);
   else
     XtVaSetValues (fplay_b,
		    XmNlabelType, XmPIXMAP,
		    XmNlabelPixmap, pixmap,
		    NULL);
   
   step_b = XtVaCreateManagedWidget
     ("Step",
      xmPushButtonWidgetClass, button_area,
      XmNwidth, 30,
      XmNheight, 24,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (step_b, XmNactivateCallback,
		  (XtCallbackProc)sps_stepCB, (XtPointer)0);
   
   reset_b = XtVaCreateManagedWidget
     ("Reset",
      xmPushButtonWidgetClass, button_area,
      XmNwidth, 30,
      XmNheight, 24,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (reset_b, XmNactivateCallback,
		  (XtCallbackProc)sps_resetCB, (XtPointer)0);
   

   return (spanel);
}


void
speedes_init ()
{
   speedes_graphics = new C_SPEEDES_GRAPHICS();
   speedes_state = speedes_graphics->get_speedes_state();
   speedes_start_time = speedes_graphics->get_start_time () + 0.001;
   speedes_end_time = speedes_graphics->get_end_time ();
   speedes_time = speedes_start_time;
   speedes_gvt = 0.0;
   sps_displist = new GR_DispList;
   gwindow->addDispList (sps_displist, "sps_displist");
   sps_links_displist = new GR_DispList;
   gwindow->addDispList (sps_links_displist, "sps_links_displist");
   
   XtVaSetValues (sps_progress_slider,
		   XmNminimum, (int)(speedes_start_time*10),
		   XmNmaximum, (int)(speedes_end_time*10),
		   NULL);
   
   spslinks = NULL;
   LMASK = 0xffff;

}


/* %%%%%%%%%%%%%%%%% OLD routines: %%%%%%%%%%%%%%%%% */


void
sps_progressCB (Widget, XtPointer, XmScaleCallbackStruct *cbs)
{
  int scale_value = cbs->value;
  char time_string[20];

  speedes_slider_time = scale_value/10.0;
  speedes_time = speedes_slider_time;
  sprintf (time_string, "%.1f", speedes_time);
  XtVaSetValues (sps_progress_text_w,
                 XmNvalue, time_string,
                 NULL);
  if (spsId)
  {
     XtRemoveWorkProc (spsId);
     spsId = XtAppAddWorkProc  (GR_appcontext, (XtWorkProc)spsWP, 0);
  }
  if (sps_displist)
  {
     sps_displist->delete_objects ();
  }
}


void
sps_timestepCB (Widget, int value)
{
   char delta_t_string[20];
   
   speedes_delta_t = value;
   sprintf (delta_t_string, "%.1f", speedes_delta_t);
   XtVaSetValues (sps_timestep_text_w,
		  XmNvalue, delta_t_string,
		  NULL);
     if (spsId)
  {
      XtRemoveWorkProc (spsId);
      spsId = XtAppAddWorkProc  (GR_appcontext, (XtWorkProc)spsWP, 0);
  }
}


void
sps_textCB (Widget text_w, int code)
{
  double value;
  char *text_w_str = XmTextFieldGetString (text_w);

  if (code==0) // Sim Time Progress:
  {
     if (sscanf (text_w_str, "%lf", &value) == 1)
     {
	speedes_time = value;
	if (spsId)
	{
	   XtRemoveWorkProc (spsId);
	   spsId = XtAppAddWorkProc  (GR_appcontext, (XtWorkProc)spsWP, 0);
	}
     }  
     else
       printf ("   invalid value entered.\n");
  }
  else if (code==1)   // Time Step:
  {
     if (sscanf (text_w_str, "%lf", &value) == 1)
     {
	speedes_delta_t = value; 
	//printf ("   speedes_delta_t is now set to: %f\n", speedes_delta_t);
	if (spsId)
	{  
	   XtRemoveWorkProc (spsId);
	   spsId = XtAppAddWorkProc  (GR_appcontext, (XtWorkProc)spsWP, 0);
	}
     }    
     else
       printf ("   invalid value entered.\n");
  }
  XtFree (text_w_str);
}


/* -------------------- */
void
sps_fplayCB ()
{
   if (spsId)
     XtRemoveWorkProc (spsId);
   speedes_direction = 1;
   spsId = XtAppAddWorkProc  (GR_appcontext, (XtWorkProc)spsWP, 0);
   if (sps_displist)
     gwindow->addDispList (sps_displist, "sps_displist");
}

void
sps_rplayCB ()
{
   if (spsId)
     XtRemoveWorkProc (spsId);
   speedes_direction = -1;
   spsId = XtAppAddWorkProc  (GR_appcontext, (XtWorkProc)spsWP, 0);
   if (sps_displist)
     gwindow->addDispList (sps_displist, "sps_displist");
}

void
sps_stepCB ()
{
   if (spsId)
     XtRemoveWorkProc (spsId); 
   sps_singlestep_mode = TRUE;
   spsId = XtAppAddWorkProc  (GR_appcontext, (XtWorkProc)spsWP, 0);
}


void
sps_pauseCB ()
{
   if (spsId)
   {
      XtRemoveWorkProc (spsId);
      spsId = NULL;
   }
}

void
sps_resetCB ()
{
   if (spsId)
   {
      XtRemoveWorkProc (spsId);
      spsId = NULL;
   }
   if (sps_links_displist)
   {
      sps_links_displist->delete_objects ();
   }   
   if (sps_displist)
   {
      sps_displist->delete_objects ();
      //gwindow->remDispList (sps_displist, "sps_displist");
      gwindow->draw ();
   }
   speedes_time = speedes_graphics->get_start_time () + 0.001;
   speedes_direction = 1;
   XmScaleSetValue (sps_progress_slider, (int)(speedes_time*10));
}


/* ================= */

Boolean
spsWP (XtPointer)
{
  static GISP_Obj *anobj=NULL;
  C_QUEUE *objects, *links;
  C_LINK *link;
  C_SPEEDES_OBJECT *speedes_object, *sensor_object, *track_object;
  int sensor_id, track_id, n_speedes_objects, n_links;
  int i, icontype, id;
  double X[3], V[3];
  double Xs[3], Vs[3];
  double Xt[3], Vt[3];
  double Vx, Vy, Vz;
  double Px, Py, Pz;
  float scale_factor, sfactor2, rx, ry;
  // float rz;
  double Rmax;
  float vmag2;
  // static GR_Spslinks *spslinks = NULL;
  double sloc[3], tloc[3];
  short rgb[3];
  int errcount = 0;
  short debug_link_type;


  while (speedes_gvt < speedes_time)
  {
     // DRE if (speedes_graphics->messages() == NULL) break;
     speedes_gvt = speedes_graphics->get_gvt();
  }
  if (speedes_gvt < speedes_time)
    return FALSE;

  //T0=times(&Tbuf);
  speedes_graphics->process(speedes_time);
  objects = speedes_state->get_objects();
  links = speedes_state->get_links();
  n_speedes_objects = objects->get_length();
  n_links = links->get_length();
  //DT=times(&Tbuf) - T0;
  //printf (":> speedes_time = %f\n", speedes_time);
  //printf (":> pre-processing takes %d ticks\n", DT);
//  printf ("==> number of SPEEDES objects is: %d, at %f\n", 
//      n_speedes_objects, speedes_time);

  //...... set up the sps_displist for GISP_Obj objects
  if (anobj != NULL)
  {
     delete[] anobj;
     anobj = NULL;
  }
  if (n_speedes_objects)
  {
    anobj = new GISP_Obj[n_speedes_objects];
  }
  sps_displist->delete_objects();
  
  //...... go through each speedes object and fill the display list

  //T0=times(&Tbuf);
  speedes_object = (C_SPEEDES_OBJECT *)objects->get_top();
  for (i=0; i<n_speedes_objects; i++)
  {
     if (speedes_object->get_alive ())
     {
	speedes_object->get_pos_vel(speedes_time,X,V);
	icontype = speedes_object->get_icon();
	if (icontype < 0)
        {
          errcount++; 
	  icontype = 199;  //  make sure it is valid............
        }
        //else if (icontype == 24)
        //  icontype = 55;
        else if (icontype == 0)
          icontype = 76; // for debugging: ROCC
        else if (icontype == 105)
          icontype = 42; // for debugging: purple ball
        //else if (icontype == 127)
        //  icontype = 43; // for debugging: brown ball -- radar?
        else if (icontype == 37)
        {
          // printf ("conv. a new impact model...\n");
          icontype = 38; // for debugging: new_impact2
        }
 
 	id = speedes_object->get_unique_id();
	
	//...... fill graphics stuff
	
	anobj[i].init(id,icontype);
	sps_displist->add_object(&anobj[i]);
        sfactor2 = speedes_object->get_scale ();
        Rmax = speedes_object->get_Rmax ();
        
        if (sfactor2 < 0.01)
          anobj[i].set_visible_flag (FALSE);
        else
        {
	  scale_factor = get_scale (icontype);
            if (icontype == 24)
               scale_factor *= 0.5; // debugging: use small scale;
            else if (icontype == 2 || icontype == 17 ||
                     icontype == 150 || icontype == 151)
               scale_factor *= 5;   // debugging: use larger scale;
            else if (icontype == 6 || icontype == 18) // rv
               scale_factor *= 5; // debugging: use larger scale; 
            else if (icontype == 127)
               scale_factor *= Rmax/RE;

	  scale_factor *= sfactor2;
	  anobj[i].scale (scale_factor, scale_factor, scale_factor);
	  anobj[i].rotate_z (180);

	  vmag2 = V[0]*V[0] + V[1]*V[1] + V[2]*V[2];
	  if (vmag2 != 0)
          {
	   if (icontype==199 || icontype==90)
	     printf ("Type: %d has non-zero velocity: %f\n",
		     icontype, vmag2); 
	   Vx = V[1];
	   Vy = V[2];
	   Vz = V[0];
	   rx = -atan2(Vy,sqrt(Vz*Vz+Vx*Vx))*180.0/M_PI;
	   ry = atan2(Vx,Vz)*180.0/M_PI;
           if (icontype==55)  // or any aircraft model.......
           {
             anobj[i].rotate_y (180);  // ??
	   }
           else if (icontype==24)  // ssts or similar (sensor?) model .....
           {
             //anobj[i].rotate_z (-90);
           }

           // added roll around the body axis, 7/29/93:
           /*
           Px = X[1];
           Py = X[2];
           Pz = X[0];
           rz = atan2(Px,Pz)*180.0/M_PI;
           anobj[i].rotate_z (rz);
           */

                     // Jeff's try: 
                     /*
                     double vhat[3], rhat[3], what[3];
                     int indx;
                     double rmag,vmag;
                     rmag = sqrt(X[0]*X[0]+X[1]*X[1]+X[2]*X[2]);
                     vmag = sqrt(V[0]*V[0]+V[1]*V[1]+V[2]*V[2]);
                     for (indx=0; indx<3; indx++) {
                       rhat[indx]=X[indx]/rmag;
                       vhat[indx]=V[indx]/vmag;
                     }
                     what[0] = rhat[1]*vhat[2]-vhat[1]*rhat[2];
                     what[1] = rhat[2]*vhat[0]-vhat[2]*rhat[0];
                     what[2] = rhat[0]*vhat[1]-vhat[0]*rhat[1];
                     rz = acos(what[0]);
                     anobj[i].rotate_z(rz*180.0/M_PI);
                     */
           anobj[i].rotate_x (rx);
	   anobj[i].rotate_y (ry);

 	  }
	  else
 	  {
	   Px = X[1];
	   Py = X[2];
	   Pz = X[0];
	   rx = -atan2(Py,sqrt(Pz*Pz+Px*Px))*180.0/M_PI;
	   ry = atan2(Px,Pz)*180.0/M_PI;
	   if (icontype!=127)   // if it is not the ground sensor:
             anobj[i].rotate_x (90);
	   anobj[i].rotate_x (rx);
	   anobj[i].rotate_y (ry);
	  }
          // for debugging only, 7/29/93..............................
	//  double magpos = sqrt(X[0]*X[0]+X[1]*X[1]+X[2]*X[2]) - 1.0;
	//  anobj[i].translate (X[1]/magpos,X[2]/magpos,X[0]/magpos);
	  anobj[i].translate (X[1]/RE,X[2]/RE,X[0]/RE);
	  //...... end of fill graphics stuff
       }
     }
     speedes_object = (C_SPEEDES_OBJECT *)speedes_object->get_link();
  }
  if (errcount>0)
     printf ("Warning: %d icontypes not set.\n", errcount);

  //DT=times(&Tbuf) - T0; 
  //printf (":> n_speedes_objects = %d, DT = %f, average DT/n = %f ticks\n",
  // n_speedes_objects, DT, DT/n_speedes_objects);


  //...... set up the link display list
  if (spslinks != NULL)
  {
     delete spslinks;
     spslinks = NULL;
  }
  sps_links_displist->delete_objects();
  if (n_links > 0)
  {
     spslinks = new GR_Spslinks (n_links);
     sps_links_displist->add_object (spslinks);
  }



  //T0=times(&Tbuf);
  link = (C_LINK *)links->get_top();
  for (i=0; i<n_links; i++)
  {
     //...... fill graphics link stuff
     sensor_id = link->get_sensor_id();
     track_id = link->get_track_id();
     sensor_object = speedes_state->get_object(sensor_id);
     track_object = speedes_state->get_object(track_id);
     if ((sensor_object != NULL)
	 && (track_object != NULL)
	 && sensor_object->get_alive()
	 && track_object->get_alive())
     {
       if (sensor_object->get_icon () == track_object->get_icon ())
	 debug_link_type = 0x0001;
       else
	 debug_link_type = 0x0002;
       if (debug_link_type & LMASK)
       //if (link->get_type () & LMASK)
       {
	 sensor_object->get_pos_vel(speedes_time,Xs,Vs);
	 track_object->get_pos_vel(speedes_time,Xt,Vt);
	 sloc[0] = (float)Xs[1]/RE;
	 sloc[1] = (float)Xs[2]/RE;
	 sloc[2] = (float)Xs[0]/RE;
	 tloc[0] = (float)Xt[1]/RE;
	 tloc[1] = (float)Xt[2]/RE;
	 tloc[2] = (float)Xt[0]/RE;
	 rgb[0] = (short)link->get_red ();
	 rgb[1] = (short)link->get_green ();
	 rgb[2] = (short)link->get_blue ();
	 spslinks->add_link (i, sloc, tloc, rgb);
	 //...... end of fill graphics link stuff
       }
     }
     link = (C_LINK *)link->get_link();
   }
  //DT=times(&Tbuf) - T0;
  //printf (":> n_links = %d, DT = %f, average DT/n = %f ticks\n",
  // n_links, DT, DT/n_links);
  
  
  if (speedes_time<(speedes_start_time+speedes_delta_t)
      && speedes_direction<0)
    printf ("Warning: trying to go beyond simulation start time limit.\n");
  else if (speedes_time>(speedes_end_time-speedes_delta_t)
	   && speedes_direction>0)
    printf ("Warning: trying to go beyond simulation end time limit.\n");
  else
    speedes_time += speedes_direction * speedes_delta_t;
  
  XmScaleSetValue (sps_progress_slider, (int)(speedes_time*10));
 
  //finish();
  //T0=times(&Tbuf);
  gwindow->draw();
  //finish();
  //DT=times(&Tbuf) - T0;
  //printf (":>  length of sps_displist = %d, ", sps_displist->length());
  //printf ("length of sps_links_displist = %d.\n", sps_links_displist->length());
  //printf (":> gwindow->draw time = %f ticks \n   (or %f ms)\n\n", DT, DT*10); 


  if (sps_singlestep_mode)
  {
     sps_singlestep_mode = FALSE;
     return TRUE;  // i.e., quit for now;
  }
  else
    return FALSE; // i.e., continue;
}

/* ===== */

void 
reprocess_links ()
{
  C_QUEUE *links;
  C_LINK *link;
  C_SPEEDES_OBJECT *sensor_object, *track_object;
  int sensor_id, track_id, n_links;
  int i;
  double Xs[3], Vs[3];
  double Xt[3], Vt[3];
  double sloc[3], tloc[3];
  short rgb[3];
  short debug_link_type;

  //  printf ("LMASK is %x\n", LMASK); // debugging....

  links = speedes_state->get_links();
  n_links = links->get_length();

  if (spslinks != NULL)
  {
     delete spslinks;
     spslinks = NULL;
  }
  sps_links_displist->delete_objects();
  if (n_links > 0)
  {
     spslinks = new GR_Spslinks (n_links);
     sps_links_displist->add_object (spslinks);
  }

  link = (C_LINK *)links->get_top();
  for (i=0; i<n_links; i++)
  {
     sensor_id = link->get_sensor_id();
     track_id = link->get_track_id();
     sensor_object = speedes_state->get_object(sensor_id);
     track_object = speedes_state->get_object(track_id);
     if ((sensor_object != NULL) 
	 && (track_object != NULL)
	 && sensor_object->get_alive() 
	 && track_object->get_alive())
     { 
       if (sensor_object->get_icon () == track_object->get_icon ())
	 debug_link_type = 0x0001;
       else
	 debug_link_type = 0x0002;
       if (debug_link_type & LMASK)
       //if (link->get_type () & LMASK)
       {
	 sensor_object->get_pos_vel(speedes_time,Xs,Vs);
	 track_object->get_pos_vel(speedes_time,Xt,Vt);
	 sloc[0] = (float)Xs[1]/RE;
	 sloc[1] = (float)Xs[2]/RE;
	 sloc[2] = (float)Xs[0]/RE;
	 tloc[0] = (float)Xt[1]/RE;
	 tloc[1] = (float)Xt[2]/RE;
	 tloc[2] = (float)Xt[0]/RE;
	 rgb[0] = (short)link->get_red ();
	 rgb[1] = (short)link->get_green ();
	 rgb[2] = (short)link->get_blue ();
	 spslinks->add_link (i, sloc, tloc, rgb);
       }
     }
     link = (C_LINK *)link->get_link();
  }
  gwindow->draw();

}



/* ===== */

void
sps_linksCB (Widget, XtPointer client_data, XtPointer)
{
  int item_no = (int) client_data;

  switch (item_no)
    {
    case 0:
      printf ("Turning all links on.\n");
      LMASK = 0xffff;
      break;
    case 1:
      printf ("Turning all links off.\n");
      LMASK = 0x0000;
      break;
    default:
      break;
    }
  reprocess_links ();
}


void
sps_com_linksCB (Widget, XtPointer client_data, XtPointer)
{
  int item_no = (int) client_data;

  switch (item_no)
    {
    case 0:
      printf ("Turning COM links on.\n");
      LMASK = LMASK | 0x0001;
      break;
    case 1:
      printf ("Turning COM links off.\n");
      LMASK = LMASK & 0xfffe;
      break;
    default:
      break;
    }
  reprocess_links ();
}

void
sps_sen_linksCB (Widget, XtPointer client_data, XtPointer)
{
  int item_no = (int) client_data;

  switch (item_no)
    {
    case 0:
      printf ("Turning SEN links on.\n");
      LMASK = LMASK | 0x0002;
      break;
    case 1:
      printf ("Turning SEN links off.\n");
      LMASK = LMASK & 0xfffd;
      break;
    default:
      break;
    }
  reprocess_links ();
}
