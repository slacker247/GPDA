/************************************************************
  tuner.C is a performance tuner, used to measure drawing
  routine performance.

  -- 3/12/93, Tung;
  -- 05/13/93: added a few more things...;

************************************************************/
#include <stdio.h>
#include <unistd.h>
#include <math.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/param.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/MessageB.h>
#include <Xm/List.h>

#include "GR_DispList.H"
#include "GR_Window.H"
#include "GR_DispObj.H"
#include "GR_Shell.H"
#include "GR_Model.H"
#include "GR_Sensor.H"
#include "blue.C"

float T0, DT;
struct tms Tbuf;

GR_Shell *tuner_shell;
GR_DispList *tuner_displist;
GR_Window *tuner_window;

void GR_initialize (int, char**);
void tuner_init ();
void tuner_doneCB ();
void tuner_blueearthCB ();
void tuner_sensorCB (Widget, int item_no, XmAnyCallbackStruct);
void tuner_listCB (Widget, XtPointer, XmListCallbackStruct* call_data);
void tuner_sliderCB (Widget, XtPointer client_data, 
                  XmScaleCallbackStruct* call_data);

Boolean first_tuner = TRUE;
   
GR_Model *allmodels;
int model_type = 1;   // some default type;
extern float get_scale (long type);
Widget createOneScale (Widget parent, char code, XmString, 
                       XtCallbackProc CB);


void tuner_draw ();
int tuner_LAT=20;
int tuner_LON=50;
int tuner_FOV=30;
float ViewDist=2.0;

Widget tuner_lat_scale, tuner_lon_scale, tuner_fov_scale;
GR_Blueearth *blueearth;
GR_Sensor *dome;
GR_Sensor *disk;
GR_Sensor *cone;

static
String fallback_resources [] =
{
  "*background: wheat",
  "*XmScale*foreground: black",
  "*XmScale*XmNhighlightOnEnter: True",
  "*XmMessageBox*foreground: white",
  "*XmMessageBox*background: steelblue",
  NULL
 };


/* --- */
void
main (int argc, char *argv[])
{
   GR_fallback_resources = fallback_resources;
   GR_startup (argc, argv);
   GR_initialize (argc, argv);
   XtAppMainLoop (GR_appcontext);
}


void
GR_initialize (int, char**)
{
   tuner_init ();
}


void
tuner_init ()
{
   Widget tuner_shell_liner, tuner_form;
   Widget tuner_list; 
   Widget tuner_frame;
   Widget tuner_control, done_button;
   Widget blueearth_button;
   //Widget sensor_button;
   Widget sensor_popup;
   XmString *xstr, title;
   int i;
   long count; 
   char line[80]; 
   float px, py, pz;

   tuner_shell = new GR_Shell;
   tuner_shell->createWidget ("Tuner");
   tuner_shell_liner = XtVaCreateManagedWidget
     ("TUNERLiner",
      xmFormWidgetClass, tuner_shell->widget(),
      XmNfractionBase, 100,
      NULL);

   tuner_control = XtVaCreateManagedWidget
     ("TUNERControl",
      xmRowColumnWidgetClass, tuner_shell_liner,
      XmNleftAttachment, XmATTACH_FORM,
      XmNrightAttachment, XmATTACH_FORM,
      XmNbottomAttachment, XmATTACH_FORM,
      XmNtopAttachment, XmATTACH_POSITION,
      XmNtopPosition, 85,
      XmNtopOffset, 10,
      XmNspacing, 20,
      XmNorientation, XmHORIZONTAL,
      XmNpacking, XmPACK_COLUMN,
      XmNnumColumns, 1,
      NULL);

   tuner_form = XtVaCreateManagedWidget
     ("TUNERForm",
      xmFormWidgetClass, tuner_shell_liner,
      XmNleftAttachment, XmATTACH_FORM,
      XmNrightAttachment, XmATTACH_FORM,
      XmNtopAttachment, XmATTACH_FORM,
      XmNbottomAttachment, XmATTACH_POSITION,
      XmNbottomPosition, 85,
      XmNfractionBase, 100,
      NULL);

   tuner_frame = XtVaCreateManagedWidget
     ("TUNERFrame",
      xmFrameWidgetClass, tuner_form,
      XmNshadowType, XmSHADOW_IN,
      XmNtopAttachment, XmATTACH_FORM,
      XmNleftAttachment, XmATTACH_FORM,
      XmNbottomAttachment, XmATTACH_FORM,
      XmNrightAttachment, XmATTACH_POSITION,
      XmNrightPosition, 50,
      NULL);

   tuner_window = new GR_Window ();
   tuner_window->doublebuffer ();
   tuner_window->rgbmode ();
   tuner_window->GR_Widget::createWidget ("Tuner_Window", tuner_frame);

   tuner_window->set_viewmode (GR_PERSPECTIVE);
   tuner_window->aspect (1);
   tuner_window->field_of_view ((float)tuner_FOV);
 
   px = ViewDist * cos(tuner_LAT*M_PI/180.0)*sin(tuner_LON*M_PI/180.0);
   py = ViewDist * sin(tuner_LAT*M_PI/180.0);
   pz = ViewDist * cos(tuner_LAT*M_PI/180.0)*cos(tuner_LON*M_PI/180.0); 
   tuner_window->view_position (px, py, pz);
   tuner_window->look_vector (0.0, 0.0, 0.0);
   tuner_window->set_drawfunction (tuner_draw);


   done_button = XtVaCreateManagedWidget ("Done",
                 xmPushButtonWidgetClass, tuner_control,
                 XmNwidth, 10,
                 XmNheight, 20,
                 XmNshadowThickness, 4,
                 NULL);
   XtAddCallback (done_button, XmNactivateCallback,
                    (XtCallbackProc)tuner_doneCB, NULL);

   blueearth_button = XtVaCreateManagedWidget
     ("BEarth",
      xmPushButtonWidgetClass, tuner_control,
      XmNwidth, 10,
      XmNheight, 20,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (blueearth_button, XmNactivateCallback,
		  (XtCallbackProc)tuner_blueearthCB, NULL);
/*
   sensor_button = XtVaCreateManagedWidget
     ("Sensor",
      xmPushButtonWidgetClass, tuner_control,
      XmNwidth, 10,
      XmNheight, 20,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (sensor_button, XmNactivateCallback,
		  (XtCallbackProc)tuner_sensorCB, NULL);
 */
   XmString sensor_label, dome_label, disk_label, cone_label;
   sensor_label = XmStringCreateSimple ("Sensor");
   dome_label = XmStringCreateSimple ("Dome");
   disk_label = XmStringCreateSimple ("Disk");
   cone_label = XmStringCreateSimple ("Cone");

   sensor_popup = XmVaCreateSimpleOptionMenu 
     (tuner_control,
      "sensor_popup",
      sensor_label, 'S', 0, (XtCallbackProc)tuner_sensorCB,
      XmVaPUSHBUTTON, dome_label, 'D', NULL, NULL,
      XmVaPUSHBUTTON, disk_label, 'K', NULL, NULL,
      XmVaPUSHBUTTON, cone_label, 'C', NULL, NULL,
      NULL);
   XmStringFree(sensor_label);
   XmStringFree(dome_label);
   XmStringFree(disk_label);
   XmStringFree(cone_label);
   XtManageChild (sensor_popup);


   title = XmStringCreateSimple ("Latitude"); 
   tuner_lat_scale = createOneScale (tuner_control, 'A', title,
                                  (XtCallbackProc)tuner_sliderCB);

   title = XmStringCreateSimple ("Longitude");
   tuner_lon_scale = createOneScale (tuner_control, 'O', title,
                                  (XtCallbackProc)tuner_sliderCB);

   title = XmStringCreateSimple ("Field of View");
   tuner_fov_scale = createOneScale (tuner_control, 'F', title,
                                  (XtCallbackProc)tuner_sliderCB);
   XmStringFree (title);


   allmodels = new GR_Model ("Models.desc");
   count = allmodels->get_p_files_read ();
   printf (" total number of models read is %d\n", count); 
   xstr = (XmString*)malloc(count*sizeof(XmString));
   for (i=0; i<count; i++)
   {
      sprintf (line, "%d  %s",
               allmodels->get_p_filetypes(i),
               allmodels->get_p_filenames(i));
      xstr[i] = XmStringCreate (line,
                XmSTRING_DEFAULT_CHARSET);
   }

   tuner_list = XmCreateScrolledList
     (tuner_form, "TUNERList", NULL, 0);
   XtVaSetValues (tuner_list,
                  XmNitems, xstr,
                  XmNitemCount, count,
                  XmNvisibleItemCount, 20,
                  NULL);
   XtAddCallback (tuner_list, XmNbrowseSelectionCallback,
                  (XtCallbackProc)tuner_listCB, NULL);
   XtVaSetValues (XtParent(tuner_list),
                  XmNtopAttachment, XmATTACH_FORM,
                  XmNrightAttachment, XmATTACH_FORM,
                  XmNbottomAttachment, XmATTACH_FORM,
                  XmNleftAttachment, XmATTACH_POSITION,
                  XmNleftPosition, 55,
                  NULL);
   XtManageChild (tuner_list);
   
   tuner_shell->realize ();
   
   printf ("getting a blue earth...\n");
   blueearth = new GR_Blueearth (1001, 6);
   printf ("getting 3 types of sensors: dome, disk and cone...\n");
   dome = new GR_Sensor (1002, 1002, 0.2); // a dome
   disk = new GR_Sensor (1003, 1003, 200, 240, 0.25, 0.02); // a disk
   cone = new GR_Sensor (1004, 1004, 15, 45); // a cone
   
   printf ("done with init...\n\n");
   
   //tuner_window->draw ();
}



void
tuner_doneCB ()
{
  exit (0);
}


void
tuner_blueearthCB ()
{
   model_type = 1001;
   tuner_window->draw ();
}

void
tuner_sensorCB (Widget, int item_no, XmAnyCallbackStruct)
{
   if (item_no == 0) // a dome:
     model_type = 1002;
   else if (item_no == 1) // a disk:
     model_type = 1003;
   else if (item_no == 2) // a cone:
     model_type = 1004;
   tuner_window->draw ();
}


void
tuner_listCB (Widget, XtPointer, XmListCallbackStruct* call_data)
{
   char *text;

   XmStringGetLtoR (call_data->item,
                    XmSTRING_DEFAULT_CHARSET,
                    &text); 
   sscanf (text, "%d%*s", &model_type);
   tuner_window->draw ();
}

void drawxyz ()
{
  register short i;

  pushattributes ();
  GR_color (255, 255, 255);
  move (0.0, 0.0, 0.0);
  draw (2.0, 0.0, 0.0);
  move (0.0, 0.0, 0.0);
  draw (0.0, 2.0, 0.0);
  move (0.0, 0.0, 0.0);
  draw (0.0, 0.0, 2.0);
  GR_color (0, 0, 0);
  for (i=1; i<=20; i++)
  {
     move (i*0.1, 0.0, 0.0);
     draw (i*0.1, 0.02, 0.0);
  }
  cmov (2.0, 0.0, 0.0);
  charstr("X");
  GR_color (255, 0, 0);
  for (i=1; i<=20; i++)
  {
     move (0.0, i*0.1, 0.0);
     draw (0.02, i*0.1, 0.0);
  }
  cmov (0.0, 2.0, 0.0);
  charstr("Y");
  GR_color (0, 0, 255);
  for (i=1; i<=20; i++)
  {
     move (0.0, 0.0, i*0.1);
     draw (0.0, 0.02, i*0.1);
  }
  cmov (0.0, 0.0, 2.0);
  charstr("Z");
  popattributes ();
}

void
tuner_draw ()
{
  float scale_factor;
  int i;
  int maxi;
  
  drawxyz ();

  finish();
  T0=times(&Tbuf);
  
  if (allmodels && model_type < 1000)
  {
     if (model_type == 33) // the earth
       maxi = 10;
     else
       maxi = 100;
       
     for (i=0; i<maxi; i++) // repeat 100 draws...
     {
	allmodels->reset();
	allmodels->set_type (model_type);
	scale_factor = get_scale (model_type);
        scale_factor *= 25;  // for better single-model view 
	allmodels->scale (scale_factor, scale_factor, scale_factor);
	allmodels->draw();
     }
     finish();
     DT=times(&Tbuf) - T0;
     printf ("==> Average drawing time for type %d is %f ms\n",
	     model_type, DT*10/maxi);
  }  
  else if (model_type == 1001) // draw a blue earth:
  {
     for (i=0; i<100; i++)
     {
	blueearth->reset();
	blueearth->scale (0.25, 0.25, 0.25);	 
	blueearth->draw();
     }
     finish();
     DT=times(&Tbuf) - T0;
     printf ("==> Average drawing time for blueearth type is %f ms\n",
	     DT/10);     
  }
  else if (model_type == 1002) // draw a dome:
  {
     for (i=0; i<100; i++)
     {
	dome->reset();
	dome->draw();
     }
     finish();
     DT=times(&Tbuf) - T0;
     printf ("==> Average drawing time for a dome sensor is %f ms\n",
	     DT/10);
  }
  else if (model_type == 1003) // draw a disk:
  {
     for (i=0; i<100; i++)
     {
	disk->reset();
	disk->draw();
     }
     finish();
     DT=times(&Tbuf) - T0;
     printf ("==> Average drawing time for a disk sensor is %f ms\n",
	     DT/10);
  }
  else if (model_type == 1004) // draw a dome:
  {
     for (i=0; i<100; i++)
     {
	cone->reset();
	cone->draw();
     }
     finish();
     DT=times(&Tbuf) - T0;
     printf ("==> Average drawing time for a cone sensor is %f ms\n",
	     DT/10);
  }
  else
    printf (" Oops, allmodels == NULL or invalid model_type?\007\n");
}

void 
tuner_sliderCB (Widget, XtPointer client_data, XmScaleCallbackStruct* call_data)
{
   char code = (char)client_data;
   float px, py, pz;

   if (code == 'A' || code == 'O')
   {
     if (code == 'A')
         tuner_LAT = call_data->value;
     else
         tuner_LON = call_data->value;

     px = ViewDist * cos(tuner_LAT*M_PI/180.0)*sin(tuner_LON*M_PI/180.0);
     py = ViewDist * sin(tuner_LAT*M_PI/180.0);
     pz = ViewDist * cos(tuner_LAT*M_PI/180.0)*cos(tuner_LON*M_PI/180.0);
     tuner_window->view_position (px, py, pz);
     tuner_window->look_vector (0.0, 0.0, 0.0);
   }
   else if (code == 'F')
   {
     tuner_FOV = call_data->value;
     tuner_window->field_of_view((float)tuner_FOV);
   }
   else
   {
     XtWarning ("Unknown code in tuner_sliderCB function.");
     return;
   }
   
   tuner_window->request_draw (250);

}


Widget
createOneScale(Widget parent, char code, XmString title, XtCallbackProc CB)
{
   Widget scale;
   Arg args[10];
   register int n;
   char name[2];
   
   name[0] = code;
   name[1] = '\0';
   
   n = 0;
   XtSetArg (args[n], XmNorientation, XmHORIZONTAL); n++;
   XtSetArg (args[n], XmNshowValue, True); n++;
   XtSetArg (args[n], XmNtitleString, title); n++;
   XtSetArg (args[n], XmNscaleMultiple, 5); n++;
   scale = XmCreateScale (parent, name, args, n);
   
   XtAddCallback (scale, XmNvalueChangedCallback, CB, (XtPointer)code);
   XtAddCallback (scale, XmNdragCallback, CB, (XtPointer)code);
   
   XtManageChild (scale);
   
   return (scale);
}
