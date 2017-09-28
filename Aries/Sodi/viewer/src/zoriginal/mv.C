/************************************************************
  mv.C is a model viewer attached to the rsd.C.
  
  -- 01/25/93: created by Tung; 
  -- 06/14/93: changed "Done" to "Quit";
  
************************************************************/

#include <math.h>
#include <sys/types.h>
#include <malloc.h>

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


GR_Shell *mv_shell;
GR_DispList *mv_displist;
GR_Window *mvwindow;
 
void model_viewerCB ();
void mv_doneCB ();
void mv_listCB (Widget, XtPointer, XmListCallbackStruct* call_data);
void mv_sliderCB (Widget, char client_data, 
                  XmScaleCallbackStruct* call_data);


Boolean first_modelviewer = TRUE;
   
GR_Model *allmodels;
int model_type = 1;   // some default type;
extern float get_scale (long type);
extern Widget createOneScale (Widget parent, char code, XmString, 
                       XtCallbackProc CB);

void mv_draw ();
int mv_LAT=20;
int mv_LON=50;
int mv_FOV=30;
float ViewDist=2.0;

Widget mv_lat_scale, mv_lon_scale, mv_fov_scale;

void
mv_init ()
{
   Widget mv_form;
   Widget mv_list; 
   Widget mv_frame;
   Widget mv_control, done_button;
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
   
   mv_shell = new GR_Shell;
   mv_shell->createWidget ("ModelViewer");
   mv_form = XmCreateForm (mv_shell->widget(), "MVForm", NULL, 0);

   mv_control = XtVaCreateManagedWidget ("MVControl",
                 xmRowColumnWidgetClass, mv_form,
                 XmNleftAttachment, XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_FORM,
                 XmNrightAttachment, XmATTACH_FORM,
                 XmNheight, 40,
                 XmNtopOffset, 10,
                 XmNleftOffset, 80,
                 XmNspacing, 25,
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
                 XmNwidth, 30,
                 XmNheight, 40,
                 XmNshadowThickness, 4,
                 NULL);
   XtAddCallback (done_button, XmNactivateCallback,
                    (XtCallbackProc)mv_doneCB, NULL);
 
   title = XmStringCreateSimple ("Latitude"); 
   mv_lat_scale = createOneScale (mv_control, 'A', title,
                                  (XtCallbackProc)mv_sliderCB);

   title = XmStringCreateSimple ("Longitude");
   mv_lon_scale = createOneScale (mv_control, 'O', title,
                                  (XtCallbackProc)mv_sliderCB);

   title = XmStringCreateSimple ("Field of View");
   mv_fov_scale = createOneScale (mv_control, 'F', title,
                                  (XtCallbackProc)mv_sliderCB);
   XmStringFree (title);

   XtManageChild (mv_form);
   XtManageChild (mv_list);
   mv_shell->realize ();
   mvwindow->draw ();
}

void 
model_viewerCB ()
{
   if (first_modelviewer)
   {
      first_modelviewer = FALSE;
      mv_init ();
   }
   else
   {
      XRaiseWindow (XtDisplay(mv_shell->widget()),
                    XtWindow(mv_shell->widget()) );
   }
}


void
mv_doneCB ()
{
  XtDestroyWidget (mv_shell->widget());
  mvwindow->set_awake (FALSE);
  first_modelviewer = TRUE;
}

void
mv_listCB (Widget, XtPointer, XmListCallbackStruct* call_data)
{
   char *text;

   XmStringGetLtoR (call_data->item,
                    XmSTRING_DEFAULT_CHARSET,
                    &text); 
   sscanf (text, "%d%*s", &model_type);
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
  float scale_factor;

  drawxyz ();
  if (allmodels)
  {
     allmodels->reset();
     allmodels->set_type (model_type);
     scale_factor = get_scale (model_type);
        scale_factor *= 25;  // for better single-model view 
     allmodels->scale (scale_factor, scale_factor, scale_factor);
     //allmodels->GR_DispObj::draw(); 
     allmodels->draw(); 
  }  
  else
     printf (" Oops, allmodels == NULL?\007\n");
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


