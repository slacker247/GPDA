/************************************************************
  Old Simulation (Sim91, Sim90,...) interface
  -- 02/11/93 by Y. Tung
************************************************************/

#include "sim_interface.H"

XtWorkProcId oldId = NULL;
Boolean oldWP (XtPointer);

GR_Shell *old_panel;
GR_DispList *old_displist;
extern GR_Window *gwindow;

void oldCB ();
void old_init ();
void old_playCB ();
void old_pauseCB ();
void old_linksCB ();
void old_resetCB ();
void old_doneCB ();

Widget old_form;
Widget old_progress_slider;
Widget old_simtime_text;

float old_simtime=0.0;
void get_old_aircraft_message (void*, char* buf, int size);



void
oldCB ()
{
   static Boolean first_oldsim = TRUE;
   void *handle;
   
   if (first_oldsim || !old_panel)
   {
      printf ("Start old simulation (SIM91 or older) interface panel.\n");
      if (first_oldsim)
      {
	 old_displist = new GR_DispList;
	 gwindow->addDispList (old_displist, "old_displist");

	 handle = msg_register_resrc ("RSDAIR", 0);
	 msg_set_message (handle,"get_aircraft_message",
			  (MessageFunc)get_old_aircraft_message);
      }
      old_init ();
      first_oldsim = FALSE;
   }
   else
   {
      XRaiseWindow (XtDisplay(old_panel->widget()),
		    XtWindow(old_panel->widget())
		    );   
   }
}

void
old_init ()
{
   Widget button_area;
   Widget play_b, pause_b, links_b, reset_b, done_b;
   Widget simtime_label;
   XmString title;
   char simtime_string[10];
   
   old_panel = new GR_Shell;
   old_panel->createWidget ("OldSim_Panel");
   old_form = XmCreateForm (old_panel->widget(),
			    "OldSim_Form",
			    NULL,
			    0);
   XtManageChild (old_form);

   title = XmStringCreateSimple ("Sim Time: ");
   simtime_label = XtVaCreateManagedWidget
     ("OldSimtimeLabel",
      xmLabelGadgetClass, old_form,
      XmNlabelString, title,
      XmNheight, 40,
      XmNwidth, 80,
      XmNtopAttachment, XmATTACH_FORM,
      XmNleftAttachment, XmATTACH_FORM,
      XmNtopOffset, 20,
      XmNleftOffset, 20,
      NULL);
   XmStringFree (title);

   old_simtime_text = XtVaCreateManagedWidget
     ("OldSimTimeText",
      xmTextFieldWidgetClass, old_form,
      XmNtraversalOn, True,
      XmNheight, 40,
      //XmNwidth, 60,
      XmNtopAttachment, XmATTACH_FORM,
      XmNleftAttachment, XmATTACH_WIDGET,
      XmNleftWidget, simtime_label,
      XmNrightAttachment, XmATTACH_FORM,
      XmNtopOffset, 20,
      XmNleftOffset, 20,
      XmNrightOffset, 30,
      XmNshadowThickness, 3,
      NULL);
   sprintf (simtime_string, "%.1f", old_simtime);
   XtVaSetValues (old_simtime_text,
		  XmNvalue, simtime_string,
		  NULL);

   /*
     old_progress_slider = XtVaCreateManagedWidget
     ("OldSim_P_Slider",
      xmScaleWidgetClass, old_form,
      XmNorientation, XmHORIZONTAL,
      XmNshowValue, True,
      XmNheight, 60,
      XmNtopAttachment, XmATTACH_WIDGET,
      XmNtopWidget, simtime_label,
      XmNleftAttachment, XmATTACH_FORM,
      XmNrightAttachment, XmATTACH_FORM,
      NULL);
      */
   
   button_area = XtVaCreateManagedWidget
      ("ButtonArea",
       xmRowColumnWidgetClass, old_form,
       XmNleftAttachment, XmATTACH_FORM,
       XmNrightAttachment, XmATTACH_FORM,
       XmNtopAttachment, XmATTACH_WIDGET,
       XmNtopWidget, simtime_label,
       XmNorientation, XmHORIZONTAL,
       XmNtopOffset, 20,
       XmNleftOffset, 20,
       XmNspacing, 14,
       XmNpacking, XmPACK_COLUMN,
       XmNnumColumns, 1,
       NULL);

   play_b = XtVaCreateManagedWidget
     ("Play",
      xmPushButtonWidgetClass, button_area,
      XmNwidth, 36,
      XmNheight, 36,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (play_b, XmNactivateCallback,
		  (XtCallbackProc)old_playCB, (XtPointer)0);
   
   pause_b = XtVaCreateManagedWidget
     ("Pause",
      xmPushButtonWidgetClass, button_area,
      XmNwidth, 36,
      XmNheight, 36,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (pause_b, XmNactivateCallback,
		  (XtCallbackProc)old_pauseCB, (XtPointer)0);

   links_b = XtVaCreateManagedWidget
     ("Links",
      xmPushButtonWidgetClass, button_area,
      XmNwidth, 36,
      XmNheight, 36,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (links_b, XmNactivateCallback,
		  (XtCallbackProc)old_linksCB, (XtPointer)0);

   reset_b = XtVaCreateManagedWidget
     ("Reset",
      xmPushButtonWidgetClass, button_area,
      XmNwidth, 36,
      XmNheight, 36,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (reset_b, XmNactivateCallback,
		  (XtCallbackProc)old_resetCB, (XtPointer)0);
   
   done_b = XtVaCreateManagedWidget
     ("Done",
      xmPushButtonWidgetClass, button_area,
      XmNwidth, 36,
      XmNheight, 36,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (done_b, XmNactivateCallback,
		  (XtCallbackProc)old_doneCB, (XtPointer)0);
   
   old_panel->realize ();
}



void
old_playCB ()
{
   printf ("... In old_playCB...\n");
   if (oldId)
      XtRemoveWorkProc (oldId);
   oldId = XtAppAddWorkProc (GR_appcontext, (XtWorkProc)oldWP, 0);
   if (old_displist)
     gwindow->addDispList (old_displist, "old_displist");
      
}

void
old_pauseCB ()
{
   printf ("... This will pause...\n");
   if (oldId)
   {
      XtRemoveWorkProc (oldId);
      oldId = NULL;
   }
}

void
old_linksCB ()
{
   printf ("... This will toggle links drawing...|n");
}

void
old_resetCB ()
{
   char time_string[10];

   printf ("... This should reset the OldSim interface...\n");
   if (oldId)
   {
      XtRemoveWorkProc (oldId);
      oldId = NULL;
   }
   
   old_simtime = 0;
   sprintf (time_string, "%.1f", old_simtime);
   XtVaSetValues (old_simtime_text,
                  XmNvalue, time_string,
                  NULL);

   if (old_displist)
   {
      old_displist->delete_objects ();
      gwindow->draw ();
   }      

}

void
old_doneCB ()
{
   old_resetCB ();
   XtDestroyWidget (old_panel->widget());
   old_panel = NULL;  // needed for consequent new panel;
}



Boolean
oldWP (XtPointer)
{
   static int no_msg_cycle = 0;
   int msg_code;

   msg_code = msg_read_message();

   if (msg_code < 0)
     perror ("Warning: msg_read_message");
   else if (msg_code > 0) // no msg, or timed out:
   {
      no_msg_cycle++;
      if ((no_msg_cycle % 10000) == 0)
      {
	 printf ("Reminder: checked 10K times and got no messages.\n");
	 no_msg_cycle = 0;
	 gwindow->request_draw(1000);
      }
   }
   else  // got one message:
     no_msg_cycle = 0;

   return FALSE; // i.e., continue;
}


/* -------- MSG interface ---------- */

struct old_update
{
   long time;
   long cmnd;
   long type;
   long obj_id;
   long px;
   long py;
   long pz;
   long rx;
   long ry;
   long rz;
   long h;
};



void
get_old_aircraft_message (void*, char* buf, int size)
{
   int i;
   int num_messages;
   old_update *update;
   float time;
   int cmnd, type, obj_id;
   float px, py, pz, rx, ry, rz, h;
   GR_Model *airobj;
   float scale_factor;
   Boolean new_airobj;
   char time_string[10];
   

   update = (old_update*)buf;
   num_messages = size/sizeof(old_update);
   
   printf ("   get_old_aircraft_message's num_messages = %d\n", num_messages);

   for (i=0; i<num_messages; i++, update++)
   {
      time = (float)(update->time)/10000;
      cmnd = (int)(update->cmnd);
      type = (int)(update->type);
      obj_id = (int)(update->obj_id);
      px = (float)(update->px)/10000;
      py = (float)(update->py)/10000;
      pz = (float)(update->pz)/10000;
      rx = (float)(update->rx)/10000;
      ry = (float)(update->ry)/10000;
      rz = (float)(update->rz)/10000;
      h =  (float)(update->h)/10000;
   
      printf (" Msg %d -- time=%f, cmnd is %d, type=%d, obj_id=%d;\n",
	      i, time, cmnd, type, obj_id);
      printf ("   px=%f, py=%f, pz=%f;\n", px, py, pz);
      printf ("   rx=%f, ry=%f, rz=%f, h=%f;\n", rx, ry, rz, h);
	   
      if (time > old_simtime)
      {
	 old_simtime = time;
	 sprintf (time_string, "%.1f", old_simtime);
	 XtVaSetValues (old_simtime_text,
			XmNvalue, time_string,
			NULL);
      }
      else if (time < old_simtime)
      {
	 perror ("get_old_aircraft_message: received a past time?");
	 old_simtime = time;
	 sprintf (time_string, "%.1f", old_simtime);
	 XtVaSetValues (old_simtime_text,
			XmNvalue, time_string,
			NULL);
      }


      if (cmnd==1 || cmnd==2) // CMND_DEFINE or CMND_ADVANCE:
      {
	 airobj = (GR_Model*)old_displist->retrieve_object(obj_id);
	 new_airobj = !airobj;

	 if (new_airobj)
	    airobj = new GR_Model (obj_id, type);
	 else
	    airobj->reset ();
	 
	 scale_factor = get_scale (type);
	 //scale_factor *= 3;
	 airobj->scale (scale_factor, scale_factor, scale_factor);
	 if (type==69 || type==104 || type==55 || type==128 || type==51
	     || type==70 || type==63 || type==20)
	 {
	    airobj->rotate_z (180);
	    airobj->rotate_x (90);
            //airobj->rotate_x (90);
            //airobj->rotate_z (90);
            //airobj->rotate_y (180);
	 }
	 else if (type==96 || type==97 || type==98)
	 {
	    airobj->rotate_y (180);
	    airobj->rotate_x (90);
	 }
	 else
	 {
	    if (type==68)
	      airobj->translate(0, 0.02, 0); // a patch for AWACS position;
	    airobj->rotate_y (90);
	    airobj->rotate_x (180);
            //airobj->rotate_x (270);
            //airobj->rotate_z (90);
	 }
	
         airobj->rotate_y (rx);
         airobj->rotate_x (ry);
         airobj->rotate_y (rz);

         //airobj->rotate_y (rz);
	 //airobj->rotate_x (ry);
	 //airobj->rotate_z (rx);

	 airobj->translate (py, pz, px);

	 if (new_airobj)
	   old_displist->add_object (airobj);
      }
      else if (cmnd==6) // CMND_UNDEFINE:
	 old_displist->delete_object (obj_id);

   }

   gwindow->draw ();
}
    
