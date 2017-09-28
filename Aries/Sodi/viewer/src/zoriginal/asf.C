/***************************************************************
  asf_.C: advanced framework -- fixed time bucket interface;
  
  -- 10/12/92 created by Y. Tung;
  -- 03/22/93 revised to use a window;
  -- 04/26/93: use a separate displist for links;
  -- 04/27/93: clean up the code;
  -- 05/07/93: defined missile interface (get_missile_message,
     etc) and used GR_AirObj and GR_Trail in both this and
     airsim interface; the airsim interface with trails has
     been tested;
  -- 06/25/93: added statistics panel;
  -- 06/28/93: let asf_CB include asf_startCB (auto_start at beginning);
  -- 07/23/93: make links & trails toggle buttons show on/off choice;
   
***************************************************************/
#include "asf.H"
  
void
asf_msg_setup ()
{
   void * handle;
   
   handle = msg_register_resrc("RSDAIR", 0);
   msg_set_message (handle,"get_aircraft_message",
		    (MessageFunc)get_aircraft_message);
   handle = msg_register_resrc("RSDSENSOR", 0);
   msg_set_message (handle,"get_sensor_message",
		    (MessageFunc)get_sensor_message);
   handle = msg_register_resrc("RSDTRACK", 0);
   msg_set_message (handle,"get_track_message",
		    (MessageFunc)get_track_message);
   handle = msg_register_resrc("RSDMISSILE", 0);
   msg_set_message (handle,"get_missile_message",
   (MessageFunc)get_missile_message);
}

void
asf_msg_unsetup ()
{
   msg_unregister_resrc ("RSDAIR");
   msg_unregister_resrc ("RSDSENSOR");
   msg_unregister_resrc ("RSDTRACK");
   msg_unregister_resrc ("RSDMISSILE");
}

void
asf_CB ()
{
   static Boolean first_asf = TRUE;

   if (first_asf || !asf_panel)
   {
      printf ("Start ASF fixed time step approach.\n");
      if (first_asf)
      {
	 first_asf = FALSE;
	 asf_displist = new GR_DispList;
	 gwindow->addDispList (asf_displist, "asf_displist");
	 asf_links_displist = new GR_DispList;
	 gwindow->addDispList (asf_links_displist, "asf_links_displist");
	 asf_links = new GR_Links (asf_displist); // will search asf_displist;
	 asf_links_displist->add_object (asf_links);
	 asf_trails_displist = new GR_DispList;
	 gwindow->addDispList (asf_trails_displist, "asf_trails_displist");

	 asf_msg_setup ();
	 asf_simtime = 0.0;
	 asf_prevtime = 0.0;
      }
      asf_init ();  
      draw_asf_links = TRUE;
      draw_asf_trails = TRUE;
      asf_startCB ();
   }
   else
   {
      XRaiseWindow (XtDisplay (asf_panel->widget ()),
		    XtWindow (asf_panel->widget ()));
   }
}

Widget asf_links_b, asf_trails_b;

void
asf_init ()
{
   Widget text_rowcol, simtime_label, group_label;
   Widget button_area;
   //Widget cpanel_b;
   //Widget links_b, trails_b;
   Widget statistics_b;
   Widget start_b, pause_b, reset_b, quit_b;
   XmString title;
   char simtime_string[20], *group_string;
   
   asf_panel = new GR_Shell;
   asf_panel->createWidget ("Asf_Panel");
   asf_form = XtVaCreateManagedWidget
     ("Asf_Form",
      xmFormWidgetClass, asf_panel->widget (),
      NULL);
   text_rowcol = XtVaCreateWidget  // not managed yet...
     ("Text_Rowcol",
      xmRowColumnWidgetClass, asf_form,
      XmNleftAttachment, XmATTACH_FORM,
      XmNrightAttachment, XmATTACH_FORM,
      XmNrightOffset, 20,
      XmNtopAttachment, XmATTACH_POSITION,
      XmNtopPosition, 10,
      XmNbottomAttachment, XmATTACH_POSITION,
      XmNbottomPosition, 55,      
      XmNpacking, XmPACK_COLUMN,
      XmNnumColumns, 2,
      XmNspacing, 8,
      XmNisAligned, True,
      XmNentryAlignment, XmALIGNMENT_END,
      NULL);
   title = XmStringCreateSimple ("Sim Time: ");
   simtime_label = XtVaCreateManagedWidget
     ("Simtime_Label",
      xmLabelGadgetClass, text_rowcol,
      XmNlabelString, title,
      XmNheight, 30,
      XmNwidth, 25,
      NULL);
   title = XmStringCreateSimple ("Group: ");
   group_label = XtVaCreateManagedWidget
     ("Group_Label",
      xmLabelGadgetClass, text_rowcol,
      XmNlabelString, title,
      XmNheight, 30,
      XmNwidth, 25,
      NULL);
   XmStringFree (title);
   
   asf_simtime_text = XtVaCreateManagedWidget
     ("Simtime_Text",
      xmTextFieldWidgetClass, text_rowcol,
      XmNeditable, False,
      NULL);
   asf_group_text = XtVaCreateManagedWidget
     ("Group_Label",
      xmTextFieldWidgetClass, text_rowcol,
      NULL);
   XtAddCallback
     (asf_group_text,
      XmNactivateCallback,
      (XtCallbackProc)asf_textCB,
      (XtPointer)0);   // use 0 to mean group name (only one);
   XtManageChild (text_rowcol);

   sprintf (simtime_string, "%.1f", asf_simtime);
   XtVaSetValues (asf_simtime_text,
                  XmNvalue, simtime_string,
                  NULL);
   //XmTextSetString (asf_simtime_text, simtime_string);
   //?? this XmTextSetString doesn't work ??

   //group_string = getenv("MESSAGERGROUP"); 
   group_string = msg_get_groupname ();
   if (group_string == NULL)
     printf ("Note: group name is not set (NULL).\n");
   else if (strcmp (group_string, "") == 0)
     printf ("Note: group name is not set (empty string).\n");
   else
   {
      printf ("The current group name is %s\n", group_string);
      XtVaSetValues (asf_group_text,
		     XmNvalue, group_string,
		     NULL);
   } 
   
   button_area = XtVaCreateManagedWidget
     ("Button_Area",
      xmRowColumnWidgetClass, asf_form,
      XmNleftAttachment, XmATTACH_FORM,
      XmNrightAttachment, XmATTACH_FORM,
      XmNtopAttachment, XmATTACH_POSITION,
      XmNtopPosition, 60,
      XmNbottomAttachment, XmATTACH_FORM,
      XmNorientation, XmHORIZONTAL,
      XmNleftOffset, 28,
      XmNspacing, 14,
      XmNpacking, XmPACK_COLUMN,
      XmNnumColumns, 2,
      NULL);

   asf_links_b = XtVaCreateManagedWidget
     ("Links Off",
      xmPushButtonWidgetClass, button_area,
      XmNwidth, 36,
      XmNheight, 36,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (asf_links_b, XmNactivateCallback,
		  (XtCallbackProc)asf_linksCB, (XtPointer)0);

   asf_trails_b = XtVaCreateManagedWidget
     ("Trails Off",
      xmPushButtonWidgetClass, button_area,
      XmNwidth, 36,
      XmNheight, 36,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (asf_trails_b, XmNactivateCallback,
		  (XtCallbackProc)asf_trailsCB, (XtPointer)0);
   // may take the C. Panel button out.....
   /*
   cpanel_b = XtVaCreateManagedWidget
     ("C. Panel",
      xmPushButtonWidgetClass, button_area,
      XmNwidth, 36,
      XmNheight, 36,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (cpanel_b, XmNactivateCallback,
		  (XtCallbackProc)asf_cpanelCB, (XtPointer)0);
   */
   XtVaCreateManagedWidget    //dummy button just to fill one position;
     (" ",
      xmPushButtonWidgetClass, button_area,
      XmNwidth, 36,
      XmNheight, 36,
      XmNshadowThickness, 4,
      NULL);

   statistics_b = XtVaCreateManagedWidget    
     (
      "Statis",
      xmPushButtonWidgetClass, button_area,
      XmNwidth, 36,
      XmNheight, 36,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (statistics_b, XmNactivateCallback,
		  (XtCallbackProc)asf_statisticsCB, (XtPointer)0);
   
   start_b = XtVaCreateManagedWidget
     ("Cont.",
      xmPushButtonWidgetClass, button_area,
      XmNwidth, 36,
      XmNheight, 36,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (start_b, XmNactivateCallback,
		  (XtCallbackProc)asf_startCB, (XtPointer)0);
   pause_b = XtVaCreateManagedWidget
     ("Pause",
      xmPushButtonWidgetClass, button_area,
      XmNwidth, 36,
      XmNheight, 36,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (pause_b, XmNactivateCallback,
		  (XtCallbackProc)asf_pauseCB, (XtPointer)0);
   reset_b = XtVaCreateManagedWidget
     ("Reset",
      xmPushButtonWidgetClass, button_area,
      XmNwidth, 36,
      XmNheight, 36,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (reset_b, XmNactivateCallback,
		  (XtCallbackProc)asf_resetCB, (XtPointer)0);
   quit_b = XtVaCreateManagedWidget
     ("Quit",
      xmPushButtonWidgetClass, button_area,
      XmNwidth, 36,
      XmNheight, 36,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (quit_b, XmNactivateCallback,
		  (XtCallbackProc)asf_quitCB, (XtPointer)0);

   asf_panel->realize ();
}


void
asf_warnCB ()
{
   char *FW;
   char *MA;
   char cmnd[80];
   
   if ((FW=getenv("FRAMEWORKHOME")) == NULL)
     FW = "/home1/simserv/esd/sim92";
   if ((MA=getenv("MACHINE")) == NULL)
     MA = "IRIS4D";
   sprintf (cmnd, "%s/bin/%s/Sim-Control&", FW, MA);
   printf ("OK, will load and do %s ...\n", cmnd);
   system (cmnd);
}

void
asf_cpanelCB ()
{
   Widget warndialog;
   XmString warnmsg, doit;
   
   warndialog =
     XmCreateWarningDialog (asf_panel->widget(), "warning", NULL, 0);
   XtSetSensitive (XmMessageBoxGetChild(warndialog,XmDIALOG_HELP_BUTTON),
		   False);
   warnmsg = XmStringCreateSimple ("Create an ASF Control Panel?");
   doit = XmStringCreateSimple ("Do it!");
   XtVaSetValues (warndialog,
		  XmNmessageString, warnmsg,
		  XmNokLabelString, doit,
		  NULL);
   XtAddCallback (warndialog,
		  XmNokCallback,
		  (XtCallbackProc)asf_warnCB,
		  NULL);
   XmStringFree (warnmsg);
   XmStringFree (doit);
   XtManageChild (warndialog);
   XtPopup (XtParent(warndialog), XtGrabNone);
}

void
asf_startCB ()
{
   if (asf_Id)
     XtRemoveWorkProc (asf_Id);
   printf (" start or resume external message handling ...\n");
   asf_Id = XtAppAddWorkProc (GR_appcontext, (XtWorkProc)asf_WP, 0);
   if (!asf_displist)
     asf_displist = new GR_DispList;

   if (!asf_links_displist)
   {
     asf_links_displist = new GR_DispList;
     asf_links = new GR_Links (asf_displist);
     asf_links_displist->add_object (asf_links);
   }

   if (!asf_trails_displist)
     asf_trails_displist = new GR_DispList;

   gwindow->addDispList (asf_displist, "asf_displist");
   gwindow->addDispList (asf_links_displist, "asf_links_displist");
   gwindow->addDispList (asf_trails_displist, "asf_trails_displist");
}


void
asf_pauseCB ()
{
   if (asf_Id)
   {
      XtRemoveWorkProc (asf_Id);
      asf_Id = NULL;
   }
}


void
asf_linksCB ()
{
   XmString links_on, links_off;

   links_on = XmStringCreateSimple ("Links On");
   links_off = XmStringCreateSimple ("Links Off");
 
   draw_asf_links = !draw_asf_links;
   if (draw_asf_links)
   {
      printf ("will draw links ....\n");
      if (asf_links_displist)
	gwindow->addDispList (asf_links_displist, "asf_links_displist");
      XtVaSetValues (asf_links_b,
                     XmNlabelString, links_off,
                     NULL);
   }
   else
   {
      printf ("will not draw links ....\n");
      gwindow->remDispList (asf_links_displist, "asf_links_displist");
      XtVaSetValues (asf_links_b,
                     XmNlabelString, links_on,
                     NULL); 
   }
   XmStringFree (links_on);
   XmStringFree (links_off);
   gwindow->draw ();
}

/*
void
asf_trailsCB ()
{
   draw_asf_trails = !draw_asf_trails;
   if (draw_asf_trails)
   {
      printf ("will draw trails ....\n");
      if (asf_trails_displist)
	gwindow->addDispList (asf_trails_displist, "asf_trails_displist");
   }
   else
   {
      printf ("will not draw trails ....\n");
      gwindow->remDispList (asf_trails_displist, "asf_trails_displist");
   }
   gwindow->draw ();
}
*/


void
asf_trailsCB ()
{
   GR_DispObj *obj;
   XmString trails_on, trails_off;

   trails_on = XmStringCreateSimple ("Trails On");
   trails_off = XmStringCreateSimple ("Trails Off");

   draw_asf_trails = !draw_asf_trails;
   if (draw_asf_trails)
   {
      printf ("will draw trails ....\n");
      if (asf_trails_displist)
      {
	 for (obj=asf_trails_displist->enumerate(1);
	      obj;
	      obj=asf_trails_displist->enumerate(0))
	 {
	    obj->set_visible_flag (1);
	 }
	gwindow->addDispList (asf_trails_displist, "asf_trails_displist");
      }
      XtVaSetValues (asf_trails_b,
                     XmNlabelString, trails_off,
                     NULL);
   }
   else
   {
      printf ("will not draw trails ....\n");
      if (asf_trails_displist)
      {
	 for (obj=asf_trails_displist->enumerate(1);
	      obj;
	      obj=asf_trails_displist->enumerate(0))
	 {
	    obj->set_visible_flag (0);
	 }
	 gwindow->addDispList (asf_trails_displist, "asf_trails_displist");
      }
      XtVaSetValues (asf_trails_b,
                     XmNlabelString, trails_on,
                     NULL);
   }
   XmStringFree (trails_on);
   XmStringFree (trails_off);
   gwindow->draw ();
}


void
asf_resetCB ()
{
   if (asf_Id)
   {
      XtRemoveWorkProc (asf_Id);
      asf_Id = NULL;
   }
   if (asf_displist)
   {
      //asf_displist->delete_objects ();
      gwindow->remDispList (asf_displist, "asf_displist");
      asf_displist = NULL;
   }
   if (asf_links)
   {
      delete asf_links;
      gwindow->remDispList (asf_links_displist, "asf_links_displist");
      asf_links_displist = NULL;
   }
   if (asf_trails_displist)
   {
      //delete asf_trails_displist;
      gwindow->remDispList (asf_trails_displist, "asf_trails_displist");
      asf_trails_displist = NULL;
   }
   asf_prevtime = 0.0;
   asf_simtime = 0.0;
   draw_asf_links = TRUE;
   draw_asf_trails = TRUE;
   gwindow->draw ();
}

void
asf_quitCB ()
{
   asf_resetCB ();
   if (asf_statistics_panel)
   {
      XtDestroyWidget (asf_statistics_panel->widget());
      asf_statistics_panel = NULL;
   }
   XtDestroyWidget (asf_panel->widget());
   asf_panel = NULL;
}

void
asf_textCB (Widget text_w, int code)
{
   static Boolean first_change_group = TRUE;
   char *text_str = XmTextFieldGetString (text_w);
   
   switch (code)
   {
    case 0:  // the only case, i.e., the group name:
      if (first_change_group)
      {
	 first_change_group = FALSE;
      }
      else
      {
	 printf ("  Changing group name...  ");
	 asf_msg_unsetup ();
      }
      msg_set_groupname (text_str);
      asf_msg_setup ();
      printf (" Now the group name has been set to %s\n",
	      (char*)msg_get_groupname ());
      XtFree (text_str);
      break;
      
    default:
      printf ("Unknown code %d when called asf_textCB\n", code);
      break;
   }
}
     

Boolean
asf_WP (XtPointer)
{
   static int no_msg_cycle = 0;
   int rtn_code;
   long timeout = 0; //timeout in second(s);
   char simtime_string[10];
   
   //rtn_code = msg_read_message (timeout); // not being supported???
   rtn_code = msg_read_message ();
   
   if (asf_simtime > (asf_prevtime + 5))
   {
      asf_prevtime = asf_simtime;
      sprintf (simtime_string, "%.1f", asf_simtime);
      XtVaSetValues (asf_simtime_text,
		     XmNvalue, simtime_string,
		     NULL);
      if (asf_statistics_panel)
	asf_statistics_update ();
      gwindow->request_draw (500);
  }
   
   if (rtn_code < 0) // error
     printf(" Warning: Message read error occurs.\007\n");
   else if (rtn_code > 0) // no message or timeout
   {
      no_msg_cycle++;
      if ((no_msg_cycle % 10000) == 0)
      {
	 printf ("Reminder: checked 10K times and got no messages.\n");
	 no_msg_cycle = 0;
	 gwindow->request_draw (1000);
      }
   }
   else // received one message
   {
      no_msg_cycle = 0;
   }
   return FALSE; // i.e., continue;
}


/* ------ air ------ */

void
get_aircraft_message (void*, char* buf, int size )
{
   int num_messages;
   Ext_Airmsg *airmsg;
   int i, j;
   float time;
   int cmnd;
   long type, obj_id, sid;
   float lat, lon, alt, heading;
   float x, y, z;
   int total_sobj;
   GR_AirObj *airobj;
   GR_Sensor *sobj;
   int heading_angle, lat_angle, lon_angle;
   float scale_factor;
   
   airmsg = (Ext_Airmsg*)buf ;
   num_messages = size/sizeof(Ext_Airmsg);

   for (i=0; i<num_messages; i++, airmsg++)
   {
      time = (float)(airmsg->time)/10000;
      asf_simtime = time;
      cmnd = (int)(airmsg->cmnd);
      type = (int)(airmsg->type);
      obj_id = (int)(airmsg->obj_id);
      lat = (float)(airmsg->lat)/10000;
      lon = (float)(airmsg->lon)/10000;
      alt = (float)(airmsg->alt)/10000/RE;
      if (alt < 15/RE)
	alt = 15/RE;
      heading = (float)(airmsg->heading)/10000;

      switch (cmnd)
      {
       case 1:              // create, or use CMND_DEFINE:
         printf ("... creating air obj: id %d, type %d, time tag: %f.\n",
		 (obj_id & 0x000fffff),
		 type,
		 time);
         airobj = new GR_AirObj (obj_id, type);
         break;
       case 2:              // update, or use CMND_ADVANCE:
	 airobj=(GR_AirObj*)asf_displist->retrieve_object (obj_id);
	 if (airobj)
	 {
            airobj->reset();
         }
         else
         {
	    cmnd = 1;
            printf ("Warn: update non-existing air object: id %d, type %d\n;",
                    (obj_id & 0x000fffff),
		    type);
            airobj = new GR_AirObj (obj_id, type);
         }
         break;
       case 6:                // delete, or use CMND_UNDEFINE:
         airobj=(GR_AirObj*)asf_displist->retrieve_delete_object (obj_id);
	 if (airobj)
	 {
            printf (" One air object %d deleted\n",
		    (obj_id & 0x000fffff)
		    );
	    airobj->delete_trail (asf_trails_displist);
	    for (j=0; j<airobj->get_total_sensors(); j++)
	    {
	       sobj = airobj->get_sobj(j);
	       asf_displist->delete_object(sobj);
	       sid = sobj->get_id();
	       printf ("  one mobile sensor %d deleted.\n",
		       (sid & 0x000fffff)
		       );
	    }
	 }
	 else
	   printf ("Cannot find air object %d that you want to delete.\n",
		   (obj_id & 0x000fffff));
	 break;
       default:
         break;
      }

      if (cmnd == 1 || cmnd == 2)
      {
	 scale_factor = get_scale (type);
	 airobj->scale (scale_factor, scale_factor, scale_factor);
	 if (type==69 || type==104 || type==55 || type==128 || type==51
	     || type==70 || type==63 || type==20)
	 {
	    airobj->rotate_z (180);
	    airobj->rotate_x (90);
	 }
	 else if (type==97 || type==98)
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
	 }
	 x = cos(lat)*sin(lon)*(1+alt);
	 y = sin(lat)*(1+alt);
	 z = cos(lat)*cos(lon)*(1+alt);
	 heading_angle = (int)(heading/M_PI*180.00);
	 lat_angle = (int)(lat/M_PI*180.00);
	 lon_angle = (int)(lon/M_PI*180.00);
	 airobj->set_xyz (x, y, z);         // for the purpose of link/unlink;
	 airobj->set_llah (lat_angle, lon_angle, alt, heading_angle);
	 airobj->rotate_z (-heading_angle);
	 airobj->rotate_x (-lat_angle);
	 airobj->rotate_y (lon_angle);
	 airobj->translate (x, y, z);

	 if (cmnd == 1)
	 {
	    asf_displist->add_object (airobj);
	    airobj->add_trail (asf_trails_displist, 255, 0, 25);
	    if (!draw_asf_trails)
	      airobj->trail_off (); // because default is on;
	 }
	 else if (cmnd == 2) // already there, test for attached sensors:
	 {
	    total_sobj = airobj->get_total_sensors();
	    for (j=0; j<total_sobj; j++)
	    {
	       sobj = airobj->get_sobj(j);
	       sobj->reset ();
	       sobj->rotate_z (-heading_angle);
	       sobj->rotate_x (-lat_angle); 
	       sobj->rotate_y (lon_angle);
	       sobj->translate (x, y, z);
	    }
	 }

	 if (cmnd == 1 || cmnd == 2)
	   airobj->add_trail_point (1, x, y, z);

      }
   }
}


/* -------- sensor ---------- */

void 
get_sensor_message (void*, char* buf, int size )
{
   int num_messages;
   Ext_Sensormsg *senmsg;
   int i;
   float time;
   int cmnd;
   char sobj_type[64];
   int  sid;
   char pobj_type[64];
   int  pid;
   float lat, lon, alt, heading;
   int sen_type, sen_status;
   float bs, cov, max, min;
   float scan_type, scan_period, scan_starttime;
   float x, y, z;
   GR_Model *pobj;
   GR_DispObj *sobj;
   int lat_angle, lon_angle;
   
   senmsg = (Ext_Sensormsg*)buf ;
   num_messages = size/sizeof(Ext_Sensormsg);
   
   for (i=0; i<num_messages; i++, senmsg++)
   {
      time = (float)(senmsg->time/10000);
      asf_simtime = time;
      cmnd = (int)(senmsg->cmnd);
      memmove (sobj_type, senmsg->sensorobj_type, 64);
      sid = (int)(senmsg->sensorobj_id);
      memmove (pobj_type, senmsg->platobj_type, 64);
      pid = (int)(senmsg->platobj_id);
      lat = (float)(senmsg->lat/10000/180.0*M_PI); // 10Kdeg to rad
      lon = (float)(senmsg->lon/10000/180.0*M_PI); // 10Kdeg to rad
      alt = (float)(senmsg->alt/10000/RE/3280); // ft to 1/RE
      sen_type = (int)(senmsg->sensor_type);
      sen_status = (int)(senmsg->sensor_status);
      bs = (float)(senmsg->bs/10000);     // in degrees
      cov = (float)(senmsg->coverage/10000);  // in degrees
      max = (float)(senmsg->max/10000)*0.0002903665; // NM to 1/RE
      min = (float)(senmsg->min/10000)*0.0002903665; // NM to 1/RE
      scan_type = (float)(senmsg->scan_type/10000);
      scan_period = (float)(senmsg->scan_period/10000);
      scan_starttime = (float)(senmsg->scan_starttime/10000);
      
      if (cmnd == 1) //create:
      {
	 if (sen_type == 0)     //fixed
	 {
	    printf ("... creating a fixed sensor, id=%d.\n",
		    (sid & 0x000fffff)
		    );
	    if (pid != sid)
	      printf ("Warning: invalid pid %d where sensor %d sits.\n",
		      pid, sid);
	    if (cov == 360.0)
	      sobj = new GR_Sensor (pid, sid, max);
            else
	      sobj = new GR_Sensor (pid, sid, bs, cov, max, min);
	    sobj->set_type (FIXED_SENSOR_TYPE);
	 }
	 else if (sen_type == 1)  // mobile
	 {  
	    printf ("... creating a mobile sensor %d on platform %d\n", 
		    (sid & 0x000fffff),
		    (pid & 0x000fffff)
		    );
	    if (cov==360.0)
	      sobj = new GR_Sensor (pid,sid,0,30,max); // a torus
	    else
	      sobj = new GR_Sensor (pid,sid,bs,cov);  // a cone
	    sobj->set_type (MOBILE_SENSOR_TYPE);
            
	    pobj = (GR_Model*)asf_displist->retrieve_object(pid);
	    if (pobj)
	      pobj->add_sobj((GR_Sensor*)sobj);
	    else
	      printf (" Warning: platform %d does not exist?\n",
		      (pid & 0x000fffff)
		      );
	 }
	 else if (sen_type == 2)   // OTH-B
	 {
	    printf ("... creating an OTH-B sensor %d\n",
		    (sid & 0x000fffff)
		    );
	    sobj = new GR_Othb (pid, sid, bs, cov, max, min);
	    sobj->set_type (OTHB_TYPE);
	 }
	 else
	 {
	    sobj = NULL;
	    printf ("... sensor type %d currently not supported.\n", sen_type);
	 }
	 
	 if (sobj)
	 {
	    if (sen_type == 1) // mobile sensor:
	    {
	       pobj = (GR_Model*) asf_displist->retrieve_object (pid);
	       lat = pobj->get_lat ();
	       lon = pobj->get_lon ();
	       alt = pobj->get_alt ();
	       heading = pobj->get_heading ();
	    }
	    if (alt == 0)
	      alt = 0.00001;
	    x = cos (lat) * sin (lon) * (1+alt);
	    y = sin (lat) * (1+alt);
	    z = cos (lat) * cos (lon) * (1+alt);
	    sobj->set_xyz (x, y, z);
	    sobj->set_llah (lat*180/M_PI, lon*180/M_PI, alt,
			    heading*180/M_PI);
	    lat_angle = (int)(lat/M_PI*180.00);
	    lon_angle = (int)(lon/M_PI*180.00);
	    sobj->rotate_x (-lat_angle);
	    sobj->rotate_y (lon_angle);
	    sobj->translate (x, y, z);
	    asf_displist->add_object (sobj);
	 }
      }
      
      else if (cmnd == 2) // update:
      {
	 switch (sen_status)
	 {
	  case 0: // online
	    if (offlinelist->inlist(sid))
	    {
	       sobj= offlinelist->retrieve_delete_object(sid);
	       asf_displist->add_object (sobj);
	       if (sen_type==1)
	       {
		  pobj = (GR_Model*)asf_displist->retrieve_object(pid);
		  if (pobj)
		    pobj->add_sobj((GR_Sensor*)sobj);
	       } 
	    }
	    break;
	  case 1: // offline
	    if (asf_displist->inlist(sid))
	    {
	       sobj = asf_displist->retrieve_delete_object(sid);
	       offlinelist->add_object (sobj);
	       if (sen_type==1)
	       {
		  pobj = (GR_Model*)asf_displist->retrieve_object(pid);
		  if (pobj)
		    pobj->delete_sobj((GR_Sensor*)sobj);
	       }
	    }
	    break;
	  case 2:  //terminated 
	    if (asf_displist->inlist(sid))
	    {
	       sobj = asf_displist->retrieve_delete_object(sid);
	       if (sen_type==1)
	       {
		  pobj = (GR_Model*)asf_displist->retrieve_object(pid);
		  if (pobj) 
		    pobj->delete_sobj((GR_Sensor*)sobj);
	       }
	    }
	    else if (offlinelist->inlist(sid))
	    {
	       sobj = offlinelist->retrieve_delete_object(sid);
	       if (sen_type==1)
	       {
		  pobj = (GR_Model*)offlinelist->retrieve_object(pid);
		  if (pobj)
		    pobj->delete_sobj((GR_Sensor*)sobj);
	       }
	    }
	    break;
	  default:
	    printf ("Bug: sensor update 2nd-level command %d undefined.\007\n",
		    sen_status);
	    break;
	 }
      }
      else
	printf ("Bug: sensor command %d undefined.\007\n", cmnd);
   }
}

/* ------ track, or in fact: link ------ */
				 
void
get_track_message (void*, char* buf, int size )
{
   int i;
   int num_messages;
   Ext_Linkmsg *linkmsg;
   GR_DispObj *sobj;
   char tobj_type[64], sobj_type[64], pobj_type[64];
   float time;
   int cmnd, tid, sid, pid;
   int sen_type;
      
   linkmsg = (Ext_Linkmsg*)buf ;
   num_messages = size/sizeof(Ext_Linkmsg);
   
   for (i=0; i<num_messages; i++, linkmsg++)
   {
      time = (float)(linkmsg->time/10000);
      asf_simtime = time;
      cmnd = (int)(linkmsg->cmnd);
      memmove (tobj_type, linkmsg->targetobj_type, 64);
      tid = (int)(linkmsg->targetobj_id);
      memmove (sobj_type, linkmsg->sensorobj_type, 64);
      sid = (int)(linkmsg->sensorobj_id);
      memmove (pobj_type, linkmsg->platobj_type, 64);
      pid = (int)(linkmsg->platobj_id);
 
      sobj = asf_displist->retrieve_object(sid);
      if (sobj)
      {
         sen_type = (int) sobj->get_type();
         switch (sen_type)
         {
	  case FIXED_SENSOR_TYPE:
	    printf (" ... linking a fixed sensor...\n");
	    if (pid != sid)
	    {
	       printf ("Warning: overwrite pid %d by sid %d.\n", pid, sid);
	       pid = sid;
	    } 
	    break;
	  case MOBILE_SENSOR_TYPE:
	    printf (" ... linking a mobile sensor...\n");
	    break;
	  case OTHB_TYPE:
	    printf (" ... linking an OTH-B sensor...\n");
	    break;
	  default:
	    printf ("Warning: sensor %d has a type %d?\n", 
		    sid, sen_type);
	    break;
         }
      }
      else
	printf ("Warning: sensor %d does not exist?\007\n", sid); 
 
      switch (cmnd)
      {
       case 18:  // unlink:
	 asf_links->delete_link (pid, tid);
	 break;
       case 20:  // link:
	 asf_links->add_link (pid, tid);
         break;
       default:
	 printf ("Track/Link command %d not defined.\n", cmnd);
	 break;
      }
   }
}


/* ---- missile ----- */

void
get_missile_message (void*, char* buf, int size )
{
   int num_messages;
   Airobjmsg *mismsg;
   int i;
   float time;
   int cmnd;
   long type, obj_id;
   float lat, lon, alt, heading, pitch, roll;
   float x, y, z;
   GR_AirObj *misobj;
   float heading_angle, pitch_angle, roll_angle, lat_angle, lon_angle;
   float scale_factor;
   
   mismsg = (Airobjmsg*)buf ;
   num_messages = size/sizeof(Airobjmsg);
   
   for (i=0; i<num_messages; i++, mismsg++)
   {
      time = (float)(mismsg->time)/10000;
      asf_simtime = time;
      cmnd = (int)(mismsg->cmnd);
      type = (int)(mismsg->type);
      obj_id = (int)(mismsg->obj_id);
      lat = (float)(mismsg->lat)/10000;
      lon = (float)(mismsg->lon)/10000;
      alt = (float)(mismsg->alt)/10000/RE;
      if (alt < 15/RE)
	alt = 15/RE;
      heading = (float)(mismsg->heading)/10000;
      pitch = (float)(mismsg->pitch)/10000;
      roll = (float)(mismsg->roll)/10000;

      switch (cmnd)
      {
       case 1:              // create, or use CMND_DEFINE:
         printf ("... creating missile obj: id %d, type %d, time tag: %f.\n",
		 (obj_id & 0x000fffff),
		 type,
		 time);
         misobj = new GR_AirObj (obj_id, type);
         break;
       case 2:              // update, or use CMND_ADVANCE:
	 misobj=(GR_AirObj*)asf_displist->retrieve_object (obj_id);
	 if (misobj)
	 {
            misobj->reset();
         }
         else
         {
	    cmnd = 1;
            printf ("Warn: update non-existing missile obj: id %d, type %d.\n",
                    (obj_id & 0x000fffff),
		    type);
            misobj = new GR_AirObj (obj_id, type);
         }
         break;
       case 6:                // delete, or use CMND_UNDEFINE:
         misobj=(GR_AirObj*)asf_displist->retrieve_delete_object (obj_id);
	 if (misobj)
	 {
            printf (" One missile object %d deleted\n",
		    (obj_id & 0x000fffff)
		    );
	 }
         break;
       default:
         break;
      }

      if (cmnd == 1 || cmnd == 2)
      {
	 scale_factor = get_scale (type);
	 misobj->scale (scale_factor, scale_factor, scale_factor);
	 if (type==2 || type==6 || type==17 || type==18 || type==19
	     || type==30 || type==34 || type==150 || type==151)
	 {
	    misobj->rotate_x (-90);
	 }
	 else // not in missile family....... may not be correct;
	 {
	    misobj->rotate_z (180);
	    misobj->rotate_x (90);
	 }
	 x = cos(lat)*sin(lon)*(1+alt);
	 y = sin(lat)*(1+alt);
	 z = cos(lat)*cos(lon)*(1+alt);
	 heading_angle = heading/M_PI*180.00;
	 pitch_angle = pitch/M_PI*180.00;
	 //roll_anlge = roll/M_PI*180.00;
	 roll_angle = 0.0;
	 lat_angle = lat/M_PI*180.00;
	 lon_angle = lon/M_PI*180.00;
	 misobj->set_xyz (x, y, z);         // for the purpose of link/unlink;
	 misobj->set_llah (lat_angle, lon_angle, alt, heading_angle);
	 misobj->rotate_z (-heading_angle);
	 misobj->rotate_x (pitch_angle - lat_angle);
	 misobj->rotate_y (roll_angle + lon_angle);
	 misobj->translate (x, y, z);

	 if (cmnd == 1)
	 {
	   asf_displist->add_object (misobj);
	   if (type==2)
	     misobj->add_trail (asf_trails_displist, 0, 0, 255);
	   else if (type==17)
	     misobj->add_trail (asf_trails_displist, 255, 0, 255);
	   else if (type==6 || type==18 || type==19)
	     misobj->add_trail (asf_trails_displist, 255, 0, 0);
	   else if (type==34) // decoy
	     misobj->add_trail (asf_trails_displist, 0, 255, 255);
	   else if (type==150)
	     misobj->add_trail (asf_trails_displist, 0, 255, 0);
	   else if (type==151)
	     misobj->add_trail (asf_trails_displist, 255, 55, 0);
	   else	     
	     misobj->add_trail (asf_trails_displist, 255, 150, 0);
	}

	 if (cmnd == 1 || cmnd == 2)
	   misobj->add_trail_point (5, x, y, z);  // every 5th update;
	 
      }
   }
}




/* === */

void
asf_statisticsCB ()
{
   if (!asf_statistics_panel)
   {
      asf_statistics_init ();
   }
   else
   {
      asf_statistics_update ();
      XRaiseWindow (XtDisplay (asf_statistics_panel->widget ()),
		    XtWindow (asf_statistics_panel->widget ()));
   }
}

void
asf_statistics_init ()
{
   Widget pane, upart, lpart;
   Widget quit_b;
   char str[80];
   
   asf_statistics_panel = new GR_Shell;
   asf_statistics_panel->createWidget ("Asf_Statistics_Panel");
   pane = XtVaCreateWidget
     ("Asf_Pane",
      xmPanedWindowWidgetClass, asf_statistics_panel->widget (),
      NULL);
   upart = XtVaCreateWidget
     ("Upart",
      xmFormWidgetClass, pane,
      NULL);

   // asf_statistics_textw is a global var already defined;
   asf_statistics_textw = XmCreateScrolledText (upart, "Textw", NULL, 0);
   sprintf (str, "Statistics Data:\n\t.....\n");
   XtVaSetValues
     (asf_statistics_textw,
      XmNscrollVertical, True,
      XmNscrollHorizontal, True,
      XmNeditMode, XmMULTI_LINE_EDIT,
      XmNeditable, False,
      XmNwordWrap, True,
      XmNvalue, str,
      NULL);
   XtVaSetValues
     (XtParent (asf_statistics_textw),
      XmNleftAttachment, XmATTACH_FORM,
      XmNrightAttachment, XmATTACH_FORM,
      XmNtopAttachment, XmATTACH_FORM,
      XmNbottomAttachment, XmATTACH_FORM,
      NULL);
   XtManageChild (asf_statistics_textw);
   XtManageChild (upart);
   
   lpart = XtVaCreateWidget
     ("Lpart",
      xmRowColumnWidgetClass, pane,
      XmNorientation, XmHORIZONTAL,
      XmNpacking, XmPACK_COLUMN,
      XmNnumColumns, 1,
      NULL);

   quit_b = XtVaCreateManagedWidget
     ("Quit",
      xmPushButtonWidgetClass, lpart,
      XmNwidth, 36,
      XmNheight, 36,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (quit_b, XmNactivateCallback,
		  (XtCallbackProc)asf_statistics_quitCB, (XtPointer)0);
   XtManageChild (lpart);
 {
    Dimension h;
    XtVaGetValues (quit_b, XmNheight, &h, NULL);
    XtVaSetValues (lpart, XmNpaneMaximum, h, XmNpaneMinimum, h, NULL);
 }
   XtManageChild (pane);
   asf_statistics_panel->realize ();
}


void
asf_statistics_quitCB ()
{
   XtDestroyWidget (asf_statistics_panel->widget());
   asf_statistics_panel = NULL;
}

void
asf_statistics_update ()
{
   char str[400];

   sprintf (str, "Statistics of Current Simulation:\n\
   AWACS: \t\t%d\n   Commercial plane: \t%d\n   Drug Plane: \t\t%d\n",
	    get_statis(asf_displist, 68),
	    get_statis(asf_displist, 69),
	    get_statis(asf_displist, 97)+ get_statis(asf_displist, 98)
	    );
   
   XtVaSetValues
     (asf_statistics_textw,
      XmNvalue, str,
      NULL);
}
