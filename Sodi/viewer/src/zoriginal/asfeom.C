/**************************************************************
  asfeom.C: advanced framework interface -- using eom approach;
  
  -- 03/25/93: created by Tung;
  -- 04/16/93: modify include files and define processing functions;
  -- 05/21/93: first time made it work (using default DT=120);
   timing: 1-min real time => Sim-Control got to 8520, RSD to 7920;
   vs. ASF 1-min real time => Sim-Control got to 10800, RSD to 10800;
  -- 05/24/93: added fw.reset () to resetCB;
  -- 06/08/93: added #include "rsdgw93/eom.H" to compensate the problem
     that Fwmgr.H didn't include it for member function update_eoms()
      which needs type C_EOM for its argument.
  -- 06/14/93: changed "C_FWMGR fw" to "C_FWMGR *fw" and use new;      
  -- 06/22/93: made use of the GR_AirObj, GR_Trail for airsim and missile,
       also changed the panel look (mainly to include trail button)
       much the same way as the changes made to asf.C on 05/07/93;  
  -- 06/25/93: make changes according to the changes on the C_FWMGR side:
       including a new list structure and that fw->get_events() returns
       a value to tell if any message is in transit;
  -- 06/28/93: added statistics panel;
    new timing: 1-min real time => Sim-Control 16640, RSD 12720;       
    also, let asfeom_CB include asfeom_startCB (auto-start at beginning);
  -- 07/01/93: use an IDlist to register deleted AirObj, if a further
    update command is issued to that AirObj the command will be ignored
    (instead of creating a new AirObj);
  -- 07/09/93: airobj now will not be created twice;
  -- 07/19/93: let resetCB also clear statistics panel value, also
    sim time will show the time that just past, not the future time;
  -- 07/23/93: make links & trails toggle buttons show on/off choice;
  -- 07/27/93: revised resetCB and startCB so that the asfeom_idlist will
    be cleared ar reset;
  
**************************************************************/
#include "asfeom.H"

void
asfeom_CB ()
{
   static Boolean first_asfeom = TRUE;
   
   if (first_asfeom || !asfeom_panel)
   {
      printf ("Start ASF-RSDx equation-of-motion interface.\n");
      if (first_asfeom)
      {
	 first_asfeom = FALSE;
	 asfeom_displist = new GR_DispList;
	 gwindow->addDispList (asfeom_displist, "asfeom_displist");
	 asfeom_links_displist = new GR_DispList;
	 gwindow->addDispList (asfeom_links_displist, "asfeom_links_displist");
	 asfeom_links = new GR_Links (asfeom_displist);
	 //  note: it will search asfeom_displist to construct links,
	 //        but will put links into asfeom_links_displist;
	 asfeom_links_displist->add_object (asfeom_links);
	 asfeom_trails_displist = new GR_DispList;
	 gwindow->addDispList (asfeom_trails_displist,
			       "asfeom_trails_displist");
	 asfeom_simtime = 0.0;
	 asfeom_prevtime = 0.0;
	 asfeom_deltatime = 120.0;
      }
      asfeom_init ();
      
      if (!fw)
      {
	 fw = new C_FWMGR ();
	 fw->set_timeout (0);
      }
      draw_asfeom_links = TRUE;
      draw_asfeom_trails = TRUE;
      if (!asfeom_idlist)
	asfeom_idlist = new IDlist (1024);
      asfeom_startCB ();
   }
   else
   {
      XRaiseWindow (XtDisplay (asfeom_panel->widget ()),
		    XtWindow (asfeom_panel->widget ()));
   }
}

Widget asfeom_links_b, asfeom_trails_b;

void
asfeom_init ()
{
   Widget text_rowcol, simtime_label, deltatime_label, group_label;
   Widget button_area;
   //Widget cpanel_b;
   //Widget links_b, trails_b;
   Widget statistics_b;
   Widget start_b, pause_b, reset_b, quit_b;
   XmString title;
   char simtime_string[20], deltatime_string[20], *group_string;

   asfeom_panel = new GR_Shell;
   asfeom_panel->createWidget ("Asfeom_Panel");
   asfeom_form = XtVaCreateManagedWidget
     ("Asfeom_Form",
      xmFormWidgetClass, asfeom_panel->widget (),
      NULL);
   text_rowcol = XtVaCreateWidget  // not managed yet...
     ("Text_Rowcol",
      xmRowColumnWidgetClass, asfeom_form,
      XmNleftAttachment, XmATTACH_FORM,
      XmNrightAttachment, XmATTACH_FORM,
      XmNrightOffset, 20,
      XmNtopAttachment, XmATTACH_POSITION,
      XmNtopPosition, 10,
      XmNbottomAttachment, XmATTACH_POSITION,
      XmNbottomPosition, 60,      
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
   title = XmStringCreateSimple ("Time Step: ");
   deltatime_label = XtVaCreateManagedWidget
     ("Deltaime_Label",
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
   
   asfeom_simtime_text = XtVaCreateManagedWidget
     ("Simtime_Text",
      xmTextFieldWidgetClass, text_rowcol,
      XmNeditable, False,
      NULL);
   asfeom_deltatime_text = XtVaCreateManagedWidget
     ("Deltatime_Text",
      xmTextFieldWidgetClass, text_rowcol,
      NULL);
   XtAddCallback
     (asfeom_deltatime_text,
      XmNactivateCallback,
      (XtCallbackProc)asfeom_textCB,
      (XtPointer)0);   // use 0 to represent the delta time;
   asfeom_group_text = XtVaCreateManagedWidget
     ("Group_Label",
      xmTextFieldWidgetClass, text_rowcol,
      NULL);
   XtAddCallback
     (asfeom_group_text,
      XmNactivateCallback,
      (XtCallbackProc)asfeom_textCB,
      (XtPointer)1);   // use 1 to represent the group name;
   XtManageChild (text_rowcol);

   sprintf (simtime_string, "%.1f", asfeom_simtime);
   XtVaSetValues (asfeom_simtime_text,
                  XmNvalue, simtime_string,
                  NULL);
   sprintf (deltatime_string, "%.1f", asfeom_deltatime);
   XtVaSetValues (asfeom_deltatime_text,
		  XmNvalue, deltatime_string,
		  NULL);

   group_string = msg_get_groupname ();
   if (group_string == NULL)
      printf ("Note: group name is not set (NULL).\n");
   else if (strcmp (group_string, "") == 0)
      printf ("Note: group name is not set (empty string).\n");
   else
   {
      printf ("The current group name is %s\n", group_string);
      XtVaSetValues (asfeom_group_text,
		     XmNvalue, group_string,
		     NULL);
   } 

   button_area = XtVaCreateManagedWidget
     ("Button_Area",
      xmRowColumnWidgetClass, asfeom_form,
      XmNleftAttachment, XmATTACH_FORM,
      XmNrightAttachment, XmATTACH_FORM,
      XmNtopAttachment, XmATTACH_POSITION,
      XmNtopPosition, 65, 
      XmNbottomAttachment, XmATTACH_FORM,
      XmNorientation, XmHORIZONTAL,
      XmNleftOffset, 28,
      XmNspacing, 14,
      XmNpacking, XmPACK_COLUMN,
      XmNnumColumns, 2,
      NULL);

   /*
   cpanel_b = XtVaCreateManagedWidget
   ("C. Panel",
   xmPushButtonWidgetClass, button_area,
   XmNwidth, 36,
   XmNheight, 36,
   XmNshadowThickness, 4,
   NULL);
   XtAddCallback (cpanel_b, XmNactivateCallback,
   (XtCallbackProc)asfeom_cpanelCB, (XtPointer)0);
   */

   asfeom_links_b = XtVaCreateManagedWidget
     ("Links Off",
      xmPushButtonWidgetClass, button_area,
      XmNwidth, 46,
      XmNheight, 36,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (asfeom_links_b, XmNactivateCallback,
		  (XtCallbackProc)asfeom_linksCB, (XtPointer)0);
   
   asfeom_trails_b = XtVaCreateManagedWidget
     ("Trails Off",
      xmPushButtonWidgetClass, button_area,
      XmNwidth, 46,
      XmNheight, 36,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (asfeom_trails_b, XmNactivateCallback,
		  (XtCallbackProc)asfeom_trailsCB, (XtPointer)0);

   XtVaCreateManagedWidget    //dummy button just to fill one position;
     (" ",
      xmPushButtonWidgetClass, button_area,
      XmNwidth, 36,
      XmNheight, 36,
      XmNshadowThickness, 4,
      NULL);

   statistics_b =  XtVaCreateManagedWidget  
     (
      "Statis",
      xmPushButtonWidgetClass, button_area,
      XmNwidth, 36,
      XmNheight, 36,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (statistics_b, XmNactivateCallback,
		  (XtCallbackProc)asfeom_statisticsCB, (XtPointer)0);
   
   
   start_b = XtVaCreateManagedWidget
     ("Cont.",
      xmPushButtonWidgetClass, button_area,
      XmNwidth, 36,
      XmNheight, 36,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (start_b, XmNactivateCallback,
		  (XtCallbackProc)asfeom_startCB, (XtPointer)0);
   pause_b = XtVaCreateManagedWidget
     ("Pause",
      xmPushButtonWidgetClass, button_area,
      XmNwidth, 36,
      XmNheight, 36,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (pause_b, XmNactivateCallback,
		  (XtCallbackProc)asfeom_pauseCB, (XtPointer)0);
   reset_b = XtVaCreateManagedWidget
     ("Reset",
      xmPushButtonWidgetClass, button_area,
      XmNwidth, 36,
      XmNheight, 36,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (reset_b, XmNactivateCallback,
		  (XtCallbackProc)asfeom_resetCB, (XtPointer)0);
   quit_b = XtVaCreateManagedWidget
     ("Quit",
      xmPushButtonWidgetClass, button_area,
      XmNwidth, 36,
      XmNheight, 36,
      XmNshadowThickness, 4,
      NULL);
   XtAddCallback (quit_b, XmNactivateCallback,
		  (XtCallbackProc)asfeom_quitCB, (XtPointer)0);

   asfeom_panel->realize ();
}


void
asfeom_warnCB ()
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
asfeom_cpanelCB ()
{
   Widget warndialog;
   XmString warnmsg, doit;
   
   warndialog =
     XmCreateWarningDialog (asfeom_panel->widget(), "warning", NULL, 0);
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
		  (XtCallbackProc)asfeom_warnCB,
		  NULL);
   XmStringFree (warnmsg);
   XmStringFree (doit);
   XtManageChild (warndialog);
   XtPopup (XtParent(warndialog), XtGrabNone);
}

void
asfeom_startCB ()
{
   if (asfeom_Id)
     XtRemoveWorkProc (asfeom_Id);
   printf (" start or resume external message handling ...\n");
   asfeom_Id = XtAppAddWorkProc (GR_appcontext, (XtWorkProc)asfeom_WP, 0);
   if (!asfeom_displist)
     asfeom_displist = new GR_DispList;

   if (!asfeom_links_displist)
   {
     asfeom_links_displist = new GR_DispList;
     asfeom_links = new GR_Links (asfeom_displist);
     asfeom_links_displist->add_object (asfeom_links);
   }
   
   if (!asfeom_trails_displist)
     asfeom_trails_displist = new GR_DispList;
   gwindow->addDispList (asfeom_displist, "asfeom_displist");
   gwindow->addDispList (asfeom_links_displist, "asfeom_links_displist");
   gwindow->addDispList (asfeom_trails_displist, "asfeom_trails_displist");

   if (!asfeom_idlist)
   {
     asfeom_idlist = new IDlist (1024); 
   }

}


void
asfeom_pauseCB ()
{
   if (asfeom_Id)
   {
      XtRemoveWorkProc (asfeom_Id);
      asfeom_Id = NULL;
   }
}


void
asfeom_linksCB ()
{
   XmString links_on, links_off;

   links_on = XmStringCreateSimple ("Links On");
   links_off = XmStringCreateSimple ("Links Off"); 
   draw_asfeom_links = !draw_asfeom_links;
   if (draw_asfeom_links)
   {
      printf ("will draw links ....\n");
      if (asfeom_links_displist)
	gwindow->addDispList (asfeom_links_displist, "asfeom_links_displist");
      XtVaSetValues (asfeom_links_b,
                     XmNlabelString, links_off,
                     NULL);
   }
   else
   {
      printf ("will not draw links ....\n");
      gwindow->remDispList (asfeom_links_displist, "asfeom_links_displist");
      XtVaSetValues (asfeom_links_b,
                     XmNlabelString, links_on,
                     NULL);
   }
   XmStringFree (links_on);
   XmStringFree (links_off);
   gwindow->draw ();
}

void
asfeom_trailsCB ()
{
   GR_DispObj *obj;
   XmString trails_on, trails_off;

   trails_on = XmStringCreateSimple ("Trails On");
   trails_off = XmStringCreateSimple ("Trails Off");

   draw_asfeom_trails = !draw_asfeom_trails;
   if (draw_asfeom_trails)
   {
      printf ("will draw trails ....\n");
      if (asfeom_trails_displist)
      {
	 for (obj=asfeom_trails_displist->enumerate(1);
	      obj;
	      obj=asfeom_trails_displist->enumerate(0))
	 {
	    obj->set_visible_flag (1);
	 }
	 gwindow->addDispList (asfeom_trails_displist,
			       "asfeom_trails_displist");
      }
      XtVaSetValues (asfeom_trails_b,
	             XmNlabelString, trails_off,
                     NULL);
   }
   else
   {
      printf ("will not draw trails ....\n");
      if (asfeom_trails_displist)
      {
	 for (obj=asfeom_trails_displist->enumerate(1);
	      obj;
	      obj=asfeom_trails_displist->enumerate(0))
	 {
	    obj->set_visible_flag (0);
	 }
	 gwindow->addDispList (asfeom_trails_displist,
			       "asfeom_trails_displist");
      }
      XtVaSetValues (asfeom_trails_b,
                     XmNlabelString, trails_on,
                     NULL);
   }
   XmStringFree (trails_on);
   XmStringFree (trails_off);
   gwindow->draw ();
}


void
asfeom_resetCB ()
{
   char simtime_string[20];
   
   if (asfeom_Id)
   {
      XtRemoveWorkProc (asfeom_Id);
      asfeom_Id = NULL;
   }
   if (asfeom_displist)
   {
      //asfeom_displist->delete_objects ();
      gwindow->remDispList (asfeom_displist, "asfeom_displist");
      asfeom_displist = NULL; 
   }
   if (asfeom_links)
   {
      delete asfeom_links;
      gwindow->remDispList (asfeom_links_displist, "asfeom_links_displist");
      asfeom_links_displist = NULL;
   }
   
   if (asfeom_trails_displist)
   {
      //delete asfeom_trails_displist;
      gwindow->remDispList(asfeom_trails_displist,"asfeom_trails_displist");
      asfeom_trails_displist = NULL; 
   }

   asfeom_prevtime = 0.0;
   asfeom_simtime = 0.0;
   draw_asfeom_links = TRUE;
   draw_asfeom_trails = TRUE;
   sprintf (simtime_string, "%.1f", asfeom_simtime);
   XtVaSetValues (asfeom_simtime_text,
                  XmNvalue, simtime_string,
                  NULL);
   
   if (asfeom_statistics_panel)
       asfeom_statistics_update ();

   if (asfeom_idlist)
   {
      delete asfeom_idlist;
      asfeom_idlist = NULL;
   }

   gwindow->draw ();
   
   // added on 5/24/93:
   fw->reset ();
}

void
asfeom_quitCB ()
{
   asfeom_resetCB ();
   if (asfeom_statistics_panel)
   {
      XtDestroyWidget (asfeom_statistics_panel->widget());
      asfeom_statistics_panel = NULL;
   }
   XtDestroyWidget (asfeom_panel->widget());
   asfeom_panel = NULL;
}

void
asfeom_textCB (Widget text_w, int code)
{
   char *text_str = XmTextFieldGetString (text_w);
   double value;
   
   switch (code)
   {
    case 0: // delta time:
      if (sscanf (text_str, "%lf", &value) == 1)
      {
	 asfeom_deltatime = value;
	 if (asfeom_Id)
	 {
	    XtRemoveWorkProc (asfeom_Id);
	    asfeom_Id = XtAppAddWorkProc
	      (GR_appcontext,
	       (XtWorkProc)asfeom_WP,
	       0);
	 }
         printf ("The current time step is: %f\n", asfeom_deltatime);
      }
      else
         printf ("Invalid time step entered!\007");
      break;

    case 1:  // the group name:
      printf ("  Changing group name...  ");
      fw->reregister (text_str);
      printf (" Now the group name has been set to %s\n",
	      (char*)msg_get_groupname ());
      XtFree (text_str);
      break;

    default:
      printf ("Unknown code %d when called asfeom_textCB\n", code);
      break;
   }
}
     

/* ---- */


Boolean
asfeom_WP (XtPointer)
{
  char simtime_string[10];
  //S_GENERIC_LIST *msglist = NULL;
  //S_GENERIC_LIST *msgitem = NULL;
  C_LIST *msglist = NULL;
  C_GENERIC_ITEM *msgitem = NULL;
  int num_events = 0;
  C_RSD_MSG *airmsg;
  SEN_config_msg_str *senmsg;
  SEN_rsdx_msg_str *linkmsg;
  RSD_UPD *missmsg;
  double gvt;
  int errno;
  static int errcount = 0;
  static int okcount = 0;
  static int check_cycle = 0;

  errno = fw->get_events (asfeom_simtime, msglist, num_events, gvt);
  if (errno != 0)
  {
     errcount++;
     //fprintf (stderr, "Warning [FWMGR]: %s\n", _sim_errmsgs[errno]);
     //fprintf (stderr, "Warning: incomplete cycle %d, complete cycle %d.\n",
     //         errcount, okcount);
     if ((errcount > 1) && (errcount*10 > okcount))
     {
	fw->set_timeout (fw->get_timeout() + 1);
	fprintf (stderr, "Increase FWMGR timeout to %d.\n",
	         fw->get_timeout());
	errcount = 0;
	okcount = 0;
     }
  }
  else
  {
     okcount++;
     if ((okcount > 1000) && (okcount > (errcount*1000)) &&
	 (fw->get_timeout() > 0))
     {
	fw->set_timeout (fw->get_timeout() - 1);
	fprintf (stderr, "Decrease FWMGR timeout to %d.\n",
		 fw->get_timeout());
	errcount = 0;
	okcount = 0;
     }
  }
     
  if ((gvt==0.0 && num_events<=0) || (gvt<asfeom_simtime))
  {
     check_cycle++;
     if (check_cycle >= 30000)
     {
	if (gvt==0.0 && num_events<=0)
	  fprintf (stderr, "Still listening and waiting.\n");
	else
	  fprintf (stderr, "Reminder: looped 30K times and GVT is: %f\n", gvt);
	check_cycle = 0;
     }
     return FALSE;
  }
  check_cycle = 0;

  if (num_events)
  {
     msgitem = (C_GENERIC_ITEM*)msglist->remove_top ();
     for (;msgitem;)
     {
	switch (msgitem->type)
	{
	 case AIRSIM_TO_RSDX_EVENT:
	   airmsg = (C_RSD_MSG*)msgitem->get_data ();
	   process_air_msg (airmsg); 
	   break;
	 case SENSOR_CONFIG_TO_RSDX_EVENT:
	   senmsg = (SEN_config_msg_str*)msgitem->get_data ();
	   process_sensor_msg (senmsg);
	   break;
	 case SENSOR_TRACK_TO_RSDX_EVENT:
	   linkmsg = (SEN_rsdx_msg_str*)msgitem->get_data ();
	   process_link_msg (linkmsg);
	   break;
	 case EG_MISSILE_TO_RSDX_EVENT:
	   missmsg = (RSD_UPD*)msgitem->get_data ();
	   process_missile_msg (missmsg);
	   break;
	 default:
	   printf ("Unknown msg type %d\n\007", msgitem->type);
	   break;
	}
	msgitem = (C_GENERIC_ITEM*)msglist->remove_top ();
     }
  } 

  if (errno == 0)  // no message in transit, time can be advanced;
  {
     sprintf (simtime_string, "%.1f", asfeom_simtime);
     XtVaSetValues (asfeom_simtime_text,
		    XmNvalue, simtime_string,
		    NULL);
     asfeom_prevtime = asfeom_simtime;
     asfeom_simtime += asfeom_deltatime;
  }

  if (num_events > 0)
  {
     if (asfeom_statistics_panel)
       asfeom_statistics_update ();
     gwindow->draw ();
  }
  return FALSE; // i.e., continue;
}



/* ==== */

void
process_air_msg (C_RSD_MSG* airmsg)
{
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
   Boolean ignore = FALSE;
   
   //printf ("Get an airsim message...\n");
   /* 
   time = (float)(airmsg->time)/10000;
   cmnd = (int)(airmsg->cmnd);
   type = (int)(airmsg->type);
   obj_id = (int)(airmsg->obj_id);
   lat = (float)(airmsg->lat)/10000;
   lon = (float)(airmsg->lon)/10000;
   alt = (float)(airmsg->alt)/10000/RE;
   if (alt < 15/RE)
     alt = 15/RE;
   heading = (float)(airmsg->heading)/10000;
   */
 
   time = (float)(airmsg->time);
   cmnd = (int)(airmsg->cmnd);
   type = (int)(airmsg->type);
   obj_id = (int)(airmsg->obj_id);
   lat = (float)(airmsg->lat);
   lon = (float)(airmsg->lon);
   alt = (float)(airmsg->alt)/RE;
   if (alt < 15/RE)
     alt = 15/RE;
   heading = (float)(airmsg->heading);

   //fprintf (stderr, "... airmsg: time %f, cmnd %d, type %d, id %d, lat %f, lon %f, alt %f, heading %f .\n",
   //   time, cmnd, type, obj_id, lat, lon, alt, heading);

   switch (cmnd)
   {
    case 1:              // create, or CMND_DEFINE:
      airobj=(GR_AirObj*)asfeom_displist->retrieve_object (obj_id);
      if (airobj)
      {
	 printf ("Warning: try to define the same object (id: %d) twice?\
 -- I'll ignore this command.\n",
		 (obj_id & 0x000fffff));
	 ignore = TRUE;
      }
      else if (asfeom_idlist->in_list (obj_id))
      {
         printf ("Warning: re-create the landed air obj: id %d (%d), why?\
 -- I'll ignore this command.\007\n",
                 (obj_id & 0x000fffff), obj_id);
         ignore = TRUE;
      }
      else
      {
	 // printf ("Creating one air object %d\n", (obj_id & 0x000fffff));
	 airobj = new GR_AirObj (obj_id, type);
      }
      break;
    case 2:              // update, or use CMND_ADVANCE:
      airobj=(GR_AirObj*)asfeom_displist->retrieve_object (obj_id);
      if (airobj)
      {
	 airobj->reset();
      }
      else
      {
	 if (asfeom_idlist->in_list (obj_id))
	 {
	    printf ("Warning: try to update an landed air obj: id %d?\
 -- I'll ignore this command.\n",
		    (obj_id & 0x000fffff));
	    ignore = TRUE;
	 }
	 else
	 {
	    printf ("Warning: try to update non-existing air obj: id %d\
 -- I'll create one for you anyway.\n",
		    (obj_id & 0x000fffff));
	    cmnd = 1;  // correct and recover from airsim error;
	    airobj = new GR_AirObj (obj_id, type);
	 }
      }
      break;
    case 6:             // delete, or CMND_UNDEFINE:
      airobj=(GR_AirObj*)asfeom_displist->retrieve_delete_object (obj_id);
      if (airobj)
      {
         // printf ("Deleting one air object %d\n", (obj_id & 0x000fffff)); 
	 airobj->delete_trail (asfeom_trails_displist);
	 for (i=0; i<airobj->get_total_sensors(); i++)
	 {
	    sobj = airobj->get_sobj(i);
	    if (sobj)
            {
               //asfeom_displist->delete_object(sobj);
               sid = sobj->get_id();
	       asfeom_displist->delete_object(sid);
	       //printf ("   one mobile sensor %d deleted.\n", sid);
            }
	 }
	 asfeom_idlist->put_list (obj_id);
      }
      else
      {
         printf ("Warning: try to delete object %d that does not exist?\007\n",
                 (obj_id & 0x000fffff));
         asfeom_idlist->put_list (obj_id);
      }
      break;
    default:
      break;
   }

   if ( (!ignore) && (cmnd == 1 || cmnd == 2) )
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
      
      airobj->set_xyz (x, y, z);           // for the purpose of link/unlink;
      airobj->set_llah (lat_angle, lon_angle, alt, heading_angle);
      
      airobj->rotate_z (-heading_angle);
      airobj->rotate_x (-lat_angle);
      airobj->rotate_y (lon_angle);
      airobj->translate (x, y, z);

      if (cmnd == 1)
      {
	 asfeom_displist->add_object (airobj);
	 airobj->add_trail (asfeom_trails_displist, 255, 0, 25);
	 if (!draw_asfeom_trails)
	   airobj->trail_off ();  // because default is on;
      }	 
      else if (cmnd == 2) // already there, test for attached sensors:
      {
	 total_sobj = airobj->get_total_sensors();
	 for (j=0; j<total_sobj;j++)
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

void
process_sensor_msg (SEN_config_msg_str* senmsg)
{
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
   
   //fprintf (stderr, "OK, got a sensor config message.......\n");
   
   time = (float)(senmsg->message_time/10000);
   cmnd = (int)(senmsg->message_code);
   memmove (sobj_type, senmsg->sensor_object_type, 64);
   sid = (int)(senmsg->sensor_object_id);
   memmove (pobj_type, senmsg->platform_object_type, 64);
   pid = (int)(senmsg->platform_object_id);
   lat = (float)(senmsg->platform_latitude/10000/180.0*M_PI); // 10Kdeg to rad
   lon = (float)(senmsg->platform_longitude/10000/180.0*M_PI); // 10Kdeg to rad
   //alt = (float)(senmsg->platform_altitude/10000)*0.0002907; // NM to 1/RE
   alt = (float)(senmsg->platform_altitude/10000/RE/3280); // ft to 1/RE
   sen_type = (int)(senmsg->sensor_type);
   sen_status = (int)(senmsg->sensor_status);
   bs = (float)(senmsg->sensor_boresight_angle/10000);     // in degrees
   cov = (float)(senmsg->sensor_coverage_angle/10000);  // in degrees
   max = (float)(senmsg->sensor_max_range/10000)*0.0002907; // NM to 1/RE
   min = (float)(senmsg->sensor_min_range/10000)*0.0002907; // NM to 1/RE
   scan_type = (float)(senmsg->sensor_scan_type/10000);
   scan_period = (float)(senmsg->sensor_scan_period/10000);
   scan_starttime = (float)(senmsg->sensor_scan_starttime/10000);
   
   if (cmnd == 1)  // create:
   {
      if (sen_type == 0)     //fixed
      {
	 //printf ("... creating a fixed sensor, id=%d.\n", sid);
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
	 //printf ("... creating a mobile sensor %d on platform %d\n",
	 //	 sid, pid);
	 if (cov==360.0)
	   sobj = new GR_Sensor (pid, sid, 0, 30, max); // a torus
	 else
	   sobj = new GR_Sensor (pid, sid, bs, cov);    // a cone
	 sobj->set_type (MOBILE_SENSOR_TYPE);
	 
	 pobj = (GR_Model*)asfeom_displist->retrieve_object(pid);
	 if (pobj)
	   pobj->add_sobj((GR_Sensor*)sobj);
	 else
	   printf (" Warning: platform %d does not exist?\n",pid);
      }
      else if (sen_type == 2)   // OTH-B
      {
	 //printf ("... creating an OTH-B sensor %d\n", sid);
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
	    pobj = (GR_Model*) asfeom_displist->retrieve_object (pid);
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
	 
	 asfeom_displist->add_object (sobj);
      }
   }
   else if (cmnd == 2) // update:
   {
      switch (sen_status)
      {
       case 0: // online
	 if (asfeom_offlinelist->inlist(sid))
	 {
	    sobj= asfeom_offlinelist->retrieve_delete_object(sid);
	    asfeom_displist->add_object (sobj);
	    if (sen_type==1)
	    {
	       pobj = (GR_Model*)asfeom_displist->retrieve_object(pid);
	       if (pobj)
		 pobj->add_sobj((GR_Sensor*)sobj);
	    }
	 }
	 break;
       case 1: // offline
	 if (asfeom_displist->inlist(sid))
	 {
	    sobj = asfeom_displist->retrieve_delete_object(sid);
	    asfeom_offlinelist->add_object (sobj);
	    if (sen_type==1)
	    {
	       pobj = (GR_Model*)asfeom_displist->retrieve_object(pid);
	       if (pobj)
		 pobj->delete_sobj((GR_Sensor*)sobj);
	    }
	 }
	 break;
       case 2:  // terminated
	 if (asfeom_displist->inlist(sid))
	 {
	    sobj = asfeom_displist->retrieve_delete_object(sid);
	    if (sen_type==1)
	    {
	       pobj = (GR_Model*)asfeom_displist->retrieve_object(pid);
	       if (pobj)
		 pobj->delete_sobj((GR_Sensor*)sobj);
	    }
	 }
	 else if (asfeom_offlinelist->inlist(sid))
	 {
	    sobj = asfeom_offlinelist->retrieve_delete_object(sid);
	    if (sen_type==1)
	    {
	       pobj = (GR_Model*)asfeom_offlinelist->retrieve_object(pid);
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

void
process_link_msg (SEN_rsdx_msg_str* linkmsg)
{
   long pid, sid, tid;
   GR_DispObj *sobj;
   char tobj_type[64], sobj_type[64], pobj_type[64];
   float time;
   int cmnd;
   long sen_type;
   
   //printf ("Get a link message...\n");
   
   time = (float)(linkmsg->detection_time/10000);
   cmnd = (int)(linkmsg->display_command);
   memmove (tobj_type, linkmsg->target_object_type, 64);
   tid = (int)(linkmsg->target_object_id);
   memmove (sobj_type, linkmsg->sensor_object_type, 64);
   sid = (int)(linkmsg->sensor_object_id);
   memmove (pobj_type, linkmsg->platform_object_type, 64);
   pid = (int)(linkmsg->platform_object_id);
   
   sobj = asfeom_displist->retrieve_object (sid);
   if (sobj)
   {
      sen_type = sobj->get_type();
      switch (sen_type)
      {
       case FIXED_SENSOR_TYPE:
	 //printf (" ... linking a fixed sensor...\n");
	 if (pid != sid)
	 {
	    printf ("Warning: overwrite pid %d by sid %d.\n", pid, sid);
	    pid = sid;
	 }
	 break;
       case MOBILE_SENSOR_TYPE:
	 //printf (" ... linking a mobile sensor...\n");
	 break;
       case OTHB_TYPE:
	 //printf (" ... linking an OTH-B sensor...\n");
	 break;
       default:
	 printf ("Warning: sensor %d has an unknown type %d?\n",
		 sid, sen_type);
	 break;
      }
   }
   else
     printf ("Warning: sensor %d does not exist?\n", sid);
   
   switch (cmnd)
   {
    case 18:  // unlink:
      asfeom_links->delete_link (pid, tid);
      break;
    case 20:  // link:
      asfeom_links->add_link (pid, tid);
      break;
    default:
      printf ("Link command %d not defined.\n", cmnd);
      break;
   }
}


void
process_missile_msg (RSD_UPD* mismsg)
{
   float time;
   int cmnd;
   long type, obj_id;
   float lat, lon, alt, heading, pitch, roll;
   float x, y, z;
   GR_AirObj *misobj;
   float heading_angle, pitch_angle, roll_angle, lat_angle, lon_angle;
   float scale_factor;
   Boolean ignore = FALSE;
   
   time = (float)(mismsg->time)/10000;
   cmnd = (int)(mismsg->cmnd);
   type = (int)(mismsg->icon_type);
   obj_id = (int)(mismsg->id);
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
      misobj=(GR_AirObj*)asfeom_displist->retrieve_object (obj_id);
      if (misobj)
      {
	 printf ("Warning: try to define the same object (id: %d) twice?\
 -- I'll ignore this command.\n",
		 (obj_id & 0x000fffff));
	 ignore = TRUE;
      }
      else
      {
	 //printf ("... creating missile obj: id %d, type %d, time tag: %f.\n",
	 // (obj_id & 0x000fffff),
	 //type,
	 //time);
	 misobj = new GR_AirObj (obj_id, type);
      }
      break;
    case 2:              // update, or use CMND_ADVANCE:
      misobj=(GR_AirObj*)asfeom_displist->retrieve_object (obj_id);
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
      misobj=(GR_AirObj*)asfeom_displist->retrieve_delete_object (obj_id);
      if (misobj)
      {
	//printf (" One missile object %d deleted\n", (obj_id & 0x000fffff));
      }
      else
	printf ("Warning: try to delete an non-existing object %d.\n",
		(obj_id & 0x000fffff));
      break;
    default:
      break;
   }

   if ( (!ignore) && (cmnd == 1 || cmnd == 2) )
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
	 asfeom_displist->add_object (misobj);
	 if (type==2)
	   misobj->add_trail (asfeom_trails_displist, 0, 0, 255);
	 else if (type==17)
	   misobj->add_trail (asfeom_trails_displist, 255, 0, 255);
	 else if (type==6 || type==18 || type==19)
	   misobj->add_trail (asfeom_trails_displist, 255, 0, 0);
	 else if (type==34) // decoy
	   misobj->add_trail (asfeom_trails_displist, 0, 255, 255);
	 else if (type==150)
	   misobj->add_trail (asfeom_trails_displist, 0, 255, 0);
	 else if (type==151)
	   misobj->add_trail (asfeom_trails_displist, 255, 55, 0);
	 else	     
	   misobj->add_trail (asfeom_trails_displist, 255, 150, 0);
      }
      
      if (cmnd == 1 || cmnd == 2)
	misobj->add_trail_point (5, x, y, z);  // every 5th update;
      
   }
}


/* === */
void
asfeom_statisticsCB ()
{
   if (!asfeom_statistics_panel)
   {
      asfeom_statistics_init ();
   }
   else
   {
      asfeom_statistics_update ();
      XRaiseWindow (XtDisplay (asfeom_statistics_panel->widget ()),
		    XtWindow (asfeom_statistics_panel->widget ()));
   }
}


void
asfeom_statistics_init ()
{
   Widget pane, upart, lpart;
   Widget quit_b;
   char str[80];
   
   asfeom_statistics_panel = new GR_Shell;
   asfeom_statistics_panel->createWidget ("Asfeom_Statistics_Panel");
   pane = XtVaCreateWidget
     ("Asfeom_Pane",
      xmPanedWindowWidgetClass, asfeom_statistics_panel->widget (),
      NULL);
   upart = XtVaCreateWidget
     ("Upart",
      xmFormWidgetClass, pane,
      NULL);

   // asfeom_statistics_textw is a global var already defined;
   asfeom_statistics_textw = XmCreateScrolledText (upart, "Textw", NULL, 0);
   sprintf (str, "Statistics Data:\n\t.....\n");
   XtVaSetValues
     (asfeom_statistics_textw,
      XmNscrollVertical, True,
      XmNscrollHorizontal, True,
      XmNeditMode, XmMULTI_LINE_EDIT,
      XmNeditable, False,
      XmNwordWrap, True,
      XmNvalue, str,
      NULL);
   XtVaSetValues
     (XtParent (asfeom_statistics_textw),
      XmNleftAttachment, XmATTACH_FORM,
      XmNrightAttachment, XmATTACH_FORM,
      XmNtopAttachment, XmATTACH_FORM,
      XmNbottomAttachment, XmATTACH_FORM,
      NULL);
   XtManageChild (asfeom_statistics_textw);
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
		  (XtCallbackProc)asfeom_statistics_quitCB, (XtPointer)0);
   XtManageChild (lpart);
 {
    Dimension h;
    XtVaGetValues (quit_b, XmNheight, &h, NULL);
    XtVaSetValues (lpart, XmNpaneMaximum, h, XmNpaneMinimum, h, NULL);
 }
   XtManageChild (pane);
   asfeom_statistics_panel->realize ();
}


void
asfeom_statistics_quitCB ()
{
   XtDestroyWidget (asfeom_statistics_panel->widget());
   asfeom_statistics_panel = NULL;
}

void
asfeom_statistics_update ()
{
   char str[400];

   sprintf (str, "Statistics of Current Simulation:\n\
   AWACS: \t\t%d\n   Commercial Plane: \t%d\n   Drug Plane: \t\t%d\n",
	    get_statis(asfeom_displist, 68),
	    get_statis(asfeom_displist, 69),
	    get_statis(asfeom_displist, 97)+ get_statis(asfeom_displist, 98)
	    );
   
   XtVaSetValues
     (asfeom_statistics_textw,
      XmNvalue, str,
      NULL);
}
