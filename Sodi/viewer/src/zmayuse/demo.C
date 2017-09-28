/************************************************************
  Animation Demo
  -- 11/20/92 by Y. Tung
  -- 05/03/93: removed a bug, make use of trail stuff;
  
************************************************************/

#include <math.h>
//#include <unistd.h>

#include "GR_Sensor.H"
#include "GR_Othb.H"
#include "GR_Model.H"
#include "GR_AirObj.H"
#include "GR_Army.H"

GR_DispList *demo_displist;
GR_DispList *demo_trail_displist;
extern GR_Window *gwindow;
extern void focusCB (Widget, XtPointer, XtPointer);
 
XtWorkProcId demoId = (long unsigned int)NULL;
Boolean demoWP (XtPointer);
int demostate = 0;
#define MAXdemostate 60
#define RE 6378.145

class Demolink: public GR_DispObj
{
 private:
   float *p_pv;
   float *p_tv;
 public:
   Demolink() {}
   void setlink (float *pv, float *tv) {p_pv=pv, p_tv=tv;}
   void objdraw();
};

void
Demolink:: objdraw()
{
  GR_color(255,255,0);
  GR_bgnline();
   GR_v3f(p_pv);
   GR_v3f(p_tv);
  GR_endline();
}


void
demoCB ()
{
  if (demostate == 0)
  {
     demoId = XtAppAddWorkProc (GR_appcontext, (XtWorkProc)demoWP, 0);
  }
  else if (demostate > MAXdemostate)
  {
     printf ("Stop animation demo.\n");
     XtRemoveWorkProc (demoId);
     demoId = (long unsigned int)NULL;
     demostate = 0;
     demo_displist->delete_objects ();
     gwindow->remDispList (demo_displist, "demo_displist");
     demo_trail_displist->delete_objects ();
     gwindow->remDispList (demo_trail_displist, "demo_trail_displist");
     gwindow->draw ();
  }
  else
  {
     XtRemoveWorkProc (demoId);
     demoId = XtAppAddWorkProc (GR_appcontext, (XtWorkProc)demoWP, 0);
  }
}


GR_Sensor *sensor, *cone, *torus;
//GR_Model *airobj, *coneplat, *plat;
GR_AirObj *airobj, *coneplat, *plat;
GR_Othb *othb;
GR_Army *army;

Boolean
demoWP (XtPointer)
{
   int i, j, total;
   long type, aid, platid;
   float scale_factor, heading_angle, cone_angle=M_PI_2/2;
   float lat, lon, alt, x0, y0, z0, x, y, z;
   float pv[3], tv[3];
   Demolink demolink0, demolink1, demolink2;
   static Boolean first_demo = TRUE;

   if (first_demo)
   {
     first_demo = FALSE;
     printf ("Starting animation demo.\n");
     demo_displist = new GR_DispList;
     gwindow->addDispList (demo_displist, "demo_displist");
     demo_trail_displist = new GR_DispList;
     gwindow->addDispList (demo_trail_displist, "demo_trail_displist");
   }
   else if (demostate == 0)
   {
     printf ("Starting animation demo again.\n");
     gwindow->addDispList (demo_displist, "demo_displist");
     gwindow->addDispList (demo_trail_displist, "demo_trail_displist");
   }

   if (demostate == 0)
   {
     printf("Demo: Drawing an ORTH-B\n");
     lat = 56*M_PI/180;
     lon = 171*M_PI/180;
     alt = 0.0501;
     x = cos(lat)*sin(lon)*(1.0+alt);
     y = sin(lat)*(1+alt);
     z = cos(lat)*cos(lon)*(1.0+alt);
     lat = 56;
     lon = 171;
     othb = new GR_Othb (1997, 1997, 250, 70, 0.36, 0.06); // an oth-b
     othb->rotate_x (-lat);
     othb->rotate_y (lon);
     othb->translate (x, y, z);
     demo_displist->add_object (othb);

     printf("Demo: Drawing a Dome\n");
     lat = 52*M_PI/180;
     lon = -90*M_PI/180;
     alt = 0.0501;
     printf("lat = %f, lon = %f\n", lat, lon);
     printf("COS(lat) = %f, COS(lon) = %f, SIN(lat) = %f, SIN(lon) = %f\n",
		cos(lat), cos(lon), sin(lat), sin(lon) );
     x = cos(lat)*sin(lon)*(1.0+alt);
     y = sin(lat)*(1+alt);
     z = cos(lat)*cos(lon)*(1.0+alt);
     lat = 52;
     lon = -90;
     printf("x = %f, y = %f, z = %f\n", x,y,z);
     float values[10];
     values[0] = 0.2;
     sensor = new GR_Sensor (1998, 1998, 1, -94.5, 48.5, values); // a dome
     sensor->translate (x/RE, y/RE, z/RE);
     sensor->rotate_x (-lat);
     sensor->rotate_y (lon);
     sensor->scale(10,10,10);
     demo_displist->add_object (sensor);

     printf("Demo: Drawing a Disk\n");
     lat = 32*M_PI/180;
     lon = -100*M_PI/180;
     alt = 0.0501;
     x = cos(lat)*sin(lon)*(1.0+alt);
     y = sin(lat)*(1+alt);
     z = cos(lat)*cos(lon)*(1.0+alt);
     lat = 32;
     lon = -100;
     values[0] = 0.02;
     sensor = new GR_Sensor (1999, 1999, 200, 240, 0.25, values); //a disk
     sensor->rotate_x (-lat);
     sensor->rotate_y (lon);
     sensor->translate (x, y, z);
     demo_displist->add_object (sensor);

     printf("Demo: Drawing an Army\n");
     lat = 32*M_PI/180;
     lon = -120*M_PI/180;
     alt = 0.0501;
     x = cos(lat)*sin(lon)*(1.0+alt);
     y = sin(lat)*(1+alt);
     z = cos(lat)*cos(lon)*(1.0+alt);
     lat = 32;
     lon = -120;
     army = new GR_Army (1991); //a army
     sensor->translate (x, y, z);
     sensor->rotate_x (-lat);
     sensor->rotate_y (lon);
     demo_displist->add_object (army);
     pv[0] = x;
     pv[1] = y;
     pv[2] = z;

     for (i=0, aid=4000; i<3; i++, aid++)
     {
       if (i==0)
       {
          lat = 28*M_PI/180;
          lon = -80*M_PI/180;
       }
       else if (i==1)
       {
          lat = 26*M_PI/180;
          lon = -84*M_PI/180;
       }
       else if (i==2)
       {
          lat = 24*M_PI/180;
          lon = -80*M_PI/180;
       }
       alt = 0.02;
       x = cos(lat)*sin(lon)*(1+alt);
       y = sin(lat)*(1+alt);
       z = cos(lat)*cos(lon)*(1+alt);
       type = 51;    // an F15
       airobj = new GR_AirObj (aid, type);
       airobj->scale (0.0003, 0.0003, 0.0003);
       airobj->rotate_z (180);
       airobj->rotate_x (90);
       if (i==0)
       {
          lat = 28;
          lon = -80;
       }
       else if (i==1)
       {
          lat = 26;
          lon = -84;
       }
       else if (i==2)
       {
          lat = 24;
          lon = -80;
       }
       heading_angle = -90; // this angle may be modified....
       airobj->set_xyz (x, y, z);
       airobj->set_llah (lat, lon, alt, heading_angle);
       airobj->rotate_z (-heading_angle);
       airobj->rotate_x (-lat);
       airobj->rotate_y (lon);
       airobj->translate (x,y,z);
       demo_displist->add_object (airobj);

       if (i == 0)
	 airobj->add_trail (demo_trail_displist, 255, 255, 0);
       else if (i == 1)
	 airobj->add_trail (demo_trail_displist, 0, 0, 2550);
       else
	 airobj->add_trail (demo_trail_displist, 255, 0, 255);

	 

     }

     printf("Demo: Drawing an Air object\n");
     lat = 40*M_PI/180;
     lon = -102*M_PI/180;
     alt = 0.15;
     x0 = cos(lat)*sin(lon)*(1+alt);
     y0 = sin(lat)*(1+alt);
     z0 = cos(lat)*cos(lon)*(1+alt);
     type = 68; // 20: Full Sensor, 51:F15, 68:AWAC;
     platid = 3000;
     coneplat = new GR_AirObj (platid, type);
     demo_displist->add_object (coneplat);
     coneplat->add_trail (demo_trail_displist, 255, 0, 0);
     scale_factor = get_scale (type);
     coneplat->scale(scale_factor, scale_factor, scale_factor);
     if (type==69 || type==104 || type==55 || type==128 || type==51 || type==70
       || type == 63 || type==20)
     {
        coneplat->rotate_z (180);
        coneplat->rotate_x (90);
     }
     else
     {
        coneplat->rotate_y (90);
        coneplat->rotate_x (180);
     }
     lat = 40;
     lon = -102;
     heading_angle = -55; // this angle may be modified....
     coneplat->set_xyz (x0, y0, z0);
     coneplat->set_llah (lat, lon, alt, heading_angle);
     coneplat->rotate_z (-heading_angle);
     coneplat->rotate_x (-lat);
     coneplat->rotate_y (lon);
     coneplat->translate (x0,y0,z0);

     cone = new GR_Sensor (platid, 2800, 15, 45);
     plat = (GR_AirObj*)demo_displist->retrieve_object (platid);
     lat = plat->get_lat();
     lon = plat->get_lon();
     alt = plat->get_alt();
     heading_angle = plat->get_heading();
     x = plat->get_x();
     y = plat->get_y();
     z = plat->get_z();
     cone->rotate_z (-heading_angle);
     cone->rotate_x (-lat);
     cone->rotate_y (lon);
     cone->translate (x, y, z);
     coneplat->add_sobj (cone);
     demo_displist->add_object (cone);

     printf("Demo: Drawing a Torus\n");
     torus = new GR_Sensor (platid, 2801, 0, 30, 0.08);
     plat = (GR_AirObj*)demo_displist->retrieve_object (platid);
     lat = plat->get_lat();
     lon = plat->get_lon();
     alt = plat->get_alt();
     heading_angle = plat->get_heading();
     x = plat->get_x();
     y = plat->get_y();
     z = plat->get_z();
     torus->rotate_z (-heading_angle);
     torus->rotate_x (-lat);
     torus->rotate_y (lon);
     torus->translate (x, y, z);
     coneplat->add_sobj (torus);
     demo_displist->add_object (torus);
   }
   else if (demostate > 0) // ???? to be determined....
   {
     if (coneplat)
     {
        coneplat->reset();
	scale_factor = get_scale (coneplat->get_type ());
        coneplat->scale(scale_factor, scale_factor, scale_factor);
        coneplat->translate (0, 0.02, 0);
        coneplat->rotate_y (90);
        coneplat->rotate_x (180);
     }
     else
     {
	printf ("... coneplat not existing? -> internal error in demo.C\n");
	return TRUE;
     }
     
     if (demostate<=20)
     {
        lat = (40 + demostate*0.5)*M_PI/180;
        lon = (-102 - demostate*1.0)*M_PI/180;
     }
     else if (demostate<=40)
     {
        lat = 50*M_PI/180;
        lon = (-122 + (demostate-20)*2.0)*M_PI/180;
     }
     else if (demostate<=60)
     {
        lat = (50 - (demostate-40)*0.5)*M_PI/180;
        lon = (-82 - (demostate-40)*1.0)*M_PI/180;
     }
     alt = 0.12;
     x0 = cos(lat)*sin(lon)*(1+alt);
     y0 = sin(lat)*(1+alt);
     z0 = cos(lat)*cos(lon)*(1+alt);
     if (demostate<=20)
     {
        lat = 30 + demostate*0.5;
        lon = -102 - demostate*1.0;
        heading_angle = -55;
     }
     else if (demostate<=40)
     {
        lat = 40;
        lon = -122 + (demostate-20)*2.0;
        heading_angle = 90;
     }
     else if (demostate<=60)
     {
        lat = 40 - (demostate-40)*0.5;
        lon = -82 - (demostate-40)*1.0;
        heading_angle = -125;
     }     
     coneplat->set_xyz (x0, y0, z0);
     coneplat->set_llah (lat, lon, alt, heading_angle);
     coneplat->rotate_z (-heading_angle);
     coneplat->rotate_x (-lat);
     coneplat->rotate_y (lon);
     coneplat->translate (x0,y0,z0);

     coneplat->add_trail_point (x0, y0, z0);  // trail points;
   
     total = coneplat->get_total_sensors();
     if (total > 0)
     {
        lat = coneplat->get_lat();
        lon = coneplat->get_lon();
        alt = coneplat->get_alt();
        heading_angle = coneplat->get_heading();
        x = coneplat->get_x();
        y = coneplat->get_y();
        z = coneplat->get_z();
        for (j=0; j<total; j++)
        {
           //printf (".. Updating one mobile sensor..,. total %d.\n", total);
           cone = coneplat->get_sobj(j);
           cone->reset ();
           cone->rotate_z (-heading_angle);
           cone->rotate_x (-lat);
           cone->rotate_y (lon);
           cone->translate (x,y,z);
        }
     }
      // fly the F15's now:
     for (i=0, aid=4000; i<3; i++, aid++)
     {
       airobj = (GR_AirObj*)demo_displist->retrieve_object(aid);
       airobj->reset();
       airobj->scale (0.0003, 0.0003, 0.0003);
       airobj->rotate_z (180);
       airobj->rotate_x (90);
       lat = airobj->get_lat();
       lon = airobj->get_lon();
       alt = airobj->get_alt();
       heading_angle = airobj->get_heading();

       lat = lat*M_PI/180;
       lon = (lon-6.0)*M_PI/180;
       x = cos(lat)*sin(lon)*(1+alt);
       y = sin(lat)*(1+alt);
       z = cos(lat)*cos(lon)*(1+alt);
       tv[0] = x;
       tv[1] = y;
       tv[2] = z;      
 
       lat = lat*180/M_PI;
       lon = lon*180/M_PI; 

       airobj->set_xyz (x, y, z);
       airobj->set_llah (lat, lon, alt, heading_angle);
       airobj->rotate_z (-heading_angle);
       airobj->rotate_x (-lat);
       airobj->rotate_y (lon);
       airobj->translate (x,y,z);

       if (i==0)
	 airobj->add_trail_point (x, y, z);
       else if (i==1)
       {
	  if (demostate % 4)
	    airobj->add_trail_point (x, y, z);

	  if (demostate % 8)
	    airobj->trail_on ();

	  if ((demostate+4) % 8)
	    airobj->trail_off ();
       }
       else
	 if (demostate % 8)
	   airobj->add_trail_point (x, y, z);

       //printf ("A printf just to slow things down.....\n");
       //sleep(1);
    
       if (demostate==1)
       {
          if (aid == 4000)  
          {
             demo_displist->add_object(demolink0);
             demolink0.setlink (pv, tv);
          }
          else if (aid == 4001)
          {
             demo_displist->add_object(demolink1);
             demolink1.setlink (pv, tv);
          }
       }
       else if (demostate>=2 && demostate<=5)
       {
          if (aid == 4000)
          {
	     demolink0.setlink (pv, tv);
          }
          else if (aid==4001)
          {
             demolink1.setlink (pv, tv);
          }
          else if (aid==4002)
          { 
             if (demostate==2)
	       demo_displist->add_object(demolink2);
	     demolink2.setlink (pv, tv);
          }
       }
       else if (demostate==6)
       {
          if (aid==4000)
             demo_displist->delete_object(&demolink0);
          if (aid==4001)
             demo_displist->delete_object(&demolink1);
          if (aid==4002)
             demo_displist->delete_object(&demolink2);
       }

     }
      
   }

   gwindow->draw();
   demostate++;
   printf(" demostate = %d\n.", demostate);
   if (demostate > MAXdemostate)
   {
      printf (" demo done.\n");
      return TRUE;
   }
   else
      return FALSE; 
}

