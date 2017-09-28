/**********************************************************
  The army object.

  -- 3/5/93: created by Y. Tung;

**********************************************************/

#include "GR_Army.H"
#include "malloc.h"
#include "math.h"
#include "def.H"
#include <Xm/MessageB.h>


/* ---------------------- */

GR_Army::GR_Army (long armytype, float latitude, float longitude, float size)
{
float          vert[3];
float          lon, lat;
float          factor = 1.00;               // to compensate for normal lines to show

   p_armytype = armytype;
   p_ulen = size/RE; //0.025;   // 0.002

   p_base[0] = 0;
   p_base[1] = 0;
   p_lupper[0] = 0;
   p_lupper[1] = p_ulen;
   p_rupper[0] = 1.65*p_ulen;
   p_rupper[1] = p_ulen;
   p_rlower[0] = 1.65*p_ulen;
   p_rlower[1] = 0;

   p_topline_base[0] = 0.6*p_ulen;
   p_topline_base[1] = 1.1*p_ulen;
  
   p_gr_objid = GR_genobj ();
   GR_makeobj (p_gr_objid);
      GR_pushattributes();

      lat = latitude*M_PI/180;
      lon = longitude*M_PI/180;
      vert[0] = cos(lat) * sin(lon) * factor;
      vert[1] = sin(lat) * factor;
      vert[2] = cos(lat) * cos(lon) * factor;

      glRotatef(longitude, 0.0, 1.0, 0.0);       // Rotate to correct longitude from Greenwich
      glRotatef(-latitude, 1.0, 0.0, 0.0);       // Rotate to correct latitude from Equator
    //glRotatef(180.0-p_orient, 0.0, 0.0, 1.0);  // Rotate to correct orientation from North
      glTranslatef(0.0, 0.0, 1.0);               // Move to correct location on surface

      GR_linewidth (2*GR_getlwidth());
      GR_color (255, 255, 0);

      draw_box ();
      //draw_inner ();
      //draw_top ();

      GR_popattributes();      
   GR_closeobj();
}


void
GR_Army::draw_box ()
{  
   GR_bgnline();
   GR_v2f(p_base);
   GR_v2f(p_lupper);
   GR_v2f(p_rupper);
   GR_v2f(p_rlower);
   GR_v2f(p_base);
   GR_endline();
}

void
GR_Army::draw_inner ()
{
     switch (p_armytype) {
       case 1:
	 draw_cross ();
	 draw_round ();
	 break;
       case 2:
	 draw_cross ();
	 draw_round ();
	 break;
       case 3:
	 draw_cross ();
	 draw_eyes ();
	 break;
       case 4:
	 draw_round ();
	 break;
       case 5:
	 draw_cross ();
	 draw_round ();
	 break;
       case 6:
	 draw_line ();
	 draw_round ();
	 break;
       case 7:
	 draw_line ();
	 draw_round ();
	 break;
       case 8:
	 draw_chopper ();
	 break;
       case 9:
	 draw_cross ();
	 draw_bird ();
	 break;
       case 10:
	 draw_cross ();
	 draw_bird ();
	 break;
       case 11:
	 draw_cross ();
	 draw_y ();
	 break;
       default:
	 draw_cross ();
	 draw_round ();
	 break;
     }
}


void
GR_Army::draw_top ()
{
   float base[2], lupper[2], mlower[2], mupper[2], rlower[2], rupper[2];
   // define six points for XX or |||

   base[0] = p_topline_base[0];
   base[1] = p_topline_base[1];
   lupper[0] = base[0];
   lupper[1] = base[1] + 0.3*p_ulen;
   mlower[0] = base[0] + 0.25*p_ulen;
   mlower[1] = base[1];
   mupper[0] = mlower[0];
   mupper[1] = lupper[1];
   rlower[0] = base[0] + 0.5*p_ulen;
   rlower[1] = base[1];
   rupper[0] = rlower[0];
   rupper[1] = lupper[1];

   if (p_armytype==1 ||
       p_armytype==6 ||
       p_armytype==8 ||
       p_armytype==9)       
   {                    // a single X
      GR_bgnline ();
      GR_v2f (base);
      GR_v2f (mupper);
      GR_endline ();

      GR_bgnline ();
      GR_v2f (lupper);
      GR_v2f (mlower);
      GR_endline ();
   }
   else if (p_armytype==2)
   {                  // a II
      GR_bgnline ();
      GR_v2f (base);
      GR_v2f (lupper);
      GR_endline ();

      GR_bgnline ();
      GR_v2f (mupper);
      GR_v2f (mlower);
      GR_endline ();      
   }
   else if (p_armytype==3 ||
	    p_armytype==4 ||
	    p_armytype==5 ||
	    p_armytype==10 ||
	    p_armytype==11)
   {                  // a XX
      GR_bgnline ();
      GR_v2f (base);
      GR_v2f (mupper);
      GR_endline ();
      
      GR_bgnline ();
      GR_v2f (lupper);
      GR_v2f (mlower);
      GR_endline ();
      
      GR_bgnline ();
      GR_v2f (mlower);
      GR_v2f (rupper);
      GR_endline ();
      
      GR_bgnline ();
      GR_v2f (mupper);
      GR_v2f (rlower);
      GR_endline ();
   }
   else if (p_armytype==7)
   {                  // a III
      GR_bgnline ();
      GR_v2f (base);
      GR_v2f (lupper);
      GR_endline ();

      GR_bgnline ();
      GR_v2f (mupper);
      GR_v2f (mlower);
      GR_endline ();
      
      GR_bgnline ();
      GR_v2f (rupper);
      GR_v2f (rlower);
      GR_endline ();
   }
}


void
GR_Army::objdraw ()
{
  GR_callobj (p_gr_objid);
}



/* ======== */

void
GR_Army::draw_cross ()
{
   GR_bgnline ();
   GR_v2f (p_base);
   GR_v2f (p_rupper);
   GR_endline ();
   
   GR_bgnline ();
   GR_v2f (p_lupper);
   GR_v2f (p_rlower);
   GR_endline ();
}

void
GR_Army::draw_round ()
{
   float center[2];
   float delta, radius;
   float base[2], lupper[2], rupper[2], rlower[2];
   
   center[0] = 0.5*(p_rupper[0] + p_lupper[0]);
   center[1] = 0.5*(p_rupper[1] + p_rlower[1]);
   delta = 0.25*(p_rupper[1] - p_rlower[1]);
   radius = 1.4142134 * delta;

   GR_arc (center[0], center[1], radius, 1350, 2250);
   GR_arc (center[0], center[1], radius, -450, 450);

   base[0] = center[0] - delta;
   base[1] = center[1] - delta;

   lupper[0] = base[0];
   lupper[1] = center[1] + delta;

   rupper[0] = center[0] + delta;
   rupper[1] = lupper[1];

   rlower[0] = rupper[0];
   rlower[1] = base[1];

   GR_bgnline ();
   GR_v2f (lupper);
   GR_v2f (rupper);
   GR_endline ();

   GR_bgnline ();
   GR_v2f (base);
   GR_v2f (rlower);
   GR_endline ();
}

void
GR_Army::draw_line ()
{
   GR_bgnline ();
   GR_v2f (p_base);
   GR_v2f (p_rupper);
   GR_endline ();
}

void
GR_Army::draw_chopper ()
{
   float clower[2], cupper[2], llower[2], lupper[2], rlower[2], rupper[2];
   float delta_x, delta_y;
   int old_linewidth;

   clower[0] = 0.5*(p_base[0] + p_rlower[0]);
   clower[1] = 0.8*p_base[1] + 0.2*p_lupper[1];
   cupper[0] = clower[0];
   cupper[1] = 0.2*p_base[1] + 0.8*p_lupper[1];

   old_linewidth = (int)GR_getlwidth ();
   GR_linewidth (4);
   GR_bgnline ();
   GR_v2f (clower);
   GR_v2f (cupper);
   GR_endline ();
   GR_linewidth (old_linewidth);

   delta_x = 0.4*(p_rlower[0] - p_base[0]);
   delta_y = 0.1*(p_lupper[1] - p_base[1]);
   llower[0] = cupper[0] - delta_x;
   llower[1] = cupper[1] - delta_y;
   lupper[0] = llower[0];
   lupper[1] = cupper[1] + delta_y;
   
   GR_bgntmesh ();
   GR_v2f (cupper);
   GR_v2f (llower);
   GR_v2f (lupper);   
   GR_endtmesh ();

   rlower[0] = cupper[0] + delta_x;
   rlower[1] = llower[1];
   rupper[0] = rlower[0];
   rupper[1] = lupper[1];
	 
   GR_bgntmesh ();
   GR_v2f (cupper);
   GR_v2f (rlower);
   GR_v2f (rupper);
   GR_endtmesh ();
}

void
GR_Army::draw_bird ()
{
   float center_a[2], center_b[2], radius;

   center_a[0] = 0.67*p_base[0] + 0.33*p_rlower[0];
   center_a[1] = p_base[1] - 0.25*(p_lupper[1] - p_base[1]);
   center_b[0] = 0.33*p_base[0] + 0.67*p_rlower[0];
   center_b[1] = center_a[1];
   radius = 0.3*(p_lupper[1] - p_base[1]);

   GR_arc (center_a[0], center_a[1], radius, 600, 1200);
   GR_arc (center_b[0], center_b[1], radius, 600, 1200);
}

void
GR_Army::draw_y ()
{
   float center[2], clower[2], llower[2], rlower[2], lupper[2], rupper[2];
   int old_linewidth;
   
   center[0] = 0.5*(p_base[0] + p_rlower[0]);
   center[1] = 0.5*(p_base[1] + p_lupper[1]);

   clower[0] = center[0];
   clower[1] = 0.67*p_base[1] + 0.33*center[1];

   llower[0] = 0.2*p_base[0] + 0.8*center[0];
   llower[1] = clower[1];

   rlower[0] = 0.2*p_rlower[0] + 0.8*center[0];
   rlower[1] = clower[1];

   lupper[0] = 0.3*p_base[0] + 0.7*center[0];
   lupper[1] = 0.3*center[1] + 0.7*p_lupper[1];

   rupper[0] = 0.3*p_rlower[0] + 0.7*center[0];
   rupper[1] = lupper[1];

   old_linewidth = (int)GR_getlwidth ();
   GR_linewidth (3);   
   GR_bgnline ();
   GR_v2f (center);
   GR_v2f (clower);
   GR_endline ();

   GR_bgnline ();
   GR_v2f (center);
   GR_v2f (lupper);
   GR_endline ();

   GR_bgnline ();
   GR_v2f (center);
   GR_v2f (rupper);
   GR_endline ();

   GR_linewidth (old_linewidth);
   GR_bgnline ();
   GR_v2f (llower);
   GR_v2f (rlower);
   GR_endline ();
}

void
GR_Army::draw_eyes ()
{
   float center_a[2], center_b[2];
   float delta, radius;
   float llower[2], lupper[2], rlower[2], rupper[2];

   center_a[0] = 0.25*p_base[0] + 0.75*p_rlower[0];
   center_a[1] = 0.5*(p_base[1] + p_lupper[1]);
   center_b[0] = 0.75*p_base[0] + 0.25*p_rlower[0];
   center_b[1] = center_a[1];
   
   delta = 0.1*(p_lupper[1] - p_base[1]);
   radius = delta;

   GR_arc (center_a[0], center_a[1], radius, 901, 2699);
   GR_arc (center_b[0], center_b[1], radius, 2701, 899);

   llower[0] = center_a[0];
   llower[1] = center_a[1] - delta;

   lupper[0] = center_a[0];
   lupper[1] = center_a[1] + delta;

   rlower[0] = center_b[0];
   rlower[1] = llower[1];

   rupper[0] = center_b[0];
   rupper[1] = lupper[1];

   GR_bgnline ();
   GR_v2f (lupper);
   GR_v2f (rlower);
   GR_endline ();

   GR_bgnline ();
   GR_v2f (llower);
   GR_v2f (rupper);
   GR_endline ();
}

