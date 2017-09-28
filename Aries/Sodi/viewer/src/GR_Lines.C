/*********************************************************************

  11/06/92, Tung: Added work dialog to report work progress.
  03/19/93: making extra line segments for long border lines;

*********************************************************************/

#include <stdio.h>
#include <math.h>
#include <malloc.h>
#include "GR_Lines.H"


GR_Lines::GR_Lines (short R, short G, short B, char* filename, long type)
{
   float vec[3], lon, lat, old_lon, old_lat, dist_sqr, extra_lon, extra_lat;
   float factor = 1.00; // to compensate for normal lines to show
   //float factor2 = 1.015;  // to compensate for long lines to show
   float threshold = 0.015;
   int num_extra_pts;
   long i,j,k;
   FILE *fileptr;
   long count, totalcount;
   int thiscount, lineno;
   char str[80];

   if (filename == NULL)
     p_filename = "./RSD_Data/World.asc";
   else
     p_filename = filename;
   fileptr = fopen (p_filename, "r+");
   if (fileptr == NULL) {
      perror ("GR_Lines: Error opening boundary file.");
      return;
   }

   p_type = type;
 
   p_R = R;
   p_G = G;
   p_B = B;
   
//   read_lines ();
  
   old_lon = 0;
   old_lat = 0;
 
   p_gr_objid = GR_genobj ();
   GR_makeobj (p_gr_objid);
   fscanf(fileptr, "%d", &count);
   for (i=0; i<count; i++)
   {
     fscanf(fileptr, "%d%d", &lineno, &thiscount);
     GR_bgnline ();
     for (j=0; j<thiscount; j++)
     {
        fscanf(fileptr, "%f%f", &lon, &lat);
         
        // a patch for the lone US border line:
        // between lon (-94.5,-123.5) and lat (48.5, 49.5),
        // Alaska between lon (-135,-145) and lat (60, 70), and
        // N. Afr. lines between lon (-15, 40) and lat (15,35)
        //
        if ( (j > 0) &&
             ( ((lon > -2.15547) && (lon < -1.64933)
               && (lat > 0.84648) && (lat < 0.86393)) ||
               ((lon < -2.3561944) && (lon > -2.5307371)
               && (lat > 1.047197) && (lat < 1.2217303)) ||
               ((lon > -0.2617992) && (lon < 0.6981316)
               && (lat > 0.2617992) && (lat < 0.610865))
             )
            ) 
        {
	   dist_sqr = (lon - old_lon) * (lon - old_lon) +
	     (lat - old_lat) * (lat - old_lat);
	   num_extra_pts = (int)(dist_sqr / threshold);
           if (num_extra_pts > 0)
           {
	     if (INFOfp != NULL)
		    fprintf (INFOfp, 
             "... making %d extra points from old_lon=%f, old_lat=%f, to lon=%f, lat=%f ...\n",
			 num_extra_pts, old_lon*180/M_PI, old_lat*180/M_PI,
			 lon*180/M_PI, lat*180/M_PI);
           }

	   for (k=1; k<=num_extra_pts;k++)
	   {
	      extra_lon = lon * k / (num_extra_pts + 1.0) +
		old_lon * (num_extra_pts + 1.0 - k) / (num_extra_pts + 1.0);
	      extra_lat =  lat * k / (num_extra_pts + 1.0) +
		old_lat * (num_extra_pts + 1.0 - k) / (num_extra_pts + 1.0);
	      vec[0] = cos(extra_lat) * sin(extra_lon) * factor;
	      vec[1] = sin(extra_lat) * factor;
	      vec[2] = cos(extra_lat) * cos(extra_lon) * factor;
	      GR_v3f (vec);
	      if (INFOfp != NULL)
	      	 fprintf (INFOfp, "   extra point at: lon=%f, lat=%f\n",
                      extra_lon*180/M_PI, extra_lat*180/M_PI); 
            }
           
        }
	old_lon = lon;
	old_lat = lat;
 
	vec[0] = cos(lat) * sin(lon) * factor;
	vec[1] = sin(lat) * factor;
	vec[2] = cos(lat) * cos(lon) * factor;
        
        GR_v3f(vec);
     }
     GR_endline ();
   }
   GR_closeobj ();
}

void
GR_Lines::read_lines ()
{
   FILE *fileptr;
   long count, totalcount;
   int thiscount;
   char str[80];


   fileptr = fopen (p_filename, "rb");
   if (fileptr == NULL)
   {
      perror ("GR_lines::read_lines' fopen");
      return;
   }

   count = 0;
   totalcount= 0;
   while (fread (&thiscount, sizeof (int), 1, fileptr))
   {
      totalcount += thiscount;
      p_polycount[count] = thiscount;

      p_polylines[count] = (struct point*) 
                           malloc (thiscount * sizeof(struct point)); 


      fread (p_polylines[count], sizeof(struct point), thiscount, fileptr);

      count++;
      p_polycount[count] = 0; // as a terminator
   }
   sprintf (str, " ... total %d vertices read...");
   printf ("%s\n", str);
}

void
GR_Lines::objdraw ()
{
   GR_color (p_R, p_G, p_B);
   glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
   glColor3f(1.0, 1.0, 1.0);
   glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
   GR_callobj (p_gr_objid);
}
	
GR_Gridlines::GR_Gridlines (short R, short G, short B, int degree, long type)
{
   int i, j;
   int gridnumber;
   float theta, phi;
   float rad = 1.000;
   float v[3];    
 
   if (degree < 5)
      degree = 5;
   else if (degree > 90)
      degree = 90;
   gridnumber = 180/degree;
   theta = degree * M_PI / 180; 

   p_type = type;
   p_gr_objid = GR_genobj ();
   GR_makeobj (p_gr_objid);
   GR_color (R,G,B);
   glColor3f(0.5, 0.5, 0.0);
   for (i=0; i<gridnumber; i++)
   {
      GR_pushmatrix ();
      GR_rotate (i*degree*10, 'y');
      GR_circ (0.0, 0.0, rad);
      GR_popmatrix ();
   }

   GR_pushmatrix ();
   for (i=0; i<(gridnumber/2); i++)
   {
     GR_bgnline();
     for (j=0, phi=0.0; j<=36; j++, phi+=M_PI/18)
     {
        v[0]=cos(theta*i)*cos(phi)*rad;
        v[1]=sin(theta*i)*rad;
        v[2]=cos(theta*i)*sin(phi)*rad;
        GR_v3f(v);
     }
     GR_endline();     
     GR_bgnline();
     for (j=0, phi=0.0; j<=36; j++, phi+=M_PI/18)
     {
        v[0]=cos(theta*i)*cos(phi)*rad;
        v[1]=-sin(theta*i)*rad;
        v[2]=cos(theta*i)*sin(phi)*rad;
        GR_v3f(v);
     }
     GR_endline();      
   }
   GR_popmatrix ();

   GR_closeobj ();
}

void
GR_Gridlines::objdraw ()
{
   GR_callobj (p_gr_objid);
}

