/*********************************************************************

  12/07/92, Tung: create new file from GR_Lines.C;

*********************************************************************/

#include <stdio.h>
#include <math.h>
#include <malloc.h>
#include "GR_2Dlines.H"

GR_2Dlines::GR_2Dlines (short R, short G, short B, char* filename, long type)
{
   float vec[2];
   long i,j;

   if (filename == NULL)
     p_filename = "../../data/World.bin";
   else
     p_filename = filename;

   p_type = type;
 
   p_R = R;
   p_G = G;
   p_B = B;
   
   read_lines ();
   
   p_gr_objid = GR_genobj ();
   GR_makeobj (p_gr_objid);
   for (i=0; p_polycount[i] != 0; i++)
   {
     GR_bgnline ();
     for (j=0; j<p_polycount[i]; j++)
     {
        vec[0] = p_polylines[i][j].lon/M_PI;
        vec[1] = p_polylines[i][j].lat/M_PI_2;
        GR_v2f(vec);
     }
     GR_endline ();
   }
   GR_closeobj ();
}

void
GR_2Dlines::read_lines ()
{
   FILE *fileptr;
   long count, totalcount;
   int thiscount;
   char str[80];


   fileptr = fopen (p_filename, "rb");
   if (fileptr == NULL)
   {
      perror ("GR_2Dlines::read_lines' fopen");
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
GR_2Dlines::objdraw ()
{
   GR_color (p_R, p_G, p_B);
   GR_callobj (p_gr_objid);
}
	








	     





