/*********************************************************

  Platform--targets links, good for Speedes;
  -- created by Y. Tung, 12/14/92;
  -- 08/24/93: got rid of malloc;
 
**********************************************************/

#include "GR_Spslinks.H"
#include <math.h>
#include "GR_DispList.H"


/* ----------- */
GR_Spslinks::GR_Spslinks (int num_pairs)
{
   if (num_pairs > 0) {
     p_array_ptr = new data_pair[num_pairs];
   }else{
     p_array_ptr = NULL;
   }

   p_total_pairs = num_pairs;
   for (int i=0; i<p_total_pairs; i++)
   {
      for (int j=0; j<3; j++)
      {
         p_array_ptr[i].ploc[j] = 0.0;
         p_array_ptr[i].tloc[j] = 0.0;
      }
      p_array_ptr[i].rgb[0] = 255;  // default link color is yellow;
      p_array_ptr[i].rgb[1] = 255;
      p_array_ptr[i].rgb[2] = 0;
   }
}

GR_Spslinks::~GR_Spslinks() {

  if (p_array_ptr != NULL) delete p_array_ptr;

}

void
GR_Spslinks::add_link (int index, double* ploc, double* tloc)
{
   int i;
   
   if (index>=0 && index <= p_total_pairs-1)
   {
      for (i=0; i<3; i++)
      {
	 p_array_ptr[index].ploc[i] = ploc[i];
	 p_array_ptr[index].tloc[i] = tloc[i];
      }
   }
   else
     printf ("Warning: array index %d out of bound, [0..%d]???\007\n",
	     index, p_total_pairs-1);
}

void
GR_Spslinks::add_link (int index, double* ploc, double* tloc, short* rgb)
{
   int i;
  
   if (index>=0 && index <= p_total_pairs-1)
   {
      for (i=0; i<3; i++)
      {
         p_array_ptr[index].ploc[i] = ploc[i];
         p_array_ptr[index].tloc[i] = tloc[i];
         p_array_ptr[index].rgb[i] = rgb[i];
      }
   }
   else
     printf ("Warning: array index %d out of bound, [0..%d]???\007\n",
             index, p_total_pairs-1);
}


void
GR_Spslinks::objdraw ()
{
   int i, j;
   double ploc[3], tloc[3];
   short rgb[3];

   GR_pushattributes ();
   //linewidth (getlwidth()*2);
   //GR_color (255, 255, 0);

   for (j=0; j<p_total_pairs; j++)
   {
      for (i=0; i<3; i++)
      {
	 ploc[i] = p_array_ptr[j].ploc[i];
	 tloc[i] = p_array_ptr[j].tloc[i];
         rgb[i] = p_array_ptr[j].rgb[i];
      }
      GR_RGBcolor (rgb[0], rgb[1], rgb[2]);
      GR_bgnline ();
      GR_v3d (ploc);
      GR_v3d (tloc);
      GR_endline ();
   }
   GR_popattributes ();
}

