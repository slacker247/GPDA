/* misc.C */
#include "misc.H"

/* --------- get_type ---- */
char* get_graphics_type_string (long type);

char*
get_type_string (long id, long type)
{
   return get_graphics_type_string (type);
}

char*
get_graphics_type_string (long type)
{
   if (type==37)
     return "Impact";
   else if (type==49)
     return "Scud Launcher";
   else if (type==51)
     return "F15";
   else if (type==55)
     return "Backfire";
   else if (type==56)
     return "Bear";
   else if (type==58)
     return "ALCM";
   else if (type==68)
     return "AWACS";
   else if (type==69)
     return "Commercial";
   else if (type==70)
     return "MIG";
   else if (type==97)
     return "Prop Plane";
   else if (type==98)
     return "Jet Plane";
   else
     return "";
}


/* ------------------------------- IDlist ----------------- */

/* RVI 7/28
IDlist::IDlist (long size)
{
   int i;
   
   p_idlist = (long*)malloc(sizeof(long)*size);
   p_maxsize = size;
   p_size = 0;
   p_curr = 0;

   for (i=0; i<p_maxsize; i++)
   {
      p_idlist[i] = -1;
   }
}

IDlist::~IDlist ()
{
   if (p_idlist)
     free (p_idlist);
}

Boolean
IDlist::in_list (long id)
{
   int i;
   Boolean ans = FALSE;

   for (i=0; (i<p_size && ans == FALSE); i++)
   {
      if (id == p_idlist[i])
	ans = TRUE;
   }
   return ans;
}

void
IDlist::put_list (long id)
{
   if (p_curr < p_maxsize)
   {
      p_idlist[p_curr] = id;
      p_size++;
      p_curr++;
   }
   else
   {
      p_curr = 0;  // overwrite from the beginning;
      p_idlist[p_curr] = id;
   }
}

void
IDlist::rm_list (long id)  // remove the first element in list that has this id:
{
   int i;
   Boolean done = FALSE;

   for (i=0; (i<p_size && done == FALSE); i++)
   {
      if (id == p_idlist[i])
      { 
         if (i != p_size-1)
         {
            p_idlist[i] = p_idlist[p_size-1];
         }
         p_idlist[p_size-1] = -1;
         if (p_curr >= p_size)  // i.e., when no overwrite happened;
           p_curr --;
         p_size--;
         done = TRUE;
      }
   }
}

void
IDlist::clean_list ()
{
   int i;
   for (i=0; i<p_size; i++)
   {
      p_idlist[i] = -1;
   }
   p_size = 0;
   p_curr = 0;
}
   
void
IDlist::print_list()
{
   int i;
   printf ("IDlist size=%d; list: ", p_size);
   for (i=0; i<p_size; i++)
   {
      printf (" %d", p_idlist[i]);
   }
   printf ("\n");
}

*/
