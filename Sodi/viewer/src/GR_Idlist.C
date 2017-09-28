/********************************************************
 
  GR_Idlist.C
  08/24/93: created by Y. Tung

*********************************************************/

#include "GR_Idlist.H"


IDlist::IDlist (long size)
{
   int i;
   
   //p_idlist = (long*)malloc(sizeof(long)*size);
   p_idlist = new long[size];
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
     //free p_idlist;
     delete p_idlist;
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


