/**********************************************************
  Platform--targets links. 
  -- 11/16/92: created by Y. Tung;
  -- 12/07/92: changed displist to adv_displist -- so that
     this GR_Links only does things for ASF; this may 
     be fixed later;
  -- 12/11/92: used newly added field, GR_DispList *p_search_list,
               to replace hard coded displist;
  -- 04/27/93: make GR_Links a GR_DispObj so that it can be added
               to a displist;
  -- 07/20/93: comment out print statements i
               "Warning: link a non-exis. [sensor,target]...";
	       
**********************************************************/

#include "GR_Links.H"
#include "GR_DispList.H"
//#include "malloc.h"
#include "math.h"

#define OTHB_TYPE 9992


tid_array::tid_array (long pid)
{
   int i;
   
   p_pid = pid;
   p_total_tids = 0;
   for (i=0; i<MAXTARGETS; i++)
     p_tid[i] = -1;
   p_next = NULL;
   p_prev = NULL;
}

void
tid_array::add_tid (long tid)
{
   if (p_total_tids < MAXTARGETS)
   {
      p_tid[p_total_tids] = tid;
      p_total_tids++;
   }
   else
   {
      printf ("Warning: malloc tid_array not implemented yet.\007\n");
   }
}

void
tid_array::delete_tid (long tid)
{
   int i;
   long last;

   if (p_total_tids > 0)
   {
      last = p_tid[p_total_tids-1];
      for (i=p_total_tids-1; i>=0; i--)
      {
	 if (p_tid[i] == tid)
	 {
	    p_tid[i]= last;
	    p_tid[p_total_tids-1] = -1;
	    p_total_tids--;
	    break;
	 }
      }
   }
   else
     printf ("Warning: attempt to delete a tid from an empty tid_array.\n");
}



/* ----------- */
GR_Links::GR_Links (GR_DispList* display_list)
{
   p_search_list = display_list;
   p_head = NULL;
   p_last = NULL;
   p_total_pids = 0;
}

void
GR_Links::add_link (long pid, long tid)
{
   tid_array *tarray;

   tarray = get_tid_array (pid);
   
   if (tarray == NULL)
   {
      tarray = new tid_array (pid);

      if (p_total_pids != 0)
      {
	 p_last->set_next (tarray);
	 tarray->set_prev (p_last);
      }
      else
      {
         p_head = tarray;
      }
      p_last = tarray;
      p_total_pids++;
   }

   tarray->add_tid (tid);
}

void
GR_Links::delete_link (long pid, long tid)
{
   tid_array *tarray, *prev, *next;
      
   tarray = get_tid_array (pid);
   if (tarray != NULL)
   {
      tarray->delete_tid (tid);
      if (tarray->get_total_tids () == 0)
      {
	 if (p_total_pids == 1)
	 {
	    p_head = NULL;
	    p_last = NULL;
	 }
	 else if (tarray == p_head)
	 {
	    next = tarray->get_next ();
	    next->set_prev (NULL);
	    p_head = next;
	 }
	 else if (tarray == p_last)
	 {
	    prev = tarray->get_prev ();
	    prev->set_next (NULL);
	    p_last = prev;
	 }
	 else
	 {
	    next = tarray->get_next ();
	    prev = tarray->get_prev ();
	    next->set_prev (prev);
	    prev->set_next (next);
	 }
	 p_total_pids--;
	 delete tarray;
      }
   }
   else
     printf ("Warning: attempt to delete an non-existing link.\n");
}

tid_array*
GR_Links::get_tid_array (long pid)
{
   int i;
   tid_array *aptr;

   aptr = p_head;
   for (i=0; i<p_total_pids; i++, aptr= aptr->get_next())
   {
      if (aptr->get_pid() == pid)
	return aptr;
   }
   return NULL;
}

void
GR_Links::objdraw ()
{
   draw_links ();
}


void
GR_Links::draw_links ()
{
   int i, j;
   long pid, tid;
   GR_DispObj *pobj, *tobj;
   float pv[3], tv[3];
   tid_array *aptr;

   float midv[3];   // for OTH-B mid point;
   float midv_ratio=1.055; // assume ion level is about 350KM;

   aptr = p_head;

   if (p_search_list != NULL)
   {
     GR_pushattributes ();
     GR_linewidth ((short)GR_getlwidth()*2);
     GR_color (255, 255, 0);

     for (i=1; i<=p_total_pids; i++, aptr=aptr->get_next())
     {
       pid = aptr->get_pid ();
       pobj = p_search_list->retrieve_object (pid);
       if (pobj == NULL)
       {
	  printf (" Warning: link a non-existing sensor (pid=%d)?\n",
		  (pid & 0x000fffff));
	  // no easy way to delete a link for all tid's attached to this pid?
       }
       else 
       {
         pv[0] = pobj->get_x ();
         pv[1] = pobj->get_y ();
         pv[2] = pobj->get_z ();
      
         for (j=0; j<aptr->get_total_tids(); j++)
         {
	    tid = aptr->get_tid (j);
	    tobj = p_search_list->retrieve_object (tid);
            if (tobj == NULL)
	    {
	       //printf (" Warning: link a non-exis. target (tid=%d)?\n",
	       //	       (tid & 0x000fffff));
	       this->delete_link (pid, tid);
	    }
            else
            { 
	       tv[0] = tobj->get_x ();
	       tv[1] = tobj->get_y ();
	       tv[2] = tobj->get_z ();

	       if (pobj->get_type() == OTHB_TYPE) // an OTH-B
	       {
		  midv[0] = (pv[0]+tv[0])*0.5*midv_ratio;
		  midv[1] = (pv[1]+tv[1])*0.5*midv_ratio;
		  midv[2] = (pv[2]+tv[2])*0.5*midv_ratio;
		  GR_bgnline ();
		  GR_v3f (pv);
		  GR_v3f (midv);
		  GR_v3f (tv);
		  GR_endline ();
	       }
	       else  // anything else
	       {
		  GR_bgnline ();
		  GR_v3f (pv);
		  GR_v3f (tv);
		  GR_endline ();
	       }
            }
         }  
       }
     }
     GR_popattributes ();
   }
}

