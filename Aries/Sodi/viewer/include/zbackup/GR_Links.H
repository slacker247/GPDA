/***************************************************************************
  GR_Links: keeps a list of arrays, each array keeps a platform id and its
            associated array of target id's. Locations of all objects are
	    determined by searching the displist. Since this GR_Links is
	    not intended to be included by the displist, it is drawn by
	    a local drawing function.
 
  -- 11/16/92: created by Y. Tung; 
  -- 11/19/92: done with the doubly-linked list of array version;
  -- 12/11/92: added a field, GR_DispList* p_search_list, to GR_Links
               for its "draw_links" method to search through for 
               sensor/target's locations.
 
****************************************************************************/

#ifndef		GRLINKS
#define		GRLINKS

#include  <stdio.h>
#include  "GR_Interface.H"
#include  "GR_DispList.H"
#include  "GR_DispObj.H"

#define MAXTARGETS 32                   // if exceed, need to malloc;

class tid_array
{
 private:
   long p_pid;
   long p_tid[MAXTARGETS];
   tid_array *p_next;
   tid_array *p_prev;

   int p_total_tids;
   
 public:
   tid_array (long pid);
   long get_pid () { return p_pid;}
   long get_tid (int index) { return p_tid[index];}
   void add_tid (long tid);
   void delete_tid (long tid);
   int get_total_tids () { return p_total_tids;}
   tid_array* get_next () { return p_next;}
   void set_next (tid_array* ptr) { p_next = ptr;}
   tid_array* get_prev () { return p_prev;}
   void set_prev (tid_array* ptr) { p_prev = ptr;}

};



class GR_Links: public GR_DispObj
{
 protected:
   tid_array *p_head;
   tid_array *p_last;
   int p_total_pids;
   GR_DispList *p_search_list;
   tid_array* get_tid_array (long pid);

 public:
   GR_Links (GR_DispList* display_list);
   void add_link (long pid, long tid);
   void delete_link (long pid, long tid);
   void draw_links ();
   void draw_link (long pid, long tid) {  };
   int get_total_pids () { return p_total_pids;}

   void objdraw ();

};

#endif

