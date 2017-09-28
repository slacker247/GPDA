/**********************************************************************
    
*********************************************************************/

#ifndef MISC
#define MISC                            "miscclass"

#include <stdio.h>
#include <malloc.h>
#include <sys/types.h>
#include "GR_Interface.H"

class IDlist
{
 private:
   long *p_idlist;
   long p_maxsize;
   long p_size;
   long p_curr;
 public:
   IDlist (long size=1024);
   ~IDlist ();
   Boolean in_list (long id);
   void put_list (long id);
   void rm_list (long id);
   void clean_list ();
   void print_list ();
};

#endif

