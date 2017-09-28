/*********************************************************************
  GISP_Obj.C:
  
  -- 06/07/93: created by Y. Tung;
  -- 08/24/93: revised;
  
*******************************************************************/

#include "GISP_Obj.H"
#include "GISP_Pers_Obj.H"
#include "GR_Idlist.H"

IDlist* GISP_pick_idlist=NULL;

/* ===== GISP_Obj functions ===== */
  
GISP_Obj::GISP_Obj (): GR_Model ()
{
}

GISP_Obj::GISP_Obj (long id, long type): GR_Model (id, type)
{
}


GISP_Obj::~GISP_Obj ()
{
}


void
GISP_Obj::objdraw ()
{
  GR_Model::objdraw();
}

void
GISP_Obj::pickEvent (GR_MouseEvent& event, GR_Window*)
{
   long id, type;

   id = get_id ();
   type = get_type ();
   printf ("GISP object #%d, type %d, was picked", id, type);
   switch (event.button)
   {
    case GR_LEFTMOUSE:
      if (event.down)
      {
         printf ("..by GR_LEFTMOUSE..");
	 if (!GISP_pick_idlist)
         {
           printf (" Creating a new GISP_pick_idlist\n");
	   GISP_pick_idlist = new IDlist (100);
	 }
	 if (GISP_pick_idlist->in_list (id))
	   printf (" --> already picked...\007\n");
	 else
	 {
	   new GISP_Pers_Obj (id, type);
	   GISP_pick_idlist->put_list (id);
	 }
         GISP_pick_idlist->print_list ();   // for debugging only....
       }
      break;
    case GR_MIDDLEMOUSE:
      if (!event.down)
      {
	  printf (".. by GR_MIDDLEMOUSE..");
      }
      break;
    case GR_RIGHTMOUSE:
      if (event.down)
      {
         printf (".. by GR_RIGHTMOUSE..");
      }
      break;
   }
   printf("\n");
}

