/***************************************************************************
  GISP_Obj.H
  A simulation graphics object based on GR_Model but has complex interactive
  functions such as query, monitor, command and traking.

  -- 06/07/93: created by Y. Tung;
  -- 08/24/93: revised;
           
****************************************************************************/

#ifndef		GISP_OBJ
#define		GISP_OBJ		"GISP_Objclass"

#include "GR_Model.H"

class GISP_Obj: public GR_Model
{
 private:

 public:
   GISP_Obj ();
   GISP_Obj (long id, long type);
   ~GISP_Obj ();
   void objdraw ();
   void pickEvent (GR_MouseEvent& event, GR_Window* window);
};
 
#endif
