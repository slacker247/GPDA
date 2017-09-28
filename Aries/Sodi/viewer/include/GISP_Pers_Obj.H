/***************************************************************************
  GISP_Obj.H
  A persistent graphics object which has complex interactive
  functions such as query, monitor, command and traking.

  -- 08/20/93: created by Y. Tung;

****************************************************************************/

#ifndef		GISP_PERS_OBJ
#define		GISP_PERS_OBJ		"GISP_Pers_Objclass"

#include "GR_Interface.H"
#include "GISP_Obj.H"
#include "host_user.H"

class GISP_Pers_Obj
{
 private:
   C_HOST_USER	*p_host_user;
   Widget	p_dialog, p_textw;
   Boolean	p_alive;
   long		p_id, p_type;
   void		init();
 
 public:
   GISP_Obj	*p_gispobj;
   Boolean      p_viewing, p_monitoring;
   GISP_Pers_Obj (long id, long type);
   int          p_timeoutid;
   long get_id () { return p_id; }
   long get_type () { return p_type; }
   C_HOST_USER *get_host_user ();
   void create_host_user ();
   void delete_host_user ();
   void set_text (char*);
   Boolean get_alive () { return p_alive; }
   void set_alive (Boolean alive) { p_alive = alive; }
   void set_object(GISP_Obj *object) { p_gispobj = object; }

   //friend void GISP_queryCB   (Widget, GISP_Pers_Obj*);
   //friend void GISP_monitorCB (Widget, GISP_Pers_Obj*);
   //friend void GISP_commandCB (Widget, GISP_Pers_Obj*);
   //friend void GISP_quitCB (Widget, GISP_Pers_Obj*);
};


void GISP_queryCB   (Widget, GISP_Pers_Obj*);
void GISP_monitorCB (Widget, GISP_Pers_Obj*);
void GISP_commandCB (Widget, GISP_Pers_Obj*);
void GISP_quitCB (Widget, GISP_Pers_Obj*);

#endif
