/**********************************************************************
  sim_interface.H --
  Everything needed to build a simulation -- graphics interface.
  04/15/93: created by Y. Tung
  07/01/93: added an ID list to keep track of the object IDs -- used
  to ignore further position update when an deleted object is in such list;
  08/20/93: added rm_list (id) in IDlist; 
    
*********************************************************************/

#ifndef SIM_INTERFACE
#define SIM_INTERFACE                            "sim_interfaceclass"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <malloc.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/param.h>

#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/MessageB.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/LabelG.h>
#include <Xm/Label.h>

#include "Messager.H"
#include "GR_Interface.H"
#include "GR_DispList.H"
#include "GR_DispObj.H"
#include "GR_Shell.H"
#include "GR_Window.H"
#include "GR_Model.H"
#include "GR_Sensor.H"
#include "GR_Othb.H"
#include "GR_Links.H"

#define RE 6378.145

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

