/**********************************************************************
  newsps.H --
  08/24/93: created by Y. Tung; 
    
*********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
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

#include "GR_Interface.H"
#include "GR_DispList.H"
#include "GR_DispObj.H"
#include "GR_Shell.H"
#include "GR_Window.H"

#define RE 6378.145
