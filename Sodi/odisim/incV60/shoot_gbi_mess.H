// shoot_gbi_mess.H message file

#ifndef shgbi_mess
#define shgbi_mess

#include "shoot_info.H"
#include "headers.H"

/************************************************************************
*	shoot_gbi message 						*
************************************************************************/
class SHOOT_GBI_MESS : public C_HEADER {

  public:

    C_SHOOT_INFO shoot_info;
    C_SHOOT_INFO *get_shoot_info() {return &shoot_info;}

};

#endif
