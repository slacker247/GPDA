// ccc_plan_mess.H message file

#ifndef cccp_mess
#define cccp_mess

#include "headers.H"

class CCC_PLAN_DATA {
  public:
	int asset_type;
	int asset_id;
	int threat_id;
	double intercept_time;
};

/************************************************************************
*	ccc_plan message 						*
************************************************************************/
class CCC_PLAN_MESS : public C_EM_COMMAND {

  public:

    int n_plans;

    void init(int np) {
	strcpy(EM_command,"CCC_PLAN");
	data_bytes = sizeof(CCC_PLAN_MESS) - sizeof(C_EM_HEADER)
			+ np*sizeof(CCC_PLAN_DATA);
	n_plans = np;
    }
};

#endif
