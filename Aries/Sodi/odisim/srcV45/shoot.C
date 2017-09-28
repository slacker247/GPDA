#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <signal.h> 

#include "host_user.H"
#include "ccc_plan_mess.H"

int stop_flag;
int cntrl_c;

void sigintfunc(int s)
{
    signal( SIGINT, sigintfunc ); 
    cntrl_c = 1;
    if (stop_flag) exit(0);
} 

void get_id();
void query();
void monitor();
void shoot_gbi();
void random_shoot_gbi();
void shoot5();

C_HOST_USER host_user;

main() {
  int option;
  C_EM_HEADER *EM_header;

  signal( SIGINT, sigintfunc ); 

  fprintf(stderr,"\f");

  while (1) {

    host_user.reset();
    stop_flag = 1;
    cntrl_c = 0;

    fprintf(stderr,"Interactive Options\n");
    fprintf(stderr,"-------------------\n");
    fprintf(stderr,"\n");
    fprintf(stderr,"0. Exit\n");
    fprintf(stderr,"1. Get ID\n");
    fprintf(stderr,"2. Query\n");
    fprintf(stderr,"3. Monitor\n");
    fprintf(stderr,"4. Shoot GBI\n");
    fprintf(stderr,"5. Random shoot all RVs\n");
    fprintf(stderr,"6. Shoot Five RVs\n");
    fprintf(stderr,"\n");

    fprintf(stderr,"Enter option(0,1,2,3,4,5,6): ");
    fflush(stderr);
    scanf("%d",&option);

//...... before doing anything, flush out all of the messages

    EM_header = NULL;
    EM_header = host_user.get_message();
    while (EM_header != NULL) {
      delete EM_header;
      EM_header = host_user.get_message();
    }

    if (option == 0) break;

    switch (option) {

      case 1:
	get_id();
        break;

      case 2:
	query();
        break;

      case 3:
	monitor();
        break;

      case 4:
	shoot_gbi();
        break;

      case 5:
	random_shoot_gbi();
        break;

      case 6:
	shoot5();
        break;

    }

  }

  return 1;

}

/************************************************************************
* get_id - get the id of an object given its name			*
************************************************************************/
void get_id() {
  int id;
  char name[40];

  fprintf(stderr,"ID> Enter object name: ");
  fflush(stderr);
  scanf("%s",name);

  id = host_user.getid(name);
  fprintf(stderr, "%s has ID %d\n",name,id);

}

/************************************************************************
* query - query an object based on its id				*
************************************************************************/
void query() {
  int id;
  C_EM_HEADER *EM_header;

  fprintf(stderr,"QUERY> Enter object id: ");
  fflush(stderr);
  scanf("%d",&id);

  EM_header = host_user.query(id);

  if (EM_header != NULL) {
    delete EM_header;
  }

}

/************************************************************************
* monitor - monitor an object based on its id				*
************************************************************************/
void monitor() {
  int id;
  C_EM_HEADER *EM_header;

  fprintf(stderr,"MONITOR> Enter object id: ");
  fflush(stderr);
  scanf("%d",&id);

//...... loop until control c monitoring the object

  stop_flag = 0;
  while (1) {

    EM_header = host_user.monitor(id);

    if (EM_header != NULL) {
      delete EM_header;
    }

    if (cntrl_c == 1) {
      cntrl_c = 0;
      host_user.disconnect();
      break;
    }

  }

}

/************************************************************************
* shoot_gbi - shoot a ground based interceptor				*
************************************************************************/
void shoot_gbi() {
  int id;
  int farm_id;
  int threat_id;
  CCC_PLAN_MESS *ccc_plan_mess;
  CCC_PLAN_DATA *ccc_plan_data;
  char *buff;
  char name[40];

  id = host_user.getid("NORAD_COMMAND_CENTER");

//...... create the command message

  buff = new char[sizeof(CCC_PLAN_MESS) + sizeof(CCC_PLAN_DATA)];

  ccc_plan_mess = (CCC_PLAN_MESS *)buff;
  ccc_plan_mess->init(1);
  ccc_plan_mess->objid = id;

  ccc_plan_data = (CCC_PLAN_DATA *)&buff[sizeof(CCC_PLAN_MESS)];
  ccc_plan_data->asset_type = -1;
  ccc_plan_data->intercept_time = 0.0;

//...... get the GBI and threat ids

  fprintf(stderr,"SHOOT GBI> Enter GBI farm name: ");
  fflush(stderr);
  scanf("%s",name);
  farm_id = host_user.getid(name);
  ccc_plan_data->asset_id = farm_id;

  fprintf(stderr,"SHOOT GBI> Enter threat id: ");
  fflush(stderr);
  scanf("%d",&threat_id);
  ccc_plan_data->threat_id = threat_id;

  printf("shooting %d at %d\n",farm_id, threat_id);

//...... send the message

  host_user.command(ccc_plan_mess);

//...... delete the buffer that was created

  delete [] buff; //RVI 2/18/98

}

/************************************************************************
* random_shoot_gbi - randomly shoot all of the RVs			*
************************************************************************/
void random_shoot_gbi() {
  int id;
  int gbi_farm_1, gbi_farm_11;
  int Krutogorova1, Krutogorova200;
  CCC_PLAN_MESS *ccc_plan_mess;
  CCC_PLAN_DATA *ccc_plan_data;
  char *buff;
  int k,g,index;
  int n;

//...... initialize how many plans to send

  n = 200;

//...... get the command center id

  id = host_user.getid("NORAD_COMMAND_CENTER");

  fprintf(stderr,"NORAD_COMMAND_CENTER = %d\n",id);

//...... get the gbi site ids

  gbi_farm_1 = host_user.getid("GBI_FARM_1");
  gbi_farm_11 = host_user.getid("GBI_FARM_11");

  fprintf(stderr,"GBI_FARM_1 = %d, GBI_FARM_11 = %d\n",
	gbi_farm_1, gbi_farm_11);

//...... get the missile ids

  Krutogorova1 = host_user.getid("Krutogorova1");
  Krutogorova200 = host_user.getid("Krutogorova200");

  fprintf(stderr,"Krutogorova1 = %d, Krutogorova200 = %d\n",
	Krutogorova1, Krutogorova200);

//...... create the command message

  buff = new char[sizeof(CCC_PLAN_MESS) + n*sizeof(CCC_PLAN_DATA)];

  ccc_plan_mess = (CCC_PLAN_MESS *)buff;
  ccc_plan_mess->init(n);
  ccc_plan_mess->objid = id;

  ccc_plan_data = (CCC_PLAN_DATA *)&buff[sizeof(CCC_PLAN_MESS)];
  ccc_plan_data->asset_type = -1;
  ccc_plan_data->intercept_time = 0.0;

//...... loop over all missiles and all sites

  index = 0;
  for (k=Krutogorova200; k<=Krutogorova1; k++) {

    g = index % (gbi_farm_1-gbi_farm_11+1) + gbi_farm_11;

    ccc_plan_data[index].asset_id = g;
    ccc_plan_data[index].threat_id = k;

    fprintf(stderr,"Shooting RV %d from farm %d\n",k,g);
    index++;

    if (index == n) break;

  }

//...... send the message

  host_user.command(ccc_plan_mess);

//...... delete the buffer that was created

  delete [] buff; //RVI 2/18/98

}



/************************************************************************
* shoot5 - shoot five RVs (hardcoded)					*
************************************************************************/
void shoot5() {
  int i,n;
  int center_id;
  int gbi_farm_id[5];
  int missile_id[5];
  CCC_PLAN_MESS *ccc_plan_mess;
  CCC_PLAN_DATA *ccc_plan_data;
  char *buff;

//...... initialize how many plans to send

  n = 5;

//...... get the command center id

  center_id = host_user.getid("NORAD_COMMAND_CENTER");
  fprintf(stderr,"NORAD_COMMAND_CENTER = %d\n",center_id);

//...... get the ids of the RVs

  missile_id[0] = host_user.getid("intmiss1");
  missile_id[1] = host_user.getid("intmiss2");
  missile_id[2] = host_user.getid("intmiss3");
  missile_id[3] = host_user.getid("intmiss4");
  missile_id[4] = host_user.getid("intmiss5");

//...... get the ids of the farms for each missile

  gbi_farm_id[0] = host_user.getid("GBI_FARM_1");
  gbi_farm_id[1] = host_user.getid("GBI_FARM_2");
  gbi_farm_id[2] = host_user.getid("GBI_FARM_3");
  gbi_farm_id[3] = host_user.getid("GBI_FARM_4");
  gbi_farm_id[4] = host_user.getid("GBI_FARM_5");

//...... create the command message

  buff = new char[sizeof(CCC_PLAN_MESS) + n*sizeof(CCC_PLAN_DATA)];

  ccc_plan_mess = (CCC_PLAN_MESS *)buff;
  ccc_plan_mess->init(n);
  ccc_plan_mess->objid = center_id;

  ccc_plan_data = (CCC_PLAN_DATA *)&buff[sizeof(CCC_PLAN_MESS)];
  ccc_plan_data->asset_type = -1;
  ccc_plan_data->intercept_time = 0.0;

//...... loop over all missile/interceptor pairs

  for (i=0; i<n; i++) {
    ccc_plan_data[i].asset_id = gbi_farm_id[i];
    ccc_plan_data[i].threat_id = missile_id[i];
  }

//...... send the message

  host_user.command(ccc_plan_mess);

//...... delete the buffer that was created

  delete [] buff; //RVI 2/18/98

}



