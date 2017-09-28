// speedes_state.H header file

#ifndef sp_state_object
#define sp_state_object

#include "speedes_object.H"
#include "link.H"
#include "queue.H"
#include "hash.H"
#include "StatsMessage.H"

/************************************************************************
*	speedes simulation state					*
************************************************************************/
class C_SPEEDES_STATE {

private:

  C_HASH *objects;		// speedes objects
  C_QUEUE links;
  StatsMessage *Smsg;
  int NumNodes;

public:

  C_SPEEDES_STATE() {objects = new C_HASH(1000);}

  //...... get the list of all SPEEDES objects

  C_QUEUE *get_objects() {objects->combine(); return (objects->get_all());}

  //...... get a pointer to an object based on its unique id

  C_SPEEDES_OBJECT *get_object(int id)
    {return (C_SPEEDES_OBJECT *)(objects->get(id));}

  //...... add a SPEEDES object to the list of objects

  void add_object(C_SPEEDES_OBJECT *spo) {objects->add(spo);}

  //...... remove a SPEEDES object from the list of objects

  void remove_object(C_SPEEDES_OBJECT *spo) {objects->dehash(spo);}

  C_SPEEDES_OBJECT *remove_object(int id)
    {return (C_SPEEDES_OBJECT *)(objects->dehash(id));}

  //...... get the list of links

  C_QUEUE *get_links() {return &links;}

  //...... add a link to  the SPEEDES state

  void add_link(C_LINK *link) {links.push_bot(link);}
  C_LINK *remove_link(int sensor_id, int track_id);

  StatsMessage * getStatsMessage() { return Smsg;}

  void setStatsMessage(StatsMessage * NewSmsg){
    cout << "mjp C_SPEEDES_STATE::setStatsMessage" << endl;
    if (Smsg == NULL){
      Smsg = new StatsMessage[NumNodes];
    }
    memcpy(Smsg, NewSmsg, NumNodes*sizeof(StatsMessage));
  }
  
  void setNnodes(int n){NumNodes = n;}
  int getNnodes(){return NumNodes;}
  
};

#endif

