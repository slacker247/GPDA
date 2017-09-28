// oid.H header file

#ifndef oid_object
#define oid_object

#include "sq_item.H"

/************************************************************************
*	oid object 							*
************************************************************************/
class C_OID : public C_SQ_ITEM {

  private:

    int object_id;
    int object_type;
    int object_node;
    void *peom;

  protected:

  public:

//  C_OID();

    void set_object_id(int oi) {object_id = oi;}
    void set_object_type(int ot) {object_type = ot;}
    void set_object_node(int on) {object_node = on;}
    void set_unique_id(int ui) {id = ui;}
    void set_peom(void *p) {peom = p;}

    int get_object_id() {return object_id;}
    int get_object_type() {return object_type;}
    int get_object_node() {return object_node;}
    int get_unique_id() {return id;}
    void *get_peom() {return peom;}

};


#endif
