// comid.H header file

#ifndef comid_object
#define comid_object

#include "sq_item.H"
#include "parser.H"

/************************************************************************
*	comid object 							*
************************************************************************/
class C_COMID : public C_SQ_ITEM {

  private:

    int local_id;
    int object_id;
    int object_type;
    int object_node;
    char *object_name;
    C_BASETYPE *comtype;

  protected:

  public:

//    C_COMID();

    void set_local_id(int li) {local_id = li;}
    void set_object_id(int oi) {object_id = oi;}
    void set_object_type(int ot) {object_type = ot;}
    void set_object_node(int on) {object_node = on;}
    void set_object_name(char *name) {object_name = name;}
    void set_unique_id(int ui) {id = ui;}
    void set_comtype(C_BASETYPE *b) {comtype = b;}

    int get_local_id() {return local_id;}
    int get_object_id() {return object_id;}
    int get_object_type() {return object_type;}
    int get_object_node() {return object_node;}
    char *get_object_name() {return object_name;}
    int get_unique_id() {return id;}
    C_BASETYPE *get_comtype() {return comtype;}

};


#endif
