// gbiman.H header file

#ifndef gbiman_object
#define gbiman_object

#include "parsman.H"
#include "gbiobj.H"

/************************************************************************
*	gbiman object - manager for space objects covering the earth	*
************************************************************************/
class C_GBIMAN : public C_PARSMAN {

  private:

    int n_sites;		// number of gbi sites
    int *gbi_id;		// id of gbi site (starts with 0)
    int *n_each_site;		// number of gbis at each site
    int *gbi_sum;		// cumulative sum of gbis at each site
    char **site_name;		// name of each site
    char **gbi_type;		// type of GBI at each site
    double *lat_site;		// latitude of each site
    double *lon_site;		// longitude of each site
    int total_gbis;		// total number of gbis

    int n_gbi_types;		// number of missile types
    C_GBIOBJ *gbiobj;		// gbi missile simulation objects
    int first_id;
    int first_node;
    int test_prox;
    double pkill;

  protected:

  public:

    C_GBIMAN();
    char *get_site_name(int s) {return site_name[s];}
    virtual void init_events();

    int get_n_sites() {return n_sites;}

    int *get_n_each_site() {return n_each_site;}
    int get_n_each_site(int s) {return n_each_site[s];}

    int *get_gbi_id() {return gbi_id;}
    int get_gbi_id(int i) {return gbi_id[i];}
    int get_uid(int site_index) {return (site_index + first_id);}
    int get_gid_site(int uid) {return (uid-first_id);}
    void get_nodid_site(int uid, int &nd, int &lid);
    void get_nodid_gbi(int gid, int &nd, int &lid);

    double get_lat_site(int gid) {return lat_site[gid];}
    double get_lon_site(int gid) {return lon_site[gid];}

    double get_pkill() {return pkill;}

};


#endif
