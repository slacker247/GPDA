// centobj.H header file

#ifndef centobj_object
#define centobj_object

#include "sensobj.H"
#include "xqueue.H"

/************************************************************************
*	centobj object 							*
************************************************************************/
class C_CENTOBJ : public C_SENSOBJ {

  private:

    double time;
    int blocking;
    C_XQUEUE tracks;
    C_XQUEUE gbis;
    C_XQUEUE pebbles;
    C_XQUEUE shots;

    int remove_tracks_on;
    double stale_track_time;

    int *gbis_used;
    int gbi_index;

  protected:

  public:

    C_CENTOBJ();
    C_XQUEUE *get_tracks() {return &tracks;}
    C_XQUEUE *get_gbis() {return &gbis;}
    C_XQUEUE *get_pebbles() {return &pebbles;}
    C_XQUEUE *get_shots() {return &shots;}

    double get_stale_track_time() {return stale_track_time;}
    void set_stale_track_time(double stt) {stale_track_time = stt;}

    void set_remove_tracks_on() {remove_tracks_on = 1;}
    void reset_remove_tracks_on() {remove_tracks_on = 0;}
    int get_remove_tracks_on() {return remove_tracks_on;}
    void exchange_remove_tracks_on(int &rto) {EXCHANGE(remove_tracks_on,rto);}

    void set_blocking() {blocking = 1;}
    void reset_blocking() {blocking = 0;}
    int get_blocking() {return blocking;}
    void exchange_blocking(int &b) {EXCHANGE(blocking,b);}

    int *get_gbis_used() {return gbis_used;}
    void set_gbis_used(int *gu) {gbis_used = gu;}
    int get_gbis_used(int site) {return gbis_used[site];}
    void exchange_gbis_used(int site, int &n) {EXCHANGE(gbis_used[site],n);}
    void exchange_gbis_used(int *&gu) {
	int *temp;
	temp = gbis_used;
	gbis_used = gu;
	gu = temp;
    }

    int get_gbi_index() {return gbi_index;}
    void set_gbi_index(int indx) {gbi_index = indx;}
    void exchange_gbi_index(int &indx) {EXCHANGE(gbi_index,indx);}

};


#endif
