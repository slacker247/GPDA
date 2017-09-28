// shoot_gbi_mess.H message file

#ifndef shinfo_object
#define shinfo_object

class C_SHOOT_INFO {

  public:

    double lat_site;
    double lon_site;
    int threat_id;
    int threat_node;
    int threat_type;
    int threat_uid;
    double X_threat_truth[3];
    double V_threat_truth[3];
    double X_threat_track[3];
    double V_threat_track[3];
    double truth_threat_time;
    double est_threat_time;
    double kill_time;

};

#endif
