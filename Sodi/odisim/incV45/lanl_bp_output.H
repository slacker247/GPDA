// lanl_bp_output.H header file

#ifndef lanl_bp_output_object
#define lanl_bp_output_object

/************************************************************************
*	lanl_bp_output object 						*
************************************************************************/
class C_LANL_BP_OUTPUT {

  private:

  protected:

  public:

    int threat_id;
    int asset_id;
    double launch_time;

//  C_LANL_BP_OUTPUT();

//...... set methods

    void set_threat_id(int i) {threat_id = i;}
    void set_asset_id(int i) {asset_id = i;}
    void set_launch_time(double t) {launch_time = t;}

//...... get methods

    int get_threat_id() {return threat_id;}
    int get_asset_id() {return asset_id;}
    double get_launch_time() {return launch_time;}

};


#endif
