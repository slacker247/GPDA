// lanl_bp_input.H header file

#ifndef lanl_bp_input_object
#define lanl_bp_input_object

/************************************************************************
*	lanl_bp_input object 						*
************************************************************************/
class C_LANL_BP_INPUT {

  private:

  protected:

  public:

    int id;
    double time;
    double Xtrack[3];
    double Vtrack[3];
    double Xerror;
    double Verror;

//  C_LANL_BP_INPUT();

//...... set methods

    void set_id(int i) {id = i;}
    void set_time(double t) {time = t;}

    void set_Xtrack(double X[3]) {
		Xtrack[0] = X[0];
		Xtrack[1] = X[1];
		Xtrack[2] = X[2];
	}

    void set_Vtrack(double V[3]) {
		Vtrack[0] = V[0];
		Vtrack[1] = V[1];
		Vtrack[2] = V[2];
	}

    void set_Xerror(double Xe) {Xerror = Xe;}
    void set_Verror(double Ve) {Verror = Ve;}

//...... get methods

    int get_id() {return id;}
    double get_time() {return time;}
    double *get_Xtrack() {return Xtrack;}
    double *get_Vtrack() {return Vtrack;}
    double get_Xerror() {return Xerror;}
    double get_Verror() {return Verror;}

};


#endif
