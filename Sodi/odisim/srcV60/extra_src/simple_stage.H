// stage.H header file

#ifndef stage_object
#define stage_object

/************************************************************************
*	stage object 							*
************************************************************************/
class C_STAGE {

  private:

    float R0[3];		// position at start of stage
    float V0[3];		// velocity at start of stage
    float A0[3];		// thrust angle
    float beta;			// a constant
    float gamma;		// a constant
    float mu;			// the mass ratio Mf/M0

  protected:

    float T;			// constant thrust throughout the stage
    float Tb;			// burn time of stage
    float Mr;			// mass of the rocket including this stage
    float Mf;			// mass of the fuel for this stage
    float Ms;			// mass of the shell

  public:

    C_STAGE();
    void init_pars(float thrust, float tburn, float mrocket, float mfuel, float mshell);
    void init_R0(float r0[3]);
    void init_R0(float r0[3], float r);
    void init_V0(float v0[3]) {V0[0] = v0[0]; V0[1] = v0[1]; V0[2] = v0[2];}
    void init_A0(float a0[3]) {A0[0] = a0[0]; A0[1] = a0[1]; A0[2] = a0[2];}
    void get_pos_vel(float t, float rt[3], float vt[3]);

};


#endif
