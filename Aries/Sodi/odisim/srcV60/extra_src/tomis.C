#include <stdio.h>
#include <math.h>

double get_acceleration(double t, double Tb, double L, double Vex);

main() {
  double Tb;
  double L;
  double Vex;
  double a0;
  double a1;

//...... SS18 stage 0

  Tb = 107.5;
  L = 0.7322;
  Vex = 2.995;

  a0 = get_acceleration(0.0, Tb, L, Vex);
  a1 = get_acceleration(Tb, Tb, L, Vex);

  printf("SS18> stage 0: a(0.0) = %f, a(%f) = %f, mf/mi = %f\n",
	a0,Tb,a1,1.0-L);

//...... SS18 stage 1

  Tb = 174.5;
  L = 0.8167;
  Vex = 3.168;

  a0 = get_acceleration(0.0, Tb, L, Vex);
  a1 = get_acceleration(Tb, Tb, L, Vex);

  printf("SS18> stage 1: a(0.0) = %f, a(%f) = %f, mf/mi = %f\n",
	a0,Tb,a1,1.0-L);

}


double get_acceleration(double t, double Tb, double L, double Vex) {
  double a;
  double gamma;

  gamma = L / Tb;
  a = (gamma*Vex) / (1.0 - gamma*t);
  a /= 0.0098;

  return a;

}
