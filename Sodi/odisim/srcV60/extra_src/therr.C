#include <stdio.h>
#include <math.h>

main() {
  int i,j;
  float theta;
  float theta2;
  float theta3;
  float maxerr;
  float averr;
  float s,s2;
  float c,c2;
  float b;
  float beta;
  float truebeta;
  float alpha;
  float correction;

  for (i = 0; i<=180; i++) {
    theta = i * 0.017453292;
    theta2 = theta*theta;
    theta3 = theta2*theta;
    correction = (0.45418)*theta - (1.8667)*theta2 + (2.5462)*theta3;
    s = sin(theta);
    c = cos(theta);
    s2 = sin(theta/2.0);
    c2 = cos(theta/2.0);
    b = 1.0 - c;

    maxerr = (1.0 + b * 0.25) * c2;

    correction = (0.57827) + (0.31670)*maxerr + (0.10501)*maxerr*maxerr;

    averr = 0.0;
    for (j=0; j<i; j++) {
      alpha = float(j) / float(i);
      truebeta = 1.0 / sqrt(1.0 - 2.0*b*alpha*(1.0-alpha));
      beta = 1.0 + b*alpha*(1.0-alpha);
      averr += beta/truebeta;
    }
    if (i!=0) averr = averr/i;

    printf("theta = %3d, maxerr = %f\%, averr = %f\%, corrction = %f\n",
	i,maxerr,averr,averr/correction);

  }

}
