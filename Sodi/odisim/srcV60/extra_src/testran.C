#include <stdio.h>
#include <stdlib.h>
#include <math.h>

unsigned long seed;

#define IA 2416
#define IC 374441
#define IM 1771875

double ranfloat() {
  seed = (seed*IA+IC)%IM;
  return double(seed) / double(IM);  
}

#define SEEDINIT 1

main() {
  int i;
  double ranval1, ranval2;
  int counter;
  unsigned long seed100;

  seed = 1;

  for (i=0; i<10; i++) {
    ranval1 = ranfloat();
    ranval2 = drand48();
    printf("%f %f\n",ranval1,ranval2);
  }

  seed = SEEDINIT;
  seed100 = 1;
  counter = 0;

  while (1) {
    ranfloat();
    counter++;
    if (seed == SEEDINIT) break;
    if (counter == 100000) {
      seed100 = seed;
    }else{
      if (seed == seed100) {
        printf("bad seed %d at counter %d\n",seed, counter-100000);
	exit(1);
      }
    }
  }

  printf("cyclic seed %d at counter %d\n",seed, counter);

}
