#include <stdio.h>

int main()
{
char          temp1[24], temp2[24], temp3[24], temp4[24], temp5[24];
char          chmsg[24];
int           rtn, color, stn;
float         f1, f2, f3, f4, f5;
float         time, X, Y, Z, Vx, Vy, Vz, alt, vel;
FILE          *infile, *outfile;

   infile = fopen("old.jdn", "r");
   outfile = fopen("new.jdn", "w");

   while (!feof(infile)) {

      fscanf(infile, "%f %s %s %s %s %d %d %f %f %f %s %d %f %f %f %f %f\n",
             &time, temp1, temp2, temp3, temp4, &rtn, &stn, &X, &Y, &Z,
             temp5, &color, &f1, &f2, &f3, &alt, &vel);

      fprintf(outfile, "%f %s %s %s %s %d %d %f %f %f %s %d %f %f %f %f %f %f\n",
             time, temp1, temp2, temp3, temp4, rtn, stn, X, Y, Z,
             temp5, color, alt, vel, f1, f2, f3, 0.0);

   }
   return (0);
}
