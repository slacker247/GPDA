#include <stdio.h>

int main()
{
char          temp1[24], temp2[24], temp3[24], temp4[24], temp5[24];
char          chmsg[64];
int           rtn, color, stn, i, j;
float         f1, f2, f3, f4, f5;
float         time, X, Y, Z, Vx, Vy, Vz, alt, vel;
float         latmax, latmin, lonmax, lonmin;
FILE          *infile, *outfile;

   infile = fopen("statebound.dat", "r");

   latmax = -1000.0;
   latmin = 1000.0;
   lonmax = -1000.0;
   lonmin = 1000.0;

   while (!feof(infile)) {

      fscanf(infile, "%d %s\n", &rtn, chmsg);
      printf("State %s has %d points\n", chmsg, rtn);

      for (i=0; i<rtn; i++) {
        fscanf(infile, "%f %f", &f1, &f2);
	if (f1 > latmax) latmax = f1;
        if (f1 < latmin) latmin = f1;
	if (f2 > lonmax) lonmax = f2;
	if (f2 < lonmin) lonmin = f2;
      }
   }

   printf("Left = %f, Right = %f, Top = %f, Bottom = %f\n", lonmin, lonmax, latmax, latmin);
   return (0);
}
