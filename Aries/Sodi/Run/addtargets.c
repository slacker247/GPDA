#include <stdio.h>
#include <stdlib.h>

int main()
{
char          temp1[24], temp2[24], temp3[24], temp4[24], temp5[24];
char          chmsg[64];
int           rtn, color, stn, i, j, n, count=0;
int           n1, n2, n3, n4, n5, n6, n7, n8, n9, n10;
float         f1, f2, f3, f4, f5;
float         time, X, Y, Z, Vx, Vy, Vz, alt, vel;
int           latmax, latmin, lonmax, lonmin;
FILE          *infile, *outfile;
char          puptemp[1024];

   infile = fopen("Targets.Iran", "r");
   outfile = fopen("targets.new", "w");

   fscanf(infile, "%d\n", &n);
   fprintf(outfile, "%5d\n", n);
   fprintf(outfile, "# Iran targets\n");

   while (!feof(infile)) {
      fscanf(infile, "%s %d %d %d %d %d %d %d %d %d %d %d %d %d %d %f %d %d %d", 
             chmsg, &latmax, &latmin, &lonmax, &lonmin,
             &n1, &n2, &n3, &n4, &n5, &n6, &n7, &n8, &n9, &n10,
             &f1, &i, &rtn, &j);
      if (feof(infile)) break;

      count++;

      fprintf(outfile, "%-20s %02d %02d   %02d %02d", chmsg, latmax, latmin, lonmax, lonmin);
      fprintf(outfile, "  %3d %3d %3d %3d %3d %3d %3d %3d %3d %3d",
              n1, n2, n3, n4, n5, n6, n7, n8, n9, n10);
      fprintf(outfile, "  %4.2f  %d  %d  %d\n", f1, i, rtn, j);

      if (n1 > 0) {
        for (n=0; n<n1; n++) {
          sprintf(temp1, "%s-MB%d", chmsg, n+1);
          fprintf(outfile, "  %-18s %02d %02d   %02d %02d",
                  temp1, latmax, latmin, lonmax, lonmin);
          fprintf(outfile, "  %4.2f  %d  %d  %d\n", f1, i, rtn, j);
        }
      }
      if (n2 > 0) {
        for (n=0; n<n2; n++) {
          sprintf(temp1, "%s-AB%d", chmsg, n+1);
          fprintf(outfile, "  %-18s %02d %02d   %02d %02d",
                  temp1, latmax, latmin, lonmax, lonmin);
          fprintf(outfile, "  %4.2f  %d  %d  %d\n", f1, i, rtn, j);
        }
      }
      if (n3 > 0) {
        for (n=0; n<n3; n++) {
          sprintf(temp1, "%s-SB%d", chmsg, n+1);
          fprintf(outfile, "  %-18s %02d %02d   %02d %02d",
                  temp1, latmax, latmin, lonmax, lonmin);
          fprintf(outfile, "  %4.2f  %d  %d  %d\n", f1, i, rtn, j);
        }
      }
      if (n4 > 0) {
        for (n=0; n<n4; n++) {
          sprintf(temp1, "%s-NW%d", chmsg, n+1);
          fprintf(outfile, "  %-18s %02d %02d   %02d %02d",
                  temp1, latmax, latmin, lonmax, lonmin);
          fprintf(outfile, "  %4.2f  %d  %d  %d\n", f1, i, rtn, j);
        }
      }
      if (n5 > 0) {
        for (n=0; n<n5; n++) {
          sprintf(temp1, "%s-CW%d", chmsg, n+1);
          fprintf(outfile, "  %-18s %02d %02d   %02d %02d",
                  temp1, latmax, latmin, lonmax, lonmin);
          fprintf(outfile, "  %4.2f  %d  %d  %d\n", f1, i, rtn, j);
        }
      }
      if (n6 > 0) {
        for (n=0; n<n6; n++) {
          sprintf(temp1, "%s-BW%d", chmsg, n+1);
          fprintf(outfile, "  %-18s %02d %02d   %02d %02d",
                  temp1, latmax, latmin, lonmax, lonmin);
          fprintf(outfile, "  %4.2f  %d  %d  %d\n", f1, i, rtn, j);
        }
      }
      if (n7 > 0) {
        for (n=0; n<n7; n++) {
          sprintf(temp1, "%s-CC%d", chmsg, n+1);
          fprintf(outfile, "  %-18s %02d %02d   %02d %02d",
                  temp1, latmax, latmin, lonmax, lonmin);
          fprintf(outfile, "  %4.2f  %d  %d  %d\n", f1, i, rtn, j);
        }
      }
      if (n8 > 0) {
        for (n=0; n<n8; n++) {
          sprintf(temp1, "%s-PC%d", chmsg, n+1);
          fprintf(outfile, "  %-18s %02d %02d   %02d %02d",
                  temp1, latmax, latmin, lonmax, lonmin);
          fprintf(outfile, "  %4.2f  %d  %d  %d\n", f1, i, rtn, j);
        }
      }
      if (n9 > 0) {
        for (n=0; n<n9; n++) {
          sprintf(temp1, "%s-TC%d", chmsg, n+1);
          fprintf(outfile, "  %-18s %02d %02d   %02d %02d",
                  temp1, latmax, latmin, lonmax, lonmin);
          fprintf(outfile, "  %4.2f  %d  %d  %d\n", f1, i, rtn, j);
        }
      }
      if (n10 > 0) {
        for (n=0; n<n10; n++) {
          sprintf(temp1, "%s-OT%d", chmsg, n+1);
          fprintf(outfile, "  %-18s %02d %02d   %02d %02d",
                  temp1, latmax, latmin, lonmax, lonmin);
          fprintf(outfile, "  %4.2f  %d  %d  %d\n", f1, i, rtn, j);
        }
      }

      fflush(outfile);
   }

   fclose(infile);
   fclose(outfile);

   printf("Processed %d records.\n", count);

   return (0);
}
