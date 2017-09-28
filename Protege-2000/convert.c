#include <stdio.h>

inline 
char *strsub(char *istr, char och, char nch)
{
int             i;
 
   for (i=0; i<strlen(istr); i++) if (istr[i] == och) istr[i] = nch;
   return (istr);
}


int Pins2Evid(char *infname, char *otfname)
{
FILE          *infile, *outfile;
int           i, j, k;
float         time, duration, disbelief, belief;
int           level, type, ext_level, node, ext_node;
char          chdur[16], chmission[32], filename[64], charg1[16];
char          charg2[16], charg3[16], chtunit[16], chdesc[64], chsource[16];
char          quote = '"';
char          temp1[64], temp2[64], temp3[24], temp4[24], temp5[24];
char          chmsg[24];
char          dsbtemp[128];

   infile = fopen(infname, "r");
   outfile = fopen(otfname, "w");

   fgets(dsbtemp, 128, infile);
   dsbtemp[0] = ' ';
   strsub(dsbtemp, '\n', ' ');
   fprintf(outfile, "%4d  %s  %s  [%s %s]\n", 0, "140900:17Jul01", "IW-DCI",
           "Generated from Protege-2000 PINS file", dsbtemp);
   fgets(dsbtemp, 128, infile);
   fgets(dsbtemp, 128, infile);
   fgets(dsbtemp, 128, infile);
   fgets(dsbtemp, 128, infile);
   fgets(dsbtemp, 128, infile);

   k = 0;
   while (!feof(infile)) {

     while (1) {
       fscanf(infile, "%s %s", temp1, temp2);

       for (i=0; i<18; i++) {
         if (strcmp(temp1, "(Time") == 0)
	   { sscanf(temp2, "%f)", &time); break; }

         if (strcmp(temp1, "(Level") == 0)
	   { sscanf(temp2, "%d)", &level); break; }

         if (strcmp(temp1, "(Duration") == 0)
	   { sscanf(temp2, "%f)", &duration); break; }

         if (strcmp(temp1, "(Type") == 0)
	   { sscanf(temp2, "%d)", &type); break; }

         if (strcmp(temp1, "(External-Level") == 0)
	   { sscanf(temp2, "%d)", &ext_level); break; }

         if (strcmp(temp1, "(Dur-Unit") == 0)
	   { strsub(temp2, ')', ' '); strsub(temp2, quote, ' ');
	   sscanf(temp2, "%s)", chdur); break; }

         if (strcmp(temp1, "(Mission-Domain") == 0)
	   { strsub(temp2, ')', ' '); strsub(temp2, quote, ' ');
	   sscanf(temp2, "%s)", chmission); break; }

         if (strcmp(temp1, "(External-Filename") == 0)
	   { strsub(temp2, ')', ' '); strsub(temp2, quote, ' ');
	   sscanf(temp2, "%s)", filename); break; }

         if (strcmp(temp1, "(Disbelief") == 0)
	   { sscanf(temp2, "%f)", &disbelief); break; }

         if (strcmp(temp1, "(Arg-3") == 0)
	   { strsub(temp2, ')', ' '); strsub(temp2, quote, ' ');
	   sscanf(temp2, "%s)", charg3); break; }

         if (strcmp(temp1, "(Time-Unit") == 0)
	   { strsub(temp2, ')', ' '); strsub(temp2, quote, ' ');
	   sscanf(temp2, "%s)", chtunit); break; }

         if (strcmp(temp1, "(Arg-1") == 0)
	   { strsub(temp2, ')', ' '); strsub(temp2, quote, ' ');
	   sscanf(temp2, "%s)", charg1); break; }

         if (strcmp(temp1, "(Node") == 0)
	   { sscanf(temp2, "%d)", &node); break; }

         if (strcmp(temp1, "(Description") == 0)
	   { strsub(temp2, ')', ' '); strsub(temp2, quote, ' ');
	   sscanf(temp2, "%s)", chdesc); break; }

         if (strcmp(temp1, "(Source") == 0)
	   { strsub(temp2, ')', ' '); strsub(temp2, quote, ' ');
	   sscanf(temp2, "%s)", chsource); break; }

         if (strcmp(temp1, "(External-Node") == 0)
	   { sscanf(temp2, "%d)", &ext_node); break; }

         if (strcmp(temp1, "(Arg-2") == 0)
	   { strsub(temp2, ')', ' '); strsub(temp2, quote, ' ');
	   sscanf(temp2, "%s)", charg2); break; }

         if (strcmp(temp1, "(Belief") == 0)
	   { sscanf(temp2, "%f)", &belief); break; }
       }

       //printf("%s %s\n", temp1, temp2);
       
       if (strstr(temp2, "))")) break;
     }

     fprintf(outfile, "%3d %3d %3d  %6.2f  %6.2f  %8.2f %s  %8.2f  %s  %-8s  %-8s  %-8s  %-16s %4d %4d   %s\n",
	    type, node, level, belief, disbelief, time, chtunit,
	    duration, chdur, charg1, charg2, charg3, chdesc,
	    ext_node, ext_level, filename);

     fgets(dsbtemp, 128, infile);
     fgets(dsbtemp, 128, infile);
     fgets(dsbtemp, 128, infile);
     k++;

   }

   fclose(infile);
   fclose(outfile);

   printf("\n\nProcessed %d records.\n", k);

   return (0);
}


int main()
{
char  infname[64];
char  otfname[64];

   strcpy(infname, "temp.pins");
   strcpy(otfname, "temp.evid");

   Pins2Evid(infname, otfname);

   return (0);
}

