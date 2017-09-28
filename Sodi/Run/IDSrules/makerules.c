#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h> 
#include <time.h>

main(int argc, char *argv[])
{
FILE *sitefp;
FILE *outfp;
FILE *fp;
int  i, ruleno;
char tgttemp[1024];
char source[32];
char chfile[64];
char chagain[32];

	sitefp = fopen("rules.list", "r");

	ruleno = 0;
	for (i=0; i<33; i++) {
	  fscanf(sitefp, "%s", chfile);
	  printf("Processing rules file %s\n", chfile);
	  fp = fopen(chfile, "r");
	  while (!feof(fp)) {
	    fgets(tgttemp, 1024, fp);
	    if (tgttemp[0] == 'a') {
	      ruleno++;
	      sprintf(source, "rule.%d", ruleno);
	      outfp = fopen(source, "w");
	      fprintf(outfp, "%s", tgttemp);
	      fclose(outfp);
	      printf("Rule file %s written\n", source);
	    }
	  }
	  fclose(fp);
	}
	fclose(sitefp);
}
