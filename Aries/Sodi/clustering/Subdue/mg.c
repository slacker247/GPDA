#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXFIELDS 100
#define MAXLINE 5000
#define TRUE 0
#define FALSE 1




/* This function separates the multiple values of a field in sub-fields,
   the result is stored in subf */
int get_subfields(char * subf[], char line[], char *tmp2,char *tmp3)
{
  int num_sf,i,j,ltmp,new_nfield,rep_flag;

  num_sf = 0;
  new_nfield = 0;
  for(i = 0 ; i < strlen(line) ; i++)
  {
    if (line[i] == ';')
      num_sf++;
  }
  num_sf++;

  if (num_sf > 1)
  {
    strcpy(tmp2,"");
    strcpy(tmp2,strtok(line,";"));

    if (tmp2[0] == ' ')
    {
      for (i = 0; i < strlen(tmp2)-1; i++)
        tmp3[i] = tmp2[i+1]; 
      tmp3[i+1] = '\0';
      strcpy(tmp2,tmp3);
      if (strchr(tmp2,'\n') == NULL)
        strcat(tmp2,"\n");
      
    } 
    else
    {
      if (strchr(tmp2,'\n') == NULL)
        strcat(tmp2,"\n");
    }
    strcpy(subf[0],tmp2);
    new_nfield++;
  }

  if (num_sf > 1)
  {
    for(i = 1 ; i < num_sf ; i++)
    {
      strcpy(tmp2,"");
      tmp2 = strtok(NULL,";");
      if (tmp2 != NULL && tmp2 != " ")
      {
        if (tmp2[0] == ' ')
        {
          strcpy(tmp3,"");
          for (j = 0; j < strlen(tmp2)-1; j++)
            tmp3[j] = tmp2[j+1]; 
          tmp3[j] = '\0';
          strcpy(tmp2,tmp3);
          if (strchr(tmp2,'\n') == NULL)
            strcat(tmp2,"\n");
        }
      }
      /* Eliminating repeated fields */
      rep_flag = 0;
      for (j=0;j<=new_nfield;j++)
      {
        if (subf[j] != NULL && tmp2 != NULL)
        {
          if (strcmp(tmp2,subf[j]) == 0)
          {
            rep_flag = 1;
          }
        }
	else rep_flag = 1;
      }
      if (rep_flag == 0 && tmp2 != NULL)
      {
        strcpy(subf[new_nfield],tmp2);
        new_nfield++;
      }
    }
  }
  else
  {
    strcpy(tmp2,"");
    strcpy(tmp2,line);
    strcpy(subf[0],tmp2);
    new_nfield = 1;
  }

    return new_nfield;

}





/* Substitutes spaces by the character _ in a string */
int fill_field(char * f)
{
  int flen,i,flag;

  flen = 0;
  i = 0;
  flag = 0;

  flen = strlen(f);

  for (i=0;i<=flen;i++)
    if(f[i] == ' ')
    {
      f[i] = '_';
      flag = 1;
    }
  return flag;
}





main (int argc, char *argv[])
{
  FILE *dfp;
  FILE *vfp;
  FILE *efp;
  FILE *ifp;
  int i,j,m,nfields,nsubf,flag,filnumb,rootnumb;
  int empty,other;
  long vtxnumber;
  char line[MAXLINE];
  char line2[MAXLINE];
  char *fnameptr[MAXFIELDS];
  char *subflist[MAXFIELDS];
  char *tmp,*tmp2,*tmp3,*tmp4;
  char vtxfile[40];
  char edgfile[40];
  char idxfile[40];
  char rootnode[40];
 
  
  /* Variables initialization */
  strcpy(line,"");
  strcpy(rootnode,"");
  for(i=0;i<MAXFIELDS;i++)
    {
      fnameptr[i] = NULL;
      subflist[i] = malloc(MAXLINE);
    }
  tmp2 = malloc(MAXLINE);
  tmp3 = malloc(MAXLINE);
  tmp4 = malloc(MAXLINE);
  i = 0;
  j = 0;
  m = 0;
  nfields = 0;
  nsubf = 0;
  flag = 0;
  vtxnumber = 1;
  filnumb = 0;
  rootnumb = 0;
  empty = FALSE;
  other = FALSE;

  /* Arguments validation */
  if (argc != 2)
  {
    printf("Usage: mg filename\n");
    return 1;
  }
  /* Openning the data file */
  if ((dfp = fopen(argv[1],"r")) == NULL)
  {
    printf("Error at open file: %s\n",argv[1]);
    return 1;
  }
  else /*Giving values to the field names array */
  {
    /* Reading the data file */
    strcpy(line,"X");
    nfields = 0;
    fscanf(dfp,"%s",line);
   
    /* Reading the field names */
    while(strcmp(line,"--ER--") != 0)
    {
      for (j=0;j<strlen(line)-1;j++)
        line2[j] = line[j+1];
      line2[j-1] = '\0';
      strcpy(line,line2);
      tmp = malloc(strlen(line));
      strcpy(tmp,"");
      strcpy(tmp,line);
      fnameptr[nfields] = tmp; 
      nfields++;
      fscanf(dfp,"%s",line);
    }
    nfields = nfields -1;
  }

  /* Creating the name of the vertex file, argv1 + .vertex */
  strcpy(vtxfile,"");
  strcpy(vtxfile,argv[1]);
  strcat(vtxfile,".vertex");


  /* Creating the name of the edges file, argv1 + .edges */
  strcpy(edgfile,"");
  strcpy(edgfile,argv[1]);
  strcat(edgfile,".edges");

  /* Creating the name of the index file, argv1 + .index */
  strcpy(idxfile,"");
  strcpy(idxfile,argv[1]);
  strcat(idxfile,".index");

  vfp = fopen(vtxfile,"w");
  efp = fopen(edgfile,"w");
  ifp = fopen(idxfile,"w");

  
  vtxnumber = 1;
  filnumb = 0;
  strcpy(rootnode,"EVENT");
  rootnumb = 0;

  fprintf(vfp,"%s %i %s\n","v",vtxnumber,"EVENT");
  rootnumb = vtxnumber;
  vtxnumber++;

  fgets(line,MAXLINE,dfp);

  /* Cycle to read the file records and create vertex, edges and index files */
  while (fgets(line,MAXLINE,dfp) != NULL)
  {
    if(strcmp(line,"--ER--\n") != 0)
    {
      for (j=0;j<strlen(line)-2;j++)
        line2[j] = line[j+1];
      line2[j-1] = '\0';
      strcpy(line,line2);
      strcat(line,"\n");

      /* This if eliminates the empty values of some fields in some records */
      if(strcmp(line,"\n") != 0 && strcmp(line,";\n") != 0 && strcmp(line,"") !=0 )
      {
        nsubf = 0;
        nsubf = get_subfields(subflist,line,tmp2,tmp3);
        for(i=0;i<nsubf;i++)
        {
          if (strcmp(subflist[i],"\n") !=0 && strcmp(subflist[i],"") != 0)
          if (strstr(subflist[i],"OTHER") == NULL)
          {
            if(strchr(subflist[i],'\n') == NULL)
            {
              m = fill_field(subflist[i]);
	      /* Printing the vertexes */
              fprintf(vfp,"%s %i %s\n","v",vtxnumber,subflist[i]);
            }
            else
            {
              m = fill_field(subflist[i]);
	      /* Printing the vertexes */
              fprintf(vfp,"%s %i %s","v",vtxnumber,subflist[i]);
            }
            /* Printing the edges */
            fprintf(efp,"%s %i %i %s\n","d",rootnumb,vtxnumber,fnameptr[filnumb]);
	    /* Printing the information necessary to connect the graph
	       in an index file */
            if (strcmp(fnameptr[filnumb],"FAC_ID") == 0)
              fprintf(ifp,"%i:%s",rootnumb,subflist[i]);
            vtxnumber++;
          }
        }
        filnumb++;
      }
      else
      {
        if (empty == TRUE) /* Flag in case we want to use empty vertexes */
        {
          fprintf(vfp,"%s %i %s\n","v",vtxnumber,"EMPTY");
          fprintf(efp,"%s %i %i %s\n","d",rootnumb,vtxnumber,fnameptr[filnumb]);
          if (strcmp(fnameptr[filnumb],"FAC_ID") == 0)
            fprintf(ifp,"%i:%s\n",rootnumb,"EMPTY");
          vtxnumber++;
          filnumb++;
        }
        else
          filnumb++;
      }
    }
    else
    {
      fprintf(vfp,"%s %i %s\n","v",vtxnumber,"EVENT");
      rootnumb = vtxnumber;
      vtxnumber++;
      filnumb = 0;
    }

 
  }


  close(dfp);
  close(vfp);
  close(efp);
  close(ifp);
}
