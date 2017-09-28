#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define MAXFIELDS 100
#define MAXLINE 500
#define MAXRECORDS 100000
#define MAXSITESIZE 20
#define MAXSITES 20000


/* Struct to store the the sites latitude and longitude */
struct fac_id {
	char site[MAXSITESIZE];
        double latitude;
	double longitude;
        } facid[MAXSITES],site_1,site_2;




/*Compute the distance between two points in the earth surface.*/
double Dist(double L1, double G1, double L2, double G2)
 {
    double Distance,DG,PI,tmp,tmp1,tmp2;

    DG = 0.0;
    Distance = 0.0;
    PI = 3.141592654;
    tmp1 = 0.0;
    tmp2 = 0.0;

    L1 = (PI * L1)/180;
    G1 = (PI * G1)/180;
    L2 = (PI * L2)/180;
    G2 = (PI * G2)/180;
    DG = G1 - G2;

    tmp1 = sin(L1) * sin(L2) + cos(L1) * cos(L2) * cos(DG);
 
    tmp = acos(tmp1);

    tmp2 = (tmp/3.141592654) * 180.0;
    Distance = 1.852 * 60.0 * tmp2;
    /*printf("%f",Distance);*/
    return Distance;
    
}	



/* Function to get the latitude and longitude of a facility id */
struct fac_id get_values(char airport[MAXSITESIZE],int numsites)
{
int i,high,low;
struct fac_id tmp_site;

  strcpy(tmp_site.site,airport);
  tmp_site.latitude = 0;
  tmp_site.longitude = 0;
  i = 0;
  high = 0;
  low = 0;


  for (low=(-1), high=numsites; high-low > 1; )
  {
    i = (high+low) / 2;
    if (strcmp(airport,facid[i].site) <= 0 )
      high = i;
    else
      low = i;
  }


  if (strcmp(airport,facid[high].site) == 0)
  {
    tmp_site.latitude = facid[high].latitude;
    tmp_site.longitude = facid[high].longitude;
  }
  else
  {
    tmp_site.latitude  = -99999.0;
    tmp_site.longitude = -99999.0;
  }

  return tmp_site;

}



main (int argc, char *argv[])
{


  /* Struct to store the number of vertex and its related facility id */
  struct idx {
	int node;
	char site[MAXSITESIZE];
	} s_index[MAXRECORDS];


  FILE *ifp;
  FILE *afp;
  FILE *cfp;
  FILE *log;
  int aindex,i,j,k,o,nfields,nsubf,flag,vtxnumber,filnumb,rootnumb;
  int fac_size,idx_size,num1,num2,nfound,repflag,com_res,bflag;
  char line[MAXLINE];
  char line2[MAXLINE];
  char comando[MAXLINE];
  char idxfile[40];
  char dstfile[40];
  char side1[MAXSITESIZE];
  char side2[MAXSITESIZE];
  char confile[40];
  char *tmp;
  double dist_val;

 
  strcpy(line,"");
  strcpy(line2,"");
  strcpy(side1,"");
  strcpy(side2,"");
  aindex = 0;
  fac_size = 0;
  idx_size = 0;
  com_res = 0;
  bflag = 0;
  i = 0;
  j = 0;
  k = 0;
  o = 0;
  num1 = 0;
  num2 = 0;
  nfound = 0;
  repflag = 0;
  dist_val = 0;

  system("date > cg.log");
  

  /* Initialization */
  for(i=0;i<MAXSITES;i++)
  {
    strcpy(facid[i].site,"");
    facid[i].latitude=0;
    facid[i].longitude=0;
  };

  strcpy(site_1.site,"");
  site_1.latitude = 0;
  site_1.longitude = 0;

  strcpy(site_2.site,"");
  site_2.latitude = 0;
  site_2.longitude = 0;
  
  /* Arguments validation */
  if (argc != 4)
  {
    printf("Usage: cg fac_ids_filename index_filename output_filename\n");
    return 1;
  }

  /* Openning the data file */
  if ((afp = fopen(argv[1],"r")) == NULL)
  {
    printf("Error at open file: %s\n",argv[1]);
    return 1;
  }
  else  /* Reading the airport values */
  {
    aindex = 0;
    while (fgets(line,MAXLINE,afp) != NULL)
    {
      strcpy(facid[aindex].site,strtok(line,":"));
      facid[aindex].latitude = atof(strtok(NULL,":"));
      facid[aindex].longitude = atof(strtok(NULL,":"));
      aindex++;
    }
    fac_size = aindex - 1;
  }


  /* Openning the second data file */
  if ((ifp = fopen(argv[2],"r")) == NULL)
  {
    printf("Error at open file: %s\n",argv[2]);
    return 1;
  }
  else  /* Reading the index values */
  {
    aindex = 0;
    while (fgets(line,MAXLINE,ifp) != NULL)
    {
      s_index[aindex].node = atoi(strtok(line,":")); 
      strcpy(s_index[aindex].site,strtok(NULL,":"));
      aindex++;
    }
    idx_size = aindex - 1;
  }

  strcpy(confile,"");
  strcpy(confile,argv[3]);
  cfp = fopen(confile,"w");

  i = 0;

  /* Cycle to give a pass to the first site */
  while ( i < idx_size )
  {
    fflush(cfp);
    strcpy(side1,s_index[i].site);
    if (strchr(side1,'\n') != NULL)
    {
      tmp = strchr(side1,'\n');
      *tmp = '\0';
    }

    site_1 = get_values(side1,fac_size);

    if (site_1.latitude == -99999.0 && site_1.longitude == -99999.0)
    {
      i++;
      continue;
    }


    num1 = s_index[i].node;
    j = i + 1;

    /* Cycle give a pass to the second facility id */
    while ( j < idx_size )
    {
      strcpy(side2,s_index[j].site);
      if (strchr(side2,'\n') != NULL)
      {
        tmp = strchr(side2,'\n');
        *tmp = '\0';
      }

      site_2 = get_values(side2,fac_size);
      num2 = s_index[j].node;
      k = 0;
      bflag = 0;

      if (site_2.latitude == -99999.0 && site_2.longitude == -99999.0)
      {
        j++;
        continue;
      }

      /* Calculating the distance between site1 and site2 */
      dist_val = Dist(site_1.latitude,site_1.longitude,site_2.latitude,site_2.longitude);      

      /* If the distance between site1 and site2 is less than 200 km then a
         near_to edge is created from site1_vertex_number to
         site2_vertex_number */
      if (dist_val < 200)
      {
        fprintf(cfp,"%s %i %i %s\n","u ",num1,num2,"near_to");
      }

      j++;
    }
    i++;
  }  

  close(cfp);
  close(ifp);
  close(afp);

  system("date >> cg.log");
}
