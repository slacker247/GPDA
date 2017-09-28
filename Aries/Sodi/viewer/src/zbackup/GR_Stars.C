#include <stdio.h>
#include <math.h>
//#include "GR_Interface.H"
#include "GR_Stars.H"

#define MAXLINE	200

GR_Stars::GR_Stars(short R, short G, short B, char* filename, long type)
{
char oneline[MAXLINE];
FILE *fp;
int skip_lines = 5, i;
int starcount = 0;
int lineno, nlines;
int starnum, catno;
char dontknow[10];
float intensity, rightasc, declin, x, y, z; 
float dtor, sinasc, cosasc, sindec, cosdec;
float baseyear;
float currentyear;
float T2;
float DeltaX, DeltaY, DeltaZ, DeltaT;
float w,d;
float DPP;
float vector[3];

   if (filename == NULL)
     p_filename = "./RSD_Data/stars.dat";
   else
     p_filename = filename;
   fp = fopen (p_filename, "r+");
   if (fp == NULL)
   {
      perror ("GR_Stars: Error opening star data file.");
      return;
   }
   p_type = type;
   p_R = R;
   p_G = G;
   p_B = B;

	dtor = atan(1.)/45.0;
	DPP  = 0.05*dtor;
	currentyear = 1997;
	baseyear = 1950;

	p_gr_objid = GR_genobj();
        p_gr_objid = p_type;
	GR_makeobj(p_gr_objid);
	GR_pushmatrix();
//	viewstars();
      	GR_rotate(0,'z');
	GR_scale(10.0, 10.0, 10.0);
	GR_color(R, G, B);
	GR_bgnpoint();

	lineno = 0;
        fgets(oneline, MAXLINE, fp);
        sscanf(oneline, "%d", &nlines);
//      printf("GR_Stars: processing %d stars\n", nlines);
	for (i=0; i<nlines; i++)
   	   {
                fgets(oneline, MAXLINE, fp);
		sscanf(oneline, "%d %f %s %f %f %f %f %f %d",
		       &starnum, &intensity, dontknow, &rightasc, &declin,
		       &x, &y, &z, &catno); 
/*
*       Convert degrees to radians
*/
		declin = declin*dtor;
		rightasc = rightasc*dtor;
/*
*       Rotate universe to current year
*/
		T2 = ((baseyear+currentyear)/2.0 - 1900.0)/100.0;
		DeltaX = 3.07234 + (0.00186*T2);
		DeltaY = 20.0468 - (0.00850*T2);
		DeltaZ = DeltaY/15.0;
		DeltaT = currentyear - baseyear;
		w = 0.00420*DeltaT*(DeltaX + (DeltaZ*sin(rightasc)*
		      tan(declin)));
		d = 0.00028*DeltaT*DeltaY*cos(rightasc);
		rightasc = rightasc+w;
		declin   = declin+d;
/*
*       Calculate angles to the currently active star
*/
		sindec = sin(declin);
		cosdec = cos(declin);
		sinasc = sin(rightasc);
		cosasc = cos(rightasc);
		z = sindec;
		y = cosdec*sinasc;
		x = cosdec*cosasc;
		vector[0] = x; vector[1] = y; vector[2] = z;
	        GR_v3f(vector);

		if ( intensity < 4. )
			{
				sindec = sin((declin+DPP));
				cosdec = cos((declin+DPP));
				z = sindec;
				y = cosdec*sinasc;
				x = cosdec*cosasc;
				vector[0] = x; vector[1] = y; vector[2] = z;
				GR_v3f(vector);
			}
		if ( intensity < 3. )
			{
				sinasc = sin((rightasc+DPP));
				cosasc = cos((rightasc+DPP));
				z = sindec;
				y = cosdec*sinasc;
				x = cosdec*cosasc;
				vector[0] = x; vector[1] = y; vector[2] = z;
				GR_v3f(vector);
			}
		if ( intensity < 2. )
			{
				sindec = sin(declin);
				cosdec = cos(declin);
				z = sindec;
				y = cosdec*sinasc;
				x = cosdec*cosasc;
				vector[0] = x; vector[1] = y; vector[2] = z;
				GR_v3f(vector);
			}

	      }
	GR_endpoint();
	GR_popmatrix();
	GR_closeobj();

	fclose(fp);
}

/*
 *  'displaystars' displays the stars object
 */
void
GR_Stars::objdraw()
{
	GR_callobj(p_gr_objid);
}

/*
 *  'changestars' changes the viewing transformation for the stars
 
changestars()
{
  editobj(stars);
  objreplace(1);
  viewstars();
  closeobj();
}

updatestars(time)
float time;
{
  editobj(stars);
    objreplace(2);
    if (rotate_on)
      rotate(0,'z');
    else
      rotate((int)(-time/24.),'z');
  closeobj();
}

*/


/*
 *  'initstars' initializes the stars software
 *
initstars()
{
	stars = 0;
}
*/
