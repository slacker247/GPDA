/*    Drive the function "apogee" by using the spread sheet values. */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define PI       3.141592654
#define RE    6378.145
#define TU     806.8118744
#define OM       0.0588336565
#define VCON     7.90536828
#define ElSq     0.006694317778

int apogee (double t_no_use,
            double r1[],
            double tgt[],
            double hapo,
            double *tof,
            double v1[]      )

{
   int    interations = 0;
   double lat2, lon2, alt;
   double r1x, r1y, r1z;
   double r2x, r2y, r2z;
   double cx,  cy,  cz;
   double r1_mag, r2_mag, ra_mag;
   double old_tof, t_TU;
   double alpha, nu;
   double C_nu, S_nu;
   double aa, bb, cc;
   double AA, BB, CC;
   double U_sqrt, C_nu1;
   double nu1, nu2;
   double ecen, param, sma;
   double E1, E2, sma_to_3half;
   double C_E1, S_E1, C_E2, S_E2;
   double v1_mag_AU, v1_mag_mks, gamma;

   lat2 = tgt[0];
   lon2 = tgt[1];
   alt  = tgt[2];

   if (t_no_use < 0.0)
      {
         printf ("The t_no_use is %12.4f.\n", t_no_use);
         printf ("The burn out position is %12.4f, %12.4f, and %12.4f km.\n",
                         r1[0], r1[1], r1[2]);
         printf ("The impact lat is %12.4f deg.\n", lat2);
         printf ("The impact lon is %12.4f deg.\n", lon2);
         printf ("The impact alt is %12.4f km.\n", alt);
         printf ("The apogee alt is %12.4f km.\n", hapo);
         printf ("The time of flight is %12.4f km.\n", *tof);
         printf ("The old burn out velocity is %9.6f, %9.6f, and %9.6f km/sec.\n",
                         v1[0], v1[1], v1[2]);
      }

   ra_mag = (RE + hapo)/RE;
   r1_mag = sqrt (r1[0]*r1[0] + r1[1]*r1[1] + r1[2]*r1[2]);
   r1_mag = r1_mag / RE;
   r1x    = (r1[0]/RE)/r1_mag;
   r1y    = (r1[1]/RE)/r1_mag;
   r1z    = (r1[2]/RE)/r1_mag;

   if (t_no_use < 0.0)
      {
         printf ("The burn out radius is %9.6f DU.\n", r1_mag);
         printf ("The burn out unit vector is %9.6f, %9.6f, and %9.6f DU.\n",
                         r1x, r1y, r1z);
         printf ("The apogee radius is %9.6f DU.\n", ra_mag);
      }

   if (ra_mag <= r1_mag)
      return -1;

   do
      {
         old_tof = *tof;
         t_TU    = old_tof/TU;
         alpha   = PI * lon2/180.0 - t_TU * OM;
         r2x     = cos(PI * lat2/180.0) * cos(alpha);
         r2y     = cos(PI * lat2/180.0) * sin(alpha);
         r2z     = sin(PI * lat2/180.0);
         C_nu    = r1x*r2x + r1y*r2y + r1z*r2z;

         if (t_no_use < 0.0)
               printf ("The cosine of nu is %9.6f.\n", C_nu);

         if (fabs(C_nu) > 1.0001)
            return -2;
         if (C_nu < -1.0)
            C_nu = -1.0;
         if (C_nu > 1.0)
            C_nu = 1.0;

         nu      = acos(C_nu);
         S_nu    = sin(nu);
         r2_mag  = 1.0 + alt/RE;

         if (t_no_use < 0.0)
            {
               printf ("The impact radius is %9.6f DU.\n", r2_mag);
               printf ("The impact unit vector is %9.6f, %9.6f, and %9.6f DU.\n",
                               r2x, r2y, r2z);
               printf ("The earth turns  %12.4f degrees during tof.\n", 180.0 * alpha/PI);
               printf ("The angle between burn out and impact is %12.4f deg.\n", 180.0 * nu/PI);
            }

         aa = ra_mag * (r1_mag - r2_mag);
         bb = r1_mag * (ra_mag - r2_mag) - r2_mag * (ra_mag - r1_mag) * C_nu;
         cc = -r2_mag * (ra_mag - r1_mag) * S_nu;
         AA = bb * bb + cc * cc;

         if (t_no_use < 0.0)
            {
               printf ("The aa coef is %12.6f.\n", aa);
               printf ("The bb coef is %12.6f.\n", bb);
               printf ("The cc coef is %12.6f.\n", cc);
               printf ("The AA coef is %12.6f.\n", AA);
            }

         if (AA < 0.00001)
            return -3;

         BB = 2.0 * aa * bb;
         CC = aa * aa - cc * cc;
         U_sqrt = BB * BB - 4.0 * AA * CC;

         if (t_no_use < 0.0)
            {
               printf ("The BB coef is %12.6f.\n", BB);
               printf ("The CC coef is %12.6f.\n", CC);
               printf ("Under the root is %12.6f.\n", U_sqrt);
            }

         if (U_sqrt < 0.00001)
            return -4;

         C_nu1 = (-BB - sqrt(U_sqrt))/(2.0 * AA);

         if (t_no_use < 0.0)
            printf ("COS(nu1) is %12.6f.\n", C_nu1);

         if (fabs(C_nu1) > 1.0001)
            return -5;
         if (C_nu1 < -1.0)
            C_nu1 = -1.0;
         if (C_nu1 > 1.0)
            C_nu1 = 1.0;

         nu1   = acos(C_nu1);
         nu2   = nu1 + nu;
         ecen  = (ra_mag - r1_mag) / (ra_mag + r1_mag * C_nu1);
         param = ra_mag * (1.0 - ecen);
         sma   = param / (1.0 - ecen * ecen);

         if (t_no_use < 0.0)
            {
               printf ("The burn out true anomaly is %9.4f deg.\n", 180.0*nu1/PI);
               printf ("The impact   true anomaly is %9.4f deg.\n", 180.0*nu2/PI);
               printf ("The eccentricity is %9.6f.\n", ecen);
               printf ("The parameter is %9.6f DU.\n", param);
               printf ("The semi-major axis is %9.6f DU.\n", sma);
            }

         if (sma < 0.00001)
            return -6;

         sma_to_3half = sqrt(sma*sma*sma);
         C_E1         = (ecen + cos(nu1)) / (1.0 + ecen * cos(nu1));
         C_E2         = (ecen + cos(nu2)) / (1.0 + ecen * cos(nu2));

         if (t_no_use < 0.0)
            {
               printf ("Semi-major axis to the 3/2 is %6.6f DU.\n", sma_to_3half);
               printf ("The burn out COS of ecen anomaly is %9.6f.\n", C_E1);
               printf ("The impact   COS of ecen anomaly is %9.6f.\n", C_E2);
            }

         if (fabs(C_E1) > 1.0001)
            return -7;
         if (C_E1 < -1.0)
            C_E1 = -1.0;
         if (C_E1 > 1.0)
            C_E1 = 1.0;

         if (fabs(C_E2) > 1.0001)
            return -8;
         if (C_E2 < -1.0)
            C_E2 = -1.0;
         if (C_E2 > 1.0)
            C_E2 = 1.0;

         E1 = acos(C_E1);
         E2 = 2.0*PI - acos(C_E1);
         S_E1 = sin(E1);
         S_E2 = sin(E2);
         t_TU = sma_to_3half * (E2 - ecen*S_E2 - E1 + ecen*S_E1);
         *tof = t_TU * TU;
         interations += 1;

         if (t_no_use < 0.0)
            {
               printf ("The burn out  ecen anomaly is %9.4f deg.\n", 180.0*E1/PI);
               printf ("The impact    ecen anomaly is %9.4f deg.\n", 180.0*E2/PI);
               printf ("The new tof is %6.6f  TU.\n", t_TU);
               printf ("The new tof is %9.4f sec.\n", *tof);
               printf ("The old tof - new tof is %9.6f sec.\n", old_tof-*tof);
               printf ("This concludes interation %d.\n\n", interations);
            }

         if (interations > 20)
            return -9;
      }
   while(fabs(*tof - old_tof) > .001); 

   v1_mag_AU  = sqrt(1.0/param) * sqrt(1.0 + 2.0*ecen*cos(nu1) + ecen*ecen);
   v1_mag_mks = v1_mag_AU * VCON;
   gamma      = atan2(ecen*sin(nu1), 1.0+ecen*cos(nu1));

   cx = r1y*r2z - r1z*r2y;
   cy = r1z*r2x - r1x*r2z;
   cz = r1x*r2y - r1y*r2x;

   v1[0] = v1_mag_mks*(sin(gamma)*r1x + cos(gamma)*(cy*r1z - cz*r1y));
   v1[1] = v1_mag_mks*(sin(gamma)*r1y + cos(gamma)*(cz*r1x - cx*r1z));
   v1[2] = v1_mag_mks*(sin(gamma)*r1z + cos(gamma)*(cx*r1y - cy*r1x));

   if (t_no_use < 0.0)
      {
         printf ("The burn out velocity magintude is %10.6f DU/TU.\n", v1_mag_AU);
         printf ("The burn out velocity magintude is %10.6f km/sec.\n", v1_mag_mks);
         printf ("The burn out angle              is %10.4f deg.\n", 180.0*gamma/PI);
         printf ("The new burn out velocity is %9.6f, %9.6f, and %9.6f km/sec.\n\n",
                         v1[0], v1[1], v1[2]);
      }
   
return interations;

}
