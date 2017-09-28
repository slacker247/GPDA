      subroutine Distance (ddist, n, xyz, ndim, m) 

      include 'shared.h'

      real ddist (n, MAX_NUM_THREATS), xyz (ndim,3)

      do 20 j = 1, num_threats
         if (threat_play (j)) then
           x_ecr = COS (we*threat_time (j))*threat_state (j,1) +
     *             SIN (we*threat_time (j))*threat_state (j,2)
           y_ecr =-SIN (we*threat_time (j))*threat_state (j,1) +
     *             COS (we*threat_time (j))*threat_state (j,2)
           z_ecr = threat_state (j,3)

          do 10 i = 1, m
             ddist (i,j) = SQRT ((xyz (i,1) - x_ecr)**2 +
     *                           (xyz (i,2) - y_ecr)**2 +
     *                           (xyz (i,3) - z_ecr)**2)
   10      continue
         endif
   20 continue

      return
      end
