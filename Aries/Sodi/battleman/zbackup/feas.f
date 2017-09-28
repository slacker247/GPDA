      subroutine Feas
 
      include 'shared.h'

      real max_coast, laz, angle (2), troot, tdum, angdum(2), miss
      real t_state(6), tl1, arg, site1, site2, tl2, rint0,
     *     rmax0, tc, tl, rint, tcross, cross, troot10,
     *     tc0, xv, xalt, r, clang, clvel, probk, state, xr

      real alt (MAX_NUM_THREATS), xyl   (MAX_NUM_SITES), 
     *     ri  (MAX_NUM_THREATS), site3 (MAX_NUM_THREATS) 

      real rintmin (MAX_NUM_SITES, MAX_NUM_THREATS), 
     *     rintmax (MAX_NUM_SITES, MAX_NUM_THREATS),
     *     tcmax   (MAX_NUM_SITES, MAX_NUM_THREATS),
     *     tcmin   (MAX_NUM_SITES, MAX_NUM_THREATS),
     *     tsol    (MAX_NUM_SITES, MAX_NUM_THREATS),
     *     angsol  (MAX_NUM_SITES, MAX_NUM_THREATS),
     *     probability_of_kill (MAX_NUM_SITES, MAX_NUM_THREATS)

      integer status (MAX_NUM_SITES, MAX_NUM_THREATS)
      integer found_tcmin (MAX_NUM_SITES, MAX_NUM_THREATS),
     *        jmax        (MAX_NUM_SITES, MAX_NUM_THREATS)

      integer i, j, k, nsol, prev, nfound, ndum, m

      real ddist (MAX_NUM_SITES, MAX_NUM_THREATS)
      real x_ecr (100), y_ecr (100), z_ecr (100)
      real threat_save (100,6)
 
      do 2 j = 1, num_threats
         x_ecr (j) = threat_state (j,1)
         y_ecr (j) = threat_state (j,2)
         z_ecr (j) = threat_state (j,3)

	 threat_save (j,1) = threat_state (j,1)
	 threat_save (j,2) = threat_state (j,2)
	 threat_save (j,3) = threat_state (j,3)
	 threat_save (j,4) = threat_state (j,4)
	 threat_save (j,5) = threat_state (j,5)
	 threat_save (j,6) = threat_state (j,6)
 
         do 1 i = 1, num_weapon_sites
            ddist (i,j) = SQRT ((site_xyz (i,1) - x_ecr (j))**2 +
     *                          (site_xyz (i,2) - y_ecr (j))**2 +
     *                          (site_xyz (i,3) - z_ecr (j))**2)
   1     continue
   2  continue

      rearth = rearth/1000.0
      do 10 j = 1, num_threats
	threat_state (j,1) = x_ecr (j)/1000.0
	threat_state (j,2) = y_ecr (j)/1000.0
	threat_state (j,3) = z_ecr (j)/1000.0
 	threat_state (j,4) = threat_state(j,4)/1000.0
 	threat_state (j,5) = threat_state(j,5)/1000.0
 	threat_state (j,6) = threat_state(j,6)/1000.0
        ri (j) = SQRT (threat_state (j,1)**2 + threat_state (j,2)**2 +
     *                 threat_state (j,3)**2)
        alt (j) = ri (j) - rearth
	if (switch) jeff_time (j) = threat_time (j)
   10 continue

      switch = .FALSE.

      do 5 i = 1, num_weapon_sites
	do 5 j = 1, num_threats
	  if (threat_play (j)) then
	      status (i,j) = 1
	    else
	      status (i,j) = 10
	  endif
	  tcmax (i,j) = threat_time (j) - (jeff_time (j) + total_delay)
	  tcmin (i,j) = dtc
   5  continue
	  
      do 50 j = 1, num_threats
	 do 40 i = 1, num_weapon_sites
	   if (tcmax (i,j) .LT. dtc) status (i,j) = 20
   40    continue
	 if (alt (j) .GT. max_alt) then
	   do 20 i = 1, num_weapon_sites
	     status (i,j) = 30
   20      continue
	 endif
	 if (alt (j) .LT. min_alt) then
	   do 30 i = 1, num_weapon_sites
	     status (i,j) = 40
   30      continue
	 endif
   50 continue

c     call Status_out (status, 50)

c  compute range to intercept at smallest coast time

      do 60 i = 1, num_weapon_sites
	xyl   (i) = COS (gbi_lat(i))
	site3 (i) = SIN (gbi_lat (i))
   60  continue

      do 80 i = 1, num_weapon_sites
	do 70 j = 1, num_threats
	  if (status (i,j) .EQ. 1) then
	    tl1 = threat_time (j) - (tcmin (i,j) + total_delay)
	    arg = gbi_lon (i) + we*tl1 + mustepan
	    site1 = xyl (i)*COS (arg)
	    site2 = xyl (i)*SIN (arg)
	    rintmin (i,j) = rearth*ACOS (
     *                   (site1*threat_state (j,1) +
     *                    site2*threat_state (j,2) +
     *                    site3 (i)*threat_state (j,3))/ri (j))

	    tl2 = jeff_time (j) + total_delay
	    arg = gbi_lon (i) + we*tl2 + mustepan
	    site1 = xyl (i)*COS (arg)
	    site2 = xyl (i)*SIN (arg)
	    rintmax (i,j) = rearth*ACOS (
     *                   (site1*threat_state (j,1) +
     *                    site2*threat_state (j,2) +
     *                    site3 (i)*threat_state (j,3))/ri (j))
	  endif
   70   continue
   80 continue

c  perform initial check to see if target is in range

      do 100 i = 1, num_weapon_sites
	do 90 j = 1, num_threats
	  if (status (i,j) .EQ. 1) then
            if ((rintmin (i,j) .GT. max_range) .AND.
     *          (rintmax (i,j) .GT. max_range)) status (i,j) = 50
	  endif
   90   continue
  100 continue

c     call Status_out (status, 100)

c  find valid coast time window for all threats and gbis

      do 130 i = 1, num_weapon_sites
	do 120 j = 1, num_threats
	  if (status (i,j) .EQ. 1) then
            jmax (i,j) = MIN0 (num_tc, IFIX(tcmax (i,j)/dtc))
	    tcmax (i,j) = jmax (i,j)*dtc
	    found_tcmin (i,j) = 0
	  endif
  120   continue
  130 continue

      do 160 i = 1, num_weapon_sites 
	do 150 j = 1, num_threats
	  if (status (i,j) .EQ. 1) then
	  rint0 = 0.0
	  rmax0 = 0.0
          do 140 k = 1, jmax (i,j)
	    tc = dtc * k
	    tl = threat_time (j) - tc - burn_time
	    arg = gbi_lon (i) + we*tl + mustepan
	    site1 = xyl (i)*COS (arg)
	    site2 = xyl (i)*SIN (arg)

c  compute range from launch site to threat

	    rint = rearth*ACOS (
     *                   (site1*threat_state (j,1) +
     *                    site2*threat_state (j,2) +
     *                    site3 (i)*threat_state (j,3))/ri (j))

c  check if rint curve crosses rmax curve
      
	    cross = (rmax (k) - rint)*(rmax0 - rint0)
	    if (cross .LT. 0.0) then

c  interpolate time value where curves cross

	      tcross = (tc - dtc)+dtc*(rint0 - rmax0)/
     *                   (rmax (k) - rmax0 - rint + rint0)

	      if (found_tcmin (i,j) .EQ. 0) then
		  found_tcmin (i,j) = 1
		  tcmin (i,j) = tcross
		else
		  tcmax (i,j) = tcross
		  go to 145
	      endif
  	    endif
	    rint0 = rint
	    rmax0 = rmax (k)
  140     continue

c  if pairs never cross the pairs are not feasible

	  if (found_tcmin (i,j) .EQ. 0) then
	    status (i,j) = 60
	    go to 150
	  endif

c  check minimum coast time against the amount of time available

  145     continue
	  max_coast  = threat_time (j) - (jeff_time (j) + total_delay)
	  if (tcmin (i,j) .GT. max_coast) then
	    status (i,j) = 70
	    go to 150
	  endif
	  endif
  150   continue
  160 continue

c     call Status_out (status, 160)

      do 200 i = 1, num_weapon_sites 
	do 190 j = 1, num_threats
	  if (status (i,j) .EQ. 1) then
            prev    = 0
            nsol    = 0
            troot10 = 0.0
            tc0     = 0.0
            jmax (i,j) = INT ((tcmax (i,j) - tcmin (i,j))/dtc ) + 2
 
            do 180 k = 1, jmax (i,j)
              tc = tcmin (i,j) + (k-1)*dtc                 !coast time (sec)
              tl = threat_time (j) - (tc + burn_time)     !launch time (sec)
              arg = gbi_lon (i) + we*tl + mustepan   !earth rotation angle (rad)
              site1 = xyl (i)*COS (arg)
              site2 = xyl (i)*SIN (arg)
	      rint = rearth*ACOS ((site1*threat_state (j,1)+
     *                           site2*threat_state (j,2)+
     *                           site3 (i)*threat_state (j,3))/ri (j))
              call Giroots (rint, alt (j), angle, troot, nfound)
              xr = (rearth + alt(j))*sin(rint/rearth)  
              xalt = (rearth + alt(j))*cos(rint/rearth)
              if(nfound .EQ. 0) then
                prev = 0
                go to 180
              endif

              if (prev .EQ. 0) go to 175
              cross = (tc - troot)*(tc0 - troot10)
              if (cross .LT. 0.0) then
                r = rint0 + (troot10 - tc0)*(rint - rint0)/
     &                      (dtc - troot + troot10)
                call Giroots (r,alt (j), angdum, tdum, ndum)
c              ---------------------------------------------------------------
c              - Check that coast time is not longer than maximum coast time -
c              - or less than minimum coast time, if so, try a later time    -
c              ---------------------------------------------------------------
                if (tdum .GT. num_tc*dtc) go to 175
                if (tdum .LT. dtc + wpn_endoacq) go to 175
		nsol = 1
                tsol (i,j)   = tdum
                angsol (i,j) = angdum(1)
                go to 190                     !have found the early solution
              endif

  175         continue
              prev    = 1
              rint0   = rint
              tc0     = tc
              troot10 = troot
  180       continue

            if (nsol .LT. 1) then
              status (i,j) = 80
            endif
 
	  endif
  190   continue
  200 continue

c     call Status_out (status, 200)
 
c        -------------------------------------------------------------
c        - Determine the launch site ECI position and launch azimuth -
c        -------------------------------------------------------------
      do 220 i = 1, num_weapon_sites 
	do 210 j = 1, num_threats
	  if (status (i,j) .EQ. 1) then
            tl      = threat_time (j) - tsol (i,j) - burn_time
            arg     = gbi_lon (i) + we*tl + mustepan
            site1 = xyl (i)*COS (arg)
            site2 = xyl (i)*SIN (arg)
            call Gilnchaz (site1, site2, site3 (i),
     *              threat_state (j,1), threat_state (j,2), 
     *              threat_state (j,3),laz)

 
c        -----------------------------------
c        - Determine IV state at intercept -
c        -----------------------------------
            call Gistate (i,tl,laz,angsol (i,j), threat_time (j),state)
 
c        -------------------------------------------
c        - Compute estimated Pk for pair:          -
c        - Use Pk table for RVs, since we wouldn't -
c        - be bothering if we thought              -
c        - the target was a penaid!                -
c        -------------------------------------------
            do 205 m = 1,6
               t_state (m) = threat_state (j,m)
  205       continue
            call Giprobk (j, state, t_state, probk, clang, clvel, miss)
 
c  determine IV state at intercept

	    probability_of_kill (i,j) = 100.0*(1.0 - probk)

          endif
  210   continue
  220 continue
   
      do 240 i = 1, num_weapon_sites
	do 230 j = 1, num_threats
	  if (status (i,j) .EQ. 1) then
	    if (probability_of_kill (i,j) .LT. wta_matrix (i,j)) then
	      wta_matrix (i,j) = probability_of_kill (i,j)
	      wta_time (i,j) = threat_time (j)
	    endif
	  endif
  230   continue
  240 continue

      rearth = rearth*1000.0
      do 250 j = 1, num_threats
	threat_state (j,1) = threat_save (j,1)
	threat_state (j,2) = threat_save (j,2)
	threat_state (j,3) = threat_save (j,3)
	threat_state (j,4) = threat_save (j,4)
	threat_state (j,5) = threat_save (j,5)
	threat_state (j,6) = threat_save (j,6)
  250 continue

      return
      end

      subroutine Status_out (status, k)

      include 'shared.h'
      integer status (MAX_NUM_SITES, MAX_NUM_THREATS)

      print *, ' status after ', k
      do 10 j = 1, num_threats
	print *, (status (i,j),i=1,num_weapon_sites)
   10 continue

      print *
      return
      end
