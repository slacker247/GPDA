      subroutine Expected_damage 
      include 'shared.h'

c  compute expected damage

      real ddist (MAX_NUM_ASSETS, MAX_NUM_THREATS)

      print *, ' '
      print *, ' ***** expected damage'

c  compute alt, lat, long for threats

      print *, ' expected impact lat      lon      time'

c  convert x,y,z of threat to lat lon

      do 10 j = 1, num_threats
	 threat_play (j) = .TRUE.
	 alt = SQRT (threat_state (j,1)**2 + threat_state (j,2)**2 + 
     *               threat_state (j,3)**2)
	 threat_lat = ASIN (threat_state (j,3)/alt) * radtodeg
	 threat_lon = ATAN2 (threat_state (j,2), 
     *                           threat_state (j,1))*radtodeg
     *                         - threat_time (j)*we*radtodeg

	if (threat_lon .LT. -180.0) threat_lon = threat_lon + 360.0
	if (threat_lon .GT. 180.0) threat_lon = threat_lon - 360.0

         print 99,threat_id (j), threat_lat, threat_lon, threat_time (j)
   10 continue
   99 format (7x, i4, 4x, f7.2, 2x, f7.2, 2x, f9.2)

      call Distance (ddist, MAX_NUM_ASSETS, asset_xyz, MAX_NUM_ASSETS,
     *               num_assets)

c  asset is threatened if within radius of expected impact
	    
      print *, '  threatened sites '
      do 30 i = 1, num_assets
         do 20 j = 1, num_threats
	    if (ddist (i,j) .LT. radius) then
               print 98,  i, threat_id (j)
	    endif
   20   continue
   30 continue

   98 format (' asset ', i4, ' targeted by threat ', i4)

      return
      end
