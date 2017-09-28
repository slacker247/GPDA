      subroutine Matrix 
 
      include 'shared.h'
 
      real ddist (MAX_NUM_SITES, MAX_NUM_THREATS)

      call Distance (ddist, MAX_NUM_SITES, site_xyz, MAX_NUM_SITES, 
     *               num_weapon_sites)

      do 40 j = 1, num_threats
        if (threat_play (j)) then
	  do 30 i = 1, num_weapon_sites
             if (ddist (i,j) .LT. wta_matrix (i,j)) then
                wta_matrix (i,j) = ddist (i,j)
                wta_time (i,j)   = threat_time (j)
             endif
   30     continue
	endif
   40 continue

      return
      end
