      subroutine Pre_process
 
      include 'shared.h'
 
      integer max_value, min_dist
      integer site (MAX_NUM_SITES)

      do 5 i = 1, MAX_NUM_SITES
	site (i) = 0
   5  continue

      write(*,*) 'No. of weapon sites is ', num_weapon_sites
      do 6 i = 1, num_weapon_sites
    6 print 13, i, (wta_matrix (i,j),j=1, num_threats)
   13 format (i2, 9(1x,i7))

      k = 1
      do 40 j = 1, num_threats

c  find which site has the best shot at threat j, save in cost and map

	 min_dist = 9000000
	 do 10 i = 1, num_weapon_sites
	    if (wta_matrix (i,j) .LT. min_dist) then
	       min_dist = wta_matrix (i,j)
	       k = i
	    endif
   10    continue
 
c  weapon site k has best shot at threat j
 
	 map (j) = k
	 site (k) = site (k) + 1

c  save wta_matrix row in cost and if weapon site k is a pebble zero it out
c  if weapon site k has more than MAX_NUM_SHOTS zero it out

	 do 20 i = 1, num_threats
	    cost (j,i) = wta_matrix (k,i)
   20    continue
	 if (pebble (k) .OR. site (k) .EQ. MAX_NUM_SHOTS) then
	    do 30 i = 1, num_threats
	       wta_matrix (k,i) = 9000000
   30       continue
	 endif
   40 continue

c set up COST matrix for auction

      max_value = 0

      do 60 i = 1, num_threats
	 do 50 j = 1, num_threats
	    if (cost (i,j) .GT. max_value) max_value = cost (i,j)
   50   continue
   60 continue

      idiv = MAX0 (IFIX(max_value*0.1), 1)

      do 80 i = 1, num_threats
	 do 70 j = 1, num_threats
	    cost (i,j) = (max_value - cost (i,j))/idiv + 5
   70   continue

   80 continue

      return
      end
