      subroutine gpals_sim
 
c GPALS simulation
 
      include 'shared.h'
c      real tmp (2)

c  set up i/o file numbers and constants, then read in data and process
 
c      call Initial
c      call Read_data
c      if (tpt) call Get_data

c      do 30 k = 1, 1

        call Read_threats

c       tbegin = etime (tmp)

        if (pebbles_on) call Conin1

        do 20 itime = 100, 2000, war_time_increment

           do 10 i = 1, MAX_NUM_THREATS
	      if (threat_play (i)) then
	         war_time (i) = war_time (i) + war_time_increment
	      endif
   10      continue

c          if (pebbles_on) call Orbit
           call Fly_threat

c set up feasibility matrix
 
	   if (tpt) then
	       call Feas
	     else
               call Matrix
	   endif

	   if (done) go to 25

   20   continue
   25   continue

c  compute expected damage from impact points

        call Expected_damage

c  weapon-target allocation

        call Pre_process
        call Wta
        call Engagement_plan
  
c        tend = etime (tmp)
c        print *, ' elapsed time = ', tend - tbegin
c	 print *, ' '
   30 continue

      return
      end                                                               
