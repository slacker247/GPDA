      subroutine threat_init(index, misstype, Plaunch, Pimpact,
     +                       Tlaunch, Limpact)
      integer     index
      real*8      misstype, Plaunch, Pimpact
      real*4      Tlaunch, Limpact
C
      include 'shared.h'
c
      character*8 chtype
      character*8 chtemp
      real*8      itemp
      equivalence (itemp, chtemp)
C
      ind = index+1
      if (ind .GT. MAX_NUM_THREATS) return
c
      itemp  = misstype
      chtype = '        '
      do i=1,8
        if (chtemp(i:i) .EQ. CHAR(0)) goto 110
        chtype(i:i) = chtemp(i:i)
      enddo
 110  threat_type(ind) = chtype
c
      itemp = Pimpact
      chtype = '        '
      do i=1,8
        if (chtemp(i:i) .EQ. CHAR(0)) goto 120
        chtype(i:i) = chtemp(i:i)
      enddo
 120  threat_impact(ind) = chtype
c
c     write(*,*) threat_type(ind), '   ', threat_impact(ind)
      return
      end

      subroutine threat_setup(x1, x2, x3, v1, v2, v3, 
     *      thetime, start_time, ntracks, id, ithrt)

      include 'shared.h'
      real*8 x1, x2, x3, v1, v2, v3, thetime, start_time
      integer ithrt, ntracks, id
      
      war_time (ithrt) = thetime
      num_threats = ntracks
      threat_state (ithrt, 1) = x1*1000.0
      threat_state (ithrt, 2) = x2*1000.0
      threat_state (ithrt, 3) = x3*1000.0
      threat_state (ithrt, 4) = v1*1000.0
      threat_state (ithrt, 5) = v2*1000.0
      threat_state (ithrt, 6) = v3*1000.0
c     print *, ' in threat_setup ',x1, x2, x3, v1, v2, v3, 
c    *                               start_time, thetime
      current_time = start_time
      threat_time (ithrt) = thetime         
      threat_id (ithrt) = id         
      threat_play (ithrt) = .TRUE.        
      return
      end

      subroutine set_tpt ()
      include 'shared.h'
      tpt = .TRUE.
      return
      end

      subroutine reset_tpt ()
      include 'shared.h'
      tpt = .FALSE.
      return
      end


      subroutine init_threat_play ()
      integer n
      include 'shared.h'

      do 10 n=1,MAX_NUM_THREATS
      	threat_play(n) = .FALSE.
10      continue

            switch = .TRUE.

      do 40 i = 1, MAX_NUM_SITES
         do 30 j = 1, MAX_NUM_THREATS
            wta_matrix (i,j) = 9000000
            wta_time   (i,j) = 0.0
   30    continue
   40 continue

      return
      end


      subroutine get_n_assign (n)

      integer n
      include 'shared.h'

      n = num_threats
      return
      end


      subroutine get_assign (i, thrt, weap, etime)

      integer i, thrt, weap
      REAL*8 etime
      include 'shared.h'

      thrt = threat_id (bid(i))
      weap = map (i)
      etime = wta_time (map (i), bid (i))
      return
      end
