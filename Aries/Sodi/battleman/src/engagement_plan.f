      subroutine Engagement_plan
c
c  engagement plan
c
      include 'shared.h'

      integer   minute, second
      logical   killed(MAX_NUM_THREATS)
      character chengage*8, chtrack*8
      character chpart1*64, chpart2*52
c
      integer   plan_threats
      character*120  plan_lines(MAX_NUM_THREATS)
      common/planout/ plan_threats, plan_lines
C
      data chtrack, chengage  / 'In_Trk', 'Engaged' /
      data killed             / MAX_NUM_THREATS*.FALSE. /
C
c      print *, ' '
c      print *, ' ***** engagement plan as follows:'
c      print *, '    weapon id   threat id    time'

      Tcurrent = current_time
      plan_threats = num_threats
      write(*,79) num_threats, Tcurrent
      write(*,84)
      write(*,80)
      write(*,81)
      write(*,89)

      do 10 i = 1, num_threats
	 id = threat_id(bid (i))
         igbi = map(i)
         if (map(i) .GT. 0) then
	   if (map(i) .LE. num_pebbles) then
	         print 99, map (i), id, wta_time (map (i), bid (i))
	      else
c	         print 98, map(i), id, wta_time(map (i), bid (i))
                 Tintercept = wta_time(map(i), bid(i))
                 Timpact = threat_time(bid(i))
C
c                write(*,*) Tcurrent, Tintercept, Timpact
                 chtrack = 'In_Trk'
                 chengage = 'Engaged'
                 if (Tcurrent .LT. Timpact) then
                    TTI = Timpact-Tcurrent
                    minute = IFIX(ABS(TTI/60.0))
                    second = IFIX(ABS(TTI - minute*60.0))
                 else
                    if (.NOT. killed(bid(i))) chtrack = 'Impact'
                    minute = 0
                    second = 0
                 endif
                 write(chpart1,91) id,
     +                             'Obj',
     +                             threat_type(bid(i)),
     +                             1,
     +                             0.67,
     +                             'Tgt',
     +                             threat_impact(bid(i)),
     +                             minute,second
C 
                 if (Tcurrent .LT. Tintercept) then
                    TTI = Tintercept-Tcurrent
                    minute = IFIX(ABS(TTI/60.0))
                    second = IFIX(ABS(TTI - minute*60.0))
                 else
                    killed(bid(i)) = .TRUE.
                    chengage = 'Killed'
                    chtrack  = 'Killed'
                    minute = 0
                    second = 0
                 endif
                 write(chpart2,92) chtrack,
     +                             gbi_name(igbi),
     +                             chengage,
     +                             gbi_gbis(igbi),
     +                             minute,second,
     +                             gbi_pk(igbi),
     +                  IFIX(ABS((Timpact-200.0-Tcurrent)/400.0))
C
                 write(*,97) chpart1, chpart2
                 plan_lines(i) = chpart1 // chpart2 // char(0)
	   endif
         endif
   10 continue

      return

 79   format ('Number of threats processed is ', i4, 
     +        ' at time ', F10.2, /)
 80   format (' Trk     Obj    Msl    Exp   Leth     Tgt ',
     +        '    Pre Imp   T to')
 81   format (' ID     Type    Type   Tgts  Value    Type',
     +        '    Loc.     Impact')
 84   format (60x, 15x, 'Track Engagement View')
 89   format ('----  -------- ------ ----  ------  -------- ',
     +        '--------  ------',
     +        ' -------- -------- -------- ---  -----',
     +        ' -----   ----')
c64 + next size MUST <= 119
 91   format (I4,2x, A8,2x, A6,1x, I4,2x, F6.2,4x, A8,1x,
     +        A8,2x, I2,':',I2.2, 1x)
c52
 92   format ('|', 1x, A8,1x, A8,1x, A8,1x, I3,2x, I2,':',I2.2, 
     +        F6.3,3x, 1x,I2,1x)
 97   format (A64, A52)
   99 format (4x,i3, ' (pebble)',1x,i4,5x,f10.2)
   98 format (4x,i3, ' (gbi)   ',1x,i4,5x,f10.2)
      end

      integer function getnthreats()
      include 'shared.h'
      integer   plan_threats
      character*120  plan_lines(MAX_NUM_THREATS)
      common/planout/ plan_threats, plan_lines
c
      get_n_threats = plan_threats
      return
      end

      subroutine getplanline(index, chline)
      character*120 chline
      include 'shared.h'
      integer   plan_threats
      character*120  plan_lines(MAX_NUM_THREATS)
      common/planout/ plan_threats, plan_lines
c
      chline = plan_lines(index)
      return
      end
