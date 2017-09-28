      subroutine Fly_threat
      include 'shared.h'

      real rm0   (MAX_NUM_THREATS), sig0  (MAX_NUM_THREATS), 
     *     alfa  (MAX_NUM_THREATS), alpsq (MAX_NUM_THREATS), 
     *     ft    (MAX_NUM_THREATS), 
     *     alpsy (MAX_NUM_THREATS), psy   (MAX_NUM_THREATS), 
     *     s0    (MAX_NUM_THREATS), s1    (MAX_NUM_THREATS), 
     *     s2    (MAX_NUM_THREATS), s3    (MAX_NUM_THREATS), 
     *     rm    (MAX_NUM_THREATS), 
     *     ro_1  (MAX_NUM_THREATS), ro_2  (MAX_NUM_THREATS), 
     *     ro_3  (MAX_NUM_THREATS), vo_1  (MAX_NUM_THREATS), 
     *     vo_2  (MAX_NUM_THREATS), vo_3  (MAX_NUM_THREATS), 
     *     time_dif (MAX_NUM_THREATS)
      logical threat_off (MAX_NUM_THREATS)

      do 100 i = 1, num_threats
         if (threat_play (i)) then
            ro_1 (i) = threat_state (i,1)
            ro_2 (i) = threat_state (i,2)
            ro_3 (i) = threat_state (i,3)
            vo_1 (i) = threat_state (i,4)
            vo_2 (i) = threat_state (i,5)
            vo_3 (i) = threat_state (i,6)
c                                                                       
            time_dif (i) = war_time (i) - threat_time (i)
      
            rm0 (i) = SQRT (ro_1 (i)**2 + ro_2 (i)**2 + ro_3 (i)**2)
   
            sig0 (i) = ro_1 (i)*vo_1 (i) + ro_2 (i)*vo_2 (i) + 
     *                 ro_3 (i)*vo_3 (i)

            alfa (i) = vo_1 (i)**2 + vo_2 (i)**2 + vo_3 (i)**2 - 
     *                 2.0*gmu/rm0 (i)
            psy (i) = time_dif (i)/rm0 (i) - sig0 (i)*time_dif (i)**2/
     *                (2.0*rm0 (i)**3)

            alpsq (i) = SQRT (-alfa (i))                                               
	 endif
  100 continue

      do 200 i = 1, num_threats

         if (threat_play (i)) then
            tol = 2.0 * ppgtol

  105       continue
	    if (tol .GT. ppgtol) then

               alpsy (i) = psy (i) * alpsq (i)

               s0 (i)= cos (alpsy (i))        
               s1 (i) = sin (alpsy (i))/alpsq (i)
               s2 (i) = (s0 (i) - 1.0)/alfa (i) 
               s3 (i) = (s1 (i) - psy (i))/alfa (i)      

               ft (i) = rm0 (i)*s1 (i) + sig0 (i)*s2 (i) + gmu*s3 (i)
               rm (i) = rm0 (i)*s0 (i) + sig0 (i)*s1 (i) + gmu*s2 (i)

               dif     = time_dif (i) - ft (i)
               psy (i) = psy (i) + dif/rm (i)
   
               tol = ABS (dif)
	       go to 105
	    endif

	 endif

  200 continue

      do 300 i = 1, num_threats
         if (threat_play (i)) then
            threat_off (i) = .FALSE.
            f  = 1.0 - gmu*s2 (i)/rm0 (i)
            fd = -gmu*s1 (i)/(rm (i)*rm0 (i))
            g  = ft (i) - gmu*s3 (i) 
            gd = 1.0 - gmu*s2 (i)/rm (i)
 
            x = ro_1 (i)*f + vo_1 (i)*g
            y = ro_2 (i)*f + vo_2 (i)*g
            z = ro_3 (i)*f + vo_3 (i)*g

            alt = SQRT (x**2 + y**2 + z**2)

            if (alt .GT. rearth) then
 
                threat_state (i,1) = x
                threat_state (i,2) = y
                threat_state (i,3) = z

                threat_state (i,4) = ro_1 (i)*fd + vo_1 (i)*gd
                threat_state (i,5) = ro_2 (i)*fd + vo_2 (i)*gd
                threat_state (i,6) = ro_3 (i)*fd + vo_3 (i)*gd

                threat_time (i) = war_time (i)
              else
                threat_off (i) = .TRUE.
            endif
	 endif
  300 continue
 
      done = .TRUE.
      do 400 i = 1, num_threats
	 if (threat_play (i) .AND. threat_off (i)) then
            alt = SQRT (threat_state (i,1)**2 + threat_state (i,2)**2 +
     *                  threat_state (i,3)**2)
            threat_play (i) = .FALSE.
c           print 99, i, threat_time (i), alt
         endif
	 if (threat_play (i)) done = .FALSE.
  400 continue
   99 format (' i = ', i3, ' terminated at ', f9.2,' at altitude of ',
     *           f10.2)

      return 
      end 
