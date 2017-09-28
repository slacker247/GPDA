      subroutine Giprobk (j,iv_state,targ_state, pk,
     &                    closing_ang, closing_vel, miss_dis )

      include 'shared.h'
c                            
c
c------------------------------------------------------------------------------
c
c  Description:
c     This routine computes the nominal probability of kill. Given the 
c     interceptor and target states at a particular time, the routine 
c     computes closing angle and closing velocity, and then uses these
c     to interpolate a value of Pk from the user-specified table. If the
c     computed closing angle or velocity is outside the range of the      
c     table, then the Pk will be set to zero. 
c     The integer index 'table' refers to the Pk table which will be used:
c
c        1 - exo mode,   RV
c        4 - exo mode,   Penaid
c
c
c--------------------------- Calling Arguments --------------------------------
c
c     Name                   I/O  Type  Description (range)
c     ----                   ---  ----  -------------------
      real iv_state    (6)  ! i   real interceptor state vec (ECI pos and vel)
      real pk               ! o   real probability of kill
      real closing_ang      ! o   real closing angle
      real closing_vel      ! o   real closing velocity
      real miss_dis         ! o   real miss distance (interceptor-target sep)
c
c------------------------------------------------------------------------------
c  BEGIN EXECUTABLE CODE

c     -----------------------------------
c     - Framework and IFG include files -
c     -----------------------------------
      integer gisrchar       !GBI interpolation index 
      integer ivel           !velocity index
      integer iang           !angle index
c
      real giinterp          !interpolated value  
c
      real targ_state(6)     !target state vector
      real ivmag             !magnitude of IV velocity
      real targmag           !magnitude of target velocity
      real pk1,pk2           !intermediate pk values 
c
c------------------------------------------------------------------------------
c  Code body
c------------------------------------------------------------------------------

c     --------------------------------
c     - Print up the targ_state array -
c     --------------------------------

c     ------------------------------------------
c     - Compute the distance from IV to target -
c     ------------------------------------------
      miss_dis = SQRT( (iv_state(1) - targ_state(1))**2+
     &                 (iv_state(2) - targ_state(2))**2+
     &                 (iv_state(3) - targ_state(3))**2 )

c     --------------------------------
c     - Compute the closing velocity -
c     --------------------------------
      closing_vel = SQRT( (iv_state(4) - targ_state(4))**2+
     &                    (iv_state(5) - targ_state(5))**2+
     &                    (iv_state(6) - targ_state(6))**2 )

c     -----------------------------
c     - Compute the closing angle Jim: the value of PI and DEGTORAD are in fwconst.com -
c     -----------------------------
      ivmag = SQRT(   iv_state(4)**2+  iv_state(5)**2+  iv_state(6)**2 )
      targmag=SQRT( targ_state(4)**2+targ_state(5)**2+targ_state(6)**2 )
      sdot = iv_state (4)*targ_state (4)+iv_state (5)*targ_state (5)+
     *       iv_state (6)*targ_state (6)
      closing_ang = (PI - ACOS (sdot/(ivmag*targmag)))/DEGTORAD

c     --------------------------------------------------------
c     - If closing angle or closing velocity are outside the -
c     - ranges in the table, then the Pk is zero.            -
c     --------------------------------------------------------
      if((closing_ang .LT. pkang(1)) .OR.
     &   (closing_ang .GT. pkang(num_pkang)) .OR.
     &   (closing_vel .LT. pkvel(1)) .OR.
     &   (closing_vel .GT. pkvel(num_pkvel))) then
        pk = 0.0
        return
      endif

c     -------------------------------------------------------------------
c     - Perform double linear interpolation to get the nominal Pk value -
c     -------------------------------------------------------------------
      iang = gisrchar(num_pkang,pkang,closing_ang)
      ivel = gisrchar(num_pkvel,pkvel,closing_vel)
c
      pk1 = Giinterp (pkang(iang),pkang(iang+1),
     &               pkill(iang,ivel),
     &               pkill(iang+1,ivel),closing_ang)
c
      pk2 = Giinterp (pkang(iang),pkang(iang+1),
     &               pkill(iang,ivel+1),
     &               pkill(iang+1,ivel+1),closing_ang)
c
      pk = Giinterp (pkvel(ivel),pkvel(ivel+1),pk1,pk2,closing_vel)
c
      if(pk .LT. 0.0) pk = 0.0
      if(pk .GT. 1.0) pk = 1.0
c
      return
      end
