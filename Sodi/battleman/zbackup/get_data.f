      subroutine Get_data

      include 'shared.h'

c------------------------------------------------------------------------------
c
c  Description:
c    This subroutine initializes the interceptor flyout data for later use
c    by the feasibility routine. The following operations are performed:
c      -Determines the min and max intercept altitude.
c      -Determines the maximum TOF for each.
c      -Generates a curve of maximum range versus coast time.
c
c    Note: In this subroutine, all "time" variables refer to the interceptor
c          coast time in seconds, unless specified otherwise.            
c
c------------------------------------------------------------------------------
c
c     Name                   Description
c     ----                   -----------
      external gipos         !GBW interceptor position routine
c
      real a0,a              !altitude values 
      real t0,t              !time values
      real theta             !angle value
      real r0,r1,r           !range values
      real tc                !coast time
      real dt                !time increment for determination of amax and rmax
      real max_coast         !maximum overall coast time
      real max_tof
      integer i,j            !i=angle index, j=time index
      integer mx_num_tc      !The maximum num tc
c
c------------------------------------------------------------------------------
c  Code body
c------------------------------------------------------------------------------
      EARTHATM = 1.014352515
      num_angles = 45
      mx_num_tc = 0               !debug loop counter
      dt = 2.0                    !time increment
      theta     = 0.5*PI       !Use theta = 90.0 deg to get max altitude
      dtheta = theta/num_angles  !angle increment
c-----------------------------------------------------------------------
c- Get minimum altitudes for interceptors
c-----------------------------------------------------------------------
      min_alt = rearth*(EARTHATM - 1.0)/1000.0    !top of atmosphere in km
c------------------------------------------------------------------------------
c                                                                             -
c       DETERMINE:                                                                      -
c       max_alt = maximum altitude for each type,needed by gifeasbl        -
c       max_tof = maximum TOF for each type,needed by EP                   -
c       max_coast  = maximum overall coast time,needed to compute num_tc      -
c                                                                             -
c------------------------------------------------------------------------------

      print *, ' TPT option on '
c        ------------------------
c        - Initialize variables -
c        ------------------------
         max_coast = 0.0
         max_alt   = 0.0
         t         = 0.0
         t0        = 0.0
         a0        = 0.0

  200    t = t + dt
         call Gipos (theta,t,r,a)
         if (a .GT. max_alt) max_alt = a
         if ((a .LT. min_alt) .AND. (a .LT. a0)) then
            max_tof = t0 + dt/(a - a0)*(min_alt - a0)
            if (max_tof .GT. max_coast) max_coast = max_tof 
            max_tof = max_tof + burn_time
	    go to 250
         endif
         t0 = t
         a0 = a
         go to 200

  250  continue

       num_tc = INT (max_coast/dtc)

       mx_num_tc = num_tc

       if(max_coast .EQ. 0.0) then
          num_tc = 1 
          max_tof = 0.0
       endif
c
      do 300 j=1,num_tc
         tc        = dtc*j
         a0        = 0.0
         r0        = 0.0
         rmax (j) = 0.0
         do 400 i=num_angles,1,-1
            theta = i*dtheta
            call Gipos (theta,tc,r,a)
            if(a .LT. min_alt) then
              if(i .EQ. num_angles) then
                rmax (j) = 0.0
                go to 300
              else 
                r1 = r0 + (r-r0)/(a-a0)*(min_alt - a0)
                if(r1 .GT. rmax (j)) rmax (j) = r1
                go to 300
              endif
            endif
            if(r .GT. rmax (j)) rmax (j) = r
            r0 = r
            a0 = a
  400    continue 
  300 continue
      rmax (1) = 0.0
 
c        ------------------------------
c        -  Determine maximum range   -
c        ------------------------------
         max_range = 0.0
         do 600 j=1,num_tc
            if(rmax(j) .GT. max_range) max_range = rmax(j)
  600    continue

c  read in tables

      in = 1
      open (in, file = 'tpt.dat', form = 'formatted', status = 'old')

      read (in,*) num_pkang
      read (in,*) num_pkvel
      read (in,*) (pkang (j),j=1,num_pkang)
      read (in,*) (pkvel (j),j=1,num_pkvel)

      do 650 k=1,num_pkang
        read (in,*) (pkill (k,j),j=1,num_pkvel)
  650 continue

      close (in)

      return
      end

      subroutine Gipos (theta,t,r,a)
c
      include 'shared.h'
c
c------------------------------------------------------------------------------
c
c  Description:
c    This subroutine computes the range and altitude from the launch point 
c    to the interceptor, given the mode, burnout elevation angle, and coast
c    time. 
c
c--------------------------- Calling Arguments --------------------------------
c
c     Name           I/O  Type  Description (range)
c     ----           ---  ----  -------------------
      real theta    ! i   real  burnout elevation angle in X,Y system (radians)
      real t        ! i   real  coast time (seconds)
      real r        ! o   real  range (km)
      real a        ! o   real  altitude above sea level (km)
c
c------------------------------------------------------------------------------
c  BEGIN EXECUTABLE CODE
c------------------------------------------------------------------------------

      real X,  Y               !interceptor position (km)
      real X0, Y0              !interceptor burnout position (km)
      real V0x,V0y             !interceptor burnout velocity (km/sec)
      real ax, bx              !local intermediate variables
      real ay, by              !local intermediate variables

c
c------------------------------------------------------------------------------
c  Code body
c------------------------------------------------------------------------------
 
      X0  = c_x*COS(theta) + k_x 

      V0x = c_vx (0)                          +
     &      c_vx (1)*theta                    + 
     &      c_vx (2)*COS (theta)               +
     &	    c_vx (3)*COS (theta)*SIN (theta)    +
     &	    c_vx (4)*COS (theta)*SIN (theta)**2 +
     &	    c_vx (5)*COS (theta)*SIN (theta)**3

      ax  = kx_t (4)          +
     &      kx_t (5)*theta    +
     &      kx_t (6)*theta**2 +
     &      kx_t (7)*theta**3 

      bx  = V0x               + 
     &      kx_t (0)          +
     &      kx_t (1)*theta    +
     &      kx_t (2)*theta**2 +
     &      kx_t (3)*theta**3 
 
      X   = (X0 + bx*t + ax*t**2)/1000.

c     ----------------------------------------- 

      Y0  = c_y*SIN(theta) + k_y

      V0y = c_vy (0)*SIN(theta)      +
     &	    c_vy (1)*SIN(theta)**2   +
     &	    c_vy (2)*SIN(theta)**3   +
     &	    c_vy (3)*SIN(theta)**4

      by  = V0y               +  
     &      ky_t (0)          +    
     &      ky_t (1)*theta    +
     &      ky_t (2)*theta**2 +
     &      ky_t (3)*theta**3 

      ay =  ky_t (4)          +
     &      ky_t (5)*theta    +
     &      ky_t (6)*theta**2 +
     &      ky_t (7)*theta**3 
 
      Y = (Y0 + by*t + ay*t**2)/1000.
 
c     ------------------------------------------

      r = ATAN (X/Y)*rearth/1000.0
      a = SQRT (X**2 + Y**2)-rearth/1000.0
 
      return
      end
