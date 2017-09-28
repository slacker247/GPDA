      subroutine Giroots (range, alt, angle_ret, tcoast_ret, nfound)
c
      include 'shared.h'
c
c------------------------------------------------------------------------------
c
c  Description:
c    This subroutine attempts to find roots for the interceptor position
c    equations given input values of range and altitude. For any input
c    values of range and altitude, there will be either 0 or 2 roots to
c    the equations. The array 'angle' contains the two elevation angles
c    and 'tcoast' contains the two coast time values on output.
c
c--------------------------- Procedures Called --------------------------------
c
c     Name                   Description
c     ----                   -----------
      external giquad        !GBI quadratic equation solver
c
c--------------------------- Calling Arguments --------------------------------
c
c     Name             I/O    Description (range)
c     ----             ---    -------------------
      real range      ! i     range to intercept (km)
      real alt        ! i     altitude above sea level (km)
      real angle (2)  ! o     two burnout elevation angles (rad)
      real tcoast(2)  ! o     two coast time values (sec)
      integer nfound  ! o     number of roots found
c
c------------------------------------------------------------------------------
c  BEGIN EXECUTABLE CODE
 
      real X,Y                  !position in cartesian coord
      real psi                  !'longitude' angle
      real r                    !distance from earth center
      real tx(2),ty(2)          !time values returned from giquad
      real thet,theta,thetb     !angle values
      real th,tha,thb           !angle values
      real tol                  !angle tolerance for endpoint locations
      real astep                !angle step size
      real dthet                !angle step size
      real acc                  !angle tolerance for solution location
      real end(2)               !curve endpoint locations
      real timx(2),timy(2)      !time values
      real txa,tya,txb,tyb      !time values
      real timxa,timya          !time values
      real txend(2),tyend(2)    !time values at first curve endpoint
      real cross                !curve crossing indicator
      real abig                 !temporary angle value
      real tbig                 !temporary time value
 
      integer i,k             !loop indexes
      integer nstep             !number of angle steps
      integer status            !status returned from giquad
c
c------------------------------------------------------------------------------
c  Code body
c------------------------------------------------------------------------------
 
      tol   = 0.02*DEGTORAD            !angle tolerance
      acc   = 0.20*DEGTORAD
      astep = 2.00*DEGTORAD            !angle step size
 
c     --------------------------------------
c     - Initialize returned values to zero -
c     --------------------------------------
      angle (1)  = 0.0
      angle (2)  = 0.0
      tcoast(1)  = 0.0
      tcoast(2)  = 0.0
      nfound     = 0
 
c     ----------------------------
c     - Convert range,alt to X,Y -
c     ----------------------------
      r   = alt + rearth
      psi = range/rearth
      X   = r*SIN (psi)
      Y   = r*COS (psi)
c
 
c     =========================================================================
 
 
c     ---------------------------------------------------------------
c     - Find the lower and upper boundaries of curves.              -
c     - Start at theta = 0 and step up until first boundary         -
c     - is found, then step down from 90.0 to find second endpoint. -
c     - Boundary is located when both the range and altitude curves -
c     - have two solutions retured from giquad.                     -
c     ---------------------------------------------------------------
      nfound = 0
      nstep  = int(0.5*PI/astep) + 1
      do 150 k=1,2
         if(k .eq. 1) then
           theta = 0.0
           dthet = astep
         else
           theta = PI/2.0
           dthet = -astep
         endif
 
c        --------------------------------------------
c        - Step through angles until curves overlap -
c        --------------------------------------------
         do 100 i =1,nstep
            thetb = theta + dthet
            call giquad(thetb,X,Y,tx,ty,status)
            if(status .eq. 1) go to 200
            theta = thetb
  100    continue
         go to 50
 
c        ---------------------------------------------------
c        - Perform bisection to refine the boundary value  -
c        ---------------------------------------------------
  200    thet = (theta + thetb)/2.0
         call giquad(thet,X,Y,tx,ty,status)
         if(status .eq. 1) then
           if(abs(theta - thetb) .le. tol) go to 300
           thetb = thet
         else
           theta = thet
         endif
c **** ADDED
         if (ABS (theta-thetb) .LE. tol) go to 300 
c ****
         go to 200
  300    end(k) = thet
         if(k .eq. 1) then
           txend(1) = tx(1)
           txend(2) = tx(2)
           tyend(1) = ty(1)
           tyend(2) = ty(2)
         endif
  150 continue
      nstep = int((end(2)-end(1))/astep) + 1
 
 
c     ========================================================================= 
 
c     ----------------------------------------------------------
c     - Search within the boundaries determined above for the  -
c     - crossing point of the range and alititude curves.      -
c     ----------------------------------------------------------
      do 700 k=1,2
         theta = end(1)
         timxa = txend(1)
         timya = tyend(k)
 
c        -----------------------------------------
c        - Step through angles between endpoints -
c        - until a curve crossing is found       -
c        -----------------------------------------
         do 400 i=1,nstep
            thetb = theta + astep
            if(thetb .gt. end(2)) thetb = end(2)
            call giquad(thetb,X,Y,timx,timy,status)
            cross = (timx(1) - timy(k))*(timxa - timya)
               
c           ----------------------------------
c           - Perform bisection to find root -
c           ----------------------------------
            if(cross .lt. 0.0) then
              tha = theta
              thb = thetb
              txa = timxa
              tya = timya
              txb = timx(1)
              tyb = timy(k)
  500         if(abs(tha - thb) .le. acc) then
                nfound = nfound + 1
c               --------------------------------------------------------------
c               - Use linear interpolation to refine angle and time solution -
c               - Make sure time solution is still positive                  -
c               --------------------------------------------------------------
                angle(nfound) = tha + (tya - txa)*(thb - tha)/
     &                                (txb - txa - tyb + tya)
                tcoast(nfound) = tya + (tyb - tya)/(thb - tha)*
     &                                 (angle(nfound) - tha)
                if(tcoast(nfound) .lt. 0.0) nfound = nfound - 1
                if(nfound .eq. 2) go to 50
                go to 600
              endif
              th = (tha + thb)/2.0
              call giquad(th,X,Y,tx,ty,status)
              cross = (tx(1) - ty(k))*(txa - tya)
              if(cross .lt. 0.0) then
                thb = th
                txb = tx(1)
                tyb = ty(k)
              else
                tha = th
                txa = tx(1)
                tya = ty(k)
              endif
              go to 500
            endif
  600       theta = thetb
  400    continue
  700 continue
c
 
c     =========================================================================
 
 
  50  continue 
 
c     --------------------------------------------------
c     - Make sure that the smaller coast time and its  -
c     - associated angle is first solution in array    -
c     --------------------------------------------------
      if (nfound .ne. 2) goto 51
      if(tcoast(1) .gt. tcoast(2)) then
        abig      = angle(1)
        tbig      = tcoast(1)
        angle(1)  = angle(2)
        tcoast(1) = tcoast(2)
        angle(2)  = abig
        tcoast(2) = tbig
      endif
c
 51   continue

      tcoast_ret = tcoast (1)
      angle_ret  = angle (1)
 
      return
      end
