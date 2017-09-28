      subroutine Giquad (theta, X, Y, tx, ty, real_sol)

      include 'shared.h'

c
c------------------------------------------------------------------------------
c
c  Description:
c    This subroutine solves the two quadratic equations fOR tx and ty,
c    given a point (X,Y) in the cartesian launch point coORdinate system,
c    a value fOR burnout elevation angle, and the mode.
c
c--------------------------- Calling Arguments --------------------------------
c
c     Name             I/O  Type  Description (range)
c     ----             ---  ----  -------------------
      real theta      ! i   real  burnout elevation angle (rad)
      real X          ! i   real  X cartesian coORd (km)
      real Y          ! i   real  Y cartesian coORd (km)
      real tx(2)      ! o   real  early tx value (late tx not valid)
      real ty(2)      ! o   real  early and late ty values
      integer real_sol  ! o   int   real_sol = 1 indicates real solutions
      real ax,bx      ! local intermediate variables
      real ay,by      ! local intermediate variables
c
c------------------------------------------------------------------------------
c  BEGIN EXECUTABLE CODE
c
      real quanx,quany        !x and y discriminant
      real X0,V0x
      real Y0,V0y
      real temp
 
c------------------------------------------------------------------------------
c  Code body
c------------------------------------------------------------------------------
 
c    -----------------------------------------
c    - Initialize returned variables to zero -
c    -----------------------------------------
      do 100 i = 1,2
         tx(i) = 0.0
         ty(i) = 0.0
 100  continue
 
c   Make sure earth radius offset is accounted fOR
 
      X0  = c_x*COS (theta) + k_x
 
      V0x = c_vx (0)                          +
     &      c_vx (1)*theta                    +
     &      c_vx (2)*COS (theta)               +
     &      c_vx (3)*COS (theta)*SIN (theta)    +
     &      c_vx (4)*COS (theta)*SIN (theta)**2 +
     &      c_vx (5)*COS (theta)*SIN (theta)**3
 
      bx  = V0x               +
     &      kx_t (0)          +
     &      kx_t (1)*theta    +
     &      kx_t (2)*theta**2 +
     &      kx_t (3)*theta**3
 
      ax  = kx_t (4)          +
     &      kx_t (5)*theta    +
     &      kx_t (6)*theta**2 +
     &      kx_t (7)*theta**3
 
 
      Y0  = c_y*SIN (theta) + k_y
 
      V0y = c_vy (0)*SIN (theta)      +
     &      c_vy (1)*SIN (theta)**2   +
     &      c_vy (2)*SIN (theta)**3   +
     &      c_vy (3)*SIN (theta)**4
 
      by  = V0y               +
     &      ky_t (0)          +
     &      ky_t (1)*theta    +
     &      ky_t (2)*theta**2 +
     &      ky_t (3)*theta**3
 
      ay  = ky_t (4)          +
     &      ky_t (5)*theta    +
     &      ky_t (6)*theta**2 +
     &      ky_t (7)*theta**3
 
c     -------------------------------------------
c     - Calculate the discriminant fOR both the -
c     - range and time curves                   -
c     -------------------------------------------
      quanx = bx**2 - 4.0*ax*(X0 - X*1000.)
      quany = by**2 - 4.0*ay*(Y0 - Y*1000.)
 
c     ---------------------------------------------
c     - If either discriminant is less than zero, -
c     - the curves  don't intersect and hence no  -
c     - feasible solution at this range           -
c     ---------------------------------------------
      if((quanx .LT. 0.0) .OR. (quany .LT. 0.0)) then
        real_sol = 0
        return
      endif
      real_sol = 1
 
c     -------------------------------------------
c     - Calculate the two range time solutions  -
c     - and put the smallest value in the first -
c     - position.                               -
c     -------------------------------------------
      tx(1)   = 0.5*(-bx - SQRT (quanx))/ax
      tx(2)   = 0.5*(-bx + SQRT (quanx))/ax
      if (tx(1) .GT. tx(2)) then
         temp = tx(2)
         tx(2) = tx(1)
         tx(1) = temp
      endif
 
c     ----------------------------------------------
c     - Calculate the two aLTitude time solutions  -
c     - and put the smallest value in the first    -
c     - position.                                  -
c     ----------------------------------------------
      ty(1) = 0.5*(-by - SQRT (quany))/ay
      ty(2) = 0.5*(-by + SQRT (quany))/ay
      if (ty(1) .GT. ty(2)) then
         temp = ty(2)
         ty(2) = ty(1)
         ty(1) = temp
      endif
 
c     ----------------------------------------------------
c     - If any time solution is negative, assume no good -
c     - solution exists                                  -
c     ----------------------------------------------------
      if(  tx(1) .LT. 0.0 .OR. tx(2) .LT. 0.0  .OR.
     &     ty(1) .LT. 0.0 .OR. ty(2) .LT. 0.0  ) then
        real_sol = 0
      endif
      return
      end
