      subroutine Gistate (i, tlnch, az, theta, time, state)

      include 'shared.h'
c
c------------------------------------------------------------------------------
c
c  Description:
c    This subroutine computes the GBI interceptor position and velocity in 
c    ECI coordinates.
c
c
c--------------------------- Procedures Called --------------------------------
c
c     Name                   Description
c     ----                   -----------
      external mxv           !Cray intrinsic matrix multiplication routine
c
c--------------------------- Calling Arguments --------------------------------
c
c     Name              I/O    Description (range)
c     ----              ---    -------------------
      real tlnch       ! i     simtime at which launch occurs
      real az          ! i     launch azimuth (rad, PI/2 = East)
      real theta       ! i     burnout elevation angle in LPS system (rad)
      real time        ! i     simtime at which IV position is desired
      real state(6)    ! o     IV ECI 6-state vector    

c
c------------------------------------------------------------------------------
c  BEGIN EXECUTABLE CODE


c     ---------------------------
c     - GBI model include files -
c     ---------------------------
      real lat               !launch site latitude (rad)
      real lon               !launch site longitude at time (rad, 0 = ECI-x)
      real rotmat(3,3)       !rotation matrix from LPS to ECI coordinates
      real pos(3)            !IV position in LPS coord
      real vel(3)            !IV velocity in LPS coord
      real t                 !coast time (sec)
c
      real X0,Y0             !interceptor burnout position (km)
      real V0x,V0y           !interceptor burnout velocity (km/sec)
c
c------------------------------------------------------------------------------
c  Code body
c------------------------------------------------------------------------------
c
      MUSTEPAN = 0.0

      lat = gbi_lat (i)
      lon = -(gbi_lon (i) + we*tlnch + MUSTEPAN)

c     ----------------------------------------------------------------------
c     - Set up the rotation matrix from LPS to ECI                         -
c     - Can zero out second column because the y component is always zero. -
c     ----------------------------------------------------------------------
      rotmat(1,1) = COS (lon)*COS (lat)
      rotmat(1,2) = 0.0                                            
      rotmat(1,3) = SIN (lon)*SIN (az) - COS (lon)*SIN (lat)*COS (az)
      rotmat(2,1) =-SIN (lon)*COS (lat)
      rotmat(2,2) = 0.0                                            
      rotmat(2,3) = COS (lon)*SIN (az) + SIN (lon)*SIN (lat)*COS (az)
      rotmat(3,1) = SIN (lat)
      rotmat(3,2) = 0.0                  
      rotmat(3,3) = COS (lat)*COS (az)

c     ----------------------------------------
c     - Compute IV LPS position and velocity -
c     ----------------------------------------
      t = time - tlnch - burn_time   !coast time 
c
      X0 = c_x*COS (theta) + k_x
c    -------------------------------------------------------------------------------
c    - Adjust coefficients to convert to earth surface coordinate system(earth_rad).     
c    -------------------------------------------------------------------------------
      V0x = c_vx (0) + c_vx (1)*theta + c_vx (2)*COS (theta)
     1	+ c_vx (3)*COS (theta)*SIN (theta)
     2	+ c_vx (4)*COS (theta)*SIN (theta)**2
     3	+ c_vx (5)*COS (theta)*SIN (theta)**3

      pos(3) = X0 + V0x*t 
     1	+ t*(kx_t (0)+kx_t (1)*theta+kx_t (2)*theta**2
     &  + kx_t (3)*theta**3)
     2	+ t**2*(kx_t (4)+kx_t (5)*theta+kx_t (6)*theta**2
     &  + kx_t (7)*theta**3)
      vel(3) = V0x 
     1	+ (kx_t (0)+kx_t (1)*theta+kx_t (2)*theta**2
     &  + kx_t (3)*theta**3)
     2	+ 2.*t*(kx_t (4)+kx_t (5)*theta+kx_t (6)*theta**2
     &  + kx_t (7)*theta**3)

c     -----------------------
c     - Convert meters to km.
c     -----------------------
      X0 = X0/1000.
      V0x = V0x/1000.
      pos(3) = pos(3)/1000.
      vel(3) = vel(3)/1000.
c
      Y0 = c_y*SIN (theta) + k_y
      V0y = c_vy (0)*SIN (theta)
     1	+ c_vy (1)*SIN (theta)**2
     2	+ c_vy (2)*SIN (theta)**3
     3	+ c_vy (3)*SIN (theta)**4
      pos(1) = Y0 + V0y*t 
     1	+ t*(ky_t (0)+ky_t (1)*theta+ky_t (2)*theta**2
     &  + ky_t (3)*theta**3)
     2	+ t**2*(ky_t (4)+ky_t (5)*theta+ky_t (6)*theta**2
     &  + ky_t (7)*theta**3)
      vel(1) = V0y 
     1	+ (ky_t (0)+ky_t (1)*theta+ky_t (2)*theta**2
     &  + ky_t (3)*theta**3)
     2	+ 2.*t*(ky_t (4)+ky_t (5)*theta+ky_t (6)*theta**2
     &  + ky_t (7)*theta**3)
      Y0 = Y0/1000.
      V0y = V0y/1000.
      pos(1) = pos(1)/1000.
      vel(1) = vel(1)/1000.
c
      pos(2) = 0.0
      vel(2) = 0.0

c     -----------------------------------------------------
c     - Perform matrix multiplication to get ECI position -
c     -----------------------------------------------------
      call Mxv (rotmat,pos,state(1))

c     -----------------------------------------------------
c     - Perform matrix multiplication to get ECI velocity -
c     -----------------------------------------------------
      call Mxv (rotmat,vel,state(4))

      return
      end

      subroutine Mxv (a, b, c)
      real a (3,3), b (3), c (3)

      c (1) = a (1,1)*b(1) + a (1,2)*b(2) + a (1,3)*b(3)
      c (2) = a (2,1)*b(1) + a (2,2)*b(2) + a (2,3)*b(3)
      c (3) = a (3,1)*b(1) + a (3,2)*b(2) + a (3,3)*b(3)

      return
      end
