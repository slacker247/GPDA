      subroutine Gilnchaz (site1, site2, site3, pip1, pip2, pip3,laz)
c
c------------------------------------------------------------------------------
c
c  Description:
c  This function computes the launch azimuth from site to pip.
c  Because the ECI - Z axis always points toward zero azimuth, the true
c  lat and lon points are not important. The value returned is in radians,
c  where North = 0.0 and East = PI/2. 
c
c
c------------------------------------------------------------------------------
c  BEGIN EXECUTABLE CODE
c
      real site1, site2, site3, pip1, pip2, pip3
c
      real v1(3)                 !cross product of site and pip vectors
      real v2(3)                 !cross product of site and ECI z-axis
      real v1mag                 !magnitude of v1
      real v2mag                 !magnitude of v2
      real cosaz                 !cosine of launch azimuth
      real laz
c
c-------------------------------------------------------------------------
c  Code body               
c-------------------------------------------------------------------------

c     -------------------------------------------------
c     - Compute cross product of site and pip vectors -
c     -------------------------------------------------
      v1(1) = site2*pip3 - site3*pip2
      v1(2) = site3*pip1 - site1*pip3
      v1(3) = site1*pip2 - site2*pip1

c     ------------------------------
c     - Magnitude of cross product -
c     ------------------------------
      v1mag = SQRT( v1(1)**2 + v1(2)**2 + v1(3)**2 )

c     ------------------------------------------
c     - Cross product of site and ECI - Z axis -
c     ------------------------------------------
      v2(1) = site2
      v2(2) = -site1
      v2(3) = 0.0

c     ------------------------------
c     - Magnitude of cross product -
c     ------------------------------
      v2mag = SQRT( v2(1)**2 + v2(2)**2 + v2(3)**2 )

c     ------------------------------------------
c     - Case where site is North or South pole -
c     ------------------------------------------
      if(v2mag .eq. 0.0) then
        laz = PI
        if(site3 .lt. 0.0) laz = 0.0 
        return
      endif

c     -------------------------------------------
c     -- Case where target is directly overhead -
c     -------------------------------------------
      if(v1mag .eq. 0.0) then
        laz = 0.0
        return
      endif

      cosaz = (v1 (1)*v2 (1) + v1 (2)*v2 (2) + v1 (3)*v2 (3))/ 
     &                         (v1mag*v2mag)
      if(abs(cosaz) .gt. 1.0) then
         if (cosaz .gt. 1.0)  cosaz = 1.0
         if (cosaz .lt. -1.0) cosaz = -1.0
      endif
      laz = ACOS (cosaz)
      if(v1(3) .lt. 0.0) laz = -laz
 
      return
      end
