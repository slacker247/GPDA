      real function Giinterp (x1,x2,y1,y2,x)
c
      implicit none 
c
c------------------------------------------------------------------------------
c
c  Description:
c    Performs linear interpolation. The two interpolation points are specified
c    by (x1,y1) and (x2,y2). The independent variable is x and the interpolated
c    value is returned in giinterp.
c
c
c--------------------------- Calling Arguments --------------------------------
c
c     Name           I/O  Type  Description (range)
c     ----           ---  ----  -------------------
      real x1       ! i   int   x - coordinate of first interpolation point
      real x2       ! i   int   x - coordinate of second interpolation point
      real y1       ! i   int   y - coord of first interpolation point
      real y2       ! i   int   y - coord of second interpolation point
      real x        ! i   int   independent variable
c
c------------------------------------------------------------------------------
c  BEGIN EXECUTABLE CODE

      real slp     !slope of curve
c
c------------------------------------------------------------------------------
c  Code body
c------------------------------------------------------------------------------
c
      slp = (y2-y1)/(x2-x1)
      Giinterp = y1 + slp*(x-x1)
c
      return 
      end
