      integer function gisrchar(num,array,value)
c
      implicit none
c
c
c------------------------------------------------------------------------------
c
c  Description
c    This function returns the index of the lower array element of the pair
c    which bracket 'value'. It is assumed that the values in 'array' are 
c    monotonically increasing.    
c
c
c--------------------------- Calling Arguments --------------------------------
c
c     Name              I/O    Description (range)
c     ----              ---    -------------------
      integer num      ! i     number of elements in the array to search
      real array(num)  ! i     array of real values to search
      real value       ! i     target value to search for
c
c------------------------------------------------------------------------------
c  BEGIN EXECUTABLE CODE
c
      integer i           !loop counter 
c
c------------------------------------------------------------------------------
c  Code body
c------------------------------------------------------------------------------
c
      if(value .lt. array(1)) then
        gisrchar = 1
        return
      endif
      if(value .ge. array(num)) then
        gisrchar = num-1
        return
      endif
c
      do 100 i=2,num
         if(value .lt. array(i)) then
           gisrchar = i-1
           return
         endif
  100 continue
c
      return
      end
