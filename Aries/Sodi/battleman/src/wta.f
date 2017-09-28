      subroutine Wta
c
c  solves the weapon-target assignment problem 
c
      include 'shared.h'
c     integer total
      integer temp
      integer  price (MAX_NUM_THREATS), index (MAX_NUM_THREATS)
c     real tbegin, tend, tmp (2)
      logical again
c
c     tbegin = etime (tmp)
c
c  initialize variables
c
      do 10 i = 1, num_threats
        price (i) = 1
        bid (i)   = 0
        index (i) = 0
   10 continue
c
c     iter = 0
      eps  = 1
c
c  iteration
c
   15 continue
c
c     iter = iter + 1
      again = .FALSE.
c
      do 40 i = 1, num_threats
        if (bid (i) .GT. 0) go to 40
c
c  bidder i bids on object k 
c
        maxval = 0
        next_maxval = 0
c
        do 20 j = 1, num_threats
          temp = cost (i,j) - price (j)
          if (temp .GT. next_maxval) then
            if (temp .GT. maxval) then
                next_maxval = maxval
                maxval = temp
                k = j
              else
                next_maxval = temp
            endif
          endif
   20   continue
c
        bid (i) = k
        price (k) = cost (i,k) - next_maxval + eps
c
c  has any other bidder bid on object k ... if so bump old bidder
c
        if (index (k) .NE. 0) then
          bid (index (k)) = 0
          again = .TRUE.
        endif
c
        index (k) = i
c
   40 continue
c
      if (again) go to 15
c
c  termination
c
c     tend = etime (tmp)
c
c     total = 0
c     do 50 i = 1, num_threats
c        total = total + cost (i, bid (i))
c  50 continue
c
      return
      end
