      subroutine Read_threats

      include 'shared.h'

c      do 10 i = 1, MAX_NUM_THREATS
c         threat_play (i) = .FALSE.
c   10 continue

      switch = .TRUE.

c      in = 2
c      open (in, form = 'formatted', status = 'old', file =
c     *                                           'arctic.threats')

c      do 20 i = 1, MAX_NUM_THREATS
c         read(in,*,end=25) (threat_state (i,k),k=1,6), threat_time (i),
c     *                      trntry (i)
c         threat_id (i) = i + 1000
c         num_threats       = i
c         threat_class (i)  = 1
c         threat_play (i)   = .TRUE.
c         war_time (i) = threat_time (i)
c
c   20 continue
c   25 continue

c      close (in)
      
      do 40 i = 1, MAX_NUM_SITES
         do 30 j = 1, MAX_NUM_THREATS
            wta_matrix (i,j) = 9000000
            wta_time   (i,j) = 0.0
   30    continue
   40 continue

c     write (*, 99) num_threats
c   99 format (' number of threats  = ', i4)

      return
      end
