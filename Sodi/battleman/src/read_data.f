      subroutine readdat

      include 'shared.h'

      integer in, id, iclass
      real xlat, xlon, dum
      integer modid(2) 
      real valu (0:11) 
      character*1 string (4)

      in = 1
      open(in, form='formatted', status='old', file='greenta.dat')
c     write(*,*) "Read_data: Reading asset data from greenta.dat" 

      num_assets = 0

      do 20 i = 1, MAX_NUM_ASSETS 
         read(in,99,end=25) id, iclass, xlat, xlon, (string(k),k=1,4), 
     *                     (valu(k),k=0,11), dum, (modid(k),k=1,2)
   99 format(i4,1x,i2,1x,2(f8.3,1x),4a1,1x,13(f6.3,1x),i1,1x,i1)
	 num_assets = i
	 asset_vu          = 10*ichar(string(1)) + ichar(string(2))
	 asset_hardns3 (i) = ichar(string(4))
	 asset_id(i)       = id
	 asset_lat(i)      = xlat
	 asset_lon(i)      = xlon
	 asset_class(i)    = iclass
	 xlat = (90.0 - xlat) / radtodeg
	 xlon = xlon / radtodeg
	 s1 = SIN (xlat)
	 x = s1 * COS (xlon)
	 y = s1 * SIN (xlon)
	 z = COS (xlat)

	 asset_xyz (i,1) = rearth * x
	 asset_xyz (i,2) = rearth * y
	 asset_xyz (i,3) = rearth * z

	 do 10 j = 0, 11
	    asset_value (i,j) = valu (j)
   10    continue

   20 continue
   25 continue

      close (in)

      nsats  = 0
      cvlans = -999.

      do 30 i = 1, MAX_NUM_WEAPONS
         pebble (i) = .FALSE.
   30 continue
       
      num_pebbles = 0
      iorb = 0
      if (pebbles_on) then

      open(in, form='formatted', status='old', file='evconst10.dat')
c     write(*,*) "Read_data: Reading pebble data from evconst10.dat" 
       
      do 50 i = 1,MAX_NUM_WEAPONS       
        read(in,*,end=55) cvalt,cvinc,cvaop,cvlan,icvinv,idx,
     +                    kvgid,idum
        if(kvgid .ge. 80000000) then
           nsats = nsats + 1
	   pebble (nsats) = .TRUE.
           if (cvlans .ne. cvlan)  then
                 iorb = iorb + 1
                 cvlans = cvlan
                 cvaop_old = cvaop
                 orbalt(nsats) = cvalt
                 orbecc(nsats) = 0.0
                 orbinc(nsats) = cvinc
                 orblan(nsats) = cvlan
                 orbaop(nsats) = cvaop
              else
                 orbalt(nsats) = orbalt(nsats-1)
                 orbecc(nsats) = orbecc(nsats-1)
                 orbinc(nsats) = orbinc(nsats-1)
                 orblan(nsats) = orblan(nsats-1)
                 orbaop(nsats) = orbaop(nsats-1)
 
           endif
           sataop(nsats) = (cvaop - cvaop_old) * degtorad
        endif       
   50 continue
   55 continue

      num_pebbles = nsats

      close (in)
      endif
C
C       ----- Read gbi information -----
C
      in = 4
      open(in, form='formatted', status='old', file='gbi.dat')
c     write(*,*) "Read_data: Reading gbi data from gbi.dat"

      num_gbis = 0
      do 60 i = 1, MAX_NUM_GBIS
	 read (in, *, end = 65) gbi_id(i), xlat, xlon,
     +         gbi_pk(i), gbi_gbis(i), gbi_name(i) 
	 gbi_lat (i) = xlat*degtorad
	 gbi_lon (i) = xlon*degtorad
	 nsats = nsats + 1
	 num_gbis = num_gbis + 1
	 pebble (nsats) = .FALSE.
	 xlat = (90.0 - xlat) / radtodeg
	 xlon = xlon / radtodeg
	 s1 = SIN (xlat)
	 x = s1 * COS (xlon)
	 y = s1 * SIN (xlon)
	 z = COS (xlat)
	 site_xyz (nsats,1) = rearth * x
	 site_xyz (nsats,2) = rearth * y
	 site_xyz (nsats,3) = rearth * z
   60 continue
   65 continue

      close (in)
C
C       ----- Tell the world what we read -----
C
      num_weapon_sites = num_pebbles + num_gbis

c     write (*, *)  ' *********** summary of simulation **********'
c     write (*, 98) iorb
c     write (*, 94) num_assets
c     write (*, 97) num_pebbles
c     write (*, 96) num_gbis
c     write (*, 95) MAX_NUM_SHOTS
 
   95 format (' number of shots/gbi = ', i4)
   94 format (' number of assets    = ', i4)
   96 format (' number of gbis      = ', i4)
   97 format (' number of pebbles   = ', i4)
   98 format (' number of orbits    = ', i4)
 
      return
      end
