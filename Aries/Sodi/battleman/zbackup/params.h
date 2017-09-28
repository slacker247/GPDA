c
c  parameters
c
c     MAX_NUM_ASSETS     ...    maximum number of assets
c     MAX_NUM_THREATS    ...    maximum number of threats
c     MAX_NUM_RINGS      ...    maximum number of pebble rings
c     MAX_NUM_SATS       ...    maximum number of pebbles per ring
c     MAX_NUM_PEBBLES    ...    maximun number of pebbles
c     MAX_NUM_GBIS       ...    maximum number of ground based intercepters
c     MAX_NUM_SITES      ...    maximum number of weapon sites
c     MAX_NUM_WEAPONS    ...    maximum number of weapons
c     MAX_NUM_SHOTS      ...    maximum number of shots per GBI farm
c
      integer MAX_NUM_ASSETS, MAX_NUM_THREATS, MAX_NUM_RINGS,
     *        MAX_NUM_SATS, MAX_NUM_PEBBLES, MAX_NUM_GBIS,
     *        MAX_NUM_SITES, MAX_NUM_SHOTS, MAX_NUM_WEAPONS, 
     *        MAX_PKANG, MAX_PKVEL
c
      parameter (MAX_NUM_ASSETS  = 1751)
      parameter (MAX_NUM_THREATS =   50)
      parameter (MAX_NUM_RINGS   =    1)
      parameter (MAX_NUM_SATS    =   24)
      parameter (MAX_NUM_PEBBLES =   MAX_NUM_RINGS * MAX_NUM_SATS)
      parameter (MAX_NUM_GBIS    =   14)
      parameter (MAX_NUM_SITES   = MAX_NUM_GBIS + MAX_NUM_PEBBLES)
      parameter (MAX_NUM_SHOTS   =   35)
      parameter (MAX_NUM_WEAPONS = MAX_NUM_RINGS *  MAX_NUM_SATS +
     *                             MAX_NUM_GBIS * MAX_NUM_SHOTS)
c
      parameter (MAX_PKANG=10, MAX_PKVEL=10)
