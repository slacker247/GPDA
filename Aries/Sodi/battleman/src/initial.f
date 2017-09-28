      subroutine Initial
      include 'shared.h'

c  constants

      pi            = 4.0 * ATAN (1.0)
      twopi         = 2.0*pi
      rearth        = 6371011.0
c     rearth        = 6378145.0
      we            = 7.29211585e-05
      gmu           = 3.9860068e+14
      ppgtol        = 1.e-5
      degtorad      = pi/180.0
      radtodeg      = 180.0 / pi
      mustepan      = 0.0
      gi_lops_delay = 5.0
      burn_time     = 77.0
      total_delay   = burn_time + gi_lops_delay
      wpn_endoacq   = 5.0
      dphase        = 0.0
      sqrt_mu       = SQRT (gmu)
      dtc           = 5.0
      radius        = 1.0e+4

c  run time parameters

      war_time_increment =   30
      pebbles_on         = .FALSE.
      tpt                = .FALSE.

c  values need by tpt codes

      c_x = 152503.0
      k_x = 2693.52  
      c_y = 168696.13058375  
      k_y = 6383196.7252063

      c_vx (0) = -196927.0
      c_vx (1) = 125368.0
      c_vx (2) = 198241.0
      c_vx (3) = -100293.2
      c_vx (4) = 42834.6   
      c_vx (5) = -9130.12

      c_vy (0) = 3381.5
      c_vy (1) = 9808.6
      c_vy (2) = -10965.5
      c_vy (3) = 4065.7

      kx_t (0) = -131.4
      kx_t (1) = 1034.7
      kx_t (2) = -1167.9
      kx_t (3) = 355.3
      kx_t (4) = -1.3
      kx_t (5) = 0.51
      kx_t (6) = 0.5
      kx_t (7) = -0.18

      ky_t (0) = -26.9
      ky_t (1) = -423.9
      ky_t (2) = 259.2
      ky_t (3) = -58.3
      ky_t (4) = -5.3
      ky_t (5) = 4.2
      ky_t (6) = -2.8
      ky_t (7) = 0.63

      return
      end
