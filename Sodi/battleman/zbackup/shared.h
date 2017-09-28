c
      include 'params.h'

      integer num_threats, num_assets, num_modules, num_pebbles,
     *        num_gbis, num_weapon_sites

      integer asset_hardns3 (MAX_NUM_ASSETS)

      real pi,  twopi, radtodeg, rearth, we, radius,
     *     gmu, ppgtol, sqrt_mu, dphase, degtorad

      integer war_time_increment, iout

      integer cost (MAX_NUM_THREATS, MAX_NUM_THREATS),
     *        map (MAX_NUM_THREATS), threat_id (MAX_NUM_THREATS)

      real war_time (MAX_NUM_THREATS)

      real asset_id (MAX_NUM_ASSETS), asset_lat (MAX_NUM_ASSETS),
     *     asset_lon (MAX_NUM_ASSETS), asset_class (MAX_NUM_ASSETS),
     *     asset_xyz (MAX_NUM_ASSETS, 3)

      real asset_value (MAX_NUM_ASSETS, 0:11)

      common /shared/ iout

      real*8 current_time
      common /shared/ pi, twopi, radtodeg, rearth, we, radius,
     *                gmu, ppgtol, dphase, sqrt_mu, degtorad,
     *                current_time

      common /shared/  war_time, war_time_increment

      common /shared/ num_threats, num_assets, num_modules, 
     *                num_pebbles, num_gbis, num_weapon_sites

      integer     threat_class (MAX_NUM_THREATS)
      real        threat_state (MAX_NUM_THREATS, 6)
      real        threat_time  (MAX_NUM_THREATS)
      character*8 threat_type(MAX_NUM_THREATS)
      character*8 threat_impact(MAX_NUM_THREATS)
      common /shared/ threat_state, threat_time, threat_class,
     +                threat_type,  threat_impact

      logical threat_play (MAX_NUM_THREATS), pebble (MAX_NUM_SITES)
      common /shared/ threat_play, pebble

      common /shared/ asset_id, asset_lat, asset_lon, asset_class,
     *                asset_value, asset_xyz

      common /shared/ asset_hardns3

c  impact_prediction

      real xold_obj_state (MAX_NUM_THREATS,6), 
     *     xnew_obj_state (MAX_NUM_THREATS,6)
      common /shared/ xold_obj_state, xnew_obj_state
      
      real orbalt (MAX_NUM_PEBBLES), orbecc (MAX_NUM_PEBBLES), 
     *     orbinc (MAX_NUM_PEBBLES), orblan (MAX_NUM_PEBBLES),
     *     orbaop (MAX_NUM_PEBBLES), angv (MAX_NUM_PEBBLES),   
     *     rot_1 (MAX_NUM_PEBBLES),  rot_2 (MAX_NUM_PEBBLES),
     *     rot_3 (MAX_NUM_PEBBLES),  rot_4 (MAX_NUM_PEBBLES),  
     *     rot_5 (MAX_NUM_PEBBLES),  rot_6 (MAX_NUM_PEBBLES)
      common/odata/ orbalt, orbecc, orbinc, orblan, orbaop,
     *              angv,   rot_1, rot_2, rot_3, rot_4,
     *              rot_5,  rot_6

      real    xmaly (MAX_NUM_PEBBLES), sataop (MAX_NUM_PEBBLES) 
      real    site_xyz (MAX_NUM_SITES, 3)
      real    gbi_lat(MAX_NUM_SITES), gbi_lon(MAX_NUM_SITES)
      real    gbi_pk(MAX_NUM_SITES)
      integer gbi_gbis(MAX_NUM_SITES), gbi_id(MAX_NUM_SITES)
      character*16 gbi_name(MAX_NUM_SITES)
      common/sdata/ xmaly, sataop, site_xyz, gbi_lat, gbi_lon,
     +               gbi_pk, gbi_id, gbi_gbis, gbi_name

      real trntry (MAX_NUM_THREATS)
      common/thtdat/  trntry

      integer wta_matrix (MAX_NUM_SITES, MAX_NUM_THREATS)
      real    wta_time   (MAX_NUM_SITES, MAX_NUM_THREATS)
      common/wta_solver/ wta_matrix, wta_time, cost, map, threat_id

      integer bid (MAX_NUM_THREATS)
      common/wta_plans/ bid

      logical pebbles_on, tpt, switch, done
      common/logicals/ pebbles_on, tpt, switch, done

c  variables needed by tpt codes

      integer num_pkang, num_pkvel, num_tc

      real burn_time, k_x, c_x, k_y, c_y, c_vx (0:5), c_vy (0:5), 
     *                kx_t (0:7), ky_t (0:7), rmax (500), mustepan, 
     *                gi_lops_delay, min_alt, max_alt,  wpn_endoacq,
     *                dtc, max_range, total_delay

      real pkang (MAX_PKANG), pkvel(MAX_PKVEL), 
     *     pkill (MAX_PKANG,MAX_PKVEL), jeff_time (MAX_NUM_THREATS)

      common /tpt_codes/ burn_time, k_x, c_x, k_y, c_y, c_vx, c_vy, 
     *                kx_t, ky_t, rmax, mustepan, gi_lops_delay, 
     *                min_alt, max_alt, wpn_endoacq, pkang, pkvel,
     *                pkill, num_pkang, num_pkvel, dtc, num_tc, 
     *                max_range, total_delay, jeff_time

