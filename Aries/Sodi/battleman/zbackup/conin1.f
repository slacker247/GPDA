      subroutine Conin1

      include 'shared.h'

      real cl_temp (MAX_NUM_PEBBLES), sl_temp (MAX_NUM_PEBBLES), 
     *     cw_temp (MAX_NUM_PEBBLES), sw_temp (MAX_NUM_PEBBLES), 
     *     ci_temp (MAX_NUM_PEBBLES), si_temp (MAX_NUM_PEBBLES)
      real xmean (MAX_NUM_PEBBLES)
c                                                                       
c   convert inclination angle from degrees to radians                   
c   convert longitude of ascending node to radians                      
c   convert argument of perigee to radians                              
c
      do 100 i = 1, num_pebbles
         if (pebble (i)) then
            orbalt (i) = orbalt (i) * 1000.0  + rearth                   
            orblan (i) = orblan (i) * degtorad
            orbaop (i) = orbaop (i) * degtorad
c                                                                       
c   calculate period and orbital velocity (rad/sec)                     
c                                                                       
             angv (i) = sqrt_mu / (orbalt (i)**1.5)
c                                                                       
c   evaluate components of rotation matrix for each orbital plane       
c   l= cap omega, w= small omega, i= inclination angle                  
c                                                                       
             cl_temp (i) = cos (orblan (i))
             sl_temp (i) = sin (orblan (i))
             cw_temp (i) = cos (orbaop (i))
             sw_temp (i) = sin (orbaop (i))
             ci_temp (i) = cos (orbinc (i))
             si_temp (i) = sin (orbinc (i))
c
             rot_1 (i) =  cl_temp (i)*cw_temp (i) - sl_temp (i)*
     *                    sw_temp (i)*ci_temp (i)
             rot_4 (i) = -cl_temp (i)*sw_temp (i) - sl_temp (i)*
     *                    cw_temp (i)*ci_temp (i)
             rot_2 (i) =  sl_temp (i)*cw_temp (i) + cl_temp (i)*
     *                    sw_temp (i)*ci_temp (i)
             rot_5 (i) = -sl_temp (i)*sw_temp (i) + cl_temp (i)*
     *                    cw_temp (i)*ci_temp (i)
             rot_3 (i) =  sw_temp (i)*si_temp (i)
             rot_6 (i) =  cw_temp (i)*si_temp (i)
c                                                                       
             xmean (i) = sataop (i) + dphase*angv (i)

	     if (xmean (i) .GT. twopi) then
                   xmaly (i) = xmean (i) - twopi
	        else
	           xmaly (i) = xmean (i)
	     endif

         endif
  100 continue

      return                                                            
      end                                                               
