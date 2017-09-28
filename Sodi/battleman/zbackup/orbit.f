      subroutine Orbit 
      include 'shared.h'
                                                                        
      do 100 i = 1, num_pebbles
            xmly = xmaly (i) + war_time * angv (i)

            r = orbalt (i) * (1.0 - orbecc (i)**2)/(1.0 + 
     *                              orbecc (i)*cos(xmly))

            rp = r * cos (xmly)
            rq = r * sin (xmly)

            site_xyz (i,1) = rot_1 (i)*rp + rot_4 (i)*rq
            site_xyz (i,2) = rot_2 (i)*rp + rot_5 (i)*rq
            site_xyz (i,3) = rot_3 (i)*rp + rot_6 (i)*rq

  100 continue

      return                                                            
      end                                                               
