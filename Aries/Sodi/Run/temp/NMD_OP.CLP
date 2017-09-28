(defrule defcon_change_authority
      (or (and (center CCC) (or (position DO_1)(position DO_2))) (and (center BMDC) (position DR)))
=>
      (assert (authority defcon_change_authority))
      (printout t "Authority to change DEFCON level granted" crlf)) 

(defrule roe_change_authority
      (center CCC) (or (position DO_1)(position DO_2))
=>
      (assert (authority roe_change_authority))
      (printout t "Authority to change Rules of Engagement granted" crlf)) 

(defrule msn_obj_change_authority
      (center BMDC) (position DR)
=>
      (assert (authority msn_obj_change_authority))
      (printout t "Authority to change Mission Objectives granted"crlf)) 

(defrule bp_change_authority
      (center BMDC) (position DR)
=>
      (assert (authority bp_change_authority))
      (printout t "Authority to change Battle Plan granted" crlf)) 

(defrule dea_change_authority
      (or (and (center CCC) (or (position DO_1)(position DO_2))) (and (center BMDC) (position DR)))
=>
      (assert (authority dea_change_authority))
      (printout t "Authority to change Defense Engagement Authority granted" crlf)) 

(defrule rp_change_authority
      (or (and (center CCC) (or (position DO_1)(position DO_2))) (and (center BMDC) (position DR)))
=>
      (assert (authority rp_change_authority))
      (printout t "Authority to change Readiness Posture granted" crlf)) 


(defrule Initial_DEFCON_Change
      (authority defcon_change_authority)
      ?current <- (defcon ?x)
      (test (> ?x 1))
=>
      (retract ?current)
      (assert (defcon 1))
      (printout t "Acknowledge DEFCON change to 1" crlf))     