;The following set of rules give authority to certain centers and 
;positions to perform the indicated changes

(defrule DEFCON_list "list of centers and positions authorized to change DEFCON"
      (or (CCC DO_1)
          (BMDC DR))
      ?change <- (change defcon)
=>
      (assert (defcon_authorization granted))
      (retract ?change))

(defrule ROE_list "list of centers and positions authorized to change ROE"
      (or (CCC DO_1)
          (CCC DO_2))
      ?change <- (change roe)
=> 
      (assert (roe_authorization granted))
      (retract ?change))

(defrule Msn_Obj_list "list of centers and positions authorized to change Msn Obj"
      (or (CCC DO_1)
          (CCC DO_2)
          (BMDC DR))
      ?change <- (change msn_obj)
=> 
      (assert (msn_obj_authorization granted))
      (retract ?change))

(defrule BP_list "list of centers and positions authorized to change Battle Plans"
      (or (CCC DO_1)
          (CCC DO_2)
          (BMDC DR))
      ?change <- (change bp)
=>
      (assert (bp_authorization granted))
      (retract ?change))

(defrule DEA_list "list of centers and positions authorized to change Defense Engagement Authority"
      (or (CCC DO_1)
          (CCC DO_2)
          (BMDC DR))
      ?change <- (change dea)
=>
      (assert (dea_authorization granted))
      (retract ?change))

(defrule RP_authorization "list of centers and positions authorized to change Readiness Posture"
      (or (CCC DO_1)
          (CCC DO_2)
          (BMDC DR))
      ?change <- (change RP)
=> 
      (assert (rp_authorization granted))
      (retract ?change))


;The next rule will determine if the track data is from a new track.
;When a new track is encountered a number of changes are indicated.
;The track should be examined to determine the appropriate levels for changes.



      