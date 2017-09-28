;File initial.clp
;Define the constructs used to initialize and update the database

(deftemplate MAIN::phase_control
	(slot phase)
	(multislot phase-after))

(deffacts MAIN::control-information
	(phase_control (phase-after initialize monitor))
	(phase_control (phase-after monitor assess))
	(phase_control (phase-after assess replan))
	(phase_control (phase-after replan execute)))


(defrule MAIN::change-phase
	(declare (salience -100))
	?phase <- (phase_control (phase ?current-phase))
	(phase_control (phase-after ?current-phase ?next-phase))
=>
	(retract ?phase)
	(assert (phase_control (phase ?next-phase))))

(defglobal MAIN
	?*defcon* = 5
	?*rp* = 2
	?*dea* = "Hold"
	?*roe* = "N. America"
	?*msn_obj* = "Current"
	?*bp* = "General Umberella")

(defclass MAIN::STATE (is-a USER)
      (role concrete) (pattern-match reactive)
      (slot DEFCON (type INTEGER) (allowed-values 1 2 3 4 5)(create-accessor read-write))
      (slot RP (type INTEGER) (allowed-values 1 2)(create-accessor read-write))
      (slot DEA (type STRING) (create-accessor read-write))
      (slot ROE (type STRING) (create-accessor read-write))
      (slot Msn_Obj_Name (type STRING) (create-accessor read-write))
      (slot BP_Name (type STRING)(create-accessor read-write)))

(defclass   MAIN::MSN_OBJ (is-a USER)
      (role concrete)(pattern-match reactive)
      (slot msn_obj_name (type STRING)(create-accessor read-write))	
      (slot strategy (type STRING) (create-accessor read-write))
      (slot tactic (type STRING) (create-accessor read-write))
      (slot pms (type FLOAT) (range 0.0 1.0)(create-accessor read-write))
      (slot mode (type INTEGER) (allowed-values 1 2 3)(create-accessor read-write))
      (slot withhold (type INTEGER)(create-accessor read-write)) 
      (slot add_bstrs (type INTEGER)(create-accessor read-write)))

(defclass   MAIN::BATTLE_PLAN (is-a USER)
      (role concrete)(pattern-match reactive)
      (slot bp_name (type STRING)(create-accessor read-write)) 	
      (slot planner_mode (type STRING)(create-accessor read-write))
      (slot target_value_cutoff (type STRING) (create-accessor read-write))
      (slot accept_kill_criteria (type STRING)(create-accessor read-write))
      (slot override_salvo (type INTEGER) (create-accessor read-write))
      (slot launch_mode (type INTEGER) (allowed-values 1 2 3)(create-accessor read-write))
      (slot rv_threshold (type FLOAT) (range 0.0 1.0)(create-accessor read-write))
      (slot pk_cutoff (type FLOAT) (range 0.0 1.0)(create-accessor read-write))
      (slot weight_population (type INTEGER) (create-accessor read-write))
      (slot weight_military (type INTEGER)(create-accessor read-write))
      (slot weight_selfdefense (type INTEGER)(create-accessor read-write))
      (slot weight_ncauthority (type INTEGER)(create-accessor read-write))
      (slot weight_industrial (type INTEGER)(create-accessor read-write)))

(defclass  MAIN::PLAYER (is-a USER)
	(role concrete)(pattern-match reactive)
	(slot node_type (type STRING)(create-accessor read-write))
	(slot node_number (type INTEGER)(create-accessor read-write))
	(slot position_type (type STRING)(create-accessor read-write))
	(slot position_number (type INTEGER)(create-accessor read-write))
	(slot active (type INTEGER)(create-accessor read-write)))

(defclass  MAIN::ENGAGEMENT (is-a USER)
	(role concrete)(pattern-match reactive)
	(slot eng_id (default 1) (create-accessor read-write))
	(slot track_id (type STRING) (default "11:22:33") (create-accessor read-write))
	(slot eng_status (type STRING)(create-accessor read-write))
	(slot cur_eng_num_weapons (type INTEGER)(create-accessor read-write))
	(slot cur_eng_tti (type FLOAT)(create-accessor read-write))
	(slot cur_eng_pk (type FLOAT)(allowed-values 0.0 1.0)(create-accessor read-write))
	(slot eng_opp_rem (type INTEGER)(create-accessor read-write)))

(defclass  MAIN::MISSILE_TRACK (is-a USER)
	(role concrete)(pattern-match reactive)
	(slot id (type STRING)(create-accessor read-write))
	(slot obj_type (type STRING)(create-accessor read-write))
	(slot missile_type (type STRING)(create-accessor read-write))
	(slot missile_class (type STRING)(create-accessor read-write))
	(slot exp_targets (type INTEGER) (create-accessor read-write))
	(slot leth_val (type FLOAT)(allowed-values 0.0 1.0)(create-accessor read-write))
	(slot launch_time (type FLOAT)(create-accessor read-write))
	(slot launch_country (type STRING)(create-accessor read-write))
	(slot launch_site (type STRING)(create-accessor read-write))
	(slot imp_lat (type FLOAT)(create-accessor read-write))
	(slot imp_long (type FLOAT)(create-accessor read-write))
	(slot earliest_imp_time (type FLOAT)(create-accessor read-write))
	(slot predicted_imp_reg (type STRING)(create-accessor read-write))
	(slot num_bstrs (type INTEGER)(create-accessor read-write))
	(slot lethals_exp (type INTEGER)(create-accessor read-write))
	(slot track_status (type STRING)(create-accessor read-write)))

(defclass  MAIN::GBI_FARM (is-a USER)
	(role concrete) (pattern-match reactive)
	(slot GBI_farm_num (type INTEGER))
	(slot weapons (type INTEGER))
	(slot weapons_held (type INTEGER)))

(defclass MAIN::WEAPONS (is-a USER)
	(role concrete) (pattern-match reactive)
	(multislot gbi_farms (type INSTANCE-NAME)(create-accessor read-write))
	(message-handler put-next)
	(message-handler modify-next))

(defmessage-handler MAIN::WEAPONS put-next primary (?value)
	(bind ?x (duplicate-instance ?value))
	(slot-direct-insert$ gbi_farms (+ (length$ gbi_farms) 1) ?x)
	(send ?value delete))


(defclass  MAIN::STRING_PROMPT (is-a USER)
	(role concrete)(pattern-match reactive)
	(slot prompt (type STRING)(create-accessor read-write)))

(defclass  MAIN::INTEGER_PROMPT (is-a USER)
	(role concrete)(pattern-match reactive)
	(slot prompt (type INTEGER)(create-accessor read-write)))

(defclass  MAIN::MSG_HEADER (is-a USER)
	(role concrete)
                  (pattern-match reactive)
	(slot destination_id (type INSTANCE-NAME)(create-accessor read-write))
	(slot source_id (type INSTANCE-NAME)(create-accessor read-write))
        (slot opcode (type INTEGER)(create-accessor read-write))
	(slot gvt (type FLOAT)(create-accessor read)))

(defclass  MAIN::MESSAGE (is-a USER)
	(role concrete) (pattern-match reactive)
	(slot message_header (type INSTANCE-NAME)(create-accessor read-write))
	(slot msg_body (type INSTANCE-NAME)(create-accessor read-write)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;This section defines the modules of the SCs created for the demo
;; and decision authority for the particular SC

(defmodule MAIN (export ?ALL))

(defmodule BMDC_DR (import MAIN deftemplate ?ALL)
		   (import MAIN defclass ?ALL)
		   (import MAIN defglobal ?ALL))

(defmodule FU_DR (import MAIN deftemplate ?ALL)
		 (import MAIN defclass  ?ALL)
		 (import MAIN defglobal ?ALL))

(defmodule CC_DO (import MAIN deftemplate ?ALL)
		 (import MAIN defclass ?ALL)
		 (import MAIN defglobal ?ALL))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;constructs belonging to the BMDC_DR MODULE

(deftemplate MAIN::Weapons
	(slot GBI_farm_num))

(deffacts BMDC_DR::Weapons
	(Weapons (GBI_farm_num 1101)))

(deftemplate BMDC_DR::Authority
      (slot change))

(deffacts BMDC_DR::Authority
      (Authority (change DEFCON))
      (Authority (change MSN_OBJ))
      (Authority (change BP))
      (Authority (change DEA))
      (Authority (change RP)))

(deftemplate MAIN::Contact
	(slot hierarchy)
	(multislot ID))

(deffacts BMDC_DR::Contacts
	(Contact (hierarchy superior) (ID CC DO))
	(Contact (hierarchy subordinate) (ID FU DR)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftemplate CC_DO::Authority
      (slot change))

(deffacts CC_DO::Authority
      	(Authority (change DEFCON))
      	(Authority (change DEA))
      	(Authority (change RP))
      	(Authority (change ROE)))

(deffacts CC_DO::Contacts
	(Contact (hierarchy subordinate) (ID BMDC DR))
	(Contact (hierarchy subordinate) (ID FU DR)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffacts FU_DR::Contacts
	(Contact (hierarchy superior) (ID CC DO))
	(Contact (hierarchy superior) (ID BMDC DR)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;next set of rules are used to initialize an active SC
;;an SC shold have a module on the focus stack
;;the module will contain assertions of his authority



	
(defrule MAIN::OP_INIT_SC 
      (phase_control (phase initialize))
       (opcode 0)      
=>
      (assert (node (send [SimCmdr] get-node_type )))
      (assert (position (send [SimCmdr] get-position_type)))
      (assert (active (send [SimCmdr] get-active)))	
      (assert (opcode 502)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defrule MAIN::Get_OP
      (object (is-a MSG_HEADER))
      (phase_control (phase initialize))	
      	
=>
     (assert (opcode (send [msg_header] get-opcode))))	

(defrule MAIN::OP_DB_STATUS  
       	(phase_control (phase initialize))
        (opcode 100)
	(object (is-a STATE) (name [msg_body]))
=>
	(bind ?*defcon* (send [msg_body] get-DEFCON))
	(bind ?*rp* (send [msg_body] get-RP))
	(bind ?*dea* (send [msg_body] get-DEA))
	(bind ?*roe* (send [msg_body] get-ROE))
	(bind ?*msn_obj* (send [msg_body] get-Msn_Obj_Name))
	(bind ?*bp* (send [msg_body] get-BP_Name))
	(assert (opcode 502)))

(defrule MAIN::OP_INIT_MO
	(phase_control (phase initialize))
        (opcode 301) 
	
=>
	(duplicate-instance [msn_obj])
	(send [msn_obj] delete)
	(assert (opcode 502)))


(defrule MAIN::OP_INIT_BP
	(phase_control (phase initialize))
        (opcode 302)  
=>
	(duplicate-instance [bp])
	(send [bp] delete)
	(assert (opcode 502)))

(defrule MAIN::OP_INIT_WA
	(phase_control (phase initialize))
	(opcode 303) 
	(object (is-a GBI_FARM) (weapons ?x) (weapons_held ?y)) 
=>
	(assert (wa (- ?x ?y)))
	(assert (opcode 502)))

(defrule MAIN::OP_WA
	(phase_control (phase monitor))
        ?op <- (opcode 107)  
    	(object (is-a GBI_FARM) (weapons ?x) (weapons_held ?y))
	?z <- (wa ?wa)
=>
	(retract ?z ?op)
	(assert (wa (- ?x ?y)))
	(assert (opcode 502)))

(defrule MAIN::OP_POT_EVENT_NEW
	(phase_control (phase monitor))
        (opcode 130) 
	(object (is-a MISSILE_TRACK) (name [msg_body]) (id ?y))
	(not (track_id ?y))

=>
	(assert (track_id ?y))
	(duplicate-instance [msg_body])
	(assert (assess ROE)))

(defrule MAIN::OP_POT_EVENT_UPDATE
	(phase_control (phase monitor))
	?x <- (opcode 130)
	(object (is-a MISSILE_TRACK) (name [msg_body]) (id ?track_id))
	(object (is-a MISSILE_TRACK) (name ?y&~[msg_body]) (id ?track_id))
=>
	(retract ?x)
	(duplicate-instance msg_body to ?y)
	(assert (assess ROE)))

(defrule MAIN::OP_NEW_ENGMT
	(phase_control (phase monitor))
	(opcode 108)
	(object (is-a ENGAGEMENT) (name [msg_body]) (eng_id ?y))
	(not (eng_id ?y))
	
=>
	(assert (eng_id ?y))
	(duplicate-instance [msg_body])		;Need engagement id so don't keep making new instances
	(assert (assess MSN_OBJ)))

(defrule MAIN::OP_UPDATE_ENGMT
	(phase_control (phase monitor))
	?x <- (opcode 108)
	(object (is-a ENGAGEMENT) (name [msg_body]) (eng_id ?eng_id))
	(object (is-a ENGAGEMENT) (name ?y&~[msg_body]) (eng_id ?eng_id))
=>
	(retract ?x)
	(duplicate-instance msg_body to ?y))

(defrule MAIN::OP_ENGMT_ENGAGED
	(phase_control (phase monitor))
; Should have id of engagement match track id, hard coded
	(object (is-a ENGAGEMENT) (eng_status "Pre Plan"|"Planned"|"Launched"|"Post IFSR"|"Await KA"|
						"Engaged"|"In Flight"))
        (opcode 108)  
=>
	(assert (opcode 502)))

(defrule MAIN::OP_ENGT_NEED_ENGAGED
	(phase_control (phase monitor))
	(object (is-a ENGAGEMENT) (eng_status "Missed"))
	(opcode 108)
=>
	(assert (assess MSN_OBJ)))


(defrule MAIN::OP_ENGT_REMOVE
	(phase_control (phase monitor))
	(object (is-a ENGAGEMENT) (name ?y) (eng_status "Killed"))
=>
	(send ?y delete)
	(assert (assess DEFCON)))

(defrule MAIN::rp
	(phase_control (phase assess))
	?x <- (assess RP)
=>
	(retract ?x)
	(assert (change RP)))

(defrule MAIN::assess_MSN_OBJ
	(phase_control (phase assess))
	?x <- (assess MSN_OBJ)
	(object (is-a ENGAGEMENT) (cur_eng_pk ?y) (eng_status ~"Pre Plan"))	;Should be averaged over all engagements
	(object (is-a MISSILE_TRACK) (exp_targets ?z))		;Should be total over all tracks
	(object (is-a GBI_FARM) (GBI_farm_num ?num) (weapons ?weapons) (weapons_held ?held))
								;Should be sum over all GBI's for current SC
	(Weapons (GBI_farm_num ?num))
	(wa ?wa)
=>
	(assert (replan_prob (replan ?z ?y ?wa  1 1))) ;Call the external replan trigger routine	
	(retract ?x)
	(assert (change MSN_OBJ)))

(defrule MAIN::roe_assess
	(phase_control (phase assess))
	(object (is-a MISSILE_TRACK) (name [msg_body]) (imp_lat ?lat) (imp_long ?long))
	?x <- (assess ROE)
=>
	(retract ?x)
	(bind ?*roe* (roe_assess ?lat ?long))
	(printout t "ROE is " ?*roe* crlf)
	(assert (assess DEA)))

(defrule MAIN::dea_assess
	(phase_control (phase assess))
	(object (is-a MISSILE_TRACK) (name [msg_body]))
	?x <- (assess DEA)
=>
	(retract ?x)
	(bind ?*dea* "Free"))
	
(defrule BMDC_DR::defcon_1
	(declare (auto-focus TRUE))
	(phase_control (phase assess))
	(Authority (change DEFCON))
	(object (is-a MISSILE_TRACK))	;Should have a way to say valid threat
	(test (<> 1 ?*defcon*))
=>
	(bind ?*defcon* 1)
	(return))

;(defrule MAIN::OP_DEFCON
;     (phase_control (phase replan))
;       (opcode 101)	  
;      (Authority (change DEFCON))		
;=>		
;      (assert (assess DEFCON)))

;(defrule MAIN::OP_RP 
;      (phase_control (phase replan))
;      (opcode 102)     
;      (Authority (change RP))
;=>	
;      (assert (assess RP)))

;(defrule MAIN::OP_DEA
;      (phase_control (phase replan))
;      (opcode 103)	     
;      (Authority (change DEA))
;=>
;      (assert (assess DEA)))

;(defrule MAIN::OP_ROE
;      (phase_control (phase replan))
;      (opcode 104)     
;      (Authority (change ROE))
;=>
;      (assert (assess ROE)))

;(defrule MAIN::OP_MSN_OBJ
;      (phase_control (phase replan))
;      (opcode 105)     
;      (Authority (change MSN_OBJ))
;=>
;      (assert (assess MSN_OBJ)))

;(defrule MAIN::OP_BP
;      (phase_control (phase replan))
;      (opcode 106)     
;      (Authority (change BP))
;=>
;      (assert (assess BP)))

(defrule MAIN::Remove_Opcode
	(phase_control (phase execute))
	?x <- (opcode ?y&~501&~502&~503)
=>
	(retract ?x))	

(defrule MAIN::Return_Header
	(phase_control (phase execute))
	(opcode ?x&501|502|503)
=>
	(modify-instance [msg_header] 
		(destination_id [BattlePlanner]) (source_id [SimCmdr])))

(defrule MAIN::OP_RTRN_ERR
	(phase_control (phase execute))
	?x <- (opcode 503)
	(object (destination_id [BattlePlanner]))
=>
	(retract ?x)
	(send [msg_header] put-opcode 503)
	(send [msg_body] delete)
	(make-instance [msg_body] of STRING_PROMPT (prompt "An error has occurred."))
	(assert (return message)))

(defrule MAIN::OP_RTRN_ACK
	(phase_control (phase execute))
	?x <- (opcode 502)
	(object (destination_id [BattlePlanner]))
=>
	(retract ?x)
	(modify-instance [msg_header] (opcode 502))
	(send [msg_body] delete)
	(make-instance [msg_body] of INTEGER_PROMPT (prompt 1))
	(assert (return message)))


(defrule MAIN::OP_RTRN_DCN
	(phase_control (phase execute))
	?x <- (opcode 501)
	(object (destination_id [BattlePlanner]))
=>
	(retract ?x)
	(modify-instance [msg_header] (opcode 501))
	(send [msg_body] delete)
	(make-instance [msg_body] of STATE 
		(DEFCON ?*defcon*)
		(RP ?*rp*)
		(DEA ?*dea*)
		(ROE ?*roe*)
		(Msn_Obj_Name ?*msn_obj*)
		(BP_Name ?*bp*))
	(assert (return message)))

(defrule MAIN::Return_Message
	?phase <- (phase_control (phase execute))
	?x <- (return message)
=>
	(modify-instance [msg] (message_header [msg_header]) (msg_body [msg_body]))
	(retract ?phase))












